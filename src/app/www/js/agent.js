// CrossTab Studio を AI やスクリプトから動かす入口（window.CTS）
//   使い方は AGENTS.md（リポジトリの最上位）にまとめてある。画面の操作と同じ関数を呼ぶだけで、計算は画面で作った解析と同じ。
//   ここで決めた関数の名前と引数の書き方は、版が変わっても変えない（変えるときは足すだけにする）
(function () {
  'use strict';
  var A = window.App, S = A.S, API_VERSION = 1;
  function appVersion() { return (window.CTS_CHANGES && window.CTS_CHANGES[0] && window.CTS_CHANGES[0].v) || ''; }
  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function fail(msg) { throw new Error(msg); }

  // アプリの準備（R の起動と解析の一覧の読み込み）が済むまで待つ
  // CTS を使い始めたら、初回起動の案内と変更点の一覧を閉じ、以後は出さない（ダイアログが図の保存などの操作をふさがないように）。
  //   見た版の記録（localStorage）は書き換えない。あとで人が同じブラウザで開いたときは、案内がふだんどおり出る
  function quiet() { A.agentMode = true; if (document.querySelector('#md_modal .md-welcome') && A.closeModal) A.closeModal(); }
  async function ready(timeoutMs) {
    quiet();
    var t0 = Date.now(), lim = timeoutMs || 180000;
    while (!(S.registry && S.registry.length) || document.getElementById('md_boot')) {
      if (Date.now() - t0 > lim) fail('CrossTab Studio did not finish starting within ' + Math.round(lim / 1000) + ' s.');
      await sleep(200);
    }
    quiet();
    return true;
  }

  // 列の指定：列番号（1 から）、見出し（質問文そのもの）、表示名。前方一致・部分一致は 1 つに決まるときだけ使う
  function col(ref) {
    if (!S.data) fail('No data loaded. Call CTS.load() first.');
    if (typeof ref === 'number') { if (!A.v(ref)) fail('Column ' + ref + ' does not exist.'); return ref; }
    var s = String(ref).trim(), vs = S.P.vars, norm = function (t) { return String(t || '').replace(/\s+/g, ' ').trim(); };
    var tries = [
      function (x) { return norm(x.header) === norm(s) || norm(x.label) === norm(s) || norm(x.full) === norm(s); },
      function (x) { return norm(x.header).indexOf(norm(s)) === 0 || norm(x.label).indexOf(norm(s)) === 0; },
      function (x) { return norm(x.header).indexOf(norm(s)) >= 0 || norm(x.label).indexOf(norm(s)) >= 0; }];
    for (var k = 0; k < tries.length; k++) {
      var hit = vs.filter(tries[k]);
      if (hit.length === 1) return hit[0].col;
      if (hit.length > 1) fail('Column "' + s + '" is ambiguous: ' + hit.map(function (x) { return x.col + ' ' + x.label; }).join(' | '));
    }
    fail('Column "' + s + '" was not found. Use CTS.columns() to see the columns.');
  }

  // ---------------- 調べる
  function analyses() {
    return S.registry.map(function (a) {
      return { id: a.id, name: a.name, group: a.group, description: a.desc,
        slots: (a.slots || []).map(function (s) { return { id: s.id, label: s.label, types: s.types, many: !!s.many, optional: !!s.optional }; }),
        options: (a.opts || []).map(function (o) { return { id: o.id, kind: o.kind, label: o.label, default: o.default, choices: o.choices ? o.choices.map(function (c) { return c.value; }) : undefined }; }) };
    });
  }
  function columns() {
    if (!S.data) return [];
    return S.P.vars.map(function (x) {
      return { col: x.col, header: x.header, label: x.label, type: x.type, n: x.n, computed: !!x.comp,
        options: A.isCat(x) ? x.cats.map(function (c) { return { value: c.v, n: c.n }; }) : undefined, summary: x.summary };
    });
  }

  // ---------------- データ
  async function load(csv, name) {
    await ready();
    var bytes = csv instanceof Uint8Array ? csv : csv instanceof ArrayBuffer ? new Uint8Array(csv) : (typeof Blob !== 'undefined' && csv instanceof Blob) ? new Uint8Array(await csv.arrayBuffer()) : new TextEncoder().encode(String(csv));
    var nm = name || 'data.csv', r = await A.xlsxIn(bytes, nm);
    await A.loadCsv(r.buf, r.name, false);
    if (!S.data) fail('The data could not be loaded.');
    return { rows: S.data.nrow, columns: S.data.ncol };
  }
  // 列の設定：type（nominal / ordinal / multi / numeric / time / text）、label（表示名）、order（選択肢の並び）、scoring（配点）
  async function setColumn(ref, o) {
    var x = A.v(col(ref)); o = o || {};
    if (o.type && o.type !== x.type) { await A.changeVar(x, o.type, o.delim || x.delim); x = A.v(x.col); if (x.type !== o.type) fail('Could not change column ' + x.col + ' to type ' + o.type + '.'); }
    A.commit(function () {
      if (typeof o.label === 'string' && o.label.trim()) { x.label = o.label.trim(); x.labelSet = true; }
      if (Array.isArray(o.order) && x.cats) {
        var miss = o.order.filter(function (v) { return !x.cats.some(function (c) { return c.v === v; }); });
        if (miss.length) fail('Unknown options for column ' + x.col + ': ' + miss.join(', '));
        x.cats.sort(function (a, b) { var i = o.order.indexOf(a.v), j = o.order.indexOf(b.v); return (i < 0 ? 1e9 : i) - (j < 0 ? 1e9 : j); });
      }
      if (o.scoring) {
        var sc = o.scoring; if (typeof sc === 'string') sc = { mode: sc };
        if (['desc', 'asc', 'custom'].indexOf(sc.mode) < 0) fail('scoring.mode must be desc, asc or custom.');
        x.score = { mode: sc.mode, pts: sc.points || {} };
      }
    }, { quiet: true });
    A.syncResults();
    return { col: x.col, type: x.type, label: x.label };
  }
  // 無効回答（解析から除く）とフラッグ（目印）。rows は元のデータの行番号（1 から）
  function markRows(rows, kind, note) {
    var k = kind === 'flag' ? 'f' : 'x';
    rows = [].concat(rows).map(Number); rows.forEach(function (r) { if (!(r >= 1 && r <= S.data.nrow && r === Math.floor(r))) fail('Row ' + r + ' does not exist (rows are numbered 1 to ' + S.data.nrow + ').'); });
    A.commit(function () { var m = A.rowMarks(); rows.forEach(function (r) { var e = m[r] || {}; e[k] = 1; if (k === 'x' && note) e.note = String(note); m[r] = e; }); }, { quiet: true });
    A.syncResults(); return A.refreshCounts();
  }
  function unmarkRows(rows, kind) {
    var k = kind === 'flag' ? 'f' : 'x';
    A.commit(function () { var m = A.rowMarks(); [].concat(rows).forEach(function (r) { var e = m[+r]; if (!e) return; delete e[k]; if (k === 'x') delete e.note; if (!Object.keys(e).length) delete m[+r]; }); }, { quiet: true });
    A.syncResults(); return A.refreshCounts();
  }

  // ---------------- 解析
  // spec：{ type, slots: { 欄: 列 | [列, …] }, options: {…}, choices: { 欄: { keep, drop, rename, order } }, reverse: [列], name, figure: {…}, inReport }
  function addAnalysis(spec) {
    if (!S.data) fail('No data loaded. Call CTS.load() first.');
    var d = A.def(spec.type); if (!d) fail('Unknown analysis type "' + spec.type + '". Use CTS.analyses() to see the types.');
    var tab, before = JSON.stringify(S.P);
    try { A.commit(function () {
      tab = A.newTab(spec.type); if (!tab) fail('Too many analyses (the maximum is ' + A.MAX_TABS + ').');
      d.slots.forEach(function (sd) {
        var ref = (spec.slots || {})[sd.id];
        if (ref === undefined || ref === null) { if (!sd.optional) fail('Slot "' + sd.id + '" (' + sd.label + ') of ' + spec.type + ' needs a column.'); return; }
        if (sd.many) {
          var cs = [].concat(ref).map(col); cs.forEach(function (c) { var x = A.v(c); if (sd.types.indexOf(x.type) < 0) fail('Column ' + c + ' (' + x.label + ', ' + x.type + ') cannot be used in slot "' + sd.id + '" (allowed: ' + sd.types.join(', ') + ').'); });
          A.setItems(tab, sd.id, cs);
          if (spec.reverse) tab.slots[sd.id].reverse = [].concat(spec.reverse).map(col).filter(function (c) { return cs.indexOf(c) >= 0; });
        } else {
          var c = col(ref), x = A.v(c);
          if (sd.types.indexOf(x.type) < 0) fail('Column ' + c + ' (' + x.label + ', ' + x.type + ') cannot be used in slot "' + sd.id + '" (allowed: ' + sd.types.join(', ') + '). Change the column type with CTS.setColumn() if needed.');
          A.setSlot(tab, sd.id, c);
        }
        var ch = (spec.choices || {})[sd.id], sl = tab.slots[sd.id];
        if (ch && sd.many && sd.types.indexOf('numeric') >= 0) { xChoices(sd, sl, ch); return; }
        if (ch && sl.cats) {
          var has = function (v) { return sl.cats.some(function (k) { return k.v === v; }); }, chk = function (list, what) { (list || []).forEach(function (v) { if (!has(v)) fail('Option "' + v + '" (' + what + ') is not in slot "' + sd.id + '". Options: ' + sl.cats.map(function (k) { return k.v; }).join(' | ')); }); };
          chk(ch.keep, 'keep'); chk(ch.drop, 'drop'); chk(Object.keys(ch.rename || {}), 'rename');
          sl.cats.forEach(function (k) {
            if (ch.keep) k.on = ch.keep.indexOf(k.v) >= 0;
            if (ch.drop && ch.drop.indexOf(k.v) >= 0) k.on = false;
            if (ch.rename && Object.prototype.hasOwnProperty.call(ch.rename, k.v)) k.label = String(ch.rename[k.v]);
          });
          // order には、元の値と、名前を変えた後の名前（まとめた群の名前）のどちらでも書ける。群の名前は、その群の選択肢をまとめて動かす
          if (ch.order) {
            var bad = ch.order.filter(function (v) { return !sl.cats.some(function (k) { return k.v === v || k.label === v; }); });
            if (bad.length) fail('Option "' + bad[0] + '" (order) is not in slot "' + sd.id + '". Use original values or the names after rename. Options: ' + sl.cats.map(function (k) { return k.label === k.v ? k.v : k.v + ' → ' + k.label; }).join(' | '));
            var at = function (k) { var i = ch.order.indexOf(k.v); if (i < 0) i = ch.order.indexOf(k.label); return i < 0 ? 1e9 : i; };
            sl.cats = sl.cats.map(function (k, i) { return { k: k, i: i }; }).sort(function (a, b) { return (at(a.k) - at(b.k)) || (a.i - b.i); }).map(function (o) { return o.k; });
          }
        }
      });
      Object.keys(spec.options || {}).forEach(function (k) {
        var od = (d.opts || []).find(function (o) { return o.id === k; }); if (!od) fail('Unknown option "' + k + '" for ' + spec.type + '. Options: ' + (d.opts || []).map(function (o) { return o.id; }).join(', '));
        var val = spec.options[k];
        if (od.kind === 'select' && !od.choices.some(function (c) { return c.value === val; })) fail('Option "' + k + '" must be one of: ' + od.choices.map(function (c) { return c.value; }).join(', '));
        tab.opts[k] = od.kind === 'check' ? !!val : val;
      });
      if (spec.name) { tab.name = String(spec.name); tab.nameAuto = false; }
      if (spec.figure) Object.assign(tab.figs[0], spec.figure);
      if (spec.inReport === false) tab.checked = false;
      if (spec.holmGroup !== undefined && spec.holmGroup !== null && String(spec.holmGroup).trim()) tab.holmGroup = String(spec.holmGroup).trim();
    }, { quiet: true }); } catch (e) { S.P = JSON.parse(before); S.undo.pop(); A.render(); throw e; }   // 指定に誤りがあれば、作りかけの解析を残さない
    A.schedule(tab.id);
    return tab.id;
  }
  // 回帰分析の説明変数（列ごとに回答をそのまま使う欄）の choices。
  //   { drop: [値], keep: [値] }：その値を持つ列すべてに当てる。{ byColumn: { 列: { drop, keep } } }：列ごとに当てる。
  //   除いた選択肢を選んだ人は推定から外す。rename と order はこの欄では使えない（列ごとの回答をそのまま使うため）
  function xChoices(sd, sl, ch) {
    ['rename', 'order'].forEach(function (k) { if (ch[k]) fail('choices.' + sd.id + '.' + k + ' is not supported for slot "' + sd.id + '" (each column is used with its own options). Use drop / keep, or byColumn: { column: { drop, keep } }.'); });
    var cats = sl.cols.map(A.v).filter(function (x) { return x && A.isCat(x) && x.type !== 'multi'; });
    function vals(x) { return x.cats.filter(function (k) { return !k.special; }).map(function (k) { return k.v; }); }
    function apply(x, o, strict) {
      var vs = vals(x), drop = [];
      ['keep', 'drop'].forEach(function (k) { (o[k] || []).forEach(function (v) { if (strict && vs.indexOf(v) < 0) fail('Option "' + v + '" (' + k + ') is not in column ' + x.col + ' (' + x.label + '). Options: ' + vs.join(' | ')); }); });
      if (o.keep) drop = vs.filter(function (v) { return o.keep.indexOf(v) < 0; });
      (o.drop || []).forEach(function (v) { if (vs.indexOf(v) >= 0 && drop.indexOf(v) < 0) drop.push(v); });
      if (drop.length >= vs.length) fail('All options of column ' + x.col + ' (' + x.label + ') would be excluded.');
      sl.drop = sl.drop || {}; if (drop.length) sl.drop[x.col] = drop; else delete sl.drop[x.col];
    }
    if (ch.keep || ch.drop) {
      [].concat(ch.keep || [], ch.drop || []).forEach(function (v) { if (!cats.some(function (x) { return vals(x).indexOf(v) >= 0; })) fail('Option "' + v + '" is not in any column of slot "' + sd.id + '".'); });
      cats.forEach(function (x) {
        var vs = vals(x), o = { drop: (ch.drop || []).filter(function (v) { return vs.indexOf(v) >= 0; }) };
        if (ch.keep && ch.keep.some(function (v) { return vs.indexOf(v) >= 0; })) o.keep = ch.keep.filter(function (v) { return vs.indexOf(v) >= 0; });
        if (o.keep || o.drop.length) apply(x, o, false);
      });
    }
    Object.keys(ch.byColumn || {}).forEach(function (ref) {
      var c = col(/^\d+$/.test(ref) ? +ref : ref), x = A.v(c);
      if (sl.cols.indexOf(c) < 0) fail('Column ' + c + ' is not in slot "' + sd.id + '".');
      if (!A.isCat(x) || x.type === 'multi') fail('Column ' + c + ' (' + x.label + ') has no options to drop (type ' + x.type + ').');
      apply(x, ch.byColumn[ref], true);
    });
    var other = Object.keys(ch).filter(function (k) { return ['keep', 'drop', 'byColumn'].indexOf(k) < 0; });
    if (other.length) fail('Unknown field "' + other[0] + '" in choices.' + sd.id + '. Use drop, keep or byColumn.');
    if (sl.drop && !Object.keys(sl.drop).length) delete sl.drop;
  }
  function removeAnalysis(id) { A.commit(function () { S.P.tabs = S.P.tabs.filter(function (t) { return t.id !== id; }); }, { quiet: true }); A.syncResults(); }

  // すべての解析の計算が済むまで待つ
  async function wait(timeoutMs) {
    var t0 = Date.now(), lim = timeoutMs || 300000;
    A.syncResults();
    while (true) {
      var pending = S.P.tabs.filter(function (t) { if (!A.tabReady(t)) return false; var r = S.results[t.id]; return !r || r.sig0 !== JSON.stringify(A.buildReq(t)); });
      if (!pending.length) break;
      if (Date.now() - t0 > lim) fail('Timed out waiting for ' + pending.length + ' analyses.');
      await sleep(150);
    }
    if (A.ensureHolmAsync) { try { await A.ensureHolmAsync(); } catch (e) { /* 報告書の多重比較の補正は、作れなくても結果は返す */ } }
    return results();
  }
  // 結果。pAdjusted：報告書の検定の一覧と同じホルム法の補正後の p（holmGroup：補正の族。空は報告書全体）。
  //   tables：報告書・画面に出る表（文字の表。数値は表示の桁）。data：表のもとの数値（xtab：クロス集計表、grids：統計量の表）
  function results() {
    var hm = (S.holm && S.holm.map) || {};
    return S.P.tabs.map(function (t) {
      var r = S.results[t.id] || null, ok = !!(r && r.ok);
      return { id: t.id, type: t.type, name: t.name, ready: A.tabReady(t), ok: ok, message: r && !r.ok ? r.message : undefined,
        test: r && r.test, p: r && r.p, pAdjusted: hm[t.id] !== undefined ? hm[t.id] : null, holmGroup: A.holmGroup(t), inReport: !!t.checked,
        significant: ok ? r.sig : undefined, n: r && r.n,
        headline: r && r.headline ? r.headline.map(function (h) { return { label: h.k, value: h.v }; }) : [], report: ok ? (t.reportText || r.report) : undefined,
        notes: ok ? (r.notes || []) : undefined,
        tables: ok ? A.tablesOf(t, r).map(function (tb) { return { caption: tb.caption, rows: tb.rows, note: tb.note || '' }; }) : undefined,
        data: ok ? JSON.parse(JSON.stringify({ xtab: r.xtab || null, grids: r.grids || [] })) : undefined };
    });
  }
  // 図を 1 つ返す。format：'png'（既定）か 'svg'。PNG の大きさは scale（倍率）、dpi（画面の 96 dpi に対する解像度）、
  //   または width（印刷の幅 mm）と dpi で決める。base64 と、図の幅・高さ（画面の px）を返す
  async function figure(id, k, o) {
    o = o || {}; k = k || 0;
    var t = A.tabById(id); if (!t) fail('Analysis "' + id + '" does not exist.');
    await wait();
    var r = S.results[t.id]; if (!r || !r.ok) fail('Analysis "' + id + '" has no result' + (r && r.message ? ': ' + r.message : '.'));
    if (!(k >= 0 && k < t.figs.length)) fail('Analysis "' + id + '" has ' + t.figs.length + ' figure(s); k must be 0 to ' + (t.figs.length - 1) + '.');
    var m = Chart.model(r); if (!m) fail('Analysis "' + id + '" has no figure.');
    var fg = Chart.render(m, t.figs[k], S.P.settings.figLang), fmt = o.format || 'png';
    if (fmt === 'svg') return { format: 'svg', mime: 'image/svg+xml', width: fg.w, height: fg.h, kind: fg.kind, base64: await toBase64('<?xml version="1.0" encoding="UTF-8"?>\n' + fg.svg) };
    if (fmt !== 'png') fail('format must be "png" or "svg".');
    var scale = +o.scale || 0, dpi = +o.dpi || 0;
    if (o.width && dpi) scale = (+o.width / 25.4 * dpi) / fg.w; else if (dpi && !scale) scale = dpi / 96;
    if (!scale) scale = S.P.settings.pngScale || 2;
    if (!(scale > 0 && scale <= 20)) fail('The scale must be between 0 and 20.');
    var png = await Chart.toPng(fg.svg, scale, dpi || 96 * scale);
    return { format: 'png', mime: 'image/png', width: fg.w, height: fg.h, kind: fg.kind, scale: scale, dpi: dpi || 96 * scale, base64: await toBase64(png) };
  }

  // ---------------- 書き出し
  function prj() { if (!S.data) fail('No data loaded.'); return A.projectJson(); }
  async function pdf() { if (!S.data) fail('No data loaded.'); await wait(); return A.reportPdf(); }
  async function docx() { if (!S.data) fail('No data loaded.'); await wait(); return A.reportDocx(); }
  function reportText() { return A.reportPlain(); }
  async function toBase64(blob) {
    var u = blob instanceof Uint8Array ? blob : new Uint8Array(await (typeof blob === 'string' ? new Blob([blob]) : blob).arrayBuffer()), s = '';
    for (var i = 0; i < u.length; i += 32768) s += String.fromCharCode.apply(null, u.subarray(i, i + 32768));
    return btoa(s);
  }
  // ブラウザの「ダウンロード」として保存する（自動操作のブラウザでは、ダウンロードの受け取りを有効にしておく）
  function download(data, name) {
    var blob = data instanceof Blob ? data : new Blob([data], { type: 'application/octet-stream' }), a = document.createElement('a');
    a.href = URL.createObjectURL(blob); a.download = name; document.body.appendChild(a); a.click();
    setTimeout(function () { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
  }

  // ---------------- まとめて実行
  // spec：{ csv, name, title, lang, columns: [{ column, type, label, order, scoring }], invalidRows: [行] | [{ row, note }], flagRows: [行],
  //         analyses: [解析の指定], output: { prj, pdf, docx, base64, download } }
  async function run(spec) {
    spec = spec || {};
    await ready();
    if (spec.csv !== undefined) await load(spec.csv, spec.name);
    if (!S.data) fail('No data. Pass csv in the spec or call CTS.load() first.');
    if (spec.title) A.commit(function () { S.P.title = String(spec.title); }, { quiet: true });
    for (var i = 0; i < (spec.columns || []).length; i++) { var c = spec.columns[i]; await setColumn(c.column, c); }
    (spec.invalidRows || []).forEach(function (r) { if (typeof r === 'object') markRows([r.row], 'invalid', r.note); else markRows([r], 'invalid'); });
    if (spec.flagRows && spec.flagRows.length) markRows(spec.flagRows, 'flag');
    if ((spec.invalidRows || []).length) await A.refreshCounts();
    var ids = (spec.analyses || []).map(addAnalysis);
    var res = await wait(spec.timeoutMs);
    var out = { ok: res.every(function (r) { return r.ok || !r.ready; }), version: appVersion(), api: API_VERSION, rows: S.data.nrow, invalid: A.invalidRows().length, analyses: ids, results: res };
    var o = spec.output || { prj: true, pdf: true };
    var base = (spec.title || A.baseName()).replace(/[\\/:*?"<>|]/g, '_');
    if (o.prj) out.prj = prj();
    if (o.pdf) out.pdf = await pdf();
    if (o.docx) out.docx = await docx();
    if (o.figures) { out.figures = []; for (var fi = 0; fi < ids.length; fi++) { var ft = A.tabById(ids[fi]), fr = S.results[ids[fi]]; if (!ft || !fr || !fr.ok) continue; for (var fk = 0; fk < ft.figs.length; fk++) { var fo = typeof o.figures === 'object' ? o.figures : {}, f1 = await figure(ids[fi], fk, fo); f1.id = ids[fi]; f1.k = fk; out.figures.push(f1); } } }
    if (o.base64) { if (out.prj) out.prjBase64 = await toBase64(out.prj); if (out.pdf) out.pdfBase64 = await toBase64(out.pdf); if (out.docx) out.docxBase64 = await toBase64(out.docx); }
    if (o.download) { if (out.prj) download(out.prj, base + '.prj'); if (out.pdf) download(out.pdf, base + '_report.pdf'); if (out.docx) download(out.docx, base + '_report.docx'); }
    A.render();
    return out;
  }

  var api = { apiVersion: API_VERSION, version: appVersion, ready: ready, analyses: analyses, columns: columns, load: load, setColumn: setColumn,
    markInvalid: function (rows, note) { return markRows(rows, 'invalid', note); }, markFlag: function (rows) { return markRows(rows, 'flag'); },
    unmarkInvalid: function (rows) { return unmarkRows(rows, 'invalid'); }, unmarkFlag: function (rows) { return unmarkRows(rows, 'flag'); },
    addAnalysis: addAnalysis, removeAnalysis: removeAnalysis, wait: wait, results: results, figure: figure,
    prj: prj, pdf: pdf, docx: docx, reportText: reportText, toBase64: toBase64, download: download, run: run };
  // どの関数を呼んでも、案内のダイアログを閉じる（以後は出さない）
  Object.keys(api).forEach(function (k) { var f = api[k]; if (typeof f === 'function') api[k] = function () { quiet(); return f.apply(null, arguments); }; });
  window.CTS = api;
})();
