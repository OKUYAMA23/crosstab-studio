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
  async function ready(timeoutMs) {
    var t0 = Date.now(), lim = timeoutMs || 180000;
    while (!(S.registry && S.registry.length) || document.getElementById('md_boot')) {
      if (Date.now() - t0 > lim) fail('CrossTab Studio did not finish starting within ' + Math.round(lim / 1000) + ' s.');
      await sleep(200);
    }
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
      if (typeof o.label === 'string' && o.label.trim()) x.label = o.label.trim();
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
        if (ch && sl.cats) {
          var has = function (v) { return sl.cats.some(function (k) { return k.v === v; }); }, chk = function (list, what) { (list || []).forEach(function (v) { if (!has(v)) fail('Option "' + v + '" (' + what + ') is not in slot "' + sd.id + '". Options: ' + sl.cats.map(function (k) { return k.v; }).join(' | ')); }); };
          chk(ch.keep, 'keep'); chk(ch.drop, 'drop'); chk(Object.keys(ch.rename || {}), 'rename'); chk(ch.order, 'order');
          sl.cats.forEach(function (k) {
            if (ch.keep) k.on = ch.keep.indexOf(k.v) >= 0;
            if (ch.drop && ch.drop.indexOf(k.v) >= 0) k.on = false;
            if (ch.rename && Object.prototype.hasOwnProperty.call(ch.rename, k.v)) k.label = String(ch.rename[k.v]);
          });
          if (ch.order) sl.cats.sort(function (a, b) { var i = ch.order.indexOf(a.v), j = ch.order.indexOf(b.v); return (i < 0 ? 1e9 : i) - (j < 0 ? 1e9 : j); });
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
    }, { quiet: true }); } catch (e) { S.P = JSON.parse(before); S.undo.pop(); A.render(); throw e; }   // 指定に誤りがあれば、作りかけの解析を残さない
    A.schedule(tab.id);
    return tab.id;
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
  function results() {
    return S.P.tabs.map(function (t) {
      var r = S.results[t.id] || null;
      return { id: t.id, type: t.type, name: t.name, ready: A.tabReady(t), ok: !!(r && r.ok), message: r && !r.ok ? r.message : undefined,
        test: r && r.test, p: r && r.p, significant: r && r.ok ? r.sig : undefined, n: r && r.n,
        headline: r && r.headline ? r.headline.map(function (h) { return { label: h.k, value: h.v }; }) : [], report: r && r.ok ? (t.reportText || r.report) : undefined };
    });
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
    if (o.base64) { if (out.prj) out.prjBase64 = await toBase64(out.prj); if (out.pdf) out.pdfBase64 = await toBase64(out.pdf); if (out.docx) out.docxBase64 = await toBase64(out.docx); }
    if (o.download) { if (out.prj) download(out.prj, base + '.prj'); if (out.pdf) download(out.pdf, base + '_report.pdf'); if (out.docx) download(out.docx, base + '_report.docx'); }
    A.render();
    return out;
  }

  window.CTS = { apiVersion: API_VERSION, version: appVersion, ready: ready, analyses: analyses, columns: columns, load: load, setColumn: setColumn,
    markInvalid: function (rows, note) { return markRows(rows, 'invalid', note); }, markFlag: function (rows) { return markRows(rows, 'flag'); },
    unmarkInvalid: function (rows) { return unmarkRows(rows, 'invalid'); }, unmarkFlag: function (rows) { return unmarkRows(rows, 'flag'); },
    addAnalysis: addAnalysis, removeAnalysis: removeAnalysis, wait: wait, results: results,
    prj: prj, pdf: pdf, docx: docx, reportText: reportText, toBase64: toBase64, download: download, run: run };
})();
