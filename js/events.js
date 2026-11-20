// events.js ― 操作（クリック・入力・ドラッグ・キー）と、小さな画面（一覧・色・メニュー・書き出し）
(function () {
  'use strict';
  var A = window.App, S = A.S, esc = A.esc, $ = A.$;
  function resOf(tab) { return tab ? S.results[tab.id] : null; }
  function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }
  // 画面の拡大率。図は、スマホ版で画面の幅に収めるために縮めて見せることがある（その倍率も掛ける）
  function zoom(el) { var z = (S.P.settings.zoom || 100) / 100, w = el && el.closest && el.closest('.md-figwrap'); if (w && w.dataset.fit) z *= +w.dataset.fit; return z; }
  var LIMITS = { fsAxis: [6, 60], fsTick: [6, 60], fsLegend: [6, 60], fsLabel: [6, 60], lwAxis: [0, 8], lwBar: [0, 8], lwGrid: [0, 8], w: [160, 2400], h: [120, 2400], barW: [10, 100], tickLen: [0, 20], labelDec: [0, 2], fs: [6, 40], lw: [0, 6] };
  var FS_STEPS = [8, 9, 10, 10.5, 11, 12, 14, 16, 18, 20, 24, 28, 32];
  var FIG_STYLE_KEYS = ['fontJa', 'fontEn', 'fsAxis', 'fsTick', 'fsLegend', 'fsLabel', 'lwAxis', 'lwBar', 'lwGrid', 'labels', 'labelDec', 'showN', 'legend', 'legFrame', 'legShape', 'legTitle', 'tickDir', 'tickLen', 'grid', 'gridColor', 'frame', 'transparent', 'palette', 'colorMode', 'barW', 'bar', 'barShape', 'legBg', 'labelWhat'];
  var T_PRESET = { grid: { headFill: '#F2F2F2', headInk: '#000000', lc: '#7F7F7F' }, three: { headFill: null, headInk: '#000000', lc: '#000000' }, stripe: { headFill: null, headInk: '#000000', lc: '#000000', stripe: '#F3F3F3' }, plain: { headFill: null, headInk: '#000000', lc: '#000000' }, head: { headFill: '#217346', headInk: '#FFFFFF', lc: '#BFBFBF' } };

  // ---------------- 値を変える（リボンと作業ウィンドウの共通の入口）
  function parseVal(el) {
    var t = el.dataset.type, val = el.value;
    if (el.type === 'checkbox') return el.checked;
    if (t === 'num') { var n = parseFloat(val); return isFinite(n) ? n : undefined; }
    if (t === 'numnull') { if (val.trim() === '' || val.trim() === '自動' || val.trim().toLowerCase() === 'auto') return null; var m = parseFloat(val); return isFinite(m) ? m : undefined; }
    if (t === 'boolstr') return val === 'true';
    if (t === 'strnull') return val === '' ? null : val;
    return val;
  }
  function setPath(path, val, o) {
    if (val === undefined) { A.render(); return; }
    var i = path.indexOf('.'), sc = path.slice(0, i), k = path.slice(i + 1), tab = A.curTab(), run = null, target;
    if (sc === 'ui') { S.ui[k] = val; A.render(); return; }
    if ((sc === 'fig' || sc === 'tbl' || sc === 'tst' || sc === 'opt' || sc === 'tab') && !tab) return;
    if (LIMITS[k] && typeof val === 'number') val = clamp(val, LIMITS[k][0], LIMITS[k][1]);
    if (sc === 'fig') { target = A.curFig(); if (!target) return; } else if (sc === 'tbl') target = tab.table; else if (sc === 'tst') target = tab.tstyle; else if (sc === 'opt') { target = tab.opts; run = tab.id; }
    else if (sc === 'tab') target = tab; else if (sc === 'set') { target = S.P.settings; if (k === 'alpha' || k === 'lead0') run = 'all'; }
    else if (sc === 'rep') target = S.P.report; else if (sc === 'proj') target = S.P; else return;
    A.commit(function () {
      if (path === 'fig.fsAll') { val = clamp(val, 6, 60); target.fsTick = val; target.fsAxis = val; target.fsLegend = val; target.fsLabel = Math.max(6, val - 1); }
      else target[k] = val;
      if (path === 'tab.name') tab.nameAuto = false;
      // 「割合」「人数」のボタンを押したら、値のラベルもそれに合わせる（ラベルを「割合だけ」「人数だけ」に固定していると、円グラフでは見た目が変わらないため）
      if (path === 'fig.value' && (target.labelWhat === 'pct' || target.labelWhat === 'n')) target.labelWhat = 'value';
      if (sc === 'fig' && (k === 'w' || k === 'h')) target.fit = false;                // 大きさを自分で決めたら、自動で広げない
    }, Object.assign({ run: run, key: path }, o || {}));
    if (path === 'set.zoom') A.applyZoom();
  }

  // ---------------- 選ぶ（画面を作り直さずに、選択の表示だけ変える）
  function selectTab(id, figK, rtab) {
    var changed = S.ui.sel !== id || (figK !== undefined && figK !== null && S.ui.selFig !== figK) || (rtab && S.ui.rtab !== rtab);
    if (S.ui.sel !== id) S.ui.selFig = 0;
    S.ui.sel = id; if (figK !== undefined && figK !== null) S.ui.selFig = figK; if (rtab) S.ui.rtab = rtab;
    if (!changed) return;
    document.querySelectorAll('.md-an').forEach(function (s) { s.classList.toggle('sel', s.dataset.an === id); });
    document.querySelectorAll('.md-figwrap').forEach(function (w) { var t = A.tabById(id); w.classList.toggle('sel', w.dataset.fig === id + ':' + S.ui.selFig && (S.ui.rtab === 'figure' || (t && t.figs.length > 1))); });
    A.render('chrome'); A.renderPane();
  }
  function scrollToAn(id) { setTimeout(function () { if (A.stopKeep) A.stopKeep(); var el = document.querySelector('.md-an[data-an="' + id + '"]'); if (el) el.scrollIntoView({ block: 'nearest', behavior: 'smooth' }); }, 30); }

  // ---------------- 小さな画面
  function closePop() { document.querySelectorAll('.md-menu, .md-colorpop, .md-palpop').forEach(function (e) { e.remove(); }); }
  function place(d, x, y) { var r = d.getBoundingClientRect(); d.style.left = Math.max(4, Math.min(x, window.innerWidth - r.width - 6)) + 'px'; d.style.top = Math.max(4, Math.min(y, window.innerHeight - r.height - 6)) + 'px'; }
  function menu(x, y, items) {
    closePop();
    var d = document.createElement('div'); d.className = 'md-menu';
    d.innerHTML = items.map(function (it, i) { return it === '-' ? '<hr>' : '<button data-i="' + i + '" class="' + (it.danger ? 'danger' : '') + '"' + (it.off ? ' disabled' : '') + '>' + esc(it.name) + (it.key ? '<kbd>' + it.key + '</kbd>' : '') + '</button>'; }).join('');
    document.body.appendChild(d); place(d, x, y);
    d.addEventListener('click', function (e) { var b = e.target.closest('button'); if (!b) return; e.stopPropagation(); var it = items[+b.dataset.i]; closePop(); it.run(); });
  }
  function modal(title, body, foot, wide) {
    closeModal();
    var d = document.createElement('div'); d.className = 'md-modal-back'; d.id = 'md_modal';
    d.innerHTML = '<div class="md-modal"' + (wide ? ' style="width:' + wide + 'px"' : '') + '><div class="md-modal-head">' + esc(title) + _t('<button data-act="closeModal" data-tip="閉じる（Esc）">×</button></div><div class="md-modal-body">') + body + '</div>' + (foot ? '<div class="md-modal-foot">' + foot + '</div>' : '') + '</div>';
    document.body.appendChild(d);
    d.addEventListener('mousedown', function (e) { if (e.target === d) closeModal(); });
    return d;
  }
  function closeModal() { var m = $('#md_modal'); if (m) m.remove(); PD = null; }
  function analysisDialog(onPick, title) {
    // スマホでは検索欄を出さない（一覧から選ぶ方が速く、入力欄に触れると画面が拡大されるため）
    var useSearch = !window.CTS_PHONE;
    modal(title, _t('<button class="md-btn" data-act="guide" style="margin-bottom:8px">どれを使えばよいか迷ったら：質問を選んで探す</button>') + (useSearch ? _t('<input class="md-in" id="md_asearch" placeholder="解析の名前や目的で探す（例：フィッシャー、複数回答、順序）" style="margin-bottom:6px">') : '') + '<div id="md_alist"></div>');
    function draw(q) {
      var groups = [], h = '';
      S.registry.forEach(function (a) { if (q && (a.name + a.desc + a.group).indexOf(q) < 0) return; var g = groups.find(function (x) { return x.name === a.group; }); if (!g) { g = { name: a.group, items: [] }; groups.push(g); } g.items.push(a); });
      groups.forEach(function (g) { h += '<div class="md-alist-group">' + esc(g.name) + '</div>' + g.items.map(function (a) { return '<button class="md-aitem" data-aid="' + a.id + '"><b>' + esc(a.name) + '</b><span>' + esc(a.desc) + '</span></button>'; }).join(''); });
      $('#md_alist').innerHTML = h || _t('<div class="md-empty">当てはまる解析がありません。別のことばで探してください。</div>');
    }
    draw('');
    if (useSearch) $('#md_asearch').addEventListener('input', function (e) { draw(e.target.value.trim()); });
    $('#md_alist').addEventListener('click', function (e) { var b = e.target.closest('[data-aid]'); if (b) { closeModal(); onPick(b.dataset.aid); } });
    if (useSearch && !window.CTS_TOUCH) $('#md_asearch').focus();   // 指の端末では、キーボードが勝手に出ないように自動で選ばない
  }
  A.askReplace = function (buf, name) {
    modal(_t('新しいデータの読み込み'), _t('<p>解析が {0} 個あります。「{1}」をどのように読み込みますか。</p><p class="md-hint">回答が増えた同じアンケートなら「解析を残して差し替える」を選ぶと、同じ質問の列に解析の設定を引き継いで、すべて計算し直します。</p>', S.P.tabs.length, esc(name)),
      _t('<button class="md-btn" data-act="closeModal">やめる</button><button class="md-btn" id="md_rep_new">新しく始める</button><button class="md-btn md-btn-primary" id="md_rep_keep">解析を残して差し替える</button>'));
    $('#md_rep_new').onclick = function () { closeModal(); A.loadCsv(buf, name, false).catch(function (e) { A.toast(e.message, true); }); };
    $('#md_rep_keep').onclick = function () { closeModal(); A.loadCsv(buf, name, true).catch(function (e) { A.toast(e.message, true); }); };
  };
  function askName(title, def, onOk) {
    modal(title, _t('<label class="md-field"><span>プロジェクトの名前</span><input type="text" id="md_pname" value="{0}"></label><p class="md-hint">データ（CSV の中身）・解析・図と表の設定をまとめて、この端末（ブラウザの中）に保存します。同じ名前があるときは上書きします。</p>', esc(def)),
      _t('<button class="md-btn" data-act="closeModal">やめる</button><button class="md-btn md-btn-primary" id="md_pname_ok">保存</button>'), 480);
    var inp = $('#md_pname'); inp.focus(); inp.select();
    function go() { var v = inp.value.trim(); if (!v) return; closeModal(); onOk(v); }
    $('#md_pname_ok').onclick = go; inp.addEventListener('keydown', function (e) { if (e.key === 'Enter') { e.stopPropagation(); go(); } });
  }
  async function refreshProjects() { S.projList = await A.listProjects(); if (!S.data) A.renderCenter(); }
  async function projectDialog() {
    var d = await A.listProjects(); S.projList = d;
    var rows = d.items.length ? d.items.map(function (p) { return '<div class="md-projrow"><button class="md-aitem" data-pname="' + esc(p.name) + '"><b>' + esc(p.name) + _t('</b><span>保存：{0}　{1} KB</span></button><button class="md-btn md-btn-sm md-danger" data-pdel="{2}" data-tip="このプロジェクトを削除します。">削除</button></div>', esc(p.saved), Math.max(1, Math.round(p.size / 1024)), esc(p.name)); }).join('')
      : _t('<div class="md-empty">保存したプロジェクトはまだありません。「上書き保存」で保存すると、ここに並びます。</div>');
    var m = modal(_t('プロジェクトを開く'), rows + (d.folder ? _t('<p class="md-hint" style="margin-top:10px">保存先：') + esc(d.folder) + '</p>' : ''), _t('<button class="md-btn" id="md_pfile">ファイルから開く…</button><button class="md-btn" data-act="closeModal">閉じる</button>'), 560);
    m.addEventListener('click', async function (e) {
      var b = e.target.closest('[data-pname]'), del = e.target.closest('[data-pdel]');
      if (b) { closeModal(); A.openProjectByName(b.dataset.pname); }
      else if (del) {
        if (del.dataset.sure) { await A.api('/api/proj/delete', { name: del.dataset.pdel }); A.toast(_t('プロジェクト「{0}」を削除しました。', del.dataset.pdel)); projectDialog(); }
        else { del.dataset.sure = '1'; del.textContent = _t('本当に削除'); }
      }
    });
    // スマホ・タブレットは、知らない拡張子のファイルを選べなくすることがあるので、種類を絞らない
    $('#md_pfile').onclick = async function () { var f = await A.pickFile(window.CTS_TOUCH ? '' : '.prj,.ctsproj,.json'); if (f) { closeModal(); A.handleFile(f); } };
  }
  function shade(hex, t) { return t >= 0 ? Chart.lerp(hex, '#FFFFFF', t) : Chart.lerp(hex, '#000000', -t); }
  function colorPop(x, y, onPick, autoLabel, extra) {
    closePop();
    var theme = ['#FFFFFF', '#000000', '#E7E6E6', '#3E4A5B', '#4E79A7', '#F28E2B', '#E15759', '#76B7B2', '#59A14F', '#B07AA1'];   // Tableau 10 を元にした、彩度を抑えた色
    var rows = [0, 1, 2, 3, 4, 5].map(function (r) {
      return theme.map(function (c) { if (r === 0) return c; if (c === '#FFFFFF') return shade(c, -[0, 0.05, 0.15, 0.25, 0.35, 0.5][r]); if (c === '#000000') return shade(c, [0, 0.5, 0.35, 0.25, 0.15, 0.05][r]); return shade(c, [0, 0.8, 0.6, 0.4, -0.25, -0.5][r]); });
    });
    var grays = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].map(function (i) { return Chart.lerp('#F2F2F2', '#0D0D0D', i / 9); });
    var std = ['#E64B35', '#4DBBD5', '#00A087', '#3C5488', '#F39B7F', '#8491B4', '#91D1C2', '#DC0000', '#7E6148', '#B09C85'];   // 論文誌でよく使われる色（ggsci の npg）
    var cud = ['#0072B2', '#E69F00', '#009E73', '#CC79A7', '#56B4E9', '#D55E00', '#F0E442', '#999999'];
    function g(list) { return '<div class="md-cp-g">' + list.map(function (c) { return '<button style="background:' + c + '" data-c="' + c + '" data-tip="' + c + '"></button>'; }).join('') + '</div>'; }
    var d = document.createElement('div'); d.className = 'md-colorpop';
    d.innerHTML = (autoLabel ? '<button class="md-btn md-btn-sm" data-c="" style="width:100%">' + esc(autoLabel) + '</button>' : '') +
      (extra || []).map(function (x) { return '<button class="md-btn md-btn-sm" data-c="' + esc(x[0]) + '" data-raw="1" style="width:100%;margin-bottom:3px">' + esc(x[1]) + '</button>'; }).join('') + _t('<div class="md-cp-t">テーマの色</div>{0}<div class="md-cp-t">白〜黒</div>{1}<div class="md-cp-t">論文誌風の色</div>{2}<div class="md-cp-t">色覚に配慮した色</div>{3}<label class="md-check" style="margin-top:6px">その他の色… <input type="color" id="md_cp_other" style="width:44px;height:24px;padding:0;border:1px solid var(--t-c6e0cf)"></label>', g([].concat.apply([], rows)), g(grays), g(std), g(cud));
    document.body.appendChild(d); place(d, x, y);
    d.addEventListener('click', function (e) { var b = e.target.closest('[data-c]'); if (b) { e.stopPropagation(); closePop(); onPick(b.dataset.c || null); } });
    $('#md_cp_other').addEventListener('change', function (e) { closePop(); onPick(e.target.value.toUpperCase()); });
  }
  // 凡例に出す系列の名前を変える（空にすると元の名前に戻す）。色と同じく、元の名前にひも付けて保つ
  function renameSeries(i, val) {
    var tab = A.curTab(), fig = A.curFig(), m = Chart.model(resOf(tab)); if (!tab || !fig || !m) return;
    var lab = Chart.seriesFor(m, fig)[i]; if (lab === undefined) return;
    val = String(val || '').trim();
    var cur = (fig.legNames || {})[lab] || lab; if ((val || lab) === cur) { A.render(); return; }
    A.commit(function () { fig.legNames = Object.assign({}, fig.legNames); if (val && val !== lab) fig.legNames[lab] = val; else delete fig.legNames[lab]; });
  }
  function pickSeriesColor(i, x, y) {
    var tab = A.curTab(), fig = A.curFig(), m = Chart.model(resOf(tab)); if (!tab || !fig || !m) return;
    var ser = Chart.colorKeys(m, fig); if (!ser[i]) return;
    var lab = ser[i];
    // 色を選んだら「系列ごとに選ぶ」にする
    colorPop(x, y, function (c) { A.commit(function () { fig.colors = Object.assign({}, fig.colors); fig.colorMode = 'series'; if (c) fig.colors[lab] = c; else delete fig.colors[lab]; }); }, _t('配色の色に戻す'));
  }

  // ---------------- 書き出しの部品
  async function copyText(text, html) {
    try {
      if (html && window.ClipboardItem) await navigator.clipboard.write([new ClipboardItem({ 'text/plain': new Blob([text], { type: 'text/plain' }), 'text/html': new Blob([html], { type: 'text/html' }) })]);
      else await navigator.clipboard.writeText(text);
      return true;
    } catch (e) { var ta = document.createElement('textarea'); ta.value = text; document.body.appendChild(ta); ta.select(); var ok = false; try { ok = document.execCommand('copy'); } catch (e2) {} ta.remove(); return ok; }
  }
  function figAt(tab, k) { var f = A.figsOf(tab, resOf(tab)); return f[k] || null; }
  function selFigObj() { var tab = A.curTab(); return tab ? figAt(tab, Math.min(S.ui.selFig, tab.figs.length - 1)) : null; }
  function csvOf(tables) { return '﻿' + tables.map(function (t) { return [[t.caption]].concat(t.rows, t.note ? [[t.note]] : []).map(function (r) { return r.map(function (c) { c = String(c); return /[",\n]/.test(c) ? '"' + c.replace(/"/g, '""') + '"' : c; }).join(','); }).join('\r\n'); }).join('\r\n\r\n'); }
  function pngBlob(fg) { return Chart.toPng(fg.r.svg, S.P.settings.pngScale || 2); }
  async function savePng(tab, k) {
    var fg = figAt(tab, k); if (!fg) return;
    try { if (await A.saveBlob(await pngBlob(fg), A.safeName(tab.name) + _t('_図') + (k + 1) + '.png', _t('PNG 画像'), '.png')) A.toast(_t('図を画像で保存しました（画面と同じ {0} × {1} の形{2}）。', fg.r.w, fg.r.h, fg.st.transparent ? _t('、背景は透明') : '')); }
    catch (e) { A.toast(e.message, true); }
  }
  async function copyPng(tab, k) {
    var fg = figAt(tab, k); if (!fg) return;
    try { await navigator.clipboard.write([new ClipboardItem({ 'image/png': await pngBlob(fg) })]); A.toast(_t('図をコピーしました。Word や PowerPoint に貼り付けられます。')); }
    catch (e) { A.toast(_t('図をコピーできませんでした。「画像で保存」を使ってください。'), true); }
  }
  async function copyTables(tab) { var ts = A.tablesOf(tab, resOf(tab)); if (!ts.length) return; if (await copyText(ts.map(A.tableTsv).join('\n\n'), ts.map(function (t) { return A.tableHtml(t, tab.tstyle); }).join('<br>'))) A.toast(_t('表をコピーしました。Excel や Word に貼り付けられます。')); else A.toast(_t('コピーできませんでした。「CSV で保存」を使ってください。'), true); }
  async function saveTablesCsv(tab) { var ts = A.tablesOf(tab, resOf(tab)); if (!ts.length) return; if (await A.saveBlob(new Blob([csvOf(ts)], { type: 'text/csv' }), A.safeName(tab.name) + _t('_表.csv'), 'CSV', '.csv')) A.toast(_t('表を CSV で保存しました。')); }
  // 表を画像にする（画面と同じデザインの決まりで SVG に描いてから PNG にする）
  function tablesPng(tab) { var svg = A.tablesSvg(tab, resOf(tab)); if (!svg) return Promise.reject(new Error(_t('表がありません。'))); return Chart.toPng(svg, S.P.settings.pngScale || 2); }
  async function excelBlob(tabs) {
    var items = tabs.map(function (t) { return { name: t.name, req: A.buildReq(t) }; });
    var d = await A.apiData('/api/excel', { items: items });
    if (!d.ok) throw new Error(d.message || _t('Excel ファイルを作れませんでした。'));
    var enc = new TextEncoder(), files = Object.keys(d.files).map(function (k) { return { name: k, data: enc.encode(d.files[k]) }; });
    return { blob: await A.zip(files, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'), notes: d.notReproduced || [], skipped: d.skipped || [] };
  }
  // R のコード：アプリの解析を R の標準の関数で計算し直すスクリプトと、そのためのデータ（CSV）
  async function rFiles(tabs) {
    var items = tabs.map(function (t) { return { name: t.name, req: A.buildReq(t) }; });
    var d = await A.apiData('/api/rscript', { items: items, dataName: _t('解析用データ.csv'), date: new Date().toLocaleDateString('ja-JP') });
    if (!d.ok) throw new Error(d.message || _t('R のコードを作れませんでした。'));
    var enc = new TextEncoder();
    return [{ name: _t('解析.R'), data: enc.encode(d.script) }, { name: _t('解析用データ.csv'), data: enc.encode(d.data) }];
  }
  A.rFiles = rFiles;
  function okTabs() { return S.P.tabs.filter(function (t) { return t.checked && resOf(t) && resOf(t).ok; }); }
  function excelNote(x) { return (x.notes.length ? _t('Excel の関数で再現していないもの：{0}。', x.notes.join(_t('、'))) : '') + (x.skipped.length ? _t('書けなかった解析：{0}。', x.skipped.join(_t('、'))) : ''); }
  // ☑ を付けた解析の図・Excel・報告書をまとめて書き出す。dir：選んだフォルダー（選べない環境では ZIP にまとめる）
  async function exportAll(dir, opt) {
    var tabs = okTabs(), out = [], used = {}, note = '';
    A.busy(true, _t('書き出しの準備をしています'));
    try {
      if (opt.figs) for (var i = 0; i < tabs.length; i++) {
        var figs = A.figsOf(tabs[i], resOf(tabs[i])), base = A.safeName(tabs[i].name);
        if (used[base]) { used[base]++; base += '(' + used[base] + ')'; } else used[base] = 1;           // 同じ名前の解析があるときは (2) を付ける
        for (var k = 0; k < figs.length; k++) { A.busy(true, _t('図を画像にしています（{0} / {1}）', i + 1, tabs.length)); try { out.push({ name: base + _t('_図') + (k + 1) + '.png', blob: await pngBlob(figs[k]) }); } finally { A.busy(false); } }
      }
      if (opt.r) { A.busy(true, _t('R のコードを作っています')); try { (await rFiles(tabs)).forEach(function (f) { out.push({ name: f.name === _t('解析.R') ? A.baseName() + _t('_解析.R') : f.name, blob: new Blob([f.data]) }); }); } finally { A.busy(false); } }
      if (opt.excel) { A.busy(true, _t('Excel ファイルを作っています')); try { var x = await excelBlob(tabs); out.push({ name: A.baseName() + _t('_解析.xlsx'), blob: x.blob }); note = excelNote(x); } finally { A.busy(false); } }
      if (opt.pdf) out.push({ name: A.baseName() + _t('_報告書.pdf'), blob: await A.reportPdf() });
      if (opt.docx) out.push({ name: A.baseName() + _t('_報告書.docx'), blob: await A.reportDocx() });
      if (opt.report) { await A.ensureHolmAsync(); out.push({ name: A.baseName() + _t('_報告書.txt'), blob: new Blob(['﻿' + A.reportPlain()], { type: 'text/plain' }) }); }
      if (!out.length) { A.toast(_t('書き出すものがありません。'), true); return; }
      if (dir) {
        for (var j = 0; j < out.length; j++) { A.busy(true, _t('ファイルを保存しています（{0} / {1}）', j + 1, out.length)); try { var fh = await dir.getFileHandle(out[j].name, { create: true }), w = await fh.createWritable(); await w.write(out[j].blob); await w.close(); } finally { A.busy(false); } }
        A.toast(out.length + _t(' 個のファイルを「{0}」に書き出しました。', dir.name) + note);
      } else {
        var files = []; for (var q = 0; q < out.length; q++) files.push({ name: out[q].name, data: new Uint8Array(await out[q].blob.arrayBuffer()) });
        if (await A.saveBlob(await A.zip(files), A.baseName() + _t('_書き出し.zip'), 'ZIP', '.zip')) A.toast(out.length + _t(' 個のファイルを 1 つの ZIP にまとめて保存しました。') + note);
      }
    } catch (e) { A.toast(e && e.name === 'AbortError' ? _t('書き出しをやめました。') : _t('書き出せませんでした（{0}）。', e.message), true); }
    finally { A.busy(false); }
  }

  // ---------------- 数値の列のデータクリーニング
  function rerunCol(col) {
    S.P.tabs.forEach(function (t) { var d = A.def(t.type); if (!d) return; var uses = d.slots.some(function (s) { var sl = t.slots[s.id]; return sl && (sl.col === col || (sl.cols || []).indexOf(col) >= 0); }); if (uses) A.schedule(t.id); });
    A.render();
  }
  function numCleanDialog(col) {
    var x = A.v(col); if (!x || !x.clean) return;
    var c = x.clean, fix = x.fix || {}, rows = '';
    function val(r) { return Object.prototype.hasOwnProperty.call(fix, r.raw) ? fix[r.raw] : (r.val === undefined ? null : r.val); }
    function row(r, kind) {
      var v0 = val(r), ex = v0 === null;
      return '<tr data-raw="' + esc(r.raw) + '" data-kind="' + kind + '"><td class="md-nc-raw">' + esc(r.raw) + '</td><td class="num">' + r.n + '</td>' +
        '<td><input type="text" inputmode="decimal" class="md-in md-nc-val" value="' + (ex ? '' : esc(String(v0))) + _t('" placeholder="（除外）"></td></tr>');
    }
    (c.fixed || []).forEach(function (r) { rows += row(r, 'fixed'); });
    var rowsF = ''; (c.fail || []).forEach(function (r) { rowsF += row(r, 'fail'); });
    var body = _t('<p>「{0}」の回答のうち、そのままでは数値として読めなかったものです。値を直すと、その回答をした人すべてに当てはめます。<b>空欄にした回答は解析から除外します。</b></p>', esc(x.label)) +
      (rows ? _t('<div class="md-sub">自動で直した回答（{0} 件）</div><p class="md-hint">全角数字・単位（歳・時間など）・「約」「くらい」を外し、「3～4」のような範囲は中央の値にしました。</p><table class="md-numclean"><thead><tr><th>回答</th><th>件数</th><th>使う値</th></tr></thead><tbody>', c.nFixed) + rows + '</tbody></table>' : '') +
      (rowsF ? _t('<div class="md-sub">数値として読めなかった回答（{0} 件）</div><p class="md-hint">値を入れると、その値として使います。空欄のままなら除外します。</p><table class="md-numclean"><thead><tr><th>回答</th><th>件数</th><th>使う値</th></tr></thead><tbody>', c.nFail) + rowsF + '</tbody></table>' : '');
    modal(_t('数値の回答を確かめる'), body, _t('<button class="md-btn" data-act="closeModal">やめる</button><button class="md-btn" id="md_nc_reset">最初の状態に戻す</button><button class="md-btn md-btn-primary" id="md_nc_ok">この値で使う</button>'), 560);
    $('#md_nc_reset').onclick = function () { closeModal(); A.commit(function () { delete x.fix; }); rerunCol(col); };
    $('#md_nc_ok').onclick = function () {
      var nf = {}, bad = [];
      document.querySelectorAll('#md_modal tr[data-raw]').forEach(function (tr) {
        var raw = tr.dataset.raw, kind = tr.dataset.kind, t = tr.querySelector('.md-nc-val').value.trim().replace(/[０-９．－]/g, function (ch) { return String.fromCharCode(ch.charCodeAt(0) - 0xFEE0); });
        var auto = (kind === 'fixed' ? ((c.fixed || []).find(function (r) { return r.raw === raw; }) || {}).val : null);
        if (t === '') { if (kind === 'fixed') nf[raw] = null; return; }
        if (!isFinite(+t)) { bad.push(raw); return; }
        if (kind === 'fail' || +t !== auto) nf[raw] = +t;
      });
      if (bad.length) { A.toast(_t('数値でない値があります（{0}）。数字を入れるか空欄にしてください。', bad.slice(0, 3).join(_t('、'))), true); return; }
      closeModal(); A.commit(function () { if (Object.keys(nf).length) x.fix = nf; else delete x.fix; }); rerunCol(col);
      A.toast(_t('数値の回答を直しました。この列を使う解析を計算し直します。'));
    };
  }
  // ---------------- 解析の名前を変える（見出しの名前を入力欄に替える）
  function renameAn(id) {
    var tab = A.tabById(id || S.ui.sel); if (!tab) return;
    if (S.ui.view !== 'analysis') { S.ui.view = 'analysis'; A.render(); }
    var span = document.querySelector('[data-anname="' + tab.id + '"]'); if (!span) return;
    var inp = document.createElement('input'); inp.type = 'text'; inp.className = 'md-an-rename'; inp.value = tab.name; span.replaceWith(inp); inp.focus(); inp.select();
    inp.addEventListener('mousedown', function (e) { e.stopPropagation(); });
    inp.addEventListener('blur', function () { var v = inp.value.trim(), cancel = inp.dataset.cancel; if (!cancel && v && v !== tab.name) A.commit(function () { tab.name = v; tab.nameAuto = false; }); else A.render(); });
  }
  function moveTab(id, d) { var i = S.P.tabs.findIndex(function (t) { return t.id === id; }), j = i + d; if (i < 0 || j < 0 || j >= S.P.tabs.length) return; A.commit(function () { var t = S.P.tabs.splice(i, 1)[0]; S.P.tabs.splice(j, 0, t); }); scrollToAn(id); }
  function addFig(tab) {
    var m = Chart.model(resOf(tab)); if (!m) return;
    var kinds = Chart.kinds(m), used = tab.figs.map(function (f) { return Chart.kindOf(m, f); }), next = kinds.find(function (k) { return used.indexOf(k) < 0; }) || kinds[0];
    A.commit(function () { var f = tab.figs.length ? A.clone(tab.figs[tab.figs.length - 1]) : A.newFig(); f.kind = next; f.colors = {}; f.vTitle = null; f.cTitle = null; f.min = f.max = f.step = null; f.w = 560; f.h = 340; f.fit = true; tab.figs.push(f); }, { quiet: true });
    S.ui.sel = tab.id; S.ui.selFig = tab.figs.length - 1; S.ui.rtab = 'figure'; A.render();
  }
  function delFig(tab, k) { if (!tab.figs[k]) return; A.commit(function () { tab.figs.splice(k, 1); }, { quiet: true }); S.ui.selFig = Math.max(0, Math.min(S.ui.selFig, tab.figs.length - 1)); A.render(); A.toast(_t('図を削除しました。Ctrl+Z で元に戻せます。')); }

  // ---------------- ボタンの動作
  var act = {
    closeModal: closeModal,
    openCsv: async function () { var f = await A.pickFile('.csv,.tsv,.txt,.xlsx,.xlsm,.xls'); if (f) A.handleFile(f); },
    replaceCsv: async function () { var f = await A.pickFile('.csv,.tsv,.txt,.xlsx,.xlsm,.xls'); if (!f) return; try { var r = await A.xlsxIn(new Uint8Array(await f.arrayBuffer()), f.name); await A.loadCsv(r.buf, r.name, true); if (S.P.srcUrl) { S.P.srcUrl = null; A.render(); } } catch (e) { A.toast(e.message, true); } },
    openProject: projectDialog,
    openProjName: function (arg) { A.openProjectByName(arg); },
    save: async function () { if (!S.data) return; if (S.projName) { await A.saveProjectAs(S.projName); refreshProjects(); } else act.saveAs(); },
    saveAs: function () { if (!S.data) { A.toast(_t('保存するデータがありません。先に CSV を入れてください。'), true); return; } askName(_t('プロジェクトを保存'), S.projName || A.baseName(), async function (name) { await A.saveProjectAs(name); refreshProjects(); }); },
    projFile: A.exportProjectFile,
    undo: A.undo, redo: A.redo,
    togglePane: function () { S.ui.pane = !S.ui.pane; A.render(); },
    panePrev: function () { stepRtab(-1); }, paneNext: function () { stepRtab(1); },
    fileMenu: function (arg, el) {
      var r = el.getBoundingClientRect();
      menu(r.left, r.bottom, [{ name: _t('CSV を開く'), key: 'Ctrl+O', run: act.openCsv }, { name: _t('プロジェクトを開く'), run: act.openProject }, '-',
        { name: _t('上書き保存'), key: 'Ctrl+S', run: act.save, off: !S.data }, { name: _t('名前を付けて保存'), run: act.saveAs, off: !S.data }, { name: _t('プロジェクトをファイルに書き出す'), run: act.projFile, off: !S.data }, '-',
        { name: _t('まとめて書き出す'), run: act.exportAll, off: !S.P.tabs.length }, '-', { name: _t('設定'), run: act.settings }, { name: _t('このアプリについて'), run: act.about }, { name: _t('ご意見・不具合'), run: act.feedback }]);
    },
    newAnalysis: function () {
      if (!S.data) { A.toast(_t('先に CSV ファイルを入れてください。'), true); return; }
      if (S.P.tabs.length >= A.MAX_TABS) { A.toast(_t('解析は {0} 個までです。使わない解析を削除してください。', A.MAX_TABS), true); return; }
      var first = !S.P.tabs.length;
      analysisDialog(function (id) {
        var t; A.commit(function () { t = A.newTab(id); }, { quiet: true });
        if (t) { S.ui.sel = t.id; S.ui.selFig = 0; S.ui.view = 'analysis'; S.ui.rtab = 'analysis'; }
        A.render();
        if (t) { scrollToAn(t.id); if (!window.CTS_TOUCH) setTimeout(function () { var sel = document.querySelector('.md-an[data-an="' + t.id + '"] select[data-slot]'); if (sel) sel.focus(); }, 60); }
      }, first ? _t('解析を始める：解析の種類を選ぶ') : _t('新しい解析：解析の種類を選ぶ'));
    },
    changeType: function () {
      var tab = A.curTab(); if (!tab) return;
      analysisDialog(function (id) {
        if (id === tab.type) return;
        A.commit(function () {
          var old = []; Object.keys(tab.slots).forEach(function (k) { A.slotCols(tab.slots[k]).forEach(function (c) { old.push({ col: c, cats: tab.slots[k].cols ? null : tab.slots[k].cats }); }); }); var d = A.def(id);
          tab.type = id; tab.slots = {}; tab.opts = {}; (d.opts || []).forEach(function (o) { tab.opts[o.id] = o.default; });
          d.slots.forEach(function (sd) {
            if (sd.many) { var cs = old.filter(function (s) { var x = A.v(s.col); return x && sd.types.indexOf(x.type) >= 0; }).map(function (s) { return s.col; }); old = old.filter(function (s) { return cs.indexOf(s.col) < 0; }); tab.slots[sd.id] = { cols: [], cats: [] }; A.setItems(tab, sd.id, cs); return; }
            var k = old.findIndex(function (s) { var x = A.v(s.col); return x && sd.types.indexOf(x.type) >= 0; });
            if (k >= 0) { var s = old.splice(k, 1)[0]; tab.slots[sd.id] = sd.excludeSpecial || !s.cats ? { col: s.col, cats: A.defaultCats(A.v(s.col), sd) } : { col: s.col, cats: s.cats }; } else tab.slots[sd.id] = { col: null, cats: [] };
          });
          tab.figs.forEach(function (f) { f.kind = null; }); A.autoName(tab);
        }, { run: tab.id });
      }, _t('解析の種類を変える'));
    },
    toFisher: function (arg) { var tab = A.tabById(arg) || A.curTab(); if (!tab) return; A.commit(function () { tab.type = 'fisher'; var keep = tab.opts.residMode; tab.opts = { residMode: keep || 'z' }; }, { run: tab.id }); },
    dupTab: function () {
      var tab = A.curTab(); if (!tab) return;
      if (S.P.tabs.length >= A.MAX_TABS) { A.toast(_t('解析は {0} 個までです。', A.MAX_TABS), true); return; }
      var c; A.commit(function () { c = A.clone(tab); c.id = 't' + (++S.P.seq); c.name = tab.name + _t('（コピー）'); c.nameAuto = false; c.collapsed = false; S.P.tabs.splice(S.P.tabs.indexOf(tab) + 1, 0, c); }, { quiet: true });
      S.ui.sel = c.id; S.ui.selFig = 0; A.schedule(c.id); A.render(); scrollToAn(c.id);
    },
    delTab: function () {
      var tab = A.curTab(); if (!tab) return; var i = S.P.tabs.indexOf(tab);
      A.commit(function () { S.P.tabs.splice(i, 1); }, { quiet: true }); delete S.results[tab.id];
      S.ui.sel = S.P.tabs.length ? S.P.tabs[Math.min(i, S.P.tabs.length - 1)].id : null; S.ui.selFig = 0; A.render();
      A.toast(_t('「{0}」を削除しました。Ctrl+Z で元に戻せます。', tab.name));
    },
    swap: function () {
      var tab = A.curTab(); if (!tab) return; var ks = tab.slots.a && tab.slots.b ? ['a', 'b'] : tab.slots.x && tab.slots.y ? ['x', 'y'] : null; if (!ks) return;
      var d = A.def(tab.type), ok = ks.every(function (k, i) { var other = tab.slots[ks[1 - i]], sd = d.slots.find(function (s) { return s.id === k; }), x = other.col !== null ? A.v(other.col) : null; return !x || sd.types.indexOf(x.type) >= 0; });
      if (!ok) { A.toast(_t('この 2 つの列は種類が違うため、入れ替えられません。'), true); return; }
      A.commit(function () { var t = tab.slots[ks[0]]; tab.slots[ks[0]] = tab.slots[ks[1]]; tab.slots[ks[1]] = t; A.autoName(tab); }, { run: tab.id });
    },
    swapAt: function (arg) { if (A.tabById(arg)) { S.ui.sel = arg; act.swap(); } },
    itemRemove: function (arg) { var p = arg.split(':'), tab = A.tabById(p[0]); if (!tab) return; var cols = tab.slots[p[1]].cols.slice(); cols.splice(+p[2], 1); S.ui.sel = tab.id; A.commit(function () { A.setItems(tab, p[1], cols); }, { run: tab.id }); },
    itemMove: function (arg) { var p = arg.split(':'), tab = A.tabById(p[0]); if (!tab) return; var cols = tab.slots[p[1]].cols.slice(), i = +p[2], j = i + (+p[3]); if (j < 0 || j >= cols.length) return; var c = cols.splice(i, 1)[0]; cols.splice(j, 0, c); S.ui.sel = tab.id; A.commit(function () { tab.slots[p[1]].cols = cols; A.autoName(tab); }, { run: tab.id }); },
    itemAddSame: function (arg) {
      var p = arg.split(':'), tab = A.tabById(p[0]); if (!tab) return; var sd = A.def(tab.type).slots.find(function (x) { return x.id === p[1]; }), cols = tab.slots[p[1]].cols.slice(), first = A.v(cols[0]);
      S.P.vars.forEach(function (x) { if (cols.indexOf(x.col) < 0 && sd.types.indexOf(x.type) >= 0 && A.sameChoices(first, x)) cols.push(x.col); });
      S.ui.sel = tab.id; A.commit(function () { A.setItems(tab, p[1], cols); }, { run: tab.id });
    },
    rerun: function () { var tab = A.curTab(); if (tab) { delete S.results[tab.id]; A.schedule(tab.id); A.render(); } },
    toggleAn: function (arg) { var tab = A.tabById(arg); if (tab) A.commit(function () { tab.collapsed = !tab.collapsed; }, { key: 'fold' }); },
    foldAll: function (arg) { A.commit(function () { S.P.tabs.forEach(function (t) { t.collapsed = arg === '1'; }); }); },
    moveAn: function (arg) { if (S.ui.sel) moveTab(S.ui.sel, +arg); },
    moveAnId: function (arg) { var p = arg.split(':'); moveTab(p[0], +p[1]); },
    renameAn: function (arg) { renameAn(arg); },
    anMenu: function (arg, el) {
      var r = el.getBoundingClientRect(); anMenuAt(arg, r.left, r.bottom);
    },
    checkAll: function (arg) { A.commit(function () { S.P.tabs.forEach(function (t) { t.checked = arg === '1'; }); }); },
    goReport: function () { S.ui.view = 'report'; S.ui.rtab = 'export'; A.render(); },
    goTab: function (arg) { setRtab(arg); },
    moreRows: function () { S.ui.rawRows += 500; A.renderCenter(); },
    freqOfVar: function () {
      var x = S.ui.selVar !== null ? A.v(S.ui.selVar) : null; if (!x || !A.isCat(x)) return; var t;
      A.commit(function () { t = A.newTab('freq'); if (t) A.setSlot(t, 'a', x.col); }, { quiet: true });
      if (t) { S.ui.sel = t.id; S.ui.selFig = 0; S.ui.view = 'analysis'; S.ui.rtab = 'analysis'; A.schedule(t.id); } A.render(); if (t) scrollToAn(t.id);
    },
    figCatMove: function (arg) {
      var p = String(arg).split(':'), tab = A.curTab(), sl = tab && tab.slots[p[0]]; if (!sl) return;
      var labs = A.figOrderLabels(sl), i = +p[1], j = i + (+p[2]); if (j < 0 || j >= labs.length) return;
      var t = labs[i]; labs[i] = labs[j]; labs[j] = t; setSlotOrder(sl, labs);
    },
    figCatOrder: function (arg) {
      var p = String(arg).split(':'), tab = A.curTab(), sl = tab && tab.slots[p[0]]; if (!sl) return;
      var labs = A.figOrderLabels(sl), vr = sl.col ? A.v(sl.col) : null, nOf = {}, first = {};
      (vr && vr.cats || []).forEach(function (c, k) { nOf[c.v] = c.n; first[c.v] = k; });
      var sumN = function (l) { return sl.cats.filter(function (c) { return c.on && c.label === l; }).reduce(function (s, c) { return s + (nOf[c.v] || 0); }, 0); };
      var pos = function (l) { return Math.min.apply(null, sl.cats.filter(function (c) { return c.label === l; }).map(function (c) { return first[c.v] === undefined ? 1e9 : first[c.v]; })); };
      if (p[1] === 'rev') labs.reverse();
      else if (p[1] === 'count') labs.sort(function (a, b) { return sumN(b) - sumN(a); });
      else labs.sort(function (a, b) { return pos(a) - pos(b); });
      setSlotOrder(sl, labs);
    },
    varCatMove: function (arg) {
      var x = A.v(S.ui.selVar), p = String(arg).split(':'), i = +p[0], j = i + (+p[1]); if (!x || j < 0 || j >= x.cats.length) return;
      reorderVar(x, function () { var c = x.cats.splice(i, 1)[0]; x.cats.splice(j, 0, c); });
    },
    varOrder: function (arg) {
      var x = A.v(S.ui.selVar); if (!x) return;
      reorderVar(x, function () {
        var core = x.cats.filter(function (c) { return !c.special; }), sp = x.cats.filter(function (c) { return c.special; });   // 「無回答」などは最後のまま
        if (arg === 'rev') core.reverse();
        else if (arg === 'count') core.sort(function (a, b) { return b.n - a.n; });
        else if (arg === 'auto') { var o = x.cats0 || []; core.sort(function (a, b) { return o.indexOf(a.v) - o.indexOf(b.v); }); }
        x.cats = core.concat(sp);
      });
    },
    catMove: function (arg) { var p = arg.split(':'), tab = A.curTab(), sl = tab.slots[p[0]], i = +p[1], j = i + (+p[2]); if (j < 0 || j >= sl.cats.length) return; A.commit(function () { var c = sl.cats.splice(i, 1)[0]; sl.cats.splice(j, 0, c); }, { run: tab.id }); },
    catTwo: function (arg) {
      var tab = A.curTab(), sl = tab.slots[arg], on = sl.cats.filter(function (c) { return c.on; }), half = Math.ceil(on.length / 2);
      A.commit(function () { var a = on.slice(0, half), b = on.slice(half), la = a.map(function (c) { return c.v; }).join(_t('・')), lb = b.map(function (c) { return c.v; }).join(_t('・')); a.forEach(function (c) { c.label = la; }); b.forEach(function (c) { c.label = lb; }); }, { run: tab.id });
    },
    catReset: function (arg) { var tab = A.curTab(), d = A.def(tab.type), sd = d.slots.find(function (s) { return s.id === arg; }), sl = tab.slots[arg]; A.commit(function () { sl.cats = sl.cols ? A.defaultCatsMany(sl.cols, sd) : A.defaultCats(A.v(sl.col), sd); }, { run: tab.id }); },
    fsStep: function (arg) { var f = A.curFig(); if (!f) return; var cur = f.fsTick, i = FS_STEPS.findIndex(function (v) { return v >= cur; }); if (i < 0) i = FS_STEPS.length - 1; if (+arg > 0 && FS_STEPS[i] === cur) i++; else if (+arg < 0) i--; setPath('fig.fsAll', FS_STEPS[clamp(i, 0, FS_STEPS.length - 1)]); },
    pickColor: function (arg, el) { var r = el.getBoundingClientRect(); pickSeriesColor(+arg, r.left - 240, r.bottom + 2); },
    pickPath: function (arg, el) { var r = el.getBoundingClientRect(), none = arg === 'tst.headFill' || arg === 'tst.stripe'; colorPop(Math.min(r.left, window.innerWidth - 250), r.bottom + 2, function (c) { if (c || none) setPath(arg, c); }, none ? _t('色を付けない') : null); },
    palettePop: function (arg, el) {
      closePop(); var r = el.getBoundingClientRect(), d = document.createElement('div'), fig = A.curFig(), tab = A.curTab(), m = Chart.model(resOf(tab));
      var only = arg === 'tst.heatPal' || (fig && m && Chart.kindOf(m, fig) === 'heat') ? [_t('濃淡'), _t('2 方向')] : null;
      d.className = 'md-palpop'; d.innerHTML = A.paletteGallery(arg, A.get(arg), only); document.body.appendChild(d); place(d, r.left, r.bottom + 2);
    },
    clearColors: function () { var f = A.curFig(); A.commit(function () { f.colors = {}; }); },
    clearLegNames: function () { var f = A.curFig(); A.commit(function () { f.legNames = {}; }); },
    pickLegBg: function (arg, el) {
      var r = el.getBoundingClientRect(), f = A.curFig(); if (!f) return;
      colorPop(Math.min(r.left, window.innerWidth - 250), r.bottom + 2, function (c) { setPath('fig.legBg', c); }, null, [['', _t('自動（図の中に置いたときだけ白）')], ['none', _t('塗りなし（透明）')]]);
    },
    // 自動保存のスイッチ。まだ名前がないときは、先に名前を付けて保存してからオンにする
    autoSave: function () {
      if (!S.data) return;
      if (S.P.settings.autoSave) { S.P.settings.autoSave = false; A.markDirty(); A.toast(_t('自動保存をオフにしました。この設定も、次に保存するときにプロジェクトに入ります。')); A.render('chrome'); return; }
      function turnOn() { S.P.settings.autoSave = true; A.toast(_t('自動保存をオンにしました。変更すると数秒後に「{0}」へ上書き保存します。', S.projName)); A.markDirty(); A.render('chrome'); }
      if (S.projName) turnOn();
      else askName(_t('自動保存するプロジェクトの名前'), A.baseName(), async function (name) {
        S.P.settings.autoSave = true;
        if (await A.saveProjectAs(name)) { refreshProjects(); A.toast(_t('自動保存をオンにしました。変更すると数秒後に「{0}」へ上書き保存します。', S.projName)); }
        else S.P.settings.autoSave = false;
        A.render('chrome');
      });
    },
    figDefault: function () { var f = A.curFig(), o = {}; FIG_STYLE_KEYS.concat(['w', 'h', 'orient']).forEach(function (k) { o[k] = f[k]; }); A.commit(function () { S.P.figDefault = A.clone(o); }); A.toast(_t('これから作る図に、この書式を使います。')); },
    figApplyAll: function () { var f = A.curFig(), o = {}, n = 0; FIG_STYLE_KEYS.forEach(function (k) { o[k] = f[k]; }); A.commit(function () { S.P.tabs.forEach(function (t) { t.figs.forEach(function (g) { Object.assign(g, A.clone(o)); n++; }); }); S.P.figDefault = Object.assign(S.P.figDefault || {}, A.clone(o)); }); A.toast(_t('すべての図の書式をそろえました（{0} 個）。', n)); },
    figReset: function () { var tab = A.curTab(), k = Math.min(S.ui.selFig, tab.figs.length - 1), kind = tab.figs[k].kind; A.commit(function () { tab.figs[k] = A.newFig(); tab.figs[k].kind = kind; if (tab.type === 'timeline' && !(S.P.figDefault && S.P.figDefault.value)) tab.figs[k].value = 'count'; }); },
    addFig: function () { var tab = A.curTab(); if (tab) addFig(tab); },
    addFigTo: function (arg) { var tab = A.tabById(arg); if (tab) addFig(tab); },
    delFig: function () { var tab = A.curTab(); if (tab) delFig(tab, Math.min(S.ui.selFig, tab.figs.length - 1)); },
    delFigAt: function (arg) { var p = arg.split(':'), tab = A.tabById(p[0]); if (tab) { S.ui.sel = tab.id; delFig(tab, +p[1]); } },
    selFig: function (arg) { S.ui.selFig = +arg; A.render(); },
    figPng: function () { var tab = A.curTab(); if (tab) savePng(tab, Math.min(S.ui.selFig, tab.figs.length - 1)); },
    figPngAt: function (arg) { var p = arg.split(':'); savePng(A.tabById(p[0]), +p[1]); },
    figCopy: function () { var tab = A.curTab(); if (tab) copyPng(tab, Math.min(S.ui.selFig, tab.figs.length - 1)); },
    figCopyAt: function (arg) { var p = arg.split(':'); copyPng(A.tabById(p[0]), +p[1]); },
    figSvg: async function () { var fg = selFigObj(), tab = A.curTab(); if (!fg) return; if (await A.saveBlob(new Blob(['<?xml version="1.0" encoding="UTF-8"?>\n' + fg.r.svg], { type: 'image/svg+xml' }), A.safeName(tab.name) + _t('_図') + (Math.min(S.ui.selFig, tab.figs.length - 1) + 1) + '.svg', _t('SVG 画像'), '.svg')) A.toast(_t('図を SVG で保存しました。')); },
    tblCopy: function () { var tab = A.curTab(); if (tab) copyTables(tab); }, tblCopyAt: function (arg) { copyTables(A.tabById(arg)); },
    tblCsv: function () { var tab = A.curTab(); if (tab) saveTablesCsv(tab); }, tblCsvAt: function (arg) { saveTablesCsv(A.tabById(arg)); },
    tblPng: async function () {
      var tab = A.curTab(); if (!tab) return;
      try { if (await A.saveBlob(await tablesPng(tab), A.safeName(tab.name) + _t('_表.png'), _t('PNG 画像'), '.png')) A.toast(_t('表を画像で保存しました。')); } catch (e) { A.toast(_t('表を画像にできませんでした（{0}）。', e.message), true); }
    },
    tDesign: function (arg) { var tab = A.curTab(); if (!tab) return; A.commit(function () { tab.tstyle.design = arg; Object.assign(tab.tstyle, T_PRESET[arg]); }); },
    tstDefault: function () { var tab = A.curTab(); A.commit(function () { S.P.tstyleDefault = A.clone(tab.tstyle); }); A.toast(_t('これから作る解析の表に、このデザインを使います。')); },
    tstApplyAll: function () { var tab = A.curTab(), o = A.clone(tab.tstyle); A.commit(function () { S.P.tabs.forEach(function (t) { var heat = t.tstyle.heat; t.tstyle = A.clone(o); t.tstyle.heat = t === tab ? o.heat : heat; }); S.P.tstyleDefault = A.clone(o); }); A.toast(_t('すべての表のデザインをそろえました（{0} 個）。', S.P.tabs.length)); },
    tstReset: function () { var tab = A.curTab(); A.commit(function () { tab.tstyle = Object.assign(A.tableDefaults(), A.clone(S.P.tstyleDefault || {})); }); },
    textCopyAt: async function (arg) { var tab = A.tabById(arg), res = resOf(tab); if (!res || !res.ok) return; if (await copyText(tab.reportText !== null ? tab.reportText : res.report)) A.toast(_t('文をコピーしました。')); },
    resetTextAt: function (arg) { var tab = A.tabById(arg); if (tab) A.commit(function () { tab.reportText = null; tab.reportBase = null; }); },
    resetAllText: function () { A.commit(function () { S.P.tabs.forEach(function (t) { t.reportText = null; t.reportBase = null; }); }); A.toast(_t('すべての文を定型文に戻しました。')); },
    repPdf: function () { A.savePdf(); },
    repDocx: function () { A.saveDocx(); },
    tblSizeAuto: function () { var tab = A.curTab(); if (!tab) return; A.commit(function () { delete tab.tstyle.tw; delete tab.tstyle.rowH; }); },
    itemRev: function (arg) {
      var p = arg.split(':'), tab = A.tabById(p[0]); if (!tab) return; var sl = tab.slots[p[1]], c = +p[2];
      A.commit(function () { var r = (sl.reverse || []).slice(), i = r.indexOf(c); if (i >= 0) r.splice(i, 1); else r.push(c); sl.reverse = r; }, { run: tab.id });
    },
    asNumeric: function (arg) { var x = A.v(+arg); if (!x) return; A.commit(function () { x.typeOrig = x.type; x.type = 'numeric'; }); rerunCol(+arg); A.toast(_t('「{0}」を数値として読みます。直した回答と読めない回答は「確かめる」で見直せます。', x.label)); },
    asChoice: function (arg) { var x = A.v(+arg); if (!x || !x.typeOrig) return; A.commit(function () { x.type = x.typeOrig; delete x.typeOrig; }); rerunCol(+arg); },
    numClean: function (arg) { numCleanDialog(+arg); },
    repTxt: async function () { await A.ensureHolmAsync(); if (await A.saveBlob(new Blob(['﻿' + A.reportPlain()], { type: 'text/plain' }), A.baseName() + _t('_報告書.txt'), _t('テキスト'), '.txt')) A.toast(_t('報告書をテキストで保存しました。')); },
    repCopy: async function () { await A.ensureHolmAsync(); if (await copyText(A.reportPlain())) A.toast(_t('報告書の全文をコピーしました。')); },
    rOnly: async function () {
      var tabs = okTabs(); if (!tabs.length) { A.toast(_t('☑ を付けた解析のうち、結果が出ているものがありません。'), true); return; }
      A.busy(true, _t('R のコードを作っています'));
      try { var fs = await rFiles(tabs); var blob = await A.zip(fs, 'application/zip'); A.busy(false);
        if (await A.saveBlob(blob, A.baseName() + _t('_Rコード.zip'), 'ZIP', '.zip')) A.toast(_t('R のコード（解析.R）とデータ（解析用データ.csv）を ZIP で保存しました。展開して同じフォルダーで実行すると、R の結果と「アプリの値」を見比べられます。')); return; }
      catch (e) { A.toast(e.message, true); }
      A.busy(false);
    },
    excelOnly: async function () {
      var tabs = okTabs(); if (!tabs.length) { A.toast(_t('☑ を付けた解析のうち、結果が出ているものがありません。'), true); return; }
      A.busy(true, _t('Excel ファイルを作っています'));
      try { var x = await excelBlob(tabs); A.busy(false); if (await A.saveBlob(x.blob, A.baseName() + _t('_解析.xlsx'), _t('Excel ブック'), '.xlsx')) A.toast(_t('Excel ファイルを保存しました。') + excelNote(x)); return; } catch (e) { A.toast(e.message, true); }
      A.busy(false);
    },
    exportAll: function () {
      var tabs = okTabs(), nfig = tabs.reduce(function (s, t) { return s + t.figs.length; }, 0), canDir = !!window.showDirectoryPicker;
      if (!tabs.length) { A.toast(_t('☑ を付けた解析のうち、結果が出ているものがありません。解析の見出しのチェックを確かめてください。'), true); return; }
      modal(_t('まとめて書き出す'), _t('<p>☑ を付けた <b>{0} 件</b>の解析を書き出します。</p><label class="md-check"><input type="checkbox" id="md_ex_fig" checked>図（PNG、{1} 個）　名前は「解析の名前_図1.png」</label><label class="md-check"><input type="checkbox" id="md_ex_xl" checked>Excel ファイル（データと、関数で再現した解析）</label><label class="md-check"><input type="checkbox" id="md_ex_r">R のコード（解析を R の標準の関数で計算し直すスクリプトと、そのデータ）</label><label class="md-check"><input type="checkbox" id="md_ex_pdf" checked>報告書（PDF。文・表・図をすべて含む）</label><label class="md-check"><input type="checkbox" id="md_ex_docx">報告書（Word。表は Word の表で、あとから直せる）</label><label class="md-check"><input type="checkbox" id="md_ex_rep">報告書（テキスト。文と表だけ）</label>', tabs.length, nfig) +
        '<p class="md-hint" style="margin-top:8px">' + (canDir ? _t('次の画面で書き出し先のフォルダーを選びます。同じ名前のファイルがあるときは上書きします。') : _t('この環境ではフォルダーを選べないため、1 つの ZIP ファイルにまとめて保存します。')) + '</p>',
        _t('<button class="md-btn" data-act="closeModal">やめる</button><button class="md-btn md-btn-primary" id="md_ex_go">') + (canDir ? _t('フォルダーを選んで書き出す') : _t('ZIP にまとめて保存')) + '</button>', 520);
      $('#md_ex_go').onclick = async function () {
        var opt = { figs: $('#md_ex_fig').checked, excel: $('#md_ex_xl').checked, report: $('#md_ex_rep').checked, pdf: $('#md_ex_pdf').checked, docx: $('#md_ex_docx').checked, r: $('#md_ex_r').checked }, dir = null;
        if (opt.pdf && A.askLocalFonts) A.askLocalFonts();                 // MS 明朝・Times New Roman を使う許可は、押した直後にしかたずねられない
        if (canDir) { try { dir = await window.showDirectoryPicker({ mode: 'readwrite' }); } catch (e) { return; } }       // フォルダーを選ぶ画面（OS 標準）
        closeModal(); exportAll(dir, opt);
      };
    },
    // 尺度得点：複数の項目の平均点・合計点を、新しい列として作る
    openUrl: function () {
      var cur = S.P.srcUrl || '';
      modal(_t('Google スプレッドシートから読み込む'),
        _t('<label class="md-field">回答のスプレッドシートの URL<input type="text" id="md_url" value="{0}" placeholder="https://docs.google.com/spreadsheets/d/…"></label><p class="md-hint" style="margin:8px 0 4px">Google フォームの「回答」タブ →「スプレッドシートにリンク」で開いたスプレッドシートの URL を貼ります。読み込むには、次のどちらかが必要です。</p><ul class="md-hint" style="margin:0;padding-left:1.4em"><li>共有を「リンクを知っている全員（閲覧者）」にする</li><li>「ファイル → 共有 → ウェブに公開」で、シートと CSV を選んで公開し、その URL を使う</li></ul><p class="md-hint" style="margin:8px 0 0;color:#C00000">どちらも、URL を知っている人なら誰でも回答を見られる状態になります。個人を特定できる回答（氏名・メールアドレスなど）を含むときは、この方法を使わず、CSV をダウンロードして入れてください。</p><p class="md-hint" style="margin:6px 0 0">URL はプロジェクトに残すので、あとで「最新に更新」を押すだけで、増えた回答を読み込み直せます（解析の設定は残ります）。</p>', esc(cur)),
        _t('<button class="md-btn" data-act="closeModal">やめる</button><button class="md-btn md-btn-primary" id="md_url_go">読み込む</button>'), 560);
      $('#md_url_go').onclick = async function () {
        var u = $('#md_url').value.trim(); if (!u) { A.toast(_t('URL を入れてください。'), true); return; }
        try { A.sheetCsvUrl(u); } catch (e) { A.toast(e.message, true); return; }
        closeModal();
        var keep = !!(S.data && S.P.tabs.length) && confirm(_t('解析を残したまま、このスプレッドシートの回答に差し替えますか。\n（「キャンセル」で新しく始めます）'));
        try { await A.loadUrl(u, keep); A.toast(_t('スプレッドシートから回答を読み込みました（{0} 件）。', S.data.nrow)); } catch (e) { A.toast(e.message, true); }
      };
    },
    refreshUrl: async function () {
      if (!S.P.srcUrl) { A.toast(_t('URL から読み込んだデータではありません。'), true); return; }
      var n0 = S.data ? S.data.nrow : 0;
      try { await A.loadUrl(S.P.srcUrl, true); A.toast(_t('最新の回答に更新しました（{0} 件。前回より {1}{2} 件）。', S.data.nrow, S.data.nrow - n0 >= 0 ? '+' : '', S.data.nrow - n0)); } catch (e) { A.toast(e.message, true); }
    },
    setTheme: function (arg) { A.setTheme(arg); refreshSettings(); },
    setLang: function (arg) { if (arg === window.CTS_LANG) return; window.ctsSetLang(arg); },
    settings: function () {
      modal(_t('設定'), '<div id="md_set_body">' + A.settingsHtml() + '</div>', _t('<button class="md-btn md-btn-primary" data-act="closeModal">閉じる</button>'), 640);
    },
    // ---- 既定の設定（初期設定・カスタム設定）
    defMode: function (arg) {
      if (arg === 'custom' && !A.customGet()) { A.toast(_t('まだカスタム設定がありません。設定を整えてから「今の設定をカスタム設定に保存」を押してください。'), true); return; }
      A.setDefMode(arg); A.render('pane'); refreshSettings();
      A.toast(arg === 'custom' ? _t('これから新しく始めるときは、カスタム設定を使います。') : _t('これから新しく始めるときは、初期設定を使います。カスタム設定は残してあります。'));
    },
    customSave: function () {
      var c = customFromNow();
      if (A.customGet() && !confirm(_t('保存してあるカスタム設定を、今の設定で置き換えますか。'))) return;
      if (!A.customSet(c)) { A.toast(_t('この端末に保存できませんでした（ブラウザの保存の設定を確かめてください）。'), true); return; }
      A.setDefMode('custom'); A.render('pane'); refreshSettings();
      A.toast(_t('今の設定をカスタム設定に保存しました。これから新しく始めるときに使います。'));
    },
    customApply: function () {
      var c = A.defMode() === 'custom' ? A.customGet() : null, base = A.baseProject();
      A.commit(function () {
        if (c) A.applyDefaults(S.P, c);
        else { S.P.settings = Object.assign(base.settings, { autoSave: S.P.settings.autoSave }); S.P.report = base.report; S.P.figDefault = null; S.P.tstyleDefault = null; }
      });
      A.applyZoom(); A.syncResults && A.syncResults(); A.render(); refreshSettings();
      A.toast((c ? _t('カスタム設定') : _t('初期設定')) + _t('を、開いているプロジェクトに当てはめました（判定と表記、報告書、これから作る図と表の書式）。今ある図と表を同じ書式にするには、「すべての図にそろえる」「すべての表にそろえる」を使ってください。'));
    },
    customExport: async function () {
      var c = A.customGet(); if (!c) { A.toast(_t('まだカスタム設定がありません。'), true); return; }
      var blob = new Blob([JSON.stringify({ app: 'CrossTab Studio', kind: 'custom-settings', custom: c }, null, 1)], { type: 'application/json' });
      try { if (await A.saveBlob(blob, _t('CrossTabStudio_カスタム設定.json'), _t('カスタム設定'), '.json')) A.toast(_t('カスタム設定を書き出しました。別の端末の「ファイル」→「設定」で読み込めます。')); } catch (e) { if (!(e && e.name === 'AbortError')) A.toast(_t('書き出せませんでした（{0}）。', e && e.message || e), true); }
    },
    customImport: async function () {
      var f = await A.pickFile('.json'); if (!f) return;
      try {
        var j = JSON.parse(await f.text()), c = j && j.kind === 'custom-settings' ? j.custom : null;
        if (!c || typeof c !== 'object') throw new Error(_t('CrossTab Studio のカスタム設定のファイルではありません。'));
        if (A.customGet() && !confirm(_t('保存してあるカスタム設定を、読み込んだ設定で置き換えますか。'))) return;
        A.customSet(c); A.setDefMode('custom'); A.render('pane'); refreshSettings(); A.toast(_t('カスタム設定を読み込みました。これから新しく始めるときに使います。'));
      } catch (e) { A.toast(e.message || _t('読み込めませんでした。'), true); }
    },
    customDelete: function () {
      if (!A.customGet()) return;
      if (!confirm(_t('この端末に保存してあるカスタム設定を消しますか。消すと元に戻せません（書き出したファイルがあれば読み込み直せます）。'))) return;
      A.customSet(null); A.setDefMode('init'); A.render('pane'); refreshSettings(); A.toast(_t('カスタム設定を消しました。初期設定を使います。'));
    },
    scoreNew: function () { scoreDialog(null); },
    periodNew: function () { periodDialog(null); },
    periodEdit: function (arg) { periodDialog(+arg); },
    periodAdd: function () { if (PD) PD.add(); },
    periodDelRow: function (arg) { if (PD) PD.del(+arg); },
    periodAuto: function () { if (PD) PD.auto(); },
    periodSave: function () { if (PD) PD.save(); },
    guide: function () { guideDialog(); },
    scoreEdit: function (arg) { scoreDialog(+arg); },
    scoreDel: function (arg) {
      var c = +arg, x = A.v(c); if (!x) return;
      var used = S.P.tabs.filter(function (t) { return Object.keys(t.slots).some(function (k) { var sl = t.slots[k]; return sl.col === c || (sl.cols || []).indexOf(c) >= 0; }); });
      if (used.length && !confirm(_t('「{0}」は {1} 件の解析で使っています。消すと、その解析の列の選択が外れます。消しますか。', x.label, used.length))) return;
      A.commit(function () {
        S.P.vars = S.P.vars.filter(function (y) { return y.col !== c; });
        S.P.tabs.forEach(function (t) { Object.keys(t.slots).forEach(function (k) { var sl = t.slots[k]; if (sl.col === c) t.slots[k] = { col: null, cats: [] }; if (sl.cols) sl.cols = sl.cols.filter(function (z) { return z !== c; }); }); });
      });
      A.syncResults();
    },
    scoreSave: function (arg) {
      var m = document.getElementById('md_modal'); if (!m) return;
      var name = m.querySelector('#md_sc_name').value.trim(), method = m.querySelector('input[name="md_sc_m"]:checked').value;
      var items = [].map.call(m.querySelectorAll('[data-scitem]:checked'), function (e) { return +e.dataset.scitem; });
      var rev = [].map.call(m.querySelectorAll('[data-screv]:checked'), function (e) { return +e.dataset.screv; }).filter(function (c) { return items.indexOf(c) >= 0; });
      if (!name) { A.toast(_t('名前を入れてください。'), true); return; }
      if (items.length < 2) { A.toast(_t('項目を 2 つ以上選んでください。'), true); return; }
      if (S.P.vars.some(function (y) { return y.label === name && y.col !== +arg; })) { A.toast(_t('同じ名前の列があります。別の名前にしてください。'), true); return; }
      closeModal();
      A.commit(function () {
        var x = arg ? A.v(+arg) : null;
        if (x) { x.label = name; x.header = name; x.comp = { items: items, reverse: rev, method: method }; }
        else {
          var col = Math.max.apply(null, S.P.vars.map(function (y) { return y.col; })) + 1;
          S.P.vars.push({ col: col, header: name, label: name, type: 'numeric', n: '–', nUnique: 0, cats: [], comp: { items: items, reverse: rev, method: method } });
        }
      });
      A.syncResults();
      A.toast(_t('「{0}」を作りました。数値の列として、t 検定・分散分析・相関などで選べます。', name));
    },
    // ご意見・不具合の報告：GitHub の Issues を、版と端末を書き込んだ状態で開く
    feedback: function () {
      var n = A.errLog().length;
      modal(_t('ご意見・不具合の報告'),
        _t('<p style="margin:0 0 8px">不具合の報告やご意見・要望は、GitHub の「Issues」で受け付けています。送るには GitHub のアカウント（無料）が必要です。</p><p style="margin:0 0 8px"><b>書いた内容は誰でも読めます。</b>アンケートの回答データ、名前や学校名などの個人情報は書かないでください。画面の写真を貼るときも、回答が写っていないか確かめてください。</p><p style="margin:0 0 4px">ボタンを押すと、アプリの版と端末の種類を書き込んだ状態で報告の画面が開きます。</p><div class="md-row" style="gap:8px;margin:8px 0"><button class="md-btn md-btn-primary" data-act="fbOpen" data-arg="bug">不具合を報告する</button><button class="md-btn" data-act="fbOpen" data-arg="idea">ご意見・要望を送る</button></div><div class="md-sub">エラーの記録</div><p class="md-hint" style="margin:0 0 6px">アプリの中で起きたエラーを、この端末の中だけに記録しています（今 {0} 件）。外には送りません。報告するときに写して、報告の「エラーの記録」の欄に貼ると、原因を調べやすくなります。</p><div class="md-row" style="gap:8px"><button class="md-btn" data-act="fbCopyLog"{1}>エラーの記録を写す</button><button class="md-btn" data-act="fbClearLog"{2}>記録を消す</button></div>', n, n ? '' : ' disabled', n ? '' : ' disabled'),
        _t('<button class="md-btn md-btn-primary" data-act="closeModal">閉じる</button>'), 560);
    },
    fbOpen: function (arg) {
      var env = envText(), base = 'https://github.com/OKUYAMA23/crosstab-studio/issues/new';
      var url = base + '?template=' + (arg === 'bug' ? 'bug_report.yml' : 'feedback.yml') + '&version=' + encodeURIComponent(window.CTS_APP_VERSION || '') + '&env=' + encodeURIComponent(env);
      window.open(url, '_blank', 'noopener');
    },
    fbCopyLog: async function () {
      var a = A.errLog(), t = a.map(function (e) { return e.t + ' [' + e.v + '] ' + e.k + ': ' + e.m + (e.s ? '\n    ' + e.s : ''); }).join('\n');
      if (await copyText(t)) A.toast(_t('エラーの記録を写しました。報告の「エラーの記録」の欄に貼り付けてください。'));
    },
    fbClearLog: function () { A.clearErrLog(); A.toast(_t('エラーの記録を消しました。')); act.feedback(); },
    about: function () { modal(_t('このアプリについて'), aboutHtml(), _t('<button class="md-btn md-btn-primary" data-act="closeModal">閉じる</button>'), 760); }
  };
  // 「このアプリについて」の中身：概要、データの扱い、解析ごとの計算方法、共通の決まり、Excel での再現、使っている部品
  function aboutHtml() {
    var web = !!window.CTS_WEB, v = window.CTS_APP_VERSION ? ' ' + esc(window.CTS_APP_VERSION) : '';
    function h(t) { return '<div class="md-sec-title" style="margin:14px 0 4px">' + t + '</div>'; }
    function tbl(head, rows) {
      return '<table class="md-t0" style="width:100%"><tr>' + head.map(function (x) { return '<th class="md-rh">' + x + '</th>'; }).join('') + '</tr>' + rows.map(function (r) {
        if (typeof r === 'string') return '<tr><th class="md-rh" colspan="' + head.length + '" style="text-align:left;background:var(--t-f3f7f4)">' + r + '</th></tr>';
        return '<tr><th class="md-rh" style="width:30%;white-space:normal">' + r[0] + '</th>' + r.slice(1).map(function (c) { return '<td class="md-tx" style="white-space:normal">' + c + '</td>'; }).join('') + '</tr>';
      }).join('') + '</table>';
    }
    var A1 = [
      _t('集計'),
      [_t('単純集計'), _t('選択肢ごとの人数と割合。「回答の偏りを検定する」を選ぶと、適合度のカイ二乗検定（chisq.test）。複数回答の質問は、選んだ人の割合（合計は 100% を超える）。')],
      [_t('複数の設問の単純集計'), _t('同じ選択肢の設問を並べて集計。順序のある回答では、フリードマン検定（friedman.test、すべての設問に答えた人のみ）と効果量 Kendall の W を選べる。')],
      [_t('数値の要約とヒストグラム'), _t('n、平均、標準偏差、最小値、四分位数、中央値、最大値。区切り方は、自動（整数で範囲が狭ければ値ごと）・スタージェスの公式・フリードマン＝ダイアコニスの方法から選ぶ。')],
      [_t('回答日時の推移'), _t('回答日時を日・週・月ごとに数え、累積の割合も示す。')],
      _t('2 つの質問の関連'),
      [_t('カイ二乗検定'), _t('chisq.test（2×2 の表でも補正なしが初期設定。イェーツの補正を選べる）。効果量は Cramér の V（2×2 では φ）。調整済み標準化残差でセルごとの多い・少ないを判定。')],
      [_t('フィッシャーの正確確率検定'), _t('fisher.test。2×2 の表では条件付き最尤推定のオッズ比と 95% 信頼区間。表が大きく正確な計算が難しいときは、モンテカルロ法（20 万回）に自動で切り替え。')],
      [_t('割合の傾向検定'), _t('prop.trend.test（コクラン・アーミテージ）。段階に等間隔の得点を与える。')],
      [_t('層別のクロス集計'), _t('mantelhaen.test（連続性の補正なし。3×2 以上の表では一般化した CMH 検定）。層ごとの chisq.test。2×2 の表では共通オッズ比（マンテル・ヘンツェル）と 95% 信頼区間、ブレスロー・デイ検定（タロンの補正）。')],
      _t('順序のある回答の比較'),
      [_t('マン・ホイットニーの U 検定'), _t('wilcox.test（正規近似、同順位と連続性の補正）。効果量 r = |Z| / √N。')],
      [_t('クラスカル・ウォリス検定'), _t('kruskal.test。効果量 ε² = H / (N − 1)。多重比較は pairwise.wilcox.test（ホルム法）。')],
      [_t('スピアマンの順位相関'), _t('cor.test(method = "spearman")。')],
      _t('平均の比較'),
      [_t('t 検定'), _t('ウェルチの t 検定（t.test）。分散が等しいと仮定するスチューデントの t 検定も選べる。効果量は Hedges の g。平均の差の 95% 信頼区間。')],
      [_t('一元配置分散分析'), _t('ウェルチの分散分析（oneway.test）と、ゲームス・ハウエルの方法による多重比較。分散が等しいと仮定する通常の分散分析（テューキーの方法、TukeyHSD）も選べる。効果量は η²。')],
      [_t('対応のある t 検定'), _t('t.test(paired = TRUE)。効果量は dz（差の平均 ÷ 差の標準偏差）。差の 95% 信頼区間。')],
      _t('同じ人の 2 つの回答'),
      [_t('ウィルコクソンの符号付順位検定'), _t('wilcox.test(paired = TRUE)。')],
      [_t('マクネマー検定'), _t('mcnemar.test（連続性の補正あり）。食い違った組が 25 未満のときは binom.test（正確二項検定）。')],
      _t('関連・相関'),
      [_t('ピアソンの相関係数'), _t('cor.test(method = "pearson")。95% 信頼区間はフィッシャーの z 変換による。散布図に回帰直線（lm）を引く。')],
      _t('回帰分析'),
      [_t('重回帰分析'), _t('lm（強制投入法）。偏回帰係数 B、標準誤差、標準化係数 β、95% 信頼区間、t、p、VIF。R²、調整済み R²、F 検定。単一回答は先頭の選択肢を基準にしたダミー変数、順序のある回答は段階の得点。')],
      [_t('ロジスティック回帰分析'), _t('glm(family = binomial)。B、標準誤差、Wald 検定、オッズ比と Wald 法の 95% 信頼区間、VIF。尤度比検定、Cox-Snell と Nagelkerke の R²、AIC、的中率（予測確率 0.5 で判定）。')],
      _t('複数回答'),
      [_t('複数回答のクロス集計'), _t('選択肢ごとに chisq.test または fisher.test、p.adjust（ホルム法）で補正。')],
      _t('尺度・多変量'),
      [_t('尺度の信頼性'), _t('クロンバックの α と標準化 α、ω 係数（1 因子の factanal の負荷量から）。項目ごとの平均・標準偏差、天井効果・床効果（平均 ± 標準偏差が得点の範囲を超えるか）、項目と残りの合計との相関、その項目を除いたときの α。逆転項目は得点を反転して計算。')],
      [_t('確認的因子分析'), _t('最尤法（基本の R の optim で推定。因子の分散を 1 に固定し、因子間の相関を推定）。χ² 検定、CFI、TLI、RMSEA と 90% 信頼区間、SRMR、標準化負荷量、標準誤差（ヘッセ行列から）、因子ごとの ω（合成信頼性）と AVE。段階の得点を数値として扱う。')],
      [_t('探索的因子分析'), _t('最尤法（factanal）。回転はプロマックス（因子間相関も示す）またはバリマックス。因子の数は、自動のときは固有値が 1 を超える数（カイザー基準）。')],
      [_t('コレスポンデンス分析'), _t('標準化残差の行列の特異値分解。行と列の両方を主座標で示す（対称マップ）。関連の有無はカイ二乗検定で示す。')],
      _t('自由記述'),
      [_t('テキストマイニング'), _t('文をブラウザの単語分割（Intl.Segmenter）で語に分け、漢字・カタカナの続く語をつなげ、助詞などを除く。頻出語、Jaccard 係数による共起ネットワーク（配置はフルクターマン・ラインゴールド法、まとまりはラベル伝播法）。グループを選ぶと、語ごとにカイ二乗検定を行い、ホルム法で補正。')]
    ];
    return '<p><b>CrossTab Studio</b>' + v + _t('　アンケート（Google フォームの回答など）の集計と検定を行うソフトです。計算はすべて') + (web ? _t('この端末のブラウザの中で動く R（webR）') : _t('このパソコンの中の R')) + (S.r ? _t('（バージョン {0}）', esc(S.r)) : '') + _t('で行います。</p>') +
      h(_t('データの扱い')) +
      '<ul style="margin:0;padding-left:1.4em">' +
      _t('<li>読み込んだデータと結果は、') + (web ? _t('この端末の中') : _t('このパソコンの中')) + _t('だけで扱い、外部に送信しません。AI のサービスにもつないでいません。</li>') +
      _t('<li>Google スプレッドシートの URL から読み込むときは、そのスプレッドシートから回答を受け取るだけで、こちらから何かを送ることはありません。読み込むには、スプレッドシートを「リンクを知っている全員」に共有するか、ウェブに公開する必要があります。</li>') +
      _t('<li>Excel ファイル（.xlsx）は、1 枚目のシートを CSV と同じ形の表に直して読み込みます。日付は「年/月/日 時:分:秒」の文字にします。プロジェクトには、直した表（CSV）を保存します。</li>') +
      _t('<li>プロジェクトは') + (web ? _t('この端末（ブラウザの中）に保存します。ブラウザのデータを消すと消えるので、大事なものは「ファイルに書き出す」で .prj ファイルにも保存してください。') : _t('この端末（ブラウザの中）に保存します。')) + '</li>' +
      '</ul>' +
      h(_t('解析と計算の方法')) + tbl([_t('解析'), _t('使う R の関数と出力')], A1) +
      h(_t('すべての解析に共通の決まり')) +
      '<ul style="margin:0;padding-left:1.4em">' +
      _t('<li>順序のある回答は、先頭の選択肢を上位として順位や得点を付けます。</li>') +
      _t('<li>無回答と、チェックを外した選択肢を選んだ人は、その解析から除きます。数値の回答は、全角数字・単位・範囲（「1〜2 時間」など）を直してから使い、数値にできない回答は除きます。</li>') +
      _t('<li>p 値は「関連がない（差がない）」という仮定のもとで、今回以上の偏りが出る確率です。有意でないことは、関連がないことの証明にはなりません。</li>') +
      _t('<li>報告書では、☑ を付けた検定をまとめてホルム法で補正した p も示します（設定で切り替えられます）。</li>') +
      _t('<li>「平均と信頼区間」の図の線は、t 分布による平均の 95% 信頼区間です。</li>') +
      _t('<li>尺度得点（データの「尺度得点を作る」）は、選んだ項目の得点（逆転項目は反転）の平均または合計です。すべての項目に答えた人だけ得点を出します。作り方は、その得点を使った解析の「方法」に書きます。</li>') +
      '</ul>' +
      h(_t('Excel での再現')) +
      _t('<p style="margin:0">Excel ファイルには、データと、解析の計算を Excel の関数で書いたセルを入れます。結果の値は貼り付けず、すべて数式なので、計算の過程を確かめられます。次のものは Excel の関数では求められないため載せていません。アプリの値を使ってください。</p>') +
      '<ul style="margin:4px 0 0;padding-left:1.4em">' +
      _t('<li>2×2 より大きい表のフィッシャーの正確確率検定の p</li>') +
      _t('<li>分散分析のあとの多重比較の p（スチューデント化された範囲の分布の関数が Excel にないため。平均の差と t は載せます）</li>') +
      _t('<li>探索的因子分析（反復計算のため）</li>') +
      _t('<li>コレスポンデンス分析の次元と座標（特異値分解のため。関連のカイ二乗検定までは載せます）</li>') +
      _t('<li>テキストマイニング（語の分割のため）</li>') +
      _t('<li>層別のクロス集計の、一般化した CMH 検定（3×2 以上の表）と、共通オッズ比の信頼区間・ブレスロー・デイ検定（層ごとのカイ二乗検定と、2×2 の表の CMH 検定・共通オッズ比までは載せます）</li>') +
      _t('<li>尺度の信頼性の ω 係数と、確認的因子分析（反復計算で求めるため）</li>') +
      _t('<li>ロジスティック回帰分析（係数を反復計算で求めるため。重回帰分析は X\'X の逆行列で再現します）</li></ul>') +
      _t('<p style="margin:4px 0 0">自由度が小数になるウェルチの検定の p は、Excel の T.DIST・F.DIST が自由度の小数を切り捨てるため、BETA.DIST で求めています。</p>') +
      h(_t('R のコードでの確かめ')) +
      _t('<p style="margin:0">「書き出し」→「R のコード」で、☑ を付けた解析を R の標準の関数（chisq.test、t.test、lm など）で計算し直すスクリプト（解析.R）と、そのためのデータ（解析用データ.csv）を保存できます。選択肢のまとめ方、絞り込み、尺度得点の作り方もスクリプトの中にコードとして書きます。数値の列は、アプリが直した値（「40分」→ 40 など）をデータに足して使います。実行すると、各解析の結果のあとに「>> アプリの値」が表示されます。テキストマイニングと回答日時の推移は、アプリの中の処理を使うためコードにしていません。確認的因子分析は、パッケージ lavaan があればそれで計算し直します。</p>') +
      h(_t('報告書（PDF）の字体')) +
      _t('<p style="margin:0">本文と表は、和文を MS 明朝、英数字を Times New Roman で書きます（') + (web ? _t('パソコンの Chrome・Edge で、字体の使用を許可したとき') : _t('パソコンに入っているとき')) + _t('）。使えないときは、形の近い BIZ UD明朝と Tinos で書きます。見出しと図は BIZ UDPゴシックです。</p>') +
      h(_t('論文などでの引用のしかた')) +
      _t('<p style="margin:0">論文・レポート・発表などでこのアプリを使ったときは、引用していただけると幸いです。このアプリは広く配布されている一般的なソフトではないため、APA 方式などでは、本文で使ったことと版を書き、参考文献にも挙げます。計算に使った R も挙げるのが一般的です。投稿先や提出先に決まりがあるときは、そちらに合わせてください。本文の書き方の例：「集計と検定には CrossTab Studio（バージョン ') + esc(window.CTS_APP_VERSION || '') + '; ' + A.citeList()[0].inText.replace(/^（|）、.*$/g, '') + _t('）を用いた。」</p>') +
      '<table class="md-t0" style="width:100%;margin-top:6px">' + A.citeList().map(function (c) {
        return '<tr><th class="md-rh" style="width:24%;white-space:normal">' + esc(c.name) + '</th><td class="md-tx" style="white-space:normal;overflow-wrap:anywhere">' + c.ref(true) + (c.inText ? _t('<br><span class="md-hint" style="margin:0">本文中：') + esc(c.inText) + '</span>' : '') + '</td></tr>';
      }).join('') + '</table>' +
      _t('<p class="md-hint" style="margin:4px 0 0">版の番号は、使った版に合わせてください（報告書の末尾にも、作ったときの版で書き出します）。SIST 02 の形の（参照 …）は、アプリを使った日に直してください。</p>') +
      h(_t('作者とソースコード')) +
      _t('<p style="margin:0">作者は奥山泰冴（Taiga Okuyama）です（© 2026 Taiga Okuyama）。このアプリのソースコードは、MIT ライセンスで GitHub に公開しています（https://github.com/OKUYAMA23/crosstab-studio）。計算の中身を確かめたり、改良したりできます。同梱している部品は、それぞれのライセンスに従います。</p>') +
      h(_t('使っている部品')) +
      _t('<p style="margin:0">R（GPL）') + (web ? _t('、webR（配布している一式は GPL-3.0）') : _t('、httpuv・jsonlite（R のパッケージ）')) + _t('、pdf-lib・fontkit（MIT ライセンス）、HarfBuzz（MIT ライセンス）、BIZ UDPゴシック・BIZ UD明朝・Tinos（SIL Open Font License 1.1）。</p>');
  }
  function anMenuAt(id, x, y) {
    var i = S.P.tabs.findIndex(function (t) { return t.id === id; }), tab = S.P.tabs[i]; if (!tab) return;
    selectTab(id);
    menu(x, y, [{ name: _t('名前を変更'), key: 'F2', run: function () { renameAn(id); } }, { name: tab.collapsed ? _t('開く') : _t('畳む'), run: function () { act.toggleAn(id); } }, '-',
      { name: _t('上へ動かす'), run: function () { moveTab(id, -1); }, off: i === 0 }, { name: _t('下へ動かす'), run: function () { moveTab(id, 1); }, off: i === S.P.tabs.length - 1 }, '-',
      { name: _t('複製'), run: act.dupTab }, { name: _t('種類を変える'), run: act.changeType }, { name: _t('図を追加'), run: act.addFig, off: !Chart.model(resOf(tab)) }, '-', { name: _t('削除'), danger: true, run: act.delTab }]);
  }
  function setRtab(k) {
    S.ui.rtab = k;
    if (k === 'data' && S.data) S.ui.view = 'data';
    else if (S.ui.view === 'data' && k !== 'settings' && k !== 'home') S.ui.view = 'analysis';
    if (S.ui.view === 'report' && (k === 'analysis' || k === 'figure' || k === 'table')) S.ui.view = 'analysis';
    if ((k === 'analysis' || k === 'figure' || k === 'table') && !A.curTab() && S.P.tabs.length) S.ui.sel = S.P.tabs[0].id;
    A.render();
  }
  function stepRtab(d) { var ks = A.RTABS.map(function (t) { return t[0]; }), i = ks.indexOf(S.ui.rtab); setRtab(ks[(i + d + ks.length) % ks.length]); }
  // 解析の選択肢を、表示名（まとめた選択肢は同じ名前）の並び labs の順にする。図に出さない選択肢は後ろに残す
  function setSlotOrder(sl, labs) {
    A.commit(function () {
      var at = function (c) { var k = c.on ? labs.indexOf(c.label) : -1; return k < 0 ? 1e9 : k; };
      sl.cats = sl.cats.map(function (c, k) { return [c, k]; }).sort(function (a, b) { return at(a[0]) - at(b[0]) || a[1] - b[1]; }).map(function (x) { return x[0]; });
    }, { quiet: true });
    A.syncResults(); A.render();
  }
  // 列の選択肢の並びを変え、その列を使っている解析の選択肢も同じ並びにする（まとめ方・チェックの有無はそのまま）
  function reorderVar(x, change) {
    A.commit(function () {
      if (!x.cats0) x.cats0 = x.cats.map(function (c) { return c.v; });          // 読み込んだときの並び（「自動の並びに戻す」用）
      change();
      var ord = x.cats.map(function (c) { return c.v; }), at = function (c) { var k = ord.indexOf(c.v); return k < 0 ? 1e9 : k; };
      S.P.tabs.forEach(function (t) {
        Object.keys(t.slots || {}).forEach(function (id) {
          var sl = t.slots[id];
          if (!sl || !sl.cats || !(sl.col === x.col || (sl.cols && sl.cols.indexOf(x.col) >= 0))) return;
          sl.cats = sl.cats.map(function (c, k) { return [c, k]; }).sort(function (a, b) { return at(a[0]) - at(b[0]) || a[1] - b[1]; }).map(function (p) { return p[0]; });
        });
      });
    }, { quiet: true });
    A.syncResults(); A.render();
  }
  function setView(v) { S.ui.view = v; if (v === 'data') S.ui.rtab = 'data'; else if (v === 'report') S.ui.rtab = 'export'; else if (S.ui.rtab === 'data') S.ui.rtab = 'home'; A.render(); }
  async function changeVar(x, type, delim) {
    if (x.comp) { A.toast(x.comp.kind === 'period' ? _t('日時で分けた回の列の種類は変えられません。') : _t('尺度得点の列の種類は変えられません。'), true); A.render(); return; }
    var needCats = ['nominal', 'ordinal', 'multi'].indexOf(type) >= 0, cats = x.cats, same = (type === 'multi') === (x.type === 'multi') && delim === x.delim && A.isCat(x);
    try {
      if (needCats && !same) { var d = await A.apiData('/api/varinfo', { col: x.col, type: type === 'multi' ? 'multi' : 'nominal', delim: delim }); if (!d.ok) throw new Error(d.message); cats = d.cats; }
      A.commit(function () {
        x.type = type; x.delim = delim; x.cats = needCats ? cats : [];
        S.P.tabs.forEach(function (t) { var d2 = A.def(t.type); d2.slots.forEach(function (sd) { var sl = t.slots[sd.id]; if (sl.cols) { if (sl.cols.indexOf(x.col) >= 0 && sd.types.indexOf(type) < 0) A.setItems(t, sd.id, sl.cols.filter(function (c) { return c !== x.col; })); else if (sl.cols.indexOf(x.col) >= 0) A.setItems(t, sd.id, sl.cols); return; } if (sl.col === x.col) t.slots[sd.id] = sd.types.indexOf(type) >= 0 ? { col: x.col, cats: needCats ? A.defaultCats(x, sd) : [] } : { col: null, cats: [] }; }); if (t.filter && t.filter.col === x.col) t.filter = null; });
      }, { quiet: true });
      A.syncResults(); A.render();
    } catch (e) { A.toast(e.message, true); A.render(); }
  }

  // ---------------- クリック
  document.addEventListener('click', function (e) {
    if (!e.target.closest('.md-menu, .md-colorpop, .md-palpop, [data-act="fileMenu"], [data-act="pickColor"], [data-act="pickPath"], [data-act="palettePop"], [data-act="anMenu"], [data-el]') || legDragged) closePop();
    var an = e.target.closest('.md-an'), fw = e.target.closest('.md-figwrap'), tb = e.target.closest('.md-tables');
    if (an && !e.target.closest('[data-act="toggleAn"], [data-act="delFigAt"], [data-ancheck]')) {
      var rt = fw ? 'figure' : tb ? 'table' : null;
      if (rt && ['figure', 'table', 'export'].indexOf(S.ui.rtab) >= 0 && S.ui.rtab !== 'export') rt = fw ? 'figure' : 'table';
      if (rt && S.ui.rtab === 'export') rt = null;
      selectTab(an.dataset.an, fw ? +fw.dataset.fig.split(':')[1] : null, rt);
    }
    var el = e.target.closest('[data-act]');
    if (el && !el.disabled) { e.preventDefault(); act[el.dataset.act] && act[el.dataset.act](el.dataset.arg, el, e); return; }
    el = e.target.closest('[data-toggle]'); if (el && !el.disabled) { setPath(el.dataset.toggle, !A.get(el.dataset.toggle)); return; }
    el = e.target.closest('[data-radio]'); if (el && !el.disabled) { var inPop = !!el.closest('.md-palpop'); setPath(el.dataset.radio, el.dataset.val); if (inPop) closePop(); return; }
    el = e.target.closest('[data-rtab]'); if (el) { setRtab(el.dataset.rtab); return; }
    el = e.target.closest('[data-view]'); if (el && !el.disabled) { setView(el.dataset.view); return; }
    el = e.target.closest('[data-varrow]'); if (el && !e.target.closest('input')) { S.ui.selVar = +el.dataset.varrow; S.ui.rtab = 'data'; A.render(); return; }
    el = e.target.closest('.md-merged'); if (el) { var inp = el.parentNode.querySelector('[data-cat$=":label"]'), p = inp.dataset.cat.split(':'), tab = A.curTab(), c = tab.slots[p[0]].cats[+p[1]]; A.commit(function () { c.label = c.v; }, { run: tab.id }); return; }
    el = e.target.closest('.md-figwrap [data-el^="legend:"]'); if (el && !legDragged) pickSeriesColor(+el.getAttribute('data-el').split(':')[1], e.clientX, e.clientY + 8);
    legDragged = false;
  });
  document.addEventListener('focusin', function (e) { var r = e.target.closest && e.target.closest('[data-varrow]'); if (r && S.ui.selVar !== +r.dataset.varrow) { S.ui.selVar = +r.dataset.varrow; A.renderPane(); document.querySelectorAll('tr[data-varrow]').forEach(function (tr) { tr.classList.toggle('sel', +tr.dataset.varrow === S.ui.selVar); }); } });

  // ---------------- 入力
  document.addEventListener('change', function (e) {
    var el = e.target, tab = A.curTab();
    if (el.id === 'md_pane_sel') { setRtab(el.value); return; }
    if (el.dataset.cust) { setCustom(el.dataset.cust, el.value); return; }
    if (el.dataset.ancheck) { var t = A.tabById(el.dataset.ancheck); if (t) A.commit(function () { t.checked = el.checked; }, { key: 'check' }); return; }
    if (el.dataset.set) { if (el.type === 'radio' && !el.checked) return; setPath(el.dataset.set, parseVal(el)); return; }
    if (el.dataset.legname !== undefined) { renameSeries(+el.dataset.legname, el.value); return; }
    if (el.dataset.additem) {
      var t3 = A.tabById(el.dataset.tab); if (!t3 || el.value === '') return;
      var cols3 = (t3.slots[el.dataset.additem].cols || []).concat([+el.value]); S.ui.sel = t3.id;
      A.commit(function () { A.setItems(t3, el.dataset.additem, cols3); }, { run: t3.id });
      return;
    }
    if (el.dataset.slot) {
      var st2 = A.tabById(el.dataset.tab) || tab; if (!st2) return;
      S.ui.sel = st2.id;
      A.commit(function () { A.setSlot(st2, el.dataset.slot, el.value === '' ? null : +el.value); }, { run: st2.id });
      return;
    }
    if (el.dataset.cat && tab) {
      var p = el.dataset.cat.split(':'), sl = tab.slots[p[0]], c = sl.cats[+p[1]];
      A.commit(function () {
        if (p[2] === 'on') c.on = el.checked;
        else { var nv = el.value.trim() || c.v, old = c.label, group = sl.cats.filter(function (k) { return k !== c && k.on && k.label === old; }); c.label = nv; if (group.length && !sl.cats.some(function (k) { return k !== c && group.indexOf(k) < 0 && k.label === nv; })) group.forEach(function (k) { k.label = nv; }); }
      }, { run: tab.id }); return;
    }
    if (el.dataset.filter && tab) {
      A.commit(function () {
        if (el.dataset.filter === 'col') tab.filter = el.value === '' ? null : { col: +el.value, values: [] };
        else { var fv = A.v(tab.filter.col), val = fv.cats[+el.dataset.idx].v, i = tab.filter.values.indexOf(val); if (el.checked && i < 0) tab.filter.values.push(val); if (!el.checked && i >= 0) tab.filter.values.splice(i, 1); }
      }, { run: tab.id }); return;
    }
    if (el.dataset.varlabel) {
      var x = A.v(+el.dataset.varlabel), nv2 = el.value.trim(); if (!x || !nv2 || nv2 === x.label) { A.render(); return; }
      A.commit(function () { x.label = nv2; S.P.tabs.forEach(A.autoName); }, { quiet: true }); A.syncResults(); A.render(); return;
    }
    if (el.dataset.varscore || el.dataset.varpt !== undefined) {
      var xs = A.v(S.ui.selVar); if (!xs) return;
      if (el.dataset.varscore) {
        var cur = A.catPoints(xs);                                    // 「自分で入れる」に変えたときは、今の点を初期値にする
        A.commit(function () { var old = xs.score || {}; xs.score = { mode: el.value, pts: el.value === 'custom' && A.scoreMode(xs) !== 'custom' ? cur : (old.pts || {}) }; }, { quiet: true });
      } else {
        var c2 = xs.cats[+el.dataset.varpt], t = el.value.trim().replace(/[０-９．－]/g, function (ch) { return String.fromCharCode(ch.charCodeAt(0) - 0xFEE0); }).replace(/−/g, '-');
        if (t !== '' && !isFinite(+t)) { A.toast(_t('点には数を入れてください（例：5、2.5、-1）。'), true); A.render(); return; }
        A.commit(function () { xs.score = xs.score || { mode: 'custom', pts: {} }; xs.score.pts = Object.assign({}, xs.score.pts); xs.score.pts[c2.v] = t === '' ? '' : +t; }, { quiet: true });
      }
      A.syncResults(); A.render(); return;
    }
    if (el.dataset.var) { var xv = A.v(S.ui.selVar); if (!xv) return; changeVar(xv, el.dataset.var === 'type' ? el.value : xv.type, el.dataset.var === 'delim' ? el.value : xv.delim); return; }
    if (el.dataset.reptext) A.render('chrome');
  });
  document.addEventListener('input', function (e) {
    var el = e.target;
    if (!el.dataset || !el.dataset.reptext) return;
    var tab = A.tabById(el.dataset.reptext), res = resOf(tab); if (!tab) return;
    A.commit(function () { tab.reportText = el.value; tab.reportBase = res && res.ok ? res.report : null; }, { quiet: true, key: 'rep:' + tab.id });
    A.autoGrow(el);
    document.querySelectorAll('textarea[data-reptext="' + tab.id + '"]').forEach(function (o) { if (o !== el) { o.value = el.value; A.autoGrow(o); } });
  });
  document.addEventListener('keydown', function (e) {
    var inText = /^(INPUT|TEXTAREA|SELECT)$/.test(e.target.tagName), k = e.key.toLowerCase();
    if (e.key === 'Escape') { var ed = $('.md-figedit, .md-an-rename'); if (ed) { ed.dataset.cancel = '1'; ed.blur(); } else if ($('.md-menu') || $('.md-colorpop') || $('.md-palpop')) closePop(); else if ($('#md_modal')) closeModal(); else if (inText) e.target.blur(); return; }
    if (e.key === 'Enter' && e.target.tagName === 'INPUT' && e.target.type !== 'checkbox') { e.target.blur(); return; }
    if (e.key === 'F2' && !inText && S.ui.sel) { e.preventDefault(); renameAn(S.ui.sel); return; }
    if (!(e.ctrlKey || e.metaKey)) return;
    if (k === 's') { e.preventDefault(); act.save(); }
    else if (k === 'o') { e.preventDefault(); act.openCsv(); }
    else if (!inText && k === 'z' && !e.shiftKey) { e.preventDefault(); A.undo(); }
    else if (!inText && (k === 'y' || (k === 'z' && e.shiftKey))) { e.preventDefault(); A.redo(); }
  });

  // ---------------- 説明（ツールチップ）
  var tipT = null, tipEl = null;
  document.addEventListener('mouseover', function (e) {
    var el = e.target.closest && e.target.closest('[data-tip]'); if (el === tipEl) return;
    clearTimeout(tipT); $('#md_tip').style.display = 'none'; tipEl = el; if (!el) return;
    tipT = setTimeout(function () {
      var t = $('#md_tip'); t.textContent = el.getAttribute('data-tip'); t.style.display = 'block';
      var r = el.getBoundingClientRect(), tr = t.getBoundingClientRect(), y = r.bottom + 6; if (y + tr.height > window.innerHeight - 4) y = r.top - tr.height - 6;
      t.style.left = clamp(r.left + r.width / 2 - tr.width / 2, 4, window.innerWidth - tr.width - 4) + 'px'; t.style.top = Math.max(4, y) + 'px';
    }, 400);
  });
  document.addEventListener('mousedown', function () { clearTimeout(tipT); $('#md_tip').style.display = 'none'; tipEl = null; });

  // ---------------- 図を直接さわる
  document.addEventListener('dblclick', function (e) {
    var nm = e.target.closest && e.target.closest('[data-anname]'); if (nm) { renameAn(nm.dataset.anname); return; }
    var el = e.target.closest && e.target.closest('.md-figwrap [data-el]'); if (!el) return;
    var wrap = el.closest('.md-figwrap'), p = wrap.dataset.fig.split(':'), key = el.getAttribute('data-el');
    if (/^legname:/.test(key)) { closePop(); S.ui.sel = p[0]; S.ui.selFig = +p[1]; editLegName(wrap, el, +key.split(':')[1]); return; }
    if (['vTitle', 'cTitle'].indexOf(key) < 0) return;
    S.ui.sel = p[0]; S.ui.selFig = +p[1];
    var wr = wrap.getBoundingClientRect(), r = el.getBoundingClientRect(), z = zoom(wrap), inp = document.createElement('input'), tab = A.curTab(), cur = A.curFig()[key];
    if (cur === null || cur === undefined) cur = el.getAttribute('data-full') || el.textContent;
    inp.className = 'md-figedit'; inp.value = cur; var w = Math.max(180, r.width / z + 30);
    inp.style.width = w + 'px'; inp.style.left = clamp((r.left - wr.left) / z + r.width / z / 2 - w / 2, 0, Math.max(0, wr.width / z - w)) + 'px'; inp.style.top = clamp((r.top - wr.top) / z + r.height / z / 2 - 13, 0, Math.max(0, wr.height / z - 26)) + 'px';
    wrap.appendChild(inp); inp.focus(); inp.select();
    inp.addEventListener('blur', function () { var cancel = inp.dataset.cancel, val = inp.value; inp.remove(); if (!cancel && val !== cur) setPath('fig.' + key, val); });
  });
  function editLegName(wrap, el, i) {
    var tab = A.curTab(), fig = A.curFig(), m = Chart.model(resOf(tab)); if (!fig || !m) return;
    var lab = Chart.seriesFor(m, fig)[i], cur = (fig.legNames || {})[lab] || lab, g = el.closest('[data-legend]') || el;
    var wr = wrap.getBoundingClientRect(), r = g.getBoundingClientRect(), er = el.getBoundingClientRect(), z = zoom(wrap), inp = document.createElement('input'), w = Math.max(160, r.width / z + 20);
    inp.className = 'md-figedit'; inp.value = cur; inp.style.width = w + 'px';
    inp.style.left = clamp((er.left - wr.left) / z - 4, 0, Math.max(0, wr.width / z - w)) + 'px'; inp.style.top = clamp((er.top - wr.top) / z + er.height / z / 2 - 13, 0, Math.max(0, wr.height / z - 26)) + 'px';
    wrap.appendChild(inp); inp.focus(); inp.select();
    inp.addEventListener('mousedown', function (ev) { ev.stopPropagation(); });
    inp.addEventListener('blur', function () { var cancel = inp.dataset.cancel, val = inp.value; inp.remove(); if (!cancel) renameSeries(i, val); });
  }
  // 凡例とカラーバーをドラッグで動かす。凡例は右・上・下の端に寄せると図の外に並べ、それ以外は置いた位置に置く
  var legDragged = false;
  document.addEventListener('mousedown', function (e) {
    if (e.button !== 0) return;
    var g = e.target.closest && e.target.closest('.md-figwrap [data-legend], .md-figwrap [data-cbar]'); if (!g) return;
    var wrap = g.closest('.md-figwrap'), p = wrap.dataset.fig.split(':'), tab = A.tabById(p[0]), k = +p[1], fig = tab && tab.figs[k]; if (!fig) return;
    e.preventDefault();
    var isLeg = g.hasAttribute('data-legend'), svg = wrap.querySelector('svg'), W = +svg.getAttribute('width'), H = +svg.getAttribute('height'), z = zoom(wrap), x0 = e.clientX, y0 = e.clientY;
    var box = (g.getAttribute('data-box') || '0,0,0,0').split(',').map(Number), moved = false, zone = null, dx = 0, dy = 0, ind = document.createElement('div');
    ind.className = 'md-snap';
    var ZONES = isLeg ? { right: [W * 0.82, 0, W * 0.18, H, _t('右に並べる')], bottom: [0, H * 0.86, W * 0.82, H * 0.14, _t('下に並べる')], top: [0, 0, W * 0.82, H * 0.12, _t('上に並べる')] }
      : { right: [W * 0.75, 0, W * 0.25, H, _t('右に置く')], bottom: [0, H * 0.75, W * 0.75, H * 0.25, _t('下に置く')] };
    function mv(ev) {
      dx = (ev.clientX - x0) / z; dy = (ev.clientY - y0) / z;
      if (!moved && Math.abs(dx) + Math.abs(dy) < 4) return;
      if (!moved) { moved = true; wrap.appendChild(ind); document.body.classList.add('md-moving'); $('#md_tip').style.display = 'none'; }
      g.setAttribute('transform', 'translate(' + dx + ',' + dy + ')');
      var r = svg.getBoundingClientRect(), fx = (ev.clientX - r.left) / z, fy = (ev.clientY - r.top) / z;
      if (isLeg) zone = fx > W * 0.82 ? 'right' : fy > H * 0.86 ? 'bottom' : fy < H * 0.12 ? 'top' : 'free';
      else zone = fx / W - 0.5 > fy / H - 0.5 ? 'right' : 'bottom';
      var zr = ZONES[zone];
      if (zr) { ind.style.display = 'flex'; ind.style.left = zr[0] + 'px'; ind.style.top = zr[1] + 'px'; ind.style.width = zr[2] + 'px'; ind.style.height = zr[3] + 'px'; ind.textContent = zr[4]; }
      else ind.style.display = 'none';
      $('#md_status .md-st-mode').textContent = zone === 'free' ? _t('凡例を移動：置いた位置に置きます') : _t('凡例を移動：') + (zr ? zr[4] : '');
    }
    function up() {
      document.removeEventListener('mousemove', mv); document.removeEventListener('mouseup', up); document.body.classList.remove('md-moving'); ind.remove();
      if (!moved) return;
      legDragged = true; setTimeout(function () { legDragged = false; }, 300);
      S.ui.sel = tab.id; S.ui.selFig = k;
      A.commit(function () {
        if (!isLeg) { fig.bar = zone; return; }
        if (zone === 'free') { fig.legend = 'free'; fig.legX = Math.round(clamp(box[0] + dx, 0, Math.max(0, W - box[2]))); fig.legY = Math.round(clamp(box[1] + dy, 0, Math.max(0, H - box[3]))); }
        else fig.legend = zone;
      });
    }
    document.addEventListener('mousemove', mv); document.addEventListener('mouseup', up);
  });
  // 表の右端・下端・右下の角をドラッグすると、表の幅と行の高さが変わる（同じ解析の表すべてに当てはめる）
  document.addEventListener('mousedown', function (e) {
    var hd = e.target.closest && e.target.closest('.md-th'); if (!hd || e.button !== 0) return;
    e.preventDefault();
    var wrap = hd.closest('.md-tblwrap'), p = wrap.dataset.tbl.split(':'), tab = A.tabById(p[0]); if (!tab) return;
    var tbl = wrap.querySelector('table.md-t'), trs = [].slice.call(tbl.rows), dir = hd.dataset.h, z = (S.P.settings.zoom || 100) / 100;
    var r0 = tbl.getBoundingClientRect(), w0 = r0.width / z, rh0 = trs.reduce(function (a, r) { return a + r.getBoundingClientRect().height; }, 0) / z / trs.length, x0 = e.clientX, y0 = e.clientY, last = null, raf = 0, nw = null, nh = null;
    // 中身に合わせた幅（これより狭くはしない。画像・PDF の表と同じ決まり）
    var keepW = tbl.style.width; tbl.style.width = ''; var wNat = tbl.getBoundingClientRect().width / z; tbl.style.width = keepW;
    S.ui.sel = tab.id;
    document.body.classList.add('md-resizing-' + dir);
    function draw() {
      raf = 0; if (!last) return;
      if (dir !== 's') { nw = Math.max(Math.ceil(wNat), Math.round(w0 + (last.clientX - x0) / z)); tbl.style.width = nw + 'px'; }
      if (dir !== 'e') { nh = Math.max(12, Math.round(rh0 + (last.clientY - y0) / z / Math.max(1, trs.length))); trs.forEach(function (r) { r.style.height = nh + 'px'; }); }
    }
    function mv(ev) { last = ev; if (!raf) raf = requestAnimationFrame(draw); }
    function up() {
      document.removeEventListener('mousemove', mv); document.removeEventListener('mouseup', up); document.body.classList.remove('md-resizing-' + dir);
      if (raf) { cancelAnimationFrame(raf); draw(); }
      if (nw === null && nh === null) return;
      var aw = Math.round(tbl.getBoundingClientRect().width / z);              // 中身より狭くはならないので、実際の幅を記録する
      A.commit(function () { if (nw !== null) { if (aw <= Math.ceil(wNat) + 1) delete tab.tstyle.tw; else tab.tstyle.tw = aw; } if (nh !== null) tab.tstyle.rowH = nh; });
    }
    document.addEventListener('mousemove', mv); document.addEventListener('mouseup', up);
  });
  document.addEventListener('dblclick', function (e) {
    var hd = e.target.closest && e.target.closest('.md-th'); if (!hd) return;
    var tab = A.tabById(hd.closest('.md-tblwrap').dataset.tbl.split(':')[0]); if (!tab) return;
    A.commit(function () { delete tab.tstyle.tw; delete tab.tstyle.rowH; });
  });
  // 図の右端・下端・右下の角をドラッグすると、その大きさに描き直しながら変わる
  document.addEventListener('mousedown', function (e) {
    var hd = e.target.closest && e.target.closest('.md-fh'); if (!hd || e.button !== 0) return;
    e.preventDefault();
    var wrap = hd.closest('.md-figwrap'), p = wrap.dataset.fig.split(':'), tab = A.tabById(p[0]), k = +p[1], fig = tab.figs[k], m = Chart.model(resOf(tab)), dir = hd.dataset.h;
    S.ui.sel = tab.id; S.ui.selFig = k;
    var snap = JSON.stringify(S.P), z = zoom(wrap), x0 = e.clientX, y0 = e.clientY, r0 = Chart.render(m, fig, S.P.settings.figLang), w0 = r0.w, h0 = r0.h, raf = 0, last = null, cap = wrap.parentNode.querySelector('.md-figsize');
    document.body.classList.add('md-resizing-' + dir);
    function draw() {
      raf = 0; if (!last) return;
      fig.fit = false;
      if (dir !== 's') fig.w = clamp(Math.round(w0 + (last.clientX - x0) / z), 160, 2400);
      if (dir !== 'e') fig.h = clamp(Math.round(h0 + (last.clientY - y0) / z), 120, 2400);
      if (dir === 's') fig.w = w0; if (dir === 'e') fig.h = h0;
      var r = Chart.render(m, fig, S.P.settings.figLang), old = wrap.querySelector('svg'), tmp = document.createElement('div'); tmp.innerHTML = r.svg; old.replaceWith(tmp.firstChild);
      if (cap) cap.textContent = r.w + ' × ' + r.h;
    }
    function mv(ev) { last = ev; if (!raf) raf = requestAnimationFrame(draw); }
    function up() {
      document.removeEventListener('mousemove', mv); document.removeEventListener('mouseup', up); document.body.classList.remove('md-resizing-' + dir);
      if (raf) { cancelAnimationFrame(raf); draw(); }
      if (JSON.stringify(S.P) !== snap) { S.undo.push(snap); if (S.undo.length > 100) S.undo.shift(); S.redo = []; A.markDirty(); }
      A.render();
    }
    document.addEventListener('mousemove', mv); document.addEventListener('mouseup', up);
  });
  document.addEventListener('contextmenu', function (e) {
    var fw = e.target.closest && e.target.closest('.md-figwrap'), hd = e.target.closest && e.target.closest('.md-an-head');
    if (fw) {
      e.preventDefault(); var p = fw.dataset.fig.split(':'); selectTab(p[0], +p[1]); var fig = A.curFig();
      var li = e.target.closest('[data-el^="legend:"], [data-el^="legname:"]'), ex = e.clientX, ey = e.clientY;
      if (li) { var si = +li.getAttribute('data-el').split(':')[1], nmEl = fw.querySelector('[data-el="legname:' + si + '"]') || li;
        menu(ex, ey, [{ name: _t('この系列の名前を変更'), run: function () { editLegName(fw, nmEl, si); } }, { name: _t('この系列の色を変える'), run: function () { pickSeriesColor(si, ex, ey + 8); } }, '-',
          { name: fig.legFrame ? _t('凡例の枠を消す') : _t('凡例を枠で囲む'), run: function () { setPath('fig.legFrame', !fig.legFrame); } }, { name: _t('凡例を消す'), run: function () { setPath('fig.legend', 'none'); } }]); return; }
      menu(e.clientX, e.clientY, [{ name: _t('図をコピー'), run: act.figCopy }, { name: _t('画像で保存'), run: act.figPng }, { name: _t('SVG で保存'), run: act.figSvg }, '-',
        { name: fig.transparent ? _t('背景を白にする') : _t('背景を透明にする'), run: function () { setPath('fig.transparent', !fig.transparent); } },
        { name: fig.labels ? _t('値のラベルを消す') : _t('値のラベルを出す'), run: function () { setPath('fig.labels', !fig.labels); } },
        { name: fig.legend === 'none' ? _t('凡例を出す') : _t('凡例を消す'), run: function () { setPath('fig.legend', fig.legend === 'none' ? 'right' : 'none'); } }, '-',
        { name: _t('図を追加'), run: act.addFig }, { name: _t('この図を削除'), danger: true, run: act.delFig }]);
    } else if (hd && !e.target.closest('input')) { e.preventDefault(); anMenuAt(hd.dataset.anhead, e.clientX, e.clientY); }
  });

  // ---------------- 解析の並べ替え（見出しをドラッグ）
  var dragId = null;
  function clearDrop() { document.querySelectorAll('.md-an.drop-before, .md-an.drop-after').forEach(function (s) { s.classList.remove('drop-before', 'drop-after'); }); }
  document.addEventListener('dragstart', function (e) {
    var hd = e.target.closest && e.target.closest('.md-an-head'); if (!hd || e.target.closest('input, button')) { if (hd) e.preventDefault(); return; }
    dragId = hd.dataset.anhead; e.dataTransfer.effectAllowed = 'move'; try { e.dataTransfer.setData('text/plain', dragId); } catch (err) {}
    hd.parentNode.classList.add('dragging');
  });
  document.addEventListener('dragover', function (e) {
    if (!dragId) return; var an = e.target.closest && e.target.closest('.md-an'); if (!an) return;
    e.preventDefault(); clearDrop(); if (an.dataset.an === dragId) return;
    var r = an.getBoundingClientRect(); an.classList.add(e.clientY < r.top + r.height / 2 ? 'drop-before' : 'drop-after');
  });
  document.addEventListener('drop', function (e) {
    if (!dragId) return; var an = e.target.closest && e.target.closest('.md-an'), id = dragId; e.preventDefault();
    var before = an && an.classList.contains('drop-before'); clearDrop(); dragId = null;
    if (!an || an.dataset.an === id) { A.render(); return; }
    A.commit(function () { var i = S.P.tabs.findIndex(function (t) { return t.id === id; }), t = S.P.tabs.splice(i, 1)[0], j = S.P.tabs.findIndex(function (x) { return x.id === an.dataset.an; }); S.P.tabs.splice(before ? j : j + 1, 0, t); });
  });
  document.addEventListener('dragend', function () { dragId = null; clearDrop(); document.querySelectorAll('.md-an.dragging').forEach(function (s) { s.classList.remove('dragging'); }); });

  // ---------------- ファイルのドロップ
  var dragN = 0;
  function hasFiles(e) { return !dragId && e.dataTransfer && [].indexOf.call(e.dataTransfer.types || [], 'Files') >= 0; }
  window.addEventListener('dragenter', function (e) { if (!hasFiles(e)) return; e.preventDefault(); dragN++; document.body.classList.add('md-dragover'); });
  window.addEventListener('dragover', function (e) { if (hasFiles(e)) e.preventDefault(); });
  window.addEventListener('dragleave', function (e) { if (!hasFiles(e)) return; dragN--; if (dragN <= 0) { dragN = 0; document.body.classList.remove('md-dragover'); } });
  window.addEventListener('drop', function (e) { if (!hasFiles(e)) return; e.preventDefault(); dragN = 0; document.body.classList.remove('md-dragover'); var f = e.dataTransfer.files[0]; if (f) A.handleFile(f); });
  window.addEventListener('beforeunload', function (e) { if (S.dirty && S.data && !window.__noGuard) { e.preventDefault(); e.returnValue = ''; } });
  window.addEventListener('pagehide', function () { try { navigator.sendBeacon('/api/bye'); } catch (e) {} });

  // ---------------- 起動
  async function ping() { try { var d = await (await fetch('/api/ping')).json(); S.rYear = d.ryear || null; if (S.r !== d.r) { S.r = d.r; A.renderStatus(); } } catch (e) { if (S.r !== null) { S.r = null; A.renderStatus(); } } }
  async function init() {
    A.render();
    try { var d = await A.api('/api/registry', {}); S.registry = d.analyses || []; } catch (e) { A.toast(e.message, true); }
    await ping(); setInterval(ping, 5000);
    S.projList = await A.listProjects();
    A.render();
    showChanges();
  }
  // ---------------- 言語を切り替えたとき：R の解析の一覧を取り直し、すべての解析を計算し直して、画面を描き直す
  window.CTS_ONLANG.push(async function (lang) {
    try { var d = await A.api('/api/registry', {}); S.registry = d.analyses || []; } catch (e) { A.toast(e.message, true); }
    if (S.P) {
      if (lang === 'en' && S.P.settings) S.P.settings.figLang = 'en';      // 英語の画面で、図に日本語の軸名が入らないようにする
      S.P.tabs.forEach(function (t) { A.autoName && A.autoName(t); });
      S.results = {}; A.scheduleAll();
    }
    A.render(); refreshSettings();
    if (document.querySelector('#md_modal .md-welcome')) { closeModal(); showChanges(); return; }   // 初めての人の案内は、選んだ言語で出し直す
    A.toast(lang === 'en' ? 'The language was changed to English. All analyses are being recalculated.' : '言語を日本語にしました。すべての解析を計算し直しています。');
  });
  // ---------------- 新しい版で初めて起動したとき：変わった点を見せる（初めて使う人には出さない）
  function showChanges() {
    var list = window.CTS_CHANGES || [], cur = window.CTS_APP_VERSION, KEY = 'cts-seen-version', seen = null;
    if (!cur) return;
    try { seen = localStorage.getItem(KEY); } catch (e) { return; }
    function mark() { try { localStorage.setItem(KEY, cur); } catch (e) {} }
    if (seen === cur) return;
    var hello = _t('<div class="md-welcome"><div class="md-welcome-h">ようこそ</div>');
    if (!seen) {                                                     // 初めて使う人：アプリの紹介と最初の一歩
      modal('CrossTab Studio', hello + '<div class="md-row" style="gap:6px;margin:2px 0 6px;align-items:center"><span>Language / 言語</span>' + [['ja', '日本語'], ['en', 'English']].map(function (l) { return '<button class="md-btn md-btn-sm' + (window.CTS_LANG === l[0] ? ' md-theme-on' : '') + '" data-act="setLang" data-arg="' + l[0] + '">' + l[1] + '</button>'; }).join('') + '</div>' + _t('<p>アンケートの回答（CSV ファイル）を読み込むと、集計・検定・図・報告書までを作れます。データはこの端末の中だけで処理され、外部には送られません。</p><p>まずは画面の「ファイルを選ぶ」から、回答の CSV を開いてください。</p><p class="md-welcome-v">バージョン ') + esc(cur) + '</p></div>',
        _t('<button class="md-btn md-btn-primary" id="md_chg_ok">はじめる</button>'), 520);
      $('#md_chg_ok').onclick = function () { mark(); closeModal(); };
      var bk = $('#md_modal'); if (bk) bk.addEventListener('mousedown', function (e) { if (e.target.closest && e.target.closest('[data-act=setLang]')) return; mark(); });
      return;
    }
    var newer = list.filter(function (c) { return vcmp(c.v, seen) > 0; });
    if (!newer.length) { mark(); return; }
    var body = hello + _t('<p>新しい版（バージョン {0}）に更新しました。この端末でお使いだったバージョン {1} からの変更点{2}は次のとおりです。</p></div>', esc(cur), esc(seen), newer.length > 1 ? _t('（{0} 版分、新しい順）', newer.length) : '') + newer.map(function (c) { return '<div class="md-chg"><div class="md-chg-v">' + esc(c.v) + (c.date ? '<span>' + esc(c.date) + '</span>' : '') + '</div><ul>' + c.items.map(function (t) { return '<li>' + esc(_t(t)) + '</li>'; }).join('') + '</ul></div>'; }).join('');
    modal('CrossTab Studio', body, _t('<button class="md-btn md-btn-primary" id="md_chg_ok">わかりました</button>'), 560);
    $('#md_chg_ok').onclick = function () { mark(); closeModal(); };
    var back = $('#md_modal'); if (back) back.addEventListener('mousedown', mark);
  }
  // ---------------- 日時で回に分ける：タイムスタンプの区切りの日時で「第 1 回」「第 2 回」…の列を作る
  //   日時は、時刻帯を考えない「1970-01-01 0:00 からの分」で扱う（R の time_minutes と同じ）
  var PD = null;
  function pdLocal(m) { var d = new Date(Math.round(m) * 60000), p = function (n) { return ('0' + n).slice(-2); }; return d.getUTCFullYear() + '-' + p(d.getUTCMonth() + 1) + '-' + p(d.getUTCDate()) + 'T' + p(d.getUTCHours()) + ':' + p(d.getUTCMinutes()); }
  function pdMin(str) { var m = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})/.exec(str || ''); return m ? Date.UTC(+m[1], +m[2] - 1, +m[3], +m[4], +m[5]) / 60000 : NaN; }
  function pdShow(m) { return pdLocal(m).replace('T', ' ').replace(/-/g, '/'); }
  // 区切りの候補：回答が 3 日以上とぎれている所の、次の回答の日の 0:00（20 か所まで）
  function pdSuggest(t) {
    var out = [];
    for (var i = 1; i < t.length && out.length < 20; i++) if (t[i] - t[i - 1] >= 3 * 1440) out.push(Math.floor(t[i] / 1440) * 1440);
    return out;
  }
  async function periodDialog(col) {
    var times = S.P.vars.filter(function (y) { return y.type === 'time'; });
    if (!times.length) { A.toast(_t('日時の列がありません。「データ」でタイムスタンプの列の種類を「日時」にしてください。'), true); return; }
    var x = col ? A.v(col) : null, c = x ? x.comp : null;
    var st = { src: c ? c.src : times[0].col, name: x ? x.label : _t('回'), same: c ? !!c.same : false, rows: [], t: [], msg: '' };
    if (c) st.rows = c.names.map(function (nm, k) { return { name: nm, start: k ? c.breaks[k - 1] : null }; });
    async function load(resetRows) {
      var d = await A.apiData('/api/timeinfo', { col: st.src });
      if (!d.ok) { A.toast(d.message || _t('日時を読み取れませんでした。'), true); return false; }
      st.t = d.t || [];
      if (!st.t.length) { A.toast(_t('この列から日時を読み取れませんでした。'), true); return false; }
      st.bad = d.bad || 0;
      if (resetRows) auto(true);
      return true;
    }
    function auto(quiet) {
      var t = st.t, br = pdSuggest(t);
      if (br.length) st.msg = _t('回答が 3 日以上とぎれている所で区切りました（{0} か所）。区切りの日時と名前は直せます。', br.length);
      else {
        var mid = Math.floor((t[0] + (t[t.length - 1] - t[0]) / 2) / 1440) * 1440;
        if (mid <= t[0]) mid = t[0] + 1;
        br = [mid];
        st.msg = _t('回答が大きくとぎれている所が見つからなかったので、仮に期間の真ん中の日で区切りました。区切りの日時を直してください。');
      }
      st.rows = [{ name: _t('第 {0} 回', 1), start: null }].concat(br.map(function (b, k) { return { name: _t('第 {0} 回', k + 2), start: pdLocal(b) }; }));
      if (!quiet) draw();
    }
    function counts() {
      var t = st.t, br = st.rows.slice(1).map(function (r) { return pdMin(r.start); }), n = st.rows.map(function () { return 0; });
      if (br.some(isNaN)) return null;
      t.forEach(function (m) { var k = 0; while (k < br.length && m >= br[k]) k++; n[k]++; });
      return n;
    }
    function rowsHtml() {
      var n = counts() || [];
      return st.rows.map(function (r, k) {
        return '<tr><td><input type="text" data-pdname="' + k + '" value="' + esc(r.name) + '" style="width:100%"></td><td>' +
          (k ? '<input type="datetime-local" data-pdstart="' + k + '" value="' + esc(r.start || '') + '" style="width:100%">' : _t('<span class="md-hint" style="margin:0;display:block;text-align:left;white-space:normal">最初の回答（{0}）から</span>', pdShow(st.t[0]))) +
          '</td><td class="num" data-pdn="' + k + '">' + (n[k] === undefined ? '–' : n[k]) + '</td><td>' + (k && st.rows.length > 2 ? '<button class="md-btn md-btn-sm" data-act="periodDelRow" data-arg="' + k + _t('" data-tip="この区切りをなくし、前の回とまとめます。">×</button>') : '') + '</td></tr>';
      }).join('');
    }
    function draw() {
      var m = document.getElementById('md_modal'); if (!m) return;
      m.querySelector('#md_pd_rows').innerHTML = rowsHtml();
      m.querySelector('#md_pd_msg').textContent = st.msg;
      m.querySelector('#md_pd_range').textContent = _t('回答の日時：{0} 〜 {1}（{2} 件）', pdShow(st.t[0]), pdShow(st.t[st.t.length - 1]), st.t.length) + (st.bad ? _t('。日時を読み取れなかった回答 {0} 件は、どの回にも入れません。', st.bad) : '');
    }
    function refreshCounts() { var n = counts(); st.rows.forEach(function (r, k) { var e = document.querySelector('[data-pdn="' + k + '"]'); if (e) e.textContent = n ? n[k] : '–'; }); }
    if (!(await load(!c))) return;
    var d = modal(x ? _t('日時で回に分ける（直す）') : _t('日時で回に分ける'),
      '<div class="md-grid2"><label class="md-field">' + _t('日時の列') + '<select id="md_pd_src">' + times.map(function (y) { return '<option value="' + y.col + '"' + (y.col === st.src ? ' selected' : '') + '>' + esc(y.label) + '</option>'; }).join('') + '</select></label>' +
      '<label class="md-field">' + _t('作る列の名前') + '<input type="text" id="md_pd_name" value="' + esc(st.name) + '"></label></div>' +
      '<p class="md-hint" id="md_pd_range" style="margin:6px 0 2px"></p>' +
      _t('<p class="md-hint" style="margin:0 0 6px">各回は、その回の開始の日時から、次の回の開始の日時の前までです。回の名前は図・表・報告書にそのまま出ます。</p>') +
      '<div style="max-height:40vh;overflow:auto;border:1px solid var(--t-d5e3da)"><table class="md-t0" style="width:100%;table-layout:fixed"><colgroup><col style="width:34%"><col><col style="width:64px"><col style="width:40px"></colgroup><tr>' +
      _t('<th class="md-rh">回の名前</th><th class="md-rh">開始</th><th class="md-rh">回答数</th><th class="md-rh"></th>') + '</tr><tbody id="md_pd_rows"></tbody></table></div>' +
      '<div class="md-row" style="gap:8px;margin:8px 0 4px">' + _t('<button class="md-btn" data-act="periodAdd">区切りを足す</button><button class="md-btn" data-act="periodAuto" data-tip="回答がとぎれている所から、区切りを決め直します。いま入れた名前と日時は元に戻ります。">自動で区切り直す</button>') + '</div>' +
      '<p class="md-hint" id="md_pd_msg" style="margin:2px 0 8px"></p>' +
      _t('<label style="display:flex;gap:6px;align-items:flex-start"><input type="checkbox" id="md_pd_same"{0}><span>同じ人たちが複数の回に答えている（だれの回答かは特定できない）</span></label>', st.same ? ' checked' : '') +
      _t('<p class="md-hint" style="margin:2px 0 0 22px">☑ を付けると、この列を使った検定の結果と報告書に「各回の回答を独立した群として扱った近似」であることを書き添えます。回ごとに別の人が答えているときは付けません。</p>'),
      _t('<button class="md-btn" data-act="closeModal">やめる</button><button class="md-btn md-btn-primary" data-act="periodSave">{0}</button>', x ? _t('直す') : _t('作る')), 600);
    draw();
    d.addEventListener('input', function (e) {
      var el = e.target;
      if (el.dataset.pdname !== undefined) st.rows[+el.dataset.pdname].name = el.value;
      else if (el.dataset.pdstart !== undefined) { st.rows[+el.dataset.pdstart].start = el.value; refreshCounts(); }
    });
    d.querySelector('#md_pd_src').addEventListener('change', async function (e) { st.src = +e.target.value; if (await load(true)) draw(); });
    PD = {
      add: function () {
        var last = st.rows.length > 1 ? pdMin(st.rows[st.rows.length - 1].start) : st.t[0], end = st.t[st.t.length - 1];
        if (isNaN(last)) last = st.t[0];
        var b = Math.floor((last + (end - last) / 2) / 1440) * 1440; if (b <= last) b = last + 60;
        st.rows.push({ name: _t('第 {0} 回', st.rows.length + 1), start: pdLocal(b) }); draw();
      },
      del: function (k) { if (k > 0 && st.rows.length > 2) { st.rows.splice(k, 1); draw(); } },
      auto: function () { auto(false); },
      save: function () {
        var m = document.getElementById('md_modal'); if (!m) return;
        var name = m.querySelector('#md_pd_name').value.trim(), same = m.querySelector('#md_pd_same').checked;
        var names = st.rows.map(function (r) { return String(r.name || '').trim(); }), br = st.rows.slice(1).map(function (r) { return pdMin(r.start); });
        if (!name) { A.toast(_t('作る列の名前を入れてください。'), true); return; }
        if (S.P.vars.some(function (y) { return y.label === name && y.col !== col; })) { A.toast(_t('同じ名前の列があります。別の名前にしてください。'), true); return; }
        if (names.some(function (z) { return !z; })) { A.toast(_t('回の名前を入れてください。'), true); return; }
        if (names.some(function (z, i) { return names.indexOf(z) !== i; })) { A.toast(_t('回の名前が重なっています。別々の名前にしてください。'), true); return; }
        if (br.some(isNaN)) { A.toast(_t('区切りの日時を入れてください。'), true); return; }
        if (br.some(function (b, i) { return i && b <= br[i - 1]; })) { A.toast(_t('区切りの日時は、上から古い順に並べてください。'), true); return; }
        var n = counts();
        if (n.some(function (z) { return !z; })) { A.toast(_t('回答が 0 件の回があります。区切りの日時を直すか、その区切りをなくしてください。'), true); return; }
        closeModal(); PD = null;
        var cats = names.map(function (z, i) { return { v: z, n: n[i], special: false }; }), comp = { kind: 'period', src: st.src, breaks: br.map(pdLocal), names: names, same: same };
        A.commit(function () {
          var y = col ? A.v(col) : null, total = n.reduce(function (a, b) { return a + b; }, 0);
          if (y) {
            var changed = JSON.stringify(y.comp.names) !== JSON.stringify(names);
            y.label = name; y.header = name; y.comp = comp; y.cats = cats; y.n = total; y.nUnique = names.length;
            if (changed) S.P.tabs.forEach(function (t) { Object.keys(t.slots).forEach(function (k) { if (t.slots[k].col === col) A.setSlot(t, k, col); }); });
          } else {
            var nc = Math.max.apply(null, S.P.vars.map(function (z) { return z.col; })) + 1;
            S.P.vars.push({ col: nc, header: name, label: name, full: name, item: '', stem: '', type: 'ordinal', delim: ';', n: total, nUnique: names.length, cats: cats, comp: comp });
          }
        });
        A.syncResults();
        A.toast(col ? _t('「{0}」を直しました。', name) : _t('「{0}」を作りました（{1} 回）。クロス集計・t 検定・分散分析などの「比べる群」で選べます。', name, names.length));
      }
    };
  }
  function scoreDialog(col, preset) {
    var x = col ? A.v(col) : null, comp = x ? x.comp : { items: preset || [], reverse: [], method: 'mean' };
    var cand = S.P.vars.filter(function (y) { return !y.comp && (y.type === 'ordinal' || y.type === 'numeric'); });
    if (cand.length < 2) { A.toast(_t('順序のある質問（または数値の質問）が 2 つ以上必要です。「データ」で列の種類を確かめてください。'), true); return; }
    var name = x ? x.label : _t('尺度得点') + (A.compVars().length + 1);
    var rows = cand.map(function (y) {
      var on = comp.items.indexOf(y.col) >= 0, rv = (comp.reverse || []).indexOf(y.col) >= 0;
      var lv = y.type === 'numeric' ? _t('数値') : (y.cats || []).filter(function (k) { return !k.special; }).length + _t(' 段階');
      return '<tr><td><input type="checkbox" data-scitem="' + y.col + '"' + (on ? ' checked' : '') + '></td><td style="text-align:left;white-space:normal;overflow-wrap:anywhere">' + esc(y.label) + '</td><td class="num">' + lv + '</td><td><label><input type="checkbox" data-screv="' + y.col + '"' + (rv ? ' checked' : '') + _t('> 逆転</label></td></tr>');
    }).join('');
    var d = modal(x ? _t('尺度得点を直す') : _t('尺度得点を作る'),
      _t('<div class="md-grid2"><label class="md-field">名前<input type="text" id="md_sc_name" value="{0}"></label><div class="md-field">計算<div class="md-row" style="gap:12px;margin-top:4px"><label><input type="radio" name="md_sc_m" value="mean"{1}> 平均点</label><label><input type="radio" name="md_sc_m" value="sum"{2}> 合計点</label></div></div></div><p class="md-hint" style="margin:6px 0">尺度に含める項目に ☑ を付けます。逆の向きに聞いた項目（例：「海は好きではない」）には「逆転」を付けると、点数を反対にしてから計算します。すべての項目に答えた人だけ得点を出し、1 つでも答えていない人は欠損にします。平均点は、項目の数が違う尺度どうしでも比べやすくなります。</p><div style="max-height:46vh;overflow:auto;border:1px solid var(--t-d5e3da)"><table class="md-t0" style="width:100%;table-layout:fixed"><colgroup><col style="width:44px"><col><col style="width:64px"><col style="width:70px"></colgroup><tr><th class="md-rh">使う</th><th class="md-rh">項目</th><th class="md-rh">段階</th><th class="md-rh">向き</th></tr>', esc(name), comp.method !== 'sum' ? ' checked' : '', comp.method === 'sum' ? ' checked' : '') + rows + '</table></div>' +
      '<div id="md_sc_prev" class="md-hint" style="margin-top:8px"></div>',
      _t('<button class="md-btn" data-act="closeModal">やめる</button><button class="md-btn md-btn-primary" data-act="scoreSave" data-arg="') + (x ? x.col : '') + '">' + (x ? _t('直す') : _t('作る')) + '</button>', 640);
    function prev() {
      var items = [].map.call(d.querySelectorAll('[data-scitem]:checked'), function (e) { return A.v(+e.dataset.scitem); });
      var el = d.querySelector('#md_sc_prev'); if (!items.length) { el.textContent = _t('項目を選ぶと、点数の付け方を表示します。'); return; }
      var y = items.find(function (z) { return z.type === 'ordinal'; }), msg = '';
      if (y) { var cs = (y.cats || []).filter(function (k) { return !k.special; }), L = cs.length; msg = _t('点数の付け方（「{0}」の例）：{1}。選択肢の並びの先頭が高い点です。逆転の項目は反対になります。', y.label, cs.map(function (k, i) { return k.v + ' = ' + (L - i); }).join(_t('、'))); }
      var ns = {}; items.forEach(function (z) { ns[z.type === 'numeric' ? _t('数値') : (z.cats || []).filter(function (k) { return !k.special; }).length + _t(' 段階')] = 1; });
      if (Object.keys(ns).length > 1) msg += _t(' 注意：段階の数が違う項目が混ざっています（{0}）。そのまま足すと、段階の多い項目の重みが大きくなります。', Object.keys(ns).join(_t('・')));
      el.textContent = msg + _t('（選んだ項目 {0} 個）', items.length);
    }
    d.addEventListener('change', prev); prev();
  }
  // ---- 解析の選び方：知りたいことと質問を選ぶと、合う解析を挙げて、設問を入れた状態で作る
  var GUIDE_GOALS = [
    ['one', _t('1 つの質問の結果をまとめたい'), _t('人数・割合、平均など')],
    ['two', _t('2 つの質問の関係や、グループの違いを調べたい'), _t('例：性別で好みが違うか、勉強時間と通学時間に関係があるか')],
    ['pair', _t('同じ人が答えた 2 つの質問を比べたい'), _t('例：事前と事後、2 つのサービスの利用頻度')],
    ['items', _t('同じ内容を測る複数の質問をまとめて扱いたい'), _t('例：「好き」を測る 4 項目（尺度）')],
    ['model', _t('1 つの値を、複数の質問から説明・予測したい'), _t('例：勉強時間を、学年・性別・通学時間から説明する')]];
  function nLev(x) { return (x.cats || []).filter(function (k) { return !k.special; }).length; }
  function tname(x) { return A.TYPE_NAMES[x.type] || x.type; }
  function guideRecs(goal, a, b, items, y, xs) {
    var R = [], M = [];
    function add(type, slots, why, main) { R.push({ type: type, slots: slots, why: why, main: !!main }); }
    var cat = function (x) { return x && (x.type === 'nominal' || x.type === 'ordinal'); };
    if (goal === 'one') {
      if (!a) return { recs: R, msg: _t('質問を選んでください。') };
      if (a.type === 'numeric') add('numsum', { y: a.col }, _t('数値の質問は、平均・標準偏差・中央値とヒストグラムでまとめます。'), true);
      else if (a.type === 'time') add('timeline', { t: a.col }, _t('日時の列は、日・週・月ごとの回答数の推移でまとめます。'), true);
      else if (a.type === 'text') add('textmine', { t: a.col }, _t('自由記述は、よく出てくる語と、一緒に使われる語の組み合わせでまとめます。'), true);
      else if (cat(a) || a.type === 'multi') add('freq', { a: a.col }, _t('選択肢ごとの人数と割合を出します。'), true);
      else M.push(_t('この列は「回答なし」なので集計できません。'));
    } else if (goal === 'two' || goal === 'pair') {
      if (!a || !b) return { recs: R, msg: _t('質問を 2 つ選んでください。') };
      if (a.col === b.col) return { recs: R, msg: _t('別の質問を選んでください。') };
      var ta = a.type, tb = b.type;
      if (goal === 'pair') {
        if (ta === 'ordinal' && tb === 'ordinal') {
          add('wsr', { x: a.col, y: b.col }, _t('同じ選択肢の段階で答える 2 つの質問を、同じ人の中で比べます。'), true);
          add('pairedt', { x: a.col, y: b.col }, _t('段階を得点（先頭の選択肢が高い点）とみなして、平均の差を調べます。'));
          if (nLev(a) !== nLev(b)) M.push(_t('2 つの質問の段階の数が違います（{0} と {1}）。同じ選択肢で答える質問どうしで使ってください。', nLev(a), nLev(b)));
        } else if (ta === 'numeric' && tb === 'numeric') add('pairedt', { x: a.col, y: b.col }, _t('同じ人の 2 つの値（事前と事後など）の平均の差を調べます。'), true);
        else if (cat(a) && cat(b) && nLev(a) === 2 && nLev(b) === 2) add('mcnemar', { x: a.col, y: b.col }, _t('同じ 2 択の質問を、同じ人の中で比べます（A は「はい」だが B は「いいえ」の人が多いか）。'), true);
        else if (ta === 'numeric' && tb === 'ordinal' || ta === 'ordinal' && tb === 'numeric') add('pairedt', { x: a.col, y: b.col }, _t('段階を得点とみなして、数値と同じ尺度なら平均の差を比べられます。単位が違う 2 つの値は比べられません。'));
        else M.push(_t('この組み合わせ（{0} と {1}）に合う、同じ人の比較の解析はありません。2 択どうし、段階どうし、数値どうしで使えます。', tname(a), tname(b)));
        return { recs: R, msg: M.join(' ') };
      }
      if (ta === 'time' || tb === 'time') { M.push(_t('日時の列は「回答日時の推移」で 1 つずつ集計してください。')); return { recs: R, msg: M.join(' ') }; }
      if (ta === 'text' || tb === 'text') {
        var t = ta === 'text' ? a : b, g = ta === 'text' ? b : a;
        if (cat(g)) add('textmine', { t: t.col, g: g.col }, _t('自由記述の語を、もう一方の質問のグループごとに比べます。'), true); else M.push(_t('自由記述は、単一回答・順序ありの質問と組み合わせて使えます。'));
        return { recs: R, msg: M.join(' ') };
      }
      if (ta === 'multi' || tb === 'multi') {
        var mm = ta === 'multi' ? a : b, gg = ta === 'multi' ? b : a;
        if (cat(gg)) add('multi', { m: mm.col, g: gg.col }, _t('複数回答の選択肢ごとに、選んだ人の割合をグループ別に比べます。'), true); else M.push(_t('複数回答は、単一回答・順序ありの質問をグループにして比べられます。'));
        return { recs: R, msg: M.join(' ') };
      }
      if (ta === 'numeric' && tb === 'numeric') {
        add('pearson', { x: a.col, y: b.col }, _t('2 つの数値の直線的な関係の強さ（相関係数 r）を調べ、散布図を描きます。'), true);
        add('spearman', { x: a.col, y: b.col }, _t('外れ値がある、分布が偏っているときは、順位による相関を使います。'));
      } else if (ta === 'numeric' || tb === 'numeric') {
        var num = ta === 'numeric' ? a : b, oth = ta === 'numeric' ? b : a, k = nLev(oth);
        if (oth.type === 'nominal') {
          if (k === 2) { add('ttest', { g: oth.col, y: num.col }, _t('2 つのグループの平均の差を調べます。'), true); add('mwu', { g: oth.col, y: num.col }, _t('外れ値がある、分布が偏っているときは、順位で比べます。')); }
          else { add('anova', { g: oth.col, y: num.col }, k + _t(' つのグループの平均の差を調べ、どの組み合わせに差があるかも出します。'), true); add('kw', { g: oth.col, y: num.col }, _t('外れ値がある、分布が偏っているときは、順位で比べます。')); if (k < 2) M.push(_t('グループの選択肢が 1 つしかありません。')); }
        } else {
          add('spearman', { x: oth.col, y: num.col }, _t('段階が上がるほど数値も大きく（小さく）なるかを、順位による相関で調べます。'), true);
          add(k === 2 ? 'ttest' : 'anova', { g: oth.col, y: num.col }, _t('段階をグループとみなして、グループごとの平均を比べます。'));
        }
        add('numsum', { y: num.col, g: oth.col }, _t('グループごとの平均・中央値・ヒストグラムで、まず様子を見ます。'));
      } else {
        var oa = ta === 'ordinal', ob = tb === 'ordinal', ka = nLev(a), kb = nLev(b);
        if (oa && ob) {
          add('spearman', { x: a.col, y: b.col }, _t('一方の段階が高いほど、もう一方も高い（低い）かを調べます。'), true);
          add('chisq', { a: a.col, b: b.col }, _t('段階の並びを使わず、答え方の組み合わせに関連があるかを調べます。'));
        } else if (oa || ob) {
          var nom = oa ? b : a, ord = oa ? a : b, kn = nLev(nom);
          if (kn === 2) { add('mwu', { g: nom.col, y: ord.col }, _t('2 つのグループで、段階のある回答の傾向が違うかを調べます。'), true); add('trend', { b: nom.col, o: ord.col }, _t('段階が上がるほど一方の選択肢の割合が増える（減る）かを調べます。')); }
          else add('kw', { g: nom.col, y: ord.col }, kn + _t(' つのグループで、段階のある回答の傾向が違うかを調べます。'), true);
          add('chisq', { a: nom.col, b: ord.col }, _t('クロス集計表で、答え方の組み合わせに関連があるかを調べます。'));
        } else {
          add('chisq', { a: a.col, b: b.col }, _t('クロス集計表を作り、2 つの答え方に関連があるかを調べます。どのセルが多い・少ないかは残差で分かります。'), true);
          add('fisher', { a: a.col, b: b.col }, _t('人数の少ないセル（期待度数 5 未満）が多いときに使います。2×2 の表ではオッズ比も出します。'));
          if (ka >= 3 && kb >= 3) add('ca', { a: a.col, b: b.col }, _t('選択肢の多い 2 つの質問の関係を、1 枚の図（布置図）で見ます。'));
        }
        add('strat', { a: a.col, b: b.col }, _t('3 つ目の質問（例：学年）で層に分けても関連があるかを確かめます。層の質問は、作ったあとで選びます。'));
      }
    } else if (goal === 'items') {
      if (items.length < 2) return { recs: R, msg: _t('質問を 2 つ以上選んでください。') };
      var cols = items.map(function (x) { return x.col; });
      if (items.every(function (x) { return x.type === 'ordinal'; })) {
        add('alpha', { items: cols }, _t('同じ内容を測る質問のまとまりの良さ（クロンバックの α）を調べます。尺度得点を作る前に確かめます。'), true);
        R.push({ type: '_score', items: cols, why: _t('項目の平均点（合計点）を新しい列にして、t 検定や相関などで使えるようにします。'), main: false });
        add('freqmulti', { items: cols }, _t('項目ごとの回答の分布を 1 つの帯グラフに並べます。'));
        if (items.length >= 3) add('factor', { items: cols }, _t('多くの項目の背後にある共通の要因（因子）を探します。項目が多いときに使います。'));
        if (items.length >= 3) add('cfa', { f1: cols }, _t('因子と項目の組み合わせを決めているとき（先行研究の尺度など）に、その組み合わせがデータに当てはまるかを調べます。2 つ以上の因子は、作ったあとで項目を分けます。'));
      } else if (items.every(function (x) { return cat(x); })) add('freqmulti', { items: cols }, _t('同じ選択肢で答える質問を並べて集計します。'));
      else M.push(_t('尺度として扱えるのは「順序あり」の質問です。「データ」で種類を確かめてください。'));
    } else if (goal === 'model') {
      if (!y) return { recs: R, msg: _t('説明したい値（目的変数）を選んでください。') };
      if (!xs.length) return { recs: R, msg: _t('説明に使う質問（説明変数）を 1 つ以上選んでください。') };
      var xc = xs.map(function (x) { return x.col; }).filter(function (c) { return c !== y.col; });
      if (y.type === 'numeric' || y.type === 'ordinal') add('reg', { y: y.col, x: xc }, _t('数値（または段階の得点）を、複数の質問から説明します。説明変数ごとの影響の大きさ（β）と説明力（R²）を出します。'), true);
      if (y.type === 'nominal' || y.type === 'ordinal' && nLev(y) === 2) {
        add('logit', { y: y.col, x: xc }, _t('2 択の回答を、複数の質問から説明します。説明変数ごとのオッズ比を出します。'), y.type === 'nominal');
        if (nLev(y) > 2) M.push(_t('ロジスティック回帰の目的変数は 2 択です。作ったあとで「選択肢の設定」でまとめるか、チェックを外して 2 つにしてください。'));
      }
      if (!R.length) M.push(_t('目的変数には、数値・順序あり・単一回答の質問を使えます。'));
    }
    return { recs: R, msg: M.join(' ') };
  }
  function guideDialog() {
    var usable = S.P.vars.filter(function (x) { return x.type !== 'empty'; });
    function opts(f, sel) { return _t('<option value="">（選ぶ）</option>') + usable.filter(f || function () { return true; }).map(function (x) { return '<option value="' + x.col + '"' + (sel === x.col ? ' selected' : '') + '>' + esc(x.label) + _t('〔{0}〕</option>', esc(tname(x))); }).join(''); }
    function checks(name, f) { return '<div style="max-height:30vh;overflow:auto;border:1px solid var(--t-d5e3da);padding:4px 8px">' + usable.filter(f).map(function (x) { return '<label style="display:block;padding:2px 0"><input type="checkbox" data-g' + name + '="' + x.col + '"> ' + esc(x.label) + _t(' <span class="md-hint">〔{0}〕</span></label>', esc(tname(x))); }).join('') + '</div>'; }
    var goals = GUIDE_GOALS.map(function (g, i) { return '<label style="display:block;padding:3px 0"><input type="radio" name="md_g_goal" value="' + g[0] + '"' + (i === 1 ? ' checked' : '') + '> <b>' + esc(g[1]) + '</b> <span class="md-hint">' + esc(g[2]) + '</span></label>'; }).join('');
    var d = modal(_t('解析の選び方'), _t('<div class="md-sec-title">1. 知りたいこと</div>{0}<div class="md-sec-title" style="margin-top:10px">2. 質問</div><div id="md_g_q"></div><div class="md-sec-title" style="margin-top:10px">3. 合う解析</div><div id="md_g_rec"></div>', goals), _t('<button class="md-btn" data-act="closeModal">閉じる</button>'), 680);
    function goal() { return d.querySelector('input[name="md_g_goal"]:checked').value; }
    function drawQ() {
      var g = goal(), h = '';
      if (g === 'one') h = _t('<label class="md-field">質問<select id="md_g_a">') + opts() + '</select></label>';
      else if (g === 'two' || g === 'pair') h = '<div class="md-grid2"><label class="md-field">' + (g === 'two' ? _t('1 つ目（グループにする質問）') : _t('1 つ目')) + '<select id="md_g_a">' + opts() + _t('</select></label><label class="md-field">2 つ目<select id="md_g_b">') + opts() + '</select></label></div>';
      else if (g === 'items') h = checks('it', function (x) { return x.type === 'ordinal' || x.type === 'nominal'; });
      else h = _t('<label class="md-field">説明したい値（目的変数）<select id="md_g_y">{0}</select></label><div class="md-hint" style="margin:6px 0 2px">説明に使う質問（説明変数）</div>', opts(function (x) { return ['numeric', 'ordinal', 'nominal'].indexOf(x.type) >= 0; })) + checks('x', function (x) { return ['numeric', 'ordinal', 'nominal'].indexOf(x.type) >= 0; });
      d.querySelector('#md_g_q').innerHTML = h; drawR();
    }
    function val(id) { var e = d.querySelector('#' + id); return e && e.value ? A.v(+e.value) : null; }
    function checked(n) { return [].map.call(d.querySelectorAll('[data-g' + n + ']:checked'), function (e) { return A.v(+e.getAttribute('data-g' + n)); }); }
    var cur = [];
    function drawR() {
      var r = guideRecs(goal(), val('md_g_a'), val('md_g_b'), checked('it'), val('md_g_y'), checked('x')); cur = r.recs;
      var h = r.msg ? '<p class="md-hint">' + esc(r.msg) + '</p>' : '';
      h += r.recs.map(function (x, i) {
        var nm = x.type === '_score' ? _t('尺度得点を作る') : (A.def(x.type) || {}).name;
        return '<div style="display:flex;gap:10px;align-items:center;padding:6px 0;border-top:1px solid #E6EEF6"><div style="flex:1"><b>' + (x.main ? _t('おすすめ：') : '') + esc(nm) + '</b><div class="md-hint" style="margin:2px 0 0">' + esc(x.why) + '</div></div><button class="md-btn' + (x.main ? ' md-btn-primary' : '') + '" data-grec="' + i + '">' + (x.type === '_score' ? _t('作る') : _t('この解析を作る')) + '</button></div>';
      }).join('');
      d.querySelector('#md_g_rec').innerHTML = h || _t('<p class="md-hint">質問を選ぶと、合う解析をここに挙げます。</p>');
    }
    d.addEventListener('change', function (e) { if (e.target.name === 'md_g_goal') drawQ(); else drawR(); });
    d.addEventListener('click', function (e) {
      var b = e.target.closest('[data-grec]'); if (!b) return; var x = cur[+b.dataset.grec]; if (!x) return;
      closeModal();
      if (x.type === '_score') { scoreDialog(null, x.items); return; }
      if (S.P.tabs.length >= A.MAX_TABS) { A.toast(_t('解析は {0} 個までです。使わない解析を削除してください。', A.MAX_TABS), true); return; }
      var t; A.commit(function () { t = A.newTab(x.type); if (!t) return; Object.keys(x.slots).forEach(function (k) { var c = x.slots[k]; if (Array.isArray(c)) A.setItems(t, k, c); else A.setSlot(t, k, c); }); }, { quiet: true });
      if (!t) return; S.ui.sel = t.id; S.ui.selFig = 0; S.ui.view = 'analysis'; S.ui.rtab = 'analysis'; A.schedule(t.id); A.render(); scrollToAn(t.id);
    });
    drawQ();
  }
  // 設定の画面が開いていれば、中身を描き直す（スクロールの位置は保つ）
  function refreshSettings() {
    var b = document.getElementById('md_set_body'); if (!b) return;
    var box = b.closest('.md-modal-body') || b.parentElement, top = box ? box.scrollTop : 0;
    b.innerHTML = A.settingsHtml(); if (box) box.scrollTop = top;
  }
  // カスタム設定の中身を、設定の画面から直接変える（なければ初期設定をもとに作る）
  function setCustom(key, val) {
    var c = A.customGet(), base = A.baseProject();
    if (!c) { var st = A.clone(base.settings); delete st.autoSave; c = { v: 1, settings: st, report: A.clone(base.report), figDefault: null, tstyleDefault: null }; }
    c.settings = c.settings || {}; c.report = c.report || {};
    if (key === 'alpha') c.settings.alpha = +val;
    else if (key === 'lead0') c.settings.lead0 = val === 'true';
    else if (key === 'tables' || key === 'holm') c.report[key] = val === 'true';
    else if (key === 'palette') { c.figDefault = c.figDefault || {}; if (val) c.figDefault.palette = val; else delete c.figDefault.palette; delete c.figDefault.colorMode; }
    else if (key === 'design') { c.tstyleDefault = Object.assign(c.tstyleDefault || A.tableDefaults(), T_PRESET[val] || {}, { design: val }); }
    c.savedAt = new Date().toISOString();
    if (!A.customSet(c)) { A.toast(_t('この端末に保存できませんでした。'), true); return; }
    refreshSettings();
  }
  A.refreshSettings = refreshSettings;
  // 今の設定からカスタム設定を作る。図と表の書式は、選んでいる図・表があればそれを、なければ「これからの既定」を使う
  function customFromNow() {
    var tab = A.curTab(), fig = tab && tab.figs && tab.figs.length ? A.curFig() : null, set = A.clone(S.P.settings);
    delete set.autoSave;
    var fd = null;
    if (fig) { fd = {}; FIG_STYLE_KEYS.concat(['w', 'h', 'orient']).forEach(function (k) { if (fig[k] !== undefined) fd[k] = A.clone(fig[k]); }); delete fd.colors; delete fd.colorMode; }
    else if (S.P.figDefault) fd = A.clone(S.P.figDefault);
    var td = tab ? A.clone(tab.tstyle) : S.P.tstyleDefault ? A.clone(S.P.tstyleDefault) : null;
    if (td) delete td.heat;
    return { v: 1, savedAt: new Date().toISOString(), settings: set, report: A.clone(S.P.report), figDefault: fd, tstyleDefault: td };
  }
  A.customFromNow = customFromNow;
  // 報告に書き込む端末の情報（アンケートのデータは含めない）
  function envText() {
    var ua = navigator.userAgent, os = /iPhone/.test(ua) ? 'iPhone' : /iPad|Macintosh.*Mobile/.test(ua) || (/Macintosh/.test(ua) && navigator.maxTouchPoints > 1) ? 'iPad' : /Android/.test(ua) ? 'Android' : /Windows/.test(ua) ? 'Windows' : /Mac OS X/.test(ua) ? 'Mac' : /CrOS/.test(ua) ? 'Chromebook' : _t('その他');
    var br = /Edg\//.test(ua) ? 'Edge' : /CriOS|Chrome\//.test(ua) ? 'Chrome' : /FxiOS|Firefox\//.test(ua) ? 'Firefox' : /Safari\//.test(ua) ? 'Safari' : _t('その他');
    var standalone = (window.matchMedia && matchMedia('(display-mode: standalone)').matches) || navigator.standalone;
    return os + ' / ' + br + (standalone ? _t('（ホーム画面のアプリ）') : '') + _t(' / 画面 ') + screen.width + '×' + screen.height + ' / ' + (window.CTS_WEB ? _t('Web 版') : _t('パソコン版')) + ' / R ' + (S.r || '?');
  }
  function vcmp(a, b) { a = String(a).split('.').map(Number); b = String(b).split('.').map(Number); for (var i = 0; i < 3; i++) { var d = (a[i] || 0) - (b[i] || 0); if (d) return d; } return 0; }
  A.showChanges = showChanges;
  A.setPath = setPath; A.act = act; A.selectTab = selectTab;
  init();
})();
