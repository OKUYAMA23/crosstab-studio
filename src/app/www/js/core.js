// core.js ― 状態（プロジェクト）、R との通信、元に戻す、ファイルとプロジェクトの読み書き、ZIP
(function () {
  'use strict';
  var MAX_TABS = Infinity;                 // 解析の数に上限は設けない（多いときは、画面に見えている解析だけ図を描く）
  var TYPE_NAMES; function mkTypes() { TYPE_NAMES = { nominal: _t('単一回答'), ordinal: _t('順序あり'), multi: _t('複数回答'), numeric: _t('数値'), text: _t('自由記述（使わない）'), time: _t('日時'), empty: _t('回答なし') }; } mkTypes(); window.CTS_ONLANG.push(mkTypes);
  var S = {
    registry: [], data: null, P: null, results: {}, undo: [], redo: [], dirty: false, projName: '', r: null, holm: null,
    ui: { rtab: 'home', view: 'analysis', sel: null, selFig: 0, pane: true, selVar: null, dataView: 'vars', paneScroll: {}, rawRows: 200 }
  };
  function tableDefaults() {
    return { design: 'grid', fontJa: 'Yu Gothic', fontEn: 'Arial', fs: 13, lw: 1, ls: 'solid', lc: '#7F7F7F', headFill: '#F2F2F2', headInk: '#000000', ink: '#000000',
      stripe: '#F5F5F5', heat: 'none', heatPal: 'green', bar: 'right', barShape: 'smooth' };
  }
  function baseProject() {
    return { title: _t('集計と検定の報告'), tabs: [], vars: [], seq: 0, figDefault: null, tstyleDefault: null,
      settings: { alpha: 0.05, lead0: true, figLang: window.CTS_LANG === 'en' ? 'en' : 'ja', zoom: 100, pngScale: 2, autoSave: false }, report: { tables: true, holm: true } };
  }
  // ---- 既定の設定：初期設定か、自分のカスタム設定か。カスタム設定は端末（ブラウザ）に保存し、新しい版が出ても残す
  var CUSTOM_KEY = 'cts-custom', MODE_KEY = 'cts-defmode';
  function customGet() { try { var c = JSON.parse(localStorage.getItem(CUSTOM_KEY) || 'null'); return c && typeof c === 'object' ? c : null; } catch (e) { return null; } }
  function customSet(c) { try { if (c) localStorage.setItem(CUSTOM_KEY, JSON.stringify(c)); else localStorage.removeItem(CUSTOM_KEY); return true; } catch (e) { return false; } }
  function defMode() { try { return localStorage.getItem(MODE_KEY) === 'custom' && customGet() ? 'custom' : 'init'; } catch (e) { return 'init'; } }
  function setDefMode(m) { try { localStorage.setItem(MODE_KEY, m); } catch (e) {} }
  // 既定の値を、プロジェクトに当てはめる（設定・報告書・これからの図と表の書式）
  function applyDefaults(P, c) {
    if (!c) return P;
    Object.assign(P.settings, c.settings || {}); Object.assign(P.report, c.report || {});
    P.figDefault = c.figDefault ? clone(c.figDefault) : null; P.tstyleDefault = c.tstyleDefault ? clone(c.tstyleDefault) : null;
    return P;
  }
  // アプリの色：緑（初期）・青・赤。端末に保存し、html の data-theme で切り替える（css/theme.css）
  function theme() { try { var t = localStorage.getItem('cts-theme'); return t === 'blue' || t === 'red' ? t : 'green'; } catch (e) { return 'green'; } }
  function setTheme(t) {
    try { localStorage.setItem('cts-theme', t); } catch (e) {}
    if (t && t !== 'green') document.documentElement.setAttribute('data-theme', t); else document.documentElement.removeAttribute('data-theme');
    var m = document.querySelector('meta[name="theme-color"]'), c = { green: '#217346', blue: '#2B579A', red: '#A4373A' }[t] || '#217346';
    if (m) m.setAttribute('content', c);
  }
  if (theme() !== 'green') setTheme(theme());                       // 起動時：ブラウザの上端の色も合わせる
  function newProject() { return defMode() === 'custom' ? applyDefaults(baseProject(), customGet()) : baseProject(); }
  S.P = newProject();

  // ---------------- 小さな道具
  function $(sel, root) { return (root || document).querySelector(sel); }
  function esc(s) { return String(s === null || s === undefined ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;'); }
  function clone(o) { return JSON.parse(JSON.stringify(o)); }
  // ---------------- エラーの記録：この端末の中だけに最近の 30 件を残す。外には送らない（報告するときに本人が写して貼る）
  var ERRKEY = 'cts-errlog';
  function errLog() { try { return JSON.parse(localStorage.getItem(ERRKEY) || '[]'); } catch (e) { return []; } }
  function logErr(kind, msg, stack) {
    try {
      var a = errLog(); a.push({ t: new Date().toISOString(), v: window.CTS_APP_VERSION || '', k: kind, m: String(msg || '').slice(0, 400), s: String(stack || '').split('\n').slice(0, 4).join(' | ').slice(0, 600) });
      localStorage.setItem(ERRKEY, JSON.stringify(a.slice(-30)));
    } catch (e) {}
  }
  function clearErrLog() { try { localStorage.removeItem(ERRKEY); } catch (e) {} }
  window.addEventListener('error', function (e) { logErr('script', e.message, (e.error && e.error.stack) || (e.filename + ':' + e.lineno)); });
  window.addEventListener('unhandledrejection', function (e) { var r = e.reason || {}; logErr('promise', r.message || r, r.stack); });
  function toast(msg, err) {
    if (err) logErr('message', msg);
    var box = $('#md_toasts'), d = document.createElement('div');
    d.className = 'md-toast' + (err ? ' err' : ''); d.textContent = msg; box.appendChild(d);
    setTimeout(function () { d.remove(); }, err ? 9000 : 4000);
  }
  // 進行の表示。0.4 秒を超える処理で出し、経過秒数を数える。10 秒を超えたら「止まっていない」ことを文でも伝える
  var busyN = 0, busyT = null, busyTick = null, busyAt = 0, busyText = '';
  function busyDraw() {
    var el = $('#md_busy'), sec = Math.floor((Date.now() - busyAt) / 1000);
    el.querySelector('.md-busy-text').textContent = busyText + (busyN > 1 ? _t('（残り {0} 件）', busyN) : '');
    el.querySelector('.md-busy-sec').textContent = sec >= 2 ? sec + _t(' 秒経過') : '';
    el.querySelector('.md-busy-long').style.display = sec >= 10 ? 'block' : 'none';
    el.classList.toggle('long', sec >= 3);
  }
  function busy(on, text) {
    if (on) {
      if (busyN === 0) { busyAt = Date.now(); busyT = setTimeout(function () { $('#md_busy').style.display = 'block'; busyDraw(); busyTick = setInterval(busyDraw, 500); }, 400); }
      busyN++; if (text) busyText = text; else if (!busyText) busyText = _t('計算しています');
      if ($('#md_busy').style.display === 'block') busyDraw();
    } else {
      busyN = Math.max(0, busyN - 1);
      if (busyN === 0) { clearTimeout(busyT); clearInterval(busyTick); busyT = busyTick = null; busyText = ''; $('#md_busy').style.display = 'none'; }
      else busyDraw();
    }
  }
  function bytesToB64(u8) { var s = '', n = 0x8000; for (var i = 0; i < u8.length; i += n) s += String.fromCharCode.apply(null, u8.subarray(i, i + n)); return btoa(s); }
  function b64ToBytes(b) { var s = atob(b), u = new Uint8Array(s.length); for (var i = 0; i < s.length; i++) u[i] = s.charCodeAt(i); return u; }
  function safeName(s) { return String(s).replace(/[\\/:*?"<>|\u0000-\u001f]+/g, '_').replace(/\s+/g, ' ').trim().slice(0, 100) || _t('無題'); }

  // ---------------- ZIP（.xlsx の作成と、フォルダーを選べないときのまとめ保存に使う）
  async function deflate(u8) {
    if (!window.CompressionStream) return null;
    try { var cs = new CompressionStream('deflate-raw'), w = cs.writable.getWriter(); w.write(u8); w.close(); return new Uint8Array(await new Response(cs.readable).arrayBuffer()); } catch (e) { return null; }
  }
  async function zip(files, mime) {                                              // files：[{ name, data: Uint8Array }]
    var enc = new TextEncoder(), parts = [], central = [], offset = 0, d = new Date();
    var time = (d.getHours() << 11) | (d.getMinutes() << 5) | (d.getSeconds() >> 1), date = ((d.getFullYear() - 1980) << 9) | ((d.getMonth() + 1) << 5) | d.getDate();
    for (var i = 0; i < files.length; i++) {
      var f = files[i], name = enc.encode(f.name), raw = f.data, comp = await deflate(raw), method = comp && comp.length < raw.length ? 8 : 0, body = method ? comp : raw, crc = Chart.crc32(raw);
      var h = new DataView(new ArrayBuffer(30));
      h.setUint32(0, 0x04034b50, true); h.setUint16(4, 20, true); h.setUint16(6, 0x0800, true); h.setUint16(8, method, true); h.setUint16(10, time, true); h.setUint16(12, date, true);
      h.setUint32(14, crc, true); h.setUint32(18, body.length, true); h.setUint32(22, raw.length, true); h.setUint16(26, name.length, true); h.setUint16(28, 0, true);
      var c = new DataView(new ArrayBuffer(46));
      c.setUint32(0, 0x02014b50, true); c.setUint16(4, 20, true); c.setUint16(6, 20, true); c.setUint16(8, 0x0800, true); c.setUint16(10, method, true); c.setUint16(12, time, true); c.setUint16(14, date, true);
      c.setUint32(16, crc, true); c.setUint32(20, body.length, true); c.setUint32(24, raw.length, true); c.setUint16(28, name.length, true); c.setUint32(42, offset, true);
      parts.push(new Uint8Array(h.buffer), name, body); central.push(new Uint8Array(c.buffer), name); offset += 30 + name.length + body.length;
    }
    var csize = central.reduce(function (s, x) { return s + x.length; }, 0), e = new DataView(new ArrayBuffer(22));
    e.setUint32(0, 0x06054b50, true); e.setUint16(8, files.length, true); e.setUint16(10, files.length, true); e.setUint32(12, csize, true); e.setUint32(16, offset, true);
    return new Blob(parts.concat(central, [new Uint8Array(e.buffer)]), { type: mime || 'application/zip' });
  }

  // ---------------- R との通信
  async function api(path, body, raw) {
    var res;
    try { res = await fetch(path + (path.indexOf('?') < 0 ? '?' : '&') + 'lang=' + (window.CTS_LANG || 'ja'), { method: 'POST', headers: { 'Content-Type': raw ? 'application/octet-stream' : 'application/json' }, body: raw ? body : JSON.stringify(body || {}) }); }
    catch (e) { S.r = null; App.renderStatus(); throw new Error(_t('計算をする R に接続できません。アプリを起動し直してください。')); }
    return res.json();
  }
  async function uploadData(bytes, name) {
    busy(true, _t('データを読み込んでいます'));
    try { var d = await api('/api/load?name=' + encodeURIComponent(name), bytes, true); if (!d.ok) throw new Error(d.message || _t('データを読み込めませんでした。')); return d; }
    finally { busy(false); }
  }
  // R がデータを持っていないとき（R を起動し直したあとなど）は、手元のデータを送り直す
  // 尺度得点（画面で作った列）の定義。R には依頼のたびに渡し、元のデータの右に足して計算してもらう
  function compVars() { return S.P.vars.filter(function (x) { return x.comp; }).sort(function (a, b) { return a.col - b.col; }); }
  function itemSlot(c) {
    var vr = v(c); if (!vr) return null;
    var s = { col: vr.col, type: vr.type, delim: vr.delim, label: vr.label, cats: (vr.cats || []).filter(function (k) { return !k.special; }).map(function (k) { return { label: k.v, values: [k.v] }; }) };
    if (vr.type === 'numeric') { if (vr.fix) s.fix = vr.fix; if (vr.unit) s.unit = vr.unit; }
    return s;
  }
  function compDefs() {
    return compVars().map(function (x) {
      if (x.comp.kind === 'period') { var sv = v(x.comp.src); return { kind: 'period', col: x.col, name: x.label, src: x.comp.src, srcLabel: sv ? sv.label : '', breaks: x.comp.breaks, names: x.comp.names, same: !!x.comp.same }; }
      return { col: x.col, name: x.label, method: x.comp.method, reverse: x.comp.reverse || [], items: x.comp.items.map(itemSlot).filter(Boolean) };
    });
  }
  async function apiData(path, body) {
    body.dataId = S.data.dataId;
    if (!body.computed && compVars().length) body.computed = compDefs();
    var d = await api(path, body);
    if (d && d.noData) { var up = await uploadData(S.data.bytes, S.data.name); S.data.dataId = up.dataId; body.dataId = up.dataId; d = await api(path, body); }
    return d;
  }

  // ---------------- 変数・解析
  function v(col) { return S.P.vars.find(function (x) { return x.col === col; }); }
  function def(type) { return S.registry.find(function (a) { return a.id === type; }); }
  function tabById(id) { return S.P.tabs.find(function (t) { return t.id === id; }) || null; }
  function curTab() { return tabById(S.ui.sel); }
  function curFig() { var t = curTab(); return t && t.figs.length ? t.figs[Math.min(S.ui.selFig, t.figs.length - 1)] : null; }
  function isCat(vr) { return vr && (vr.type === 'nominal' || vr.type === 'ordinal' || vr.type === 'multi'); }
  function defaultCats(vr, sd) {
    var cats = (vr.cats || []).map(function (c) { return { v: c.v, label: c.v, on: !(sd && sd.excludeSpecial && c.special) }; });
    if (vr.type === 'multi') {
      // 複数回答で 1% 未満しか選ばれていない回答は、「その他」に書かれた自由記述とみなして 1 つにまとめておく
      var lim = Math.max(1, Math.floor(vr.n * 0.01)), rare = cats.filter(function (c, i) { return vr.cats[i].n <= lim; });
      if (rare.length >= 2 && rare.length < cats.length) rare.forEach(function (c) { c.label = _t('その他'); });
    }
    return cats;
  }
  // 複数の設問を選ぶ欄の名前：同じ問いの項目なら「問い（項目・項目）」、そうでなければ設問名を並べる
  function itemsName(cols) {
    var vs = cols.map(v).filter(Boolean); if (!vs.length) return null;
    var same = vs.every(function (x) { return x.item && x.stem && x.stem === vs[0].stem; });
    return same ? vs[0].stem + _t('（{0}）', vs.map(function (x) { return x.item; }).join(_t('・'))) : vs.map(function (x) { return x.label; }).join(' / ');
  }
  function autoName(tab) {
    if (!tab.nameAuto) return;
    var d = def(tab.type), labs = d.slots.map(function (s) { var sl = tab.slots[s.id]; if (!sl) return null; if (s.many) return itemsName(sl.cols || []); var vr = v(sl.col); return vr ? vr.label : null; }).filter(Boolean);
    tab.name = labs.length ? labs.join(' × ') : d.name;
    // 回帰分析は説明変数を並べると長くなるので「目的変数 ← 説明変数 n 個」にする
    if ((tab.type === 'reg' || tab.type === 'logit') && tab.slots.y && v(tab.slots.y.col)) { var nx = (tab.slots.x && tab.slots.x.cols || []).length; tab.name = v(tab.slots.y.col).label + (nx ? _t(' ← 説明変数 {0} 個', nx) : ''); }
  }
  // 複数の設問の選択肢：最初の設問の並びを基に、ほかの設問にしかない選択肢を後ろに足す
  function defaultCatsMany(cols, sd) {
    var out = [];
    cols.map(v).filter(Boolean).sort(function (a, b) { return (b.cats || []).length - (a.cats || []).length; }).forEach(function (x) { (x.cats || []).forEach(function (c) { if (!out.some(function (k) { return k.v === c.v; })) out.push({ v: c.v, label: c.v, on: !(sd && sd.excludeSpecial && c.special) }); }); });
    return out;
  }
  // 2 つの設問が「同じ選択肢」か：選ばれなかった選択肢は CSV に現れないので、一方の選択肢がもう一方に含まれていれば同じとみなす
  function sameChoices(a, b) {
    if (!a || !b) return false;
    var A2 = (a.cats || []).map(function (c) { return c.v; }), B2 = (b.cats || []).map(function (c) { return c.v; }), inter = A2.filter(function (x) { return B2.indexOf(x) >= 0; }).length;
    return inter >= 2 && inter >= Math.min(A2.length, B2.length);
  }
  function slotCols(sl) { return sl ? (sl.cols ? sl.cols.slice() : (sl.col !== null && sl.col !== undefined ? [sl.col] : [])) : []; }
  function setItems(tab, sid, cols) {
    var d = def(tab.type), sd = d.slots.find(function (s) { return s.id === sid; }), old = tab.slots[sid] || { cols: [], cats: [] }, cats = defaultCatsMany(cols, sd);
    // 並びは選択肢のいちばん多い設問に合わせ、それまでに書き換えた名前とチェックは残す
    var kept = cats.map(function (c) { var o = (old.cats || []).find(function (k) { return k.v === c.v; }); return o ? { v: c.v, label: o.label, on: o.on } : c; });
    if (old.cats && old.cats.length && old.cats.every(function (c) { return cats.some(function (k) { return k.v === c.v; }); }) && cats.length === old.cats.length) kept = old.cats.slice();   // 選択肢が変わらないときは、自分で決めた並びも残す
    tab.slots[sid] = { cols: cols, cats: cols.length ? kept : [] };
    autoName(tab);
  }
  function setSlot(tab, sid, col) {
    var d = def(tab.type), sd = d.slots.find(function (s) { return s.id === sid; }), vr = v(col);
    tab.slots[sid] = vr ? { col: col, cats: isCat(vr) ? defaultCats(vr, sd) : [] } : { col: null, cats: [] };
    autoName(tab);
  }
  function newFig() { return Object.assign(Chart.defaults(), clone(S.P.figDefault || {})); }
  function newTab(type) {
    if (S.P.tabs.length >= MAX_TABS) { toast(_t('解析は {0} 個までです。使わない解析を削除してください。', MAX_TABS), true); return null; }
    var d = def(type), tab = { id: 't' + (++S.P.seq), type: type, name: d.name, nameAuto: true, slots: {}, filter: null, opts: {}, figs: [newFig()],
      table: { pct: 'row', expected: false, resid: false, marks: true }, tstyle: Object.assign(tableDefaults(), clone(S.P.tstyleDefault || {})), reportText: null, collapsed: false, checked: true };
    d.slots.forEach(function (s) { tab.slots[s.id] = s.many ? { cols: [], cats: [] } : { col: null, cats: [] }; });
    (d.opts || []).forEach(function (o) { tab.opts[o.id] = o.default; });
    // 設問ごとに色を変える図なので、既定の色を多色にする（これからの図の既定で色を決めているときは、それに従う）
    if (d.slots.some(function (s) { return s.many; })) { if (!(S.P.figDefault && S.P.figDefault.palette)) tab.figs[0].palette = 'cud'; if (!(S.P.figDefault && S.P.figDefault.orient)) tab.figs[0].orient = 'v'; }
    // 回答日時の推移は、日ごとの回答数で見るのが自然なので、縦軸を人数にする
    if (type === 'timeline' && !(S.P.figDefault && S.P.figDefault.value)) tab.figs[0].value = 'count';
    S.P.tabs.push(tab);
    return tab;
  }
  function tabReady(tab) { var d = def(tab.type); return !!d && d.slots.every(function (s) { var sl = tab.slots[s.id]; return s.optional || (sl && (s.many ? (sl.cols || []).length >= 2 : sl.col !== null && sl.col !== undefined)); }); }
  function buildReq(tab) {
    var d = def(tab.type), slots = {};
    d.slots.forEach(function (s) {
      var sl = tab.slots[s.id], onCats = sl.cats.filter(function (c) { return c.on; }).map(function (c) { return { label: c.label, values: [c.v] }; });
      if (s.many) {
        if (s.optional && !(sl.cols || []).length) return;                      // 任意の欄（項目の組）を使わないとき
        var vs = sl.cols.map(v);
        slots[s.id] = { many: true, cols: sl.cols, type: vs[0].type, delim: vs[0].delim, labels: vs.map(function (x) { return x.label; }), fulls: vs.map(function (x) { return x.full || x.label; }),
          items: vs.map(function (x) { return x.item || ''; }), stems: vs.map(function (x) { return x.stem || ''; }), cats: onCats, reverse: (sl.reverse || []).filter(function (c) { return sl.cols.indexOf(c) >= 0; }), vars: sl.cols.map(itemSlot).filter(Boolean) };
        return;
      }
      if (s.optional && (sl.col === null || sl.col === undefined)) return;       // 任意の欄を使わないとき
      var vr = v(sl.col);
      slots[s.id] = { col: vr.col, type: vr.type, delim: vr.delim, label: vr.label, full: vr.full || vr.label, cats: sl.cats.filter(function (c) { return c.on; }).map(function (c) { return { label: c.label, values: [c.v] }; }) };
      if (vr.type === 'numeric') { if (vr.fix) slots[s.id].fix = vr.fix; if (vr.unit) slots[s.id].unit = vr.unit; }
    });
    var flt = null;
    if (tab.filter && tab.filter.col !== null && tab.filter.values && tab.filter.values.length) { var fv = v(tab.filter.col); if (fv) flt = { col: fv.col, type: fv.type, delim: fv.delim, label: fv.label, full: fv.full || fv.label, values: tab.filter.values }; }
    var usesComp = Object.keys(slots).some(function (k) { var s = slots[k]; return (s.cols || [s.col]).some(function (c) { var x = v(c); return x && x.comp; }); }) || (flt && v(flt.col) && v(flt.col).comp);
    return { computed: usesComp ? compDefs() : undefined, type: tab.type, slots: slots, filter: flt, opts: Object.assign({}, tab.opts, { alpha: S.P.settings.alpha, lead0: S.P.settings.lead0 }) };
  }
  var timers = {}, seqs = {};
  function schedule(id) { clearTimeout(timers[id]); timers[id] = setTimeout(function () { runTab(id); }, 100); }
  function scheduleAll() { S.P.tabs.forEach(function (t) { schedule(t.id); }); }
  async function runTab(id) {
    var tab = tabById(id);
    if (!tab || !S.data) return;
    if (!tabReady(tab)) { delete S.results[id]; App.render(); return; }
    var req = buildReq(tab), sig = JSON.stringify(req), my = (seqs[id] = (seqs[id] || 0) + 1);
    busy(true, _t('解析を計算しています'));
    try { var rq = App.prepReq ? await App.prepReq(tab, req) : req; if (seqs[id] !== my) return; var res = await apiData('/api/analyze', rq); if (seqs[id] !== my) return; res.sig0 = sig; S.results[id] = res; S.holm = null; }
    catch (e) { if (seqs[id] !== my) return; S.results[id] = { ok: false, message: e.message, sig0: sig }; }
    finally { busy(false); }
    App.render();
  }
  // 設定が変わった解析だけ計算し直す（元に戻したあとなど）
  function syncResults() {
    Object.keys(S.results).forEach(function (id) { if (!tabById(id)) delete S.results[id]; });
    S.P.tabs.forEach(function (t) {
      if (!tabReady(t)) { delete S.results[t.id]; return; }
      var r = S.results[t.id]; if (!r || r.sig0 !== JSON.stringify(buildReq(t))) schedule(t.id);
    });
  }

  // ---------------- 元に戻す・やり直す
  var lastKey = null, lastAt = 0;
  function commit(fn, o) {
    o = o || {};
    var now = Date.now();
    if (!(o.key && o.key === lastKey && now - lastAt < 1200)) { S.undo.push(JSON.stringify(S.P)); if (S.undo.length > 100) S.undo.shift(); }
    S.redo = []; lastKey = o.key || null; lastAt = now;
    fn(); markDirty();
    if (o.run === 'all') scheduleAll(); else if (o.run) schedule(o.run);
    if (!o.quiet) App.render(o.only);
  }
  // ---------------- 未保存の印と自動保存
  // 自動保存がオンで、名前を付けて一度保存してあるときは、変更が止まって 3 秒後に上書き保存する
  var autoT = null;
  function markDirty() {
    S.dirty = true; clearTimeout(autoT);
    if (S.P.settings.autoSave && S.projName && S.data) autoT = setTimeout(autoSaveNow, 3000);
  }
  async function autoSaveNow() {
    if (!S.dirty || !S.projName || !S.P.settings.autoSave) return;
    if (S.saving) { autoT = setTimeout(autoSaveNow, 1500); return; }
    await saveProjectAs(S.projName, { quiet: true });
  }
  function fixSel() { if (!curTab()) { S.ui.sel = S.P.tabs.length ? S.P.tabs[S.P.tabs.length - 1].id : null; S.ui.selFig = 0; } }
  function restore(from, to) {
    if (!from.length) return;
    to.push(JSON.stringify(S.P)); S.P = JSON.parse(from.pop()); markDirty(); lastKey = null;
    fixSel(); syncResults(); App.applyZoom(); App.render();
  }
  function undo() { restore(S.undo, S.redo); }
  function redo() { restore(S.redo, S.undo); }

  // ---------------- ファイル
  function pickFile(accept) {
    // 入力欄はページに置いてから開く。置かずに開くと、iPhone・iPad の Safari は選んだあとの知らせを落とすことがある
    return new Promise(function (resolve) {
      var old = document.getElementById('md_pick'); if (old) old.remove();
      var inp = document.createElement('input'); inp.type = 'file'; inp.accept = accept; inp.id = 'md_pick';
      inp.style.cssText = 'position:fixed;left:-1000px;top:0;width:1px;height:1px;opacity:0';
      var done = false;
      function fin() { if (done) return; var f = inp.files && inp.files[0]; if (!f) return; done = true; resolve(f); setTimeout(function () { inp.remove(); }, 1000); }
      inp.addEventListener('change', fin); inp.addEventListener('input', fin);
      document.body.appendChild(inp); inp.click();
    });
  }
  async function saveBlob(blob, name, desc, ext) {
    if (window.CTS_SAVE) return window.CTS_SAVE(blob, name, desc, ext);       // iPad 版：保存の仕組みが違う
    if (window.showSaveFilePicker) {
      try {
        var acc = {}; acc[blob.type.split(';')[0] || 'application/octet-stream'] = [ext];
        var h = await window.showSaveFilePicker({ suggestedName: name, types: [{ description: desc, accept: acc }] }), w = await h.createWritable(); await w.write(blob); await w.close(); return true;
      } catch (e) { if (e && e.name === 'AbortError') return false; }
    }
    var a = document.createElement('a'); a.href = URL.createObjectURL(blob); a.download = name; document.body.appendChild(a); a.click();
    setTimeout(function () { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
    return true;
  }
  function baseName() { return safeName((S.projName || (S.data ? S.data.name : 'project')).replace(/\.(csv|tsv|txt|prj|ctsproj)$/i, '')); }

  function setData(d, bytes, name) { S.data = { name: name, bytes: bytes, dataId: d.dataId, nrow: d.nrow, ncol: d.ncol, rows: d.rows, rowsShown: d.rowsShown }; }
  async function loadCsv(bytes, name, keepTabs) {
    var d = await uploadData(bytes, name), oldVars = S.P.vars;
    setData(d, bytes, name);
    if (keepTabs) {
      // 同じ質問文の列に、表示名・種類・解析の設定を引き継ぐ
      var nv = clone(d.vars), map = {};
      nv.forEach(function (x) {
        var o = oldVars.find(function (y) { return y.header === x.header; });
        if (o) { map[o.col] = x.col; if (!/…$/.test(o.label)) x.label = o.label; if (o.type !== x.type && ['nominal', 'ordinal'].indexOf(o.type) >= 0 && ['nominal', 'ordinal'].indexOf(x.type) >= 0) x.type = o.type; }
      });
      // 尺度得点の列は、元の列の番号を付け直して右に足し直す
      oldVars.filter(function (o) { return o.comp; }).sort(function (a, b) { return a.col - b.col; }).forEach(function (o) {
        if (o.comp.kind === 'period') { if (!map[o.comp.src]) return; var nc2 = nv.length + 1, pv = clone(o); pv.col = nc2; pv.comp.src = map[o.comp.src]; map[o.col] = nc2; nv.push(pv); return; }
        var items = o.comp.items.map(function (c) { return map[c]; }).filter(Boolean); if (items.length < 2) return;
        var nc = nv.length + 1, cv = clone(o); cv.col = nc; cv.comp.items = items; cv.comp.reverse = (o.comp.reverse || []).map(function (c) { return map[c]; }).filter(Boolean);
        map[o.col] = nc; nv.push(cv);
      });
      S.P.vars = nv;
      S.P.tabs.forEach(function (t) {
        Object.keys(t.slots).forEach(function (sid) {
          var sl = t.slots[sid];
          if (sl.cols) { sl.cols = sl.cols.map(function (c) { return map[c]; }).filter(Boolean); if (sl.cols.length < 1) sl.cats = []; return; }
          if (sl.col === null) return;
          var nc = map[sl.col], vr = nc ? nv.find(function (x) { return x.col === nc; }) : null;
          if (!vr) { t.slots[sid] = { col: null, cats: [] }; return; }
          sl.col = nc; (vr.cats || []).forEach(function (c) { if (!sl.cats.some(function (k) { return k.v === c.v; })) sl.cats.push({ v: c.v, label: c.v, on: true }); });
        });
        if (t.filter && t.filter.col !== null) { if (map[t.filter.col]) t.filter.col = map[t.filter.col]; else t.filter = null; }
      });
      S.undo = []; S.redo = []; markDirty(); S.results = {}; scheduleAll();
      toast(_t('データを差し替えました（回答 {0} 件）。すべての解析を計算し直しました。', d.nrow));
    } else {
      S.P = newProject(); S.P.vars = clone(d.vars); S.results = {}; S.undo = []; S.redo = []; S.projName = ''; S.dirty = false;
      S.ui.view = 'analysis'; S.ui.rtab = 'home'; S.ui.selVar = null; S.ui.sel = null;
      toast(_t('読み込みました（回答 {0} 件、{1} 列）。', d.nrow, d.ncol));
    }
    App.applyZoom(); App.render();
  }
  function projectJson() { return JSON.stringify({ app: 'CrossTabStudio', format: 2, saved: new Date().toISOString(), data: { name: S.data.name, b64: bytesToB64(S.data.bytes) }, project: S.P }); }
  // 以前の形式（解析ごとに図が 1 つ）のプロジェクトも開けるようにする
  function normalizeTab(t) {
    t.figs = (t.figs || (t.fig ? [t.fig] : [{}])).map(function (f) { var o = Object.assign(Chart.defaults(), f); delete o.title; delete o.fsTitle; if (o.grid === true) o.grid = 'solid'; if (o.grid === false) o.grid = 'none'; return o; });
    delete t.fig;
    t.table = Object.assign({ pct: 'row', expected: false, resid: false, marks: true }, t.table || {}); t.tstyle = Object.assign(tableDefaults(), t.tstyle || {});
    if (t.collapsed === undefined) t.collapsed = false; if (t.checked === undefined) t.checked = true;
    return t;
  }
  async function openProjectText(text, name) {
    var j;
    try { j = JSON.parse(text); } catch (e) { throw new Error(_t('プロジェクトのファイルが壊れています。')); }
    if (j && j.ok === false) throw new Error(j.message || _t('プロジェクトを開けませんでした。'));
    if (!j || j.app !== 'CrossTabStudio' || !j.data || !j.project) throw new Error(_t('CrossTab Studio のプロジェクトではありません。'));
    var bytes = b64ToBytes(j.data.b64), d = await uploadData(bytes, j.data.name);
    setData(d, bytes, j.data.name);
    var P = Object.assign(baseProject(), j.project);                // 保存したプロジェクトは、プロジェクトの設定のまま開く
    P.settings = Object.assign(baseProject().settings, j.project.settings || {}); P.report = Object.assign(baseProject().report, j.project.report || {});
    P.tabs = (P.tabs || []).filter(function (t) { return def(t.type); }).slice(0, MAX_TABS).map(normalizeTab);
    // 以前の版で省略された表示名（「…」で終わるもの）は、省略しない名前に直す。報告書に使う名前（full）も足す
    var fixed = false;
    (P.vars || []).forEach(function (x) { var f = d.vars.find(function (y) { return y.header === x.header; }); if (!f) return; x.full = f.full; if (/…$/.test(x.label)) { x.label = f.label; fixed = true; } });
    S.P = P; if (fixed) P.tabs.forEach(autoName);
    S.P = P; S.results = {}; S.undo = []; S.redo = []; S.dirty = false; S.projName = name;
    S.ui.view = 'analysis'; S.ui.sel = P.tabs.length ? P.tabs[0].id : null; S.ui.selFig = 0; S.ui.rtab = 'home';
    App.applyZoom(); App.render(); scheduleAll();
    toast(_t('プロジェクト「{0}」を開きました（解析 {1} 件）。', name, P.tabs.length));
  }
  // アプリの中の置き場（アプリのフォルダーの projects）に保存する。CSV も一緒に入るので、開くときに CSV は要らない
  async function saveProjectAs(name, o) {
    o = o || {};
    if (!S.data) { toast(_t('保存するデータがありません。先に CSV を入れてください。'), true); return false; }
    if (!o.quiet) busy(true, _t('プロジェクトを保存しています'));
    var stamp = JSON.stringify(S.P); S.saving = true; App.renderTitle();
    try {
      var d = await api('/api/proj/save?name=' + encodeURIComponent(name), new TextEncoder().encode(projectJson()), true);
      if (!d.ok) throw new Error(d.message || _t('保存できませんでした。'));
      S.projName = d.name; S.dirty = JSON.stringify(S.P) !== stamp;              // 保存中に変えた分は未保存のまま残す
      if (S.dirty) markDirty();
      if (!o.quiet) toast(_t('プロジェクト「{0}」を保存しました。', d.name));
      return true;
    } catch (e) { toast((o.quiet ? _t('自動保存できませんでした：') : '') + e.message, true); return false; }
    finally { S.saving = false; if (!o.quiet) busy(false); App.renderTitle(); }
  }
  async function listProjects() { try { var d = await api('/api/proj/list', {}); return d.ok ? d : { items: [], folder: '' }; } catch (e) { return { items: [], folder: '' }; } }
  async function openProjectByName(name) {
    busy(true, _t('プロジェクトを開いています'));
    try { var res = await fetch('/api/proj/load', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ name: name }) }); await openProjectText(await res.text(), name); }
    catch (e) { toast(e.message, true); } finally { busy(false); }
  }
  async function exportProjectFile() {
    if (!S.data) return;
    if (await saveBlob(new Blob([projectJson()], { type: 'application/octet-stream' }), baseName() + '.prj', _t('CrossTab Studio のプロジェクト'), '.prj')) toast(_t('プロジェクトをファイルに書き出しました。別のパソコンに持っていくときに使えます。'));
  }
  // Excel ファイルなら CSV に直す（名前の拡張子は .csv に替える。データは CSV としてプロジェクトに入る）
  async function xlsxIn(buf, name) {
    var isZip = buf[0] === 0x50 && buf[1] === 0x4B, isOle = buf[0] === 0xD0 && buf[1] === 0xCF;
    if (!(/\.(xlsx|xlsm|xls)$/i.test(name) || isZip && !/\.(prj|ctsproj)/i.test(name)) || !(isZip || isOle)) return { buf: buf, name: name };
    var r = await App.xlsxToCsv(buf);
    if (r.nSheets > 1) toast(_t('シートが {0} 枚あります。1 枚目の「{1}」を読み込みます。ほかのシートを使うときは、そのシートを 1 枚目にして保存し直してください。', r.nSheets, r.sheet));
    return { buf: r.bytes, name: name.replace(/\.(xlsx|xlsm|xls)$/i, '') + '.csv' };
  }
  // Google スプレッドシート（フォームの回答先）の URL を、CSV を取り出す URL に直す
  function sheetCsvUrl(s) {
    s = String(s || '').trim(); var m;
    if (!/^https?:\/\//i.test(s)) throw new Error(_t('URL（https:// で始まるもの）を入れてください。'));
    if (/docs\.google\.com\/forms\//i.test(s)) throw new Error(_t('これは Google フォームの URL です。フォームの「回答」タブの「スプレッドシートにリンク」で開いたスプレッドシートの URL を入れてください。'));
    if ((m = /docs\.google\.com\/spreadsheets\/d\/e\/([^/?#]+)/i.exec(s))) {          // 「ウェブに公開」の URL
      var g = /[?&#]gid=(\d+)/.exec(s);
      return 'https://docs.google.com/spreadsheets/d/e/' + m[1] + '/pub?output=csv' + (g ? '&single=true&gid=' + g[1] : '');
    }
    if ((m = /docs\.google\.com\/spreadsheets\/d\/([^/?#]+)/i.exec(s))) {             // 共有のリンク（編集画面の URL）
      var g2 = /[?&#]gid=(\d+)/.exec(s);
      return 'https://docs.google.com/spreadsheets/d/' + m[1] + '/export?format=csv' + (g2 ? '&gid=' + g2[1] : '');
    }
    return s;                                                                    // CSV を直接返す URL
  }
  function URL_HELP() { return _t('スプレッドシートの共有を「リンクを知っている全員（閲覧者）」にするか、「ファイル → 共有 → ウェブに公開」で CSV を選んで作った URL を使ってください。'); }
  async function fetchSheet(url) {
    var u = sheetCsvUrl(url), r;
    try { r = await fetch(u, { credentials: 'omit', cache: 'no-store', redirect: 'follow' }); }
    catch (e) { throw new Error(_t('スプレッドシートを読み込めませんでした。共有されていないか、通信できません。') + URL_HELP()); }
    if (!r.ok) throw new Error(_t('スプレッドシートを読み込めませんでした（{0}）。', r.status) + URL_HELP());
    var buf = new Uint8Array(await r.arrayBuffer()), head = new TextDecoder().decode(buf.subarray(0, 300)).toLowerCase();
    if (/<!doctype html|<html/.test(head)) throw new Error(_t('CSV ではなく、ログインなどの画面が返ってきました。') + URL_HELP());
    return buf;
  }
  // URL から読み込む。keepTabs のときは解析を残して差し替える（最新の回答に更新）
  async function loadUrl(url, keepTabs) {
    busy(true, _t('スプレッドシートから回答を読み込んでいます'));
    try {
      var buf = await fetchSheet(url), d = new Date(), name = _t('Google スプレッドシートの回答_') + d.getFullYear() + String(d.getMonth() + 1).padStart(2, '0') + String(d.getDate()).padStart(2, '0') + '.csv';
      busy(false);
      await loadCsv(buf, name, keepTabs);
      S.P.srcUrl = String(url).trim(); markDirty(); App.render();
    } finally { busy(false); }
  }
  async function handleFile(file) {
    try {
      var buf = new Uint8Array(await file.arrayBuffer()), name = file.name;
      var r = await xlsxIn(buf, name); buf = r.buf; name = r.name;
      var isProj = /\.(prj|ctsproj)(\.json)?$/i.test(file.name) || (buf[0] === 0x7B && new TextDecoder().decode(buf.subarray(0, 60)).indexOf('"CrossTabStudio"') >= 0);
      if (isProj) { await openProjectText(new TextDecoder().decode(buf), file.name.replace(/\.(prj|ctsproj)(\.json)?$/i, '')); return; }
      if (S.data && S.P.tabs.length) { App.askReplace(buf, name); return; }
      await loadCsv(buf, name, false);
    } catch (e) { toast(e.message, true); }
  }

  window.App = window.App || {};
  Object.assign(window.App, { theme: theme, setTheme: setTheme, baseProject: baseProject, customGet: customGet, customSet: customSet, defMode: defMode, setDefMode: setDefMode, applyDefaults: applyDefaults, loadUrl: loadUrl, sheetCsvUrl: sheetCsvUrl, xlsxIn: xlsxIn, compVars: compVars, compDefs: compDefs, errLog: errLog, clearErrLog: clearErrLog, logErr: logErr, S: S, MAX_TABS: MAX_TABS, TYPE_NAMES: TYPE_NAMES, $: $, esc: esc, clone: clone, toast: toast, busy: busy, api: api, apiData: apiData, zip: zip, safeName: safeName,
    v: v, def: def, tabById: tabById, slotCols: slotCols, sameChoices: sameChoices, setItems: setItems, defaultCatsMany: defaultCatsMany, curTab: curTab, curFig: curFig, isCat: isCat, defaultCats: defaultCats, setSlot: setSlot, autoName: autoName, newTab: newTab, newFig: newFig, tabReady: tabReady,
    buildReq: buildReq, markDirty: markDirty, schedule: schedule, scheduleAll: scheduleAll, syncResults: syncResults, commit: commit, undo: undo, redo: redo, fixSel: fixSel, tableDefaults: tableDefaults,
    pickFile: pickFile, saveBlob: saveBlob, baseName: baseName, loadCsv: loadCsv, handleFile: handleFile, newProject: newProject,
    saveProjectAs: saveProjectAs, listProjects: listProjects, openProjectByName: openProjectByName, exportProjectFile: exportProjectFile });
})();
