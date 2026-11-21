// view.js ― 画面を描く（リボン、作業ウィンドウ、縦に並ぶ解析、表、報告書）
(function () {
  'use strict';
  var A = window.App, S = A.S, esc = A.esc, $ = A.$;
  var RTABS, FONTS_JA, KIND_TIPS, LEG_POS, T_DESIGNS, HEAT_BY;
  var FONTS_EN = ['Arial', 'Helvetica', 'Calibri', 'Segoe UI', 'Verdana', 'Times New Roman', 'Cambria'];
  var JA_ALIAS = { 'MS Gothic': 'ＭＳ ゴシック', 'MS PGothic': 'ＭＳ Ｐゴシック', 'MS Mincho': 'ＭＳ 明朝', 'Yu Gothic': '游ゴシック', 'Yu Mincho': '游明朝', 'Meiryo': 'メイリオ' };
  function mkConsts() {
    RTABS = [['home', _t('ホーム')], ['data', _t('データ')], ['analysis', _t('解析')], ['figure', _t('図')], ['table', _t('表')], ['export', _t('書き出し')]];
    FONTS_JA = [['MS Gothic', _t('MS ゴシック')], ['MS PGothic', _t('MS P ゴシック')], ['Yu Gothic', _t('游ゴシック')], ['Meiryo', _t('メイリオ')], ['BIZ UDGothic', _t('BIZ UD ゴシック')], ['MS Mincho', _t('MS 明朝')], ['Yu Mincho', _t('游明朝')], ['Hiragino Sans', _t('ヒラギノ角ゴ')], ['Hiragino Mincho ProN', _t('ヒラギノ明朝')]];
    KIND_TIPS = { cloud: _t('よく出た語ほど大きな文字で並べます。'), wbar: _t('よく出た語を、出現した回答数の多い順に棒で示します。'), net: _t('同じ回答に一緒に出る語どうしを線で結びます。'), hist: _t('数値の分布を、区切った範囲ごとの度数の棒で表します。'), map: _t('似た回答をする選択肢どうしが近くに来るように並べた図です。'), pie: _t('割合を円で表します。グループや設問が複数あるときは、1 つずつ円を並べます。'), stack100: _t('グループごとに、回答の割合を 100% の帯で比べます。クロス集計の基本の図です。'), group: _t('回答ごとの棒をグループ別に並べます。'), stack: _t('人数を積み上げた棒で比べます。'), heat: _t('表の形のまま、値が大きいセルを濃くします。カラーバーで色と値の対応を示します。'), bar: _t('選択肢ごとの棒グラフです。'), line: _t('時間の流れに沿った変化を線で結んで示します。'), mean: _t('群ごとの平均（点）と 95% 信頼区間（線）を示します。平均の比較（t 検定・分散分析）の基本の図です。'), box: _t('数値の分布を箱とひげで比べます。'), scatter: _t('2 つの数値の関係を点で表します。') };
    LEG_POS = [['right', _t('右')], ['bottom', _t('下')], ['top', _t('上')], ['inTR', _t('図の中（右上）')], ['inTL', _t('図の中（左上）')], ['free', _t('自由な位置')], ['none', _t('なし')]];
    T_DESIGNS = [['grid', _t('格子'), 't_grid', _t('すべてのセルを線で囲みます。')], ['three', _t('三線'), 't_three', _t('上下の太い線と見出しの下の線だけの表です。論文でよく使われます。')], ['stripe', _t('縞'), 't_stripe', _t('1 行おきに薄い色を付けます。')], ['plain', _t('線なし'), 't_plain', _t('見出しの下の線だけにします。')], ['head', _t('見出し塗り'), 't_head', _t('見出しの行を濃い色で塗ります。')]];
    HEAT_BY = [['none', _t('なし')], ['row', _t('行の割合')], ['col', _t('列の割合')], ['count', _t('人数')], ['resid', _t('残差')]];
  }
  mkConsts(); window.CTS_ONLANG.push(mkConsts);

  // ---------------- 部品
  function attrs(o) { return Object.keys(o).map(function (k) { return o[k] === false || o[k] === null || o[k] === undefined ? '' : (o[k] === true ? ' ' + k : ' ' + k + '="' + esc(o[k]) + '"'); }).join(''); }
  function rbGroup(label) { return '<div class="md-rgroup"><div class="md-rgroup-body">' + [].slice.call(arguments, 1).join('') + '</div><div class="md-rgroup-label">' + esc(label) + '</div></div>'; }
  function rbStack() { return '<div class="md-stack">' + [].slice.call(arguments).join('') + '</div>'; }
  function rbBtn(size, a) {
    return '<button class="md-rbtn md-' + size + (a.on ? ' on' : '') + '"' + attrs({ 'data-act': a.act, 'data-arg': a.arg, 'data-toggle': a.toggle, 'data-radio': a.radio, 'data-val': a.val, 'data-tip': a.tip, disabled: !!a.off }) + '>' +
      (a.ic ? window.icon(a.ic, size === 'large' ? 30 : 20) : (a.html || '')) + '<span class="md-rl">' + esc(a.name) + '</span></button>';
  }
  function rbLarge(a) { return rbBtn('large', a); }
  function rbSmall(a) { return rbBtn('small', a); }
  function get(path) {
    var i = path.indexOf('.'), sc = path.slice(0, i), k = path.slice(i + 1), t = A.curTab();
    var o = sc === 'fig' ? A.curFig() : sc === 'tbl' ? t && t.table : sc === 'tst' ? t && t.tstyle : sc === 'opt' ? t && t.opts : sc === 'tab' ? t : sc === 'set' ? S.P.settings : sc === 'rep' ? S.P.report : sc === 'proj' ? S.P : sc === 'ui' ? S.ui : null;
    if (!o) return undefined;
    if (path === 'fig.fsAll') return o.fsTick;
    return o[k];
  }
  function rbToggle(path, name, ic, tip, off) { var on = !!get(path); if (ic === 'check') ic = on ? 'checkall' : 'uncheck'; return rbSmall({ toggle: path, name: name, ic: ic, tip: tip, on: on, off: off }); }
  function rbRadio(path, val, name, ic, tip, off, large) { return rbBtn(large ? 'large' : 'small', { radio: path, val: val, name: name, ic: ic, tip: tip, on: String(get(path)) === String(val), off: off }); }
  function options(list, cur) { return list.map(function (o) { var val = Array.isArray(o) ? o[0] : o, lab = Array.isArray(o) ? o[1] : o; return '<option value="' + esc(val) + '"' + (String(val) === String(cur) ? ' selected' : '') + '>' + esc(lab) + '</option>'; }).join(''); }
  function rbSelect(label, path, list, tip, off, type) { return '<label class="md-rfield"' + attrs({ 'data-tip': tip }) + '>' + esc(label) + '<select' + attrs({ 'data-set': path, 'data-type': type, disabled: !!off }) + '>' + options(list, get(path)) + '</select></label>'; }
  function rbCombo(label, path, dl, tip, off, type) { var val = get(path); return '<label class="md-rfield"' + attrs({ 'data-tip': tip }) + '>' + esc(label) + '<input' + attrs({ 'data-set': path, 'data-type': type || 'num', list: dl, value: val === null || val === undefined ? '' : val, disabled: !!off }) + '></label>'; }
  // 並べ替えの一覧：行をクリックして選び、上の固定のボタン（または Alt＋↑↓）で選んだ行を動かす。動かした行は選んだまま
  //   key：一覧の名前（var:列番号 / fig:欄の名前）。rows：{ id, label, extra }
  function orderList(key, rows) {
    var sel = S.ui.ordSel && S.ui.ordSel.key === key ? S.ui.ordSel.id : null, i0 = rows.findIndex(function (r) { return r.id === sel; }), last = rows.length - 1;
    function btn(dir, text, tip, off) { return '<button class="md-btn md-btn-sm" data-act="ordMove" data-arg="' + esc(key) + '|' + dir + '" data-tip="' + tip + '"' + (off ? ' disabled' : '') + '>' + text + '</button>'; }
    return '<div class="md-ordtools">' + btn('top', _t('先頭へ'), _t('選んだ選択肢をいちばん上へ動かします。'), i0 <= 0) + btn('-1', _t('▲ 上へ'), _t('選んだ選択肢を 1 つ上へ動かします（Alt＋↑）。'), i0 <= 0) +
      btn('1', _t('▼ 下へ'), _t('選んだ選択肢を 1 つ下へ動かします（Alt＋↓）。'), i0 < 0 || i0 === last) + btn('bottom', _t('末尾へ'), _t('選んだ選択肢をいちばん下へ動かします。'), i0 < 0 || i0 === last) + '</div>' +
      '<div class="md-ordlist" tabindex="0" data-ordlist="' + esc(key) + '">' + rows.map(function (r, i) {
        return '<div class="md-ordrow' + (i === i0 ? ' sel' : '') + '" data-ordkey="' + esc(key) + '" data-ordid="' + esc(r.id) + '" data-tip="' + esc(r.label) + '"><span class="md-ordno">' + (i + 1) + '</span><span class="md-bl">' + esc(r.label) + '</span>' + (r.extra || '') + '</div>';
      }).join('') + '</div>';
  }
  function sec(title) { return '<div class="md-sec"><div class="md-sec-title">' + esc(title) + '</div>' + [].slice.call(arguments, 1).join('') + '</div>'; }
  function hint(t) { return '<div class="md-hint">' + esc(t) + '</div>'; }
  function grid2() { return '<div class="md-grid2">' + [].slice.call(arguments).join('') + '</div>'; }
  function grid3() { return '<div class="md-grid2" style="grid-template-columns:1fr 1fr 1fr">' + [].slice.call(arguments).join('') + '</div>'; }
  function fText(label, path, o) { o = o || {}; var val = get(path); return '<label class="md-field"><span>' + esc(label) + '</span><input type="text"' + attrs({ 'data-set': path, 'data-type': o.type, value: val === null || val === undefined ? '' : val, placeholder: o.ph, list: o.list }) + '></label>'; }
  function fSelect(label, path, list, type) { return '<label class="md-field"><span>' + esc(label) + '</span><select' + attrs({ 'data-set': path, 'data-type': type }) + '>' + options(list, get(path)) + '</select></label>'; }
  function fCheck(label, path) { return '<label class="md-check"><input type="checkbox"' + attrs({ 'data-set': path, 'data-type': 'bool', checked: !!get(path) }) + '>' + esc(label) + '</label>'; }
  function fColor(label, path, tip) { var c = get(path); return '<label class="md-field"><span>' + esc(label) + '</span><button class="md-colbtn" data-act="pickPath" data-arg="' + path + '" data-tip="' + esc(tip || _t('色を選びます。')) + '"><i style="background:' + (c || 'transparent') + '"></i>' + esc(c || _t('なし')) + '</button></label>'; }
  // 絵で選ぶタイル
  function tiles(path, list, cur) {
    cur = cur === undefined ? get(path) : cur;
    return '<div class="md-tiles">' + list.map(function (t) { return '<span class="md-tile-cap"><button class="md-tile' + (String(cur) === String(t[0]) ? ' on' : '') + '" data-radio="' + path + '" data-val="' + esc(t[0]) + '" data-tip="' + esc(t[3] || t[1]) + '">' + t[2] + '</button>' + esc(t[1]) + '</span>'; }).join('') + '</div>';
  }
  function mini(body) { return '<svg width="30" height="30" viewBox="0 0 30 30" fill="none" stroke="#1E5C3A" stroke-width="1.4" stroke-linecap="round">' + body + '</svg>'; }
  var PLOT = '<path d="M5 4v21h21" />', BLK = function (x, y, w, h) { return '<rect x="' + x + '" y="' + y + '" width="' + w + '" height="' + h + '" fill="#8FBFA3" stroke="none"/>'; };
  var MINI = {
    leg: { right: mini('<path d="M3 4v21h15"/>' + BLK(21, 6, 6, 3) + BLK(21, 11, 6, 3)), bottom: mini('<path d="M5 3v17h21"/>' + BLK(8, 24, 6, 3) + BLK(17, 24, 6, 3)), top: mini('<path d="M5 9v17h21"/>' + BLK(8, 3, 6, 3) + BLK(17, 3, 6, 3)),
      inTR: mini(PLOT + BLK(18, 6, 6, 3) + BLK(18, 11, 6, 3)), inTL: mini(PLOT + BLK(8, 6, 6, 3) + BLK(8, 11, 6, 3)), free: mini(PLOT + BLK(13, 12, 6, 3) + BLK(13, 17, 6, 3) + '<path d="M22 6l-3 3M22 6v3M22 6h-3" stroke="#2E7D32"/>'), none: mini(PLOT) },
    shape: { square: mini('<rect x="9" y="9" width="12" height="12" fill="#8FBFA3"/>'), circle: mini('<circle cx="15" cy="15" r="6.5" fill="#8FBFA3"/>'), wide: mini('<rect x="5" y="11" width="20" height="8" fill="#8FBFA3"/>') },
    tick: { out: mini('<path d="M9 4v21h17M9 9H5M9 15H5M9 21H5"/>'), 'in': mini('<path d="M9 4v21h17M9 9h4M9 15h4M9 21h4"/>'), none: mini('<path d="M9 4v21h17"/>') },
    grid: { solid: mini(PLOT + '<path d="M5 10h21M5 17h21" stroke="#7FA08F"/>'), dash: mini(PLOT + '<path d="M5 10h21M5 17h21" stroke="#7FA08F" stroke-dasharray="4 3"/>'), dot: mini(PLOT + '<path d="M5 10h21M5 17h21" stroke="#7FA08F" stroke-dasharray="1 3"/>'), none: mini(PLOT) },
    frame: { L: mini(PLOT), box: mini('<rect x="5" y="5" width="20" height="20"/>') },
    cmode: { palette: mini('<rect x="3" y="9" width="24" height="12" fill="#fff"/>' + ['#1E5C3A', '#4F8A66', '#8FBFA3', '#D3E8DA'].map(function (c, i) { return '<rect x="' + (3 + i * 6) + '" y="9" width="6" height="12" fill="' + c + '" stroke="none"/>'; }).join('') + '<rect x="3" y="9" width="24" height="12"/>'),
      series: mini('<path d="M5 4v22h21"/><rect x="8" y="15" width="4" height="11" fill="#4E79A7" stroke="none"/><rect x="14" y="10" width="4" height="16" fill="#E15759" stroke="none"/><rect x="20" y="18" width="4" height="8" fill="#59A14F" stroke="none"/><path d="M19 4l5 0M21.5 1.5v5" stroke="#2E7D32" stroke-width="1.8"/>') },
    ls: { solid: mini('<path d="M4 15h22" stroke-width="2"/>'), dashed: mini('<path d="M4 15h22" stroke-width="2" stroke-dasharray="5 3"/>'), dotted: mini('<path d="M4 15h22" stroke-width="2" stroke-dasharray="1 4"/>') },
    bar: { right: mini('<rect x="4" y="6" width="15" height="18"/>' + BLK(22, 6, 4, 18)), bottom: mini('<rect x="5" y="4" width="20" height="14"/>' + BLK(5, 22, 20, 4)), none: mini('<rect x="5" y="6" width="20" height="18"/>') },
    barShape: { smooth: '<svg width="30" height="30" viewBox="0 0 30 30"><defs><linearGradient id="mdg1" x1="0" x2="1"><stop offset="0" stop-color="#fff"/><stop offset="1" stop-color="#14532D"/></linearGradient></defs><rect x="4" y="10" width="22" height="10" fill="url(#mdg1)" stroke="#1E5C3A"/></svg>',
      step: '<svg width="30" height="30" viewBox="0 0 30 30">' + [0, 1, 2, 3].map(function (k) { return '<rect x="' + (4 + k * 5.5) + '" y="10" width="5.5" height="10" fill="' + Chart.lerp('#FFFFFF', '#14532D', (k + 0.5) / 4) + '"/>'; }).join('') + '<rect x="4" y="10" width="22" height="10" fill="none" stroke="#1E5C3A"/></svg>' }
  };
  function fromMini(group, names) { return names.map(function (n) { return [n[0], n[1], MINI[group][n[0]], n[2]]; }); }
  // 配色の一覧（濃淡・2 方向・多色・白黒）。only：出す種類を絞る
  function paletteGallery(path, cur, only, seed) {
    var groups = {}, order = [];
    Object.keys(Chart.PALETTES).forEach(function (k) { var p = Chart.PALETTES[k]; if (only && only.indexOf(p.group) < 0) return; if (p.hide && cur !== k) return; if (!groups[p.group]) { groups[p.group] = []; order.push(p.group); } groups[p.group].push(k); });
    return order.map(function (g) {
      return '<div class="md-pal-g">' + esc(_t(g)) + '</div><div class="md-pals">' + groups[g].map(function (k) {
        var P = Chart.PALETTES[k], sw = Chart.swatches(k, P.cat ? Math.min(8, P.cat.length) : P.pat ? 6 : 7, seed);
        return '<button class="md-palbtn' + (cur === k ? ' on' : '') + '" data-radio="' + path + '" data-val="' + k + '" data-tip="' + esc(_t(P.name) + (P.tip ? _t('：') + _t(P.tip) : '')) + '"><span class="md-palsw">' + sw.map(function (c) { return '<i style="background:' + patCss(c) + '"></i>'; }).join('') + '</span><span class="md-palname">' + esc(_t(P.name)) + '</span></button>';
      }).join('') + '</div>';
    }).join('');
  }
  function patCss(c) {
    if (!/^pat:/.test(c)) return c;
    return { solid: '#000', white: '#fff', gray: '#9E9E9E', diag: 'repeating-linear-gradient(45deg,#000 0 1px,#fff 1px 4px)', back: 'repeating-linear-gradient(-45deg,#000 0 1px,#fff 1px 4px)', cross: 'repeating-linear-gradient(0deg,#000 0 1px,transparent 1px 4px),repeating-linear-gradient(90deg,#000 0 1px,#fff 1px 4px)', horiz: 'repeating-linear-gradient(0deg,#000 0 1px,#fff 1px 4px)', dots: 'radial-gradient(#000 1px,#fff 1.5px) 0 0/4px 4px' }[c.slice(4)] || '#fff';
  }
  // p 値。有意水準の ±0.005 の範囲では、丸めで判定を誤解させないよう小数第 4 位まで出す（有意の判定には丸める前の値を使う）
  function fmtP(p) { if (p === null || p === undefined) return '–'; if (p < 0.001) return '< ' + (S.P.settings.lead0 ? '0.001' : '.001'); var s = p.toFixed(Math.abs(p - S.P.settings.alpha) < 0.005 ? 4 : 3); return S.P.settings.lead0 ? s : s.replace(/^0/, ''); }
  function pText(p) { return 'p ' + (p < 0.001 ? '' : '= ') + fmtP(p); }
  // 係数・信頼区間：小数第 2 位に丸めると 0 になる値は、符号が分かるよう有効数字 2 桁で出す
  function fixed2s(v) { v = +v; return v !== 0 && Math.abs(v) < 0.005 ? v.toFixed(Math.min(12, 1 - Math.floor(Math.log10(Math.abs(v))))) : fixed(v, 2); }
  function fixed(v, d) { var s = (+v).toFixed(d); return /^-0\.?0*$/.test(s) ? s.slice(1) : s; }   // 「-0.00」は「0.00」にする
  function fmtCell(val, f) {
    if (val === null || val === undefined || (typeof val === 'number' && isNaN(val))) return '–';
    if (f === 'int') return String(Math.round(val)); if (f === 'pct') return (val * 100).toFixed(1); if (f === 'num1') return fixed(val, 1);
    if (f === 'num2') return fixed(val, 2); if (f === 'num2s') return fixed2s(val); if (f === 'num3') return fixed(val, 3); if (f === 'p') return fmtP(val); return String(val);
  }

  // ---------------- 表（画面・コピー・報告書で同じものを使う）
  // 表の形：{ caption, rows:[[文字列…]], total:{row,col}, bold:[[0/1]], note, heat:{ V, lo, hi, by, label } }
  function xtabTables(res, tab) {
    var x = res.xtab, t = tab.table, out = [], nr = x.rows.length, nc = x.cols.length;
    var rt = x.count.map(function (r) { return r.reduce(function (a, b) { return a + b; }, 0); }), ct = x.cols.map(function (_, j) { return x.count.reduce(function (a, r) { return a + r[j]; }, 0); }), n = rt.reduce(function (a, b) { return a + b; }, 0);
    var marks = t.marks && x.resid && res.sig, mode = t.pct;
    function cell(c, base) { return mode === 'none' || !base ? String(c) : c + ' (' + (c / base * 100).toFixed(1) + '%)'; }
    var rows = [[''].concat(x.cols, [_t('合計')])], bold = [[]];
    for (var i = 0; i < nr; i++) {
      var r = [x.rows[i]], b = [0];
      for (var j = 0; j < nc; j++) { var mk = marks ? (x.resid[i][j] > x.crit ? ' ▲' : x.resid[i][j] < -x.crit ? ' ▽' : '') : ''; r.push(cell(x.count[i][j], mode === 'row' ? rt[i] : ct[j]) + mk); b.push(mk ? 1 : 0); }
      r.push(cell(rt[i], mode === 'row' ? rt[i] : n)); rows.push(r); bold.push(b);
    }
    rows.push([_t('合計')].concat(ct.map(function (c, j) { return cell(c, mode === 'row' ? n : ct[j]); }), [cell(n, n)]));
    var notes = [];
    if (mode !== 'none') notes.push(_t('（　）内は{0}の合計に対する割合。', mode === 'row' ? _t('行') : _t('列')));
    if (marks) notes.push(_t('▲：期待される値より有意に多い　▽：有意に少ない（調整済み標準化残差の絶対値が {0} を超えるセル{1}）。', x.crit.toFixed(2), x.residMode === 'bonf' ? _t('、ボンフェローニ法で補正') : ''));
    else if (t.marks && x.resid && res.sig === false) notes.push(_t('検定全体が有意でないため、セルごとの印（▲▽）は付けていません。'));
    // セルの色分け（表のデザインの設定）
    var by = tab.tstyle.heat, heat = null;
    if (by === 'resid' && !x.resid) by = 'row';
    if (by && by !== 'none') {
      var V = x.count.map(function (r2, i2) { return r2.map(function (c, j2) { return by === 'row' ? (rt[i2] ? c / rt[i2] * 100 : 0) : by === 'col' ? (ct[j2] ? c / ct[j2] * 100 : 0) : by === 'resid' ? x.resid[i2][j2] : c; }); });
      var flat = [].concat.apply([], V), mx = Math.max.apply(null, flat.map(Math.abs));
      heat = by === 'resid' ? { V: V, lo: -Math.max(2, Math.ceil(mx)), hi: Math.max(2, Math.ceil(mx)), by: by, label: _t('調整済み標準化残差') }
        : { V: V, lo: 0, hi: by === 'count' ? mx : Math.min(100, Math.ceil(mx / 10) * 10), by: by, label: by === 'row' ? _t('行の割合 (%)') : by === 'col' ? _t('列の割合 (%)') : _t('人数') };
    }
    out.push({ caption: x.rowLabel + ' × ' + x.colLabel + _t('（人数{0}）', mode === 'none' ? '' : mode === 'row' ? _t('と行の割合') : _t('と列の割合')), rows: rows, bold: bold, total: { row: true, col: true }, note: notes.join(' '), heat: heat });
    function mtab(cap, M, d, note) { var rr = [[''].concat(x.cols)]; M.forEach(function (r3, i3) { rr.push([x.rows[i3]].concat(r3.map(function (v) { return v === null ? '–' : fixed(v, d); }))); }); out.push({ caption: cap, rows: rr, note: note }); }
    if (t.expected && x.expected) mtab(_t('期待度数（関連がないと仮定したときの人数）'), x.expected, 1, _t('5 未満のセルが多いときは、フィッシャーの正確確率検定が適しています。'));
    if (t.resid && x.resid) mtab(_t('調整済み標準化残差'), x.resid, 2, _t('絶対値が {0} を超えるセルは、期待される値との差が有意。', x.crit.toFixed(2)));
    return out;
  }
  function gridTable(g) {
    var rows = [[g.corner || ''].concat(g.cols)], bold = [[]];
    g.rows.forEach(function (rn, i) {
      var c = g.cells[i], b = [0];
      rows.push([rn].concat(c.map(function (val, j) { b.push((g.mark && g.mark[i] && g.mark[i][j]) || (g.markRows && g.markRows[i] && j === g.markCol - 1) ? 1 : 0); return fmtCell(val, g.fmt[j]); })));
      bold.push(b);
    });
    if (g.total) rows.push([_t('合計')].concat(g.total.map(function (val, j) { return fmtCell(val, g.fmt[j]); })));
    return { caption: g.title, rows: rows, bold: bold, total: { row: !!g.total }, note: g.note || '', textCols: g.fmt.map(function (f) { return f === 'text'; }) };
  }
  function tablesOf(tab, res) {
    if (!res || !res.ok) return [];
    var out = [];
    if (res.xtab) out = out.concat(xtabTables(res, tab));
    (res.grids || []).forEach(function (g) { out.push(gridTable(g)); });
    return out;
  }
  // デザイン（線・色・文字）を 1 つずつのセルに直接書く。Word や Excel に貼っても見た目が保たれる
  function tableHtml(t, ts, handles) {
    ts = ts || A.tableDefaults();
    var nR = t.rows.length, line = ts.lw + 'px ' + ts.ls + ' ' + ts.lc, thick = Math.max(1, ts.lw * 1.75) + 'px ' + (ts.ls === 'double' ? 'solid' : ts.ls) + ' ' + ts.lc, D = ts.design;
    var ff = "'" + ts.fontEn + "', '" + ts.fontJa + "'" + (JA_ALIAS[ts.fontJa] ? ", '" + JA_ALIAS[ts.fontJa] + "'" : '') + ', sans-serif';
    var h = '<table class="md-t" style="border-collapse:collapse;font-family:' + esc(ff) + ';font-size:' + ts.fs + 'px;color:' + ts.ink + ';background:#FFFFFF' + (+ts.tw > 0 ? ';width:' + (+ts.tw) + 'px' : '') + '">';
    h += '<caption style="caption-side:top;text-align:left;font-weight:bold;padding:0 0 4px;color:' + ts.ink + '">' + esc(t.caption) + '</caption>';
    t.rows.forEach(function (r, i) {
      h += +ts.rowH > 0 ? '<tr style="height:' + (+ts.rowH) + 'px">' : '<tr>';
      r.forEach(function (c, j) {
        var head = i === 0, rowHead = j === 0, st = 'padding:4px 9px;', last = i === nR - 1;
        if (D === 'grid') st += 'border:' + line + ';';
        else {
          if (i === 0 && D !== 'plain') st += 'border-top:' + (D === 'head' ? line : thick) + ';';
          if (head) st += 'border-bottom:' + line + ';';
          if (last) st += 'border-bottom:' + (D === 'plain' ? 'none' : D === 'head' ? line : thick) + ';';
          if (D === 'head' && !head && !last) st += 'border-bottom:' + Math.max(0.5, ts.lw / 2) + 'px ' + ts.ls + ' ' + ts.lc + ';';
        }
        var bg = '', ink = ts.ink;
        if (head && (D === 'grid' || D === 'head') && ts.headFill) { bg = ts.headFill; ink = ts.headInk; }
        else if (rowHead && D === 'grid' && ts.headFill) { bg = ts.headFill; ink = ts.headInk; }
        else if (D === 'stripe' && !head && i % 2 === 1 && ts.stripe) bg = ts.stripe;
        var isTot = (t.total && t.total.row && last) || (t.total && t.total.col && j === r.length - 1);
        if (t.heat && !head && !rowHead && !isTot) { var hv = t.heat.V[i - 1][j - 1]; bg = Chart.heatColor({ palette: ts.heatPal }, t.heat.by, (hv - t.heat.lo) / ((t.heat.hi - t.heat.lo) || 1)); ink = Chart.inkOn(bg); }
        if (bg) st += 'background:' + bg + ';';
        st += 'color:' + ink + ';';
        var txt = (t.textCols && t.textCols[j - 1]) || rowHead;
        st += 'text-align:' + (head ? 'center' : txt ? 'left' : 'right') + ';' + (head || rowHead || (t.bold && t.bold[i] && t.bold[i][j]) ? 'font-weight:bold;' : '') + (head || txt ? 'white-space:normal;max-width:200px;line-break:strict;' + (rowHead ? 'min-width:7em;' : head ? 'min-width:3.5em;' : '') : 'white-space:nowrap;');
        h += (head || rowHead ? '<th' : '<td') + ' style="' + st + '">' + esc(c) + (head || rowHead ? '</th>' : '</td>');
      });
      h += '</tr>';
    });
    h += '</table>';
    if (handles) h = '<div class="md-tbox">' + h + handles + '</div>';
    if (t.heat && ts.bar !== 'none') h = '<div class="md-tbar-wrap" style="display:flex;gap:14px;align-items:' + (ts.bar === 'right' ? 'flex-start;flex-direction:row' : 'flex-start;flex-direction:column') + '">' + h + colorbarHtml(t.heat, ts) + '</div>';
    return h + (t.note ? '<div class="md-tnote" style="font-family:' + esc(ff) + '">' + esc(t.note) + '</div>' : '<div class="md-tgap"></div>');
  }
  function colorbarHtml(heat, ts) {
    var vert = ts.bar === 'right', n = ts.barShape === 'step' ? 5 : 24, cells = '', mid = heat.by === 'resid' ? 0 : (heat.lo + heat.hi) / 2, f = function (v) { return String(Math.round(v * 10) / 10); };
    for (var k = 0; k < n; k++) { var c = Chart.heatColor({ palette: ts.heatPal }, heat.by, ((vert ? n - 1 - k : k) + 0.5) / n); cells += '<div style="flex:1;background:' + c + '"></div>'; }
    var bar = '<div style="display:flex;flex-direction:' + (vert ? 'column' : 'row') + ';' + (vert ? 'width:14px;height:120px' : 'width:200px;height:14px') + ';border:1px solid #000">' + cells + '</div>';
    var labs = vert ? '<div style="display:flex;flex-direction:column;justify-content:space-between;height:122px;font-size:' + (ts.fs - 1) + 'px"><span>' + f(heat.hi) + '</span><span>' + f(mid) + '</span><span>' + f(heat.lo) + '</span></div>'
      : '<div style="display:flex;justify-content:space-between;width:202px;font-size:' + (ts.fs - 1) + 'px"><span>' + f(heat.lo) + '</span><span>' + f(mid) + '</span><span>' + f(heat.hi) + '</span></div>';
    return '<div class="md-tbar" style="font-size:' + (ts.fs - 1) + 'px;color:' + ts.ink + ';' + (vert ? 'margin-top:22px' : '') + '"><div style="margin-bottom:3px">' + esc(heat.label) + '</div><div style="display:flex;gap:4px;flex-direction:' + (vert ? 'row' : 'column') + '">' + bar + labs + '</div></div>';
  }
  // 表を画像にするための SVG（画面の表と同じデザインの決まりで描く）
  var tctx = document.createElement('canvas').getContext('2d');
  function tableSvg(t, ts) {
    var ff = "'" + ts.fontEn + "', '" + ts.fontJa + "'" + (JA_ALIAS[ts.fontJa] ? ", '" + JA_ALIAS[ts.fontJa] + "'" : '') + ', sans-serif', fs = +ts.fs || 13, lh = fs * 1.35, px = 9, py = 4, o = [], D = ts.design;
    function tw(s, bold) { tctx.font = (bold ? 'bold ' : '') + fs + 'px ' + ff.replace(/'/g, '"'); return tctx.measureText(String(s)).width; }
    function wrapT(s, maxW, bold) { s = String(s); var out = [], cur = ''; for (var i = 0; i < s.length; i++) { if (cur && tw(cur + s[i], bold) > maxW) { out.push(cur); cur = s[i]; } else cur += s[i]; } out.push(cur); return out; }
    function X(v) { return String(v).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); }
    var nR = t.rows.length, nC = t.rows[0].length, cells = t.rows.map(function (r, i) { return r.map(function (c, j) { var head = i === 0, rowHead = j === 0, txt = (t.textCols && t.textCols[j - 1]) || rowHead, bold = head || rowHead || !!(t.bold && t.bold[i] && t.bold[i][j]); return { lines: head || txt ? wrapT(c, 200, bold) : [String(c)], bold: bold, align: head ? 'middle' : txt ? 'start' : 'end' }; }); });
    var cw = [], rh = [], i, j;
    for (j = 0; j < nC; j++) cw[j] = Math.ceil(Math.max.apply(null, cells.map(function (r) { return Math.max.apply(null, r[j].lines.map(function (l) { return tw(l, r[j].bold); })); })) + px * 2);
    for (i = 0; i < nR; i++) rh[i] = Math.ceil(Math.max.apply(null, cells[i].map(function (c) { return c.lines.length; })) * lh + py * 2);
    if (+ts.rowH > 0) for (i = 0; i < nR; i++) rh[i] = Math.max(rh[i], +ts.rowH);
    var W0 = cw.reduce(function (a, b) { return a + b; }, 0);
    if (+ts.tw > W0) { var add = +ts.tw - W0; cw = cw.map(function (c) { return c + add * c / W0; }); }
    var W = cw.reduce(function (a, b) { return a + b; }, 0), H = rh.reduce(function (a, b) { return a + b; }, 0), capH = lh + 4, y0 = capH, dash = ts.ls === 'dashed' ? ' stroke-dasharray="5 3"' : ts.ls === 'dotted' ? ' stroke-dasharray="1 3"' : '';
    function ln(x1, y1, x2, y2, w) { if (w > 0) o.push('<line x1="' + x1 + '" y1="' + y1 + '" x2="' + x2 + '" y2="' + y2 + '" stroke="' + ts.lc + '" stroke-width="' + w + '"' + dash + '/>'); }
    o.push('<text x="0" y="' + (fs * 0.95) + '" font-family="' + ff + '" font-size="' + fs + '" font-weight="bold" fill="' + ts.ink + '">' + X(t.caption) + '</text>');
    var y = y0, thick = Math.max(1, ts.lw * 1.75);
    for (i = 0; i < nR; i++) {
      var x = 0, head = i === 0, last = i === nR - 1;
      for (j = 0; j < nC; j++) {
        var c = cells[i][j], rowHead = j === 0, bg = '', ink = ts.ink, isTot = (t.total && t.total.row && last) || (t.total && t.total.col && j === nC - 1);
        if (head && (D === 'grid' || D === 'head') && ts.headFill) { bg = ts.headFill; ink = ts.headInk; } else if (rowHead && D === 'grid' && ts.headFill) { bg = ts.headFill; ink = ts.headInk; } else if (D === 'stripe' && !head && i % 2 === 1 && ts.stripe) bg = ts.stripe;
        if (t.heat && !head && !rowHead && !isTot) { var hv = t.heat.V[i - 1][j - 1]; bg = Chart.heatColor({ palette: ts.heatPal }, t.heat.by, (hv - t.heat.lo) / ((t.heat.hi - t.heat.lo) || 1)); ink = Chart.inkOn(bg); }
        if (bg) o.push('<rect x="' + x + '" y="' + y + '" width="' + cw[j] + '" height="' + rh[i] + '" fill="' + bg + '"/>');
        if (D === 'grid' && ts.lw > 0) o.push('<rect x="' + x + '" y="' + y + '" width="' + cw[j] + '" height="' + rh[i] + '" fill="none" stroke="' + ts.lc + '" stroke-width="' + ts.lw + '"' + dash + '/>');
        var tx = c.align === 'middle' ? x + cw[j] / 2 : c.align === 'start' ? x + px : x + cw[j] - px, ty = y + (rh[i] - c.lines.length * lh) / 2 + fs * 0.98;
        c.lines.forEach(function (l, k) { o.push('<text x="' + tx + '" y="' + (ty + k * lh) + '" font-family="' + ff + '" font-size="' + fs + '"' + (c.bold ? ' font-weight="bold"' : '') + ' text-anchor="' + c.align + '" fill="' + ink + '">' + X(l) + '</text>'); });
        x += cw[j];
      }
      if (D !== 'grid') {
        if (head && D !== 'plain') ln(0, y, W, y, D === 'head' ? ts.lw : thick);
        if (head) ln(0, y + rh[i], W, y + rh[i], ts.lw);
        else if (last) { if (D !== 'plain') ln(0, y + rh[i], W, y + rh[i], D === 'head' ? ts.lw : thick); }
        else if (D === 'head') ln(0, y + rh[i], W, y + rh[i], Math.max(0.5, ts.lw / 2));
      }
      y += rh[i];
    }
    var totalW = W, totalH = y0 + H;
    if (t.heat && ts.bar !== 'none') {                                            // カラーバー
      var vert = ts.bar === 'right', n = ts.barShape === 'step' ? 5 : 24, bl = vert ? Math.min(140, Math.max(80, H - 20)) : Math.min(220, Math.max(120, W)), bt = 14, bx = vert ? W + 18 : 0, by = vert ? y0 + lh + 4 : y0 + H + lh + 12, mid = t.heat.by === 'resid' ? 0 : (t.heat.lo + t.heat.hi) / 2, f1 = function (v) { return String(Math.round(v * 10) / 10); }, fsb = fs - 1;
      o.push('<text x="' + bx + '" y="' + (by - 6) + '" font-family="' + ff + '" font-size="' + fsb + '" fill="' + ts.ink + '">' + X(t.heat.label) + '</text>');
      for (var k2 = 0; k2 < n; k2++) { var col = Chart.heatColor({ palette: ts.heatPal }, t.heat.by, ((vert ? n - 1 - k2 : k2) + 0.5) / n); o.push(vert ? '<rect x="' + bx + '" y="' + (by + bl * k2 / n) + '" width="' + bt + '" height="' + (bl / n + 0.6) + '" fill="' + col + '"/>' : '<rect x="' + (bx + bl * k2 / n) + '" y="' + by + '" width="' + (bl / n + 0.6) + '" height="' + bt + '" fill="' + col + '"/>'); }
      o.push('<rect x="' + bx + '" y="' + by + '" width="' + (vert ? bt : bl) + '" height="' + (vert ? bl : bt) + '" fill="none" stroke="#000000" stroke-width="1"/>');
      [[t.heat.hi, 0], [mid, 0.5], [t.heat.lo, 1]].forEach(function (q) {
        if (vert) o.push('<text x="' + (bx + bt + 5) + '" y="' + (by + bl * q[1] + fsb * 0.35) + '" font-family="' + ff + '" font-size="' + fsb + '" fill="' + ts.ink + '">' + f1(q[0]) + '</text>');
        else o.push('<text x="' + (bx + bl * (1 - q[1])) + '" y="' + (by + bt + fsb * 1.1) + '" font-family="' + ff + '" font-size="' + fsb + '" text-anchor="middle" fill="' + ts.ink + '">' + f1(q[0]) + '</text>');
      });
      if (vert) { tctx.font = fsb + 'px ' + ff.replace(/'/g, '"'); totalW = Math.max(totalW, bx + Math.max(bt + 8 + tctx.measureText('-00.0').width, tctx.measureText(t.heat.label).width)); totalH = Math.max(totalH, by + bl + 6); } else totalH = by + bt + fsb * 1.1 + 8;
    }
    if (t.note) { var nl = wrapT(t.note, Math.max(totalW, 360), false), ny = totalH + 6 + fs; nl.forEach(function (l, k3) { o.push('<text x="0" y="' + (ny + k3 * lh) + '" font-family="' + ff + '" font-size="' + (fs - 1) + '" fill="#444444">' + X(l) + '</text>'); }); totalH = ny + (nl.length - 1) * lh + 6; totalW = Math.max(totalW, Math.max.apply(null, nl.map(function (l) { return tw(l, false); }))); }
    return { g: o.join(''), w: Math.ceil(totalW) + 2, h: Math.ceil(totalH) + 2 };
  }
  function tableSvgDoc(t, ts) { var r = tableSvg(t, ts); return '<svg xmlns="http://www.w3.org/2000/svg" width="' + (r.w + 20) + '" height="' + (r.h + 16) + '" viewBox="0 0 ' + (r.w + 20) + ' ' + (r.h + 16) + '"><g transform="translate(10,8)">' + r.g + '</g></svg>'; }
  function tablesSvg(tab, res, tsOver) {
    var ts = tablesOf(tab, res), y = 10, w = 0, parts = [];
    ts.forEach(function (t) { var r = tableSvg(t, tsOver || tab.tstyle); parts.push('<g transform="translate(10,' + y + ')">' + r.g + '</g>'); y += r.h + 16; w = Math.max(w, r.w); });
    if (!ts.length) return null;
    return '<svg xmlns="http://www.w3.org/2000/svg" width="' + (w + 20) + '" height="' + (y - 4) + '" viewBox="0 0 ' + (w + 20) + ' ' + (y - 4) + '"><rect width="100%" height="100%" fill="#FFFFFF"/>' + parts.join('') + '</svg>';
  }
  function tableTsv(t) { return t.caption + '\n' + t.rows.map(function (r) { return r.join('\t'); }).join('\n') + (t.note ? '\n' + t.note : ''); }

  // ---------------- 図
  // 図は、結果と設定が同じなら前に描いたものを使い回す（解析が多いときに、画面の描き直しを軽くする）
  var figCache = new Map();
  function figsOf(tab, res) {
    var m = Chart.model(res); if (!m) return [];
    return tab.figs.map(function (f) {
      var key = tab.id + '|' + (res.sig0 || '') + '|' + S.P.settings.figLang + '|' + JSON.stringify(f), hit = figCache.get(key);
      if (!hit || hit.res !== res) { hit = { res: res, r: Chart.render(m, f, S.P.settings.figLang) }; figCache.set(key, hit); if (figCache.size > 600) figCache.delete(figCache.keys().next().value); }
      return { m: m, st: f, r: hit.r };
    });
  }
  function figSvgHtml(tab, k) { var fg = figsOf(tab, S.results[tab.id])[k]; return fg ? fg.r : null; }

  // ---------------- 上の部分
  function renderTitle() {
    var auto = !!(S.data && S.P.settings.autoSave), st = S.saving ? _t('保存しています…') : S.dirty ? _t('未保存の変更あり') : S.projName ? (auto ? _t('自動保存済み') : _t('保存済み')) : '';
    $('#md_title').innerHTML = window.icon('k_stack100', 20) + '<b>CrossTab Studio</b>' +
      '<button class="md-autosave' + (auto ? ' on' : '') + '" data-act="autoSave"' + (S.data ? '' : ' disabled') + _t(' data-tip="オンにすると、変更するたびに数秒後にプロジェクトを自動で上書き保存します。"><span>自動保存</span><i></i><span>{0}</span></button><button class="md-qsave{1}" data-act="save"{2} data-tip="{3}">{4}<span>上書き保存</span></button><span class="md-sub">アンケートの集計と検定</span><span class="md-file">', auto ? _t('オン') : _t('オフ'), S.dirty && S.data ? ' dirty' : '', S.data ? '' : ' disabled', S.dirty ? _t('未保存の変更があります。押すと上書き保存します（Ctrl+S）。') : _t('プロジェクトを上書き保存します（Ctrl+S）。'), window.icon('save', 18)) +
      esc(S.data ? (S.projName ? S.projName + _t('　') : '') + _t('（{0}）', S.data.name) + (st ? _t('　') + st : '') : '') + '</span>';
  }
  function renderTabs() {
    $('#md_tabs').innerHTML = _t('<button class="md-tab md-tab-file" data-act="fileMenu" data-tip="開く・保存などのファイルの操作">ファイル</button>{0}<button class="md-pane-toggle" data-act="togglePane" data-tip="右の作業ウィンドウを開く・閉じる">作業ウィンドウ ', RTABS.map(function (t) { return '<button class="md-tab' + (S.ui.rtab === t[0] ? ' active' : '') + '" data-rtab="' + t[0] + '">' + t[1] + '</button>'; }).join('')) + (S.ui.pane ? '›' : '‹') + '</button>';
  }
  function renderRibbon() {
    var tab = A.curTab(), res = tab ? S.results[tab.id] : null, ok = !!(res && res.ok), has = !!S.data, noTab = !tab, m = ok ? Chart.model(res) : null, fig = A.curFig(), noFig = !m || !fig;
    var kindNow = !noFig ? Chart.kindOf(m, fig) : null, isBar = kindNow && ['stack100', 'stack', 'group', 'bar', 'box', 'mean', 'coef'].indexOf(kindNow) >= 0, noVal = noFig || ['stack100', 'stack', 'box', 'mean', 'coef', 'scatter'].indexOf(kindNow) >= 0;
    var idx = tab ? S.P.tabs.indexOf(tab) : -1, canSwap = tab && ((tab.slots.a && tab.slots.b) || (tab.slots.x && tab.slots.y)), xt = !!(res && res.ok && res.xtab), h = {};
    var gFile = rbGroup(_t('ファイル'), rbLarge({ act: 'openCsv', name: _t('CSV を開く'), ic: 'csv', tip: _t('回答の CSV ファイル、または Excel ファイル（.xlsx。1 枚目のシート）を選んで読み込みます（Ctrl+O）。') }),
      rbStack(rbSmall({ act: 'openProject', name: _t('プロジェクトを開く'), ic: 'open', tip: _t('保存したプロジェクトの一覧から開きます。CSV は要りません。') }),
        rbSmall({ act: 'save', name: _t('上書き保存'), ic: 'save', tip: _t('データ・解析・図と表の設定をまとめて保存します（Ctrl+S）。'), off: !has }),
        rbSmall({ act: 'saveAs', name: _t('名前を付けて保存'), ic: 'saveas', tip: _t('別の名前でプロジェクトを保存します。'), off: !has })));
    var gEdit = rbGroup(_t('編集'), rbStack(rbSmall({ act: 'undo', name: _t('元に戻す'), ic: 'undo', tip: _t('直前の操作を取り消します（Ctrl+Z）。'), off: !S.undo.length }), rbSmall({ act: 'redo', name: _t('やり直す'), ic: 'redo', tip: _t('取り消した操作をもう一度行います（Ctrl+Y）。'), off: !S.redo.length })));
    var first = !S.P.tabs.length, bNew = rbLarge({ act: 'newAnalysis', name: first ? _t('解析を始める') : _t('新しい解析'), ic: 'add', tip: first ? _t('解析の種類を選んで、最初の解析を始めます。') : _t('解析の種類を選んで、いちばん下に新しい解析を足します。'), off: !has });
    var sOrder = rbStack(rbSmall({ act: 'moveAn', arg: '-1', name: _t('上へ'), ic: 'up', tip: _t('選んだ解析を 1 つ上へ動かします。見出しをドラッグしても並べ替えられます。'), off: idx < 1 }),
      rbSmall({ act: 'moveAn', arg: '1', name: _t('下へ'), ic: 'down', tip: _t('選んだ解析を 1 つ下へ動かします。'), off: idx < 0 || idx >= S.P.tabs.length - 1 }));
    var sFold = rbStack(rbSmall({ act: 'foldAll', arg: '1', name: _t('すべて畳む'), ic: 'fold', tip: _t('すべての解析を畳んで、名前だけの一覧にします。有意な解析は名前が赤い太字になります。'), off: !S.P.tabs.length }),
      rbSmall({ act: 'foldAll', arg: '0', name: _t('すべて開く'), ic: 'unfold', tip: _t('すべての解析を開きます。'), off: !S.P.tabs.length }));
    var sEditAn = rbStack(rbSmall({ act: 'renameAn', name: _t('名前を変更'), ic: 'rename', tip: _t('選んだ解析の名前を変えます（F2）。図のファイル名にも使われます。'), off: noTab }),
      rbSmall({ act: 'dupTab', name: _t('複製'), ic: 'dup', tip: _t('選んだ解析と同じ設定の解析をもう 1 つ作ります。'), off: noTab }),
      rbSmall({ act: 'delTab', name: _t('削除'), ic: 'del', tip: _t('選んだ解析を削除します（元に戻せます）。'), off: noTab }));
    var bExport = rbLarge({ act: 'exportAll', name: _t('まとめて書き出す'), ic: 'exportall', tip: _t('☑ を付けた解析の図・Excel ファイル・報告書を、選んだフォルダーに書き出します。'), off: !has || !S.P.tabs.length });
    h.home = gFile + gEdit + rbGroup(_t('解析'), bNew, sEditAn, sOrder, sFold) + rbGroup(_t('書き出し'), bExport);
    var sv = S.ui.selVar !== null ? A.v(S.ui.selVar) : null;
    h.data = rbGroup(_t('取り込み'), rbLarge({ act: 'openCsv', name: _t('CSV を開く'), ic: 'csv', tip: _t('回答の CSV ファイル、または Excel ファイル（.xlsx。1 枚目のシート）を選んで読み込みます（Ctrl+O）。') }),
      rbLarge({ act: 'replaceCsv', name: _t('差し替え'), ic: 'refresh', tip: _t('回答が増えた新しい CSV（または Excel ファイル）に差し替えます。同じ質問の列には、解析の設定を引き継ぎます。'), off: !has }),
      rbStack(rbSmall({ act: 'openUrl', name: _t('URL から'), ic: 'link', tip: _t('Google フォームの回答のスプレッドシートを、URL から読み込みます（共有の設定が必要です）。') }),
        rbSmall({ act: 'refreshUrl', name: _t('最新に更新'), ic: 'refresh', tip: _t('URL から読み込んだスプレッドシートを読み込み直し、増えた回答を反映します。解析の設定は残ります。'), off: !(has && S.P.srcUrl) }))) +
      rbGroup(_t('列を作る'), rbLarge({ act: 'scoreNew', name: _t('尺度得点を作る'), ic: 'add', tip: _t('複数の項目（順序のある質問）の平均点・合計点を、新しい列として作ります。t 検定や相関などで使えます。'), off: !has }), rbLarge({ act: 'periodNew', name: _t('日時で回に分ける'), ic: 'period', tip: _t('タイムスタンプの日時で回答を「第 1 回」「第 2 回」…に分けた列を作ります。回どうしの比較（クロス集計・t 検定・分散分析など）に使えます。'), off: !has || !S.P.vars.some(function (y) { return y.type === 'time'; }) })) +
      rbGroup(_t('表示'), rbRadio('ui.dataView', 'vars', _t('質問の一覧'), 'vars', _t('列（質問）ごとの種類と回答数を表示します。'), !has, true), rbRadio('ui.dataView', 'raw', _t('回答の一覧'), 'data', _t('回答そのものを表で表示します。'), !has, true)) +
      rbGroup(_t('選んだ列'), rbStack(_t('<label class="md-rfield" data-tip="選んだ列の種類を変えます。自動の判定が合わないときに直します。">種類<select data-var="type"') + (sv ? '' : ' disabled') + '>' + options([['nominal', _t('単一回答')], ['ordinal', _t('順序あり')], ['multi', _t('複数回答')], ['numeric', _t('数値')], ['time', _t('日時')], ['text', _t('使わない（自由記述など）')]], sv ? (sv.type === 'empty' ? 'text' : sv.type) : '') + '</select></label>',
        rbSmall({ act: 'freqOfVar', name: _t('この列を集計'), ic: 'add', tip: _t('選んだ列の単純集計を作ります。'), off: !sv || !A.isCat(sv) })));
    h.analysis = rbGroup(_t('解析'), bNew, rbLarge({ act: 'changeType', name: _t('種類を変える'), ic: 'change', tip: _t('選んだ列はそのままで、解析の種類だけを変えます。'), off: noTab }), sEditAn) +
      rbGroup(_t('並びと開閉'), sOrder, sFold) +
      rbGroup(_t('列'), rbLarge({ act: 'swap', name: _t('行と列を入れ替え'), ic: 'swap', tip: _t('2 つの質問の役割（行と列）を入れ替えます。'), off: !canSwap })) +
      rbGroup(_t('計算'), rbLarge({ act: 'rerun', name: _t('計算し直す'), ic: 'refresh', tip: _t('選んだ解析をもう一度計算します。'), off: noTab }));
    var km = m ? Chart.kinds(m) : ['stack100', 'group', 'stack', 'heat'], heat = kindNow === 'heat';
    h.figure = rbGroup(_t('図'), rbStack(rbSmall({ act: 'addFig', name: _t('図を追加'), ic: 'figadd', tip: _t('この解析に、別の種類の図をもう 1 つ足します。図 1、図 2 … と番号が付きます。'), off: !m }),
        rbSmall({ act: 'delFig', name: _t('図を削除'), ic: 'figdel', tip: _t('選んだ図を削除します（元に戻せます）。'), off: noFig }),
        rbSmall({ act: 'figReset', name: _t('最初の状態'), ic: 'refresh', tip: _t('選んだ図の設定を最初の状態に戻します。'), off: noFig }))) +
      rbGroup(_t('種類'), km.map(function (k) { return rbBtn('large', { radio: 'fig.kind', val: k, name: _t(Chart.KIND_NAMES[k]), ic: 'k_' + k, tip: KIND_TIPS[k], off: noFig, on: k === kindNow }); }).join('')) +
      rbGroup(_t('向きと値'), rbStack(rbRadio('fig.orient', 'h', _t('横向き'), 'horiz', _t('棒を横向きにします。選択肢の名前が長いときに読みやすくなります。'), !isBar), rbRadio('fig.orient', 'v', _t('縦向き'), 'vert', _t('棒を縦向きにします。'), !isBar)),
        rbStack(rbRadio('fig.value', 'pct', _t('割合'), 'pct', _t('割合（%）で表します。'), noVal), rbRadio('fig.value', 'count', _t('人数'), 'nlabel', _t('人数で表します。'), noVal))) +
      (heat ? rbGroup(_t('カラーバー'), rbStack(rbSelect(_t('基準'), 'fig.heatBy', HEAT_BY.slice(1), _t('セルの色を決める値です。'), noFig), rbSelect(_t('位置'), 'fig.bar', [['right', _t('右')], ['bottom', _t('下')], ['none', _t('なし')]], _t('色と値の対応を示す帯（カラーバー）の位置です。'), noFig), rbSelect(_t('形'), 'fig.barShape', [['smooth', _t('なめらか')], ['step', _t('5 段階')]], _t('カラーバーの形です。'), noFig)))
        : rbGroup(_t('凡例'), rbStack(rbSelect(_t('位置'), 'fig.legend', LEG_POS, _t('凡例（色の説明）の位置です。「なし」で消せます。'), noFig), rbSelect(_t('形'), 'fig.legShape', [['square', _t('四角')], ['circle', _t('丸')], ['wide', _t('横長')]], _t('凡例の色見本の形です。'), noFig), rbToggle('fig.legFrame', _t('枠で囲む'), 'check', _t('凡例を線で囲みます。'), noFig)))) +
      rbGroup(_t('目盛り'), rbStack(rbSelect(_t('向き'), 'fig.tickDir', [['out', _t('外向き')], ['in', _t('内向き')], ['none', _t('なし')]], _t('軸の目盛りの向きです。'), noFig || heat || kindNow === 'pie'), rbSelect(_t('目盛線'), 'fig.grid', [['solid', _t('実線')], ['dash', _t('破線')], ['dot', _t('点線')], ['none', _t('なし')]], _t('目盛りの位置に引く線の種類です。'), noFig || heat || kindNow === 'pie'), rbSelect(_t('枠'), 'fig.frame', [['L', _t('左と下だけ')], ['box', _t('四方を囲む')]], _t('グラフを描く範囲を四方の線で囲むか、左と下の軸だけにするかを選びます。'), noFig || heat || kindNow === 'pie'))) +
      rbGroup(_t('文字'), rbStack(rbSelect(_t('和文'), 'fig.fontJa', FONTS_JA, _t('図の中の日本語の書体です。'), noFig), rbSelect(_t('欧文'), 'fig.fontEn', FONTS_EN, _t('図の中の英数字の書体です。'), noFig),
        '<div class="md-rfield">' + rbCombo(_t('大きさ'), 'fig.fsAll', 'dl_fs', _t('図の文字の大きさをまとめて変えます。細かくは作業ウィンドウで変えられます。'), noFig) +
        _t('<button class="md-rbtn md-small" data-act="fsStep" data-arg="1" data-tip="文字を 1 段階大きくします。"{0}>{1}</button><button class="md-rbtn md-small" data-act="fsStep" data-arg="-1" data-tip="文字を 1 段階小さくします。"', noFig ? ' disabled' : '', window.icon('fup', 20)) + (noFig ? ' disabled' : '') + '>' + window.icon('fdown', 20) + '</button></div>')) +
      rbGroup(_t('色と表示'), rbLarge({ act: 'palettePop', arg: 'fig.palette', name: _t('配色'), ic: 'palette', tip: _t('色の組み合わせを一覧から選びます。系列ごとに色を選ぶときは、作業ウィンドウの「色」で「系列ごとに選ぶ」にします。'), off: noFig }),
        rbStack(rbToggle('fig.labels', _t('値のラベル'), 'labels', _t('棒の中や先に数値を書きます。入りきらない所は自動で省きます。'), noFig), rbToggle('fig.showN', _t('人数 (n)'), 'nlabel', _t('グループの名前の下に人数を書きます。'), noFig), rbToggle('fig.transparent', _t('背景を透明に'), 'transparent', _t('図の背景を透明にします。スライドに重ねるときに使います。'), noFig)));
    var ts = tab ? tab.tstyle : null;
    h.table = rbGroup(_t('デザイン'), T_DESIGNS.map(function (d) { return rbBtn('large', { act: 'tDesign', arg: d[0], name: d[1], ic: d[2], tip: d[3], off: !ok, on: ts && ts.design === d[0] }); }).join('')) +
      rbGroup(_t('割合'), rbStack(rbRadio('tbl.pct', 'row', _t('行の割合'), 'pct', _t('行（グループ）ごとの割合を人数に添えます。'), !xt), rbRadio('tbl.pct', 'col', _t('列の割合'), 'pct', _t('列（回答）ごとの割合を人数に添えます。'), !xt), rbRadio('tbl.pct', 'none', _t('人数のみ'), 'table', _t('割合を出さず、人数だけの表にします。'), !xt))) +
      rbGroup(_t('加える'), rbStack(rbToggle('tbl.marks', _t('多い・少ないの印'), 'resid', _t('期待される値より有意に多いセルに ▲、少ないセルに ▽ を付けます。'), !(xt && res.xtab.resid)), rbToggle('tbl.expected', _t('期待度数'), 'table', _t('関連がないと仮定したときの人数の表を加えます。'), !(xt && res.xtab.expected)), rbToggle('tbl.resid', _t('残差'), 'resid', _t('調整済み標準化残差の表を加えます。'), !(xt && res.xtab.resid)))) +
      rbGroup(_t('セルの色分け'), rbStack(rbSelect(_t('基準'), 'tst.heat', HEAT_BY, _t('セルに値の大きさに応じた色を付けます。'), !xt), rbSelect(_t('カラーバー'), 'tst.bar', [['right', _t('右')], ['bottom', _t('下')], ['none', _t('なし')]], _t('色と値の対応を示す帯（カラーバー）の位置です。'), !xt), rbSelect(_t('形'), 'tst.barShape', [['smooth', _t('なめらか')], ['step', _t('5 段階')]], _t('カラーバーの形です。'), !xt)),
        rbLarge({ act: 'palettePop', arg: 'tst.heatPal', name: _t('配色'), ic: 'palette', tip: _t('セルの色分けに使う色を選びます。'), off: !xt })) +
      rbGroup(_t('文字と線'), rbStack(rbSelect(_t('和文'), 'tst.fontJa', FONTS_JA, _t('表の日本語の書体です。'), !ok), rbSelect(_t('欧文'), 'tst.fontEn', FONTS_EN, _t('表の英数字の書体です。'), !ok), rbCombo(_t('大きさ'), 'tst.fs', 'dl_fs', _t('表の文字の大きさです。'), !ok)),
        rbStack(rbCombo(_t('線の太さ'), 'tst.lw', 'dl_lw', _t('表の線の太さです。'), !ok), rbSelect(_t('線の種類'), 'tst.ls', [['solid', _t('実線')], ['dashed', _t('破線')], ['dotted', _t('点線')], ['double', _t('二重線')]], _t('表の線の種類です。'), !ok),
          _t('<div class="md-rfield" data-tip="表の線の色です。">線の色<button class="md-colbtn md-colbtn-sm" data-act="pickPath" data-arg="tst.lc"') + (ok ? '' : ' disabled') + '><i style="background:' + (ts ? ts.lc : '#7F7F7F') + '"></i></button></div>')) +
      rbGroup(_t('表の書き出し'), rbStack(rbSmall({ act: 'tblCopy', name: _t('コピー'), ic: 'copy', tip: _t('表をデザインごとコピーします。Word や Excel に貼り付けられます。'), off: !ok }), rbSmall({ act: 'tblCsv', name: _t('CSV で保存'), ic: 'csv', tip: _t('表を CSV ファイルで保存します。'), off: !ok }), rbSmall({ act: 'tblPng', name: _t('画像で保存'), ic: 'png', tip: _t('表を見たままの画像（PNG）で保存します。'), off: !ok })));
    var anyEdited = S.P.tabs.some(function (t) { return t.reportText !== null; });
    h.export = rbGroup(_t('まとめて'), bExport, rbStack(rbSmall({ act: 'checkAll', arg: '1', name: _t('すべて選ぶ'), ic: 'checkall', tip: _t('すべての解析に ☑ を付けます。'), off: !S.P.tabs.length }), rbSmall({ act: 'checkAll', arg: '0', name: _t('すべて外す'), ic: 'uncheck', tip: _t('すべての解析の ☑ を外します。'), off: !S.P.tabs.length }))) +
      rbGroup(_t('選んだ図'), rbLarge({ act: 'figPng', name: _t('画像で保存'), ic: 'png', tip: _t('選んだ図を、画面に出ている形と大きさのまま PNG 画像で保存します。'), off: noFig }),
        rbStack(rbSmall({ act: 'figSvg', name: _t('SVG で保存'), ic: 'svg', tip: _t('選んだ図を SVG（拡大しても粗くならない形式）で保存します。'), off: noFig }), rbSmall({ act: 'figCopy', name: _t('図をコピー'), ic: 'copy', tip: _t('選んだ図を画像としてコピーします。'), off: noFig }))) +
      rbGroup(_t('報告書'), rbLarge({ act: 'goReport', name: _t('報告書を見る'), ic: 'report', tip: _t('☑ を付けた解析の文と表を 1 つにまとめた報告書を表示します。'), off: !has }),
        rbLarge({ act: 'repDocx', name: _t('Word で保存'), ic: 'docx', tip: _t('報告書を Word ファイル（.docx）で保存します。文は MS 明朝・Times New Roman、表は Word の表（あとから直せる）、図は画像で入ります。'), off: !has }),
        rbLarge({ act: 'repTxt', name: _t('テキストで保存'), ic: 'txt', tip: _t('報告書を 1 つのテキストファイル（.txt）で保存します。'), off: !has }),
        rbStack(rbToggle('rep.tables', _t('表を入れる'), 'table', _t('報告書に各解析の表を入れます。'), !has), rbToggle('rep.holm', _t('補正後の p'), 'holm', _t('検定を繰り返したことを考えて、ホルム法で補正した p を一覧に入れます。'), !has),
          rbSmall({ act: 'resetAllText', name: _t('定型文に戻す'), ic: 'refresh', tip: _t('書き換えた文をすべて、結果から作った定型文に戻します。'), off: !anyEdited }))) +
      rbGroup(_t('計算の確かめ'), rbLarge({ act: 'excelOnly', name: _t('Excel で保存'), ic: 'excel', tip: _t('データと、☑ を付けた解析を Excel の関数で再現したシートを保存します。'), off: !has || !S.P.tabs.length }) +
        rbLarge({ act: 'rOnly', name: _t('R のコード'), ic: 'rcode', tip: _t('☑ を付けた解析を R の標準の関数で計算し直すスクリプト（.R）と、そのためのデータ（CSV）を ZIP で保存します。R で実行すると、結果と「アプリの値」を見比べられます。'), off: !has || !S.P.tabs.length })) +
      rbGroup(_t('プロジェクト'), rbLarge({ act: 'projFile', name: _t('ファイルに書き出す'), ic: 'saveas', tip: _t('プロジェクトを 1 つのファイル（.prj）にして、好きな場所に保存します。別のパソコンに移すときに使います。'), off: !has }));
    h.settings = rbGroup(_t('判定と表記'), rbStack(rbSelect(_t('有意水準'), 'set.alpha', [[0.05, _t('5%（0.05）')], [0.01, _t('1%（0.01）')], [0.1, _t('10%（0.10）')]], _t('「有意」と判定する基準です。すべての解析に使われます。'), false, 'num'),
        rbSelect(_t('p の表記'), 'set.lead0', [['true', 'p = 0.012'], ['false', 'p = .012']], _t('p 値の書き方です。投稿先や指導者の指定に合わせます。'), false, 'boolstr'))) +
      rbGroup(_t('図'), rbStack(rbSelect(_t('自動の文字'), 'set.figLang', [['ja', _t('日本語')], ['en', _t('英語')]], _t('軸の名前など、図に自動で入る文字の言語です。'), false),
        rbSelect(_t('画像の細かさ'), 'set.pngScale', [[1, _t('標準')], [2, _t('2 倍（資料向け）')], [4, _t('4 倍（印刷向け）')]], _t('画像で保存するときの細かさです。貼り付けたときの大きさは、画面と同じになります。'), false, 'num'))) +
      rbGroup(_t('画面'), rbStack(rbSelect(_t('拡大'), 'set.zoom', [[90, '90%'], [100, '100%'], [115, '115%'], [130, '130%']], _t('画面全体の大きさを変えます。'), false, 'num'))) +
      rbGroup(_t('情報'), rbLarge({ act: 'about', name: _t('このアプリ'), ic: 'info', tip: _t('使っている統計手法と、計算をしている R の情報を表示します。') }) + rbLarge({ act: 'feedback', name: _t('ご意見・不具合'), ic: 'report', tip: _t('不具合の報告や、ご意見・要望を送る方法を表示します（GitHub を使います）。') }));
    $('#md_ribbon').innerHTML = RTABS.map(function (t) { return '<div class="md-ribbon-body' + (S.ui.rtab === t[0] ? ' active' : '') + '" data-tab="' + t[0] + '">' + h[t[0]] + '</div>'; }).join('');
  }

  // ---------------- 下の帯（表示の切り替え）
  function renderSheets() {
    var sigN = S.P.tabs.filter(function (t) { var r = S.results[t.id]; return r && r.ok && r.sig === true; }).length;
    $('#md_sheets').innerHTML = [['analysis', _t('解析'), _t('解析を上から順に並べた画面です。')], ['data', _t('データ'), _t('読み込んだデータ（質問と回答）です。')], ['report', _t('報告書'), _t('☑ を付けた解析をまとめた報告書です。')]].map(function (v) {
      return '<button class="md-sheet md-sheet-fixed' + (S.ui.view === v[0] ? ' active' : '') + '" data-view="' + v[0] + '" data-tip="' + v[2] + '"' + (S.data ? '' : ' disabled') + '>' + v[1] + '</button>';
    }).join('') + _t('<span class="md-sheet-count">解析 {0} 件', S.P.tabs.length) + (S.P.tabs.length ? _t('　有意 {0} 件', sigN) : '') + '</span>';
  }

  // ---------------- 中央
  function renderCenter() {
    var el = $('#md_view'), keep = el.scrollTop, key = S.data ? S.ui.view + '/' + S.ui.dataView : 'drop', same = el.dataset.key === key;
    var an = same ? anchorOf(el) : null, hs = same ? sizesOf(el) : null;
    if (!S.data) el.innerHTML = dropHtml();
    else if (S.ui.view === 'data') el.innerHTML = S.ui.dataView === 'raw' ? rawHtml() : varsHtml();
    else if (S.ui.view === 'report') { el.innerHTML = reportHtml(); ensureHolm(); }
    else el.innerHTML = analysisPage();
    if (hs) applySizes(el, hs);
    if (A.fitFigs) A.fitFigs();
    el.scrollTop = same ? keep : 0; el.dataset.key = key;
    growLater(el);
    if (an) keepAnchor(el, an);
  }
  // ---- 描き直しても、見ていた場所（選んでいる図、なければ画面のいちばん上にあった部品）を同じ高さに保つ
  //   描き直した直後は、画面の外の解析の高さがまだ決まっておらず（content-visibility）、文の欄も縮んでいる。
  //   スクロールの数値だけを戻すと別の場所を指すので、見ていた部品の位置で合わせ直す
  var ANCHORS = '.md-an-head[data-anhead], .md-figwrap[data-fig], .md-tblwrap[data-tbl], textarea[data-reptext], .md-an[data-an]';
  function keyOf(n) {
    var a = ['data-fig', 'data-tbl', 'data-anhead', 'data-reptext', 'data-an'];
    for (var i = 0; i < a.length; i++) if (n.hasAttribute(a[i])) return n.tagName.toLowerCase() + '[' + a[i] + '="' + n.getAttribute(a[i]) + '"]';
    return null;
  }
  function anchorOf(el) {
    var vr = el.getBoundingClientRect(); if (!vr.height) return null;
    var pick = null, sel = el.querySelector('.md-figwrap.sel');
    if (sel) { var r = sel.getBoundingClientRect(); if (r.bottom > vr.top + 20 && r.top < vr.bottom - 20) pick = sel; }
    if (!pick) {
      var ns = el.querySelectorAll(ANCHORS);
      for (var i = 0; i < ns.length; i++) { var q = ns[i].getBoundingClientRect(); if (q.height && q.bottom > vr.top + 1 && !ns[i].matches('.md-an')) { pick = ns[i]; break; } }
      if (!pick) for (var j = 0; j < ns.length; j++) { var q2 = ns[j].getBoundingClientRect(); if (q2.height && q2.bottom > vr.top + 1) { pick = ns[j]; break; } }
    }
    var k = pick && keyOf(pick); if (!k) return null;
    return { sel: k, off: pick.getBoundingClientRect().top - vr.top };
  }
  function sizesOf(el) {
    var h = { an: {}, ta: {} };
    el.querySelectorAll('.md-an[data-an]').forEach(function (s) { h.an[s.dataset.an] = s.offsetHeight; });
    el.querySelectorAll('textarea[data-reptext]').forEach(function (t) { h.ta[t.dataset.reptext] = t.style.height; });
    return h;
  }
  // 新しい欄に、前の高さを仮に入れておく（画面の外の解析が、決まった高さ 640px で数えられないように）
  function applySizes(el, h) {
    el.querySelectorAll('.md-an[data-an]').forEach(function (s) { var v = h.an[s.dataset.an]; if (v) s.style.containIntrinsicSize = 'auto ' + v + 'px'; });
    el.querySelectorAll('textarea[data-reptext]').forEach(function (t) { var v = h.ta[t.dataset.reptext]; if (v) t.style.height = v; });
  }
  var anchorSeq = 0, userMoved = false;
  ['wheel', 'touchmove', 'keydown', 'mousedown'].forEach(function (t) { document.addEventListener(t, function (e) { if (e.target.closest && e.target.closest('#md_view')) userMoved = true; }, { passive: true, capture: true }); });
  function stopKeep() { anchorSeq++; }   // ほかの処理が意図して画面を動かすときに呼ぶ（その動きを打ち消さないように）
  function keepAnchor(el, an) {
    var my = ++anchorSeq; userMoved = false;
    function fix() {
      if (my !== anchorSeq || userMoved) return;
      var n = el.querySelector(an.sel); if (!n) return;
      var d = n.getBoundingClientRect().top - el.getBoundingClientRect().top - an.off;
      if (Math.abs(d) > 1) el.scrollTop += d;
    }
    fix(); requestAnimationFrame(function () { fix(); requestAnimationFrame(fix); });
    [60, 160, 400, 800].forEach(function (ms) { setTimeout(fix, ms); });
  }
  // 文の欄の高さ合わせは、画面に近づいた欄だけ行う（解析が多いとき、すべての欄を一度に測ると描き直しが重くなるため）
  var growIO = window.IntersectionObserver ? new IntersectionObserver(function (es) { es.forEach(function (e) { if (e.isIntersecting) { growIO.unobserve(e.target); autoGrow(e.target); } }); }, { rootMargin: '600px 0px' }) : null;
  function growLater(root) { var ts = root.querySelectorAll('textarea[data-auto]'); if (!growIO) { ts.forEach(autoGrow); return; } growIO.disconnect(); ts.forEach(function (t) { growIO.observe(t); }); }
  function autoGrow(t) { t.style.height = 'auto'; t.style.height = (t.scrollHeight + 4) + 'px'; }
  function dropHtml() {
    var list = S.projList && S.projList.items && S.projList.items.length ? _t('<div class="md-projlist"><div class="md-sec-title">保存したプロジェクト</div>') + S.projList.items.slice(0, 8).map(function (p) {
      return '<button class="md-projitem" data-act="openProjName" data-arg="' + esc(p.name) + _t('" data-tip="このプロジェクトを開きます。CSV は要りません。"><b>') + esc(p.name) + '</b><span>' + esc(p.saved) + '</span></button>';
    }).join('') + (S.projList.items.length > 8 ? _t('<button class="md-link" data-act="openProject">ほかのプロジェクトを見る</button>') : '') + '</div>' : '';
    return '<div class="md-drop" id="md_drop">' + window.icon('csv', 56) + _t('<h1>ここに CSV ファイルをドロップ</h1><p>Google フォームの回答（スプレッドシートの「ファイル → ダウンロード → カンマ区切り形式（.csv）」）をそのまま入れられます。Excel ファイル（.xlsx）も、1 枚目のシートを読み込めます。1 行目が質問、2 行目以降が回答の表です。</p><div class="md-row"><button class="md-btn md-btn-primary" data-act="openCsv">ファイルを選ぶ</button><button class="md-btn" data-act="openUrl">Google スプレッドシートの URL から</button></div>{0}<p>データはこのパソコンの中だけで処理され、外部には送信されません。</p></div>', list);
  }
  // 数値の列：直した回答・読めない回答の件数と、確かめる画面へのボタン。数値として読める選択肢の列には切り替えのボタン
  function numCell(x) {
    var c = x.clean;
    if (x.type === 'numeric' && c && (c.nFixed || c.nFail)) return _t('<div class="md-numfix">直した {0} 件・読めない {1} 件{2} <button class="md-btn md-btn-sm" data-act="numClean" data-arg="{3}">確かめる</button>', c.nFixed, c.nFail, x.fix && Object.keys(x.fix).length ? _t('（手直し {0}）', Object.keys(x.fix).length) : '', x.col) + (x.typeOrig ? ' <button class="md-btn md-btn-sm" data-act="asChoice" data-arg="' + x.col + _t('">選択肢に戻す</button>') : '') + '</div>';
    if (x.type === 'numeric' && x.typeOrig) return '<div class="md-numfix"><button class="md-btn md-btn-sm" data-act="asChoice" data-arg="' + x.col + _t('">選択肢に戻す</button></div>');
    if (x.numCand && x.type !== 'numeric') return '<div class="md-numfix"><button class="md-btn md-btn-sm" data-act="asNumeric" data-arg="' + x.col + _t('" data-tip="単位や全角文字を直して、数値として読み直します。ヒストグラムや平均の比較に使えるようになります。">数値として読む</button></div>');
    return '';
  }
  // 「質問の一覧」の種類の欄：その場で種類を選び直せる（スマートフォンでも作業ウィンドウを開かずに変えられるように）
  var TYPE_OPTS = function () { return [['nominal', _t('単一回答')], ['ordinal', _t('順序あり')], ['multi', _t('複数回答')], ['numeric', _t('数値')], ['time', _t('日時')], ['text', _t('使わない（自由記述など）')]]; };
  function typeSelect(x) { return '<select class="md-typesel" data-vartype="' + x.col + '" data-tip="' + _t('列の種類を変えます。自動の判定が合わないときに直します。') + '">' + options(TYPE_OPTS(), x.type === 'empty' ? 'text' : x.type) + '</select>'; }
  function varsHtml() {
    var h = _t('<div class="md-page md-wide"><table class="md-vars"><thead><tr><th>列</th><th style="min-width:300px">表示名（図・表・報告書に使う名前）</th><th>質問文</th><th style="min-width:150px">種類</th><th>回答数</th><th>選択肢</th></tr></thead><tbody>');
    S.P.vars.forEach(function (x) {
      if (x.comp) { h += compRow(x); return; }
      h += '<tr class="' + (S.ui.selVar === x.col ? 'sel' : '') + '" data-varrow="' + x.col + '"><td class="num">' + x.col + '</td><td><input type="text" data-varlabel="' + x.col + '" value="' + esc(x.label) + '"></td><td class="q" data-tip="' + esc(x.header) + '">' + esc(x.header.replace(/\s+/g, ' ')) + '</td><td>' + typeSelect(x) + '</td><td class="num">' + x.n + '</td><td class="num">' + (A.isCat(x) ? x.cats.length : '–') + numCell(x) + '</td></tr>';
    });
    return h + '</tbody></table></div>';
  }
  // 尺度得点の行：作り方（項目・逆転・平均か合計か）と、直す・消すボタン
  function compRow(x) {
    if (x.comp.kind === 'period') {
      var src = A.v(x.comp.src), cs = x.cats || [];
      return '<tr class="md-comprow" data-varrow="' + x.col + '"><td class="num">' + x.col + '</td><td><input type="text" data-varlabel="' + x.col + '" value="' + esc(x.label) + '"></td><td class="q">' + esc(_t('「{0}」の日時で分けた {1} 回', src ? src.label : '?', cs.length) + (x.comp.same ? _t('（同じ人たちが答えている）') : '')) + '</td>' +
        _t('<td>日時で分けた回<br><button class="md-btn md-btn-sm" data-act="periodEdit" data-arg="{0}">直す</button> <button class="md-btn md-btn-sm" data-act="scoreDel" data-arg="{1}">消す</button></td>', x.col, x.col) +
        '<td class="num">' + cs.reduce(function (a, k) { return a + (k.n || 0); }, 0) + '</td><td>' + esc(cs.map(function (k) { return k.v; }).join(_t('、'))) + '</td></tr>';
    }
    var names = x.comp.items.map(function (c) { var y = A.v(c); return y ? (y.label + ((x.comp.reverse || []).indexOf(c) >= 0 ? _t('（逆転）') : '')) : '?'; }).join(_t('、'));
    return '<tr class="md-comprow" data-varrow="' + x.col + '"><td class="num">' + x.col + '</td><td><input type="text" data-varlabel="' + x.col + '" value="' + esc(x.label) + '"></td><td class="q">' + esc((x.comp.method === 'sum' ? _t('合計：') : _t('平均：')) + names) + '</td>' +
      _t('<td>尺度得点（{0}）<br><button class="md-btn md-btn-sm" data-act="scoreEdit" data-arg="{1}">直す</button> <button class="md-btn md-btn-sm" data-act="scoreDel" data-arg="{2}">消す</button></td><td class="num">–</td><td class="num">–</td></tr>', x.comp.method === 'sum' ? _t('合計') : _t('平均'), x.col, x.col);
  }
  function rawHtml() {
    var rows = S.data.rows, lim = Math.min(rows.length, S.ui.rawRows);
    var mk = S.P.rowMarks || {};
    var h = '<div class="md-page md-wide"><table class="md-vars md-raw"><thead><tr><th>#</th>' + _t('<th data-tip="チェックした行は、すべての解析から除きます。">無効</th><th data-tip="目印です。解析には含めます。">フラッグ</th>') + S.P.vars.filter(function (x) { return !x.comp; }).map(function (x) { return '<th data-tip="' + esc(x.header) + '">' + esc(x.label) + '</th>'; }).join('') + '</tr></thead><tbody>';
    for (var i = 0; i < lim; i++) h += '<tr id="md_row_' + (i + 1) + '" class="' + (mk[i + 1] && mk[i + 1].x ? 'md-rx' : '') + (mk[i + 1] && mk[i + 1].f ? ' md-rf' : '') + '"><td class="num">' + (i + 1) + '</td>' +
      '<td class="md-rm"><input type="checkbox" data-rowmark="' + (i + 1) + ':x"' + (mk[i + 1] && mk[i + 1].x ? ' checked' : '') + '></td><td class="md-rm"><input type="checkbox" data-rowmark="' + (i + 1) + ':f"' + (mk[i + 1] && mk[i + 1].f ? ' checked' : '') + '></td>' + rows[i].map(function (c) { return '<td>' + esc(c === null ? '' : c) + '</td>'; }).join('') + '</tr>';
    h += '</tbody></table>';
    if (lim < S.data.nrow) h += '<div class="md-empty">' + lim + _t(' 件まで表示しています（全 {0} 件）。', S.data.nrow) + (lim < rows.length ? _t('<button class="md-btn" data-act="moreRows">さらに 500 件表示</button>') : _t('画面に出せるのは {0} 件までです。解析はすべての回答で行います。', rows.length)) + '</div>';
    return h + '</div>';
  }
  var CHEV = { open: '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="#1F4030" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M3 5.5l5 5 5-5"/></svg>', closed: '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" stroke="#1F4030" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M5.500 3l5 5-5 5"/></svg>' };
  function anSummary(tab, res) {
    if (!A.tabReady(tab)) return _t('列を選んでください');
    if (!res) return _t('計算しています');
    if (!res.ok) return _t('計算できません');
    return res.test + (res.p !== null && res.p !== undefined ? _t('　') + pText(res.p) + (res.sig ? _t('　有意') : _t('　有意ではない')) : '');
  }
  function analysisPage() {
    if (!S.P.tabs.length) {
      var cnt = {}; S.P.vars.forEach(function (x) { cnt[x.type] = (cnt[x.type] || 0) + 1; });
      return _t('<div class="md-page"><div class="md-block"><div class="md-empty"><b>データを読み込みました（回答 {0} 件、{1} 列）</b><br>{2}として読み取りました。<button class="md-link" data-view="data">列の読み取り方を確かめる</button><br><br><button class="md-btn md-btn-primary" data-act="newAnalysis">解析を始める</button><br><br>解析の種類を選ぶと、この画面に解析の欄ができます。その欄で設問を選ぶと、すぐに結果が出ます。<br>解析を足すと、この画面の下へ順に並びます。見出しの ∨ で畳めます。</div></div></div>', S.data.nrow, S.data.ncol, ['nominal', 'ordinal', 'multi', 'numeric'].filter(function (k) { return cnt[k]; }).map(function (k) { return A.TYPE_NAMES[k] + ' ' + cnt[k] + _t(' 列'); }).join(_t('、')));
    }
    return '<div class="md-page" id="md_anpage">' + S.P.tabs.map(sectionHtml).join('') + '<div class="md-addrow"><button class="md-btn" data-act="newAnalysis"' + (S.P.tabs.length >= A.MAX_TABS ? ' disabled' : '') + _t('>＋ 新しい解析</button></div></div>');
  }
  function sectionHtml(tab, i) {
    var res = S.results[tab.id], sel = S.ui.sel === tab.id, sig = !!(res && res.ok && res.sig === true), n = S.P.tabs.length;
    var h = '<section class="md-an' + (sel ? ' sel' : '') + (tab.collapsed ? ' collapsed' : '') + '" data-an="' + tab.id + '"><div class="md-an-head"' + (window.CTS_TOUCH ? '' : ' draggable="true"') + ' data-anhead="' + tab.id + '">' +
      '<button class="md-chev" data-act="toggleAn" data-arg="' + tab.id + '" data-tip="' + (tab.collapsed ? _t('この解析を開きます。') : _t('この解析を畳みます。')) + '">' + (tab.collapsed ? CHEV.closed : CHEV.open) + '</button>' +
      '<input type="checkbox" data-ancheck="' + tab.id + '"' + (tab.checked ? ' checked' : '') + _t(' data-tip="まとめて書き出すときの対象にします。">') +
      '<span class="md-an-no">' + (i + 1) + '</span><span class="md-an-name' + (sig ? ' sig' : '') + '" data-anname="' + tab.id + '" data-tip="' + (sig ? _t('有意な結果です。') : '') + _t('右クリックで名前を変更できます。見出しをドラッグすると並べ替えられます。">') + esc(tab.name) + '</span>' +
      '<span class="md-an-sum">' + esc(anSummary(tab, res)) + '</span><span class="md-an-btns">' +
      '<button data-act="moveAnId" data-arg="' + tab.id + _t(':-1" data-tip="1 つ上へ動かします。"') + (i === 0 ? ' disabled' : '') + '>▲</button><button data-act="moveAnId" data-arg="' + tab.id + _t(':1" data-tip="1 つ下へ動かします。"') + (i === n - 1 ? ' disabled' : '') + '>▼</button>' +
      '<button data-act="anMenu" data-arg="' + tab.id + _t('" data-tip="名前の変更・複製・削除など">⋯</button></span></div>');
    if (!tab.collapsed) h += '<div class="md-an-body">' + sectionBody(tab, res) + '</div>';
    return h + '</section>';
  }
  // 設問を選ぶ欄（解析の欄の中に置く）
  var sameChoices = function (a, b) { return A.sameChoices(a, b); };
  function manyHtml(tab, sd) {
    var sl = tab.slots[sd.id], cols = sl.cols || [], first = cols.length ? A.v(cols[0]) : null;
    var list = cols.map(function (c, i) { var x = A.v(c); return '<div class="md-item"><span class="md-item-no">' + (i + 1) + '</span><span class="md-item-name">' + esc(x ? x.label : '') + '</span>' +
      '<button class="md-mv" data-act="itemMove" data-arg="' + tab.id + ':' + sd.id + ':' + i + _t(':-1" data-tip="1 つ上へ"{0}>▲</button><button class="md-mv" data-act="itemMove" data-arg="{1}:{2}:{3}:1" data-tip="1 つ下へ"{4}>▼</button>{5}<button class="md-mv md-danger" data-act="itemRemove" data-arg="{6}:{7}:{8}" data-tip="この設問を外します。">×</button></div>', i === 0 ? ' disabled' : '', tab.id, sd.id, i, i === cols.length - 1 ? ' disabled' : '', tab.type === 'alpha' || tab.type === 'factor' || tab.type === 'cfa' ? '<button class="md-mv md-rev' + ((sl.reverse || []).indexOf(c) >= 0 ? ' on' : '') + '" data-act="itemRev" data-arg="' + tab.id + ':' + sd.id + ':' + c + _t('" data-tip="逆の向きの項目（逆転項目）として、得点を反転して計算します。">逆転</button>') : '', tab.id, sd.id, i); }).join('');
    var opts = '<option value="">' + (cols.length ? _t('＋ 設問を追加') : sd.optional ? _t('（使わない。使うときは設問を選ぶ）') : _t('（1 つ目の設問を選んでください）')) + '</option>' + S.P.vars.filter(function (x) { return cols.indexOf(x.col) < 0; }).map(function (x) {
      var ok = sd.types.indexOf(x.type) >= 0, mark = first && ok && sameChoices(first, x);
      return '<option value="' + x.col + '"' + (ok ? '' : ' disabled') + '>' + (mark ? '◎ ' : '') + esc(x.label) + _t('　〔{0}〕</option>', esc(A.TYPE_NAMES[x.type] || x.type));
    }).join('');
    var same = first ? S.P.vars.filter(function (x) { return cols.indexOf(x.col) < 0 && sd.types.indexOf(x.type) >= 0 && sameChoices(first, x); }) : [];
    return '<div class="md-qrow md-qrow-many"><span class="md-qlab">' + esc(sd.label) + (sd.hint ? '<small>' + esc(sd.hint) + '</small>' : '') + '</span><div>' +
      (list ? '<div class="md-items">' + list + '</div>' : '') +
      '<select class="md-in' + (cols.length < 2 && !(sd.optional && !cols.length) ? ' md-empty-sel' : '') + '" data-additem="' + sd.id + '" data-tab="' + tab.id + '">' + opts + '</select>' +
      (same.length ? '<div class="md-qact" style="text-align:left;margin-top:6px"><button class="md-btn md-btn-sm" data-act="itemAddSame" data-arg="' + tab.id + ':' + sd.id + _t('" data-tip="選んだ設問と同じ選択肢の設問（◎ の付いたもの）を、すべて追加します。">同じ選択肢の設問をすべて追加（{0} 個）</button></div>', same.length) : '') +
      (cols.length === 1 ? _t('<div class="md-hint" style="margin:6px 0 0">比べる設問をもう 1 つ以上選んでください。◎ は、選んだ設問と同じ選択肢の設問です。</div>') : '') + '</div></div>';
  }
  function qselHtml(tab) {
    var d = A.def(tab.type), cols = d.slots.map(function (sd) { return sd.many ? null : tab.slots[sd.id].col; });
    var dups = cols.filter(function (c, i) { return c !== null && cols.indexOf(c) !== i; });
    var two = (tab.slots.a && tab.slots.b) || (tab.slots.x && tab.slots.y);
    return '<div class="md-qsel">' + d.slots.map(function (sd) {
      if (sd.many) return manyHtml(tab, sd);
      var sl = tab.slots[sd.id], dup = sl.col !== null && dups.indexOf(sl.col) >= 0;
      return '<label class="md-qrow"><span class="md-qlab">' + esc(sd.label) + (sd.hint ? '<small>' + esc(sd.hint) + '</small>' : '') + '</span>' +
        '<select class="md-in' + (dup ? ' md-dup' : '') + (sl.col === null && !sd.optional ? ' md-empty-sel' : '') + '" data-slot="' + sd.id + '" data-tab="' + tab.id + '"' + (dup ? _t(' data-tip="もう 1 つの欄と同じ設問です。別の設問を選んでください。"') : '') + '>' + varOptions(sd, sl.col) + '</select></label>';
    }).join('') + (two && cols.every(function (c) { return c !== null; }) ? '<div class="md-qact"><button class="md-btn md-btn-sm" data-act="swapAt" data-arg="' + tab.id + _t('" data-tip="2 つの設問の役割（行と列など）を入れ替えます。">2 つの設問を入れ替える</button></div>') : '') + '</div>';
  }
  function sectionBody(tab, res) {
    var d = A.def(tab.type), h = '';
    if (!A.tabReady(tab)) return '<div class="md-qintro"><b>' + esc(d.name) + _t('</b>　{0}</div>{1}<div class="md-hint" style="margin:6px 0 4px">設問を選ぶと、すぐに結果（p 値・表・図・報告書の文）が出ます。選択肢の名前やまとめ方、除外、絞り込みは、右の作業ウィンドウで設定できます。</div>', esc(d.desc), qselHtml(tab));
    h += qselHtml(tab);
    if (!res) return h + _t('<div class="md-empty">計算しています。</div>');
    if (!res.ok) return h + '<ul class="md-notes"><li class="err">' + esc(res.message || _t('計算できませんでした。')) + '</li></ul>';
    var a = S.P.settings.alpha, verdict = res.sig === true ? _t('有意（p < {0}）', a) : res.sig === false ? _t('有意ではない（p ≥ {0}）', a) : '';
    h += '<div class="md-result' + (res.sig ? ' sig' : '') + '"><span class="md-test">' + esc(res.test) + '</span>' +
      (res.headline || []).map(function (k) { var val = String(k.v); return '<span class="md-kv">' + esc(k.k) + (/^</.test(val) ? ' ' : ' = ') + esc(val) + '</span>'; }).join('') +
      (res.n && res.n.excluded > 0 ? _t('<span class="md-kv" data-tip="無回答、またはチェックを外した選択肢を選んだ人です。">除外 {0} 人</span>', res.n.excluded) : '') + (verdict ? '<span class="md-verdict">' + verdict + '</span>' : '') + '</div>';
    if (res.suggest === 'fisher') h += '<ul class="md-notes"><li>' + esc(res.notes[res.notes.length - 1]) + ' <button class="md-btn md-btn-sm" data-act="toFisher" data-arg="' + tab.id + _t('">フィッシャーの正確確率検定に切り替える</button></li></ul>');
    // 表を先に、図をその下に置く
    var tbs = tablesOf(tab, res);
    if (tbs.length) h += _t('<div class="md-block"><div class="md-block-head">表<span class="md-sp"><button class="md-btn md-btn-sm" data-act="tblCopyAt" data-arg="{0}" data-tip="デザインごとコピーします。Excel や Word に貼り付けられます。">コピー</button><button class="md-btn md-btn-sm" data-act="tblCsvAt" data-arg="{1}">CSV で保存</button></span></div><div class="md-block-body md-tables" data-tables="', tab.id, tab.id) + tab.id + '">' + tbs.map(function (t, ti) {
      return '<div class="md-tblwrap" data-tbl="' + tab.id + ':' + ti + '">' + tableHtml(t, tab.tstyle,
        _t('<div class="md-th md-th-e" data-h="e" data-tip="ドラッグで表の幅を変えます。"></div><div class="md-th md-th-s" data-h="s" data-tip="ドラッグで行の高さを変えます。"></div><div class="md-th md-th-se" data-h="se" data-tip="ドラッグで表の幅と行の高さを変えます。ダブルクリックで元の大きさに戻します。"></div>')) + '</div>';
    }).join('') + '</div></div>';
    var figs = figsOf(tab, res);
    if (Chart.model(res)) {
      h += _t('<div class="md-block"><div class="md-block-head">図<span class="md-sp"><button class="md-btn md-btn-sm" data-act="addFigTo" data-arg="{0}" data-tip="別の種類の図をもう 1 つ足します。">＋ 図を追加</button></span></div><div class="md-block-body">', tab.id);
      figs.forEach(function (fg, k) {
        var selF = S.ui.sel === tab.id && S.ui.selFig === k && (S.ui.rtab === 'figure' || figs.length > 1);
        h += _t('<div class="md-figrow"><div class="md-figcap">図 ') + (k + 1) + _t('　') + _t(Chart.KIND_NAMES[fg.r.kind]) + _t('　<span class="md-figsize">') + fg.r.w + ' × ' + fg.r.h + '</span><span class="md-sp"><button class="md-btn md-btn-sm" data-act="figCopyAt" data-arg="' + tab.id + ':' + k + _t('">コピー</button><button class="md-btn md-btn-sm" data-act="figPngAt" data-arg="') + tab.id + ':' + k + _t('">画像で保存</button>') +
          '<button class="md-btn md-btn-sm" data-act="delFigAt" data-arg="' + tab.id + ':' + k + _t('" data-tip="この図を削除します（元に戻せます）。">削除</button></span></div>') +
          '<div class="md-figwrap' + (selF ? ' sel' : '') + '" data-fig="' + tab.id + ':' + k + '">' + fg.r.svg + _t('<div class="md-fh md-fh-e" data-h="e" data-tip="ドラッグで幅を変えます。"></div><div class="md-fh md-fh-s" data-h="s" data-tip="ドラッグで高さを変えます。"></div><div class="md-fh md-fh-se" data-h="se" data-tip="ドラッグで大きさを変えます。"></div></div></div>');
      });
      if (!figs.length) h += _t('<div class="md-hint" style="margin:0">図はありません。「＋ 図を追加」で足せます。</div>');
      h += '</div></div>';
    }
    var edited = tab.reportText !== null, stale = edited && tab.reportBase && tab.reportBase !== res.report;
    h += _t('<div class="md-block"><div class="md-block-head">報告書の文（定型文）<span class="md-sp">{0}<button class="md-btn md-btn-sm" data-act="textCopyAt" data-arg="{1}">コピー</button></span></div><div class="md-block-body">', edited ? '<button class="md-btn md-btn-sm" data-act="resetTextAt" data-arg="' + tab.id + _t('" data-tip="結果から作った定型文に戻します。">定型文に戻す</button>') : '', tab.id) +
      (stale ? _t('<ul class="md-notes"><li>この文を書き換えたあとで結果が変わりました。数値が合っているか確かめるか、「定型文に戻す」を押してください。</li></ul>') : '') +
      '<textarea class="md-in" data-auto data-reptext="' + tab.id + '" rows="3">' + esc(edited ? tab.reportText : res.report) + '</textarea></div></div>';
    var notes = (res.notes || []).slice(0, res.suggest === 'fisher' ? -1 : undefined);
    if (notes.length) h += '<ul class="md-notes">' + notes.map(function (nn) { return '<li class="info">' + esc(nn) + '</li>'; }).join('') + '</ul>';
    return h;
  }

  // ---------------- 報告書（☑ を付けた解析を 1 つのテキストにまとめる）
  function repItems() {
    return S.P.tabs.filter(function (t) { return t.checked; }).map(function (t, i) { var r = S.results[t.id]; return { tab: t, res: r, no: i + 1, ok: !!(r && r.ok), text: r && r.ok ? (t.reportText !== null ? t.reportText : r.report) : '' }; });
  }
  function holmItems() { return repItems().filter(function (x) { return x.ok && x.res.p !== null && x.res.p !== undefined; }); }
  // ホルム法で補正する範囲（族）。解析ごとに名前を付けられ、同じ名前の検定どうしで補正する。名前のない検定は「報告書全体」の族にまとめる（既定）
  function holmGroup(tab) { return String(tab.holmGroup || '').trim(); }
  function holmSig(items) { return JSON.stringify(items.map(function (x) { return [x.tab.id, x.res.p, holmGroup(x.tab)]; })); }
  async function ensureHolmAsync() {
    var items = holmItems(), sig = holmSig(items);
    if (!items.length) { S.holm = { sig: sig, map: {}, k: 0, kOf: {}, groups: [] }; return; }
    if (S.holm && S.holm.sig === sig) return;
    var names = [], map = {}, kOf = {};
    items.forEach(function (x) { var g = holmGroup(x.tab); if (names.indexOf(g) < 0) names.push(g); });
    for (var gi = 0; gi < names.length; gi++) {
      var its = items.filter(function (x) { return holmGroup(x.tab) === names[gi]; });
      var d = await A.apiData('/api/padjust', { p: its.map(function (x) { return x.res.p; }) });
      if (!d.ok) return;
      its.forEach(function (x, i) { map[x.tab.id] = d.p[i]; kOf[x.tab.id] = its.length; });
    }
    S.holm = { sig: sig, map: map, k: items.length, kOf: kOf, groups: names.map(function (g) { return { name: g, k: items.filter(function (x) { return holmGroup(x.tab) === g; }).length }; }) };
  }
  function ensureHolm() {
    var sig = holmSig(holmItems());
    if ((S.holm && S.holm.sig === sig) || ensureHolm.busy === sig) return;
    ensureHolm.busy = sig;
    ensureHolmAsync().then(function () { ensureHolm.busy = null; if (S.ui.view === 'report') renderCenter(); }).catch(function () { ensureHolm.busy = null; });
  }
  function star(p) { return p !== null && p !== undefined && p < S.P.settings.alpha ? '*' : ''; }
  function summaryTable() {
    var holm = S.P.report.holm, items = repItems().filter(function (x) { return x.ok; });
    var grouped = holm && S.holm && (S.holm.groups || []).some(function (g) { return g.name; });
    var rows = [[_t('番号'), _t('解析'), _t('検定'), _t('統計量'), 'p'].concat(holm ? [_t('補正後の p')] : [], grouped ? [_t('補正の族')] : [], [_t('効果量'), 'n'])];
    items.forEach(function (x) {
      var r = x.res, hp = S.holm && S.holm.map[x.tab.id] !== undefined ? S.holm.map[x.tab.id] : null, hasP = r.p !== null && r.p !== undefined;
      rows.push([String(x.no), x.tab.name, r.test, (r.row && r.row.stat) || '–', hasP ? fmtP(r.p) + star(r.p) : '–'].concat(holm ? [hasP && hp !== null ? fmtP(hp) + star(hp) : '–'] : [], grouped ? [hasP ? holmGroup(x.tab) || _t('報告書全体') : '–'] : [], [(r.row && r.row.effect) || '–', String(r.n ? r.n.used : '')]));
    });
    var k = S.holm ? S.holm.k : 0;
    return { caption: _t('検定の一覧'), rows: rows, textCols: [true, true], note: _t('有意水準は {0}%。', S.P.settings.alpha * 100) + _t('* は有意水準を下回った検定（丸める前の p で判定）。') + (holm ? (grouped ? _t('補正後の p は、「補正の族」が同じ検定ごとにホルム法で補正した値（{0}）。', S.holm.groups.map(function (g) { return _t('{0} {1} 件', g.name || _t('報告書全体'), g.k); }).join(_t('、'))) + _t('検定を繰り返すと、関連がなくても偶然に有意となる検定が出やすくなるため、結論には補正後の p も参照する。')
      : _t('補正後の p は、この報告書の {0} 件の検定をまとめてホルム法で補正した値。検定を繰り返すと、関連がなくても偶然に有意となる検定が出やすくなるため、結論には補正後の p も参照する。', k)) : '') };
  }
  function repMeta() {
    var d = new Date(), pad = function (n) { return (n < 10 ? '0' : '') + n; };
    var nx = A.invalidRows().length;
    return (nx ? _t('データ：{0}（回答 {1} 件、無効回答 {3} 件を除く）　有意水準：{2}%　作成日：', S.data.name, S.data.nrow - nx, S.P.settings.alpha * 100, nx) : _t('データ：{0}（回答 {1} 件）　有意水準：{2}%　作成日：', S.data.name, S.data.nrow, S.P.settings.alpha * 100)) + d.getFullYear() + '-' + pad(d.getMonth() + 1) + '-' + pad(d.getDate());
  }
  // ---------------- 引用のしかた：このアプリと R の参考文献の書き方（作成者・URL はここで決める）
  var CITE = { en: 'Okuyama, T.', full: 'Okuyama, Taiga', ja: '奥山泰冴', jaShort: '奥山', url: 'https://okuyama23.github.io/crosstab-studio/' };
  function citeList() {
    var v = window.CTS_APP_VERSION || '', y = ((window.CTS_CHANGES || [])[0] || {}).date, yr = y ? y.slice(0, 4) : 'n.d.';
    var rv = S.r || '', ry = S.rYear || 'n.d.', url = CITE.url, bare = url.replace(/^https?:\/\//, '');
    function it(t, html) { return html ? '<i>' + t + '</i>' : t; }
    return [
      { name: _t('APA（第 7 版）'), ref: function (h) { return CITE.en + ' (' + yr + '). ' + it('CrossTab Studio', h) + ' (Version ' + v + ') [Computer software]. ' + url; }, inText: _t('（{0}, {1}）、日本語の文では {2}（{3}）', CITE.en.split(',')[0], yr, CITE.jaShort, yr) },
      { name: _t('MLA（第 9 版）'), ref: function (h) { return CITE.full + '. ' + it('CrossTab Studio', h) + '. Version ' + v + ', ' + yr + ', ' + bare + '.'; } },
      { name: _t('シカゴ（第 17 版、著者－年方式）'), ref: function (h) { return CITE.full + '. ' + yr + '. ' + it('CrossTab Studio', h) + '. Version ' + v + '. ' + url + '.'; } },
      { name: _t('日本語の論文の例（SIST 02 に準じた形）'), ref: function () { return CITE.ja + _t('. CrossTab Studio. バージョン {0}, {1}. {2}, (参照 ', v, yr, url) + (function (d) { return d.getFullYear() + '-' + ('0' + (d.getMonth() + 1)).slice(-2) + '-' + ('0' + d.getDate()).slice(-2); })(new Date()) + ').'; } },
      { name: _t('R（APA 第 7 版）'), ref: function (h) { return 'R Core Team. (' + ry + '). ' + it('R: A language and environment for statistical computing', h) + (rv ? ' (Version ' + rv + ')' : '') + ' [Computer software]. R Foundation for Statistical Computing. https://www.R-project.org/'; } }
    ];
  }
  // 報告書の末尾に入れる「使用したソフトウェア」
  function citeReport() {
    var c = citeList();
    return { title: _t('使用したソフトウェア'), text: _t('集計・検定・図の作成には CrossTab Studio（バージョン {0}）を用い、計算には R{1}を用いた。この結果を論文・レポート・発表などで使うときは、CrossTab Studio を引用していただけると幸いです。引用の例（APA 第 7 版）：', window.CTS_APP_VERSION || '', S.r ? _t('（バージョン {0}）', S.r) : ''),
             refs: [c[0].ref(false), c[4].ref(false)] };
  }
  function reportPlain() {
    var items = repItems(), L = [S.P.title, repMeta(), ''];
    if (!items.length) return L.concat([_t('（☑ を付けた解析がありません）')]).join('\r\n');
    if (items.some(function (x) { return x.ok; })) L.push('■ ' + tableTsv(summaryTable()), '');
    items.forEach(function (x) {
      L.push('■ ' + x.no + '. ' + x.tab.name + _t('（{0}）', x.ok ? x.res.test : A.def(x.tab.type).name));
      if (!x.ok) { L.push(x.res ? (x.res.message || _t('計算できませんでした。')) : _t('列がまだ選ばれていません。'), ''); return; }
      if (x.res.method) L.push(_t('【方法】') + x.res.method, _t('【結果】') + x.text, ''); else L.push(x.text, '');
      if (S.P.report.tables) tablesOf(x.tab, x.res).forEach(function (t) { L.push(tableTsv(t), ''); });
    });
    var cr = citeReport(); L.push('■ ' + cr.title, cr.text); cr.refs.forEach(function (r) { L.push(r); });
    return L.join('\n').replace(/\r?\n/g, '\r\n');
  }
  function reportHtml() {
    var n = repItems().length;
    return '<div class="md-page"><div class="md-doc"><input class="md-doctitle" type="text" data-set="proj.title" value="' + esc(S.P.title) + _t('" data-tip="報告書の題名です。"><div class="md-meta">☑ を付けた {0} 件の解析を、上から順にまとめています。PDF には図と表も入ります。文は各解析の「報告書の文」で書き換えられます。<span class="md-row" style="margin-top:6px"><button class="md-btn md-btn-primary" data-act="repPdf">PDF で保存（図・表を含む）</button><button class="md-btn" data-act="repDocx">Word で保存</button><button class="md-btn" data-act="repTxt">テキストで保存</button><button class="md-btn" data-act="repCopy">全文をコピー</button><button class="md-btn" data-view="analysis">解析に戻る</button></span></div>', n) +
      '<pre class="md-reportpre">' + esc(reportPlain()) + '</pre></div></div>';
  }

  // ---------------- 作業ウィンドウ
  function renderPane() {
    var pane = $('#md_pane'), body = $('#md_pane_body');
    pane.classList.toggle('closed', !S.ui.pane);
    if (!S.ui.pane) return;
    var prevKey = body.dataset.key; if (prevKey) S.ui.paneScroll[prevKey] = body.scrollTop;
    $('#md_pane_sel').innerHTML = options(RTABS, S.ui.rtab);
    var f = { home: paneHome, data: paneData, analysis: paneAnalysis, figure: paneFigure, table: paneTable, export: paneExport, settings: paneSettings }[S.ui.rtab], key = S.ui.rtab + ':' + (S.ui.sel || '');
    var ae = document.activeElement, ordKey = ae && ae.dataset && ae.dataset.ordlist;   // 並べ替えの一覧を操作中なら、描き直した後も続けて操作できるようにする
    body.innerHTML = f(); body.dataset.key = key; body.scrollTop = S.ui.paneScroll[key] || 0;
    if (ordKey) { var ol = [].find.call(body.querySelectorAll('[data-ordlist]'), function (l) { return l.dataset.ordlist === ordKey; }); if (ol) ol.focus({ preventScroll: true }); }
    body.querySelectorAll('textarea[data-auto]').forEach(autoGrow);
  }
  function target(tab) { return _t('<div class="md-target" data-tip="作業ウィンドウの設定は、この解析に効きます。解析の見出しや図をクリックすると切り替わります。">対象：<b>') + (S.P.tabs.indexOf(tab) + 1) + '. ' + esc(tab.name) + '</b></div>'; }
  function needTab(what) { var nm = S.P.tabs.length ? _t('新しい解析') : _t('解析を始める'); return sec(what, hint(S.data ? (S.P.tabs.length ? _t('解析の見出しをクリックして、対象の解析を選んでください。') : _t('「解析を始める」で解析を作ってください。')) : _t('先に CSV ファイルを入れてください。')), S.data ? '<button class="md-btn md-btn-primary" data-act="newAnalysis">' + nm + '</button>' : _t('<button class="md-btn md-btn-primary" data-act="openCsv">CSV を開く</button>')); }
  function step(n, done, title, sub, act, arg) { return '<button class="md-step' + (done ? ' done' : '') + '" data-act="' + act + '"' + (arg ? ' data-arg="' + arg + '"' : '') + '><span class="md-num">' + n + '</span><span>' + esc(title) + '<small>' + esc(sub) + '</small></span></button>'; }
  function paneHome() {
    var anyOk = S.P.tabs.some(function (t) { return S.results[t.id] && S.results[t.id].ok; });
    return sec(_t('はじめに（作業の順番）'),
      step(1, !!S.data, _t('CSV を入れる'), _t('回答のファイルを画面にドロップします。'), 'openCsv'),
      step(2, S.P.tabs.length > 0, S.P.tabs.length ? _t('解析を足す') : _t('解析を始める'), _t('種類を選ぶと、画面の下へ順に並びます。'), 'newAnalysis'),
      step(3, anyOk, _t('設問を選ぶ'), _t('解析の欄で設問を選ぶと、すぐに p 値・表・図・文が出ます。'), 'goTab', 'analysis'),
      step(4, false, _t('図と表を整える'), _t('図や表をクリックして、文字・線・色を変えます。'), 'goTab', 'figure'),
      step(5, false, _t('まとめて書き出す'), _t('☑ を付けた解析の図・Excel・報告書を保存します。'), 'exportAll')) +
      sec(_t('今の状態'), hint(!S.data ? _t('データがまだありません。') : _t('データ：{0}（回答 {1} 件、{2} 列）。解析 {3} 件。', S.data.name, S.data.nrow, S.data.ncol, S.P.tabs.length)),
        hint(_t('見出しの ∨ で解析を畳めます。有意な解析は、畳んでいても名前が赤い太字になります。'))) +
      sec(_t('保存'), hint(_t('「上書き保存」で、データ（CSV の中身）・解析・図と表の設定を 1 つのプロジェクトとしてアプリの中に保存します。次からは「プロジェクトを開く」で、CSV なしで元どおりに開けます。') + (S.projName ? _t('今のプロジェクト：') + S.projName : '')),
        '<div class="md-row"><button class="md-btn" data-act="save"' + (S.data ? '' : ' disabled') + _t('>上書き保存</button><button class="md-btn" data-act="openProject">プロジェクトを開く</button></div>'));
  }
  // 無効回答とフラッグの一覧（行番号、回答の一部、解除などのボタン）
  function rowSnippet(r) {
    var row = S.data.rows && S.data.rows[r - 1]; if (!row) return '';
    var parts = [];
    S.P.vars.forEach(function (x, j) { if (x.comp || x.type === 'time' || parts.length >= 3) return; var c = row[j]; if (c !== null && c !== '' && c !== undefined) parts.push(String(c)); });
    var s = parts.join(' / '); return s.length > 60 ? s.slice(0, 60) + '…' : s;
  }
  function marksSecs() {
    var mk = S.P.rowMarks || {}, xs = A.markedRows('x'), fs = A.markedRows('f');
    if (!xs.length && !fs.length) return sec(_t('無効回答とフラッグ'), hint(_t('「データ」タブの「回答の一覧」で、行の「無効」にチェックを入れると、その回答をすべての解析から除きます。「フラッグ」は目印で、解析には含めます。どちらもここに一覧が出ます。')));
    function item(r, kind) {
      return '<div class="md-mkrow"><div class="md-mkhead"><b>' + _t('{0} 行目', r) + '</b><span class="md-mksnip" data-tip="' + esc(rowSnippet(r)) + '">' + esc(rowSnippet(r)) + '</span></div><div class="md-row">' +
        _t('<button class="md-btn md-btn-sm" data-act="rowShow" data-arg="{0}" data-tip="「回答の一覧」でこの行を表示します。">表示</button>', r) +
        (kind === 'f' && !(mk[r] && mk[r].x) ? _t('<button class="md-btn md-btn-sm" data-act="rowMark" data-arg="{0}:x" data-tip="この回答を無効にして、解析から除きます。">無効にする</button>', r) : '') +
        _t('<button class="md-btn md-btn-sm" data-act="rowUnmark" data-arg="{0}:{1}" data-tip="印を外します。">解除</button>', r, kind) + '</div>' +
        (kind === 'x' ? _t('<input type="text" class="md-in" data-rownote="{0}" value="{1}" placeholder="理由（例：金額が 1 億円）" data-tip="無効にした理由のメモです。">', r, esc((mk[r] && mk[r].note) || '')) : '') + '</div>';
    }
    return (xs.length ? sec(_t('無効回答（{0} 件）', xs.length), xs.map(function (r) { return item(r, 'x'); }).join(''), hint(_t('この行は、すべての解析・報告書・Excel・R のコードから除いています。「回答の内訳」の人数も除いた後の数です。'))) : '') +
      (fs.length ? sec(_t('フラッグ（{0} 件）', fs.length), fs.map(function (r) { return item(r, 'f'); }).join(''), hint(_t('フラッグは目印です。解析には含めています。'))) : '');
  }
  function paneData() {
    if (!S.data) return needTab(_t('データ'));
    var x = S.ui.selVar !== null ? A.v(S.ui.selVar) : null;
    if (!x) return marksSecs() + sec(_t('列の設定'), hint(_t('左の一覧で列をクリックすると、その列の種類と回答の内訳をここで確認できます。'))) +
      sec(_t('種類の見分け方'), hint(_t('単一回答：1 つだけ選ぶ質問。順序あり：「あてはまる〜あてはまらない」のように段階がある質問。複数回答：あてはまるものをすべて選ぶ質問。自動の判定が合わないときは、種類を選び直してください。')));
    var mx = Math.max.apply(null, (x.cats || []).map(function (c) { return c.n; }).concat([1]));
    var h = (S.ui.dataView === 'raw' ? marksSecs() : '') + sec(_t('列 {0} の設定', x.col), _t('<label class="md-field"><span>表示名</span><input type="text" data-varlabel="') + x.col + '" value="' + esc(x.label) + '"></label>',
      grid2(_t('<label class="md-field"><span>種類</span><select data-var="type">') + options([['nominal', _t('単一回答')], ['ordinal', _t('順序あり')], ['multi', _t('複数回答')], ['numeric', _t('数値')], ['text', _t('使わない（自由記述など）')]], x.type === 'time' || x.type === 'empty' ? 'text' : x.type) + '</select></label>',
        x.type === 'multi' ? _t('<label class="md-field"><span>回答の区切り</span><select data-var="delim">') + options([[';', _t('セミコロン（;）')], [', ', _t('カンマと空白（, ）')], [_t('、'), _t('読点（、）')]], x.delim) + '</select></label>' : '<span></span>'),
      hint(_t('表示名は図・表・報告書に出ます。短く書き直すと読みやすくなります。'))) +
      sec(_t('質問文'), '<div class="md-hint" style="color:var(--t-1e2f26);white-space:pre-wrap">' + esc(x.header) + '</div>');
    if (A.isCat(x)) h += sec(_t('回答の内訳（{0} 人が回答）', x.n), x.type === 'multi' ? x.cats.map(function (c) {
      return '<div class="md-bar" data-tip="' + esc(c.v) + '"><span class="md-bl">' + esc(c.v) + '</span><span class="md-bb"><i style="width:' + (c.n / mx * 100).toFixed(1) + '%"></i></span><span class="md-bn">' + c.n + '</span></div>';
    }).join('') : orderList('var:' + x.col, x.cats.map(function (c) { return { id: c.v, label: c.v, extra: '<span class="md-bb"><i style="width:' + (c.n / mx * 100).toFixed(1) + '%"></i></span><span class="md-bn">' + c.n + '</span>' }; })) + (x.type !== 'multi' ? _t('<div class="md-row" style="margin-top:6px"><button class="md-btn md-btn-sm" data-act="varOrder" data-arg="rev" data-tip="選択肢の並びを逆にします。">逆の順</button><button class="md-btn md-btn-sm" data-act="varOrder" data-arg="count" data-tip="人数の多い選択肢から並べます。">人数の多い順</button><button class="md-btn md-btn-sm" data-act="varOrder" data-arg="auto" data-tip="データを読み込んだときの並びに戻します。">自動の並び</button></div>') : ''), hint(x.type === 'ordinal' ? _t('段階の順に並べてください。選択肢をクリックして選び、上のボタン（または Alt＋↑↓）で動かします。並びは、この列を使う解析の表と図にもすぐ反映されます。点の付け方は下の「配点」で選べます。') : x.type === 'multi' ? _t('複数回答は、選択肢ごとに選んだ人数を数えます。') : _t('並びは、表や図の順番になります。選択肢をクリックして選び、上のボタン（または Alt＋↑↓）で動かすと、この列を使う解析の表と図にもすぐ反映されます。')),
      _t('<button class="md-btn" data-act="freqOfVar">この列を集計する</button>'));
    else if (x.type === 'numeric' && x.summary) h += sec(_t('数値の要約'), hint(_t('最小 {0}、中央値 {1}、平均 {2}、最大 {3}（{4} 人が回答）', x.summary.min, x.summary.median, (+x.summary.mean).toFixed(2), x.summary.max, x.n)));
    else h += sec(_t('回答'), hint(x.n + _t(' 人が回答しています。この種類の列は解析に使いません。')));
    if (x.type === 'ordinal' && !x.comp) {
      var mode = A.scoreMode(x), cp = A.catPoints(x);
      h += sec(_t('配点'), _t('<label class="md-field"><span>点の付け方</span><select data-varscore="mode">') + options([['desc', _t('上の選択肢ほど高い点')], ['asc', _t('上の選択肢ほど低い点')], ['custom', _t('点を自分で入れる')]], mode) + '</select></label>' +
        x.cats.map(function (c, i) {
          if (c.special) return '';
          return '<div class="md-bar md-ptrow"><span class="md-bl" data-tip="' + esc(c.v) + '">' + esc(c.v) + '</span>' +
            (mode === 'custom' ? '<input type="text" inputmode="decimal" class="md-pt" data-varpt="' + i + '" value="' + esc(String(cp[c.v])) + _t('" data-tip="この選択肢の点を入れます。小数や負の数も使えます。">') : '<span class="md-pt">' + cp[c.v] + '</span>') + '</div>';
        }).join(''),
        hint(_t('平均の比較・相関・回帰・尺度得点など、得点を使う解析すべてに使います。順位を使う検定では、点の高い側を上位として扱います。')));
    }
    return h;
  }
  function varOptions(sd, cur) {
    var ok = [], ng = [];
    S.P.vars.forEach(function (x) {
      var allowed = sd.types.indexOf(x.type) >= 0;
      (allowed ? ok : ng).push('<option value="' + x.col + '"' + (cur === x.col ? ' selected' : '') + (allowed ? '' : ' disabled') + '>' + esc(x.label) + _t('　〔{0}〕</option>', esc(A.TYPE_NAMES[x.type] || x.type)));
    });
    return '<option value="">' + (sd.optional ? _t('（使わない）') : _t('（選んでください）')) + _t('</option><optgroup label="使える列">') + ok.join('') + '</optgroup>' + (ng.length ? _t('<optgroup label="この欄には使えない列">') + ng.join('') + '</optgroup>' : '');
  }
  function catsEditor(tab, sid, sl, x) {
    if (!sl.cats.length || (x && !A.isCat(x))) return '';
    var counts = {}; A.slotCols(sl).map(A.v).filter(Boolean).forEach(function (vv) { (vv.cats || []).forEach(function (c) { counts[c.v] = (counts[c.v] || 0) + c.n; }); });
    var h = '<div class="md-cats">', blocks = A.catBlocks(sl.cats), bof = function (i) { return blocks.findIndex(function (b) { return b.indexOf(i) >= 0; }); };
    sl.cats.forEach(function (c, i) {
      var merged = c.on && sl.cats.some(function (k, j) { return j !== i && k.on && k.label === c.label; });
      h += '<div class="md-cat' + (c.on ? '' : ' off') + '"><input type="checkbox" data-cat="' + sid + ':' + i + ':on"' + (c.on ? ' checked' : '') + _t(' data-tip="チェックを外すと、この選択肢を選んだ人を解析から除きます。">') +
        '<input type="text" data-cat="' + sid + ':' + i + ':label" value="' + esc(c.label) + _t('" data-tip="元の回答：') + esc(c.v) + _t('。名前を書き換えられます。ほかの選択肢と同じ名前にすると、1 つにまとめます。">') +
        (merged ? _t('<button class="md-merged" data-tip="同じ名前の選択肢とまとめています。押すと、この選択肢だけ元の名前に戻して分けます。">まとめ</button>') : '') + '<span class="md-n">' + (counts[c.v] || 0) + '</span>' +
        '<button class="md-mv" data-act="catMove" data-arg="' + sid + ':' + i + (merged ? _t(':-1" data-tip="まとめた群ごと 1 つ上へ"') : _t(':-1" data-tip="1 つ上へ"')) + (bof(i) === 0 ? ' disabled' : '') + '>▲</button><button class="md-mv" data-act="catMove" data-arg="' + sid + ':' + i + (merged ? _t(':1" data-tip="まとめた群ごと 1 つ下へ"') : _t(':1" data-tip="1 つ下へ"')) + (bof(i) === blocks.length - 1 ? ' disabled' : '') + '>▼</button>' +
        (c.label !== c.v ? _t('<div class="md-orig">元の回答：') + esc(c.v) + '</div>' : '') + '</div>';
    });
    var onN = sl.cats.filter(function (c) { return c.on; }).length;
    return h + '</div><div class="md-row" style="margin-bottom:8px">' + (onN >= 3 ? '<button class="md-btn md-btn-sm" data-act="catTwo" data-arg="' + sid + _t('" data-tip="チェックの入った選択肢を、上半分と下半分の 2 つにまとめます。">上下 2 つにまとめる</button>') : '') +
      '<button class="md-btn md-btn-sm" data-act="catReset" data-arg="' + sid + _t('" data-tip="名前・並び・チェックを最初の状態に戻します。">最初の状態に戻す</button></div>');
  }
  // 回帰分析の説明変数：選択肢のある列ごとに、使う選択肢のチェックを出す（外した選択肢を選んだ人は推定から除く）
  function xDropEditor(tab, sid, sl) {
    var dr = sl.drop || {};
    return sl.cols.map(A.v).filter(function (x) { return x && A.isCat(x) && x.type !== 'multi'; }).map(function (x) {
      var off = dr[x.col] || [];
      return '<div class="md-hint" style="margin:6px 0 2px;color:var(--t-1e2f26)">' + esc(x.label) + '</div><div class="md-cats md-xcats">' + x.cats.map(function (c, i) {
        if (c.special) return '';
        var on = off.indexOf(c.v) < 0;
        return '<label class="md-cat' + (on ? '' : ' off') + '" style="cursor:pointer"><input type="checkbox" data-xdrop="' + sid + ':' + x.col + ':' + i + '"' + (on ? ' checked' : '') + _t(' data-tip="チェックを外すと、この選択肢を選んだ人を解析から除きます。">') + '<span style="flex:1;padding:4px">' + esc(c.v) + '</span><span class="md-n">' + c.n + '</span></label>';
      }).join('') + '</div>';
    }).join('');
  }
  function paneAnalysis() {
    var tab = A.curTab(); if (!tab) return needTab(_t('解析'));
    var d = A.def(tab.type), multiNote = tab.type === 'multi' || (tab.type === 'freq' && tab.slots.a.col !== null && A.v(tab.slots.a.col) && A.v(tab.slots.a.col).type === 'multi');
    var h = target(tab) + sec(_t('解析の種類'), '<div style="font-weight:700;margin-bottom:2px">' + esc(d.name) + '</div>', hint(d.desc), _t('<button class="md-btn" data-act="changeType">種類を変える</button>'));
    var slotsH = d.slots.map(function (sd) {
      var sl = tab.slots[sd.id];
      // 回帰分析の説明変数は、設問ごとの回答をそのまま使う（まとめた選択肢の一覧は意味を持たないので出さない）
      if (sd.many && sd.types.indexOf('numeric') >= 0) return '<div class="md-sub"><b>' + esc(sd.label) + '</b>' + (sl.cols.length ? _t('：{0} 個の設問', sl.cols.length) : '') + '</div>' + xDropEditor(tab, sd.id, sl) + hint(_t('説明変数は、それぞれの設問の回答をそのまま使います（「無回答」などは除きます）。単一回答の設問は、選択肢の並びの先頭を基準にします。チェックを外した選択肢を選んだ人は、この解析から除きます。'));
      if (sd.many) return '<div class="md-sub"><b>' + esc(sd.label) + '</b>' + (sl.cols.length ? _t('：{0} 個の設問（人数は合計）', sl.cols.length) : '') + '</div>' + (sl.cols.length ? catsEditor(tab, sd.id, sl, null) : hint(_t('設問はまだ選ばれていません。中央の解析の欄で選んでください。')));
      var x = sl.col !== null ? A.v(sl.col) : null;
      return '<div class="md-sub"><b>' + esc(sd.label) + '</b>' + (x ? _t('：') + esc(x.label) : '') + '</div>' + (x ? catsEditor(tab, sd.id, sl, x) : hint(_t('設問はまだ選ばれていません。中央の解析の欄で選んでください。')));
    }).join('');
    // 選択肢を直せる欄があるときだけ、直し方の説明を付ける（数値や回帰分析の説明変数だけのときは付けない）
    h += sec(_t('選択肢の設定'), slotsH, slotsH.indexOf('<div class="md-cats">') < 0 ? '' : hint(_t('選択肢の名前は書き換えられます。同じ名前にした選択肢は 1 つにまとめ、チェックを外した選択肢は解析から除きます。') + (multiNote ? _t('複数回答で選んだ人が 1% 未満の回答（自由記述）は、最初から「その他」にまとめています。') : '')));
    var fv = tab.filter && tab.filter.col !== null ? A.v(tab.filter.col) : null;
    h += sec(_t('絞り込み（一部の回答者だけで解析）'), _t('<label class="md-field"><span>条件にする質問</span><select data-filter="col"><option value="">（絞り込まない）</option>') +
      S.P.vars.filter(A.isCat).map(function (x) { return '<option value="' + x.col + '"' + (fv && fv.col === x.col ? ' selected' : '') + '>' + esc(x.label) + '</option>'; }).join('') + '</select></label>' +
      (fv ? '<div class="md-cats">' + fv.cats.map(function (c, i) { return '<label class="md-cat" style="cursor:pointer"><input type="checkbox" data-filter="val" data-idx="' + i + '"' + (tab.filter.values.indexOf(c.v) >= 0 ? ' checked' : '') + '><span style="flex:1;padding:4px">' + esc(c.v) + '</span><span class="md-n">' + c.n + '</span></label>'; }).join('') + '</div>' +
        hint(tab.filter.values.length ? _t('チェックした回答をした人だけで解析します。') : _t('回答にチェックを入れると絞り込みます（今は全員で解析しています）。')) : hint(_t('例：Instagram を利用している人だけで解析する。'))));
    if ((d.opts || []).length) h += sec(_t('検定の設定'), d.opts.map(function (o) {
      return (o.kind === 'check' ? fCheck(o.label, 'opt.' + o.id) : o.kind === 'text' ? fText(o.label, 'opt.' + o.id) : fSelect(o.label, 'opt.' + o.id, o.choices.map(function (c) { return [c.value, c.label]; }))) + (o.hint ? hint(o.hint) : '');
    }).join(''));
    return h;
  }
  function legOpts() {
    var bg = get('fig.legBg'), lab = bg === 'none' ? _t('塗りなし（透明）') : bg ? bg : _t('自動');
    return '<div class="md-row">' + fCheck(_t('枠で囲む'), 'fig.legFrame') + fCheck(_t('質問名を見出しに付ける'), 'fig.legTitle') + '</div>' +
      grid2(_t('<label class="md-field"><span>背景の色</span><button class="md-colbtn" data-act="pickLegBg" data-tip="凡例の背景の色を選びます。「塗りなし」で透明になります。"><i style="background:') + (bg && bg !== 'none' ? bg : 'transparent') + '"></i>' + esc(lab) + '</button></label>', '') +
      hint(_t('背景の「自動」は、図の中に置いたときだけ白を薄く敷きます。系列の名前は下の「色」の節で変えられます。図の凡例の名前をダブルクリックしても変えられます。'));
  }
  function paneFigure() {
    var tab = A.curTab(); if (!tab) return needTab(_t('図'));
    var res = S.results[tab.id], m = res && res.ok ? Chart.model(res) : null, st = A.curFig();
    if (!m) return target(tab) + sec(_t('図'), hint(_t('結果が出ると、ここで図を整えられます。先に「解析」で列を選んでください。')), _t('<button class="md-btn" data-act="goTab" data-arg="analysis">解析の設定へ</button>'));
    if (!st) return target(tab) + sec(_t('図'), hint(_t('この解析には図がありません。')), _t('<button class="md-btn md-btn-primary" data-act="addFig">図を追加</button>'));
    var kind = Chart.kindOf(m, st), km = Chart.kinds(m), ser = Chart.seriesFor(m, st), cols = Chart.seriesColors(ser, st), heat = kind === 'heat', pie = kind === 'pie', k = Math.min(S.ui.selFig, tab.figs.length - 1);
    var h = target(tab) + (tab.figs.length > 1 ? '<div class="md-row" style="margin:4px 0">' + tab.figs.map(function (_, i) { return '<button class="md-btn md-btn-sm' + (i === k ? ' md-btn-primary' : '') + '" data-act="selFig" data-arg="' + i + _t('">図 ') + (i + 1) + '</button>'; }).join('') + '</div>' : '');
    h += sec(_t('図の種類'), tiles('fig.kind', km.map(function (x) { return [x, _t(Chart.KIND_NAMES[x]), window.icon('k_' + x, 30), KIND_TIPS[x]]; }), kind), hint(_t('図は、設定を変えるたびにその場で描き直します。')));
    // 選択肢の並び：この解析の表と図の並びを、図を見ながら変える（まとめた選択肢は 1 つとして動かす）
    var ordSlots = Object.keys(tab.slots || {}).filter(function (id) { var sl = tab.slots[id]; return sl && sl.cats && A.figOrderLabels(sl).length > 1; });
    if (ordSlots.length) h += sec(_t('選択肢の並び'), ordSlots.map(function (id) {
      var sl = tab.slots[id], labs = A.figOrderLabels(sl), vr = sl.col ? A.v(sl.col) : null;
      return '<div class="md-sub">' + esc(vr ? vr.label : _t('選択肢')) + '</div>' + orderList('fig:' + id, labs.map(function (l) { return { id: l, label: l }; })) + _t('<div class="md-row" style="margin:4px 0 6px"><button class="md-btn md-btn-sm" data-act="figCatOrder" data-arg="{0}:rev" data-tip="選択肢の並びを逆にします。">逆の順</button><button class="md-btn md-btn-sm" data-act="figCatOrder" data-arg="{0}:count" data-tip="人数の多い選択肢から並べます。">人数の多い順</button><button class="md-btn md-btn-sm" data-act="figCatOrder" data-arg="{0}:data" data-tip="「データ」タブで決めた並びに戻します。">データの並び</button></div>', id);
    }).join(''), hint(_t('選択肢をクリックして選び、上のボタン（または Alt＋↑↓）で動かします。変わるのは、この解析の表と図の並びです。すべての解析で変えるときは、「データ」タブでその列を選んで変えてください。')));
    // グラフの枠：描く範囲を四方の線で囲むか、左と下の軸だけにするか（見つけやすいように、図の種類のすぐ下に置く）
    if (!pie && !heat && kind !== 'cloud' && kind !== 'net') h += sec(_t('グラフの枠'), tiles('fig.frame', fromMini('frame', [['L', _t('左と下だけ')], ['box', _t('四方を囲む')]])), hint(_t('「四方を囲む」にすると、グラフを描く範囲を枠で囲みます。枠の太さは「線の太さ」の「軸と枠」で変えられます。')));
    if (!pie) h += sec(_t('軸の名前'), grid2(fText(heat ? _t('上の見出し') : _t('数値の軸の名前'), 'fig.vTitle', { type: 'strnull', ph: _t('自動') }), fText(_t('分類の軸の名前'), 'fig.cTitle', { type: 'strnull', ph: _t('自動') })),
      hint(_t('空欄は自動の名前になります。消したいときは、図の上の文字をダブルクリックして空にしてください。図の中に題名は入れません。')));
    if (heat) h += sec(_t('カラーバー（色と値の対応）'), fSelect(_t('色を決める値'), 'fig.heatBy', HEAT_BY.slice(1).filter(function (x) { return x[0] !== 'resid' || m.resid; })), _t('<div class="md-sub">位置</div>{0}<div class="md-sub">形</div>', tiles('fig.bar', fromMini('bar', [['right', _t('右')], ['bottom', _t('下')], ['none', _t('なし')]]))) + tiles('fig.barShape', fromMini('barShape', [['smooth', _t('なめらか')], ['step', _t('5 段階')]])));
    else if (pie) { if (ser.length > 1) h += sec(_t('凡例'), hint(_t('図の上の凡例をドラッグしても動かせます。')), _t('<div class="md-sub">位置</div>') + tiles('fig.legend', fromMini('leg', LEG_POS.filter(function (x) { return x[0] !== 'inTR' && x[0] !== 'inTL'; }))), legOpts()); }
    else {
      var scat = kind === 'scatter', outN = Chart.render(m, st, S.P.settings.figLang).outside;
      h += sec(_t('目盛り'), (scat ? '<div class="md-sub">' + _t('縦軸') + '</div>' : '') + grid3(fText(_t('最小'), 'fig.min', { type: 'numnull', ph: _t('自動') }), fText(_t('最大'), 'fig.max', { type: 'numnull', ph: _t('自動') }), fText(_t('間隔'), 'fig.step', { type: 'numnull', ph: _t('自動') })) +
        (scat ? '<div class="md-sub">' + _t('横軸') + '</div>' + grid3(fText(_t('最小'), 'fig.xMin', { type: 'numnull', ph: _t('自動') }), fText(_t('最大'), 'fig.xMax', { type: 'numnull', ph: _t('自動') }), fText(_t('間隔'), 'fig.xStep', { type: 'numnull', ph: _t('自動') })) : '') +
        (outN ? hint(_t('軸の範囲の外にある {0} 個の点は描いていません。計算には含めています。', outN)) : ''),
        _t('<div class="md-sub">目盛りの向き</div>{0}<div class="md-sub">目盛線</div>', tiles('fig.tickDir', fromMini('tick', [['out', _t('外向き')], ['in', _t('内向き')], ['none', _t('なし')]]))) + tiles('fig.grid', fromMini('grid', [['solid', _t('実線')], ['dash', _t('破線')], ['dot', _t('点線')], ['none', _t('なし')]])) +
        grid2(fText(_t('目盛りの長さ'), 'fig.tickLen', { type: 'num' }), fColor(_t('目盛線の色'), 'fig.gridColor')),
        hint(_t('範囲の空欄は自動です。数値の文字が重なるときは、自動で間引きます。')));
      if (ser.length > 1) h += sec(_t('凡例'), hint(_t('図の上の凡例をドラッグしても動かせます。右・上・下の端に寄せると図の外に並べ、それ以外は置いた位置に置きます。')), _t('<div class="md-sub">位置</div>{0}<div class="md-sub">色見本の形</div>', tiles('fig.legend', fromMini('leg', LEG_POS))) + tiles('fig.legShape', fromMini('shape', [['square', _t('四角')], ['circle', _t('丸')], ['wide', _t('横長')]])),
        legOpts());
    }
    h += sec(_t('文字'), grid2(fSelect(_t('和文の書体'), 'fig.fontJa', FONTS_JA), fSelect(_t('欧文の書体'), 'fig.fontEn', FONTS_EN)),
      '<div class="md-grid2" style="grid-template-columns:1fr 1fr 1fr 1fr">' + fText(_t('目盛り'), 'fig.fsTick', { type: 'num', list: 'dl_fs' }) + fText(_t('軸の名前'), 'fig.fsAxis', { type: 'num', list: 'dl_fs' }) + fText(_t('凡例'), 'fig.fsLegend', { type: 'num', list: 'dl_fs' }) + fText(_t('値'), 'fig.fsLabel', { type: 'num', list: 'dl_fs' }) + '</div>',
      hint(_t('大きさの単位はピクセルです。英数字は欧文の書体、日本語は和文の書体で描きます。入りきらない文字は「…」で省き、重ならないようにします。')));
    h += sec(_t('線の太さ'), grid3(fText(_t('軸と枠'), 'fig.lwAxis', { type: 'num', list: 'dl_lw' }), fText(_t('棒の枠'), 'fig.lwBar', { type: 'num', list: 'dl_lw' }), fText(_t('目盛線'), 'fig.lwGrid', { type: 'num', list: 'dl_lw' })));
    h += sec(_t('色'), colorSec(m, st, ser, cols, heat, pie));
    h += sec(_t('大きさ'), grid3(fText(_t('幅'), 'fig.w', { type: 'num' }), fText(_t('高さ'), 'fig.h', { type: 'num' }), fText(_t('棒の太さ (%)'), 'fig.barW', { type: 'num' })), fCheck(_t('文字や棒が重ならない大きさまで自動で広げる'), 'fig.fit'),
      hint(_t('図の右端・下端・右下の角をドラッグすると、その大きさに変わります。保存される画像は、画面に出ている形と大きさのままです。')));
    if (!heat && kind !== 'box' && kind !== 'scatter' && kind !== 'line') h += sec(_t('値のラベル'), '<div class="md-row">' + fCheck(_t('値のラベルを出す'), 'fig.labels') + '</div>',
      grid2(fSelect(_t('書く内容'), 'fig.labelWhat', [['value', _t('軸と同じ値')], ['pct', _t('割合 (%)')], ['n', _t('人数（件数）')], ['both', _t('割合と人数の両方')]]), fText(_t('割合の小数の桁'), 'fig.labelDec', { type: 'num' })),
      hint(pie ? _t('「両方」にすると、割合と人数を 2 行で書きます。扇が小さくて入りきらないところは書きません。') : _t('「両方」にすると「45.0% (9)」のように書きます。人数の軸のときは「9 (45.0%)」です。入りきらないところは自動で省きます。')));
    var omit = st.labels ? Chart.render(m, st, S.P.settings.figLang).omitted : 0;      // 場所が足りずに省いた値のラベル
    h += sec(_t('表示'), '<div class="md-row">' + (heat ? fCheck(_t('値のラベル'), 'fig.labels') : '') + fCheck(_t('人数 (n)'), 'fig.showN') + fCheck(_t('背景を透明にする'), 'fig.transparent') + '</div>',
      omit ? hint(_t('場所が足りないため、{0} か所の値のラベルを省いています。図を大きくするか、数値の文字を小さくしてください。', omit)) : '');
    h += sec(_t('書式の使い回し'), _t('<div class="md-row"><button class="md-btn" data-act="figDefault" data-tip="文字・線・色・大きさの設定を、これから作る図の最初の状態にします。">これからの図の既定にする</button><button class="md-btn" data-act="figApplyAll" data-tip="文字・線・色・凡例・目盛りの設定を、すべての図にそろえます。">すべての図にそろえる</button><button class="md-btn" data-act="figReset">最初の状態に戻す</button></div>'));
    return h;
  }
  // 色の節：「配色から選ぶ」か「系列ごとに選ぶ」かを選ぶ。初めは配色から選ぶ
  function colorSec(m, st, ser, cols, heat, pie) {
    var keys = Chart.colorKeys(m, st), mode = Chart.colorMode(st), own = keys.length > 0 && mode === 'series', single = !ser.length;
    var kc = Chart.seriesColors(keys, st), h = '';
    if (keys.length) h += _t('<div class="md-sub">色の決め方</div>') + tiles('fig.colorMode', [['palette', _t('配色から選ぶ'), MINI.cmode.palette, _t('配色の一覧から 1 つ選ぶと、系列の順にその色を使います。')], ['series', _t('系列ごとに選ぶ'), MINI.cmode.series, _t('系列ごとに好きな色を選びます。まだ選んでいない系列は、配色の色のままです。')]], mode);
    if (!own) h += paletteGallery('fig.palette', st.palette, heat ? ['濃淡', '2 方向'] : null, st.seed);
    if (keys.length && (own || ser.length > 1)) {
      h += '<div class="md-sub">' + (own ? (single ? _t('図の色') : pie ? _t('回答ごとの色と名前') : _t('系列ごとの色と名前')) : (pie ? _t('回答ごとの名前') : _t('系列ごとの名前'))) + '</div>' + keys.map(function (s, i) {
        var nm = (st.legNames || {})[s] || '', set = own && st.colors && st.colors[s];
        return '<div class="md-bar"><button class="md-swatch md-swatch-pick' + (set ? ' set' : '') + '" style="background:' + patCss(kc[i]) + '" data-act="pickColor" data-arg="' + i + '" data-tip="' + (own ? _t('この系列の色を選びます。') : _t('押すと「系列ごとに選ぶ」に切り替えて、この系列の色を選びます。')) + '"></button>' +
          (single ? '<span class="md-hint" style="margin:0">' + (set ? _t('選んだ色') : _t('配色の色')) + '</span>' : '<input type="text" class="md-legname" data-legname="' + i + '" value="' + esc(nm || s) + _t('" data-tip="凡例に出す名前です。空にすると元の名前に戻ります。元の名前：') + esc(s) + '">') + '</div>';
      }).join('');
    }
    if (own && Object.keys(st.colors || {}).length) h += _t('<button class="md-btn md-btn-sm" data-act="clearColors" style="margin-top:4px" data-tip="選んだ色を消して、すべての系列を配色の色に戻します。">すべて配色の色に戻す</button>');
    if (Object.keys(st.legNames || {}).length) h += _t('<button class="md-btn md-btn-sm" data-act="clearLegNames" style="margin-top:4px;margin-left:4px">名前を元に戻す</button>');
    return h + hint(own ? _t('色見本を押して色を選びます。図の凡例の色を押しても選べます。') : _t('順序のある回答は濃淡か 2 方向、種類の違う回答は多色が向いています。白黒で印刷するときは「白黒の模様」が使えます。'));
  }
  function paneTable() {
    var tab = A.curTab(); if (!tab) return needTab(_t('表'));
    var res = S.results[tab.id], ts = tab.tstyle;
    if (!res || !res.ok) return target(tab) + sec(_t('表'), hint(_t('結果が出ると、ここで表を整えられます。')));
    var xt = !!res.xtab, h = target(tab) + sec(_t('デザイン'), '<div class="md-tiles">' + T_DESIGNS.map(function (d) { return '<span class="md-tile-cap"><button class="md-tile' + (ts.design === d[0] ? ' on' : '') + '" data-act="tDesign" data-arg="' + d[0] + '" data-tip="' + esc(d[3]) + '">' + window.icon(d[2], 30) + '</button>' + d[1] + '</span>'; }).join('') + '</div>', hint(_t('デザインを選ぶと、線と色がそのデザインの標準に変わります。そのあとで下の設定を変えられます。')));
    if (xt) h += sec(_t('表に出す内容'), '<div class="md-row">' + ['row', 'col', 'none'].map(function (k) { return '<label class="md-check"><input type="radio" name="tblpct" data-set="tbl.pct" value="' + k + '"' + (tab.table.pct === k ? ' checked' : '') + '>' + { row: _t('行の割合'), col: _t('列の割合'), none: _t('人数のみ') }[k] + '</label>'; }).join('') + '</div>' +
      (res.xtab.resid ? fCheck(_t('多い・少ないの印（▲▽）を付ける'), 'tbl.marks') + '<div class="md-row">' + fCheck(_t('期待度数の表'), 'tbl.expected') + fCheck(_t('残差の表'), 'tbl.resid') + '</div>' : ''));
    if (xt) h += sec(_t('セルの色分け'), fSelect(_t('色を決める値'), 'tst.heat', HEAT_BY.filter(function (x) { return x[0] !== 'resid' || res.xtab.resid; })),
      ts.heat !== 'none' ? _t('<div class="md-sub">配色</div>{0}<div class="md-sub">カラーバーの位置</div>{1}<div class="md-sub">カラーバーの形</div>', paletteGallery('tst.heatPal', ts.heatPal, ['濃淡', '2 方向']), tiles('tst.bar', fromMini('bar', [['right', _t('右')], ['bottom', _t('下')], ['none', _t('なし')]]))) + tiles('tst.barShape', fromMini('barShape', [['smooth', _t('なめらか')], ['step', _t('5 段階')]])) : '',
      hint(_t('値の大きさに応じてセルに色を付け、カラーバーで色と値の対応を示します。残差は 0 を中央にした 2 方向の色になります。')));
    h += sec(_t('文字'), grid2(fSelect(_t('和文の書体'), 'tst.fontJa', FONTS_JA), fSelect(_t('欧文の書体'), 'tst.fontEn', FONTS_EN)), grid2(fText(_t('文字の大きさ'), 'tst.fs', { type: 'num', list: 'dl_fs' }), fColor(_t('文字の色'), 'tst.ink')));
    h += sec(_t('大きさ'), grid2(fText(_t('表の幅（px）'), 'tst.tw', { type: 'num' }), fText(_t('行の高さ（px）'), 'tst.rowH', { type: 'num' })),
      hint(_t('表の右端・下端・右下の角をドラッグしても変えられます。中身より小さくはなりません。0 にすると中身に合わせた大きさに戻ります。')) + _t('<button class="md-btn md-btn-sm" data-act="tblSizeAuto">中身に合わせた大きさに戻す</button>'));
    h += sec(_t('線'), _t('<div class="md-sub">線の種類</div>') + tiles('tst.ls', fromMini('ls', [['solid', _t('実線')], ['dashed', _t('破線')], ['dotted', _t('点線')]])), grid2(fText(_t('線の太さ'), 'tst.lw', { type: 'num', list: 'dl_lw' }), fColor(_t('線の色'), 'tst.lc')));
    h += sec(_t('見出しと縞の色'), grid3(fColor(_t('見出しの地'), 'tst.headFill', _t('見出しの行（格子では見出しの列も）の地の色です。')), fColor(_t('見出しの文字'), 'tst.headInk'), fColor(_t('縞の色'), 'tst.stripe', _t('「縞」のデザインで 1 行おきに付ける色です。'))));
    h += sec(_t('書式の使い回し'), _t('<div class="md-row"><button class="md-btn" data-act="tstDefault" data-tip="この表のデザインを、これから作る解析の表の最初の状態にします。">これからの表の既定にする</button><button class="md-btn" data-act="tstApplyAll" data-tip="この表のデザインを、すべての解析の表にそろえます。">すべての表にそろえる</button><button class="md-btn" data-act="tstReset">最初の状態に戻す</button></div>'));
    return h;
  }
  function paneExport() {
    if (!S.data) return needTab(_t('書き出し'));
    var tab = A.curTab(), res = tab ? S.results[tab.id] : null;
    var h = sec(_t('書き出す解析（☑）'), S.P.tabs.length ? '<div class="md-cats">' + S.P.tabs.map(function (t, i) { var r = S.results[t.id], sig = r && r.ok && r.sig; return '<label class="md-cat" style="cursor:pointer"><input type="checkbox" data-ancheck="' + t.id + '"' + (t.checked ? ' checked' : '') + '><span style="flex:1;padding:4px" class="' + (sig ? 'md-sigtext' : '') + '">' + (i + 1) + '. ' + esc(t.name) + _t('</span><span class="md-n">図 ') + t.figs.length + '</span></label>'; }).join('') + '</div>' : hint(_t('解析がまだありません。')),
      _t('<div class="md-row"><button class="md-btn md-btn-sm" data-act="checkAll" data-arg="1">すべて選ぶ</button><button class="md-btn md-btn-sm" data-act="checkAll" data-arg="0">すべて外す</button></div>'));
    h += sec(_t('まとめて書き出す'), hint(_t('☑ を付けた解析について、次のものから選んで、選んだフォルダーに書き出します。')),
      _t('<div class="md-hint" style="color:var(--t-1e2f26)">・図：「解析の名前_図1.png」のように、解析ごとに 1 から番号を付けます。画面に出ている形と大きさのままです。<br>・Excel ファイル：CSV をシートにし、その右側に Excel の関数で解析を再現します（値の貼り付けはしません）。<br>・R のコード：解析を R の標準の関数で計算し直すスクリプトと、そのデータです。<br>・報告書：すべての解析の文と表を、PDF・Word・テキストのいずれかにまとめます。</div>'),
      '<button class="md-btn md-btn-primary" data-act="exportAll"' + (S.P.tabs.length ? '' : ' disabled') + _t('>まとめて書き出す</button>'));
    h += sec(_t('報告書に入れる内容'), fCheck(_t('各解析の表を入れる'), 'rep.tables'), fCheck(_t('ホルム法で補正した p を一覧に入れる'), 'rep.holm'), hint(_t('補正後の p は、検定を何度も行うと偶然に有意となる結果が増える問題に対応するための値です。')));
    if (tab && res && res.ok && res.p !== null && res.p !== undefined && S.P.report.holm) {
      var gl = []; S.P.tabs.forEach(function (t) { var g = holmGroup(t); if (g && gl.indexOf(g) < 0) gl.push(g); });
      h += sec(_t('ホルム法で補正する範囲（族）'), target(tab), _t('<label class="md-field"><span>補正の族の名前</span><input type="text" list="md_holmgroups" placeholder="報告書全体" data-holmgroup="') + tab.id + '" value="' + esc(holmGroup(tab)) + '"></label><datalist id="md_holmgroups">' + gl.map(function (g) { return '<option value="' + esc(g) + '">'; }).join('') + '</datalist>',
        hint(_t('同じ名前を付けた検定どうしで補正します。空欄の検定は「報告書全体」の族にまとめます。例：仮説の検定に「仮説」、確認のための検定に「確認」と付けると、確認の検定を足しても仮説の補正後の p は変わりません。')));
    }
    if (tab && res && res.ok) h += sec(_t('選んだ解析の文'), target(tab), '<textarea class="md-in" data-auto data-reptext="' + tab.id + '" rows="5">' + esc(tab.reportText !== null ? tab.reportText : res.report) + '</textarea>',
      hint(_t('文は書き換えられます。定型文は結果の数値から決まった型で作っています。考察（なぜそうなったか）は含まれないため、自分で書き足してください。')), tab.reportText !== null ? '<button class="md-btn" data-act="resetTextAt" data-arg="' + tab.id + _t('">定型文に戻す</button>') : '');
    return h;
  }
  // 既定の設定：新しく始めるときに、初期設定とカスタム設定のどちらを使うか。カスタム設定の中身は、ここで直接選べる
  function defaultsSec() {
    var mode = A.defMode(), c = A.customGet(), base = A.baseProject();
    var cs = c ? c.settings || {} : base.settings, cr = c ? c.report || {} : base.report;
    var pal = c && c.figDefault && c.figDefault.palette || '', des = c && c.tstyleDefault && c.tstyleDefault.design || 'grid';
    var seg = function (v, label) { return '<button class="md-btn' + (mode === v ? ' md-btn-primary' : '') + '" data-act="defMode" data-arg="' + v + '"' + (v === 'custom' && !c ? ' disabled' : '') + '>' + label + '</button>'; };
    var sel = function (key, cur, list) { return '<select data-cust="' + key + '">' + list.map(function (o) { return '<option value="' + esc(o[0]) + '"' + (String(o[0]) === String(cur) ? ' selected' : '') + '>' + esc(o[1]) + '</option>'; }).join('') + '</select>'; };
    var groups = {}, order = [];
    Object.keys(Chart.PALETTES).forEach(function (k) { var p = Chart.PALETTES[k]; if (p.hide) return; if (!groups[p.group]) { groups[p.group] = []; order.push(p.group); } groups[p.group].push(k); });
    var palSel = '<select data-cust="palette"><option value=""' + (pal ? '' : ' selected') + _t('>初期のまま（グレー）</option>') + order.map(function (g) { return '<optgroup label="' + esc(_t(g)) + '">' + groups[g].map(function (k) { return '<option value="' + k + '"' + (k === pal ? ' selected' : '') + '>' + esc(_t(Chart.PALETTES[k].name)) + '</option>'; }).join('') + '</optgroup>'; }).join('') + '</select>';
    var row = function (label, ctl) { return '<tr><th class="md-rh" style="text-align:left;white-space:nowrap">' + label + '</th><td style="text-align:left">' + ctl + '</td></tr>'; };
    var d = c ? new Date(c.savedAt || 0) : null;
    var editor = '<table class="md-t0 md-custed" style="width:100%;margin:6px 0">' +
      row(_t('有意水準'), sel('alpha', cs.alpha, [[0.05, _t('5%（0.05）')], [0.01, _t('1%（0.01）')], [0.1, _t('10%（0.10）')]])) +
      row(_t('p 値の表記'), sel('lead0', cs.lead0 === false ? 'false' : 'true', [['true', 'p = 0.012'], ['false', 'p = .012']])) +
      row(_t('報告書の表'), sel('tables', cr.tables === false ? 'false' : 'true', [['true', _t('入れる')], ['false', _t('入れない')]])) +
      row(_t('補正後の p'), sel('holm', cr.holm === false ? 'false' : 'true', [['true', _t('一覧に入れる（ホルム法）')], ['false', _t('入れない')]])) +
      row(_t('図の配色'), palSel) +
      row(_t('表のデザイン'), sel('design', des, T_DESIGNS.map(function (t) { return [t[0], t[1]]; }))) + '</table>';
    return sec(_t('既定の設定（新しく始めるとき）'), '<div class="md-row" style="gap:6px">' + seg('init', _t('初期設定')) + seg('custom', _t('カスタム設定')) + '</div>' +
      _t('<div class="md-hint" style="margin:8px 0 0">カスタム設定の中身{0}</div>{1}<div class="md-row" style="gap:6px;flex-wrap:wrap;margin-top:6px"><button class="md-btn" data-act="customSave" data-tip="開いている画面の設定と、選んでいる図・表の書式（字体・大きさなども含む）を、まとめてカスタム設定にします。">今の設定と図・表の書式をまとめて取り込む</button><button class="md-btn" data-act="customApply"{2}>開いているプロジェクトに当てはめる</button></div><div class="md-row" style="gap:6px;flex-wrap:wrap;margin-top:6px"><button class="md-btn" data-act="customExport"{3}>書き出す</button><button class="md-btn" data-act="customImport">読み込む</button><button class="md-btn" data-act="customDelete"{4}>消す</button></div>', c && d && !isNaN(d) ? _t('（最後に変えた日時 {0}/{1}/{2} {3}:{4}）', d.getFullYear(), d.getMonth() + 1, d.getDate(), d.getHours(), String(d.getMinutes()).padStart(2, '0')) : _t('（まだありません。ここで選ぶと作られます）'), editor, S.data ? '' : ' disabled', c ? '' : ' disabled', c ? '' : ' disabled'),
      hint(_t('カスタム設定は、この端末に保存するので、新しい版になっても残ります。「初期設定」を選んでいる間も消えません。保存したプロジェクトを開いたときは、プロジェクトの設定を使います。別の端末で使うときは、書き出したファイルを読み込んでください。')));
  }
  // アプリの色（画面の枠の色。図と表の色は変えない）
  var THEMES; function mkThemes() { THEMES = [['green', _t('緑（初期）'), '#217346'], ['blue', _t('青'), '#2B579A'], ['red', _t('赤'), '#A4373A']]; } mkThemes(); window.CTS_ONLANG.push(mkThemes);
  function themeSec() {
    var cur = A.theme();
    return sec(_t('アプリの色'), '<div class="md-row" style="gap:6px;flex-wrap:wrap">' + THEMES.map(function (t) {
      return '<button class="md-btn' + (cur === t[0] ? ' md-theme-on' : '') + '" data-act="setTheme" data-arg="' + t[0] + '" style="display:inline-flex;align-items:center;gap:6px"><span style="width:14px;height:14px;background:' + t[2] + ';display:inline-block"></span>' + t[1] + '</button>';
    }).join('') + '</div>', hint(_t('画面の枠（タイトル・リボン・作業ウィンドウ・ボタン）の色です。図と表の色は変わりません。この端末に保存するので、新しい版になっても残ります。')));
  }
  // 言語：画面・結果の文・報告書・Excel・R のコードの言語。端末に保存する
  function langSec() {
    var cur = window.CTS_LANG;
    return sec(window.CTS_LANG === 'en' ? 'Language' : '言語 / Language', '<div class="md-row" style="gap:6px;flex-wrap:wrap">' + [['ja', '日本語'], ['en', 'English']].map(function (l) {
      return '<button class="md-btn' + (cur === l[0] ? ' md-theme-on' : '') + '" data-act="setLang" data-arg="' + l[0] + '">' + l[1] + '</button>';
    }).join('') + '</div>', hint(_t('画面の文字、解析の結果の文、報告書、Excel ファイル、R のコードの言語です。データの質問文や選択肢はそのまま表示します。この端末に保存するので、新しい版になっても残ります。')));
  }
  // 設定の画面（「ファイル」→「設定」）。今開いているプロジェクトの設定と、端末に保存する設定をまとめて出す
  function paneSettings() {
    return langSec() + themeSec() + defaultsSec() + _t('<div class="md-hint" style="margin:10px 0 0;font-weight:700;color:var(--ink-ui)">開いているプロジェクトの設定</div>') + sec(_t('判定と表記'), grid2(fSelect(_t('有意水準'), 'set.alpha', [[0.05, _t('5%（0.05）')], [0.01, _t('1%（0.01）')], [0.1, _t('10%（0.10）')]], 'num'), fSelect(_t('p 値の表記'), 'set.lead0', [['true', 'p = 0.012'], ['false', 'p = .012']], 'boolstr')),
      hint(_t('有意水準を変えると、すべての解析の判定と定型文が変わります。'))) +
      sec(_t('図'), grid2(fSelect(_t('自動で入る文字'), 'set.figLang', [['ja', _t('日本語')], ['en', _t('英語')]]), fSelect(_t('画像の細かさ'), 'set.pngScale', [[1, _t('標準')], [2, _t('2 倍（資料向け）')], [4, _t('4 倍（印刷向け）')]], 'num')),
        grid2(fSelect(_t('印刷の幅と解像度'), 'set.pngPrint', [['', _t('指定しない（上の倍率）')], ['84-300', _t('1 段幅 84 mm・300 dpi')], ['84-600', _t('1 段幅 84 mm・600 dpi')], ['174-300', _t('2 段幅 174 mm・300 dpi')], ['174-600', _t('2 段幅 174 mm・600 dpi')]]), '<span></span>'),
        hint(_t('印刷の幅と解像度を選ぶと、図をその幅に印刷したときにその解像度になるよう、画像の大きさを決めます（論文の 1 段は約 84 mm、2 段は約 174 mm）。文字を残したまま拡大できる形が必要なときは、図の右クリックの「SVG で保存」を使ってください。')),
        hint(_t('画像の細かさを上げても、Word や PowerPoint に貼ったときの大きさは画面と同じです（解像度の情報を画像に書き込みます）。回答の選択肢の名前はデータのまま表示します。'))) +
      sec(_t('画面'), grid2(fSelect(_t('画面の拡大'), 'set.zoom', [[90, '90%'], [100, '100%'], [115, '115%'], [130, '130%']], 'num'), '<span></span>')) +
      sec(_t('計算について'), hint(_t('集計と検定はすべて、このパソコンの中の R{0} で計算しています。データが外部に送られることはありません。', S.r ? _t('（バージョン {0}）', S.r) : '')), _t('<button class="md-btn" data-act="about">使っている手法を見る</button>')) +
      sec(_t('ご意見・不具合の報告'), hint(_t('不具合の報告や、ご意見・要望は GitHub で受け付けています。')), _t('<button class="md-btn" data-act="feedback">報告のしかたを見る</button>'));
  }
  function renderStatus() {
    var tab = A.curTab(), res = tab ? S.results[tab.id] : null, mode = !S.data ? '' : S.ui.view === 'data' ? _t('データ') : S.ui.view === 'report' ? _t('報告書') : tab ? _t('解析 {0}：', S.P.tabs.indexOf(tab) + 1) + A.def(tab.type).name : _t('解析');
    var h = '<span class="md-st-mode">' + esc(mode) + '</span>';
    if (S.ui.view === 'analysis' && res && res.ok && res.n) h += '<span class="md-st-num">n = ' + res.n.used + _t('（対象 {0}）</span>', res.n.total);
    if (S.ui.view === 'analysis' && res && res.ok && res.p !== null && res.p !== undefined) h += '<span class="md-st-num">' + pText(res.p) + '</span>';
    if (S.data) { var nxs = A.invalidRows().length; h += '<span>' + esc(S.data.name) + (nxs ? _t('　回答 {0} 件（無効 {1} 件を除く）</span>', S.data.nrow - nxs, nxs) : _t('　回答 {0} 件</span>', S.data.nrow)); }
    h += '<span class="md-st-r">' + (S.r ? _t('計算：R ') + esc(S.r) : _t('<b style="color:#C00000">R に接続できません</b>')) + '</span>';
    $('#md_status').innerHTML = h;
  }
  function applyZoom() { var z = (S.P.settings.zoom || 100) / 100, app = document.querySelector('.md-app'); app.style.zoom = z; app.style.height = (100 / z) + 'vh'; }
  function render(only) {
    renderTitle(); renderTabs(); renderRibbon(); renderSheets(); renderStatus();
    if (only === 'chrome') return;
    if (only !== 'pane') renderCenter();
    renderPane();
  }
  Object.assign(A, { settingsHtml: paneSettings, render: render, stopKeep: stopKeep, renderTitle: renderTitle, renderStatus: renderStatus, renderCenter: renderCenter, renderPane: renderPane, applyZoom: applyZoom, tablesOf: tablesOf, tableTsv: tableTsv, tableHtml: tableHtml, tablesSvg: tablesSvg, figsOf: figsOf,
    reportPlain: reportPlain, ensureHolmAsync: ensureHolmAsync, holmGroup: holmGroup, holmItems: holmItems, repItems: repItems, get: get, autoGrow: autoGrow, RTABS: RTABS, paletteGallery: paletteGallery, repItems: repItems, T_DESIGNS: T_DESIGNS, tableSvgDoc: tableSvgDoc, summaryTable: summaryTable, repMeta: repMeta, citeList: citeList, citeReport: citeReport });
})();
