// chart.js ― R が計算した集計結果から、図（SVG）を画面側で描く。
// 文字・線・目盛り・凡例・配色・大きさを変えても R を呼び直さないので、古いパソコンでも軽い。
// 図の中に題名（キャプション）は入れない。
(function () {
  'use strict';
  var mctx = document.createElement('canvas').getContext('2d'), uid = 0;
  var JA_ALIAS = { 'MS Gothic': 'ＭＳ ゴシック', 'MS PGothic': 'ＭＳ Ｐゴシック', 'MS Mincho': 'ＭＳ 明朝', 'Yu Gothic': '游ゴシック', 'Yu Mincho': '游明朝', 'Meiryo': 'メイリオ', 'BIZ UDGothic': 'BIZ UDゴシック', 'BIZ UDMincho': 'BIZ UD明朝' };
  var KIND_NAMES = { stack100: '帯グラフ', group: '集合棒', stack: '積み上げ棒', heat: '色分けの表', bar: '棒グラフ', line: '折れ線グラフ', pie: '円グラフ', mean: '平均と信頼区間', coef: '係数と信頼区間', box: '箱ひげ図', scatter: '散布図', hist: 'ヒストグラム', map: '布置図', cloud: 'ワードクラウド', wbar: '頻出語の棒グラフ', net: '共起ネットワーク' };
  // 配色。ramp＝濃淡（濃い色→淡い色の段階）、div＝2 方向（低い側→中央→高い側）、cat＝多色、pat＝白黒の模様。hide＝一覧に出さない（古いプロジェクトのため残す）
  //   濃淡と 2 方向は ColorBrewer、viridis 系は matplotlib の知覚的に均等な配色、多色は論文誌でよく使われる配色（ggsci）と Paul Tol・Okabe-Ito の色覚に配慮した配色を元にした
  var PALETTES = {
    gray: { name: 'グレー', group: '濃淡', ramp: ['#1A1A1A', '#525252', '#969696', '#D9D9D9', '#F7F7F7'] },
    blue: { name: '青', group: '濃淡', ramp: ['#08306B', '#2171B5', '#6BAED6', '#C6DBEF', '#F7FBFF'] },
    green: { name: '緑', group: '濃淡', ramp: ['#00441B', '#238B45', '#74C476', '#C7E9C0', '#F7FCF5'] },
    teal: { name: '青緑', group: '濃淡', ramp: ['#014636', '#02818A', '#67A9CF', '#D0D1E6', '#FFF7FB'] },
    purple: { name: '紫', group: '濃淡', ramp: ['#3F007D', '#6A51A3', '#9E9AC8', '#DADAEB', '#FCFBFD'] },
    red: { name: '赤', group: '濃淡', ramp: ['#67000D', '#CB181D', '#FB6A4A', '#FCBBA1', '#FFF5F0'] },
    orange: { name: '橙', group: '濃淡', ramp: ['#7F2704', '#D94801', '#FD8D3C', '#FDD0A2', '#FFF5EB'] },
    brown: { name: '茶', group: '濃淡', ramp: ['#662506', '#CC4C02', '#FE9929', '#FEE391', '#FFFFE5'] },
    viridis: { name: 'viridis', tip: '紫から黄へ。明るさが一様に変わり、白黒で印刷しても順序が読める', group: '濃淡', ramp: ['#440154', '#3B528B', '#21918C', '#5EC962', '#FDE725'], full: true },
    cividis: { name: 'cividis', tip: '紺から黄へ。色覚の違いがあっても同じように見える', group: '濃淡', ramp: ['#00204D', '#414D6B', '#7C7B78', '#BCAF6F', '#FFEA46'], full: true },
    magma: { name: 'magma', tip: '濃い紫から淡い黄へ', group: '濃淡', ramp: ['#2C115F', '#721F81', '#B73779', '#F1605D', '#FEB078', '#FCFDBF'], full: true },
    rdbu: { name: '青—赤', group: '2 方向', div: ['#2166AC', '#67A9CF', '#F7F7F7', '#EF8A62', '#B2182B'] },
    prgn: { name: '紫—緑', group: '2 方向', div: ['#762A83', '#AF8DC3', '#F7F7F7', '#7FBF7B', '#1B7837'] },
    brbg: { name: '茶—青緑', group: '2 方向', div: ['#8C510A', '#D8B365', '#F5F5F5', '#5AB4AC', '#01665E'] },
    puor: { name: '橙—紫', group: '2 方向', div: ['#B35806', '#F1A340', '#F7F7F7', '#998EC3', '#542788'] },
    cud: { name: 'Okabe-Ito', tip: '色覚の違いがあっても見分けやすい 8 色', group: '多色', cat: ['#0072B2', '#E69F00', '#009E73', '#D55E00', '#56B4E9', '#CC79A7', '#F0E442', '#000000'] },
    muted: { name: 'Paul Tol', tip: '落ち着いた色合いで、色覚の違いがあっても見分けやすい', group: '多色', cat: ['#332288', '#88CCEE', '#44AA99', '#117733', '#999933', '#DDCC77', '#CC6677', '#882255'] },
    npg: { name: 'Nature 風', tip: 'Nature 誌の図でよく見る配色', group: '多色', cat: ['#E64B35', '#4DBBD5', '#00A087', '#3C5488', '#F39B7F', '#8491B4', '#91D1C2', '#7E6148'] },
    aaas: { name: 'Science 風', tip: 'Science 誌の図でよく見る配色', group: '多色', cat: ['#3B4992', '#EE0000', '#008B45', '#631879', '#008280', '#BB0021', '#5F559B', '#A20056'] },
    nejm: { name: 'NEJM 風', tip: 'NEJM 誌の図でよく見る配色', group: '多色', cat: ['#BC3C29', '#0072B5', '#E18727', '#20854E', '#7876B1', '#6F99AD', '#FFDC91', '#EE4C97'] },
    lancet: { name: 'Lancet 風', tip: 'Lancet 誌の図でよく見る配色', group: '多色', cat: ['#00468B', '#ED0000', '#42B540', '#0099B4', '#925E9F', '#FDAF91', '#AD002A', '#ADB6B6'] },
    tableau: { name: 'Tableau', group: '多色', cat: ['#4E79A7', '#F28E2B', '#E15759', '#76B7B2', '#59A14F', '#EDC948', '#B07AA1', '#9C755F'] },
    dark2: { name: '濃い色', group: '多色', cat: ['#1B9E77', '#D95F02', '#7570B3', '#E7298A', '#66A61E', '#E6AB02', '#A6761D', '#666666'] },
    paired: { name: '対の色', group: '多色', cat: ['#A6CEE3', '#1F78B4', '#B2DF8A', '#33A02C', '#FB9A99', '#E31A1C', '#FDBF6F', '#FF7F00'] },
    office: { name: 'Office', group: '多色', hide: true, cat: ['#4472C4', '#ED7D31', '#A5A5A5', '#FFC000', '#5B9BD5', '#70AD47'] },
    set2: { name: '淡い色', group: '多色', hide: true, cat: ['#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3'] },
    mono: { name: '白黒の模様', group: '白黒', pat: ['solid', 'white', 'diag', 'dots', 'back', 'cross', 'horiz', 'gray'] }
  };
  function defaults() {
    return { kind: null, orient: 'h', value: 'pct', w: 560, h: 340, fit: true, fontJa: 'MS Gothic', fontEn: 'Arial',
      fsAxis: 12, fsTick: 12, fsLegend: 12, fsLabel: 11, lwAxis: 1, lwBar: 0.75, lwGrid: 0.5, labels: true, labelDec: 1, showN: true,
      legend: 'right', legFrame: false, legShape: 'square', legTitle: false, tickDir: 'out', tickLen: 4, grid: 'solid', gridColor: '#B3B3B3', frame: 'L',
      transparent: false, palette: 'gray', colorMode: null, colors: {}, min: null, max: null, step: null, vTitle: null, cTitle: null, barW: 70,
      heatBy: 'row', bar: 'right', barShape: 'smooth', seed: 1, legX: null, legY: null, legBg: null, legNames: {}, labelWhat: 'value' };
  }
  function fam(st) { var a = JA_ALIAS[st.fontJa]; return "'" + st.fontEn + "', '" + st.fontJa + "'" + (a ? ", '" + a + "'" : '') + ', sans-serif'; }
  function tw(s, size, st, bold) { mctx.font = (bold ? 'bold ' : '') + size + 'px ' + fam(st).replace(/'/g, '"'); return mctx.measureText(s).width; }
  function esc(s) { return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;'); }
  function wrap(s, maxW, size, st) {
    s = String(s); var lines = [], cur = '';
    for (var i = 0; i < s.length; i++) {
      var ch = s[i];
      if (cur && tw(cur + ch, size, st) > maxW) {
        var m = /[A-Za-z0-9.%]+$/.exec(cur);                                      // 英単語の途中では切らない
        if (m && /[A-Za-z0-9.%]/.test(ch) && m[0].length < cur.length) { lines.push(cur.slice(0, cur.length - m[0].length)); cur = m[0] + ch; }
        else { lines.push(cur); cur = ch; }
      } else cur += ch;
    }
    if (cur) lines.push(cur);
    return lines.length ? lines : [''];
  }
  // 行数を制限し、あふれた分は「…」にする（文字どうしが重ならないようにするため）
  function clampLines(lines, maxN, maxW, size, st) {
    if (lines.length <= maxN) return lines;
    var out = lines.slice(0, Math.max(1, maxN)), last = out[out.length - 1];
    while (last.length > 1 && tw(last + '…', size, st) > maxW) last = last.slice(0, -1);
    out[out.length - 1] = last + '…'; return out;
  }
  function fitText0(s, maxW, size, st) { s = String(s); if (tw(s, size, st) <= maxW) return s; while (s.length > 1 && tw(s + '…', size, st) > maxW) s = s.slice(0, -1); return s + '…'; }
  function hex2rgb(h) { h = h.replace('#', ''); return [parseInt(h.substr(0, 2), 16), parseInt(h.substr(2, 2), 16), parseInt(h.substr(4, 2), 16)]; }
  function rgb2hex(c) { return '#' + c.map(function (v) { v = Math.round(Math.max(0, Math.min(255, v))); return (v < 16 ? '0' : '') + v.toString(16); }).join('').toUpperCase(); }
  function lerp(a, b, t) { var x = hex2rgb(a), y = hex2rgb(b); return rgb2hex([x[0] + (y[0] - x[0]) * t, x[1] + (y[1] - x[1]) * t, x[2] + (y[2] - x[2]) * t]); }
  function lum(h) { var c = hex2rgb(h).map(function (v) { v /= 255; return v <= 0.03928 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4); }); return 0.2126 * c[0] + 0.7152 * c[1] + 0.0722 * c[2]; }
  function inkOn(h) { return /^#/.test(h) ? (lum(h) > 0.36 ? '#000000' : '#FFFFFF') : '#000000'; }
  function div3(d, t) { return t < 0.5 ? lerp(d[0], d[1], t * 2) : lerp(d[1], d[2], (t - 0.5) * 2); }
  function rampAt(st, t) { t = Math.max(0, Math.min(1, t)); var k = st.length - 1, i = Math.min(k - 1, Math.floor(t * k)); return lerp(st[i], st[i + 1], t * k - i); }
  // 系列に使う範囲：単色の濃淡は白に近い端を使わない。viridis などは全体を使う
  function rampSeries(p, t) { return rampAt(p.ramp, p.full ? t * 0.92 : 0.05 + t * 0.68); }
  function divSeries(p, t) { var d = p.div.slice(); d[2] = lerp(d[2], '#BDBDBD', 0.6); return rampAt(d, t); }
  // 系列の色（模様のときは 'pat:名前'）。個別に指定した色は、系列の名前にひも付けて保つ
  // 色の決め方：palette＝配色の一覧から選んだ色を順に使う、series＝系列ごとに選んだ色を使う（選んでいない系列は配色の色）
  //   古いプロジェクト（決め方の項目がない）は、色を個別に選んでいれば「系列ごと」とみなす
  function colorMode(st) { return st.colorMode === 'series' || st.colorMode === 'palette' ? st.colorMode : (st.colors && Object.keys(st.colors).length ? 'series' : 'palette'); }
  function seriesColors(labels, st) {
    var p = PALETTES[st.palette] || PALETTES.gray, n = labels.length, own = colorMode(st) === 'series' && st.colors;
    return labels.map(function (lab, i) {
      if (own && own[lab]) return own[lab];
      if (p.cat) return i < p.cat.length ? p.cat[i] : '#BBBBBB';
      if (p.pat) return 'pat:' + p.pat[i % p.pat.length];
      if (p.div) return n === 1 ? p.div[0] : divSeries(p, i / (n - 1));
      return n === 1 ? rampSeries(p, p.full ? 0.3 : 0.4) : rampSeries(p, i / (n - 1));
    });
  }
  function swatches(key, n, seed) { return seriesColors(new Array(n || 4).fill(0).map(function (_, i) { return 's' + i; }), { palette: key, colors: {}, seed: seed || 1 }); }
  // 色分けの表の色（t：0〜1。残差は 0.5 が中央）。小さい値ほど淡く、大きい値ほど濃い
  function heatColor(st, by, t) {
    var p = PALETTES[st.palette] || PALETTES.gray;
    if (by === 'resid') return rampAt((p.div || PALETTES.rdbu.div), t);
    if (p.ramp) { var r = p.ramp.slice().reverse(); return p.full ? rampAt(r, t) : rampAt(r, t * 0.92); }
    if (p.div) return rampAt(p.div.slice(Math.floor(p.div.length / 2)), t);
    return lerp('#FFFFFF', '#2B2B2B', t * 0.85);
  }
  function nice(lo, hi, n) {
    if (!(hi > lo)) hi = lo + 1;
    var raw = (hi - lo) / (n || 5), mag = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / mag;
    var step = (f <= 1 ? 1 : f <= 2 ? 2 : f <= 2.5 ? 2.5 : f <= 5 ? 5 : 10) * mag;
    return { min: Math.floor(lo / step + 1e-9) * step, max: Math.ceil(hi / step - 1e-9) * step, step: step };
  }
  function isNum(v) { return v !== null && v !== undefined && v !== '' && isFinite(+v); }
  function scaleFor(st, lo, hi, fixed) {
    var s = fixed || nice(lo, hi, 5), mn = isNum(st.min) ? +st.min : s.min, mx = isNum(st.max) ? +st.max : s.max;
    if (!(mx > mn)) mx = mn + 1;
    var step = isNum(st.step) && +st.step > 0 ? +st.step : (isNum(st.min) || isNum(st.max) ? nice(mn, mx, 5).step : s.step);
    if ((mx - mn) / step > 50) step = (mx - mn) / 50;
    var ticks = [];
    for (var v = Math.ceil(mn / step - 1e-9) * step; v <= mx + step * 1e-6; v += step) ticks.push(Math.round(v * 1e6) / 1e6);
    return { min: mn, max: mx, step: step, ticks: ticks };
  }
  function fmtTick(v) { return String(Math.round(v * 1000) / 1000); }
  function sum(a) { return a.reduce(function (s, v) { return s + (v || 0); }, 0); }
  function maxOf(a) { return a.length ? Math.max.apply(null, a) : 0; }

  // R の結果から図のもとになるデータを取り出す
  function model(res) {
    if (!res || !res.ok) return null;
    var f = res.fig || {};
    if (f.type === 'xtab' && res.xtab) return { type: 'xtab', cats: res.xtab.rows, series: res.xtab.cols, count: res.xtab.count, resid: res.xtab.resid || null, catTitle: res.xtab.rowLabel, seriesTitle: res.xtab.colLabel, prefer: f.prefer };
    if (f.type === 'items' && res.xtab) return { type: 'items', items: res.xtab.rows, choices: res.xtab.cols, count: res.xtab.count, prefer: f.prefer };
    if (f.type === 'freq') return { type: 'freq', cats: f.cats, series: f.series, count: f.count, base: f.base, catTitle: f.catTitle, multi: !!f.multi, time: !!f.time, prefer: f.prefer };
    if (f.type === 'multi') return { type: 'multi', cats: f.cats, series: f.series, count: f.count, pct: f.pct, catTitle: f.catTitle, seriesTitle: f.seriesTitle };
    if (f.type === 'box') return { type: 'box', cats: f.cats, box: f.box, catTitle: f.catTitle, valueTitle: f.valueTitle, series: [], prefer: f.prefer, coef: f.coef || null, ref: f.ref };
    if (f.type === 'scatter') return { type: 'scatter', x: f.x, y: f.y, xTitle: f.xTitle, yTitle: f.yTitle, series: [], cats: [], fit: f.fit || null };
    if (f.type === 'hist') return { type: 'hist', edges: f.edges, counts: f.counts, series: f.series, xTitle: f.xTitle, seriesTitle: f.seriesTitle, intBins: !!f.intBins, box: f.box, boxCats: f.boxCats, catTitle: f.catTitle, cats: [] };
    if (f.type === 'words') return { type: 'words', words: f.words, n: f.n, base: f.base, net: f.net, prefer: f.prefer || 'wbar', series: [], cats: [] };
    if (f.type === 'map') return { type: 'map', pts: f.pts, series: f.pts.map(function (p) { return p.name; }), xTitle: f.xTitle, yTitle: f.yTitle, cats: [] };
    return null;
  }
  function kinds(m) {
    if (!m) return [];
    if (m.type === 'freq') return m.time ? ['line', 'bar'] : m.multi ? ['bar'] : ['bar', 'pie'];   // 複数回答は合計が 100% を超えるので円グラフにしない。日時の推移は折れ線
    if (m.type === 'box' && m.coef) return ['coef'];                     // 回帰係数の図（点と 95% 信頼区間）
    if (m.type === 'box') return m.box && m.box.length && m.box[0].mean !== undefined && m.prefer === 'mean' ? ['mean', 'box'] : ['box'];   // 平均の比較（t 検定・分散分析）は平均と信頼区間
    return { xtab: ['stack100', 'group', 'stack', 'heat', 'pie'], items: ['stack100', 'group', 'heat', 'pie'], multi: ['group'], scatter: ['scatter'], hist: ['hist', 'box'], map: ['map'], words: ['wbar', 'cloud', 'net'] }[m.type] || [];
  }
  // その図で色を分けている系列の名前（凡例の並び）。図の種類によって変わる
  function seriesFor(m, st) { var k = kindOf(m, st); if (m.type === 'items') return k === 'group' ? m.items : m.choices; if (k === 'pie' && m.type === 'freq') return m.cats; return m.series || []; }
  // 色を選べる系列の名前（色の個別指定のキー）。系列が 1 つの図は、図の中で使う名前（box・mean・pt・回答数）にする
  function colorKeys(m, st) {
    var k = kindOf(m, st); if (k === 'heat' || k === 'cloud' || k === 'net') return [];
    var ser = seriesFor(m, st); if (ser.length) return ser;
    return { box: ['box'], mean: ['mean'], coef: ['mean'], scatter: ['pt'], wbar: ['回答数'] }[k] || [];
  }
  function kindOf(m, st) { var k = kinds(m); return k.indexOf(st.kind) >= 0 ? st.kind : (m.prefer && k.indexOf(m.prefer) >= 0 ? m.prefer : k[0]); }

  function render(m, st0, lang) {
    var st = Object.assign(defaults(), st0 || {}); lang = lang || 'ja';
    var W = Math.max(160, +st.w || 560), H = Math.max(120, +st.h || 340), F = fam(st), o = [], defs = [], kind = kindOf(m, st), id = 'c' + (++uid);
    var L = lang === 'en' ? { pct: 'Percentage (%)', count: 'Count', n: 'n = ', row: 'Row %', col: 'Column %', resid: 'Adjusted residual' } : { pct: '割合 (%)', count: '人数（人）', n: 'n = ', row: '行の割合 (%)', col: '列の割合 (%)', resid: '調整済み標準化残差' };
    var dec = Math.max(0, Math.min(2, isNum(st.labelDec) ? +st.labelDec : 1));
    // 積み上げた棒・円グラフは図の中に空きがないので、凡例を「図の中」にしたときは右に出す（棒や文字と重ねない）
    if ((kind === 'stack100' || kind === 'stack' || kind === 'pie') && (st.legend === 'inTR' || st.legend === 'inTL')) st.legend = 'right';
    // 複数の設問の単純集計：集合棒は「横軸＝回答、色＝設問」、それ以外は「行＝設問、色＝回答」にする
    if (m.type === 'items') {
      if (kind === 'group') m = { type: 'xtab', cats: m.choices, series: m.items, count: m.choices.map(function (_, j) { return m.items.map(function (_, i) { return m.count[i][j]; }); }), pctCol: true, catTitle: lang === 'en' ? 'Response' : '回答', seriesTitle: lang === 'en' ? 'Item' : '設問' };
      else m = { type: 'xtab', cats: m.items, series: m.choices, count: m.count, catTitle: lang === 'en' ? 'Item' : '設問', seriesTitle: lang === 'en' ? 'Response' : '回答' };
    }
    // 頻出語の棒グラフは、複数回答の単純集計と同じ形にして描く
    if (m.type === 'words' && kind === 'wbar') { var nW = Math.min(m.words.length, Math.max(5, +st.topWords || 20)); m = { type: 'freq', cats: m.words.slice(0, nW), series: ['回答数'], count: m.n.slice(0, nW).map(function (v) { return [v]; }), base: m.base, catTitle: lang === 'en' ? 'Word' : '語', multi: true }; kind = 'bar'; }
    if (m.type === 'words' && kind === 'net') m = { type: 'net', nodes: m.net.nodes, edges: m.net.edges, series: [], cats: [] };
    // ヒストグラムの図を箱ひげ図に切り替えたとき
    if (m.type === 'hist' && kind === 'box') m = { type: 'box', cats: m.boxCats, box: m.box, catTitle: m.catTitle, valueTitle: m.xTitle, series: [] };
    // 円グラフ：単純集計は 1 つの円、クロス集計は行（グループ・設問）ごとに円を並べる。色は回答ごと
    if (kind === 'pie') {
      var pies = m.type === 'freq' ? [{ title: '', n: m.base || sum(m.count.map(function (r) { return r[0]; })), v: m.count.map(function (r) { return r[0]; }) }]
        : m.cats.map(function (c, i) { return { title: c, n: sum(m.count[i]), v: m.count[i] }; });
      m = { type: 'pie', pies: pies, series: m.type === 'freq' ? m.cats : m.series, seriesTitle: m.type === 'freq' ? m.catTitle : m.seriesTitle };
    }
    // 凡例に出す系列の名前（作業ウィンドウや凡例のダブルクリックで変えた名前）。色は元の名前にひも付けたまま
    var names = st.legNames || {}, dispS = (m.series || []).map(function (s) { return names[s] ? names[s] : s; });
    // 値のラベルに書く内容：value＝軸と同じ値、pct＝割合、n＝人数（件数）、both＝割合と人数の両方
    function labelParts(cnt, pc, countAxis) {
      var w = st.labelWhat || 'value', ps = pc.toFixed(dec) + '%', ns = String(cnt);
      if (w === 'value') w = countAxis ? 'n' : 'pct';
      if (w === 'pct') return [ps]; if (w === 'n') return [ns];
      return countAxis ? [ns, '(' + ps + ')'] : [ps, '(' + ns + ')'];
    }
    function labelText(cnt, pc, countAxis) { return labelParts(cnt, pc, countAxis).join(' '); }
    function r1(v) { return Math.round(v * 10) / 10; }
    function T(x, y, s, a) {
      a = a || {};
      o.push('<text x="' + r1(x) + '" y="' + r1(y) + '" font-family="' + F + '" font-size="' + (a.size || st.fsTick) + '"' + (a.bold ? ' font-weight="bold"' : '') +
        ' text-anchor="' + (a.anchor || 'start') + '" fill="' + (a.fill || '#000000') + '"' + (a.halo ? ' stroke="#FFFFFF" stroke-width="3" paint-order="stroke" stroke-linejoin="round"' : '') +
        (a.rot ? ' transform="rotate(' + a.rot + ' ' + r1(x) + ' ' + r1(y) + ')"' : '') + (a.el ? ' data-el="' + a.el + '"' : '') + (a.full ? ' data-full="' + esc(a.full) + '"' : '') + (a.tip ? ' data-tip="' + esc(a.tip) + '"' : '') + '>' + esc(s) + '</text>');
    }
    var lhA = st.fsAxis * 1.25;
    // 軸の名前は「…」で省略せず、幅に合わせて折り返す（最大 5 行）
    function aLines(text, width) { return text ? clampLines(wrap(text, Math.max(60, width), st.fsAxis, st), 5, Math.max(60, width), st.fsAxis, st) : []; }
    function aRes(lines) { return lines.length ? lines.length * lhA + st.fsAxis * 0.35 : 0; }
    function drawBottomTitle(lines, cx, el, full) { var yT = H - bottom - aRes(lines); lines.forEach(function (l, k) { T(cx, yT + st.fsAxis * 0.95 + k * lhA, l, { size: st.fsAxis, anchor: 'middle', el: el, full: full }); }); }
    function drawTopTitle(lines, cx, yT, el, full) { lines.forEach(function (l, k) { T(cx, yT + st.fsAxis * 0.95 + k * lhA, l, { size: st.fsAxis, anchor: 'middle', el: el, full: full }); }); }
    function drawSideTitle(lines, cy, el, full) { lines.forEach(function (l, k) { T(left + st.fsAxis * 0.9 + k * lhA, cy, l, { size: st.fsAxis, anchor: 'middle', rot: -90, el: el, full: full }); }); }
    function TL(x, y, lines, a) { var lh = (a.size || st.fsTick) * 1.25; lines.forEach(function (l, i) { T(x, y + i * lh, l, a); }); }
    function line(x1, y1, x2, y2, w, color, dash) { if (w > 0) o.push('<line x1="' + r1(x1) + '" y1="' + r1(y1) + '" x2="' + r1(x2) + '" y2="' + r1(y2) + '" stroke="' + (color || '#000000') + '" stroke-width="' + w + '"' + (dash ? ' stroke-dasharray="' + dash + '"' : '') + '/>'); }
    function fillOf(c) {                                                          // 模様は <defs> に登録して参照する
      if (!/^pat:/.test(c)) return c;
      var k = c.slice(4), pid = id + k;
      if (k === 'solid') return '#000000'; if (k === 'white') return '#FFFFFF'; if (k === 'gray') return '#9E9E9E';
      if (!defs.some(function (d) { return d.indexOf('id="' + pid + '"') >= 0; })) {
        var body = { diag: '<path d="M-1 7L7 -1M3 9L9 3M-1 1L1 -1" stroke="#000" stroke-width="1.2"/>', back: '<path d="M-1 1L7 9M-1 -5L9 5M5 -1L9 3" stroke="#000" stroke-width="1.2"/>',
          cross: '<path d="M0 4H8M4 0V8" stroke="#000" stroke-width="1"/>', horiz: '<path d="M0 2H8M0 6H8" stroke="#000" stroke-width="1.2"/>', dots: '<circle cx="2" cy="2" r="1.1" fill="#000"/><circle cx="6" cy="6" r="1.1" fill="#000"/>' }[k] || '';
        defs.push('<pattern id="' + pid + '" width="8" height="8" patternUnits="userSpaceOnUse"><rect width="8" height="8" fill="#FFFFFF"/>' + body + '</pattern>');
      }
      return 'url(#' + pid + ')';
    }
    function rect(x, y, w, h, fill, tip, extra) { o.push('<rect x="' + r1(x) + '" y="' + r1(y) + '" width="' + r1(Math.max(0, w)) + '" height="' + r1(Math.max(0, h)) + '" fill="' + fillOf(fill) + '"' + (st.lwBar > 0 ? ' stroke="#000000" stroke-width="' + st.lwBar + '"' : '') + (tip ? ' data-tip="' + esc(tip) + '"' : '') + (extra || '') + '/>'); }
    var gridDash = st.grid === 'dash' ? '4 3' : st.grid === 'dot' ? '1 3' : null, gridOn = st.grid && st.grid !== 'none';
    function tick(x, y, horizAxis) {                                              // 軸の上の目盛り。horizAxis：横軸の目盛りか
      if (st.tickDir === 'none' || !(st.lwAxis > 0)) return;
      var len = (+st.tickLen || 4) * (st.tickDir === 'in' ? -1 : 1);
      if (horizAxis) line(x, y, x, y + len, st.lwAxis); else line(x - len, y, x, y, st.lwAxis);
    }
    var tickPad = (st.tickDir === 'out' ? (+st.tickLen || 4) : 0) + 3;

    // 選択肢が多いときは、文字や棒が重ならない大きさまで広げる（「自動で広げる」がオンのとき）
    if (st.fit !== false && m.cats && m.cats.length && kind !== 'scatter') {
      var nC = m.cats.length, nS = Math.max(1, (m.series || []).length), lhF = st.fsTick * 1.25, extraN = (st.showN && m.type === 'xtab' && kind !== 'heat') ? 1 : 0;
      var grp = (kind === 'group' || kind === 'bar') ? nS : 1, bw = Math.max(10, Math.min(100, +st.barW || 70)) / 100;
      var chrome = 22 + (st.fsTick + 8) + st.fsAxis * 1.5 + ((st.legend === 'top' || st.legend === 'bottom') && nS > 1 ? st.fsLegend * 1.25 + 12 : 0);
      if (kind === 'heat') {
        var cwH = Math.min(W * 0.28, maxOf(m.cats.map(function (c) { return tw(c, st.fsTick, st); })) + 2);
        var rowMin = maxOf(m.cats.map(function (c) { return wrap(c, cwH, st.fsTick, st).length * lhF + 6; }).concat([st.labels ? st.fsLabel * 2.6 + 6 : 18]));
        H = Math.max(H, Math.ceil(chrome + 3 * lhF + nC * rowMin + (st.bar === 'bottom' ? 44 : 0)));
      } else if (st.orient !== 'v') {
        var cwB = Math.min(W * 0.30, maxOf(m.cats.map(function (c) { return tw(c, st.fsTick, st); })) + 2);
        var labMax = maxOf(m.cats.map(function (c) { return (wrap(c, cwB, st.fsTick, st).length + extraN) * lhF + 8; }));
        H = Math.max(H, Math.ceil(chrome + nC * Math.max(labMax, (grp * (st.labels ? st.fsLabel + 3 : 8)) / bw, 22)));
      } else {
        var perBar = st.labels ? Math.max(st.fsLabel + 4, grp > 1 ? 14 : 30) : 8;
        W = Math.max(W, Math.ceil(70 + (nS > 1 && st.legend === 'right' ? W * 0.25 : 0) + nC * Math.max(grp * perBar / bw, 34)));
      }
    }
    if (!st.transparent) o.push('<rect x="0" y="0" width="' + W + '" height="' + H + '" fill="#FFFFFF"/>');
    var top = 12, bottom = 10, left = 10, right = 14;

    // ---- 凡例の大きさを先に決める
    var colors = seriesColors(m.series || [], st), lg = null, sw = st.fsLegend, gap = 6, lhL = st.fsLegend * 1.25;
    var swW = st.legShape === 'wide' ? sw * 1.8 : sw, legFill = st.legBg && st.legBg !== 'none' ? st.legBg : null, legInk = legFill ? inkOn(legFill) : '#000000', padL = st.legFrame || legFill ? 7 : 0;
    function layoutLegend() {
      lg = null;
    if ((m.series || []).length > 1 && kind !== 'heat' && st.legend !== 'none') {
        var ttl = st.legTitle && m.seriesTitle ? m.seriesTitle : '';
        if (st.legend === 'top' || st.legend === 'bottom') {
          var rows = [[]], x = 0, avail = W - 30 - padL * 2;
          dispS.forEach(function (s, i) {
            var lines = wrap(s, avail - swW - gap, st.fsLegend, st), iw = swW + gap + maxOf(lines.map(function (l) { return tw(l, st.fsLegend, st); })) + 16;
            if (x + iw > avail && rows[rows.length - 1].length) { rows.push([]); x = 0; }
            rows[rows.length - 1].push({ i: i, w: iw, lines: lines }); x += iw;
          });
          var tl = ttl ? wrap(ttl, avail, st.fsLegend, st) : [];
          lg = { flow: true, rows: rows, title: tl, w: Math.min(avail, maxOf(rows.map(function (r) { return sum(r.map(function (it) { return it.w; })); }))) + padL * 2,
            h: rows.reduce(function (s, r) { return s + maxOf(r.map(function (it) { return it.lines.length; })) * lhL + 4; }, 0) + tl.length * lhL + padL * 2 };
          if (st.legend === 'top') { lg.y = top; top += lg.h + 8; } else { bottom += lg.h + 4; lg.y = H - bottom + 6; }
          lg.x = (W - lg.w) / 2;
        } else {
          var maxw = Math.min(W * (st.legend === 'right' ? 0.32 : 0.42), maxOf(dispS.map(function (s) { return tw(s, st.fsLegend, st); })) + 2);
          var items = dispS.map(function (s) { return wrap(s, maxw, st.fsLegend, st); }), tl2 = ttl ? wrap(ttl, maxw + swW + gap, st.fsLegend, st) : [];
          lg = { flow: false, items: items, title: tl2, w: Math.max(swW + gap + maxOf(items.map(function (ls) { return maxOf(ls.map(function (l) { return tw(l, st.fsLegend, st); })); })), maxOf(tl2.map(function (l) { return tw(l, st.fsLegend, st, true); }))) + padL * 2,
            h: items.reduce(function (s, ls) { return s + Math.max(ls.length * lhL, sw) + 6; }, 0) - 6 + tl2.length * lhL + (tl2.length ? 3 : 0) + padL * 2 };
          if (st.legend === 'right') { right += lg.w + 14; lg.x = W - right + 14; lg.y = top + 2; }
        }
      }
    }
    layoutLegend();
    // 凡例を図の中に置くと棒や数値に重なるときは、右に出す（割合の軸を 100% より先へ伸ばさない）
    if (lg && !lg.flow && (st.legend === 'inTR' || st.legend === 'inTL') && (kind === 'group' || kind === 'bar')) {
      var hzL = st.orient !== 'v', lenL = hzL ? W - left - right - Math.min(W * 0.30, maxOf(m.cats.map(function (c) { return tw(c, st.fsTick, st); }))) - 34 : H - top - bottom - (st.fsTick * 2.5 + 10) - st.fsAxis * 1.5;
      var pctMode = st.value !== 'count', rtL = m.count.map(function (r) { return sum(r); }), hiL = 0;
      m.count.forEach(function (r, i) { r.forEach(function (c, j) { hiL = Math.max(hiL, !pctMode ? c : m.type === 'multi' ? m.pct[i][j] * 100 : m.type === 'freq' ? (m.base ? c / m.base * 100 : 0) : (rtL[i] ? c / rtL[i] * 100 : 0)); }); });
      var needL = (hzL ? lg.w : lg.h) + 16 + (st.labels ? 40 : 0), facL = lenL - needL > 60 ? lenL / (lenL - needL) : 99;
      if (facL > 1.45 || (pctMode && hiL <= 100 && hiL * facL > 100)) { st.legend = 'right'; layoutLegend(); }
    }
    function swatch(x, y, c, i) {
      var ex = ' data-el="legend:' + i + '"';
      if (st.legShape === 'circle') o.push('<circle cx="' + r1(x + sw / 2) + '" cy="' + r1(y + sw / 2) + '" r="' + r1(sw / 2) + '" fill="' + fillOf(c) + '"' + (st.lwBar > 0 ? ' stroke="#000000" stroke-width="' + st.lwBar + '"' : '') + ex + '/>');
      else if (st.legShape === 'wide') rect(x, y + sw * 0.2, swW, sw * 0.6, c, null, ex);
      else rect(x, y, sw, sw, c, null, ex);
    }
    function drawLegend(plot) {
      if (!lg) return;
      if (st.legend === 'inTR') { lg.x = plot.x1 - lg.w - 8; lg.y = plot.y0 + 8; } else if (st.legend === 'inTL') { lg.x = plot.x0 + 8; lg.y = plot.y0 + 8; }
      else if (st.legend === 'free') { lg.x = Math.max(0, Math.min(W - lg.w, isNum(st.legX) ? +st.legX : W - lg.w - 12)); lg.y = Math.max(0, Math.min(H - lg.h, isNum(st.legY) ? +st.legY : top + 4)); }
      var inside = st.legend === 'inTR' || st.legend === 'inTL' || st.legend === 'free';
      o.push('<g data-legend="1" data-box="' + [r1(lg.x), r1(lg.y), r1(lg.w), r1(lg.h)].join(',') + '" data-tip="ドラッグで凡例を動かせます。色見本をクリックすると色を、名前をダブルクリックすると名前を変えられます。">');
      o.push('<rect x="' + r1(lg.x - 4) + '" y="' + r1(lg.y - 4) + '" width="' + r1(lg.w + 8) + '" height="' + r1(lg.h + 8) + '" fill="#FFFFFF" fill-opacity="0"/>');   // つかむための透明な面
      // 背景：自動（図の中に置くときだけ白を薄く敷く）・塗りなし・指定の色
      var bgFill = legFill || (st.legBg === 'none' || !inside ? null : '#FFFFFF'), padded = !!(st.legFrame || legFill), ex4 = padded ? 0 : 4;
      if (st.legFrame || bgFill) o.push('<rect x="' + r1(lg.x - ex4) + '" y="' + r1(lg.y - ex4) + '" width="' + r1(lg.w + ex4 * 2) + '" height="' + r1(lg.h + ex4 * 2) + '" fill="' + (bgFill || 'none') + '"' + (bgFill && !legFill ? ' fill-opacity="0.9"' : '') + (st.legFrame ? ' stroke="#000000" stroke-width="' + Math.max(0.5, st.lwAxis) + '"' : '') + '/>');
      var x = lg.x + padL, y = lg.y + padL;
      lg.title.forEach(function (l) { T(lg.flow ? lg.x + lg.w / 2 : x, y + st.fsLegend * 0.86, l, { size: st.fsLegend, bold: true, anchor: lg.flow ? 'middle' : 'start', fill: legInk }); y += lhL; });
      if (lg.title.length && !lg.flow) y += 3;
      if (lg.flow) lg.rows.forEach(function (row) {
        var tot = sum(row.map(function (it) { return it.w; })), xx = lg.x + (lg.w - tot) / 2 + 8, rh = maxOf(row.map(function (it) { return it.lines.length; })) * lhL + 4;
        row.forEach(function (it) { swatch(xx, y + 1, colors[it.i], it.i); TL(xx + swW + gap, y + 1 + sw * 0.86, it.lines, { size: st.fsLegend, el: 'legname:' + it.i, fill: legInk }); xx += it.w; });
        y += rh;
      });
      else lg.items.forEach(function (ls, i) { swatch(x, y, colors[i], i); TL(x + swW + gap, y + sw * 0.86, ls, { size: st.fsLegend, el: 'legname:' + i, fill: legInk }); y += Math.max(ls.length * lhL, sw) + 6; });
      o.push('</g>');
    }
    var cTitle, vTitle, plot;
    if (kind === 'heat') drawHeat(); else if (kind === 'scatter') drawScatter(); else if (kind === 'pie') drawPie(); else if (kind === 'hist') drawHist(); else if (kind === 'map') drawMap(); else if (kind === 'cloud') drawCloud(); else if (kind === 'net') drawNet(); else drawAxes();
    drawLegend(plot);
    return { svg: '<svg xmlns="http://www.w3.org/2000/svg" width="' + W + '" height="' + H + '" viewBox="0 0 ' + W + ' ' + H + '">' + (defs.length ? '<defs>' + defs.join('') + '</defs>' : '') + o.join('') + '</svg>', w: W, h: H, kind: kind };

    function frameAndAxes(x0, y0, x1, y1) {
      if (st.frame === 'box') { line(x0, y0, x1, y0, st.lwAxis); line(x1, y0, x1, y1, st.lwAxis); }
      line(x0, y0, x0, y1, st.lwAxis); line(x0, y1, x1, y1, st.lwAxis);
    }
    // 横軸の数値ラベルが重なるときは、間引く
    function thin(ticks, pxPer, widthOf) { var need = maxOf(ticks.map(widthOf)) + 8, k = Math.max(1, Math.ceil(need / Math.max(1, pxPer))); return function (i) { return i % k === 0; }; }

    // ---- 円グラフ（12 時の位置から時計回り。入りきらない値は書かない）
    function drawPie() {
      var x0 = left, x1 = W - right, y0 = top, y1 = H - bottom, n = m.pies.length, lhT = st.fsTick * 1.25, best = null;
      plot = { x0: x0, y0: y0, x1: x1, y1: y1 };
      for (var c = 1; c <= n; c++) {                                                // 円がいちばん大きくなる並べ方（列の数）を選ぶ
        var rows = Math.ceil(n / c), cw = (x1 - x0) / c, ch = (y1 - y0) / rows;
        var tl = m.pies.map(function (p) { var l = p.title ? clampLines(wrap(p.title, Math.max(40, cw - 8), st.fsTick, st), 3, Math.max(40, cw - 8), st.fsTick, st) : []; if (st.showN && n > 1) l.push('(' + L.n + p.n + ')'); return l; });
        var th = maxOf(tl.map(function (l) { return l.length; })) * lhT + (n > 1 ? 4 : 0), r = Math.min(cw - 10, ch - th - 10) / 2;
        if (!best || r > best.r) best = { c: c, rows: rows, cw: cw, ch: ch, tl: tl, th: th, r: r };
      }
      var R = Math.max(8, best.r), colors = seriesColors(m.series, st), sw = st.lwBar > 0 ? st.lwBar : 1, sc = st.lwBar > 0 ? '#000000' : '#FFFFFF';
      m.pies.forEach(function (p, k) {
        var col = k % best.c, row = Math.floor(k / best.c), cx = x0 + best.cw * (col + 0.5), cyTop = y0 + best.ch * row, cy = cyTop + best.th + (best.ch - best.th) / 2;
        best.tl[k].forEach(function (l, j) { T(cx, cyTop + st.fsTick + j * lhT, l, { anchor: 'middle', bold: j < best.tl[k].length - (st.showN && n > 1 ? 1 : 0) }); });
        var tot = sum(p.v), a = -Math.PI / 2;
        if (!tot) { o.push('<circle cx="' + r1(cx) + '" cy="' + r1(cy) + '" r="' + r1(R) + '" fill="none" stroke="#999999"/>'); return; }
        p.v.forEach(function (v, j) {
          if (!v) return;
          var ang = v / tot * Math.PI * 2, b = a + ang, pc = v / tot * 100, tip = (p.title ? p.title + ' / ' : '') + dispS[j] + '：' + v + '人（' + pc.toFixed(1) + '%）';
          if (ang >= Math.PI * 2 - 1e-9) o.push('<circle cx="' + r1(cx) + '" cy="' + r1(cy) + '" r="' + r1(R) + '" fill="' + fillOf(colors[j]) + '" stroke="' + sc + '" stroke-width="' + sw + '" data-tip="' + esc(tip) + '"/>');
          else o.push('<path d="M' + r1(cx) + ' ' + r1(cy) + 'L' + r1(cx + R * Math.cos(a)) + ' ' + r1(cy + R * Math.sin(a)) + 'A' + r1(R) + ' ' + r1(R) + ' 0 ' + (ang > Math.PI ? 1 : 0) + ' 1 ' + r1(cx + R * Math.cos(b)) + ' ' + r1(cy + R * Math.sin(b)) + 'Z" fill="' + fillOf(colors[j]) + '" stroke="' + sc + '" stroke-width="' + sw + '" stroke-linejoin="round" data-tip="' + esc(tip) + '"/>');
          if (st.labels) {
            // 割合と人数の両方を出すときは 2 行にする
            var parts = labelParts(v, pc, st.value === 'count'), mid = a + ang / 2, lr = R * (parts.length > 1 ? 0.6 : 0.64), isPat = /^pat:/.test(colors[j]) && !/pat:(solid|white|gray)/.test(colors[j]);
            var lwP = maxOf(parts.map(function (t) { return tw(t, st.fsLabel, st); })), lhP = st.fsLabel * 1.15, hP = parts.length * lhP;
            if (R >= 24 && ang * lr >= lwP + 6 && ang * lr >= hP + 2 && lr + hP / 2 < R + 2) {
              var ly = cy + lr * Math.sin(mid) - (parts.length - 1) * lhP / 2 + st.fsLabel * 0.35;
              parts.forEach(function (t, q) { T(cx + lr * Math.cos(mid), ly + q * lhP, t, { size: st.fsLabel, anchor: 'middle', fill: colors[j] === 'pat:solid' ? '#FFFFFF' : inkOn(colors[j]), halo: isPat, tip: tip }); });
            }
          }
          a = b;
        });
      });
    }
    // ---- 棒グラフ・箱ひげ図（分類の軸と数値の軸を持つ図）
    function drawAxes() {
      var nc = m.cats.length, ns = Math.max(1, (m.series || []).length), isBox = kind === 'box' || kind === 'mean' || kind === 'coef', isLine = kind === 'line', lines = [];
      var rowTot = isBox ? [] : m.count.map(function (r) { return sum(r); });
      var mode = kind === 'stack100' ? 'pct' : kind === 'stack' ? 'count' : (st.value === 'count' ? 'count' : 'pct');
      var colTot = isBox ? [] : (m.series || []).map(function (_, j) { return sum(m.count.map(function (r) { return r[j]; })); });
      function pctOf(i, j) { return m.type === 'multi' ? m.pct[i][j] * 100 : m.type === 'freq' ? (m.base ? m.count[i][j] / m.base * 100 : 0) : m.pctCol ? (colTot[j] ? m.count[i][j] / colTot[j] * 100 : 0) : (rowTot[i] ? m.count[i][j] / rowTot[i] * 100 : 0); }
      function val(i, j) { return mode === 'count' ? m.count[i][j] : pctOf(i, j); }
      var stacked = kind === 'stack100' || kind === 'stack', hi = 0, sc, vAuto;
      if (isBox) {
        var all = isNum(m.ref) ? [m.ref] : []; m.box.forEach(function (b) { all = kind !== 'box' ? all.concat([b.mean, b.lo, b.hi].filter(isNum)) : all.concat(b.stats, b.out || []); });
        sc = scaleFor(st, Math.min.apply(null, all), Math.max.apply(null, all), nice(Math.min.apply(null, all), Math.max.apply(null, all), 5)); vAuto = m.valueTitle || '';
      } else {
        var lwMax = 0;
        for (var i = 0; i < nc; i++) for (var j = 0; j < ns; j++) { hi = Math.max(hi, stacked ? (kind === 'stack' ? rowTot[i] : 100) : val(i, j)); if (st.labels && !stacked) lwMax = Math.max(lwMax, tw(labelText(m.count[i][j], pctOf(i, j), mode === 'count'), st.fsLabel, st)); }
        // 棒の先に書く数値と、図の中に置く凡例が、棒や枠と重ならないだけの余白を数値の軸に取る
        var hz = st.orient !== 'v' && !isLine, estLen = hz ? W - left - right - Math.min(W * 0.30, maxOf(m.cats.map(function (c) { return tw(c, st.fsTick, st); }))) - 34 : H - top - bottom - (st.fsTick * 2.5 + 10) - st.fsAxis * 1.5, extra = 0;
        if (lwMax) { if (hz) extra = lwMax + 8; else { var ttE = (W - left - right - 50) / nc * Math.max(10, Math.min(100, +st.barW || 70)) / 100 / ns; extra = (ttE >= lwMax + 1 ? st.fsLabel : lwMax) + 8; } }
        if (lg && !lg.flow && !stacked && (st.legend === 'inTR' || st.legend === 'inTL')) extra += (hz ? lg.w : lg.h) + 16;
        var headroom = stacked ? 1 : (estLen > extra + 40 ? estLen / (estLen - extra) : 1.8);
        // 割合の軸は 100% を超えて伸ばさない（入りきらない値のラベルは棒の中に書く）
        var fixedSc = kind === 'stack100' ? { min: 0, max: 100, step: 20 } : null;
        if (!fixedSc && mode === 'pct' && hi <= 100 && nice(0, hi * headroom, 5).max > 100) fixedSc = { min: 0, max: 100, step: 20 };
        sc = scaleFor(st, 0, hi * headroom, fixedSc); vAuto = mode === 'pct' ? L.pct : L.count;
      }
      vTitle = st.vTitle === null || st.vTitle === undefined ? vAuto : st.vTitle;
      cTitle = st.cTitle === null || st.cTitle === undefined ? (m.catTitle || '') : st.cTitle;
      var showN = st.showN && m.type === 'xtab' && !m.pctCol, horiz = st.orient !== 'v' && !isLine, lhT = st.fsTick * 1.25, x0, x1, y0, y1, labs, cw;
      if (horiz) {
        cw = Math.min(W * 0.30, maxOf(m.cats.map(function (c) { return tw(c, st.fsTick, st); })) + 2);
        var bL = aLines(vTitle, W - left - right - 40), sL;
        y0 = top; y1 = H - bottom - (st.fsTick + tickPad + 4) - aRes(bL); if (y1 - y0 < 30) y1 = y0 + 30;
        sL = aLines(cTitle, y1 - y0 + 10);
        var bandH = (y1 - y0) / nc, maxL = Math.max(1, Math.floor((bandH - 2) / lhT));
        labs = m.cats.map(function (c, i) { var l = wrap(c, cw, st.fsTick, st), nl = showN && l.length + 1 <= maxL ? ['(' + L.n + rowTot[i] + ')'] : []; return clampLines(l, maxL - nl.length, cw, st.fsTick, st).concat(nl); });
        var lw = maxOf(labs.map(function (l) { return maxOf(l.map(function (s) { return tw(s, st.fsTick, st); })); }));
        x0 = left + aRes(sL) + lw + tickPad + 4; x1 = W - right - tw(fmtTick(sc.max), st.fsTick, st) / 2;
      } else {
        var tickW = maxOf(sc.ticks.map(function (t) { return tw(fmtTick(t), st.fsTick, st); }));
        var sL = aLines(vTitle, H - top - bottom - 60), bL = aLines(cTitle, W - left - right - 40);
        x0 = left + aRes(sL) + tickW + tickPad + 4; x1 = W - right; if (x1 - x0 < 30) x1 = x0 + 30;
        var band0 = (x1 - x0) / nc;
        labs = m.cats.map(function (c, i) { if (isLine) return [String(c)]; var l = clampLines(wrap(c, Math.max(band0 - 6, st.fsTick), st.fsTick, st), 4, Math.max(band0 - 6, st.fsTick), st.fsTick, st); if (showN && band0 > tw('(' + L.n + rowTot[i] + ')', st.fsTick, st)) l.push('(' + L.n + rowTot[i] + ')'); return l; });
        y0 = top + st.fsTick * 0.5; y1 = H - bottom - (maxOf(labs.map(function (l) { return l.length; })) * lhT + tickPad + 4) - aRes(bL);
      }
      if (x1 - x0 < 30) x1 = x0 + 30; if (y1 - y0 < 30) y1 = y0 + 30;
      plot = { x0: x0, y0: y0, x1: x1, y1: y1 };
      var len = horiz ? x1 - x0 : y1 - y0;
      function pos(v) { var t = (v - sc.min) / (sc.max - sc.min); t = Math.max(0, Math.min(1, t)); return horiz ? x0 + t * len : y1 - t * len; }
      var pxPer = len / Math.max(1, sc.ticks.length - 1), show = horiz ? thin(sc.ticks, pxPer, function (t) { return tw(fmtTick(t), st.fsTick, st); }) : thin(sc.ticks, pxPer, function () { return st.fsTick; });
      sc.ticks.forEach(function (t, i) {
        var p = pos(t);
        if (horiz) { if (gridOn && t > sc.min) line(p, y0, p, y1, st.lwGrid, st.gridColor, gridDash); tick(p, y1, true); if (show(i)) T(p, y1 + tickPad + st.fsTick * 0.85, fmtTick(t), { anchor: 'middle' }); }
        else { if (gridOn && t > sc.min) line(x0, p, x1, p, st.lwGrid, st.gridColor, gridDash); tick(x0, p, false); if (show(i)) T(x0 - tickPad, p + st.fsTick * 0.35, fmtTick(t), { anchor: 'end' }); }
      });
      if (isBox && isNum(m.ref) && m.ref > sc.min && m.ref < sc.max) { var pr = pos(m.ref); if (horiz) line(pr, y0, pr, y1, Math.max(0.75, st.lwAxis), '#000000', '4 3'); else line(x0, pr, x1, pr, Math.max(0.75, st.lwAxis), '#000000', '4 3'); }   // 回帰係数の図の基準線（β = 0、オッズ比 = 1）
      var band = (horiz ? y1 - y0 : x1 - x0) / nc, thick = band * Math.max(10, Math.min(100, +st.barW || 70)) / 100;
      var skipK = horiz ? Math.max(1, Math.ceil(lhT / Math.max(1, band))) : isLine ? Math.max(1, Math.ceil((maxOf(labs.map(function (l) { return tw(l[0], st.fsTick, st); })) + 8) / Math.max(1, band))) : Math.max(1, Math.ceil(st.fsTick * 1.3 / Math.max(1, band)));   // 折れ線の日付は折り返さずに間引く       // 帯が細すぎるときは名前を間引く
      for (var c = 0; c < nc; c++) {
        var b0 = (horiz ? y0 : x0) + band * c + (band - thick) / 2, mid = b0 + thick / 2;
        if (c % skipK === 0) { if (horiz) TL(x0 - tickPad - 1, mid - (labs[c].length - 1) * lhT / 2 + st.fsTick * 0.35, labs[c], { anchor: 'end' }); else TL(mid, y1 + tickPad + st.fsTick * 0.9, labs[c], { anchor: 'middle' }); }
        if (isBox) { (kind !== 'box' ? drawMean : drawBox)(m.box[c], b0, thick, mid, pos, horiz, m.cats[c]); continue; }
        if (isLine) { for (var s0 = 0; s0 < ns; s0++) { (lines[s0] = lines[s0] || []).push([mid, pos(val(c, s0)), m.cats[c] + (ns > 1 ? ' / ' + dispS[s0] : '') + '：' + m.count[c][s0] + (mode === 'count' ? '' : '（' + pctOf(c, s0).toFixed(1) + '%）')]); } continue; }
        var acc = 0;
        for (var s = 0; s < ns; s++) {
          var v = val(c, s), cnt = m.count[c][s], pc = pctOf(c, s), isPat = /^pat:/.test(colors[s]) && !/pat:(solid|white|gray)/.test(colors[s]);
          var tip = m.cats[c] + (ns > 1 ? ' / ' + dispS[s] : '') + '：' + cnt + '人（' + pc.toFixed(1) + '%）';
          var lab = labelText(cnt, pc, mode === 'count'), lwid = tw(lab, st.fsLabel, st), a, b2, t0, tt;
          if (stacked) { a = pos(acc); b2 = pos(acc + v); acc += v; t0 = b0; tt = thick; } else { a = pos(Math.max(sc.min, 0)); b2 = pos(v); tt = thick / ns; t0 = b0 + tt * s; }
          var segLen = Math.abs(b2 - a), ink = colors[s] === 'pat:solid' ? '#FFFFFF' : inkOn(colors[s]);
          if (horiz) rect(Math.min(a, b2), t0, segLen, tt, colors[s], tip); else rect(t0, Math.min(a, b2), tt, segLen, colors[s], tip);
          if (!st.labels || (stacked && cnt === 0)) continue;
          if (stacked) {
            if (horiz ? (segLen > lwid + 4 && tt > st.fsLabel + 2) : (segLen > st.fsLabel + 3 && tt > lwid + 2)) {
              if (horiz) T((a + b2) / 2, t0 + tt / 2 + st.fsLabel * 0.35, lab, { size: st.fsLabel, anchor: 'middle', fill: ink, halo: isPat, tip: tip });
              else T(t0 + tt / 2, (a + b2) / 2 + st.fsLabel * 0.35, lab, { size: st.fsLabel, anchor: 'middle', fill: ink, halo: isPat, tip: tip });
            }
          } else if (horiz) {
            if (tt < st.fsLabel * 0.95) continue;
            if (x1 - b2 >= lwid + 6 || segLen < lwid + 8) T(b2 + 4, t0 + tt / 2 + st.fsLabel * 0.35, lab, { size: st.fsLabel });
            else T(b2 - 4, t0 + tt / 2 + st.fsLabel * 0.35, lab, { size: st.fsLabel, anchor: 'end', fill: ink, halo: isPat });        // 先に空きがないときは棒の中に書く
          } else {
            var upright = tt >= lwid + 1, need = (upright ? st.fsLabel : lwid) + 6;
            if (!upright && tt < st.fsLabel * 0.95) continue;
            if (b2 - y0 >= need || segLen < need + 2) { if (upright) T(t0 + tt / 2, b2 - 4, lab, { size: st.fsLabel, anchor: 'middle' }); else T(t0 + tt / 2 + st.fsLabel * 0.35, b2 - 4, lab, { size: st.fsLabel, rot: -90 }); }
            else if (upright) T(t0 + tt / 2, b2 + st.fsLabel + 2, lab, { size: st.fsLabel, anchor: 'middle', fill: ink, halo: isPat });
            else T(t0 + tt / 2 + st.fsLabel * 0.35, b2 + 4, lab, { size: st.fsLabel, rot: -90, anchor: 'end', fill: ink, halo: isPat });
          }
        }
      }
      lines.forEach(function (pts, si) {                                         // 折れ線：点を線で結ぶ。点が多いときは印を省く
        var col = fillOf(colors[si] || '#000000'), lw = Math.max(1.5, st.lwAxis * 1.5);
        o.push('<path d="' + pts.map(function (q, i) { return (i ? 'L' : 'M') + r1(q[0]) + ' ' + r1(q[1]); }).join('') + '" fill="none" stroke="' + col + '" stroke-width="' + lw + '" stroke-linejoin="round" stroke-linecap="round"/>');
        if (pts.length <= 60) pts.forEach(function (q) { o.push('<circle cx="' + r1(q[0]) + '" cy="' + r1(q[1]) + '" r="' + r1(lw + 1.5) + '" fill="' + col + '" stroke="#FFFFFF" stroke-width="0.8" data-tip="' + esc(q[2]) + '"/>'); });
      });
      frameAndAxes(x0, y0, x1, y1);                                              // 軸は最後に描いて棒の上に重ねる
      if (horiz) { drawBottomTitle(bL, (x0 + x1) / 2, 'vTitle', vTitle); drawSideTitle(sL, (y0 + y1) / 2, 'cTitle', cTitle); }
      else { drawBottomTitle(bL, (x0 + x1) / 2, 'cTitle', cTitle); drawSideTitle(sL, (y0 + y1) / 2, 'vTitle', vTitle); }
    }
    function drawBox(b, b0, thick, mid, pos, horiz, name) {
      var s = b.stats, col = seriesColors(['box'], st)[0], w = Math.max(0.5, st.lwAxis), tip = name + '：中央値 ' + s[2] + '（第1四分位 ' + s[1] + '、第3四分位 ' + s[3] + '、n = ' + b.n + '）';
      var p = s.map(pos), c1 = b0 + thick * 0.25, c2 = b0 + thick * 0.75;
      function dot(cx, cy) { o.push('<circle cx="' + r1(cx) + '" cy="' + r1(cy) + '" r="2.5" fill="none" stroke="#000000" stroke-width="' + w + '"/>'); }
      if (horiz) {
        line(p[0], mid, p[1], mid, w); line(p[3], mid, p[4], mid, w); line(p[0], c1, p[0], c2, w); line(p[4], c1, p[4], c2, w);
        rect(p[1], b0, p[3] - p[1], thick, col, tip); line(p[2], b0, p[2], b0 + thick, Math.max(1.5, w * 2)); (b.out || []).forEach(function (v) { dot(pos(v), mid); });
      } else {
        line(mid, p[0], mid, p[1], w); line(mid, p[3], mid, p[4], w); line(c1, p[0], c2, p[0], w); line(c1, p[4], c2, p[4], w);
        rect(b0, p[3], thick, p[1] - p[3], col, tip); line(b0, p[2], b0 + thick, p[2], Math.max(1.5, w * 2)); (b.out || []).forEach(function (v) { dot(mid, pos(v)); });
      }
    }
    // 平均と 95% 信頼区間：点が平均、線が信頼区間（両端に短い横線）
    function drawMean(b, b0, thick, mid, pos, horiz, name) {
      if (!isNum(b.mean)) return;
      var col = seriesColors(['mean'], st)[0], w = Math.max(0.75, st.lwAxis), cap = Math.min(thick * 0.3, 10), ci = isNum(b.lo) && isNum(b.hi), r = Math.max(3.5, Math.min(7, thick * 0.12));
      var tip = name + '：' + (m.coef || '平均') + ' ' + fmtV(b.mean) + (ci ? '（95% 信頼区間 ' + fmtV(b.lo) + '～' + fmtV(b.hi) + '）' : '') + (b.n ? '、n = ' + b.n : '');
      var pm = pos(b.mean), fill = /^pat:/.test(col) ? '#000000' : col;
      if (ci) {
        var pl = pos(b.lo), ph = pos(b.hi);
        if (horiz) { line(pl, mid, ph, mid, w); line(pl, mid - cap, pl, mid + cap, w); line(ph, mid - cap, ph, mid + cap, w); }
        else { line(mid, pl, mid, ph, w); line(mid - cap, pl, mid + cap, pl, w); line(mid - cap, ph, mid + cap, ph, w); }
      }
      o.push('<circle cx="' + r1(horiz ? pm : mid) + '" cy="' + r1(horiz ? mid : pm) + '" r="' + r1(r) + '" fill="' + fill + '" stroke="#000000" stroke-width="' + w + '" data-tip="' + esc(tip) + '"/>');
      if (st.labels) { var t = fmtV(b.mean); if (horiz) T(pm, mid - r - 4, t, { size: st.fsLabel, anchor: 'middle' }); else T(mid + r + 4, pm + st.fsLabel * 0.35, t, { size: st.fsLabel }); }
    }
    function fmtV(v) { var big = (m.box || []).some(function (b) { return isNum(b.mean) && Math.abs(b.mean) >= 100; }); var s = v.toFixed(big ? 1 : 2); return /^-0\.?0*$/.test(s) ? s.slice(1) : s; }   // 図の中で小数の桁をそろえる
    // ---- 色分けの表（セルの値に応じて色を付け、カラーバーで対応を示す）
    function drawHeat() {
      var nr = m.cats.length, nc = m.series.length, rowTot = m.count.map(function (r) { return sum(r); }), colTot = m.series.map(function (_, j) { return sum(m.count.map(function (r) { return r[j]; })); });
      var by = st.heatBy === 'resid' && !m.resid ? 'row' : (st.heatBy || 'row'), lhT = st.fsTick * 1.25;
      var V = m.count.map(function (r, i) { return r.map(function (v, j) { return by === 'row' ? (rowTot[i] ? v / rowTot[i] * 100 : 0) : by === 'col' ? (colTot[j] ? v / colTot[j] * 100 : 0) : by === 'resid' ? m.resid[i][j] : v; }); });
      var flat = [].concat.apply([], V), lo = by === 'resid' ? -Math.max(2, Math.ceil(maxOf(flat.map(Math.abs)))) : 0, hiV = by === 'resid' ? -lo : (by === 'count' ? nice(0, maxOf(flat), 4).max : Math.min(100, nice(0, maxOf(flat), 4).max));
      function tOf(v) { return hiV > lo ? Math.max(0, Math.min(1, (v - lo) / (hiV - lo))) : 0; }
      cTitle = st.cTitle === null || st.cTitle === undefined ? (m.catTitle || '') : st.cTitle;
      vTitle = st.vTitle === null || st.vTitle === undefined ? (m.seriesTitle || '') : st.vTitle;
      var barLab = by === 'row' ? L.row : by === 'col' ? L.col : by === 'resid' ? L.resid : L.count, barOn = st.bar && st.bar !== 'none';
      if (barOn && st.bar === 'right') right += 16 + 6 + Math.max(tw(fmtTick(hiV), st.fsTick, st), tw(fmtTick(lo), st.fsTick, st)) + st.fsTick * 1.5 + 8;
      if (barOn && st.bar === 'bottom') bottom += 14 + 6 + st.fsTick * 2.6 + 6;
      var cw = Math.min(W * 0.28, maxOf(m.cats.map(function (c) { return tw(c, st.fsTick, st); })) + 2);
      var sL = aLines(cTitle, H - top - bottom - 60), x0 = left + aRes(sL) + cw + 8, x1 = W - right; if (x1 - x0 < 40) x1 = x0 + 40;
      var tL = aLines(vTitle, x1 - x0 + 20);
      var cellW = (x1 - x0) / nc, cl = m.series.map(function (s) { return clampLines(wrap(s, Math.max(cellW - 6, st.fsTick), st.fsTick, st), 4, Math.max(cellW - 6, st.fsTick), st.fsTick, st); }), hl = maxOf(cl.map(function (l) { return l.length; }));
      var y0 = top + aRes(tL) + 4 + hl * lhT + 4, y1 = H - bottom; if (y1 - y0 < 30) y1 = y0 + 30;
      var cellH = (y1 - y0) / nr, maxL = Math.max(1, Math.floor((cellH - 2) / lhT)), rl = m.cats.map(function (c) { return clampLines(wrap(c, cw, st.fsTick, st), maxL, cw, st.fsTick, st); });
      plot = { x0: x0, y0: y0, x1: x1, y1: y1 };
      drawTopTitle(tL, (x0 + x1) / 2, top, 'vTitle', vTitle); drawSideTitle(sL, (y0 + y1) / 2, 'cTitle', cTitle);
      cl.forEach(function (l, j) { TL(x0 + cellW * (j + 0.5), y0 - 6 - (l.length - 1) * lhT, l, { anchor: 'middle' }); });
      for (var i = 0; i < nr; i++) {
        TL(x0 - 8, y0 + cellH * (i + 0.5) - (rl[i].length - 1) * lhT / 2 + st.fsTick * 0.35, rl[i], { anchor: 'end' });
        for (var j = 0; j < nc; j++) {
          var fill = heatColor(st, by, tOf(V[i][j])), ink = inkOn(fill), cx = x0 + cellW * (j + 0.5), cy = y0 + cellH * (i + 0.5), cnt = m.count[i][j], pr = rowTot[i] ? cnt / rowTot[i] * 100 : 0;
          rect(x0 + cellW * j, y0 + cellH * i, cellW, cellH, fill, m.cats[i] + ' / ' + m.series[j] + '：' + cnt + '人（行の ' + pr.toFixed(1) + '%' + (m.resid ? '、残差 ' + m.resid[i][j].toFixed(2) : '') + '）');
          if (!st.labels) continue;
          var l1 = String(cnt), l2 = by === 'resid' ? m.resid[i][j].toFixed(2) : by === 'count' ? '' : '(' + V[i][j].toFixed(dec) + '%)';
          if (st.value === 'count' || !l2) { if (tw(l1, st.fsLabel, st) < cellW - 2 && cellH > st.fsLabel) T(cx, cy + st.fsLabel * 0.35, l1, { size: st.fsLabel, anchor: 'middle', fill: ink }); }
          else if (cellH > st.fsLabel * 2.6 && tw(l2, st.fsLabel, st) < cellW - 2) { T(cx, cy - 1, l1, { size: st.fsLabel, anchor: 'middle', fill: ink }); T(cx, cy + st.fsLabel * 1.15, l2, { size: st.fsLabel, anchor: 'middle', fill: ink }); }
          else if (cellH > st.fsLabel && tw(l2, st.fsLabel, st) < cellW - 2) T(cx, cy + st.fsLabel * 0.35, l2.replace(/[()]/g, ''), { size: st.fsLabel, anchor: 'middle', fill: ink });
        }
      }
      if (!barOn) return;
      // カラーバー（色と値の対応）。smooth：なめらかな帯、step：5 段階
      var steps = st.barShape === 'step' ? 5 : 24, vert = st.bar === 'right', bx, byy, bl, bt = 14, k, ticksV = by === 'resid' ? [lo, 0, hiV] : [lo, (lo + hiV) / 2, hiV];
      if (vert) { bx = x1 + 16; byy = y0; bl = Math.min(y1 - y0, 220); } else { bl = Math.min(x1 - x0, 260); bx = x0 + (x1 - x0 - bl) / 2; byy = y1 + 12; }
      o.push('<g data-cbar="1" data-tip="ドラッグでカラーバーを図の右か下に動かせます。">');
      o.push(vert ? '<rect x="' + r1(bx - 4) + '" y="' + r1(byy - 4) + '" width="' + r1(W - bx) + '" height="' + r1(bl + 8) + '" fill="#FFFFFF" fill-opacity="0"/>' : '<rect x="' + r1(bx - 4) + '" y="' + r1(byy - 4) + '" width="' + r1(bl + 8) + '" height="' + r1(H - byy + 4) + '" fill="#FFFFFF" fill-opacity="0"/>');
      for (k = 0; k < steps; k++) {
        var tc = (k + 0.5) / steps, f2 = heatColor(st, by, tc);
        if (vert) o.push('<rect x="' + r1(bx) + '" y="' + r1(byy + bl * (1 - (k + 1) / steps)) + '" width="' + bt + '" height="' + r1(bl / steps + 0.6) + '" fill="' + f2 + '"/>');
        else o.push('<rect x="' + r1(bx + bl * k / steps) + '" y="' + r1(byy) + '" width="' + r1(bl / steps + 0.6) + '" height="' + bt + '" fill="' + f2 + '"/>');
      }
      var bwL = Math.max(0.5, Math.min(1, st.lwAxis || 1));
      if (vert) {
        o.push('<rect x="' + r1(bx) + '" y="' + r1(byy) + '" width="' + bt + '" height="' + r1(bl) + '" fill="none" stroke="#000000" stroke-width="' + bwL + '"/>');
        ticksV.forEach(function (tv) { var yy = byy + bl * (1 - tOf(tv)); line(bx + bt, yy, bx + bt + 3, yy, bwL); T(bx + bt + 6, yy + st.fsTick * 0.35, fmtTick(tv), {}); });
        T(W - 8, byy + bl / 2, fitText0(barLab, bl + 30, st.fsTick, st), { anchor: 'middle', rot: 90 });
      } else {
        o.push('<rect x="' + r1(bx) + '" y="' + r1(byy) + '" width="' + r1(bl) + '" height="' + bt + '" fill="none" stroke="#000000" stroke-width="' + bwL + '"/>');
        ticksV.forEach(function (tv) { var xx = bx + bl * tOf(tv); line(xx, byy + bt, xx, byy + bt + 3, bwL); T(xx, byy + bt + 5 + st.fsTick * 0.85, fmtTick(tv), { anchor: 'middle' }); });
        T(bx + bl / 2, byy + bt + 5 + st.fsTick * 2.2, barLab, { anchor: 'middle' });
      }
      o.push('</g>');
    }
    function drawScatter() {
      var sx = nice(Math.min.apply(null, m.x), Math.max.apply(null, m.x), 5), sy = scaleFor(st, 0, 0, nice(Math.min.apply(null, m.y), Math.max.apply(null, m.y), 5));
      vTitle = st.vTitle === null || st.vTitle === undefined ? (m.yTitle || '') : st.vTitle; cTitle = st.cTitle === null || st.cTitle === undefined ? (m.xTitle || '') : st.cTitle;
      var tickW = maxOf(sy.ticks.map(function (t) { return tw(fmtTick(t), st.fsTick, st); }));
      var sL = aLines(vTitle, H - top - bottom - 60), bL = aLines(cTitle, W - left - right - 40);
      var x0 = left + aRes(sL) + tickW + tickPad + 4, x1 = W - right - 6, y0 = top + 6, y1 = H - bottom - (st.fsTick + tickPad + 4) - aRes(bL);
      if (x1 - x0 < 30) x1 = x0 + 30; if (y1 - y0 < 30) y1 = y0 + 30; plot = { x0: x0, y0: y0, x1: x1, y1: y1 };
      function px(v) { return x0 + (v - sx.min) / (sx.max - sx.min) * (x1 - x0); } function py(v) { return y1 - (v - sy.min) / (sy.max - sy.min) * (y1 - y0); }
      var showY = thin(sy.ticks, (y1 - y0) / Math.max(1, sy.ticks.length - 1), function () { return st.fsTick; }), xt = [];
      for (var v = sx.min; v <= sx.max + sx.step * 1e-6; v += sx.step) xt.push(Math.round(v * 1e6) / 1e6);
      var showX = thin(xt, (x1 - x0) / Math.max(1, xt.length - 1), function (t) { return tw(fmtTick(t), st.fsTick, st); });
      sy.ticks.forEach(function (t, i) { if (gridOn && t > sy.min) line(x0, py(t), x1, py(t), st.lwGrid, st.gridColor, gridDash); tick(x0, py(t), false); if (showY(i)) T(x0 - tickPad, py(t) + st.fsTick * 0.35, fmtTick(t), { anchor: 'end' }); });
      xt.forEach(function (t, i) { tick(px(t), y1, true); if (showX(i)) T(px(t), y1 + tickPad + st.fsTick * 0.85, fmtTick(t), { anchor: 'middle' }); });
      var col = fillOf(seriesColors(['pt'], st)[0]);
      m.x.forEach(function (xv, i) { o.push('<circle cx="' + r1(px(xv)) + '" cy="' + r1(py(m.y[i])) + '" r="3" fill="' + col + '" fill-opacity="0.55" stroke="#000000" stroke-width="' + Math.min(0.5, st.lwBar) + '"/>'); });
      if (m.fit && m.fit.length === 2) {                                         // 回帰直線（表示する範囲で切る）
        var xa = Math.min.apply(null, m.x), xb = Math.max.apply(null, m.x), ya = m.fit[0] + m.fit[1] * xa, yb = m.fit[0] + m.fit[1] * xb;
        var clip = function (yv) { return Math.max(sy.min, Math.min(sy.max, yv)); };
        if (Math.abs(m.fit[1]) > 1e-12) { var xa2 = (clip(ya) - m.fit[0]) / m.fit[1], xb2 = (clip(yb) - m.fit[0]) / m.fit[1]; xa = Math.max(xa, Math.min(xa2, xb2)); xb = Math.min(xb, Math.max(xa2, xb2)); }
        line(px(xa), py(m.fit[0] + m.fit[1] * xa), px(xb), py(m.fit[0] + m.fit[1] * xb), Math.max(1.2, st.lwAxis * 1.5), '#C00000');
      }
      frameAndAxes(x0, y0, x1, y1);
      drawBottomTitle(bL, (x0 + x1) / 2, 'cTitle', cTitle); drawSideTitle(sL, (y0 + y1) / 2, 'vTitle', vTitle);
    }
    // ---- ヒストグラム（横軸＝値、縦軸＝度数。グループがあるときは階級の中に並べる）
    function drawHist() {
      var E = m.edges, K = E.length - 1, ns = Math.max(1, m.series.length), C = m.counts;
      var maxC = 0; for (var i = 0; i < K; i++) for (var j = 0; j < ns; j++) maxC = Math.max(maxC, C[i][j] || 0);
      var sy = scaleFor(st, 0, 0, nice(0, Math.max(1, maxC), 5));
      vTitle = st.vTitle === null || st.vTitle === undefined ? (lang === 'en' ? 'Frequency' : '度数') : st.vTitle;
      cTitle = st.cTitle === null || st.cTitle === undefined ? (m.xTitle || '') : st.cTitle;
      var tickW = maxOf(sy.ticks.map(function (t) { return tw(fmtTick(t), st.fsTick, st); }));
      var sL = aLines(vTitle, H - top - bottom - 60), bL = aLines(cTitle, W - left - right - 40);
      var x0 = left + aRes(sL) + tickW + tickPad + 4, x1 = W - right - 6, y0 = top + 6 + (st.labels ? st.fsLabel + 2 : 0), y1 = H - bottom - (st.fsTick + tickPad + 4) - aRes(bL);
      if (x1 - x0 < 30) x1 = x0 + 30; if (y1 - y0 < 30) y1 = y0 + 30; plot = { x0: x0, y0: y0, x1: x1, y1: y1 };
      function px(v) { return x0 + (v - E[0]) / (E[K] - E[0]) * (x1 - x0); } function py(v) { return y1 - (v - sy.min) / (sy.max - sy.min) * (y1 - y0); }
      var showY = thin(sy.ticks, (y1 - y0) / Math.max(1, sy.ticks.length - 1), function () { return st.fsTick; });
      sy.ticks.forEach(function (t, i2) { if (gridOn && t > sy.min) line(x0, py(t), x1, py(t), st.lwGrid, st.gridColor, gridDash); tick(x0, py(t), false); if (showY(i2)) T(x0 - tickPad, py(t) + st.fsTick * 0.35, fmtTick(t), { anchor: 'end' }); });
      var xt = [];
      if (m.intBins) for (var b = 0; b < K; b++) xt.push(E[b] + 0.5); else xt = E.slice();
      var showX = thin(xt, (x1 - x0) / Math.max(1, xt.length - 1), function (t) { return tw(fmtTick(t), st.fsTick, st) + 6; });
      xt.forEach(function (t, i2) { tick(px(t), y1, true); if (showX(i2)) T(px(t), y1 + tickPad + st.fsTick * 0.85, fmtTick(t), { anchor: 'middle' }); });
      var bwFrac = ns > 1 ? Math.max(10, Math.min(100, +st.barW || 70)) / 100 : 1;
      for (var k = 0; k < K; k++) {
        var xa = px(E[k]), xb = px(E[k + 1]), wBin = xb - xa, inner = wBin * bwFrac, off = (wBin - inner) / 2, sw = inner / ns;
        for (var s2 = 0; s2 < ns; s2++) {
          var c = C[k][s2] || 0; if (!c) continue;
          var lab = (m.intBins ? fmtTick(E[k] + 0.5) : fmtTick(E[k]) + '〜' + fmtTick(E[k + 1])) + (ns > 1 ? '（' + m.series[s2] + '）' : '') + '：' + c;
          rect(xa + off + sw * s2, py(c), sw, py(0) - py(c), colors[s2], lab);
          if (st.labels && ns === 1 && sw >= tw(String(c), st.fsLabel, st) * 0.9) T(xa + off + sw * (s2 + 0.5), py(c) - 3, String(c), { size: st.fsLabel, anchor: 'middle' });
        }
      }
      frameAndAxes(x0, y0, x1, y1);
      drawBottomTitle(bL, (x0 + x1) / 2, 'cTitle', cTitle); drawSideTitle(sL, (y0 + y1) / 2, 'vTitle', vTitle);
    }
    // ---- ワードクラウド：多く出た語ほど大きく。中心からうずまき状に、重ならない場所を探して置く
    function drawCloud() {
      var N = Math.min(m.words.length, Math.max(10, +st.topWords || 60)), mx = Math.max.apply(null, m.n.slice(0, N)), mn = Math.min.apply(null, m.n.slice(0, N));
      var big = Math.max(18, Math.min(W, H) / 7), small = Math.max(9, st.fsTick - 1), placed = [], cx = W / 2, cy = H / 2, pal = seriesColors(m.words.slice(0, Math.min(N, 8)), Object.assign({}, st, { palette: st.palette === 'gray' || st.palette === 'mono' ? 'npg' : st.palette, colors: {} }));
      plot = { x0: 6, y0: 6, x1: W - 6, y1: H - 6 };
      for (var i = 0; i < N; i++) {
        var t = mx > mn ? Math.sqrt((m.n[i] - mn) / (mx - mn)) : 1, fs = small + (big - small) * t, w = tw(m.words[i], fs, st) + 2, h = fs * 1.05, done = false;
        for (var k = 0; k < 2200 && !done; k++) {
          var a = k * 0.21, r = 2.2 * a, x = cx + r * Math.cos(a) * (W / H) - w / 2, y = cy + r * Math.sin(a) + h * 0.35;
          var b = [x, y - h * 0.85, x + w, y + h * 0.2];
          if (b[0] < 4 || b[2] > W - 4 || b[1] < 4 || b[3] > H - 4) continue;
          if (placed.some(function (q) { return b[0] < q[2] && b[2] > q[0] && b[1] < q[3] && b[3] > q[1]; })) continue;
          placed.push(b); done = true;
          var col = pal[i % pal.length]; if (/^pat:/.test(col)) col = '#333333';
          T(x + 1, y, m.words[i], { size: Math.round(fs * 10) / 10, fill: col, bold: t > 0.45, tip: m.words[i] + '：' + m.n[i] + ' 件の回答' });
        }
      }
    }
    // ---- 共起ネットワーク：円の大きさ＝その語が出た回答数、線の太さ＝一緒に出る強さ（Jaccard 係数）、色＝まとまり
    function drawNet() {
      var nd = m.nodes, ed = m.edges; if (!nd.length) return;
      var xs = nd.map(function (p) { return p.x; }), ys = nd.map(function (p) { return p.y; }), x0 = Math.min.apply(null, xs), x1 = Math.max.apply(null, xs), y0 = Math.min.apply(null, ys), y1 = Math.max.apply(null, ys);
      var mx = Math.max.apply(null, nd.map(function (p) { return p.n; })), rMax = Math.max(10, Math.min(W, H) / 16), pad = rMax + st.fsTick * 2.5;
      var ux = (W - pad * 2) / Math.max(1e-6, x1 - x0), uy = (H - pad * 2) / Math.max(1e-6, y1 - y0);
      var px = function (v) { return W / 2 + (v - (x0 + x1) / 2) * ux; }, py = function (v) { return H / 2 + (v - (y0 + y1) / 2) * uy; };
      plot = { x0: 4, y0: 4, x1: W - 4, y1: H - 4 };
      var nc = Math.max.apply(null, nd.map(function (p) { return p.c; })), pal = seriesColors(new Array(Math.min(nc, 8)).fill(0).map(function (_, i) { return 'c' + i; }), Object.assign({}, st, { palette: st.palette === 'gray' || st.palette === 'mono' ? 'npg' : st.palette, colors: {} }));
      var wmax = Math.max.apply(null, ed.map(function (e) { return e.w; }).concat([1e-6]));
      // 語の名前どうしが重ならないように、近すぎる点を少しずつ押し離す（図の枠の中に収める）
      var P = nd.map(function (p) { var r = 4 + (rMax - 4) * Math.sqrt(p.n / mx); return { x: px(p.x), y: py(p.y), w: Math.max(r * 2, tw(p.label, st.fsTick, st) + 6), h: Math.max(r * 2, st.fsTick + 6) }; });
      for (var it = 0; it < 240; it++) {
        var moved = false;
        for (var i = 0; i < P.length; i++) for (var j = i + 1; j < P.length; j++) {
          var a = P[i], b = P[j], ox = (a.w + b.w) / 2 - Math.abs(a.x - b.x), oy = (a.h + b.h) / 2 - Math.abs(a.y - b.y);
          if (ox > 0 && oy > 0) { moved = true; if (ox < oy * 1.6) { var sx = (a.x <= b.x ? -1 : 1) * ox / 2; a.x += sx; b.x -= sx; } else { var sy = (a.y <= b.y ? -1 : 1) * oy / 2; a.y += sy; b.y -= sy; } }
        }
        P.forEach(function (q) { q.x = Math.max(q.w / 2 + 4, Math.min(W - q.w / 2 - 4, q.x)); q.y = Math.max(q.h / 2 + 4, Math.min(H - q.h / 2 - 4, q.y)); });
        if (!moved) break;
      }
      nd.forEach(function (p, i) { p.X = P[i].x; p.Y = P[i].y; });
      px = function (v) { return v; }; py = px;
      nd = nd.map(function (p) { return Object.assign({}, p, { x: p.X, y: p.Y }); });
      ed.forEach(function (e) { var a = nd[e.a], b = nd[e.b]; line(px(a.x), py(a.y), px(b.x), py(b.y), 0.6 + 2.4 * e.w / wmax, '#9E9E9E'); });
      nd.forEach(function (p) {
        var r = 4 + (rMax - 4) * Math.sqrt(p.n / mx), col = p.c <= 8 ? pal[(p.c - 1) % pal.length] : '#D9D9D9'; if (/^pat:/.test(col)) col = '#BDBDBD';
        o.push('<circle cx="' + r1(px(p.x)) + '" cy="' + r1(py(p.y)) + '" r="' + r1(r) + '" fill="' + col + '" fill-opacity="0.85" stroke="#FFFFFF" stroke-width="1" data-tip="' + esc(p.label + '：' + p.n + ' 件の回答') + '"/>');
      });
      nd.forEach(function (p) { T(px(p.x), py(p.y) + st.fsTick * 0.35, p.label, { size: st.fsTick, anchor: 'middle', halo: true }); });
    }
    // ---- 布置図（コレスポンデンス分析）：縦横を同じ縮尺にして、行と列の選択肢を点と名前で示す
    function drawMap() {
      var xs = [], ys = [];
      m.pts.forEach(function (p) { xs = xs.concat(p.x); ys = ys.concat(p.y); });
      var ax = Math.max.apply(null, xs.map(Math.abs).concat([1e-6])), ay = Math.max.apply(null, ys.map(Math.abs).concat([1e-6]));
      var sx = nice(-ax * 1.15, ax * 1.15, 4), sy2 = nice(-ay * 1.15, ay * 1.15, 4);
      function tk(sc) { var t = []; for (var v = sc.min; v <= sc.max + sc.step * 1e-6; v += sc.step) t.push(Math.round(v * 1e6) / 1e6); return t; }
      sx.ticks = tk(sx); sy2.ticks = tk(sy2);
      cTitle = st.cTitle === null || st.cTitle === undefined ? (m.xTitle || '') : st.cTitle; vTitle = st.vTitle === null || st.vTitle === undefined ? (m.yTitle || '') : st.vTitle;
      var tickW = maxOf(sy2.ticks.map(function (t) { return tw(fmtTick(t), st.fsTick, st); }));
      var sL = aLines(vTitle, H - top - bottom - 60), bL = aLines(cTitle, W - left - right - 40);
      var X0 = left + aRes(sL) + tickW + tickPad + 4, X1 = W - right - 6, Y0 = top + 6, Y1 = H - bottom - (st.fsTick + tickPad + 4) - aRes(bL);
      if (X1 - X0 < 40) X1 = X0 + 40; if (Y1 - Y0 < 40) Y1 = Y0 + 40;
      var u = Math.min((X1 - X0) / (sx.max - sx.min), (Y1 - Y0) / (sy2.max - sy2.min));       // 縦横同じ縮尺
      var cx = (X0 + X1) / 2, cy = (Y0 + Y1) / 2, mx = (sx.max + sx.min) / 2, my = (sy2.max + sy2.min) / 2;
      var x0 = cx - (sx.max - sx.min) / 2 * u, x1 = cx + (sx.max - sx.min) / 2 * u, y0 = cy - (sy2.max - sy2.min) / 2 * u, y1 = cy + (sy2.max - sy2.min) / 2 * u;
      plot = { x0: x0, y0: y0, x1: x1, y1: y1 };
      function px(v) { return cx + (v - mx) * u; } function py(v) { return cy - (v - my) * u; }
      sy2.ticks.forEach(function (t) { tick(x0, py(t), false); T(x0 - tickPad, py(t) + st.fsTick * 0.35, fmtTick(t), { anchor: 'end' }); });
      sx.ticks.forEach(function (t) { tick(px(t), y1, true); T(px(t), y1 + tickPad + st.fsTick * 0.85, fmtTick(t), { anchor: 'middle' }); });
      line(px(0), y0, px(0), y1, Math.max(0.5, st.lwGrid), '#9E9E9E', '4 3'); line(x0, py(0), x1, py(0), Math.max(0.5, st.lwGrid), '#9E9E9E', '4 3');
      var boxes = [], fsP = st.fsLabel + 1;
      function free(b) { if (b[0] < x0 || b[2] > x1 || b[1] < y0 || b[3] > y1) return false; return !boxes.some(function (q) { return b[0] < q[2] && b[2] > q[0] && b[1] < q[3] && b[3] > q[1]; }); }
      m.pts.forEach(function (p, si) {
        var col = fillOf(colors[si] || '#000000');
        p.x.forEach(function (xv, i) {
          var X = px(xv), Y = py(p.y[i]), r = 4;
          if (si === 0) o.push('<circle cx="' + r1(X) + '" cy="' + r1(Y) + '" r="' + r + '" fill="' + col + '" stroke="#000000" stroke-width="0.6" data-tip="' + esc(p.labels[i]) + '"/>');
          else o.push('<path d="M' + r1(X) + ' ' + r1(Y - r - 1) + 'L' + r1(X + r + 1) + ' ' + r1(Y + r) + 'L' + r1(X - r - 1) + ' ' + r1(Y + r) + 'Z" fill="' + col + '" stroke="#000000" stroke-width="0.6" data-tip="' + esc(p.labels[i]) + '"/>');
          boxes.push([X - r, Y - r, X + r, Y + r]);
        });
      });
      m.pts.forEach(function (p, si) {
        p.x.forEach(function (xv, i) {
          var X = px(xv), Y = py(p.y[i]), s = p.labels[i], w = tw(s, fsP, st), h = fsP;
          var cand = [[X + 7, Y + h * 0.35, 'start'], [X - 7, Y + h * 0.35, 'end'], [X, Y - 8, 'middle'], [X, Y + h + 6, 'middle'], [X + 7, Y - 6, 'start'], [X + 7, Y + h + 2, 'start'], [X - 7, Y - 6, 'end'], [X - 7, Y + h + 2, 'end']];
          var pick = cand[0];
          for (var c = 0; c < cand.length; c++) { var q = cand[c], bx = q[2] === 'start' ? q[0] : q[2] === 'end' ? q[0] - w : q[0] - w / 2, b = [bx - 1, q[1] - h, bx + w + 1, q[1] + 2]; if (free(b)) { pick = q; break; } }
          var bx2 = pick[2] === 'start' ? pick[0] : pick[2] === 'end' ? pick[0] - w : pick[0] - w / 2; boxes.push([bx2 - 1, pick[1] - h, bx2 + w + 1, pick[1] + 2]);
          T(pick[0], pick[1], s, { size: fsP, anchor: pick[2], bold: si === 0, halo: true });
        });
      });
      frameAndAxes(x0, y0, x1, y1);
      drawBottomTitle(bL, (x0 + x1) / 2, 'cTitle', cTitle); drawSideTitle(sL, (y0 + y1) / 2, 'vTitle', vTitle);
    }
  }

  // ---- PNG にする。scale は解像度の倍率。画面と同じ大きさで貼り付けられるよう、解像度（dpi）の情報を書き込む
  var crcT = null;
  function crc32(u8, crc) { if (!crcT) { crcT = new Uint32Array(256); for (var n = 0; n < 256; n++) { var c = n; for (var k = 0; k < 8; k++) c = c & 1 ? 0xEDB88320 ^ (c >>> 1) : c >>> 1; crcT[n] = c >>> 0; } } crc = (crc === undefined ? 0 : crc) ^ 0xFFFFFFFF; for (var i = 0; i < u8.length; i++) crc = crcT[(crc ^ u8[i]) & 0xFF] ^ (crc >>> 8); return (crc ^ 0xFFFFFFFF) >>> 0; }
  function withDpi(u8, dpi) {                                                     // IHDR の直後に pHYs を入れる
    var ppm = Math.round(dpi / 0.0254), ch = new Uint8Array(21), dv = new DataView(ch.buffer);
    dv.setUint32(0, 9); ch.set([112, 72, 89, 115], 4); dv.setUint32(8, ppm); dv.setUint32(12, ppm); ch[16] = 1; dv.setUint32(17, crc32(ch.subarray(4, 17)));
    var out = new Uint8Array(u8.length + 21); out.set(u8.subarray(0, 33), 0); out.set(ch, 33); out.set(u8.subarray(33), 54); return out;
  }
  function toPng(svg, scale) {
    return new Promise(function (resolve, reject) {
      var img = new Image(), url = URL.createObjectURL(new Blob([svg], { type: 'image/svg+xml;charset=utf-8' }));
      img.onload = function () {
        var c = document.createElement('canvas'); c.width = Math.round(img.width * scale); c.height = Math.round(img.height * scale);
        var g = c.getContext('2d'); g.scale(scale, scale); g.drawImage(img, 0, 0); URL.revokeObjectURL(url);
        c.toBlob(function (b) { if (!b) return reject(new Error('画像にできませんでした。')); b.arrayBuffer().then(function (ab) { resolve(new Blob([withDpi(new Uint8Array(ab), 96 * scale)], { type: 'image/png' })); }); }, 'image/png');
      };
      img.onerror = function () { URL.revokeObjectURL(url); reject(new Error('画像にできませんでした。')); };
      img.src = url;
    });
  }
  window.Chart = { colorMode: colorMode, colorKeys: colorKeys, defaults: defaults, model: model, kinds: kinds, kindOf: kindOf, seriesFor: seriesFor, render: render, toPng: toPng, seriesColors: seriesColors, swatches: swatches, heatColor: heatColor, PALETTES: PALETTES, KIND_NAMES: KIND_NAMES, lerp: lerp, crc32: crc32, inkOn: inkOn };
})();
