// pdfreport.js ― 報告書を PDF にする（図・表・報告書の文をすべて含める）。
//   ・本文と表は、和文を MS 明朝、欧文・数字を Times New Roman で書く。見出しと図はゴシック（BIZ UDPゴシック）
//     MS 明朝と Times New Roman は配布できない字体なので、端末に入っているものを使う（パソコンの Chrome・Edge で、字体の使用を許可したとき）。
//     使えないとき（スマートフォンなど）は、形と幅の近い BIZ UD明朝・Tinos（どちらも SIL Open Font License）で代わりに書く
//   ・使った文字だけを PDF に埋め込むので、ファイルは小さい
//   ・図と表は、画面と同じ SVG を線と文字のまま PDF に描く（拡大しても粗くならない）。PDF 用に、図と表の字形を埋め込む字体にそろえて組み直す
//   ・使うときに初めて部品（pdf-lib・fontkit・字体）を読み込む
(function () {
  'use strict';
  var A = window.App, S = A.S;
  var FAMILY = 'CTSReportFont', loaded = null;
  var PW = 595.28, PH = 841.89, M = 56, CW = PW - M * 2, FOOT = 36;          // A4 縦、余白 約 20 mm
  var NOEND = '（「『【〔〈《([{“‘';
  var NOSTART = '、。，．・：；？！）」』】〕〉》’”ーぁぃぅぇぉっゃゅょゎァィゥェォッャュョヮ…％,.:;?!)]}';

  // 使った文字を集める（1 回目の組版）。2 回目は、集めた文字だけの字体（harfbuzz で作る）を埋め込む
  //   used：字体ごとの「使った文字」（1 回目だけ集める。1 回目は文字を描かない）
  var used = null;
  function dt(pg, str, o) {
    if (used) { var k = o.font.key, u = used[k] || (used[k] = new Set()); Array.from(str).forEach(function (c) { u.add(c); }); return; }
    if (o.fakeBold) {                                              // 明朝には太字がないので、輪郭を少し太らせて太字に見せる（Word と同じ方法）
      pg.pushOperators(PDFLib.pushGraphicsState(), PDFLib.setTextRenderingMode(PDFLib.TextRenderingMode.FillAndOutline), PDFLib.setLineWidth(o.size * 0.035), PDFLib.setStrokingColor(o.color || PDFLib.rgb(0, 0, 0)));
      pg.drawText(str, o); pg.pushOperators(PDFLib.popGraphicsState()); return;
    }
    pg.drawText(str, o);
  }
  var hb = null;
  async function hbSubset(fontBuf, chars, index) {
    if (!hb) { var mod = await WebAssembly.instantiate(await getBuf('vendor/harfbuzz-subset.wasm'), {}); hb = mod.instance.exports; }
    var X = hb, f = new Uint8Array(fontBuf), p = X.malloc(f.byteLength);
    new Uint8Array(X.memory.buffer).set(f, p);
    var blob = X.hb_blob_create(p, f.byteLength, 2, 0, 0), face = X.hb_face_create(blob, index || 0); X.hb_blob_destroy(blob);
    var inp = X.hb_subset_input_create_or_fail(), us = X.hb_subset_input_unicode_set(inp);
    chars.forEach(function (c) { X.hb_set_add(us, c.codePointAt(0)); });
    var sub = X.hb_subset_or_fail(face, inp); X.hb_subset_input_destroy(inp);
    if (!sub) { X.hb_face_destroy(face); X.free(p); throw new Error(_t('字体を小さくできませんでした。')); }
    var rb = X.hb_face_reference_blob(sub), off = X.hb_blob_get_data(rb, 0), len = X.hb_blob_get_length(rb);
    var out = new Uint8Array(X.memory.buffer).slice(off, off + len);
    X.hb_blob_destroy(rb); X.hb_face_destroy(sub); X.hb_face_destroy(face); X.free(p);
    return out;
  }
  function loadScript(src) {
    return new Promise(function (res, rej) { var s = document.createElement('script'); s.src = src; s.onload = res; s.onerror = function () { rej(new Error(src + _t(' を読み込めませんでした。'))); }; document.head.appendChild(s); });
  }
  async function getBuf(url) { var r = await fetch(url); if (!r.ok) throw new Error(url + _t(' を読み込めませんでした（{0}）。', r.status)); return r.arrayBuffer(); }
  // TTC（複数の字体をまとめたファイル。MS 明朝など）から、PostScript 名の合う字体の番号を探す
  function ttcIndex(b, ps) {
    var dv = new DataView(b.buffer, b.byteOffset, b.byteLength);
    if (b.length < 12 || String.fromCharCode(b[0], b[1], b[2], b[3]) !== 'ttcf') return 0;
    var n = dv.getUint32(8);
    for (var i = 0; i < n; i++) {
      var off = dv.getUint32(12 + i * 4), nt = dv.getUint16(off + 4);
      for (var t = 0; t < nt; t++) {
        var r = off + 12 + t * 16; if (String.fromCharCode(b[r], b[r + 1], b[r + 2], b[r + 3]) !== 'name') continue;
        var no = dv.getUint32(r + 8), cnt = dv.getUint16(no + 2), so = no + dv.getUint16(no + 4);
        for (var k = 0; k < cnt; k++) {
          var q = no + 6 + k * 12, pid = dv.getUint16(q), nid = dv.getUint16(q + 6), len = dv.getUint16(q + 8), o = so + dv.getUint16(q + 10), str = '';
          if (nid !== 6) continue;
          if (pid === 3 || pid === 0) for (var j = 0; j < len; j += 2) str += String.fromCharCode(dv.getUint16(o + j)); else for (var j2 = 0; j2 < len; j2++) str += String.fromCharCode(b[o + j2]);
          if (str === ps) return i;
        }
      }
    }
    return 0;
  }
  // 端末に入っている MS 明朝・Times New Roman を読む（ボタンを押した直後に呼ぶ。許可を求める画面が出ることがある）
  var PS = { mj: 'MS-Mincho', tr: 'TimesNewRomanPSMT', tb: 'TimesNewRomanPS-BoldMT' }, localP = null;
  function askLocalFonts() {
    if (localP) return localP;
    localP = (async function () {
      if (!window.queryLocalFonts) return null;
      try {
        var list = await window.queryLocalFonts({ postscriptNames: [PS.mj, PS.tr, PS.tb] }), by = {};
        list.forEach(function (f) { by[f.postscriptName] = f; });
        if (!by[PS.mj] || !by[PS.tr]) return null;
        var out = {};
        for (var k in PS) if (by[PS[k]]) { var b = new Uint8Array(await (await by[PS[k]].blob()).arrayBuffer()); out[k] = { buf: b, index: ttcIndex(b, PS[k]), ps: PS[k] }; }
        return out;
      } catch (e) { return null; }
    })();
    localP.then(function (r) { if (!r) localP = null; });   // 断られたときは、次に押したときにもう一度たずねる
    return localP;
  }
  function fk(bytes, ps, index) {
    var f = null; try { f = ps ? window.fontkit.create(bytes, ps) : null; } catch (e) {}
    if (!f) f = window.fontkit.create(bytes);
    return f.fonts ? f.fonts[index || 0] : f;
  }
  async function addFace(name, src, weight) { var f = new FontFace(name, src, { weight: weight }); await f.load(); document.fonts.add(f); }
  async function ensureLibs() {
    if (!window.PDFLib) await loadScript('vendor/pdf-lib.min.js');
    if (!window.fontkit) await loadScript('vendor/fontkit.umd.min.js');
    var loc = await askLocalFonts();
    var key = loc ? 'local' : 'free';
    if (loaded && loaded.key === key) return loaded;
    var L = { key: key, local: !!loc, src: {} };
    if (!loaded || !loaded.src.g) {
      var reg = new Uint8Array(await getBuf('fonts/BIZUDPGothic-Regular.ttf')), bold = new Uint8Array(await getBuf('fonts/BIZUDPGothic-Bold.ttf'));
      // 図と表の文字の幅を、PDF に入れる字体で測るため、画面にも同じ字体を登録する
      await addFace(FAMILY, reg.slice(0).buffer, '400'); await addFace(FAMILY, bold.slice(0).buffer, '700');
      L.src.g = { buf: reg, index: 0 }; L.src.gb = { buf: bold, index: 0 };
    } else { L.src.g = loaded.src.g; L.src.gb = loaded.src.gb; }
    if (loc) {
      L.src.mj = loc.mj; L.src.tr = loc.tr; L.src.tb = loc.tb || loc.tr;
      // 画面で幅を測るための登録。名前で見つからないときは、読んだファイルの中身で登録する
      async function addLocal(name, src, x, w) { try { await addFace(name, src, w); } catch (e) { await addFace(name, x.buf.slice(0).buffer, w); } }
      await addLocal('CTSMincho', "local('MS Mincho'), local('MS-Mincho')", loc.mj, '400');
      await addLocal('CTSTimes', "local('Times New Roman'), local('TimesNewRomanPSMT')", loc.tr, '400');
      await addLocal('CTSTimes', loc.tb ? "local('Times New Roman Bold'), local('TimesNewRomanPS-BoldMT')" : "local('Times New Roman'), local('TimesNewRomanPSMT')", loc.tb || loc.tr, '700');
    } else {
      var mj = new Uint8Array(await getBuf('fonts/BIZUDMincho-Regular.ttf')), tr = new Uint8Array(await getBuf('fonts/Tinos-Regular.ttf')), tb = new Uint8Array(await getBuf('fonts/Tinos-Bold.ttf'));
      L.src.mj = { buf: mj, index: 0 }; L.src.tr = { buf: tr, index: 0 }; L.src.tb = { buf: tb, index: 0 };
      await addFace('CTSMincho', mj.slice(0).buffer, '400'); await addFace('CTSTimes', tr.slice(0).buffer, '400'); await addFace('CTSTimes', tb.slice(0).buffer, '700');
    }
    // 1 回目（文字を集めるだけ）は、元の字体で幅を測る
    L.full = {};
    Object.keys(L.src).forEach(function (k) { var x = L.src[k]; L.full[k] = fk(x.buf, x.ps, x.index); });
    loaded = L; return loaded;
  }
  // 字体の幅を測る部品（1 回目用）。pdf-lib の字体と同じ形（widthOfTextAtSize）にそろえる
  function measureFont(key, f) { return { key: key, fk: f, widthOfTextAtSize: function (str, size) { return f.layout(String(str)).advanceWidth / f.unitsPerEm * size; } }; }
  // 和文と欧文で字体を分けて書く。欧文の字体にある文字（全角の記号と和文を除く）は欧文の字体で書く
  function Mixed(J, Lf, latinHas, fakeBold) {
    function isLat(ch) { var cp = ch.codePointAt(0); return cp < 0x3000 && !(cp >= 0xFF00 && cp <= 0xFFEF) && latinHas(cp); }
    function runs(str) { var out = [], cur = '', lat = null; Array.from(String(str)).forEach(function (ch) { var l = isLat(ch); if (cur && l !== lat) { out.push([cur, lat]); cur = ''; } cur += ch; lat = l; }); if (cur) out.push([cur, lat]); return out; }
    return {
      mixed: true,
      widthOfTextAtSize: function (str, size) { return runs(str).reduce(function (a, r) { return a + (r[1] ? Lf : J).widthOfTextAtSize(r[0], size); }, 0); },
      draw: function (pg, str, o) {
        var ang = o.rotate ? o.rotate.angle * Math.PI / 180 : 0, ca = Math.cos(ang), sa = Math.sin(ang), x = o.x, y = o.y;
        runs(str).forEach(function (r) {
          var f = r[1] ? Lf : J; dt(pg, r[0], Object.assign({}, o, { x: x, y: y, font: f, fakeBold: !r[1] && fakeBold }));
          var w = f.widthOfTextAtSize(r[0], o.size); x += w * ca; y += w * sa;
        });
      }
    };
  }
  function Single(f, fakeBold) { return { widthOfTextAtSize: function (str, size) { return f.widthOfTextAtSize(str, size); }, draw: function (pg, str, o) { dt(pg, str, Object.assign({}, o, { font: f, fakeBold: fakeBold })); } }; }
  // 書体の組（G：ゴシック、GB：ゴシックの太字、S：本文の明朝＋Times、SB：その太字）
  function fontSet(F, latinHas) { return { G: Single(F.g), GB: Single(F.gb), S: Mixed(F.mj, F.tr, latinHas), SB: Mixed(F.mj, F.tb, latinHas, true) }; }

  // ---------------- 色
  var NAMED = { white: '#FFFFFF', black: '#000000', gray: '#808080', grey: '#808080', red: '#FF0000' };
  function col(c) {
    if (!c || c === 'none' || c === 'transparent') return null;
    c = String(c).trim(); if (NAMED[c.toLowerCase()]) c = NAMED[c.toLowerCase()];
    var m = /^rgba?\(([^)]+)\)$/i.exec(c);
    if (m) { var v = m[1].split(',').map(parseFloat); return PDFLib.rgb(v[0] / 255, v[1] / 255, v[2] / 255); }
    if (/^#[0-9a-f]{3}$/i.test(c)) c = '#' + c[1] + c[1] + c[2] + c[2] + c[3] + c[3];
    if (!/^#[0-9a-f]{6}$/i.test(c)) return PDFLib.rgb(0, 0, 0);
    return PDFLib.rgb(parseInt(c.substr(1, 2), 16) / 255, parseInt(c.substr(3, 2), 16) / 255, parseInt(c.substr(5, 2), 16) / 255);
  }
  function hexOf(c) { if (NAMED[String(c).toLowerCase()]) return NAMED[String(c).toLowerCase()]; if (/^#[0-9a-f]{3}$/i.test(c)) return '#' + c[1] + c[1] + c[2] + c[2] + c[3] + c[3]; return c; }

  // ---------------- 変換行列（SVG の座標 → SVG の座標）
  function mul(a, b) { return [a[0] * b[0] + a[2] * b[1], a[1] * b[0] + a[3] * b[1], a[0] * b[2] + a[2] * b[3], a[1] * b[2] + a[3] * b[3], a[0] * b[4] + a[2] * b[5] + a[4], a[1] * b[4] + a[3] * b[5] + a[5]]; }
  function parseTf(s) {
    var m = [1, 0, 0, 1, 0, 0], re = /(translate|rotate|scale|matrix)\s*\(([^)]*)\)/g, r;
    while ((r = re.exec(s || ''))) {
      var v = r[2].split(/[\s,]+/).filter(Boolean).map(Number), t;
      if (r[1] === 'translate') t = [1, 0, 0, 1, v[0] || 0, v[1] || 0];
      else if (r[1] === 'scale') t = [v[0], 0, 0, v.length > 1 ? v[1] : v[0], 0, 0];
      else if (r[1] === 'matrix') t = v;
      else { var a = (v[0] || 0) * Math.PI / 180, c = Math.cos(a), sn = Math.sin(a), cx = v[1] || 0, cy = v[2] || 0; t = mul(mul([1, 0, 0, 1, cx, cy], [c, sn, -sn, c, 0, 0]), [1, 0, 0, 1, -cx, -cy]); }
      m = mul(m, t);
    }
    return m;
  }
  function ap(m, x, y) { return [m[0] * x + m[2] * y + m[4], m[1] * x + m[3] * y + m[5]]; }

  // ---------------- 文書を組む
  function Builder(doc, FS) {
    var B = this, page = null, y = 0;
    B.pages = [];
    B.newPage = function () { page = doc.addPage([PW, PH]); B.pages.push(page); y = M; };
    B.room = function () { return PH - FOOT - M * 0.5 - y; };
    B.need = function (h) { if (!page || y + h > PH - FOOT - M * 0.5) B.newPage(); };
    B.gap = function (h) { y += h; };
    // 文を幅で折り返す。英数字の続き（p = 0.012 など）は途中で切らない
    function units(str) { return str.match(/[A-Za-z0-9.,=<>≦≧±+\-−×/%'()\[\]_:]+|\s+|[\s\S]/gu) || []; }
    function wrap(str, font, size, maxW) {
      var out = [];
      String(str).split(/\r?\n/).forEach(function (para) {
        var cur = '', us = units(para);
        for (var i = 0; i < us.length; i++) {
          var u = us[i];
          if (font.widthOfTextAtSize(cur + u, size) <= maxW || !cur) {
            if (!cur && font.widthOfTextAtSize(u, size) > maxW) {           // 1 つの塊が幅を超えるときは文字で切る
              Array.from(u).forEach(function (ch) { if (cur && font.widthOfTextAtSize(cur + ch, size) > maxW) { out.push(cur); cur = ch; } else cur += ch; });
            } else cur += u;
            continue;
          }
          if (NOSTART.indexOf(u[0]) >= 0 && u.length === 1) { out.push(cur + u); cur = ''; continue; }   // 句読点は前の行に付ける
          var carry = '';
          while (cur.length > 1 && NOEND.indexOf(cur[cur.length - 1]) >= 0) { carry = cur[cur.length - 1] + carry; cur = cur.slice(0, -1); }   // 開きかっこで行を終えない
          out.push(cur.replace(/\s+$/, '')); cur = carry + (/^\s+$/.test(u) ? '' : u);
        }
        out.push(cur);
      });
      return out;
    }
    B.wrap = wrap;
    B.text = function (str, o) {
      o = o || {}; var font = o.bold ? FS.GB : FS.S, size = o.size || 10.5, lh = o.lh || size * 1.75, x0 = M + (o.indent || 0), maxW = (o.width || CW) - (o.indent || 0);
      var lines = wrap(str, font, size, maxW);
      if (o.keep) B.need(lh * Math.min(lines.length, o.keep));
      lines.forEach(function (l) {
        B.need(lh);
        var x = x0; if (o.align === 'center') x = M + (CW - font.widthOfTextAtSize(l, size)) / 2;
        if (l) font.draw(page, l, { x: x, y: PH - (y + size * 1.08), size: size, color: col(o.color || '#000000') });
        y += lh;
      });
    };
    B.rule = function (w, color) { B.need(4); page.drawLine({ start: { x: M, y: PH - y }, end: { x: M + CW, y: PH - y }, thickness: w || 0.6, color: col(color || '#808080') }); y += 4; };

    // ---------- SVG を PDF に描く
    B.svg = async function (svgStr, o) {
      o = o || {};
      var dom = new DOMParser().parseFromString(svgStr, 'image/svg+xml'), root = dom.documentElement;
      if (!root || root.nodeName !== 'svg') return;
      var vb = (root.getAttribute('viewBox') || '').split(/[\s,]+/).map(Number), W = +root.getAttribute('width') || vb[2], H = +root.getAttribute('height') || vb[3];
      var s = Math.min(o.maxScale || 0.75, CW / W), avail = PH - FOOT - M * 1.5;
      if (H * s > avail) s = avail / H;
      var h = H * s; B.need(h + (o.below || 0));
      var ox = M + (CW - W * s) / 2, oy = y, defs = {};
      [].forEach.call(root.querySelectorAll('pattern, linearGradient'), function (d) { defs[d.getAttribute('id')] = d; });
      // 模様で塗る図形が四角形以外にあるときは、画像にして入れる（PDF の切り抜きを使わずに済ませる）
      var rasterize = [].some.call(root.querySelectorAll('path, circle, polygon'), function (e) { return /^url\(/.test(e.getAttribute('fill') || '') && !e.closest('pattern'); });
      if (rasterize) {
        if (used) { y += h; return; }                                // 1 回目は文字を集めるだけなので、画像は作らない
        var png = await Chart.toPng(svgStr, 3), img = await doc.embedPng(await png.arrayBuffer());
        page.drawImage(img, { x: ox, y: PH - oy - h, width: W * s, height: h }); y += h; return;
      }
      function P(m, x, yy) { var q = ap(m, x, yy); return { x: ox + q[0] * s, y: PH - (oy + q[1] * s) }; }
      function num(e, a, d) { var v = e.getAttribute(a); if (v === null || v === '') return d; if (/%$/.test(v)) return (parseFloat(v) / 100) * (a === 'width' || a === 'x' ? W : H); return parseFloat(v); }
      function inh(e, a) { while (e && e.getAttribute) { var v = e.getAttribute(a); if (v !== null) return v; e = e.parentNode; } return null; }
      function op(e) { var v = 1; for (var n = e; n && n.getAttribute; n = n.parentNode) { var o1 = n.getAttribute('opacity'); if (o1 !== null) v *= parseFloat(o1); } var fo = e.getAttribute('fill-opacity'); return v * (fo !== null ? parseFloat(fo) : 1); }
      function dash(e) { var d = inh(e, 'stroke-dasharray'); return d && d !== 'none' ? d.split(/[\s,]+/).map(function (v) { return parseFloat(v) * s; }) : undefined; }
      function strokeOf(e) { var c = inh(e, 'stroke'); if (!c || c === 'none') return null; var w = parseFloat(inh(e, 'stroke-width') || '1'); return w > 0 ? { color: col(c), width: w * s } : null; }
      function patternRect(m, x, yy, w, hh, pat) {
        var a = P(m, x, yy + hh), pw = +pat.getAttribute('width') || 8, ph = +pat.getAttribute('height') || 8;
        page.pushOperators(PDFLib.pushGraphicsState(), PDFLib.rectangle(a.x, a.y, w * s, hh * s), PDFLib.clip(), PDFLib.endPath());
        for (var ty = Math.floor(yy / ph) * ph; ty < yy + hh; ty += ph)
          for (var tx = Math.floor(x / pw) * pw; tx < x + w; tx += pw)
            [].forEach.call(pat.children, function (c) { draw(c, mul(m, [1, 0, 0, 1, tx, ty])); });
        page.pushOperators(PDFLib.popGraphicsState());
      }
      function gradRect(m, x, yy, w, hh, g) {
        var stops = [].map.call(g.querySelectorAll('stop'), function (st) { return { o: parseFloat(st.getAttribute('offset')) / (/%/.test(st.getAttribute('offset')) ? 100 : 1), c: hexOf(st.getAttribute('stop-color') || '#000') }; });
        var vert = (parseFloat(g.getAttribute('y2') || '0') !== parseFloat(g.getAttribute('y1') || '0')) && (parseFloat(g.getAttribute('x2') || '0') === parseFloat(g.getAttribute('x1') || '0'));
        var N = 64;
        for (var k = 0; k < N; k++) {
          var t = (k + 0.5) / N, i = 0; while (i < stops.length - 2 && t > stops[i + 1].o) i++;
          var a0 = stops[i], a1 = stops[Math.min(i + 1, stops.length - 1)], u = a1.o > a0.o ? Math.max(0, Math.min(1, (t - a0.o) / (a1.o - a0.o))) : 0;
          var c = Chart.lerp(a0.c, a1.c, u), rx = vert ? x : x + w * k / N, ry = vert ? yy + hh * k / N : yy, rw = vert ? w : w / N + 0.3, rh = vert ? hh / N + 0.3 : hh, q = P(m, rx, ry + rh);
          page.drawRectangle({ x: q.x, y: q.y, width: rw * s, height: rh * s, color: col(c) });
        }
      }
      function draw(e, m) {
        if (e.nodeType !== 1) return;
        var tag = e.nodeName.toLowerCase();
        if (tag === 'defs' || tag === 'pattern' || tag === 'lineargradient' || tag === 'title' || tag === 'style') return;
        if (e.getAttribute('display') === 'none' || e.getAttribute('visibility') === 'hidden') return;
        if (e.getAttribute('transform')) m = mul(m, parseTf(e.getAttribute('transform')));
        var fillA = inh(e, 'fill'), fill = fillA === null ? '#000000' : fillA, o1 = op(e), stk = strokeOf(e);
        var ref = /^url\(#([^)]+)\)/.exec(fill), refEl = ref ? defs[ref[1]] : null;
        if (tag === 'svg' || tag === 'g') { [].forEach.call(e.children, function (c) { draw(c, m); }); return; }
        if (tag === 'rect') {
          var x = num(e, 'x', 0), yy = num(e, 'y', 0), w = num(e, 'width', 0), hh = num(e, 'height', 0);
          if (!(w > 0 && hh > 0)) return;
          var invisible = o1 <= 0.001;
          if (!invisible && refEl && refEl.nodeName === 'pattern') patternRect(m, x, yy, w, hh, refEl);
          else if (!invisible && refEl) gradRect(m, x, yy, w, hh, refEl);
          var q = P(m, x, yy + hh), fc = refEl ? null : col(fill);
          if ((fc && !invisible) || stk) page.drawRectangle({ x: q.x, y: q.y, width: w * s, height: hh * s, color: invisible ? undefined : fc || undefined, opacity: o1, borderColor: stk ? stk.color : undefined, borderWidth: stk ? stk.width : 0, borderDashArray: dash(e) });
          return;
        }
        if (tag === 'line') {
          if (!stk) return;
          var a = P(m, num(e, 'x1', 0), num(e, 'y1', 0)), b = P(m, num(e, 'x2', 0), num(e, 'y2', 0));
          page.drawLine({ start: a, end: b, thickness: stk.width, color: stk.color, dashArray: dash(e), opacity: op(e) });
          return;
        }
        if (tag === 'circle' || tag === 'ellipse') {
          var c0 = P(m, num(e, 'cx', 0), num(e, 'cy', 0)), rr = (tag === 'circle' ? num(e, 'r', 0) : num(e, 'rx', 0)) * s, fc2 = col(fill);
          if (!(rr > 0) || (!fc2 && !stk)) return;
          page.drawEllipse({ x: c0.x, y: c0.y, xScale: rr, yScale: tag === 'circle' ? rr : num(e, 'ry', 0) * s, color: fc2 || undefined, opacity: o1, borderColor: stk ? stk.color : undefined, borderWidth: stk ? stk.width : 0, borderOpacity: 1 });
          return;
        }
        if (tag === 'path' || tag === 'polygon' || tag === 'polyline') {
          var d = tag === 'path' ? e.getAttribute('d') : 'M' + (e.getAttribute('points') || '').trim().replace(/\s+/g, ' ') + (tag === 'polygon' ? 'Z' : '');
          if (!d) return;
          var org = P(m, 0, 0), fc3 = col(fill);
          if (!fc3 && !stk) return;
          page.drawSvgPath(d, { x: org.x, y: org.y, scale: s * Math.sqrt(Math.abs(m[0] * m[3] - m[1] * m[2])) || s, color: fc3 || undefined, opacity: o1, borderColor: stk ? stk.color : undefined, borderWidth: stk ? stk.width / s : 0, borderDashArray: dash(e) });
          return;
        }
        if (tag === 'text') {
          var str = (e.textContent || '').replace(/\s+/g, ' ');
          if (!str.trim()) return;
          var size = parseFloat(inh(e, 'font-size') || '12') * s, bold = /bold|[6-9]00/.test(inh(e, 'font-weight') || ''), serif = /CTSMincho|CTSTimes/.test(inh(e, 'font-family') || '');
          var font = serif ? (bold ? FS.SB : FS.S) : (bold ? FS.GB : FS.G);
          var anchor = inh(e, 'text-anchor') || 'start', tw = font.widthOfTextAtSize(str, size), dx = anchor === 'middle' ? -tw / 2 : anchor === 'end' ? -tw : 0;
          var p0 = P(m, num(e, 'x', 0), num(e, 'y', 0)), ang = Math.atan2(-m[1], m[0]), ca = Math.cos(ang), sa = Math.sin(ang);
          var tx0 = p0.x + dx * ca, ty0 = p0.y + dx * sa, deg = ang * 180 / Math.PI, fcol = col(fill) || PDFLib.rgb(0, 0, 0);
          var halo = e.getAttribute('stroke'), hw = parseFloat(e.getAttribute('stroke-width') || '0') * s / 2;
          if (halo && halo !== 'none' && hw > 0) [[1, 0], [-1, 0], [0, 1], [0, -1], [0.7, 0.7], [-0.7, 0.7], [0.7, -0.7], [-0.7, -0.7]].forEach(function (v) {
            font.draw(page, str, { x: tx0 + v[0] * hw, y: ty0 + v[1] * hw, size: size, color: col(halo), rotate: PDFLib.degrees(deg) });
          });
          font.draw(page, str, { x: tx0, y: ty0, size: size, color: fcol, opacity: o1, rotate: PDFLib.degrees(deg) });
        }
      }
      draw(root, [1, 0, 0, 1, 0, 0]);
      y += h;
    };
  }

  // 表を描く。1 ページに収まらない長い表は、見出しの行を付けて分けて描く（縮めすぎて読めなくならないように）
  async function tableBlock(B, t, ts) {
    var svg = A.tableSvgDoc(t, ts), m = /width="([0-9.]+)" height="([0-9.]+)"/.exec(svg), W = +m[1], H = +m[2];
    var s = Math.min(0.72, CW / W), avail = PH - FOOT - M * 1.5;
    if (H * s <= avail) { await B.svg(svg, { maxScale: 0.72 }); return; }
    var body = t.rows.slice(1), per = Math.max(3, Math.floor((t.rows.length) * avail / (H * s)) - 3), k = 0;
    for (var i = 0; i < body.length; i += per, k++) {
      var part = Object.assign({}, t, { caption: t.caption + (k ? _t('（続き）') : ''), rows: [t.rows[0]].concat(body.slice(i, i + per)), note: i + per >= body.length ? t.note : '' });
      if (t.bold) part.bold = [t.bold[0]].concat(t.bold.slice(1 + i, 1 + i + per));
      if (t.total) part.total = { row: !!(t.total.row && i + per >= body.length) };
      await B.svg(A.tableSvgDoc(part, ts), { maxScale: 0.72 }); B.gap(6);
    }
  }
  // ---------------- 報告書の中身
  async function buildPdf() {
    var lib = await ensureLibs();
    await A.ensureHolmAsync();
    var latinHas = function (cp) { return lib.full.tr.hasGlyphForCodePoint(cp); };
    // 1 回目：使う文字を集める（保存しない）。幅は元の字体で測る
    used = { tr: new Set(Array.from('0123456789 /')) };
    var F1 = {}; Object.keys(lib.full).forEach(function (k) { F1[k] = measureFont(k, lib.full[k]); });
    var d1 = await PDFLib.PDFDocument.create();
    await compose(d1, fontSet(F1, latinHas), false);
    var chars = used; used = null;
    // 2 回目：字体ごとに、使う文字だけの字体（harfbuzz で作る）を埋め込んで作る
    var doc = await PDFLib.PDFDocument.create(), F2 = {};
    doc.registerFontkit(window.fontkit);
    for (var k in lib.src) {
      var cs = chars[k] && chars[k].size ? chars[k] : new Set([' ']), x = lib.src[k];
      F2[k] = await doc.embedFont(await hbSubset(x.buf, cs, x.index), { subset: false }); F2[k].key = k;
    }
    return compose(doc, fontSet(F2, latinHas), true);
  }
  async function compose(doc, FS, final) {
    doc.setTitle(S.P.title || _t('CrossTab Studio 報告書')); doc.setCreator('CrossTab Studio'); doc.setProducer('CrossTab Studio'); try { doc.setLanguage('ja-JP'); } catch (e) {}
    var B = new Builder(doc, FS), items = A.repItems(), lang = S.P.settings.figLang, figNo = 0;
    var fontOver = { fontJa: FAMILY, fontEn: FAMILY }, serifOver = { fontJa: 'CTSMincho', fontEn: 'CTSTimes' };   // 図はゴシック、表は明朝＋Times
    B.newPage();
    B.text(S.P.title || _t('報告書'), { size: 17, bold: true, lh: 26 });
    B.text(A.repMeta(), { size: 9, color: '#404040', lh: 15 });
    B.gap(4); B.rule(0.8, '#000000'); B.gap(8);
    if (!items.length) { B.text(_t('☑ を付けた解析がありません。'), { size: 10.5 }); }
    var tsBase = Object.assign({}, (S.P.tabs[0] && S.P.tabs[0].tstyle) || {}, serifOver);
    if (items.some(function (x) { return x.ok; })) {
      var sum = A.summaryTable();            // 解析の名前は各節の見出しにあるので、一覧では番号だけにして読める大きさを保つ
      sum = Object.assign({}, sum, { rows: sum.rows.map(function (r) { return r.filter(function (c, j) { return j !== 1; }); }), textCols: [true] });
      var sumSvg = A.tableSvgDoc(sum, tsBase);
      if (sumSvg) { await B.svg(sumSvg, { maxScale: 0.72 }); B.gap(14); }
    }
    for (var i = 0; i < items.length; i++) {
      var x = items[i];
      B.need(60);
      B.text(x.no + '. ' + x.tab.name, { size: 12.5, bold: true, lh: 20, keep: 1 });
      B.text(x.ok ? x.res.test : A.def(x.tab.type).name, { size: 9, color: '#404040', lh: 15 });
      B.gap(3);
      if (!x.ok) { B.text(x.res ? (x.res.message || _t('計算できませんでした。')) : _t('列がまだ選ばれていません。'), { size: 10.5 }); B.gap(12); continue; }
      if (x.res.method) {                                            // 方法を書く解析（テキストマイニング）は、方法と結果を分ける
        B.text(_t('方法'), { size: 10.5, bold: true, lh: 17, keep: 2 }); B.text(x.res.method, { size: 10.5, lh: 18.5, keep: 3 }); B.gap(4);
        B.text(_t('結果'), { size: 10.5, bold: true, lh: 17, keep: 2 });
      }
      if (x.text) B.text(x.text, { size: 10.5, lh: 18.5, keep: 3 });
      B.gap(8);
      if (S.P.report.tables) {
        var ts = Object.assign({}, x.tab.tstyle || {}, serifOver), tlist = A.tablesOf(x.tab, x.res);
        for (var ti = 0; ti < tlist.length; ti++) { await tableBlock(B, tlist[ti], ts); B.gap(10); }
      }
      if (S.P.report.figs !== false) {
        var m = Chart.model(x.res);
        for (var k = 0; m && k < x.tab.figs.length; k++) {
          var st = Object.assign({}, x.tab.figs[k], fontOver), r = Chart.render(m, st, lang);
          figNo++;
          await B.svg(r.svg, { below: 22 });
          B.gap(4);
          B.text(_t('図 {0}　', figNo) + x.tab.name + (x.tab.figs.length > 1 ? _t('（{0}）', k + 1) : ''), { size: 9, align: 'center', lh: 15 });
          B.gap(10);
        }
      }
      B.gap(10);
    }
    // 使用したソフトウェアと引用の例
    var cr = A.citeReport();
    B.need(90); B.rule(0.5, '#000000'); B.gap(6);
    B.text(cr.title, { size: 11, bold: true, lh: 18, keep: 2 }); B.text(cr.text, { size: 9.5, lh: 16 });
    cr.refs.forEach(function (r) { B.text(r, { size: 9.5, lh: 16, indent: 14 }); });
    // ページ番号
    var n = B.pages.length;
    B.pages.forEach(function (pg, i2) { var t = (i2 + 1) + ' / ' + n, w = FS.S.widthOfTextAtSize(t, 9); FS.S.draw(pg, t, { x: (PW - w) / 2, y: FOOT - 14, size: 9, color: col('#606060') }); });
    return final ? new Blob([await doc.save()], { type: 'application/pdf' }) : null;
  }
  async function reportPdf() {
    A.busy(true, _t('PDF を作っています（初回は字体を読み込みます）'));
    try { return await buildPdf(); } finally { A.busy(false); }
  }
  async function savePdf() {
    if (!S.data) { A.toast(_t('先に CSV ファイルを入れてください。'), true); return; }
    askLocalFonts();
    try {
      var blob = await reportPdf();
      if (await A.saveBlob(blob, A.baseName() + _t('_報告書.pdf'), 'PDF', '.pdf')) A.toast(_t('報告書を PDF で保存しました。'));
    } catch (e) { A.toast(e && e.name === 'AbortError' ? _t('保存をやめました。') : _t('PDF を作れませんでした（{0}）。', e && e.message || e), true); }
  }
  A.reportPdf = reportPdf; A.savePdf = savePdf; A.askLocalFonts = askLocalFonts;
})();
