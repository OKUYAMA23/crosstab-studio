// Excel ファイル（.xlsx）を CSV に直す。最初のシートを読み、日付は「年/月/日 時:分:秒」の文字にする
(function () {
  'use strict';
  async function inflate(data) {
    var ds = new DecompressionStream('deflate-raw');
    var buf = await new Response(new Blob([data]).stream().pipeThrough(ds)).arrayBuffer();
    return new Uint8Array(buf);
  }
  // zip の中のファイルを取り出す（中央ディレクトリを読む）
  async function unzip(u8) {
    var dv = new DataView(u8.buffer, u8.byteOffset, u8.byteLength), e = -1;
    for (var i = u8.length - 22; i >= Math.max(0, u8.length - 65557); i--) if (dv.getUint32(i, true) === 0x06054b50) { e = i; break; }
    if (e < 0) throw new Error('Excel ファイルとして読めませんでした。ファイルが壊れていないか確かめてください。');
    var n = dv.getUint16(e + 10, true), p = dv.getUint32(e + 16, true), out = {}, td = new TextDecoder();
    for (var k = 0; k < n; k++) {
      var meth = dv.getUint16(p + 10, true), csz = dv.getUint32(p + 20, true), nl = dv.getUint16(p + 28, true), xl = dv.getUint16(p + 30, true), cl = dv.getUint16(p + 32, true), off = dv.getUint32(p + 42, true);
      var name = td.decode(u8.subarray(p + 46, p + 46 + nl));
      out[name] = { meth: meth, csz: csz, off: off };
      p += 46 + nl + xl + cl;
    }
    return async function (name) {
      var f = out[name] || out[Object.keys(out).find(function (x) { return x.toLowerCase() === name.toLowerCase(); })]; if (!f) return null;
      var o = f.off, s = o + 30 + dv.getUint16(o + 26, true) + dv.getUint16(o + 28, true), d = u8.subarray(s, s + f.csz);
      d = f.meth === 8 ? await inflate(d) : d;
      return new TextDecoder().decode(d);
    };
  }
  function xml(t) { return t ? new DOMParser().parseFromString(t, 'application/xml') : null; }
  function tags(el, n) { return el ? Array.prototype.slice.call(el.getElementsByTagNameNS('*', n)) : []; }
  function kids(el, n) { return Array.prototype.filter.call(el.childNodes, function (c) { return c.localName === n; }); }
  // 共有文字列：ふりがな（rPh）は除き、書式の付いた部分（r）はつなげる
  function siText(si) {
    var t = kids(si, 't'); if (t.length) return t.map(function (x) { return x.textContent; }).join('');
    return kids(si, 'r').map(function (r) { return kids(r, 't').map(function (x) { return x.textContent; }).join(''); }).join('');
  }
  var DATE_IDS = [14, 15, 16, 17, 18, 19, 20, 21, 22, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 45, 46, 47, 50, 51, 52, 53, 54, 55, 56, 57, 58];
  function isDateCode(code) {
    var c = String(code).replace(/"[^"]*"/g, '').replace(/\[[^\]]*\]/g, '').replace(/\\./g, '');
    return /[ymdhs]/i.test(c) && !/^general$/i.test(c);
  }
  function col2n(ref) { var m = /^([A-Z]+)/.exec(ref), n = 0; if (!m) return -1; for (var i = 0; i < m[1].length; i++) n = n * 26 + m[1].charCodeAt(i) - 64; return n - 1; }
  function pad(x) { return (x < 10 ? '0' : '') + x; }
  function serialToText(v, d1904) {
    var ms = Math.round((v + (d1904 ? 1462 : 0)) * 86400) * 1000, d = new Date(Date.UTC(1899, 11, 30) + ms);
    var day = d.getUTCFullYear() + '/' + pad(d.getUTCMonth() + 1) + '/' + pad(d.getUTCDate()), tm = pad(d.getUTCHours()) + ':' + pad(d.getUTCMinutes()) + ':' + pad(d.getUTCSeconds());
    if (v < 1) return tm;
    return Math.abs(v - Math.round(v)) < 1e-9 ? day : day + ' ' + tm;
  }
  function numText(s) { var x = Number(s); if (!isFinite(x)) return s; return String(Number(x.toPrecision(15))); }
  async function xlsxToCsv(u8) {
    if (u8[0] === 0xD0 && u8[1] === 0xCF) throw new Error('古い形式の Excel ファイル（.xls）は読めません。Excel で「名前を付けて保存」から .xlsx か CSV にしてから入れてください。');
    if (typeof DecompressionStream === 'undefined') throw new Error('このブラウザは Excel ファイルの読み込みに対応していません。CSV にしてから入れてください。');
    var get = await unzip(u8);
    var wb = xml(await get('xl/workbook.xml')); if (!wb) throw new Error('Excel ファイルとして読めませんでした。');
    var sheets = tags(wb, 'sheet'); if (!sheets.length) throw new Error('シートが見つかりませんでした。');
    var d1904 = tags(wb, 'workbookPr').some(function (w) { var a = w.getAttribute('date1904'); return a === '1' || a === 'true'; });
    var rid = sheets[0].getAttributeNS('http://schemas.openxmlformats.org/officeDocument/2006/relationships', 'id') || sheets[0].getAttribute('r:id');
    var rels = xml(await get('xl/_rels/workbook.xml.rels')), target = 'worksheets/sheet1.xml';
    tags(rels, 'Relationship').forEach(function (r) { if (r.getAttribute('Id') === rid) target = r.getAttribute('Target'); });
    var path = target.charAt(0) === '/' ? target.slice(1) : 'xl/' + target.replace(/^\.\//, '');
    var sh = xml(await get(path)); if (!sh) throw new Error('最初のシートを読めませんでした。');
    var sst = xml(await get('xl/sharedStrings.xml')), strs = sst ? tags(sst, 'si').map(siText) : [];
    var st = xml(await get('xl/styles.xml')), fmts = {}, dateXf = [];
    if (st) {
      tags(st, 'numFmt').forEach(function (f) { fmts[+f.getAttribute('numFmtId')] = f.getAttribute('formatCode'); });
      var cx = tags(st, 'cellXfs')[0];
      if (cx) dateXf = kids(cx, 'xf').map(function (x) { var id = +x.getAttribute('numFmtId'); return DATE_IDS.indexOf(id) >= 0 || (fmts[id] != null && isDateCode(fmts[id])); });
    }
    var rows = [], maxC = 0;
    tags(sh, 'row').forEach(function (r, ri) {
      var rn = +(r.getAttribute('r') || (ri + 1)) - 1, line = [], ci = 0;
      kids(r, 'c').forEach(function (c) {
        var ref = c.getAttribute('r'), j = ref ? col2n(ref) : ci; ci = j + 1;
        var t = c.getAttribute('t'), vEl = kids(c, 'v')[0], v = vEl ? vEl.textContent : null, out = '';
        if (t === 's') out = v == null ? '' : (strs[+v] || '');
        else if (t === 'inlineStr') { var is = kids(c, 'is')[0]; out = is ? siText(is) : ''; }
        else if (t === 'str' || t === 'e') out = v == null ? '' : v;
        else if (t === 'b') out = v === '1' ? 'TRUE' : 'FALSE';
        else if (v != null && v !== '') { var s = +c.getAttribute('s') || 0; out = dateXf[s] ? serialToText(+v, d1904) : numText(v); }
        line[j] = out;
      });
      while (line.length && (line[line.length - 1] == null || line[line.length - 1] === '')) line.pop();
      rows[rn] = line; if (line.length > maxC) maxC = line.length;
    });
    while (rows.length && !(rows[rows.length - 1] || []).length) rows.pop();
    if (!rows.length) throw new Error('最初のシートが空です。');
    var csv = [];
    for (var i = 0; i < rows.length; i++) {
      var L = rows[i] || [], f = [];
      for (var k = 0; k < maxC; k++) { var x = L[k] == null ? '' : String(L[k]); f.push(/[",\r\n]/.test(x) ? '"' + x.replace(/"/g, '""') + '"' : x); }
      csv.push(f.join(','));
    }
    return { bytes: new TextEncoder().encode('﻿' + csv.join('\r\n') + '\r\n'), sheet: sheets[0].getAttribute('name'), nSheets: sheets.length };
  }
  window.App = window.App || {};
  window.App.xlsxToCsv = xlsxToCsv;
})();
