// 報告書を Word（.docx）で作る。文は MS 明朝・Times New Roman、見出しは MS ゴシック・Arial。表は Word の表（直せる）、図は PNG
(function () {
  'use strict';
  var A = window.App, S = A.S;
  var TW = 9070;                                          // 本文の幅（twip）：A4 の幅 11906 − 左右の余白 25 mm × 2
  function x(s) { return String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;'); }
  function runs(text, o) {
    o = o || {};
    var rp = '<w:rPr>' + (o.g ? _t('<w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:eastAsia="ＭＳ ゴシック"/>') : '') + (o.b ? '<w:b/><w:bCs/>' : '') + (o.color ? '<w:color w:val="' + o.color.replace('#', '') + '"/>' : '') + (o.sz ? '<w:sz w:val="' + Math.round(o.sz * 2) + '"/><w:szCs w:val="' + Math.round(o.sz * 2) + '"/>' : '') + '</w:rPr>';
    return String(text == null ? '' : text).split('\n').map(function (l, i) { return (i ? '<w:r>' + rp + '<w:br/></w:r>' : '') + '<w:r>' + rp + '<w:t xml:space="preserve">' + x(l) + '</w:t></w:r>'; }).join('');
  }
  function para(text, o) {
    o = o || {};
    var pp = '<w:pPr>' + (o.style ? '<w:pStyle w:val="' + o.style + '"/>' : '') + (o.keep ? '<w:keepNext/>' : '') + (o.pb ? '<w:pageBreakBefore/>' : '') +
      '<w:spacing w:before="' + Math.round((o.before || 0) * 20) + '" w:after="' + Math.round((o.after == null ? 4 : o.after) * 20) + '"/>' +
      (o.indent ? '<w:ind w:left="' + Math.round(o.indent * 20) + '" w:hanging="' + Math.round(o.indent * 20) + '"/>' : '') + (o.align ? '<w:jc w:val="' + o.align + '"/>' : '') +
      (o.border ? '<w:pBdr><w:bottom w:val="single" w:sz="' + o.border + '" w:space="4" w:color="000000"/></w:pBdr>' : '') + '</w:pPr>';
    return '<w:p>' + pp + (o.raw || runs(text, o)) + '</w:p>';
  }
  // 表：上下の太い罫線と、見出しの下・合計の上の細い罫線（論文でよく使う形）
  function tableXml(t) {
    var nR = t.rows.length, nC = t.rows[0].length, totRow = t.total && t.total.row;
    // 列の幅：中身の長さ（全角は半角 2 文字分）から決め、本文の幅を超えるときは長い列だけを狭めて折り返す
    var w = []; for (var j = 0; j < nC; j++) w[j] = 160 + 92 * Math.max.apply(null, t.rows.map(function (r, i) { var s0 = String(r[j] == null ? '' : r[j]).replace(/[^\x00-\xff]/g, 'xx').length; return i === 0 ? Math.min(s0, 16) : s0; }));
    var sumW = function (c) { return w.reduce(function (a, v) { return a + Math.min(v, c); }, 0); };
    if (sumW(1e9) > TW) { var lo = 300, hi = Math.max.apply(null, w); while (hi - lo > 10) { var mid = (lo + hi) / 2; if (sumW(mid) > TW) hi = mid; else lo = mid; } w = w.map(function (v) { return Math.round(Math.min(v, lo)); }); }
    var h = '<w:tbl><w:tblPr><w:tblW w:w="' + w.reduce(function (a, b) { return a + b; }, 0) + '" w:type="dxa"/><w:tblLayout w:type="fixed"/><w:tblBorders><w:top w:val="single" w:sz="12" w:color="000000"/><w:bottom w:val="single" w:sz="12" w:color="000000"/></w:tblBorders>' +
      '<w:tblCellMar><w:left w:w="70" w:type="dxa"/><w:right w:w="70" w:type="dxa"/></w:tblCellMar></w:tblPr><w:tblGrid>' + w.map(function (v) { return '<w:gridCol w:w="' + v + '"/>'; }).join('') + '</w:tblGrid>';
    // 本文の行がすべて日本語の文字を含む列（検定の名前など）は左に寄せる
    var jaCol = []; for (var j2 = 0; j2 < nC; j2++) { var body = t.rows.slice(1, totRow ? -1 : undefined).map(function (r) { return String(r[j2] == null ? '' : r[j2]); }).filter(function (v) { return v && v !== '–'; }); jaCol[j2] = body.length > 0 && body.every(function (v) { return /[\u3040-\u30ff\u4e00-\u9fff]/.test(v); }); }
    for (var i = 0; i < nR; i++) {
      var head = i === 0, tr = totRow && i === nR - 1;
      h += '<w:tr>' + (head ? '<w:trPr><w:tblHeader/><w:cantSplit/></w:trPr>' : '<w:trPr><w:cantSplit/></w:trPr>');
      for (var c = 0; c < nC; c++) {
        var txt = t.rows[i][c], left = c === 0 || (t.textCols && t.textCols[c]) || jaCol[c], b = head || (t.bold && t.bold[i] && t.bold[i][c]);
        var bd = head ? '<w:bottom w:val="single" w:sz="6" w:color="000000"/>' : tr ? '<w:top w:val="single" w:sz="6" w:color="000000"/>' : '';
        h += '<w:tc><w:tcPr><w:tcW w:w="' + w[c] + '" w:type="dxa"/>' + (bd ? '<w:tcBorders>' + bd + '</w:tcBorders>' : '') + '<w:vAlign w:val="center"/></w:tcPr>' +
          '<w:p><w:pPr><w:spacing w:before="20" w:after="20" w:line="240" w:lineRule="auto"/><w:jc w:val="' + (head ? 'center' : left ? 'left' : 'right') + '"/></w:pPr>' + runs(txt, { sz: 9, b: !!b }) + '</w:p></w:tc>';
      }
      h += '</w:tr>';
    }
    return h + '</w:tbl>';
  }
  function tableBlock(t) {
    return para(t.caption, { sz: 9.5, b: true, keep: true, before: 4, after: 2 }) + tableXml(t) + (t.note ? para(t.note, { sz: 8.5, after: 8 }) : para('', { after: 6 }));
  }
  function svgSize(svg) { var m = /width="([0-9.]+)" height="([0-9.]+)"/.exec(svg); return m ? [+m[1], +m[2]] : [560, 340]; }
  function picXml(rid, id, wpx, hpx) {
    var cx = Math.round(wpx * 9525), cy = Math.round(hpx * 9525), max = 5760000;            // 図の幅は 16 cm まで
    if (cx > max) { cy = Math.round(cy * max / cx); cx = max; }
    return '<w:r><w:drawing><wp:inline distT="0" distB="0" distL="0" distR="0"><wp:extent cx="' + cx + '" cy="' + cy + '"/><wp:docPr id="' + id + _t('" name="図 ') + id + '"/>' +
      '<a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">' +
      '<pic:nvPicPr><pic:cNvPr id="' + id + '" name="image' + id + '.png"/><pic:cNvPicPr/></pic:nvPicPr><pic:blipFill><a:blip r:embed="' + rid + '"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>' +
      '<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="' + cx + '" cy="' + cy + '"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr></pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r>';
  }
  var STYLES = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">' +
    _t('<w:docDefaults><w:rPrDefault><w:rPr><w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman" w:eastAsia="ＭＳ 明朝" w:cs="Times New Roman"/><w:sz w:val="21"/><w:szCs w:val="21"/><w:lang w:val="en-US" w:eastAsia="ja-JP"/></w:rPr></w:rPrDefault><w:pPrDefault><w:pPr><w:spacing w:after="80" w:line="300" w:lineRule="auto"/><w:jc w:val="both"/></w:pPr></w:pPrDefault></w:docDefaults><w:style w:type="paragraph" w:default="1" w:styleId="Normal"><w:name w:val="Normal"/><w:qFormat/></w:style><w:style w:type="paragraph" w:styleId="Title"><w:name w:val="Title"/><w:basedOn w:val="Normal"/><w:qFormat/><w:pPr><w:jc w:val="left"/></w:pPr><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:eastAsia="ＭＳ ゴシック"/><w:b/><w:sz w:val="34"/></w:rPr></w:style><w:style w:type="paragraph" w:styleId="Heading1"><w:name w:val="heading 1"/><w:basedOn w:val="Normal"/><w:next w:val="Normal"/><w:qFormat/><w:pPr><w:keepNext/><w:spacing w:before="240" w:after="60"/><w:jc w:val="left"/><w:outlineLvl w:val="0"/></w:pPr><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:eastAsia="ＭＳ ゴシック"/><w:b/><w:sz w:val="25"/></w:rPr></w:style><w:style w:type="paragraph" w:styleId="Heading2"><w:name w:val="heading 2"/><w:basedOn w:val="Normal"/><w:next w:val="Normal"/><w:qFormat/><w:pPr><w:keepNext/><w:spacing w:before="80" w:after="40"/><w:outlineLvl w:val="1"/></w:pPr><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:eastAsia="ＭＳ ゴシック"/><w:b/><w:sz w:val="21"/></w:rPr></w:style>') +
    '<w:style w:type="paragraph" w:styleId="Caption"><w:name w:val="caption"/><w:basedOn w:val="Normal"/><w:qFormat/><w:pPr><w:jc w:val="center"/></w:pPr><w:rPr><w:sz w:val="18"/></w:rPr></w:style></w:styles>';
  var NS = 'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"';
  async function buildDocx() {
    await A.ensureHolmAsync();
    var items = A.repItems(), lang = S.P.settings.figLang, body = '', media = [], figNo = 0;
    body += para(S.P.title || _t('報告書'), { style: 'Title', after: 2 });
    body += para(A.repMeta(), { sz: 9, color: '#404040', border: 8, after: 10 });
    if (!items.length) body += para(_t('☑ を付けた解析がありません。'));
    if (items.some(function (y) { return y.ok; })) {
      var sum = A.summaryTable(); sum = Object.assign({}, sum, { rows: sum.rows.map(function (r) { return r.filter(function (c, j) { return j !== 1; }); }), textCols: [true] });
      body += tableBlock(sum);
    }
    for (var i = 0; i < items.length; i++) {
      var it = items[i];
      body += para(it.no + '. ' + it.tab.name, { style: 'Heading1' });
      body += para(it.ok ? it.res.test : A.def(it.tab.type).name, { sz: 9, color: '#404040', keep: true });
      if (!it.ok) { body += para(it.res ? (it.res.message || _t('計算できませんでした。')) : _t('列がまだ選ばれていません。')); continue; }
      if (it.res.method) body += para(_t('方法'), { style: 'Heading2' }) + para(it.res.method) + para(_t('結果'), { style: 'Heading2' });
      if (it.text) body += para(it.text);
      if (S.P.report.tables) A.tablesOf(it.tab, it.res).forEach(function (t) { body += tableBlock(t); });
      if (S.P.report.figs !== false) {
        var m = Chart.model(it.res);
        for (var k = 0; m && k < it.tab.figs.length; k++) {
          var r = Chart.render(m, it.tab.figs[k], lang), sz = svgSize(r.svg), png = await Chart.toPng(r.svg, 3);
          figNo++; var rid = 'rIdImg' + figNo; media.push({ rid: rid, name: 'image' + figNo + '.png', data: new Uint8Array(await png.arrayBuffer()) });
          body += para('', { raw: picXml(rid, figNo, sz[0], sz[1]), align: 'center', keep: true, before: 6, after: 2 });
          body += para(_t('図 {0}　', figNo) + it.tab.name + (it.tab.figs.length > 1 ? _t('（{0}）', k + 1) : ''), { style: 'Caption', after: 10 });
        }
      }
    }
    var cr = A.citeReport();
    body += para(cr.title, { style: 'Heading1' }) + para(cr.text, { sz: 9.5 });
    cr.refs.forEach(function (rf) { body += para(rf, { sz: 9.5, indent: 14, align: 'left' }); });
    var sect = '<w:sectPr><w:footerReference w:type="default" r:id="rIdFt"/><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1418" w:right="1418" w:bottom="1418" w:left="1418" w:header="851" w:footer="567" w:gutter="0"/></w:sectPr>';
    var doc = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:document ' + NS + '><w:body>' + body + sect + '</w:body></w:document>';
    var fld = function (f) { return '<w:r><w:fldChar w:fldCharType="begin"/></w:r><w:r><w:instrText xml:space="preserve"> ' + f + ' </w:instrText></w:r><w:r><w:fldChar w:fldCharType="separate"/></w:r><w:r><w:t>1</w:t></w:r><w:r><w:fldChar w:fldCharType="end"/></w:r>'; };
    var footer = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:ftr ' + NS + '><w:p><w:pPr><w:jc w:val="center"/></w:pPr>' + fld('PAGE') + '<w:r><w:t xml:space="preserve"> / </w:t></w:r>' + fld('NUMPAGES') + '</w:p></w:ftr>';
    var now = new Date().toISOString().replace(/\.\d+Z$/, 'Z');
    var files = {
      '[Content_Types].xml': '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Default Extension="png" ContentType="image/png"/>' +
        '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/><Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>' +
        '<Override PartName="/word/settings.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"/><Override PartName="/word/footer1.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"/>' +
        '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/><Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/></Types>',
      '_rels/.rels': '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/><Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/><Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/></Relationships>',
      'docProps/core.xml': '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><dc:title>' + x(S.P.title || _t('報告書')) + '</dc:title><dc:creator>CrossTab Studio</dc:creator><dcterms:created xsi:type="dcterms:W3CDTF">' + now + '</dcterms:created><dcterms:modified xsi:type="dcterms:W3CDTF">' + now + '</dcterms:modified></cp:coreProperties>',
      'docProps/app.xml': '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"><Application>CrossTab Studio</Application></Properties>',
      'word/document.xml': doc, 'word/styles.xml': STYLES, 'word/footer1.xml': footer,
      'word/settings.xml': '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:settings xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:updateFields w:val="true"/><w:defaultTabStop w:val="840"/><w:compat><w:compatSetting w:name="compatibilityMode" w:uri="http://schemas.microsoft.com/office/word" w:val="15"/></w:compat></w:settings>',
      'word/_rels/document.xml.rels': '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rIdSt" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/><Relationship Id="rIdSe" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings" Target="settings.xml"/><Relationship Id="rIdFt" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer" Target="footer1.xml"/>' +
        media.map(function (m2) { return '<Relationship Id="' + m2.rid + '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/' + m2.name + '"/>'; }).join('') + '</Relationships>'
    };
    var enc = new TextEncoder(), list = Object.keys(files).map(function (k2) { return { name: k2, data: enc.encode(files[k2]) }; });
    media.forEach(function (m2) { list.push({ name: 'word/media/' + m2.name, data: m2.data }); });
    return A.zip(list, 'application/vnd.openxmlformats-officedocument.wordprocessingml.document');
  }
  async function reportDocx() { A.busy(true, _t('Word ファイルを作っています')); try { return await buildDocx(); } finally { A.busy(false); } }
  async function saveDocx() {
    if (!S.data) { A.toast(_t('先に CSV ファイルを入れてください。'), true); return; }
    try { var blob = await reportDocx(); if (await A.saveBlob(blob, A.baseName() + _t('_報告書.docx'), _t('Word 文書'), '.docx')) A.toast(_t('報告書を Word で保存しました。表は Word の表なので、そのまま直せます。')); }
    catch (e) { A.toast(e && e.name === 'AbortError' ? _t('保存をやめました。') : _t('Word ファイルを作れませんでした（{0}）。', e && e.message || e), true); }
  }
  A.reportDocx = reportDocx; A.saveDocx = saveDocx;
})();
