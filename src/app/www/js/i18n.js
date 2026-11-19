// i18n.js ― 画面の言語（日本語・英語）。文字列は _t('日本語') で包み、英語のときは辞書（i18n_en.js の CTS_EN）の訳を返す。
// {0}, {1} … は 2 番目以降の引数で置き換える。訳がない文字列は日本語のまま返し、CTS_MISS に記録する（確認用）。
(function () {
  var lang = 'ja';
  try { var s = localStorage.getItem('cts-lang'); lang = s === 'en' || s === 'ja' ? s : (/^ja\b/i.test(navigator.language || 'ja') ? 'ja' : 'en'); } catch (e) {}
  window.CTS_LANG = lang; window.CTS_ONLANG = []; window.CTS_MISS = {};
  var JA = /[぀-ヿ一-鿿]/, has = Object.prototype.hasOwnProperty;
  window._k = function (s) { return s; };      // 辞書に載せるための印だけ（訳は使う所で _t(…) する）
  window._t = function (s) {
    var r = s;
    if (window.CTS_LANG === 'en') { var d = window.CTS_EN || {}; if (has.call(d, s)) r = d[s]; else if (JA.test(s)) window.CTS_MISS[s] = 1; }
    if (arguments.length > 1) { var a = arguments; r = String(r).replace(/\{(\d)\}/g, function (m, i) { var v = a[+i + 1]; return v === undefined || v === null ? '' : String(v); }); }
    return r;
  };
  // index.html に直接書いた文字（data-i18n / data-i18n-tip を付けた要素）を、今の言語にする
  function applyStatic() {
    [].forEach.call(document.querySelectorAll('[data-i18n]'), function (e) { if (e.dataset.ja === undefined) e.dataset.ja = e.textContent; e.textContent = window._t(e.dataset.ja); });
    [].forEach.call(document.querySelectorAll('[data-i18n-tip]'), function (e) { if (e.dataset.jaTip === undefined) e.dataset.jaTip = e.getAttribute('data-tip') || ''; e.setAttribute('data-tip', window._t(e.dataset.jaTip)); });
  }
  // index.html に直接書いた文字（辞書に載せるための印。tools/i18n.py extract が拾う）
  [_k('作業ウィンドウに出す内容を選びます。上のタブと連動します。'), _k('前のタブへ'), _k('次のタブへ'), _k('作業ウィンドウを閉じます。'), _k('ここにドロップして読み込む'), _k('計算しています'), _k('処理は続いています。止まってはいません。そのままお待ちください。'), _k('アンケートの集計と検定'), _k('起動しています'), _k('計算に使う R を読み込んでいます')];
  window.CTS_ONLANG.push(applyStatic);
  document.documentElement.setAttribute('lang', lang);
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', applyStatic); else applyStatic();
  window.ctsSetLang = function (l) {
    window.CTS_LANG = l === 'en' ? 'en' : 'ja';
    try { localStorage.setItem('cts-lang', window.CTS_LANG); } catch (e) {}
    document.documentElement.setAttribute('lang', window.CTS_LANG);
    window.CTS_ONLANG.forEach(function (f) { try { f(window.CTS_LANG); } catch (e) { if (window.console) console.error(e); } });
  };
})();
