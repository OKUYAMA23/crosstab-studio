// phone.js ― スマホ（幅の狭い画面）向けの見た目と動き（events.js のあとに読み込む）
//   ・リボンは常に出しておく（タブを押すと中身が替わる）
//   ・作業ウィンドウは画面の下半分に出し、上半分で図を見ながら設定できるようにする
//   ・図は画面の幅に収まるように縮めて見せる（保存するときの大きさは変えない）
//   ・図を押したとき、画面がその解析の先頭へ飛ばないように、図の位置を保つ
//   ・iPhone で入力欄に触れたときに画面が勝手に拡大されないようにする
(function () {
  'use strict';
  var A = window.App, S = A.S, root = document.documentElement;
  var IOS = /iPad|iPhone|iPod/.test(navigator.userAgent) || (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);
  // iOS の Safari は、16px より小さい文字の入力欄に触れると画面を拡大する。maximum-scale=1 で止める（2 本指の拡大はそのまま使える）
  if (IOS) {
    var vp = document.querySelector('meta[name=viewport]');
    if (vp && !/maximum-scale/.test(vp.content)) vp.content += ', maximum-scale=1';
  }
  function isPhone() { return Math.min(window.innerWidth, window.screen ? screen.width : 9999) < 700 || window.innerWidth < 700; }
  function apply() {
    var ph = isPhone(); root.classList.toggle('md-phone', ph); window.CTS_PHONE = ph;
    if (ph && !root.dataset.phoneInit) { root.dataset.phoneInit = '1'; S.ui.pane = false; A.render(); }
    fitFigs();
  }
  // ---- 図を画面の幅に収める（CSS の zoom で縮めて見せる。ドラッグの計算は events.js の zoom() が倍率を考える）
  function fitFigs() {
    if (document.body.className.indexOf('md-resizing') >= 0) return;
    var ws = document.querySelectorAll('.md-figwrap'); if (!ws.length) return;
    // 幅は 1 回だけ測る（解析が多いときに、欄ごとに測って描き直しが重くならないように）
    var host0 = ws[0].parentNode, availAll = host0 ? host0.clientWidth - 8 : 300;
    ws.forEach(function (w) {
      var svg = w.querySelector('svg'); if (!svg) return;
      var fw = +svg.getAttribute('width') || 560, avail = availAll;
      var fit = root.classList.contains('md-phone') && fw > avail && avail > 80 ? Math.max(0.3, avail / (fw + 14)) : 1;
      var old = +(w.dataset.fit || 1);
      if (Math.abs(old - fit) > 0.001) { if (fit === 1) { delete w.dataset.fit; w.style.zoom = ''; } else { w.dataset.fit = fit.toFixed(4); w.style.zoom = fit.toFixed(4); } }
    });
  }
  A.fitFigs = fitFigs;   // 描き直した直後に縮め、位置を合わせる前に図の高さを決める（view.js の renderCenter から呼ぶ）
  var mo = new MutationObserver(function () { if (!fitQ) fitQ = requestAnimationFrame(function () { fitQ = 0; fitFigs(); }); }), fitQ = 0;
  function observe() { var c = document.querySelector('.md-center'); if (c) mo.observe(c, { childList: true, subtree: true }); }
  function view() { return document.getElementById('md_view'); }
  // ---- 図を押したあと、描き直しや入力欄の選択で画面が動いても、押した図を同じ高さに戻す
  function keepFig(e) {
    if (!root.classList.contains('md-phone')) return;
    var w = e.target.closest && e.target.closest('.md-figwrap'); if (!w || !w.dataset.fig) return;
    var key = w.dataset.fig, top0 = w.getBoundingClientRect().top;
    [0, 80, 300].forEach(function (ms) {
      setTimeout(function () {
        var w2 = document.querySelector('.md-figwrap[data-fig="' + key + '"]'), v = view(); if (!w2 || !v) return;
        var d = w2.getBoundingClientRect().top - top0; if (Math.abs(d) > 2) v.scrollTop += d;
      }, ms);
    });
  }
  ['click', 'dblclick', 'contextmenu'].forEach(function (t) { document.addEventListener(t, keepFig, true); });
  // 入力欄の外に触れたら、入力欄の選択を外す（選んだままだと、Safari がその欄へ画面を戻すため）
  document.addEventListener('touchstart', function (e) {
    var a = document.activeElement;
    if (a && /^(INPUT|SELECT|TEXTAREA)$/.test(a.tagName) && !(e.target.closest && e.target.closest('input, select, textarea, label'))) a.blur();
  }, { passive: true, capture: true });
  // ---- 画面の枠がずれたら戻す
  //   入力欄に触れると、Safari は欄を見せるために、本来は動かない外枠（ページ全体や .md-app）まで上へずらす。
  //   キーボードを閉じてもそのまま残り、タブやタイトルバーが画面の外に出て戻せなくなるため、入力が終わったら必ず 0 に戻す
  var LOCK = [document.documentElement, document.body];
  function locked() { return LOCK.concat([].slice.call(document.querySelectorAll('.md-app, .md-main, .md-center, .md-tabs-wrap'))); }
  function inField() { var a = document.activeElement; return !!a && (/^(INPUT|SELECT|TEXTAREA)$/.test(a.tagName) || a.isContentEditable); }
  function resetFrame() {
    if (window.scrollX || window.scrollY) window.scrollTo(0, 0);
    locked().forEach(function (el) { if (el.scrollTop) el.scrollTop = 0; if (el.scrollLeft) el.scrollLeft = 0; });
  }
  document.addEventListener('focusout', function () { setTimeout(function () { if (!inField()) resetFrame(); }, 150); }, true);
  document.addEventListener('scroll', function (e) {
    if (inField()) return;
    var t = e.target === document ? document.documentElement : e.target;
    if (locked().indexOf(t) >= 0 || e.target === document) resetFrame();
  }, { passive: true, capture: true });
  if (window.visualViewport) window.visualViewport.addEventListener('resize', function () { if (!inField()) setTimeout(resetFrame, 50); });
  window.addEventListener('pageshow', resetFrame);
  // ---- 作業ウィンドウを開いたら、選んでいる図を上の欄の先頭に出す
  function showSelFig() {
    var v = view(); if (!v) return;
    var w = document.querySelector('.md-figwrap.sel') || (S.ui.sel && document.querySelector('.md-figwrap[data-fig="' + S.ui.sel + ':' + (S.ui.selFig || 0) + '"]'));
    if (!w) return;
    var r = w.getBoundingClientRect(), vr = v.getBoundingClientRect();
    if (r.top < vr.top || r.bottom > vr.bottom) { if (A.stopKeep) A.stopKeep(); } else return;
    v.scrollTop += r.top - vr.top - 4;
  }
  // タイトルバーの右端に「設定」（作業ウィンドウを開く・閉じる）を置く。タイトルバーは描き直されるので、そのたびに足す
  var paneWas = false;
  function paneBtn() {
    var t = document.getElementById('md_title'); if (!t) return;
    var b = t.querySelector('.md-pane-open');
    if (!b) { b = document.createElement('button'); b.className = 'md-pane-open'; b.setAttribute('data-act', 'togglePane'); t.appendChild(b); }
    var on = !!S.ui.pane; b.classList.toggle('on', on); b.textContent = on ? '詳細を閉じる' : '詳細';
    if (on && !paneWas && root.classList.contains('md-phone')) setTimeout(showSelFig, 60);
    paneWas = on;
  }
  new MutationObserver(function () { paneBtn(); }).observe(document.getElementById('md_title'), { childList: true });
  var render0 = A.render; A.render = function () { var r = render0.apply(this, arguments); paneBtn(); return r; };
  paneBtn();
  window.addEventListener('resize', apply);
  window.addEventListener('orientationchange', function () { setTimeout(apply, 300); });
  observe(); apply();
  window.CTS_BOOT.ready.then(function () { setTimeout(function () { observe(); apply(); }, 300); }, function () {});
})();
