// touch.js ― iPad 版：指で操作するための置き換え（events.js より前に読み込む）。
//   長押し → 右クリックのメニュー、2 回続けてタップ → ダブルクリック、
//   凡例・カラーバー・図の端を指でドラッグ → マウスのドラッグと同じ処理
(function () {
  'use strict';
  // 指が主な入力の端末（iPad・スマホ）だけ、指向けの見た目にする。タッチ画面付きのパソコンでも、指で触ったときの置き換えは働く
  var coarse = !!(window.matchMedia && matchMedia('(pointer: coarse)').matches);
  window.CTS_TOUCH = coarse;
  if (coarse) document.documentElement.classList.add('md-touch');
  if (!('ontouchstart' in window) && !(navigator.maxTouchPoints > 0)) return;
  var DRAG = '.md-figwrap [data-legend], .md-figwrap [data-cbar], .md-fh, .md-th';
  var lp = null, lpFired = false, lastTap = { t: 0, x: 0, y: 0 }, lastSynthDbl = 0, drag = null;
  function mouse(type, target, t, extra) {
    var ev = new MouseEvent(type, Object.assign({ bubbles: true, cancelable: true, view: window, clientX: t.clientX, clientY: t.clientY, screenX: t.screenX, screenY: t.screenY, button: type === 'contextmenu' ? 2 : 0, buttons: type === 'mouseup' ? 0 : 1 }, extra || {}));
    target.dispatchEvent(ev); return ev;
  }
  document.addEventListener('touchstart', function (e) {
    if (e.touches.length !== 1) { clearTimeout(lp); drag = null; return; }
    var t = e.touches[0], tg = e.target;
    lpFired = false;
    var d = tg.closest && tg.closest(DRAG);
    if (d) {                                         // 図の部品は、指を置いた所からドラッグできるようにする
      e.preventDefault();
      drag = { el: tg, x: t.clientX, y: t.clientY, moved: false, start: Date.now() };
      mouse('mousedown', tg, t);
    }
    var inField = /^(INPUT|TEXTAREA|SELECT)$/.test(tg.tagName);
    clearTimeout(lp);
    if (!inField) lp = setTimeout(function () {
      if (drag && drag.moved) return;
      lpFired = true;
      if (drag) { mouse('mouseup', document, t); drag = null; }
      mouse('contextmenu', tg, t);
    }, 550);
    lastTap.start = { x: t.clientX, y: t.clientY };
  }, { passive: false, capture: true });
  document.addEventListener('touchmove', function (e) {
    var t = e.touches[0]; if (!t) return;
    if (lastTap.start && Math.abs(t.clientX - lastTap.start.x) + Math.abs(t.clientY - lastTap.start.y) > 10) clearTimeout(lp);
    if (!drag) return;
    e.preventDefault();
    if (Math.abs(t.clientX - drag.x) + Math.abs(t.clientY - drag.y) > 4) drag.moved = true;
    mouse('mousemove', document, t);
  }, { passive: false, capture: true });
  document.addEventListener('touchend', function (e) {
    clearTimeout(lp);
    var t = e.changedTouches[0];
    if (lpFired) { e.preventDefault(); lpFired = false; drag = null; return; }      // 長押しのあとのタップは送らない
    if (drag) {
      e.preventDefault();
      mouse('mouseup', document, t);
      if (!drag.moved) mouse('click', drag.el, t);   // 動かさなかったときは、ふつうのタップ（色見本を押すなど）
      drag = null;
    }
    // 2 回続けてタップ → ダブルクリック
    var now = Date.now();
    if (now - lastTap.t < 380 && Math.abs(t.clientX - lastTap.x) + Math.abs(t.clientY - lastTap.y) < 24) {
      lastSynthDbl = now; lastTap.t = 0;
      var tg = document.elementFromPoint(t.clientX, t.clientY) || e.target;
      setTimeout(function () { mouse('dblclick', tg, t, { detail: 2 }); }, 0);
    } else { lastTap.t = now; lastTap.x = t.clientX; lastTap.y = t.clientY; }
  }, { passive: false, capture: true });
  document.addEventListener('touchcancel', function () { clearTimeout(lp); if (drag) { mouse('mouseup', document, { clientX: drag.x, clientY: drag.y }); drag = null; } }, true);
  // Safari が自分で出すダブルクリックとの二重を防ぐ
  document.addEventListener('dblclick', function (e) { if (e.isTrusted && Date.now() - lastSynthDbl < 700) { e.stopImmediatePropagation(); e.preventDefault(); } }, true);
  // 長押しで Safari の文字選択・コピーのメニューが出ないようにする（入力欄は除く）
  document.addEventListener('contextmenu', function (e) { if (e.isTrusted) e.preventDefault(); }, true);
})();
