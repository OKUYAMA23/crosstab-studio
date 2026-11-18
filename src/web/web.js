// web.js ― iPad 版：保存（ダウンロード）と起動中の表示（events.js のあとに読み込む）
(function () {
  'use strict';
  var A = window.App, S = A.S;
  // ---------------- 保存：Claude のページでは、ダウンロードの確認画面を通して保存する
  var dlP = null;
  function downloads() {
    if (dlP) return dlP;
    dlP = (window.claude && typeof window.claude.use === 'function') ? Promise.resolve(window.claude.use('downloads')).catch(function () { return null; }) : Promise.resolve(null);
    return dlP;
  }
  window.CTS_SAVE = async function (blob, name) {
    var dl = await downloads();
    if (dl) {
      try { await dl.save({ filename: name, data: blob }); return true; }
      catch (e) {
        var c = e && e.code;
        if (c === 'declined') return false;
        if (c === 'rate_limited') { A.toast('ほかの保存の確認が開いています。閉じてからもう一度押してください。', true); return false; }
        A.toast('この画面では保存できませんでした（' + (c || (e && e.message) || '不明') + '）。', true); return false;
      }
    }
    // Claude の外（ふつうの Safari など）で開いたとき
    try {
      var f = new File([blob], name, { type: blob.type || 'application/octet-stream' });
      if (navigator.canShare && navigator.canShare({ files: [f] })) { await navigator.share({ files: [f] }); return true; }
    } catch (e) { if (e && e.name === 'AbortError') return false; }
    var a = document.createElement('a'); a.href = URL.createObjectURL(blob); a.download = name; document.body.appendChild(a); a.click();
    setTimeout(function () { URL.revokeObjectURL(a.href); a.remove(); }, 4000);
    return true;
  };

  // ---------------- 起動中の表示（R を読み込む間、何をしているかと経過秒数を出す）
  var box = document.getElementById('md_boot'), t0 = Date.now(), timer = null;
  function setStep(n, msg) {
    if (!box) return;
    if (n < 0) {
      box.classList.add('err');
      box.querySelector('.md-boot-msg').textContent = 'R を起動できませんでした。ページを読み込み直してください。（' + msg + '）';
      clearInterval(timer); return;
    }
    for (var k = 1; k <= 3; k++) { var el = box.querySelector('[data-step="' + k + '"]'); if (el) el.className = k < n ? 'done' : k === n ? 'now' : ''; }
    box.querySelector('.md-boot-msg').textContent = msg;
  }
  window.CTS_BOOT.onStep(setStep);
  setStep(1, '計算に使う R を読み込んでいます（初回は 20 MB ほどの読み込みがあります）');
  timer = setInterval(function () {
    var s = Math.floor((Date.now() - t0) / 1000), p = Math.min(92, 6 + 86 * (1 - Math.exp(-s / 12)));
    box.querySelector('.md-boot-sec').textContent = s + ' 秒';
    box.querySelector('.md-boot-bar i').style.width = p + '%';
    if (s > 40) box.querySelector('.md-boot-note').textContent = '回線が遅いと 1 分以上かかることがあります。2 回目からは速くなります。';
  }, 300);
  // 縦向きなど幅が狭いときは、作業ウィンドウを閉じた状態で始める（開くと図の上に重なる）
  if (window.innerWidth <= 1000) { S.ui.pane = false; A.render(); }
  window.CTS_BOOT.ready.then(function () {
    clearInterval(timer);
    box.querySelector('.md-boot-bar i').style.width = '100%';
    setTimeout(function () { box.remove(); A.render(); }, 250);
  }, function () {});
})();

// ---------------- 公開終了の合図：ping.json の active が false なら、公開を終えたことを知らせて止める
//   ping.json はアプリと同じ場所から読む（手元に置いた写しは、その写しの ping.json を読むので止まらない）。
//   取れないとき（通信がないとき）は何もしない。以前あった「2 週間つながないと止まる」決まりは 0.8.3 でなくした。
(function () {
  'use strict';
  async function check() {
    try {
      var r = await fetch('ping.json?t=' + Date.now(), { cache: 'no-store' });
      if (!r.ok) return;
      var d = await r.json();
      if (d && d.active === false) {
        var el = document.createElement('div'); el.id = 'md_lock';
        el.innerHTML = '<div class="md-lock-box"><img src="icons/icon-192.png" width="64" height="64" alt=""><h1>公開を終了しました</h1><p>' + String(d.message || 'このアプリの公開は終了しました。').replace(/[&<>"]/g, function (c) { return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]; }) + '</p></div>';
        document.body.appendChild(el);
      }
    } catch (e) { /* 通信できないときは何もしない */ }
  }
  check();
})();
