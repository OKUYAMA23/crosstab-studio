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

// ---------------- 利用の確認：最後にインターネットで確かめてから 14 日を過ぎたら、機能を止める
//   起動したとき・画面に戻ったとき・通信が戻ったときに ping.json を取りに行き、取れた日時を覚える。
//   ping.json の active が false なら、公開を終えたものとして止める（公開を止めるときの終了の合図）。
(function () {
  'use strict';
  var A = window.App, KEY = 'cts-last-online', LIMIT = 14 * 864e5;   // 画面や説明書には書かない（止まったときだけ理由を出す）
  function get() { try { return +localStorage.getItem(KEY) || 0; } catch (e) { return 0; } }
  function set(t) { try { localStorage.setItem(KEY, String(t)); } catch (e) {} }
  if (!get()) set(Date.now());                                       // 初めて開いたとき（一式を受け取った時点で通信できている）
  function lock(title, msg, canRetry) {
    var el = document.getElementById('md_lock');
    if (!el) { el = document.createElement('div'); el.id = 'md_lock'; document.body.appendChild(el); }
    el.innerHTML = '<div class="md-lock-box"><img src="icons/icon-192.png" width="64" height="64" alt=""><h1>' + title + '</h1><p>' + msg + '</p>' +
      (canRetry ? '<button class="md-btn md-btn-primary" id="md_lock_retry">もう一度確かめる</button>' : '') + '<p class="md-lock-note">保存したプロジェクトは消えていません。</p></div>';
    if (canRetry) document.getElementById('md_lock_retry').onclick = function () { this.textContent = '確かめています…'; check(true); };
  }
  function unlock() { var el = document.getElementById('md_lock'); if (el) el.remove(); }
  function evaluate() {
    var last = get(), now = Date.now(), left = last + LIMIT - now;
    if (now + 36e5 < last) left = -1;                                // 端末の時計が戻されているときは確かめ直しを求める
    if (left <= 0) {
      lock('インターネットにつないでください', 'このアプリは、2 週間に 1 回以上、インターネットにつないだ状態で開く必要があります。最後に確かめてから 2 週間を過ぎたため、今は機能を使えません。インターネットにつないでから「もう一度確かめる」を押してください。', true);
      return;
    }
    unlock();
  }
  async function check(manual) {
    try {
      var r = await fetch('ping.json?t=' + Date.now(), { cache: 'no-store' });
      if (r.ok) {
        var d = await r.json();
        if (d && d.active === false) { lock('公開を終了しました', esc2(d.message || 'このアプリの公開は終了しました。'), false); return; }
        set(Date.now());
      }
    } catch (e) { /* 通信できないときは、覚えている日時で判断する */ }
    evaluate();
    if (manual && document.getElementById('md_lock_retry')) document.getElementById('md_lock_retry').textContent = 'もう一度確かめる';
  }
  function esc2(s) { return String(s).replace(/[&<>"]/g, function (c) { return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]; }); }
  evaluate();
  check(false);
  window.addEventListener('online', function () { check(false); });
  document.addEventListener('visibilitychange', function () { if (document.visibilityState === 'visible') check(false); });
  setInterval(function () { check(false); }, 60 * 60 * 1000);
  window.CTS_ONLINE = { check: check, last: get };
})();
