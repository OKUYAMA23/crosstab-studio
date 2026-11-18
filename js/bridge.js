// bridge.js ― iPad 版（webR）用。パソコン版では R のローカルサーバーに送っていた /api/… の要求を、
// ブラウザの中で動く R（webR）に渡す。画面側のプログラム（core.js など）はパソコン版と同じものを使う。
// プロジェクトは端末のブラウザの中（IndexedDB）に保存する。
(function () {
  'use strict';
  var R_FILES = ['data.R', 'stats.R', 'analyses.R', 'analyses2.R', 'excel.R', 'server.R', 'web_json.R', 'web_server.R'];
  var realFetch = window.fetch.bind(window), webR = null, rVersion = null, rYear = null, bootErr = null;
  var bootStep = function () {};
  window.CTS_WEB = true;
  window.CTS_VERSION = '0.9.2';

  // ---------------- R（webR）を起動する
  // ホーム画面のアプリとして開いたときは、先に一式を端末に保存する（2 回目からは通信なしで起動できる）
  async function ensureOffline() {
    if (!('serviceWorker' in navigator) || window.CTS_NO_SW) return;
    try {
      var sw = navigator.serviceWorker, updating = false;
      sw.addEventListener('message', function (e) {
        var d = e.data || {}; if (d.type === 'cts-cache') bootStep(0, (updating ? '新しい版を読み込んでいます（' : 'アプリをこの端末に保存しています（') + d.done + ' / ' + d.total + '）');
      });
      var had = !!sw.controller, reg = await sw.register('sw.js');
      // 保存済みの版で開いたときは、R を起動する前に新しい版があるかを確かめる。
      // あれば読み込み画面のまま新しい版を保存し、すぐに読み込み直す（古い版の画面を出してから切り替える二度手間をなくす）
      if (had) {
        bootStep(0, '新しい版がないか確かめています');
        var found = await bootUpdate(sw, reg, function () { updating = true; bootStep(0, '新しい版を読み込んでいます'); });
        if (found) { location.reload(); await new Promise(function () {}); }
      }
      watchUpdate(sw, reg, had);
      if (had) return;
      bootStep(0, 'アプリをこの端末に保存しています（初回だけ 20 MB ほど）');
      await sw.ready;
      if (!sw.controller) await new Promise(function (res) { sw.addEventListener('controllerchange', res, { once: true }); setTimeout(res, 5000); });
    } catch (e) { /* 保存できない場所（Claude のページなど）では、そのまま起動する */ }
  }
  // 起動時の版の確認：通信できないときや確認に時間がかかるとき（4 秒）は、保存済みの版で起動する。
  //   新しい版が見つかったら、保存が終わって切り替わるまで待つ（最大 3 分。失敗したら保存済みの版で起動）
  async function bootUpdate(sw, reg, onFound) {
    try {
      if (!navigator.onLine) return false;
      await Promise.race([reg.update(), new Promise(function (res) { setTimeout(res, 4000); })]);
      var nw = reg.installing || reg.waiting; if (!nw) return false;
      onFound();
      return await new Promise(function (res) {
        var t = setTimeout(function () { res(false); }, 180000);
        sw.addEventListener('controllerchange', function () { clearTimeout(t); res(true); }, { once: true });
        nw.addEventListener('statechange', function () { if (nw.state === 'redundant') { clearTimeout(t); res(false); } });
        if (nw.state === 'activated') { clearTimeout(t); res(true); }
      });
    } catch (e) { return false; }
  }
  // ---------------- 新しい版が出たとき
  //   ホーム画面のアプリは、閉じずに裏へ回しただけだと読み込み直さないので、新しい版を保存し終えても古い画面のまま動き続ける。
  //   新しい版の保存が終わったら、データを開いていなければすぐに切り替え、開いていれば画面の上に「更新する」を出す
  function watchUpdate(sw, reg, had) {
    sw.addEventListener('controllerchange', function () { if (had) newVersionReady(); had = true; });
    function check() { if (reg && reg.update) reg.update().catch(function () {}); }
    document.addEventListener('visibilitychange', function () { if (document.visibilityState === 'visible') check(); });
    window.addEventListener('focus', check);
    setInterval(check, 30 * 60 * 1000);
  }
  function newVersionReady() {
    var S = window.App && window.App.S;
    if (!S || !S.data) { location.reload(); return; }
    if (document.getElementById('md_update')) return;
    var bar = document.createElement('div'); bar.id = 'md_update';
    bar.innerHTML = '<span>新しい版を用意しました。更新すると切り替わります。</span><button class="md-btn md-btn-primary md-btn-sm">更新する</button><button class="md-btn md-btn-sm" data-later>あとで</button>';
    document.body.appendChild(bar);
    var go = bar.querySelector('.md-btn-primary');
    go.onclick = function () {
      if (S.dirty && !go.dataset.sure) { go.dataset.sure = '1'; go.textContent = '保存していない変更は消えます。更新する'; return; }
      location.reload();
    };
    bar.querySelector('[data-later]').onclick = function () { bar.remove(); };
  }
  var ready = (async function () {
    try {
      await ensureOffline();
      bootStep(1, '計算に使う R を読み込んでいます（初回は 20 MB ほどの読み込みがあります）');
      var mod = await import(new URL('webr/webr.js', document.baseURI).href);
      webR = new mod.WebR({ baseUrl: new URL('webr/', document.baseURI).href, channelType: mod.ChannelType.PostMessage, interactive: false });
      await webR.init();
      bootStep(2, '集計と検定の部品を読み込んでいます');
      try { webR.FS.mkdir('/cts'); } catch (e) {}
      for (var j = 0; j < R_FILES.length; j++) {
        var src = await (await realFetch('R/' + R_FILES[j] + '.txt', { cache: 'no-cache' })).text();
        await webR.FS.writeFile('/cts/' + R_FILES[j], new TextEncoder().encode(src));
      }
      await webR.evalRVoid('for (f in c(' + R_FILES.map(function (f) { return '"' + f + '"'; }).join(',') + ')) eval(parse(file = file.path("/cts", f), encoding = "UTF-8", keep.source = FALSE), envir = globalenv())');
      rVersion = await webR.evalRString('paste(R.version$major, R.version$minor, sep = ".")'); rYear = await webR.evalRString('R.version$year');
      bootStep(3, '画面を用意しています');
      return true;
    } catch (e) { bootErr = e; bootStep(-1, String(e && e.message || e)); throw e; }
  })();
  window.CTS_BOOT = { ready: ready, onStep: function (f) { bootStep = f; }, evalString: async function (code) { await ready; return webR.evalRString(code); } };

  // ---------------- JSON の値を R の式に直す（jsonlite::fromJSON(simplifyVector = FALSE) と同じ形）
  function rStr(s) { return JSON.stringify(String(s)).replace(/\u0000/g, ''); }
  function toR(v) {
    if (v === null || v === undefined) return 'NULL';
    if (typeof v === 'boolean') return v ? 'TRUE' : 'FALSE';
    if (typeof v === 'number') { if (!isFinite(v)) return 'NA'; return Number.isInteger(v) && Math.abs(v) < 2147483647 ? v + 'L' : String(v); }
    if (typeof v === 'string') return rStr(v);
    if (Array.isArray(v)) return 'list(' + v.map(toR).join(',') + ')';
    var ks = Object.keys(v);
    if (!ks.length) return 'structure(list(), names = character(0))';
    return 'list(' + ks.map(function (k) { return rStr(k) + '=' + toR(v[k]); }).join(',') + ')';
  }

  // ---------------- 要求を 1 つずつ R に渡す（同時に来ても順番に処理する）
  var queue = Promise.resolve();
  function callR(path, qs, bytes, jsonText) {
    var job = queue.then(async function () {
      await ready;
      await webR.FS.writeFile('/tmp/cts_req.bin', bytes || new Uint8Array(0));
      var code = jsonText ? toR(JSON.parse(jsonText)) : 'list()';
      await webR.FS.writeFile('/tmp/cts_req.R', new TextEncoder().encode(code));
      var status = await webR.evalRNumber('web_handle(' + rStr(path) + ',' + rStr(qs) + ', "POST")');
      var out = await webR.FS.readFile('/tmp/cts_res.bin');
      return new Response(out, { status: status, headers: { 'Content-Type': 'application/json; charset=utf-8' } });
    });
    queue = job.catch(function () {});
    return job;
  }
  function jsonResp(o, status) { return new Response(JSON.stringify(o), { status: status || 200, headers: { 'Content-Type': 'application/json; charset=utf-8' } }); }

  // ---------------- 文字コード：Shift_JIS の CSV は、ここで UTF-8 に直してから R に渡す
  function toUtf8Bytes(bytes) {
    try { new TextDecoder('utf-8', { fatal: true }).decode(bytes); return bytes; } catch (e) {}
    if (bytes.length >= 2 && ((bytes[0] === 0xFF && bytes[1] === 0xFE) || (bytes[0] === 0xFE && bytes[1] === 0xFF))) return bytes;   // UTF-16 は R が読む
    try { return new TextEncoder().encode(new TextDecoder('shift_jis').decode(bytes)); } catch (e) { return bytes; }
  }

  // ---------------- プロジェクトの置き場（この端末のブラウザの中）
  var memStore = {}, dbP = null;
  function db() {
    if (dbP) return dbP;
    dbP = new Promise(function (res) {
      try {
        var rq = indexedDB.open('CrossTabStudio', 1);
        rq.onupgradeneeded = function () { rq.result.createObjectStore('projects', { keyPath: 'name' }); };
        rq.onsuccess = function () { res(rq.result); }; rq.onerror = function () { res(null); };
      } catch (e) { res(null); }
    });
    return dbP;
  }
  async function store(mode, fn) {
    var d = await db();
    if (!d) return fn(null);
    return new Promise(function (res, rej) {
      try { var tx = d.transaction('projects', mode), os = tx.objectStore('projects'), r = fn(os); tx.oncomplete = function () { res(r && r.result !== undefined ? r.result : r); }; tx.onerror = function () { rej(tx.error); }; }
      catch (e) { rej(e); }
    });
  }
  function projName(x) { return String(x || '').replace(/[\\/:*?"<>|\u0000-\u001F]/g, '_').replace(/^\.+/, '').trim().slice(0, 80); }
  function stamp(t) { var d = new Date(t), p = function (n) { return (n < 10 ? '0' : '') + n; }; return d.getFullYear() + '-' + p(d.getMonth() + 1) + '-' + p(d.getDate()) + ' ' + p(d.getHours()) + ':' + p(d.getMinutes()); }
  async function projApi(path, qs, bytes, body) {
    if (path === '/api/proj/save') {
      var nm = projName(new URLSearchParams(qs).get('name'));
      if (!nm) return jsonResp({ ok: false, message: 'プロジェクトの名前を入れてください。' });
      var rec = { name: nm, saved: Date.now(), text: new TextDecoder().decode(bytes) };
      try { var d = await db(); if (d) await store('readwrite', function (os) { return os.put(rec); }); else memStore[nm] = rec; }
      catch (e) { return jsonResp({ ok: false, message: 'この端末に保存できませんでした（' + e.message + '）。「ファイルに書き出す」で保存してください。' }); }
      return jsonResp({ ok: true, name: nm, folder: 'この端末のブラウザの中' });
    }
    if (path === '/api/proj/list') {
      var all = [];
      try { var d2 = await db(); all = d2 ? await store('readonly', function (os) { return os.getAll(); }) : Object.values(memStore); } catch (e) { all = Object.values(memStore); }
      all.sort(function (a, b) { return b.saved - a.saved; });
      return jsonResp({ ok: true, folder: 'この端末のブラウザの中', items: all.map(function (r) { return { name: r.name, saved: stamp(r.saved), size: r.text.length }; }) });
    }
    var b = body ? JSON.parse(body) : {}, key = projName(b.name);
    if (path === '/api/proj/load') {
      var r2 = null;
      try { var d3 = await db(); r2 = d3 ? await store('readonly', function (os) { return os.get(key); }) : memStore[key]; } catch (e) {}
      if (!r2) return jsonResp({ ok: false, message: 'プロジェクト「' + key + '」が見つかりません。' });
      return new Response(r2.text, { headers: { 'Content-Type': 'application/json; charset=utf-8' } });
    }
    if (path === '/api/proj/delete') {
      try { var d4 = await db(); if (d4) await store('readwrite', function (os) { return os.delete(key); }); else delete memStore[key]; } catch (e) {}
      return jsonResp({ ok: true });
    }
    return jsonResp({ ok: false, message: '見つかりません。' }, 404);
  }

  // ---------------- /api/… の要求を横取りする
  async function bodyBytes(body) {
    if (body === undefined || body === null) return new Uint8Array(0);
    if (typeof body === 'string') return new TextEncoder().encode(body);
    if (body instanceof Uint8Array) return body;
    if (body instanceof ArrayBuffer) return new Uint8Array(body);
    if (body instanceof Blob) return new Uint8Array(await body.arrayBuffer());
    return new TextEncoder().encode(String(body));
  }
  window.fetch = async function (url, opt) {
    var u = typeof url === 'string' ? url : url && url.url;
    if (!u || u.indexOf('/api/') !== 0) return realFetch(url, opt);
    opt = opt || {};
    var i = u.indexOf('?'), path = i >= 0 ? u.slice(0, i) : u, qs = i >= 0 ? u.slice(i + 1) : '';
    var bytes = await bodyBytes(opt.body), isJson = !/octet-stream/.test((opt.headers && opt.headers['Content-Type']) || '') && typeof opt.body === 'string';
    if (path === '/api/ping') {
      if (!rVersion) throw new Error(bootErr ? 'R を起動できませんでした' : 'R を起動しています');
      return jsonResp({ ok: true, app: 'CrossTabStudio', version: '0.5.0-web', r: rVersion, ryear: rYear });
    }
    if (path === '/api/bye') return jsonResp({ ok: true });
    if (path.indexOf('/api/proj/') === 0) return projApi(path, qs, bytes, isJson ? opt.body : null);
    if (path === '/api/load') bytes = toUtf8Bytes(bytes);
    try { return await callR(path, qs, bytes, isJson ? opt.body : null); }
    catch (e) { return jsonResp({ ok: false, message: '計算の途中でエラーが起きました（' + (e && e.message || e) + '）。' }); }
  };
  var realBeacon = navigator.sendBeacon ? navigator.sendBeacon.bind(navigator) : null;
  try { navigator.sendBeacon = function (u, d) { if (String(u).indexOf('/api/') === 0) return true; return realBeacon ? realBeacon(u, d) : false; }; } catch (e) {}
})();
