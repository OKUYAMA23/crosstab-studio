// sw.js ― アプリの一式を端末に保存して、通信なしでも起動できるようにする（ホーム画面のアプリ用）。
// 版が変わると CACHE の名前が変わり、新しい一式を保存し直して古いものを消す。
const CACHE = 'cts-0.9.0-975a79d5';
const FILES = ["./", "R/analyses.R.txt", "R/analyses2.R.txt", "R/data.R.txt", "R/excel.R.txt", "R/server.R.txt", "R/stats.R.txt", "R/web_json.R.txt", "R/web_server.R.txt", "css/app.css", "css/phone.css", "css/web.css", "fonts/OFL_BIZUDMincho.txt", "fonts/OFL_BIZUDPGothic.txt", "fonts/OFL_Tinos.txt", "icons/apple-touch-icon.png", "icons/icon-192.png", "icons/icon-512.png", "icons/icon-maskable-512.png", "index.html", "js/bridge.js", "js/changelog.js", "js/chart.js", "js/core.js", "js/docxreport.js", "js/events.js", "js/icons.js", "js/pdfreport.js", "js/phone.js", "js/textmine.js", "js/touch.js", "js/view.js", "js/web.js", "js/xlsx.js", "manifest.webmanifest", "vendor/fontkit.umd.min.js", "vendor/harfbuzz-subset.wasm", "vendor/harfbuzzjs.LICENSE.txt", "vendor/pdf-lib.LICENSE.txt", "vendor/pdf-lib.min.js", "webr/LICENSE.md", "webr/R.js", "webr/R.wasm", "webr/libRblas.so.wasm", "webr/libRlapack.so.wasm", "webr/vfs/usr/lib/R/library/translations/DESCRIPTION.txt", "webr/webr-worker.js", "webr/webr.js"];
// 字体（PDF の報告書に使う大きなファイル）は版ごとに保存し直さず、別の入れ物に 1 回だけ保存する（更新のたびに数十 MB を取り直さないため）
const FONT_CACHE = 'cts-fonts-1';
const FONTS = ["fonts/BIZUDMincho-Regular.ttf", "fonts/BIZUDPGothic-Bold.ttf", "fonts/BIZUDPGothic-Regular.ttf", "fonts/Tinos-Bold.ttf", "fonts/Tinos-Regular.ttf"];
async function fillFonts() {
  const c = await caches.open(FONT_CACHE);
  for (const f of FONTS) { try { if (!(await c.match(f))) { const r = await fetch(f); if (r.ok) await c.put(f, r); } } catch (e) {} }
}
async function fontFromCache(req) {
  const c = await caches.open(FONT_CACHE), key = new URL(req.url).pathname;
  let r = await c.match(key);
  if (!r) { r = await fetch(req.url); if (r.ok) await c.put(key, r.clone()); }
  return r;
}
self.addEventListener('install', e => {
  e.waitUntil((async () => {
    const c = await caches.open(CACHE);
    let done = 0;
    await Promise.all(FILES.map(async f => {
      const r = await fetch(f, { cache: 'reload' });
      if (!r.ok) throw new Error(f + ' ' + r.status);
      await c.put(f, r);
      done++;
      const cs = await self.clients.matchAll({ includeUncontrolled: true });
      cs.forEach(cl => cl.postMessage({ type: 'cts-cache', done, total: FILES.length }));
    }));
    await self.skipWaiting();
  })());
});
self.addEventListener('activate', e => {
  e.waitUntil((async () => {
    for (const k of await caches.keys()) if (k !== CACHE && k !== FONT_CACHE) await caches.delete(k);
    await self.clients.claim();
  })());
  fillFonts();
});
// 保存した一式から返す。大きさ（Content-Length）を必ず付ける（webR がファイルの大きさを HEAD で確かめるため）
async function fromCache(req) {
  const url = new URL(req.url);
  const key = url.pathname.endsWith('/') ? url.pathname + 'index.html' : url.pathname;
  const c = await caches.open(CACHE);
  let r = await c.match(key, { ignoreSearch: true });
  if (!r) {
    r = await fetch(new Request(req.url, { method: 'GET', credentials: 'same-origin' }));
    if (!r.ok) return r;
  }
  const body = await r.arrayBuffer();
  const h = new Headers(r.headers);
  h.set('Content-Length', String(body.byteLength)); h.delete('Content-Encoding'); h.delete('Accept-Ranges');
  return new Response(req.method === 'HEAD' ? null : body, { status: 200, headers: h });
}
self.addEventListener('fetch', e => {
  const url = new URL(e.request.url);
  if (url.origin !== location.origin || !['GET', 'HEAD'].includes(e.request.method)) return;
  if (url.pathname.endsWith('/ping.json')) return;                 // 利用の確認は、必ずネットから取る（保存しない）
  if (/\/fonts\/[^/]+\.ttf$/.test(url.pathname)) { e.respondWith(fontFromCache(e.request).catch(() => fetch(e.request))); return; }
  e.respondWith(fromCache(e.request).catch(() => fetch(e.request)));
});
