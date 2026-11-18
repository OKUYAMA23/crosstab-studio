// sw.js ― アプリの一式を端末に保存して、通信なしでも起動できるようにする（ホーム画面のアプリ用）。
// 版が変わると CACHE の名前が変わり、新しい一式を保存し直して古いものを消す。
const CACHE = 'cts-__VERSION__';
const FILES = __FILES__;
// 字体（PDF の報告書に使う大きなファイル）は版ごとに保存し直さず、別の入れ物に 1 回だけ保存する（更新のたびに数十 MB を取り直さないため）
const FONT_CACHE = 'cts-fonts-1';
const FONTS = __FONTS__;
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
