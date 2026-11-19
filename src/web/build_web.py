# iPad 版（webR）の一式を dist_web に組み立てる。画面のプログラムと R のコードはパソコン版と同じものを使う。
import os, shutil, re
# 使い方：python web/build_web.py（出力先は環境変数 CTS_OUT、webR 0.6.1 の dist の場所は WEBR_DIST で変えられる）
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__))); WEB = ROOT + '/web'
OUT = os.environ.get('CTS_OUT', os.path.join(ROOT, 'dist_web'))
WEBR = os.environ.get('WEBR_DIST', os.path.join(ROOT, 'node_modules', 'webr', 'dist'))
shutil.rmtree(OUT, ignore_errors=True); os.makedirs(OUT + '/R')
shutil.copytree(ROOT + '/app/www/css', OUT + '/css'); shutil.copytree(ROOT + '/app/www/js', OUT + '/js')
for d in ['vendor', 'fonts']: shutil.copytree(ROOT + '/app/www/' + d, OUT + '/' + d)
for f in ['web.css', 'phone.css']: shutil.copy(WEB + '/' + f, OUT + '/css/' + f)
for f in ['bridge.js', 'touch.js', 'web.js', 'phone.js']: shutil.copy(WEB + '/' + f, OUT + '/js/' + f)
import re as _re
VERSION = _re.search(r"v: '([0-9.]+)'", open(ROOT + '/app/www/js/changelog.js', encoding='utf-8').read()).group(1)
_b = open(OUT + '/js/bridge.js', encoding='utf-8').read().replace('__CTS_VERSION__', VERSION); open(OUT + '/js/bridge.js', 'w', encoding='utf-8').write(_b)
for f in ['i18n.R', 'i18n_en.R', 'data.R', 'stats.R', 'analyses.R', 'analyses2.R', 'excel.R', 'rexport.R', 'server.R']: shutil.copy(ROOT + '/app/R/' + f, OUT + '/R/' + f)
for f in ['web_json.R', 'web_server.R']: shutil.copy(WEB + '/' + f, OUT + '/R/' + f)
# webR：この一式で使うもの（npm の webr 0.6.1。配布元の整合性ハッシュと一致することを確かめたもの）だけを入れる
# 置き場（Claude のページ）が .so と拡張子なしのファイルを置けないため、.so は .so.wasm に名前を変え、webR の worker の
# locateFile（ファイルの場所を決める関数）だけを書き換えて読ませる。R のコードは .R の代わりに .R.txt で置く。
KEEP = ['R.js', 'R.wasm', 'libRblas.so', 'libRlapack.so', 'webr.js', 'webr-worker.js', 'LICENSE.md', 'vfs/usr/lib/R/library/translations/DESCRIPTION']
n = 0; size = 0
for rel in KEEP:
    src = os.path.join(WEBR, rel) if rel != 'LICENSE.md' else os.path.join(WEBR, '..', 'LICENSE.md')
    dst = OUT + '/webr/' + (rel + '.wasm' if rel.endswith('.so') else rel + '.txt' if rel.endswith('/DESCRIPTION') else rel); os.makedirs(os.path.dirname(dst), exist_ok=True); shutil.copy(src, dst)
    n += 1; size += os.path.getsize(dst)
wk = OUT + '/webr/webr-worker.js'; t = open(wk, encoding='utf-8').read()
OLD = 'c.locateFile=t=>J.baseUrl+t,'
assert t.count(OLD) == 1
t = t.replace(OLD, 'c.locateFile=t=>J.baseUrl+t.replace(/\\.so$/,".so.wasm").replace(/\\/DESCRIPTION$/,"/DESCRIPTION.txt"),'); open(wk, 'w', encoding='utf-8').write(t)
for f in os.listdir(OUT + '/R'): os.rename(OUT + '/R/' + f, OUT + '/R/' + f + '.txt')
# 文言を iPad 版に合わせる（パソコン版のファイルは変えない）
WORDS = {
  'view.js': [("<h1>ここに CSV ファイルをドロップ</h1>", "<h1>CSV ファイルを選ぶ</h1>"),
              ("データはこのパソコンの中だけで処理され、外部には送信されません。", "データはこの端末の中だけで処理され、外部には送信されません。"),
              ("回答のファイルを画面にドロップします。", "「CSV を開く」で回答のファイルを選びます。"),
              ("1 つのプロジェクトとしてアプリの中に保存します。", "1 つのプロジェクトとしてこの端末（ブラウザの中）に保存します。ブラウザのデータを消すと消えるので、大事なものは「ファイルに書き出す」でも保存してください。"),
              ("集計と検定はすべて、このパソコンの中の R", "集計と検定はすべて、この端末のブラウザの中で動く R"),
              ("プロジェクトを 1 つのファイル（.prj）にして、好きな場所に保存します。別のパソコンに移すときに使います。", "プロジェクトを 1 つのファイル（.prj）にして保存します。別の端末やパソコン版でも開けます。")],
  'events.js': [("アプリのフォルダーの「projects」に保存します。", "この端末（ブラウザの中）に保存します。")],
  'core.js': [("別のパソコンに持っていくときに使えます。", "パソコン版でも開けます。")],
}
for fn, pairs in WORDS.items():
    t = open(OUT + '/js/' + fn, encoding='utf-8').read()
    for a2, b2 in pairs:
        assert a2 in t, (fn, a2); t = t.replace(a2, b2)
    open(OUT + '/js/' + fn, 'w', encoding='utf-8').write(t)
html = open(ROOT + '/app/www/index.html', encoding='utf-8').read()
html = html.replace('<link rel="stylesheet" href="css/app.css">', '<link rel="stylesheet" href="css/app.css">\n<link rel="stylesheet" href="css/web.css">\n<link rel="stylesheet" href="css/phone.css">')
html = html.replace('<title>CrossTab Studio</title>', '<title>CrossTab Studio</title>\n<link rel="manifest" href="manifest.webmanifest">\n<link rel="apple-touch-icon" href="icons/apple-touch-icon.png">\n<link rel="icon" type="image/png" href="icons/icon-192.png">\n<meta name="theme-color" content="#217346">\n<meta name="apple-mobile-web-app-capable" content="yes">\n<meta name="mobile-web-app-capable" content="yes">\n<meta name="apple-mobile-web-app-title" content="CrossTab">\n<meta name="apple-mobile-web-app-status-bar-style" content="default">')
boot = '''<div id="md_boot"><img src="icons/icon-192.png" width="76" height="76" alt="">
<h1>CrossTab Studio</h1><p data-i18n>アンケートの集計と検定</p><div class="md-boot-bar"><i></i></div>
<div class="md-boot-row"><span class="md-boot-msg" data-i18n>起動しています</span><span class="md-boot-sec"></span></div>
<div class="md-boot-steps"><div data-step="1" data-i18n>計算に使う R を読み込んでいます</div><div data-step="2" data-i18n>集計と検定の部品を読み込んでいます</div><div data-step="3" data-i18n>画面を用意しています</div></div>
<div class="md-boot-note"></div></div>
'''
html = html.replace('<div class="md-app">', boot + '<div class="md-app">', 1)
html = html.replace('<script src="js/icons.js"></script>', '<script src="js/bridge.js"></script>\n<script src="js/touch.js"></script>\n<script src="js/icons.js"></script>')
html = html.replace('<script src="js/events.js"></script>', '<script src="js/events.js"></script>\n<script src="js/web.js"></script>\n<script src="js/phone.js"></script>')
open(OUT + '/index.html', 'w', encoding='utf-8').write(html)
# ホーム画面のアプリ（PWA）の設定：マニフェスト、アイコン、一式を端末に保存する sw.js
os.makedirs(OUT + '/icons')
for f in os.listdir(WEB + '/pwa'):
    if f.endswith('.png'): shutil.copy(WEB + '/pwa/' + f, OUT + '/icons/' + f)
shutil.copy(WEB + '/pwa/manifest.webmanifest', OUT + '/manifest.webmanifest')
open(OUT + '/.nojekyll', 'w').close()
# ソースを公開しているので、読みにくくする処理はしない（R のコードは R/*.R.txt のまま置き、bridge.js が読み込む）
import json, hashlib
open(OUT + '/ping.json', 'w').write('{"active": true}')
files = sorted(os.path.relpath(os.path.join(dp, f), OUT).replace(os.sep, '/') for dp, dn, fn in os.walk(OUT) for f in fn if not f.startswith('.') and f != 'ping.json')
fonts = [f for f in files if f.startswith('fonts/') and f.endswith('.ttf')]; files = [f for f in files if f not in fonts]   # 字体は別に保存する（sw.js）
wfiles = [f for f in files if f.startswith('webr/')]; files = [f for f in files if f not in wfiles]   # R 本体（webR）も別に保存する（版の更新で取り直さない）
h = hashlib.md5(b''.join(open(OUT + '/' + f, 'rb').read() for f in files)).hexdigest()[:8]
hw = hashlib.md5(b''.join(open(OUT + '/' + f, 'rb').read() for f in wfiles)).hexdigest()[:8]
sw = open(WEB + '/pwa/sw.template.js', encoding='utf-8').read().replace('__VERSION__', VERSION + '-' + h).replace('__FILES__', json.dumps(['./'] + files)).replace('__FONTS__', json.dumps(fonts)).replace('__WEBR_VERSION__', hw).replace('__WEBR__', json.dumps(wfiles))
open(OUT + '/sw.js', 'w', encoding='utf-8').write(sw)
print('webr files', n, round(size / 1e6, 1), 'MB', '| cache', VERSION + '-' + h, len(files), 'files | webr cache', hw, len(wfiles), 'files')
