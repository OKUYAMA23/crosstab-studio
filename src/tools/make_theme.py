# アプリの色（緑・オレンジ・青・赤）を作る。
#   画面の部品に使っている緑系の色を CSS 変数 --t-xxxxxx に置き換え、色ごとの値を css/theme.css に書く。
#   図と表（報告書・論文に使うもの）の色は変えない。
#   使い方：python3 tools/make_theme.py（何度実行しても同じ結果になる）
import re, colorsys, os
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# 画面の部品の緑系の色（chart.js の配色や、表のデザインの色は含めない）
THEMED = ['#14532D', '#1A5C38', '#1E2F26', '#1E2F28', '#1E5C3A', '#1F4030', '#217346', '#4F8A66', '#567565', '#5F7D6C', '#6F8A7B', '#6FA98A',
          '#7DB896', '#7FA08F', '#8CC3A2', '#8FA89A', '#8FBFA3', '#9DB3A7', '#B5D6C2', '#B8C7BE', '#C6E0CF', '#CDE8D6', '#CFE6D8', '#D3E8DA',
          '#D5E3DA', '#DDEFE3', '#E1EDE6', '#E3EFE8', '#E6F2EA', '#E9EFEB', '#EAF3EE', '#EAF5EE', '#EDF6F0', '#F1F8F3', '#F3F7F4', '#F4F8F5',
          '#F4F9F6', '#F7FBF8', '#FAFDFB', '#FBFDFC']
BASE_HUE = 145
# 色ごとの色相。強調の色（主ボタン・選択中のタブなど）は、白い文字が読める濃さの色を直接決める
THEMES = {
  'blue':   {'hue': 214, 'fix': {'#217346': '#2B579A', '#1A5C38': '#234A84'}, 'meta': '#2B579A'},
  'red':    {'hue': 356, 'fix': {'#217346': '#A4373A', '#1A5C38': '#872D30'}, 'meta': '#A4373A'},
}
def var(h): return '--t-' + h[1:].lower()
def rot(h, hue):
    r, g, b = [int(h[i:i + 2], 16) / 255 for i in (1, 3, 5)]
    hh, l, s = colorsys.rgb_to_hls(r, g, b)
    nh = ((hh * 360 - BASE_HUE + hue) % 360) / 360
    r, g, b = colorsys.hls_to_rgb(nh, l, s)
    return '#%02X%02X%02X' % tuple(round(v * 255) for v in (r, g, b))
# 緑（初期）で文字と地の差が足りなかった補足の文字だけ、少し濃くする
BASE_FIX = {'#567565': '#4A685A'}
css = ['/* アプリの色。tools/make_theme.py で作る（手で直さない） */', ':root { ' + ' '.join('%s: %s;' % (var(h), BASE_FIX.get(h, h)) for h in THEMED) + ' }']
for name, t in THEMES.items():
    css.append('html[data-theme="%s"] { ' % name + ' '.join('%s: %s;' % (var(h), t['fix'].get(h, rot(h, t['hue']))) for h in THEMED) + ' }')
# アイコンなどの SVG は色を属性で書いているので、CSS で上書きする（図の中は除く）
for h in THEMED:
    for a in ('fill', 'stroke', 'stop-color'):
        css.append('[%s="%s" i]:not(.md-figwrap *) { %s: var(%s); }' % (a, h, a, var(h)))
open(os.path.join(ROOT, 'app/www/css/theme.css'), 'w', encoding='utf-8').write('\n'.join(css) + '\n')
# CSS の中の色を変数に置き換える
def sub_css(path):
    s = open(path, encoding='utf-8').read(); n0 = s
    for h in THEMED: s = re.sub(re.escape(h) + r'(?![0-9A-Fa-f])', 'var(%s)' % var(h), s, flags=re.I)
    if s != n0: open(path, 'w', encoding='utf-8').write(s); print('css', os.path.relpath(path, ROOT))
for p in ['app/www/css/app.css', 'web/web.css', 'web/phone.css']: sub_css(os.path.join(ROOT, p))
# JS の HTML に直接書いた style の色（style="… #xxxxxx …"）を変数に置き換える
def sub_js_style(path):
    s = open(path, encoding='utf-8').read(); n0 = s
    def fix(m):
        t = m.group(0)
        for h in THEMED: t = re.sub(re.escape(h) + r'(?![0-9A-Fa-f])', 'var(%s)' % var(h), t, flags=re.I)
        return t
    s = re.sub(r'style="[^"]*"', fix, s)
    if s != n0: open(path, 'w', encoding='utf-8').write(s); print('js', os.path.relpath(path, ROOT))
for p in ['app/www/js/view.js', 'app/www/js/events.js']: sub_js_style(os.path.join(ROOT, p))
for name, t in THEMES.items(): print(name, {h: t['fix'].get(h, rot(h, t['hue'])) for h in ['#1F4030', '#567565', '#DDEFE3', '#E6F2EA', '#1E5C3A']})
