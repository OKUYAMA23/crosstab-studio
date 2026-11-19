#!/usr/bin/env python3
"""i18n.py ― CrossTab Studio の英語化の道具

  python3 tools/i18n.py wrap  FILE...   日本語の文字列を _t('…')（JS）/ tr_("…")（R）で包む（一度だけ使う）
  python3 tools/i18n.py extract         包んだ文字列を集め、i18n/en.json に足す（訳は空のまま）
  python3 tools/i18n.py missing [N]     訳がない文字列を、使われている場所と一緒に表示する
  python3 tools/i18n.py gen             en.json から app/www/js/i18n_en.js と app/R/i18n_en.R を作る

辞書の鍵は日本語の文字列そのもの。訳がない文字列は日本語のまま表示される。
"""
import re, sys, json, os, glob
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DICT = os.path.join(ROOT, 'i18n', 'en.json')
JA = re.compile(r'[\u3040-\u30ff\u4e00-\u9fff\uff01-\uff60\u3000-\u303f\u2460-\u24ff]')
JS_FILES = ['app/www/js/i18n.js', 'app/www/js/changelog.js', 'app/www/js/core.js', 'app/www/js/view.js', 'app/www/js/events.js', 'app/www/js/chart.js', 'app/www/js/xlsx.js',
            'app/www/js/textmine.js', 'app/www/js/pdfreport.js', 'app/www/js/docxreport.js', 'web/bridge.js', 'web/web.js',
            'web/phone.js', 'web/touch.js']
R_FILES = ['app/R/data.R', 'app/R/stats.R', 'app/R/analyses.R', 'app/R/analyses2.R', 'app/R/excel.R', 'app/R/rexport.R', 'app/R/server.R']


def tokens(src, is_r):
    """(start, end, kind, text) を返す。kind: str / punct / word。コメントは飛ばす"""
    out = []; i = 0; n = len(src)
    while i < n:
        c = src[i]
        if c in ' \t\r\n': i += 1; continue
        if not is_r and c == '/' and src[i + 1:i + 2] == '/':
            j = src.find('\n', i); i = n if j < 0 else j; continue
        if not is_r and c == '/' and src[i + 1:i + 2] == '*':
            j = src.find('*/', i + 2); i = n if j < 0 else j + 2; continue
        if is_r and c == '#':
            j = src.find('\n', i); i = n if j < 0 else j; continue
        if c in '\'"' or (c == '`' and not is_r):
            q = c; j = i + 1
            while j < n and src[j] != q:
                j += 2 if src[j] == '\\' else 1
            out.append((i, j + 1, 'str', src[i + 1:j])); i = j + 1; continue
        if not is_r and c == '/':           # 正規表現の literal を飛ばす（直前が演算子や括弧のとき）
            prev = out[-1] if out else None
            if prev is None or (prev[2] == 'punct' and prev[3] in '(,=:[!&|?{};+') or (prev[2] == 'word' and prev[3] in ('return', 'typeof')):
                j = i + 1; inclass = False
                while j < n and src[j] != '\n':
                    if src[j] == '\\': j += 2; continue
                    if src[j] == '[': inclass = True
                    elif src[j] == ']': inclass = False
                    elif src[j] == '/' and not inclass: break
                    j += 1
                if j < n and src[j] == '/':
                    j += 1
                    while j < n and src[j].isalpha(): j += 1
                    out.append((i, j, 'regex', src[i:j])); i = j; continue
        m = re.match(r'[A-Za-z_.$][A-Za-z0-9_.$]*', src[i:i + 80])
        if m: out.append((i, i + m.end(), 'word', m.group())); i += m.end(); continue
        m = re.match(r'===|!==|==|!=|<-|%in%|%%|&&|\|\||<=|>=|=>|\[\[|\]\]', src[i:i + 4])
        if m: out.append((i, i + m.end(), 'punct', m.group())); i += m.end(); continue
        out.append((i, i + 1, 'punct', c)); i += 1
    return out


CMP = {'===', '!==', '==', '!=', '%in%', 'case'}
SKIPFN_JS = {'indexOf', 'lastIndexOf', 'includes', 'split', 'replace', 'test', 'match', 'startsWith', 'endsWith', 'getItem', 'setItem', 'querySelector', 'querySelectorAll', 'getElementById'}
SKIPFN_R = {'grepl', 'gsub', 'sub', 'regexpr', 'gregexpr', 'strsplit', 'startsWith', 'endsWith', 'match', 'identical', 'regmatches', 'chartr', 'fixed', 'iconv', 'Encoding', 'readLines', 'file.path', 'tr_'}


def wrap(path):
    is_r = path.endswith('.R'); src = open(path, encoding='utf-8').read(); tk = tokens(src, is_r)
    fn = 'tr_' if is_r else '_t'; edits = []; skipped = []
    for k, (a, b, kind, text) in enumerate(tk):
        if kind != 'str' or not JA.search(text): continue
        if src[a] == '`': skipped.append(('template', text)); continue
        p1 = tk[k - 1] if k else (0, 0, '', ''); p2 = tk[k - 2] if k > 1 else (0, 0, '', '')
        nx = tk[k + 1] if k + 1 < len(tk) else (0, 0, '', '')
        why = None
        if p1[3] in CMP or nx[3] in CMP: why = 'compare'
        elif p1[3] == '(' and p2[3] in (SKIPFN_R if is_r else SKIPFN_JS): why = 'fn ' + p2[3]
        elif p1[3] == ',' and p2[2] == 'regex': why = 'replace-arg'
        elif not is_r and nx[3] == ':' and p1[3] in ('{', ','): why = 'key'
        elif is_r and nx[3] == '=' : why = 'name'
        elif is_r and p1[3] == '[[': why = 'index'
        elif not is_r and p1[3] == '[' and p2[2] in ('word', 'punct') and p2[3] not in ('(', ',', '[', '=', ':', '?', 'return', '{', '+'): why = 'index'
        if why: skipped.append((why, text)); continue
        edits.append((a, b))
    for a, b in reversed(edits):
        src = src[:a] + fn + '(' + src[a:b] + ')' + src[b:]
    open(path, 'w', encoding='utf-8').write(src)
    return len(edits), skipped


def unesc(s, q):
    return re.sub(r'\\(u[0-9a-fA-F]{4}|.)', lambda m: chr(int(m.group(1)[1:], 16)) if m.group(1)[0] == 'u' and len(m.group(1)) == 5 else {'n': '\n', 't': '\t'}.get(m.group(1), m.group(1)), s)


def keys_in(path):
    is_r = path.endswith('.R'); src = open(path, encoding='utf-8').read(); tk = tokens(src, is_r)
    fns = ('tr_', 'trf') if is_r else ('_t', '_k'); out = []
    for k, (a, b, kind, text) in enumerate(tk):
        if kind == 'str' and k > 1 and tk[k - 1][3] == '(' and tk[k - 2][3] in fns:
            line = src.count('\n', 0, a) + 1
            out.append((unesc(text, src[a]), '%s:%d' % (os.path.relpath(path, ROOT), line)))
    return out


def load():
    return json.load(open(DICT, encoding='utf-8')) if os.path.exists(DICT) else {}


def save(d):
    os.makedirs(os.path.dirname(DICT), exist_ok=True)
    json.dump(d, open(DICT, 'w', encoding='utf-8'), ensure_ascii=False, indent=0, sort_keys=True)


def all_keys():
    ks = {}
    files = [os.path.join(ROOT, f) for f in JS_FILES + R_FILES]
    out = os.environ.get('CTS_OUT')
    if out: files += glob.glob(os.path.join(out, 'js', '*.js'))
    for f in files:
        if os.path.exists(f):
            for k, where in keys_in(f): ks.setdefault(k, []).append(where)
    return ks


if __name__ == '__main__':
    cmd = sys.argv[1]
    if cmd == 'wrap':
        for f in sys.argv[2:]:
            n, sk = wrap(f); print('%s: %d wrapped, %d skipped' % (f, n, len(sk)))
            for why, t in sk: print('   [%s] %s' % (why, t[:90]))
    elif cmd == 'extract':
        d = load(); ks = all_keys(); new = 0
        for k in ks:
            if k not in d: d[k] = ''; new += 1
        unused = [k for k in d if k not in ks]
        save(d); print('keys %d, new %d, unused %d, untranslated %d' % (len(ks), new, len(unused), sum(1 for k in ks if not d.get(k))))
        if '-v' in sys.argv:
            for k in unused: print('  unused:', k[:80])
    elif cmd == 'missing':
        d = load(); ks = all_keys(); lim = int(sys.argv[2]) if len(sys.argv) > 2 else 50
        pat = sys.argv[3] if len(sys.argv) > 3 else ''
        miss = [(k, w) for k, w in ks.items() if not d.get(k) and pat in w[0]]
        def loc(w): f, l = w[0].rsplit(':', 1); return (f, int(l))
        miss.sort(key=lambda x: loc(x[1]))
        last = None; batch = []
        for k, w in miss[:lim]:
            f, l = loc(w)
            if (f, l) != last:
                last = (f, l); src = open(os.path.join(ROOT, f), encoding='utf-8').read().split('\n')[l - 1].strip()
                print('# %s:%d  %s' % (f, l, src[:110] if len(src) > 0 else ''))
            batch.append(k); print('%d\t%s' % (len(batch), json.dumps(k, ensure_ascii=False)))
        json.dump(batch, open('/tmp/i18n_batch.json', 'w', encoding='utf-8'), ensure_ascii=False)
        print('# missing', len(miss))
    elif cmd == 'apply':
        # 訳を「番号<TAB>訳」の行で書いたファイルを、直前の missing の一覧に当てる。訳の中の \n は改行にする
        batch = json.load(open('/tmp/i18n_batch.json', encoding='utf-8')); d = load(); n = 0
        for line in open(sys.argv[2], encoding='utf-8').read().split('\n'):
            if not line.strip() or line.startswith('#'): continue
            i, v = line.split('\t', 1); k = batch[int(i) - 1]
            if v.startswith('@'):   # @日本語=>English || 日本語=>English … ：鍵の中の日本語の部分だけを置き換える（HTML の長い鍵のため）
                out = k
                for pair in v[1:].split(' || '):
                    a, b = pair.split('=>', 1)
                    assert a in out, (i, a)
                    out = out.replace(a, b)
                d[k] = out; n += 1; continue
            d[k] = re.sub(r'\\(\\|n)', lambda m: '\\' if m.group(1) == '\\' else '\n', v); n += 1
        save(d); print('applied', n, 'of', len(batch))
    elif cmd == 'set':
        # 鍵を指定して直す：set '日本語' 'English'
        d = load(); d[sys.argv[2]] = sys.argv[3]; save(d)
    elif cmd == 'gen':
        d = {k: v for k, v in load().items() if v}
        js = '// i18n_en.js ― 英語の辞書（tools/i18n.py gen で i18n/en.json から作る。直接は直さない）\nwindow.CTS_EN = ' + json.dumps(d, ensure_ascii=False, indent=0, sort_keys=True) + ';\n'
        open(os.path.join(ROOT, 'app/www/js/i18n_en.js'), 'w', encoding='utf-8').write(js)
        def rq(s): return '"' + s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t') + '"'
        r = '# i18n_en.R ― 英語の辞書（tools/i18n.py gen で i18n/en.json から作る。直接は直さない）\nEN_DICT <- list2env(list(\n' + ',\n'.join('%s = %s' % (rq(k), rq(v)) for k, v in sorted(d.items())) + '\n), hash = TRUE)\n'
        open(os.path.join(ROOT, 'app/R/i18n_en.R'), 'w', encoding='utf-8').write(r)
        print('entries', len(d))
