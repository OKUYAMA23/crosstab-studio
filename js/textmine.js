// textmine.js ― テキストマイニングの前処理：自由記述を単語に分けて、解析の依頼（req）に付ける。
//   単語の分割は、ブラウザに組み込まれた日本語の分かち書き（Intl.Segmenter、ICU の辞書）を使う。使えない古いブラウザでは、
//   文字の種類（漢字・カタカナ・英字・ひらがな）の切れ目で分ける。
(function () {
  'use strict';
  var A = window.App;
  var SEG = null; try { if (window.Intl && Intl.Segmenter) SEG = new Intl.Segmenter('ja', { granularity: 'word' }); } catch (e) { SEG = null; }
  var KK = /^[一-鿿㐀-䶿々〆ヶ゠-ヿ]+$/, HIRA = /^[぀-ゟー]+$/, KANJI1 = /^[一-鿿㐀-䶿々]$/;
  // 意味の薄い語（助詞・助動詞・形式名詞・よく使う動詞や副詞など）
  var STOP = new Set(('こと もの ため よう それ これ あれ どれ ここ そこ あそこ どこ いる ある する なる できる 思う 思い 思っ 思います 感じ 感じる ない いい よい 良い です ます でした ました けど ' +
    'でも から ので って とか など なんか もっと とても すごく ちょっと 特に 特 なし 特になし 自分 人 時 方 的 等 私 僕 俺 皆 みんな 今 中 上 下 前 後 他 何 気 事 物 所 点 化 性 さん ' +
    'ください ところ という といった について による により として そう どう こう ああ いう いっ 言う くる いく 行く 来る みる 見る あり なり し さ れる られる せる たい だろう でしょう まし ' +
    'なかっ かっ くれ もらう あげ すぎ ぐらい くらい ほど まで より だけ しか ばかり 及び また さらに しかし ただ そして 今後 今回 回答 わから わかり 分から 分かり 少し 多く 多い あまり 色々 いろいろ ' +
    'ほしい 欲しい 必要 大切 大事 よく 良く 無い みたい だけど だけ しまう しまい ほう ほうが 出る 出 行き 行っ 変わっ 変 たち 達 いつ いつも ずっと やっぱり やはり すごい とき 場合 ' +
    // 英語の回答のための語（冠詞・代名詞・前置詞・接続詞・助動詞・よく使う動詞や副詞など）
    'a an the and or but if so as of at by for from in into on onto to with without about above below over under up down out off than then too very ' +
    'i me my mine myself we us our ours you your yours he him his she her hers it its they them their theirs this that these those who whom whose which what ' +
    'is am are was were be been being have has had having do does did doing done will would shall should can could may might must ' +
    "not no nor yes don't doesn't didn't isn't aren't wasn't weren't can't cannot couldn't won't wouldn't i'm i've i'd i'll it's that's there's " +
    'there here when where why how all any both each few more most other some such only own same just also again further once ever never ' +
    'get got make made go going went think thought feel felt like really much many lot lots thing things something anything nothing way well good ' +
    'one two because while during before after since until through between however although though etc').split(/\s+/));
  function tokenize(text, compound, stop) {
    var s = String(text || '').normalize('NFKC').toLowerCase().trim(); if (!s) return [];
    var segs = [];
    if (SEG) { var pos = 0; Array.from(SEG.segment(s)).forEach(function (x) { segs.push({ t: x.segment, w: x.isWordLike, i: x.index }); }); }
    else { var re = /[一-鿿㐀-䶿々〆ヶ]+|[゠-ヿ]+|[a-z][a-z0-9'\-]*|[0-9]+|[぀-ゟ]+|[\s\S]/g, m; while ((m = re.exec(s))) segs.push({ t: m[0], w: /[\w぀-ヿ一-鿿]/.test(m[0]), i: m.index }); }
    var toks = [];
    segs.forEach(function (g) {
      if (!g.w) { toks.push(null); return; }
      var prev = toks.length ? toks[toks.length - 1] : null;
      if (compound && prev && KK.test(g.t) && KK.test(prev.t) && prev.end === g.i) { prev.t += g.t; prev.end = g.i + g.t.length; return; }
      toks.push({ t: g.t, end: g.i + g.t.length });
    });
    return toks.filter(function (x) {
      if (!x) return false; var t = x.t;
      if (STOP.has(t) || stop.has(t)) return false;
      if (/^[0-9.,%]+$/.test(t)) return false;
      if (HIRA.test(t) && t.length < 3) return false;
      if (t.length === 1 && !KANJI1.test(t)) return false;
      return true;
    }).map(function (x) { return x.t; });
  }
  // runTab から呼ばれる：テキストマイニングのときだけ、自由記述を取り寄せて単語に分ける
  A.prepReq = async function (tab, req) {
    if (req.type !== 'textmine') return req;
    var d = await A.apiData('/api/coltext', { t: req.slots.t, g: req.slots.g || null, filter: req.filter });
    if (!d.ok) throw new Error(d.message || _t('自由記述を読み込めませんでした。'));
    var o = req.opts || {}, compound = o.compound !== false, stop = new Set(String(o.stop || '').split(/[、,，\s]+/).map(function (w) { return w.normalize('NFKC').toLowerCase().trim(); }).filter(Boolean));
    var docs = d.text.map(function (t) { return tokenize(t, compound, stop); });
    var out = Object.assign({}, req, { docs: docs, seg: SEG ? 'icu' : 'chartype', nstop: STOP.size, stopUser: Array.from(stop) });   // 報告書の「方法」に書くため
    if (d.group) out.grp = d.group;
    return out;
  };
  A.tokenize = tokenize;
})();
