# data.R ― CSV の読み込みと、列の種類（単一回答・順序・複数回答・数値・自由記述）の判定
# Google フォームの回答（スプレッドシートから書き出した CSV）を想定している。

# 利用者に見せるエラー（原因と次にすることを日本語で書く）
stop_user <- function(...) {
  stop(structure(class = c("user_error", "error", "condition"),
                 list(message = paste0(...), call = NULL)))
}

# 尺度の外にある回答（順序の検定では既定で除く）
SPECIAL_PATTERN <- "わからない|分からない|判断できない|該当しない|回答したくない|答えたくない|無回答|覚えていない|don't know|do not know|not sure|unsure|prefer not|rather not|no answer|not applicable|^n/?a$"   # 英語の回答にも対応（大文字・小文字は区別しない）

# 順序のある選択肢の辞書。先頭が「上位」（程度が大きい・頻度が高い・肯定的）。
SCALES <- list(
  c("とてもあてはまる", "非常にあてはまる", "あてはまる", "ややあてはまる", "どちらかといえばあてはまる",
    "どちらともいえない", "どちらでもない", "どちらかといえばあてはまらない", "あまりあてはまらない",
    "あてはまらない", "まったくあてはまらない", "全くあてはまらない"),
  c("とてもそう思う", "非常にそう思う", "そう思う", "ややそう思う", "どちらかといえばそう思う",
    "どちらともいえない", "どちらでもない", "どちらかといえばそう思わない", "あまりそう思わない",
    "そう思わない", "まったくそう思わない", "全くそう思わない"),
  c("いつもある", "よくある", "何度もある", "ときどきある", "時々ある", "たまにある", "一度ある",
    "あまりない", "ほとんどない", "まったくない", "全くない", "一度もない"),
  c("ほぼ毎日", "毎日", "週に5～6日", "週に3～4日", "週に1～2日", "週に1日程度", "月に数日",
    "月に1日以下", "ほとんど利用しない", "利用していない", "まったく利用していない"),
  c("増えると思う", "やや増えると思う", "変わらないと思う", "やや減ると思う", "減ると思う"),
  c("増えた", "やや増えた", "変わらない", "やや減った", "減った"),
  c("とても満足", "満足", "やや満足", "どちらともいえない", "やや不満", "不満", "とても不満"),
  c("とても良い", "良い", "やや良い", "ふつう", "普通", "どちらともいえない", "やや悪い", "悪い", "とても悪い"),
  c("賛成", "どちらかといえば賛成", "どちらともいえない", "どちらかといえば反対", "反対"),
  c("とても好き", "好き", "どちらかといえば好き", "どちらともいえない", "どちらかといえば嫌い", "嫌い", "とても嫌い"),
  c("よく知っている", "どのような機能か知っている", "内容を知っている", "知っている", "少し知っている",
    "名前だけ知っている", "聞いたことがある", "あまり知らない", "知らない", "まったく知らない"),
  c("とても重要", "重要", "やや重要", "どちらともいえない", "あまり重要でない", "重要でない"),
  c("とても感じる", "感じる", "やや感じる", "どちらともいえない", "あまり感じない", "感じない", "まったく感じない"),
  # 英語の回答（大文字・小文字と空白は区別しない）
  c("strongly agree", "agree", "somewhat agree", "slightly agree", "neither agree nor disagree", "neutral", "undecided",
    "slightly disagree", "somewhat disagree", "disagree", "strongly disagree"),
  c("very satisfied", "satisfied", "somewhat satisfied", "neither satisfied nor dissatisfied", "neutral",
    "somewhat dissatisfied", "dissatisfied", "very dissatisfied"),
  c("always", "almost always", "very often", "often", "frequently", "sometimes", "occasionally", "rarely", "seldom", "almost never", "never"),
  c("extremely important", "very important", "important", "moderately important", "somewhat important", "slightly important",
    "not very important", "not important", "not at all important"),
  c("excellent", "very good", "good", "fair", "average", "poor", "very poor"),
  c("very likely", "likely", "somewhat likely", "neutral", "somewhat unlikely", "unlikely", "very unlikely"),
  c("very much", "quite a bit", "a lot", "somewhat", "a little", "not much", "not at all"),
  c("extremely interested", "very interested", "interested", "somewhat interested", "slightly interested", "not very interested",
    "not interested", "not at all interested"),
  c("every day", "daily", "almost every day", "several times a week", "once a week", "several times a month", "once a month",
    "less than once a month", "rarely", "never"),
  c("love", "like", "neutral", "dislike", "hate"),
  c("like very much", "like", "neither like nor dislike", "dislike", "dislike very much"),
  c("very high", "high", "moderate", "low", "very low"),
  c("very easy", "easy", "neutral", "difficult", "very difficult"),
  c("increased", "slightly increased", "no change", "unchanged", "slightly decreased", "decreased")
)
# 2 択（名義として扱うが、並びは決めておく）
BINARIES <- list(c("はい", "いいえ"), c("ある", "ない"), c("いる", "いない"), c("あり", "なし"),
                 c("知っている", "知らない"), c("思う", "思わない"), c("できる", "できない"),
                 c("している", "していない"), c("yes", "no"), c("true", "false"))

norm_label <- function(x) {
  x <- chartr("０１２３４５６７８９〜~－", "0123456789～～-", x)
  tolower(gsub("[[:space:]　]", "", x))
}

# バイト列を UTF-8 の文字列にする（BOM 付き UTF-8、UTF-8、Shift_JIS(CP932) を判定）
bytes_to_text <- function(raw) {
  if (length(raw) >= 3 && identical(as.integer(raw[1:3]), c(239L, 187L, 191L))) raw <- raw[-(1:3)]
  if (length(raw) >= 2 && identical(as.integer(raw[1:2]), c(255L, 254L))) {
    txt <- iconv(list(raw[-(1:2)]), from = "UTF-16LE", to = "UTF-8")
    if (!is.na(txt)) return(txt)
  }
  txt <- rawToChar(raw[raw != as.raw(0)])
  Encoding(txt) <- "UTF-8"
  if (validUTF8(txt)) return(txt)
  conv <- iconv(list(raw), from = "CP932", to = "UTF-8")
  if (is.na(conv)) stop_user(tr_("文字コードを判定できませんでした。CSV を UTF-8 で保存し直してください。"))
  conv
}

read_survey_csv <- function(raw, filename = "") {
  if (length(raw) >= 4 && identical(as.integer(raw[1:2]), c(80L, 75L))) {
    stop_user(tr_("Excel ブック（.xlsx）または ZIP は読み込めません。スプレッドシートから「CSV」でダウンロードしたファイルを入れてください。"))
  }
  if (length(raw) == 0) stop_user(tr_("ファイルが空です。回答が入った CSV を入れてください。"))
  txt <- bytes_to_text(raw)
  first <- sub("\r?\n.*", "", substr(txt, 1, 5000))
  sep <- if (grepl("\\.tsv$", filename, ignore.case = TRUE) ||
             (lengths(regmatches(first, gregexpr("\t", first))) > lengths(regmatches(first, gregexpr(",", first))))) "\t" else ","
  df <- tryCatch(
    utils::read.table(text = txt, sep = sep, header = TRUE, quote = "\"", colClasses = "character",
                      check.names = FALSE, na.strings = character(0), fill = TRUE, comment.char = "",
                      encoding = "UTF-8", stringsAsFactors = FALSE, blank.lines.skip = TRUE),
    error = function(e) stop_user(tr_("CSV として読めませんでした（"), conditionMessage(e), tr_("）。Google スプレッドシートの「ファイル → ダウンロード → カンマ区切り形式（.csv）」で保存したファイルを使ってください。")))
  if (ncol(df) < 1 || nrow(df) < 1) stop_user(tr_("回答の行がありません。1 行目が質問、2 行目以降が回答の CSV を入れてください。"))
  for (j in seq_along(df)) {
    v <- df[[j]]
    Encoding(v) <- "UTF-8"
    v <- trimws(v)
    v[v == ""] <- NA_character_
    df[[j]] <- v
  }
  nm <- names(df); Encoding(nm) <- "UTF-8"
  nm[is.na(nm) | nm == ""] <- paste0(tr_("列"), which(is.na(nm) | nm == ""))
  names(df) <- nm
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  if (nrow(df) < 1) stop_user(tr_("回答の行がありません。"))
  df
}

# 質問文から表示名を作る。省略（…）はしない。
#   ふつうの質問：「Q5 SNSやインターネット上で知り合い、…友人がいますか。」のように、1 行目を最後まで使う
#   表形式の質問（末尾が [項目]）：「Q8 最近1か月、孤独だと感じる。」のように、番号と項目を使う。
#   項目が短い（サービス名など）ときは、何を聞いたかが分かるよう問いの最初の文を（ ）で添える
# 質問文を「番号・問い・項目」に分ける。表形式の質問をまとめて扱う解析で使う
question_parts <- function(h) {
  first <- trimws(gsub("[[:space:]\u3000]+", " ", sub("[\r\n].*", "", h)))
  flat <- trimws(gsub("[[:space:]\u3000]+", " ", h))
  m <- regmatches(flat, regexec("\\[([^\\]]*)\\]\\s*$", flat, perl = TRUE))[[1]]
  item <- if (length(m) == 2 && nzchar(trimws(m[2]))) trimws(m[2]) else ""
  stem <- trimws(sub("\\s*\\[[^\\]]*\\]\\s*$", "", first, perl = TRUE))
  list(item = item, stem = if (nzchar(item)) stem else "")
}
question_label <- function(h) {
  flat <- trimws(gsub("[[:space:]\u3000]+", " ", h))
  first <- trimws(gsub("[[:space:]\u3000]+", " ", sub("[\r\n].*", "", h)))
  item <- NA_character_
  m <- regmatches(flat, regexec("\\[([^\\]]*)\\]\\s*$", flat, perl = TRUE))[[1]]
  if (length(m) == 2 && nzchar(trimws(m[2]))) item <- trimws(m[2])
  body <- trimws(sub("\\s*\\[[^\\]]*\\]\\s*$", "", first, perl = TRUE))
  code <- NA_character_
  m2 <- regmatches(body, regexec("^\\s*([QqＱ問]\\s*[0-9０-９]+[A-Za-z]?)", body, perl = TRUE))[[1]]
  if (length(m2) == 2) { code <- gsub("\\s", "", m2[2]); body <- trimws(sub("^\\s*[QqＱ問]\\s*[0-9０-９]+[A-Za-z]?[\\s\u3000.．:：、]*", "", body, perl = TRUE)) }
  if (!is.na(item)) {
    lab <- item
    if (nchar(item) < 10 && nzchar(body)) lab <- paste0(item, "（", sub("^([^。？?]*[。？?]).*$", "\\1", body), "）")
    return(if (is.na(code)) lab else paste(code, lab))
  }
  if (!nzchar(body)) return(if (is.na(code)) flat else code)
  if (is.na(code)) body else paste(code, body)
}

split_multi <- function(x, delim) {
  lapply(strsplit(x, delim, fixed = TRUE), function(t) { t <- trimws(t); unique(t[!is.na(t) & t != ""]) })
}

guess_delim <- function(x) {
  nn <- x[!is.na(x)]
  if (length(nn) < 3) return(NA_character_)
  for (d in c(";", ", ", "、")) {
    has <- grepl(d, nn, fixed = TRUE)
    if (mean(has) < 0.05 && sum(has) < 5) next
    tok <- unique(unlist(split_multi(nn, d)))
    if (length(tok) <= 30 && length(tok) < length(unique(nn))) return(d)
  }
  NA_character_
}

# 数の範囲の選択肢を小さい順に並べる。並べられないときは NULL
#   すべての選択肢が数を 1 つ（ちょうどの値、「〜以下・未満」「〜以上・超」）か 2 つ（範囲）含み、
#   数以外の部分（単位など）が同じで、範囲どうしが重ならないときだけ並べる
range_order <- function(x) {
  if (length(x) < 2) return(NULL)
  y <- gsub("(?<=[0-9]),(?=[0-9]{3})", "", x, perl = TRUE)               # 桁区切りのコンマを外す
  # すべてに共通する頭の部分（「1日1回」「1日2回」の「1日」）は比べる対象から外す
  ch <- strsplit(y, ""); m <- min(lengths(ch)); p <- 0
  while (p < m && length(unique(vapply(ch, `[`, "", p + 1))) == 1) p <- p + 1
  while (p > 0 && grepl("[0-9.]", ch[[1]][p + 1])) p <- p - 1                # 数の途中では切らない
  if (p > 0) y <- substring(y, p + 1)
  nums <- regmatches(y, gregexpr("[0-9]+(\\.[0-9]+)?", y))
  k <- lengths(nums); if (any(k < 1 | k > 2)) return(NULL)
  low <- grepl("以下|未満|まで|以内|より少な|less|under|below|fewer", y, ignore.case = TRUE)
  high <- grepl("以上|超|より多|or more|more than|over|above", y, ignore.case = TRUE)
  if (any(low & high) || any(k == 2 & (low | high))) return(NULL)
  rest <- gsub("[0-9]+(\\.[0-9]+)?|以下|未満|まで|以内|より少ない|以上|超|より多い|程度|くらい|ぐらい|約|[-～~ー–—・/]|or less|less than|under|below|fewer than|or more|more than|over|above", "", y, ignore.case = TRUE)
  rest <- gsub("[[:space:]]", "", rest)
  repeat { r2 <- gsub("(.+)\\1", "\\1", rest, perl = TRUE); if (identical(r2, rest)) break; rest <- r2 }   # 「円～円」の「円円」を「円」にする
  if (length(unique(rest)) != 1) return(NULL)
  lo <- hi <- numeric(length(y))
  for (i in seq_along(y)) {
    v <- as.numeric(nums[[i]])
    if (k[i] == 2) { if (v[1] > v[2]) return(NULL); lo[i] <- v[1]; hi[i] <- v[2] }
    else if (low[i]) { lo[i] <- -Inf; hi[i] <- v }
    else if (high[i]) { lo[i] <- v; hi[i] <- Inf }
    else lo[i] <- hi[i] <- v
  }
  o <- order(lo, hi)
  if (anyDuplicated(lo[o]) || any(hi[o][-length(o)] > lo[o][-1])) return(NULL)   # 範囲が重なるときは並べない
  o
}

# 頻度の選択肢を、1 週間あたりの回数に直して多い順に並べる（「ほぼ毎日」を先頭にする既存の頻度の尺度と同じ向き）。並べられないときは NULL
#   すべての選択肢を回数に直せて、回数がすべて異なり、期間（日・週・月・年）を含む選択肢があるときだけ並べる
freq_order <- function(x) {
  if (length(x) < 3 || !any(grepl("毎日|日|週|月|年", x))) return(NULL)
  key <- vapply(x, function(s) {
    s <- gsub("(?<=[にで週月年日])数(?=[回日])", "3", s, perl = TRUE)        # 「週に数回」は 3 回とみなす
    day <- NA_real_
    if (grepl("[0-9]+日に?[0-9]", s)) { day <- as.numeric(sub("^.*?([0-9]+)日に?[0-9].*$", "\\1", s)); s <- sub("[0-9]+日に?(?=[0-9])", "", s, perl = TRUE) }   # 「1日に2回」
    v <- as.numeric(regmatches(s, gregexpr("[0-9]+(\\.[0-9]+)?", s))[[1]])
    adj <- if (grepl("以下|未満|より少な", s)) -0.5 else if (grepl("以上|より多|超", s)) 0.5 else 0
    if (!length(v)) {
      if (grepl("まったく|全く|一度も|したことがない|使ったことがない", s)) return(0)
      if (grepl("ほとんど", s) && grepl("ない", s)) return(0.1)
      if (!grepl("あまり|たまに|時々|ときどき", s) && grepl("しない|使わない|行かない|やらない|なし$|ない$", s)) return(0)
      if (grepl("ほぼ毎日", s)) return(6)
      if (grepl("毎日", s)) return(7)
      if (grepl("毎週", s)) return(1)
      if (grepl("毎月", s)) return(1 / 4.345)
      if (grepl("毎年", s)) return(1 / 52)
      return(NA_real_)
    }
    if (length(v) > 2) return(NA_real_)
    n <- mean(v)
    per <- if (!is.na(day)) 7 / day else if (grepl("週", s)) 1 else if (grepl("月", s)) 1 / 4.345 else if (grepl("年", s)) 1 / 52 else if (grepl("日", s)) 7 else NA_real_
    (n + adj) * per
  }, 0)
  if (anyNA(key) || anyDuplicated(key)) return(NULL)
  order(-key)
}

# 選択肢の並びを決める。戻り値：list(order = 並べた選択肢, ordinal = 順序があるか)
order_categories <- function(vals, counts) {
  nv <- norm_label(vals)
  special <- grepl(SPECIAL_PATTERN, vals, ignore.case = TRUE)
  core <- which(!special)
  put <- function(idx) c(idx, which(special)[order(-counts[special])])
  if (length(core) == 0) return(list(order = order(-counts), ordinal = FALSE))
  # 数値だけの選択肢（1〜5 の段階など）：大きい値を上位にする
  num <- suppressWarnings(as.numeric(nv[core]))
  if (!anyNA(num) && length(core) >= 2) return(list(order = put(core[order(-num)]), ordinal = TRUE))
  # 数字以外が同じ形（高校1年・高校2年 など）：数字の小さい順
  if (length(core) >= 2 && all(grepl("[0-9]", nv[core]))) {
    shape <- gsub("[0-9]+(\\.[0-9]+)?", "#", nv[core])
    if (length(unique(shape)) == 1 && lengths(regmatches(shape[1], gregexpr("#", shape[1]))) == 1) {
      n1 <- as.numeric(regmatches(nv[core], regexpr("[0-9]+(\\.[0-9]+)?", nv[core])))
      if (!anyDuplicated(n1)) return(list(order = put(core[order(n1)]), ordinal = TRUE))
    }
  }
  # 数の範囲の選択肢（499円以下・500-2,999円・10,000円以上、1日1回 など）：範囲の小さい順（「高校1年」の規則と同じ向き）
  rk <- range_order(nv[core])
  if (!is.null(rk)) return(list(order = put(core[rk]), ordinal = TRUE))
  for (sc in SCALES) {
    pos <- match(nv[core], norm_label(sc))
    if (!anyNA(pos) && length(core) >= 2) return(list(order = put(core[order(pos)]), ordinal = TRUE))
  }
  # 頻度の選択肢（ほぼ毎日・週に2～3回程度・週に1回以下・まったく使用しない など）：多い順
  fk <- freq_order(nv[core])
  if (!is.null(fk)) return(list(order = put(core[fk]), ordinal = TRUE))
  for (sc in BINARIES) {
    pos <- match(nv[core], norm_label(sc))
    if (!anyNA(pos)) return(list(order = put(core[order(pos)]), ordinal = FALSE))
  }
  list(order = put(core[order(-counts[core])]), ordinal = FALSE)
}

category_info <- function(x, type, delim) {
  if (type == "multi") {
    tok <- unlist(split_multi(x[!is.na(x)], delim))
  } else tok <- x[!is.na(x)]
  if (length(tok) == 0) return(list(cats = list(), ordinal = FALSE))
  tb <- table(tok)
  vals <- names(tb); Encoding(vals) <- "UTF-8"; counts <- as.integer(tb)
  if (type == "multi") {
    sp <- grepl(SPECIAL_PATTERN, vals, ignore.case = TRUE)
    ord <- c(which(!sp)[order(-counts[!sp])], which(sp)[order(-counts[sp])]); ordinal <- FALSE
  } else {
    oc <- order_categories(vals, counts); ord <- oc$order; ordinal <- oc$ordinal
  }
  list(cats = lapply(ord, function(i) list(v = vals[i], n = counts[i], special = grepl(SPECIAL_PATTERN, vals[i], ignore.case = TRUE))),
       ordinal = ordinal)
}

to_number <- function(x) suppressWarnings(as.numeric(gsub(",", "", chartr("０１２３４５６７８９．－", "0123456789.-", x))))

# ---------------------------------------------------------------- 数値の回答を直す（データクリーニング）
# 数字で答える質問には「１５歳」「約3時間」「3～4」「わからない」のような回答が混ざる。
# 直せるものは直し（全角→半角、単位・「約」「くらい」を外す、範囲は中央の値、「万」は 10000 倍）、直せないものは欠損にする。
# status：ok＝そのまま読めた、fixed＝直した、range＝範囲を中央の値にした、fail＝数値にできない、empty＝回答なし
NUM_UNITS <- "(歳|才|人|名|時間|分間|分|秒|円|回|日間|日|冊|本|個|点|件|週間|週|か月|ヶ月|ヵ月|カ月|ケ月|年間|年|台|枚|杯|km|kg|cm|mm|m|g|%)$"
clean_number <- function(x, unit = NULL) {
  x <- as.character(x); n <- length(x); unit <- if (is.null(unit) || !length(unit)) NA_character_ else as.character(unit)
  val <- rep(NA_real_, n); status <- rep("empty", n)
  z <- trimws(x); has <- !is.na(z) & z != ""
  direct <- suppressWarnings(as.numeric(z))
  okd <- has & !is.na(direct); val[okd] <- direct[okd]; status[okd] <- "ok"
  rest <- which(has & !okd)
  if (length(rest)) {
    w <- chartr("０１２３４５６７８９．＋，％〜～－", "0123456789.+,%~~-", z[rest])
    w <- gsub("[[:space:]　]", "", w)
    w <- gsub("(?<=[0-9]),(?=[0-9]{3})", "", w, perl = TRUE)
    w <- sub("^(約|およそ|だいたい|大体|ほぼ|おおよそ)", "", w)
    w <- sub("(くらい|ぐらい|位|程度|前後|ほど)$", "", w)
    w <- sub("^(about|around|approx\\.?|approximately|roughly|nearly|almost)", "", w, ignore.case = TRUE)   # 英語の回答
    w <- sub("(ish|orso)$", "", w, ignore.case = TRUE)
    # 時間と分：質問が「何分」なら「1時間」→ 60、「何時間」なら「30分」→ 0.5 に直す（質問の単位がわからないときは数字だけを使う）
    hm <- regmatches(w, regexec("^([0-9]+(?:\\.[0-9]+)?)時間(半|([0-9]+)分)?$", w)); ish <- lengths(hm) == 4
    hv <- rep(NA_real_, length(w)); if (any(ish)) hv[ish] <- vapply(hm[ish], function(v) as.numeric(v[2]) + if (v[3] == "半") 0.5 else if (nzchar(v[4])) as.numeric(v[4]) / 60 else 0, 0)
    mm <- regmatches(w, regexec("^([0-9]+(?:\\.[0-9]+)?)分(間)?$", w)); ism <- lengths(mm) >= 2 & !ish
    mv <- rep(NA_real_, length(w)); if (any(ism)) mv[ism] <- vapply(mm[ism], function(v) as.numeric(v[2]), 0)
    for (k in 1:2) {
      w <- sub("(くらい|ぐらい|位|程度|前後|ほど|以上|以下|未満|超|弱|強|頃|ごろ)$", "", w)
      w <- sub(NUM_UNITS, "", w)
      w <- sub("(minutes?|mins?|hours?|hrs?|yearsold|years?|yrs?|times|people|persons?|days?|weeks?|months?|books?|yen|dollars?|usd|ormore|orless|plus|\\+)$", "", w, ignore.case = TRUE)
    }
    man <- grepl("^-?[0-9]+(\\.[0-9]+)?万$", w)
    v <- suppressWarnings(as.numeric(sub("万$", "", w))); v[man] <- v[man] * 10000
    rg <- regmatches(w, regexec("^(-?[0-9]+(?:\\.[0-9]+)?)[^0-9.]{0,4}?(?:~|-|ー|から|－|to)(-?[0-9]+(?:\\.[0-9]+)?)[^0-9]*$", w))
    isr <- lengths(rg) == 3 & is.na(v)
    if (any(isr)) v[isr] <- vapply(rg[isr], function(m) mean(as.numeric(m[2:3])), 0)
    if (identical(unit, "min")) { v[ish] <- hv[ish] * 60 }
    else if (identical(unit, "hour")) { v[ish] <- hv[ish]; v[ism] <- mv[ism] / 60 }
    else v[ish & is.na(v)] <- hv[ish & is.na(v)]
    val[rest] <- v
    status[rest] <- ifelse(is.na(v), "fail", ifelse(isr, "range", "fixed"))
  }
  list(value = val, status = status)
}
# 利用者が直した値（fix：回答 → 値。値が null なら除外）を反映した数値
numeric_values <- function(x, fix = NULL, unit = NULL) {
  cn <- clean_number(x, unit); v <- cn$value
  if (length(fix)) {
    keys <- names(fix); idx <- match(as.character(x), keys); hit <- which(!is.na(idx))
    if (length(hit)) v[hit] <- vapply(fix[idx[hit]], function(f) if (is.null(f) || !length(f) || is.na(suppressWarnings(as.numeric(f)))) NA_real_ else as.numeric(f), 0)
  }
  v
}
# 数値の列の読み取りの記録（直した回答と、数値にできない回答の一覧）
clean_info <- function(x, unit = NULL) {
  cn <- clean_number(x, unit); st <- cn$status
  pick <- function(s, lim = 300) {
    i <- which(st %in% s); if (!length(i)) return(list())
    raw <- as.character(x[i]); tb <- sort(table(raw), decreasing = TRUE); tb <- tb[seq_len(min(lim, length(tb)))]
    vals <- cn$value[i][match(names(tb), raw)]
    lapply(seq_along(tb), function(k) list(raw = names(tb)[k], n = as.integer(tb[k]), val = if (is.na(vals[k])) NULL else vals[k]))
  }
  list(nOk = sum(st == "ok"), nFixed = sum(st %in% c("fixed", "range")), nFail = sum(st == "fail"),
       fixed = pick(c("fixed", "range")), fail = pick("fail"))
}

# 列の種類：nominal（単一回答）/ ordinal（順序のある単一回答）/ multi（複数回答）/ numeric（数値）
#           text（自由記述）/ time（日時）/ empty（回答なし）
detect_variable <- function(x, header) {
  nn <- x[!is.na(x)]
  n <- length(nn); nu <- length(unique(nn))
  base <- list(type = "empty", delim = ";", n = n, nUnique = nu, cats = list())
  if (n == 0) return(base)
  if (grepl("タイムスタンプ|timestamp", header, ignore.case = TRUE) ||
      mean(grepl("^[0-9]{4}[/-][0-9]{1,2}[/-][0-9]{1,2}([ T].*)?$", nn)) > 0.9) { base$type <- "time"; return(base) }
  # 数値：そのまま、または直して数値として読める回答が 8 割以上で、値の種類が多い（または質問文が数を尋ねている）
  unit <- if (grepl("何分|（分）|\\(分\\)|分くらい|分程度|分間", header) || grepl("minutes|\\(min\\)|\\bin min", header, ignore.case = TRUE)) "min"
          else if (grepl("何時間|（時間）|\\(時間\\)|時間くらい|時間程度", header) || grepl("\\bhours\\b|\\(h\\)|\\(hours?\\)", header, ignore.case = TRUE)) "hour" else NULL
  cn <- clean_number(nn, unit); okshare <- mean(!is.na(cn$value)); numv <- cn$value[!is.na(cn$value)]
  asks_num <- grepl("何歳|年齢|何人|何時間|何分|何回|何日|何冊|何円|金額|時間数|人数|回数|半角数字|数字で|数値で", header) ||
              grepl("how (many|much|old|long)|\\bage\\b|number of|in minutes|in hours|enter a number|numbers? only", header, ignore.case = TRUE)
  # 質問文が数を尋ねていても、「1～2人」のような選択肢（そのままでは数値でない回答が半数以上）は選択肢として扱う
  if (okshare >= 0.8 && (length(unique(numv)) > 11 || (asks_num && mean(cn$status == "ok") >= 0.5 && length(unique(numv)) >= 2))) {
    base$type <- "numeric"
    base$summary <- list(min = min(numv), max = max(numv), mean = mean(numv), median = stats::median(numv))
    base$clean <- clean_info(nn, unit); base$unit <- unit
    return(base)
  }
  if (okshare >= 0.5 && length(unique(numv)) >= 2) { base$numCand <- TRUE; base$clean <- clean_info(nn, unit); base$unit <- unit }   # 数値として読み直せる列
  says_multi <- grepl("複数回答|複数選択|すべて選|全て選|いくつでも", header) || grepl("select all|check all|choose all|all that apply|multiple answers", header, ignore.case = TRUE)
  d <- guess_delim(x)
  if (!is.na(d) || says_multi) {
    if (is.na(d)) d <- ";"
    info <- category_info(x, "multi", d)
    if (length(info$cats) <= 40) { base$type <- "multi"; base$delim <- d; base$cats <- info$cats; return(base) }
  }
  if (nu > 20 || (nu > 12 && nu / n > 0.5)) { base$type <- "text"; return(base) }
  info <- category_info(x, "nominal", ";")
  base$type <- if (info$ordinal) "ordinal" else "nominal"
  base$cats <- info$cats
  base
}

describe_data <- function(df) {
  labs <- vapply(names(df), question_label, "")
  labs <- make.unique(unname(labs), sep = " ")
  vars <- lapply(seq_along(df), function(j) {
    info <- detect_variable(df[[j]], names(df)[j])
    qp <- question_parts(names(df)[j])
    c(list(col = j, header = names(df)[j], label = labs[j], full = labs[j], item = qp$item, stem = qp$stem), info)
  })
  vars
}
