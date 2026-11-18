# analyses.R ― 解析の登録簿。解析を 1 つ足すときは register() を 1 回書くだけで、
# 一覧画面・入力欄・計算・定型文がそろう。

ANALYSES <- list()
register <- function(id, name, group, desc, slots, opts = list(), fun) {
  ANALYSES[[id]] <<- list(id = id, name = name, group = group, desc = desc, slots = slots, opts = opts, fun = fun)
}
S <- function(id, label, types, hint = "", exclude_special = FALSE, many = FALSE, optional = FALSE) {
  list(id = id, label = label, types = A(types), hint = hint, excludeSpecial = exclude_special, many = many, optional = optional)
}
O_check <- function(id, label, default = FALSE, hint = "") list(id = id, kind = "check", label = label, default = default, hint = hint)
O_select <- function(id, label, choices, default, hint = "") {
  list(id = id, kind = "select", label = label, default = default, hint = hint,
       choices = lapply(names(choices), function(k) list(value = k, label = choices[[k]])))
}
OPT_RESID <- O_select("residMode", "残差の判定基準", list(z = "有意水準どおり（補正なし）", bonf = "ボンフェローニ法で補正"), "z",
                      "どのセルが多い・少ないかを判定する基準です。補正すると、セルの数が多い表で誤って有意と判定することが減ります。")

hl <- function(k, v) list(k = k, v = v)
cat_types <- c("nominal", "ordinal")
rank_types <- c("ordinal", "numeric")

finish <- function(res, alpha) {
  res$ok <- TRUE
  res$alpha <- alpha
  if (!is.null(res$p) && !is.na(res$p)) res$sig <- res$p < alpha
  if (is.null(res$notes)) res$notes <- character(0)
  res$notes <- A(res$notes)
  res
}

small_n_note <- function(chk) {
  if (chk$share_lt5 > 0.2 || chk$min < 1) {
    paste0("期待度数が 5 未満のセルが ", chk$n_lt5, " 個（", fmt_pct(chk$share_lt5, 0), "%）あります",
           if (chk$min < 1) "（1 未満のセルを含む）" else "",
           "。カイ二乗検定の近似が不正確になる目安（5 未満が 20% 超、または 1 未満がある）に当たります。フィッシャーの正確確率検定を使うか、選択肢をまとめてください。")
  } else NULL
}

# ---------------------------------------------------------------- 単純集計
an_freq <- function(df, req) {
  s <- req$slots$a; alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  res <- list(test = "単純集計")
  if (identical(s$type, "multi")) {
    M <- slot_multi(df, s); ok <- !is.na(M[, 1]); M <- M[ok, , drop = FALSE]
    n <- nrow(M); if (n == 0) stop_user(q(s$label), " に回答がありません。")
    cnt <- colSums(M); pct <- cnt / n
    res$n <- n_info(nrow(df), n)
    res$grids <- list(list(kind = "grid", title = "選択肢ごとの回答数（複数回答）", corner = s$label, rows = A(colnames(M)),
                           cols = A(c("回答数", "回答者に占める割合 (%)")), fmt = A(c("int", "pct")),
                           cells = mat(cbind(cnt, pct)), note = paste0("割合の分母は回答者数（n = ", n, "）。複数回答のため合計は 100% を超えることがある。")))
    res$fig <- list(type = "freq", cats = A(colnames(M)), series = A(s$label), count = mat(cnt), base = n,
                    catTitle = s$label, multi = TRUE)
    o <- order(-cnt)
    res$headline <- list(hl("回答者", as.character(n)), hl("最多", colnames(M)[o[1]]))
    res$report <- paste0(q(s$label), "（複数回答、回答者 n = ", n, "）で最も多く選ばれたのは", q(colnames(M)[o[1]]),
                         "（", cnt[o[1]], "人、", fmt_pct(pct[o[1]]), "%）",
                         if (length(o) > 1) paste0("で、次いで", q(colnames(M)[o[2]]), "（", cnt[o[2]], "人、", fmt_pct(pct[o[2]]), "%）であった。") else "であった。")
    return(finish(res, alpha))
  }
  f <- slot_factor(df, s); f <- f[!is.na(f)]; n <- length(f)
  if (n == 0) stop_user(q(s$label), " に回答がありません。")
  tb <- table(f); cnt <- as.integer(tb); pct <- cnt / n
  res$n <- n_info(nrow(df), n)
  res$grids <- list(list(kind = "grid", title = "度数分布", corner = s$label, rows = A(names(tb)), cols = A(c("度数", "割合 (%)")),
                         fmt = A(c("int", "pct")), cells = mat(cbind(cnt, pct)), total = A(c(n, 1))))
  res$fig <- list(type = "freq", cats = A(names(tb)), series = A(s$label), count = mat(cnt), base = n, catTitle = s$label)
  o <- order(-cnt)
  txt <- paste0(q(s$label), "の回答（n = ", n, "）で最も多かったのは", q(names(tb)[o[1]]), "（", cnt[o[1]], "人、", fmt_pct(pct[o[1]]), "%）",
                if (length(o) > 1) paste0("で、次いで", q(names(tb)[o[2]]), "（", cnt[o[2]], "人、", fmt_pct(pct[o[2]]), "%）であった。") else "であった。")
  res$headline <- list(hl("n", as.character(n)), hl("最多", paste0(names(tb)[o[1]], "（", fmt_pct(pct[o[1]]), "%）")))
  if (isTRUE(opt(req, "gof", FALSE))) {
    if (length(tb) < 2) stop_user("適合度の検定には選択肢が 2 つ以上必要です。")
    ct <- suppressWarnings(stats::chisq.test(tb))
    res$p <- unname(ct$p.value); res$test <- "適合度のカイ二乗検定"
    st <- paste0("χ²(", ct$parameter, ") = ", fmt_num(ct$statistic), ", ", fmt_p(res$p, lead0))
    res$headline <- c(res$headline, list(hl(paste0("χ²(", ct$parameter, ")"), fmt_num(ct$statistic)), hl("p", fmt_p(res$p, lead0, FALSE))))
    res$row <- list(stat = paste0("χ²(", ct$parameter, ") = ", fmt_num(ct$statistic)), effect = "")
    if (any(ct$expected < 5)) res$notes <- "期待度数が 5 未満の選択肢があります。適合度の検定の近似が不正確な可能性があります。"
    txt <- paste0(txt, "すべての選択肢が同じ割合で選ばれるという仮定に対する適合度のカイ二乗検定の結果、回答の偏りは",
                  if (res$p < alpha) "統計的に有意であった" else "統計的に有意ではなかった", "（", st, "）。")
  }
  res$report <- txt
  finish(res, alpha)
}
register("freq", "単純集計", "集計", "1 つの質問について、選択肢ごとの人数と割合を出します。複数回答にも使えます。",
         list(S("a", "集計する質問", c("nominal", "ordinal", "multi"))),
         list(O_check("gof", "回答の偏りを検定する（適合度の検定）", FALSE, "すべての選択肢が同じ割合で選ばれる、という仮定と比べます。単一回答のときだけ使えます。")),
         an_freq)

# ---------------------------------------------------------------- 複数の設問の単純集計
# 同じ選択肢で答える複数の設問（例：サービスごとの利用頻度）を並べて集計する。
# 表は「行＝設問、列＝回答」。図の既定は、回答ごとの棒を設問ごとに色を変えて並べる形。
items_setup <- function(df, slot) {
  cols <- unlist(slot$cols); k <- length(cols)
  if (k < 2) stop_user("比べる設問を 2 つ以上選んでください（現在 ", k, " 個）。")
  if (anyDuplicated(cols)) stop_user("同じ設問が 2 回選ばれています。別の設問を選んでください。")
  if (length(slot$cats) == 0) stop_user("選択肢がすべて外されています。「選択肢の設定」で 1 つ以上にチェックを入れてください。")
  lev <- unique(slot_labels(slot)); keys <- character(0); vals <- character(0)
  for (c in slot$cats) for (v in unlist(c$values)) { keys <- c(keys, v); vals <- c(vals, as.character(c$label)) }
  F <- lapply(cols, function(j) factor(vals[match(df[[j]], keys)], levels = lev))
  items <- unlist(slot$items); stems <- unlist(slot$stems); labels <- unlist(slot$labels); fulls <- unlist(slot$fulls)
  same_stem <- length(items) == k && all(nzchar(items)) && length(unique(stems)) == 1 && nzchar(stems[1])
  names <- if (same_stem) items else labels
  list(cols = cols, k = k, lev = lev, F = F, names = make.unique(names, sep = " "), same_stem = same_stem, stem = if (same_stem) stems[1] else "", fulls = fulls)
}
an_freqmulti <- function(df, req) {
  s <- req$slots$items; alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  it <- items_setup(df, s); k <- it$k
  cnt <- t(vapply(it$F, function(f) as.integer(table(f)), integer(length(it$lev))))
  if (length(it$lev) == 1) cnt <- matrix(cnt, ncol = 1)
  dimnames(cnt) <- list(it$names, it$lev)
  nn <- rowSums(cnt)
  if (any(nn == 0)) stop_user("回答のない設問があります（", paste(it$names[nn == 0], collapse = "、"), "）。その設問を外してください。")
  keepc <- colSums(cnt) > 0; tab <- as.table(cnt[, keepc, drop = FALSE])
  res <- list(test = "複数の設問の単純集計", n = n_info(nrow(df), max(nn)))
  res$xtab <- xtab_obj(tab, "設問", "回答", resid = FALSE)
  res$fig <- list(type = "items", prefer = "stack100")
  rp <- prop.table(tab, 1); mi <- apply(tab, 1, which.max)
  subj <- if (it$same_stem) paste0(q(it$stem), "の各項目（", paste(it$names, collapse = "、"), "）") else paste(vapply(it$fulls, q, ""), collapse = "、")
  first <- colnames(tab)[1]
  txt <- paste0(subj, "の回答を集計した（回答者 ", paste0(it$names, " n = ", nn, collapse = "、"), "）。")
  txt <- paste0(txt, "最も多い回答は、", paste0(it$names, "では", q(colnames(tab)[mi]), "（", vapply(seq_len(k), function(i) fmt_pct(rp[i, mi[i]]), ""), "%）", collapse = "、"), "であった。")
  txt <- paste0(txt, q(first), "と答えた割合は、", paste0(it$names, " ", fmt_pct(rp[, 1]), "%", collapse = "、"), "であった。")
  res$headline <- list(hl("設問", as.character(k)), hl("回答者", if (length(unique(nn)) == 1) as.character(nn[1]) else paste0(min(nn), "〜", max(nn))))
  res$row <- list(stat = "", effect = "")
  res$notes <- "割合の分母は、設問ごとの回答者数です。"
  if (isTRUE(opt(req, "friedman", FALSE))) {
    sc <- sapply(it$F, function(f) length(it$lev) + 1L - as.integer(f)); ok <- stats::complete.cases(sc); m <- sc[ok, , drop = FALSE]
    if (nrow(m) < 2) stop_user("すべての設問に答えた人が 2 人未満のため、フリードマン検定ができません。")
    ft <- stats::friedman.test(m); Q <- unname(ft$statistic); W <- Q / (nrow(m) * (k - 1))
    res$chk <- list(Q = Q, W = W, n = nrow(m))
    res$p <- unname(ft$p.value); res$test <- "複数の設問の単純集計（フリードマン検定）"
    mr <- colMeans(t(apply(m, 1, rank)))
    res$headline <- c(res$headline, list(hl(paste0("χ²(", ft$parameter, ")"), fmt_num(Q)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("Kendall の W", fmt_num(W))))
    res$row <- list(stat = paste0("χ²(", ft$parameter, ") = ", fmt_num(Q)), effect = paste0("W = ", fmt_num(W)))
    res$grids <- list(list(kind = "grid", title = "設問ごとの平均順位（フリードマン検定）", corner = "設問", rows = A(it$names), cols = A("平均順位"), fmt = A("num2"), cells = mat(matrix(mr, ncol = 1)),
                           note = paste0("すべての設問に答えた ", nrow(m), " 人で計算した。順位は", q(it$lev[1]), "の側を上位とし、1 人の中で設問に順位を付けた。")))
    o <- order(-mr); sig <- res$p < alpha
    txt <- paste0(txt, "すべての設問に答えた ", nrow(m), " 人についてフリードマン検定を行った結果、設問の間の回答の傾向の差は",
                  if (sig) "統計的に有意であった" else "統計的に有意ではなかった", "（χ²(", ft$parameter, ") = ", fmt_num(Q), ", ", fmt_p(res$p, lead0), ", Kendall の W = ", fmt_num(W), "）。",
                  if (sig) paste0("平均順位は", paste0(it$names[o], "（", fmt_num(mr[o]), "）", collapse = "、"), "の順に高かった。") else "")
    res$notes <- c(res$notes, "フリードマン検定は、同じ人が答えた複数の設問の間で回答の傾向に差があるかを調べる検定です。Kendall の W は効果量（0〜1）です。")
  }
  res$report <- txt
  finish(res, alpha)
}
register("freqmulti", "複数の設問の単純集計", "集計", "同じ選択肢で答える複数の設問（例：サービスごとの利用頻度）を並べて集計し、設問ごとに色を変えたグラフにします。",
         list(S("items", "比べる設問（同じ選択肢）", cat_types, "例：Q4 の各サービス", FALSE, TRUE)),
         list(O_check("friedman", "設問の間で回答の傾向を比べる（フリードマン検定）", FALSE, "順序のある回答で、同じ人が答えた複数の設問の間に差があるかを調べます。すべての設問に答えた人だけで計算します。")),
         an_freqmulti)

# ---------------------------------------------------------------- クロス集計（カイ二乗・フィッシャー）
crosstab_core <- function(df, req, method) {
  sa <- req$slots$a; sb <- req$slots$b
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  mt <- make_table(slot_factor(df, sa), slot_factor(df, sb)); tab <- mt$tab
  if (mt$n == 0) stop_user("2 つの質問の両方に答えた人がいません。列の組み合わせや絞り込みを見直してください。")
  if (nrow(tab) < 2 || ncol(tab) < 2)
    stop_user("検定には、行と列のそれぞれに回答のある選択肢が 2 つ以上必要です（現在 ", nrow(tab), " 行 × ", ncol(tab), " 列）。選択肢の設定や絞り込みを見直してください。")
  xt <- xtab_obj(tab, sa$label, sb$label, alpha, opt(req, "residMode", "z"))
  chk <- expected_check(tab); V <- cramers_v(tab); is22 <- all(dim(tab) == 2)
  res <- list(xtab = xt, n = n_info(nrow(df), mt$n), fig = list(type = "xtab"), notes = character(0))
  if (length(mt$dropped)) res$notes <- c(res$notes, paste0("回答が 0 人の選択肢（", paste(mt$dropped, collapse = "、"), "）は表から除きました。"))
  vname <- if (is22) "φ" else "Cramér's V"
  if (method == "chisq") {
    yates <- isTRUE(opt(req, "yates", FALSE)) && is22
    ct <- suppressWarnings(stats::chisq.test(tab, correct = yates))
    res$p <- unname(ct$p.value)
    res$test <- if (yates) "カイ二乗検定（イェーツの補正）" else "カイ二乗検定"
    stat <- paste0("χ²(", ct$parameter, ") = ", fmt_num(ct$statistic))
    stats_txt <- paste0(stat, ", ", fmt_p(res$p, lead0), ", ", vname, " = ", fmt_num(V))
    res$headline <- list(hl(paste0("χ²(", ct$parameter, ")"), fmt_num(ct$statistic)), hl("p", fmt_p(res$p, lead0, FALSE)),
                         hl(vname, fmt_num(V)), hl("n", as.character(mt$n)))
    res$row <- list(stat = stat, effect = paste0(vname, " = ", fmt_num(V)))
    sn <- small_n_note(chk)
    if (!is.null(sn)) { res$notes <- c(res$notes, sn); res$suggest <- "fisher" }
    caution <- if (!is.null(sn)) paste0("なお、期待度数が 5 未満のセルが ", chk$n_lt5, " 個（全 ", chk$cells, " セルの ", fmt_pct(chk$share_lt5, 0), "%）あり、カイ二乗検定の近似が不正確な可能性がある。") else ""
  } else {
    sim <- FALSE
    # 大きな表の正確な計算は数分かかることがあるため、既定の作業領域で解けないときは
    # モンテカルロ法（20 万回、乱数の種は固定）に切り替える。
    ft <- tryCatch(stats::fisher.test(tab, workspace = 2e6), error = function(e) NULL)
    if (is.null(ft)) { set.seed(20240101); ft <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 200000); sim <- TRUE }
    res$p <- unname(ft$p.value)
    res$test <- "フィッシャーの正確確率検定"
    stats_txt <- paste0(fmt_p(res$p, lead0), ", ", vname, " = ", fmt_num(V))
    res$headline <- list(hl("p", fmt_p(res$p, lead0, FALSE)), hl(vname, fmt_num(V)), hl("n", as.character(mt$n)))
    res$row <- list(stat = "–", effect = paste0(vname, " = ", fmt_num(V)))
    if (is22) {
      or <- unname(ft$estimate); ci <- ft$conf.int
      or_txt <- paste0("オッズ比 = ", fmt_num(or), ", 95%CI [", fmt_num(ci[1]), ", ", fmt_num(ci[2]), "]")
      stats_txt <- paste0(stats_txt, ", ", or_txt)
      res$headline <- append(res$headline, list(hl("オッズ比", paste0(fmt_num(or), " [", fmt_num(ci[1]), ", ", fmt_num(ci[2]), "]"))), 2)
      res$row$effect <- paste0("オッズ比 = ", fmt_num(or))
      res$notes <- c(res$notes, paste0("オッズ比は、", rownames(tab)[1], "が", rownames(tab)[2], "に比べて", q(colnames(tab)[1]), "と答えるオッズの比（条件付き最尤推定値）です。"))
    }
    if (sim) res$notes <- c(res$notes, "表が大きく正確な計算に時間がかかりすぎるため、p 値はモンテカルロ法（20 万回、乱数の種は固定）による推定値です。")
    caution <- if (sim) "p 値はモンテカルロ法（20 万回）による推定値である。" else ""
  }
  sig <- res$p < alpha
  txt <- paste0(q(sa$label), "と", q(sb$label), "のクロス集計を行った（有効回答 n = ", mt$n, "）。", modal_sentence(tab, sb$label),
                res$test, "の結果、",
                if (sig) "両者の関連は統計的に有意であった" else "両者の間に統計的に有意な関連は認められなかった",
                "（", stats_txt, "）。", if (sig) resid_sentence(xt, tab) else "", caution)
  res$report <- txt
  finish(res, alpha)
}
register("chisq", "カイ二乗検定", "2 つの質問の関連", "2 つの質問の答え方に関連があるかを調べます。クロス集計の基本の検定です。",
         list(S("a", "行（比べるグループ）", cat_types, "例：性別、学年"), S("b", "列（回答）", cat_types, "例：ある・ない")),
         list(O_check("yates", "イェーツの補正を使う（2×2 の表のとき）", FALSE, "2×2 の表で p 値を控えめにする補正です。人数が少ない 2×2 の表では、フィッシャーの正確確率検定のほうが適しています。"), OPT_RESID),
         function(df, req) crosstab_core(df, req, "chisq"))
register("fisher", "フィッシャーの正確確率検定", "2 つの質問の関連", "人数が少ないセルがあるときに使う、関連の検定です。2×2 の表ではオッズ比も出します。",
         list(S("a", "行（比べるグループ）", cat_types, "例：性別、学年"), S("b", "列（回答）", cat_types, "例：ある・ない")),
         list(OPT_RESID),
         function(df, req) crosstab_core(df, req, "fisher"))

# ---------------------------------------------------------------- 順位による群の比較
rank_setup <- function(df, req) {
  sg <- req$slots$g; sy <- req$slots$y
  g <- slot_factor(df, sg); y <- slot_score(df, sy)
  ok <- !is.na(g) & !is.na(y)
  g <- droplevels(g[ok]); y <- y[ok]
  list(sg = sg, sy = sy, g = g, y = y, n = sum(ok), numeric = identical(sy$type, "numeric"),
       ylev = if (identical(sy$type, "numeric")) NULL else unique(slot_labels(sy)))
}
rank_tables <- function(rs, df) {
  g <- rs$g; y <- rs$y; r <- rank(y)
  ng <- as.integer(table(g)); mr <- as.numeric(tapply(r, g, mean))
  out <- list()
  if (rs$numeric) {
    sm <- t(sapply(levels(g), function(l) { v <- y[g == l]; c(length(v), mean(v), stats::sd(v), stats::median(v), stats::quantile(v, .25), stats::quantile(v, .75), mean(r[g == l])) }))
    out$grids <- list(list(kind = "grid", title = "群ごとの要約", corner = rs$sg$label, rows = A(levels(g)),
                           cols = A(c("n", "平均", "標準偏差", "中央値", "第1四分位", "第3四分位", "平均順位")),
                           fmt = A(c("int", "num2", "num2", "num2", "num2", "num2", "num1")), cells = mat(sm)))
    bx <- lapply(levels(g), function(l) { v <- y[g == l]; b <- grDevices::boxplot.stats(v); list(stats = A(b$stats), out = A(b$out), n = length(v)) })
    out$fig <- list(type = "box", cats = A(levels(g)), box = bx, catTitle = rs$sg$label, valueTitle = rs$sy$label)
  } else {
    yf <- factor(rs$ylev[length(rs$ylev) + 1 - y], levels = rs$ylev)
    tab <- table(g, yf); tab <- tab[, colSums(tab) > 0, drop = FALSE]
    out$xtab <- xtab_obj(tab, rs$sg$label, rs$sy$label, resid = FALSE)
    med <- vapply(levels(g), function(l) median_label(y[g == l], rs$ylev), "")
    out$grids <- list(list(kind = "grid", title = "群ごとの順位の要約", corner = rs$sg$label, rows = A(levels(g)),
                           cols = A(c("n", "平均順位", "中央値にあたる回答")), fmt = A(c("int", "num1", "text")),
                           cells = lapply(seq_along(ng), function(i) list(ng[i], mr[i], gsub("[「」]", "", med[i]))),
                           note = paste0("順位は", q(rs$ylev[1]), "の側を上位として付けた。")))
    out$fig <- list(type = "xtab"); out$med <- med
  }
  out$ng <- ng; out$mr <- mr
  out
}
direction_phrase <- function(rs, hi, lo) {
  if (rs$numeric) paste0(hi, "のほうが", lo, "より値が大きい傾向にあった")
  else paste0(hi, "のほうが", lo, "より", q(rs$ylev[1]), "の側の回答が多かった")
}

an_mwu <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  rs <- rank_setup(df, req); g <- rs$g; y <- rs$y
  if (nlevels(g) != 2) stop_user("マン・ホイットニーの U 検定は 2 つの群の比較です。", q(rs$sg$label), "は現在 ", nlevels(g), " 群です。「選択肢の設定」で 2 つにする（まとめる・チェックを外す）か、3 群以上のときはクラスカル・ウォリス検定を使ってください。")
  if (length(unique(y)) < 2) stop_user("すべての回答が同じ値のため、検定できません。")
  l <- levels(g); x1 <- y[g == l[1]]; x2 <- y[g == l[2]]; n1 <- length(x1); n2 <- length(x2); N <- n1 + n2
  wt <- wilcox_two(x1, x2)
  U1 <- unname(wt$statistic); U <- min(U1, n1 * n2 - U1)
  tt <- table(c(x1, x2)); sigma <- sqrt(n1 * n2 / 12 * ((N + 1) - sum(tt^3 - tt) / (N * (N - 1))))
  d <- U1 - n1 * n2 / 2; Z <- (d - sign(d) * 0.5) / sigma; r_eff <- abs(Z) / sqrt(N)
  tb <- rank_tables(rs, df)
  res <- list(test = "マン・ホイットニーの U 検定", p = unname(wt$p.value), n = n_info(nrow(df), N), xtab = tb$xtab, grids = tb$grids, fig = tb$fig)
  stats_txt <- paste0("U = ", fmt_num(U, 1), ", |Z| = ", fmt_num(abs(Z)), ", ", fmt_p(res$p, lead0), ", r = ", fmt_num(r_eff))
  res$headline <- list(hl("U", fmt_num(U, 1)), hl("|Z|", fmt_num(abs(Z))), hl("p", fmt_p(res$p, lead0, FALSE)), hl("r", fmt_num(r_eff)), hl("n", as.character(N)))
  res$row <- list(stat = paste0("U = ", fmt_num(U, 1)), effect = paste0("r = ", fmt_num(r_eff)))
  hi <- if (tb$mr[1] >= tb$mr[2]) 1 else 2; lo <- 3 - hi
  sig <- res$p < alpha
  res$notes <- "p 値は正規近似（同順位の補正と連続性の補正あり）によります。効果量 r は |Z| / √N です。"
  res$report <- paste0(q(rs$sg$label), "の 2 群（", l[1], " n = ", n1, "、", l[2], " n = ", n2, "）で", q(rs$sy$label), "の回答を比較した。",
                       "マン・ホイットニーの U 検定の結果、2 群の回答の分布には",
                       if (sig) "統計的に有意な差が認められた" else "統計的に有意な差は認められなかった", "（", stats_txt, "）。",
                       if (sig) paste0("平均順位は", l[hi], "（", fmt_num(tb$mr[hi], 1), "）が", l[lo], "（", fmt_num(tb$mr[lo], 1), "）より高く、", direction_phrase(rs, l[hi], l[lo]), "。") else "",
                       if (!rs$numeric) paste0("中央値にあたる回答は、", l[1], "が", tb$med[1], "、", l[2], "が", tb$med[2], "であった。") else "")
  finish(res, alpha)
}
register("mwu", "マン・ホイットニーの U 検定", "順序のある回答の比較", "2 つのグループで、段階のある回答（あてはまる〜あてはまらない など）の傾向が違うかを調べます。",
         list(S("g", "グループ（2 群）", cat_types, "例：性別", TRUE), S("y", "比べる回答（順序あり）", rank_types, "段階で答える質問、または数値", TRUE)),
         list(), an_mwu)

an_kw <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  rs <- rank_setup(df, req); g <- rs$g; y <- rs$y; k <- nlevels(g); N <- rs$n
  if (k < 2) stop_user("比較するには、", q(rs$sg$label), "に回答のある群が 2 つ以上必要です。")
  if (length(unique(y)) < 2) stop_user("すべての回答が同じ値のため、検定できません。")
  kt <- stats::kruskal.test(y, g); H <- unname(kt$statistic); eps <- H / (N - 1)
  tb <- rank_tables(rs, df)
  res <- list(test = "クラスカル・ウォリス検定", p = unname(kt$p.value), n = n_info(nrow(df), N), xtab = tb$xtab, grids = tb$grids, fig = tb$fig)
  stats_txt <- paste0("H(", kt$parameter, ") = ", fmt_num(H), ", ", fmt_p(res$p, lead0), ", ε² = ", fmt_num(eps))
  res$headline <- list(hl(paste0("H(", kt$parameter, ")"), fmt_num(H)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("ε²", fmt_num(eps)), hl("n", as.character(N)))
  res$row <- list(stat = paste0("H(", kt$parameter, ") = ", fmt_num(H)), effect = paste0("ε² = ", fmt_num(eps)))
  sig <- res$p < alpha; post <- ""
  if (k >= 3) {
    pw <- pairwise_wilcox_holm(y, g)
    pairs <- list(); pv <- numeric(0)
    for (j in seq_len(ncol(pw))) for (i in seq_len(nrow(pw))) if (!is.na(pw[i, j])) {
      pairs[[length(pairs) + 1]] <- paste0(colnames(pw)[j], " – ", rownames(pw)[i]); pv <- c(pv, pw[i, j]) }
    res$grids <- c(res$grids, list(list(kind = "grid", title = "多重比較（マン・ホイットニーの U 検定、ホルム法で補正）", corner = "組み合わせ",
                                        rows = A(unlist(pairs)), cols = A("補正後の p"), fmt = A("p"), cells = mat(matrix(pv, ncol = 1)),
                                        mark = mat(matrix(as.numeric(pv < alpha), ncol = 1)))))
    if (sig) {
      sp <- which(pv < alpha)
      post <- if (length(sp)) paste0("多重比較（ホルム法で補正したマン・ホイットニーの U 検定）では、", paste0(unlist(pairs)[sp], "（", vapply(pv[sp], fmt_p, "", lead0 = lead0), "）", collapse = "、"), "の間に有意な差が認められた。")
              else "多重比較（ホルム法で補正したマン・ホイットニーの U 検定）では、有意な差のある組み合わせは認められなかった。"
    }
  }
  o <- order(-tb$mr)
  res$notes <- "効果量 ε² は H / (N − 1) です。"
  res$report <- paste0(q(rs$sg$label), "の ", k, " 群で", q(rs$sy$label), "の回答を比較した（n = ", N, "）。クラスカル・ウォリス検定の結果、群の間に",
                       if (sig) "統計的に有意な差が認められた" else "統計的に有意な差は認められなかった", "（", stats_txt, "）。",
                       if (sig) paste0("平均順位は", paste0(levels(g)[o], "（", vapply(tb$mr[o], fmt_num, "", d = 1), "）", collapse = "、"), "の順に高かった。") else "", post)
  finish(res, alpha)
}
register("kw", "クラスカル・ウォリス検定", "順序のある回答の比較", "3 つ以上のグループで、段階のある回答の傾向が違うかを調べます。どの組み合わせに差があるかも出します。",
         list(S("g", "グループ（3 群以上）", cat_types, "例：学年、利用頻度", TRUE), S("y", "比べる回答（順序あり）", rank_types, "段階で答える質問、または数値", TRUE)),
         list(), an_kw)

# ---------------------------------------------------------------- スピアマンの順位相関
an_spearman <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  sx <- req$slots$x; sy <- req$slots$y
  x <- slot_score(df, sx); y <- slot_score(df, sy); ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; n <- sum(ok)
  if (n < 4) stop_user("相関を求めるには、両方に答えた人が 4 人以上必要です（現在 ", n, " 人）。")
  if (length(unique(x)) < 2 || length(unique(y)) < 2) stop_user("どちらかの回答がすべて同じ値のため、相関を求められません。")
  ct <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE)); rho <- unname(ct$estimate)
  res <- list(test = "スピアマンの順位相関", p = unname(ct$p.value), n = n_info(nrow(df), n))
  numx <- identical(sx$type, "numeric"); numy <- identical(sy$type, "numeric")
  if (!numx && !numy) {
    lx <- unique(slot_labels(sx)); ly <- unique(slot_labels(sy))
    tab <- table(factor(lx[length(lx) + 1 - x], levels = lx), factor(ly[length(ly) + 1 - y], levels = ly))
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
    res$xtab <- xtab_obj(tab, sx$label, sy$label, resid = FALSE); res$fig <- list(type = "xtab", prefer = "heat")
  } else res$fig <- list(type = "scatter", x = A(x), y = A(y), xTitle = sx$label, yTitle = sy$label)
  a <- abs(rho); strength <- if (a < 0.2) "ごく弱い" else if (a < 0.4) "弱い" else if (a < 0.7) "中程度の" else "強い"
  stats_txt <- paste0("ρ = ", fmt_num(rho), ", ", fmt_p(res$p, lead0), ", n = ", n)
  res$headline <- list(hl("ρ", fmt_num(rho)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("n", as.character(n)))
  res$row <- list(stat = paste0("ρ = ", fmt_num(rho)), effect = paste0("ρ = ", fmt_num(rho)))
  sig <- res$p < alpha
  top <- function(s) if (identical(s$type, "numeric")) "値が大きい人" else paste0(q(unique(slot_labels(s))[1]), "の側の回答をした人")
  res$notes <- c("相関の強さの表現（|ρ| が 0.2 未満：ごく弱い、0.4 未満：弱い、0.7 未満：中程度、0.7 以上：強い）は目安であり、分野によって基準が異なります。",
                 "相関は 2 つの回答の関連を示すもので、一方が他方の原因であることを示すものではありません。")
  res$report <- paste0(q(sx$label), "と", q(sy$label), "の関連をスピアマンの順位相関係数で調べた。その結果、",
                       if (sig) paste0(strength, if (rho > 0) "正" else "負", "の相関が認められた") else "統計的に有意な相関は認められなかった",
                       "（", stats_txt, "）。",
                       if (sig) paste0(q(sx$label), "で", top(sx), "ほど、", q(sy$label), "で", top(sy), if (rho > 0) "が多い傾向にあった。" else "が少ない傾向にあった。") else "")
  finish(res, alpha)
}
register("spearman", "スピアマンの順位相関", "順序のある回答の比較", "段階のある 2 つの回答が、一方が高いほど他方も高い（または低い）関係にあるかを調べます。",
         list(S("x", "回答 1（順序あり）", rank_types, "", TRUE), S("y", "回答 2（順序あり）", rank_types, "", TRUE)), list(), an_spearman)

# ---------------------------------------------------------------- 複数回答 × グループ
an_multi <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  sm <- req$slots$m; sg <- req$slots$g
  M <- slot_multi(df, sm); g <- slot_factor(df, sg); ok <- !is.na(M[, 1]) & !is.na(g)
  M <- M[ok, , drop = FALSE]; g <- droplevels(g[ok]); n <- sum(ok)
  if (n == 0) stop_user("両方の質問に答えた人がいません。")
  if (nlevels(g) < 2) stop_user("比較するには、", q(sg$label), "に回答のある群が 2 つ以上必要です。")
  ng <- as.integer(table(g)); items <- colnames(M)
  cnt <- t(sapply(seq_along(items), function(j) as.integer(tapply(M[, j], g, sum))))
  pct <- sweep(cnt, 2, ng, "/")
  pv <- rep(NA_real_, length(items)); meth <- rep("", length(items))
  for (j in seq_along(items)) {
    tab <- rbind(cnt[j, ], ng - cnt[j, ])
    if (any(rowSums(tab) == 0)) next
    E <- outer(rowSums(tab), colSums(tab)) / sum(tab)
    if (any(E < 5)) {
      ft <- tryCatch(stats::fisher.test(tab, workspace = 2e6), error = function(e) { set.seed(20240101); stats::fisher.test(tab, simulate.p.value = TRUE, B = 200000) })
      pv[j] <- ft$p.value; meth[j] <- "フィッシャーの正確確率検定"
    } else { pv[j] <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))$p.value; meth[j] <- "カイ二乗検定" }
  }
  padj <- stats::p.adjust(pv, method = "holm")
  cells <- lapply(seq_along(items), function(j) c(as.list(unlist(lapply(seq_along(ng), function(i) list(cnt[j, i], pct[j, i])))), list(meth[j], pv[j], padj[j])))
  cols <- c(unlist(lapply(levels(g), function(l) c(paste0(l, " 人数"), paste0(l, " %")))), "検定", "p", "補正後の p")
  fm <- c(rep(c("int", "pct"), length(ng)), "text", "p", "p")
  res <- list(test = "複数回答のクロス集計", n = n_info(nrow(df), n))
  res$grids <- list(list(kind = "grid", title = "選択肢ごとの選択率と検定", corner = sm$label, rows = A(items), cols = A(cols), fmt = A(fm), cells = cells,
                         markCol = length(cols), markRows = A(as.numeric(!is.na(padj) & padj < alpha)),
                         note = paste0("割合の分母は各群の回答者数（", paste0(levels(g), " n = ", ng, collapse = "、"), "）。補正後の p はホルム法による。")))
  res$fig <- list(type = "multi", cats = A(items), series = A(levels(g)), count = mat(cnt), pct = mat(pct), catTitle = sm$label, seriesTitle = sg$label)
  sgn <- which(!is.na(padj) & padj < alpha)
  res$p <- if (all(is.na(padj))) NULL else min(padj, na.rm = TRUE)
  res$headline <- list(hl("選択肢", as.character(length(items))), hl("有意な選択肢", as.character(length(sgn))), hl("最小の補正後 p", fmt_p(res$p, lead0, FALSE)), hl("n", as.character(n)))
  res$row <- list(stat = paste0("有意な選択肢 ", length(sgn), " / ", length(items)), effect = "")
  used <- unique(meth[meth != ""])
  res$notes <- c("選択肢の数だけ検定を繰り返すため、p 値はホルム法で補正しています。", "期待度数が 5 未満のセルを含む選択肢にはフィッシャーの正確確率検定、それ以外にはカイ二乗検定を使っています。")
  res$report <- paste0(q(sm$label), "（複数回答）の各選択肢が選ばれた割合を、", q(sg$label), "の群別に集計した（n = ", n, "）。選択肢ごとに",
                       paste0(used, collapse = "または"), "を行い、p 値をホルム法で補正した結果、",
                       if (length(sgn)) paste0(paste0(q(items[sgn]), "（", vapply(sgn, function(j) paste0(levels(g), " ", fmt_pct(pct[j, ]), "%", collapse = "、"), ""), "、補正後 ", vapply(padj[sgn], fmt_p, "", lead0 = lead0), "）", collapse = "、"), "で群の間に有意な差が認められた。")
                       else "いずれの選択肢でも群の間に有意な差は認められなかった。")
  finish(res, alpha)
}
register("multi", "複数回答のクロス集計", "複数回答", "「あてはまるものをすべて」の質問で、選択肢ごとの選ばれた割合をグループ別に比べます。",
         list(S("m", "複数回答の質問", "multi"), S("g", "グループ", cat_types, "例：性別、学年", TRUE)), list(), an_multi)

# ---------------------------------------------------------------- 同じ人の 2 つの回答の比較
paired_setup <- function(df, req, need2 = FALSE) {
  sx <- req$slots$x; sy <- req$slots$y
  lx <- unique(slot_labels(sx)); ly <- unique(slot_labels(sy))
  if (!identical(lx, ly)) stop_user("この検定では、2 つの質問の選択肢（名前と並び）が同じである必要があります。「選択肢の設定」でそろえてください（現在：", paste(lx, collapse = "／"), " と ", paste(ly, collapse = "／"), "）。")
  if (need2 && length(lx) != 2) stop_user("マクネマー検定は 2 択どうしの比較です。「選択肢の設定」で選択肢を 2 つにまとめてください（現在 ", length(lx), " 個）。")
  fx <- slot_factor(df, sx); fy <- slot_factor(df, sy); ok <- !is.na(fx) & !is.na(fy)
  list(sx = sx, sy = sy, fx = fx[ok], fy = fy[ok], lev = lx, n = sum(ok))
}
an_wsr <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  ps <- paired_setup(df, req); k <- length(ps$lev)
  if (ps$n < 1) stop_user("両方の質問に答えた人がいません。")
  x <- k + 1L - as.integer(ps$fx); y <- k + 1L - as.integer(ps$fy); d <- x - y
  nz <- sum(d != 0); if (nz == 0) stop_user("2 つの回答がすべての人で同じため、検定できません。")
  wt <- wilcox_paired(x, y)
  p <- unname(wt$p.value); Z <- stats::qnorm(p / 2, lower.tail = FALSE); r_eff <- Z / sqrt(nz)
  tab <- table(ps$fx, ps$fy)
  res <- list(test = "ウィルコクソンの符号付順位検定", p = p, n = n_info(nrow(df), ps$n),
              xtab = xtab_obj(tab, ps$sx$label, ps$sy$label, resid = FALSE), fig = list(type = "xtab", prefer = "heat"))
  up <- sum(d > 0); dn <- sum(d < 0); eq <- sum(d == 0)
  res$grids <- list(list(kind = "grid", title = "回答の変化", corner = "", rows = A(c(paste0(ps$sx$label, " のほうが上位"), paste0(ps$sy$label, " のほうが上位"), "同じ")),
                         cols = A(c("人数", "割合 (%)")), fmt = A(c("int", "pct")), cells = mat(cbind(c(up, dn, eq), c(up, dn, eq) / ps$n)),
                         note = paste0("上位は", q(ps$lev[1]), "の側。")))
  stats_txt <- paste0("V = ", fmt_num(wt$statistic, 1), ", |Z| = ", fmt_num(Z), ", ", fmt_p(p, lead0), ", r = ", fmt_num(r_eff))
  res$headline <- list(hl("V", fmt_num(wt$statistic, 1)), hl("|Z|", fmt_num(Z)), hl("p", fmt_p(p, lead0, FALSE)), hl("r", fmt_num(r_eff)), hl("n", as.character(ps$n)))
  res$row <- list(stat = paste0("V = ", fmt_num(wt$statistic, 1)), effect = paste0("r = ", fmt_num(r_eff)))
  sig <- p < alpha
  res$notes <- "p 値は正規近似によります。|Z| は p 値から求めた値、効果量 r は |Z| / √（差が 0 でない人数）です。"
  res$report <- paste0("同じ回答者の", q(ps$sx$label), "と", q(ps$sy$label), "の回答を比較した（n = ", ps$n, "）。ウィルコクソンの符号付順位検定の結果、2 つの回答には",
                       if (sig) "統計的に有意な差が認められた" else "統計的に有意な差は認められなかった", "（", stats_txt, "）。",
                       q(ps$sx$label), "のほうが", q(ps$lev[1]), "の側の回答だった人は ", up, " 人、", q(ps$sy$label), "のほうがそうだった人は ", dn, " 人、同じ回答の人は ", eq, " 人であった。")
  finish(res, alpha)
}
register("wsr", "ウィルコクソンの符号付順位検定", "同じ人の 2 つの回答", "同じ選択肢で答える 2 つの質問（例：2 つのサービスの利用頻度）を、同じ人の中で比べます。",
         list(S("x", "回答 1（順序あり）", "ordinal", "", TRUE), S("y", "回答 2（同じ選択肢）", "ordinal", "", TRUE)), list(), an_wsr)

an_mcnemar <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  ps <- paired_setup(df, req, need2 = TRUE)
  if (ps$n < 1) stop_user("両方の質問に答えた人がいません。")
  tab <- table(ps$fx, ps$fy); b <- tab[1, 2]; c <- tab[2, 1]
  if (b + c == 0) stop_user("2 つの回答がすべての人で同じため、検定できません。")
  exact <- (b + c) < 25
  if (exact) { p <- stats::binom.test(b, b + c, 0.5)$p.value; stat <- paste0("不一致 ", b, " 対 ", c); tname <- "マクネマー検定（正確二項検定）" }
  else { mt <- stats::mcnemar.test(tab, correct = TRUE); p <- unname(mt$p.value); stat <- paste0("χ²(1) = ", fmt_num(mt$statistic)); tname <- "マクネマー検定" }
  px <- mean(as.integer(ps$fx) == 1); py <- mean(as.integer(ps$fy) == 1)
  res <- list(test = tname, p = p, n = n_info(nrow(df), ps$n), xtab = xtab_obj(tab, ps$sx$label, ps$sy$label, resid = FALSE), fig = list(type = "xtab", prefer = "heat"))
  res$headline <- list(hl(if (exact) "不一致の組" else "χ²(1)", if (exact) paste0(b, " 対 ", c) else fmt_num(mt$statistic)), hl("p", fmt_p(p, lead0, FALSE)), hl("n", as.character(ps$n)))
  res$row <- list(stat = stat, effect = "")
  res$notes <- if (exact) "回答が食い違った人が 25 人未満のため、正確二項検定を使いました。" else "連続性の補正を行っています。"
  sig <- p < alpha
  res$report <- paste0("同じ回答者について、", q(ps$sx$label), "と", q(ps$sy$label), "で", q(ps$lev[1]), "と答えた割合を比較した（n = ", ps$n, "）。割合はそれぞれ ",
                       fmt_pct(px), "% と ", fmt_pct(py), "% で、", tname, "の結果、その差は", if (sig) "統計的に有意であった" else "統計的に有意ではなかった",
                       "（", stat, ", ", fmt_p(p, lead0), "）。")
  finish(res, alpha)
}
register("mcnemar", "マクネマー検定", "同じ人の 2 つの回答", "同じ 2 択で答える 2 つの質問を、同じ人の中で比べます（例：A は「はい」だが B は「いいえ」の人が多いか）。",
         list(S("x", "回答 1（2 択）", cat_types, "", TRUE), S("y", "回答 2（同じ 2 択）", cat_types, "", TRUE)), list(), an_mcnemar)

# ---------------------------------------------------------------- 割合の傾向検定
an_trend <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  sb <- req$slots$b; so <- req$slots$o
  lb <- unique(slot_labels(sb))
  if (length(lb) != 2) stop_user("傾向検定では、", q(sb$label), "を 2 つの選択肢にする必要があります。「選択肢の設定」でまとめてください（現在 ", length(lb), " 個）。")
  mt <- make_table(slot_factor(df, so), slot_factor(df, sb)); tab <- mt$tab
  if (nrow(tab) < 3) stop_user("傾向検定には、", q(so$label), "に回答のある段階が 3 つ以上必要です（現在 ", nrow(tab), " 段階）。2 段階のときはカイ二乗検定を使ってください。")
  if (ncol(tab) < 2) stop_user(q(sb$label), "の回答が 1 種類しかないため、検定できません。")
  x <- tab[, 1]; nn <- rowSums(tab); pt <- stats::prop.trend.test(x, nn); pr <- x / nn
  res <- list(test = "コクラン・アーミテージの傾向検定", p = unname(pt$p.value), n = n_info(nrow(df), mt$n),
              xtab = xtab_obj(tab, so$label, sb$label, resid = FALSE), fig = list(type = "xtab"))
  stat <- paste0("χ²(1) = ", fmt_num(pt$statistic))
  res$headline <- list(hl("χ²(1)", fmt_num(pt$statistic)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("n", as.character(mt$n)))
  res$row <- list(stat = stat, effect = "")
  sig <- res$p < alpha
  res$notes <- paste0("段階には並びの順に 1, 2, 3, … の等間隔の得点を与えています。")
  res$report <- paste0(q(so$label), "の段階ごとに、", q(sb$label), "で", q(colnames(tab)[1]), "と答えた割合を求めた（n = ", mt$n, "）。割合は",
                       paste0(rownames(tab), " ", fmt_pct(pr), "%", collapse = "、"), "であった。コクラン・アーミテージの傾向検定の結果、段階にそった割合の直線的な増減は",
                       if (sig) "統計的に有意であった" else "統計的に有意ではなかった", "（", stat, ", ", fmt_p(res$p, lead0), "）。")
  finish(res, alpha)
}
register("trend", "割合の傾向検定（コクラン・アーミテージ）", "2 つの質問の関連", "段階が上がるほど「はい」の割合が増える（減る）という一方向の傾向があるかを調べます。",
         list(S("b", "2 択の回答", cat_types, "例：ある・ない", TRUE), S("o", "段階（順序あり）", "ordinal", "例：学年、利用頻度", TRUE)), list(), an_trend)

# ---------------------------------------------------------------- 層別のクロス集計（コクラン・マンテル・ヘンツェル検定）
# ブレスロー・デイ検定（タロンの補正つき）：2×2 の表で、層ごとのオッズ比が等しいかを調べる
breslow_day <- function(t3, psi) {
  K <- dim(t3)[3]; d <- numeric(K); v <- numeric(K)
  for (k in seq_len(K)) {
    tk <- t3[, , k]; a <- tk[1, 1]; m1 <- sum(tk[1, ]); n1 <- sum(tk[, 1]); N <- sum(tk)
    lo <- max(0, m1 + n1 - N); hi <- min(m1, n1)
    A2 <- 1 - psi; B <- N - m1 - n1 + psi * (m1 + n1); C <- -psi * m1 * n1
    E <- if (abs(A2) < 1e-12) -C / B else { r <- (-B + c(-1, 1) * sqrt(B^2 - 4 * A2 * C)) / (2 * A2); r[r >= lo - 1e-9 & r <= hi + 1e-9][1] }
    d[k] <- a - E; v[k] <- 1 / (1 / E + 1 / (m1 - E) + 1 / (n1 - E) + 1 / (N - m1 - n1 + E))
  }
  st <- sum(d^2 / v) - sum(d)^2 / sum(v)
  list(stat = st, df = K - 1, p = stats::pchisq(st, K - 1, lower.tail = FALSE))
}
an_strat <- function(df, req) {
  sa <- req$slots$a; sb <- req$slots$b; sz <- req$slots$z
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  a <- slot_factor(df, sa); b <- slot_factor(df, sb); z <- slot_factor(df, sz)
  ok <- !is.na(a) & !is.na(b) & !is.na(z)
  if (!any(ok)) stop_user("3 つの質問すべてに答えた人がいません。列の組み合わせや絞り込みを見直してください。")
  a <- droplevels(a[ok]); b <- droplevels(b[ok]); z <- droplevels(z[ok])
  if (nlevels(a) < 2 || nlevels(b) < 2) stop_user("行と列のそれぞれに、回答のある選択肢が 2 つ以上必要です。選択肢の設定や絞り込みを見直してください。")
  if (nlevels(z) < 2) stop_user("層には、回答のある選択肢が 2 つ以上必要です。層を分けない場合は、カイ二乗検定を使ってください。")
  t3 <- table(a, b, z); n <- sum(t3)
  # 層ごとの検定
  lay <- levels(z); rows <- list(); dropped <- character(0); sigl <- character(0)
  for (k in seq_along(lay)) {
    tk <- t3[, , k, drop = TRUE]; nk <- sum(tk); tk2 <- tk[rowSums(tk) > 0, colSums(tk) > 0, drop = FALSE]
    if (nrow(tk2) < 2 || ncol(tk2) < 2) { rows[[k]] <- list(nk, NA, NA, NA, NA, NA); dropped <- c(dropped, lay[k]); next }
    ct <- suppressWarnings(stats::chisq.test(tk2, correct = FALSE)); V <- cramers_v(tk2); ch <- expected_check(tk2)
    rows[[k]] <- list(nk, unname(ct$statistic), unname(ct$parameter), unname(ct$p.value), V, ch$share_lt5 * 100)
    if (ct$p.value < alpha) sigl <- c(sigl, lay[k])
  }
  tt <- table(a, b); ctt <- suppressWarnings(stats::chisq.test(tt, correct = FALSE))
  rows[[length(rows) + 1]] <- list(n, unname(ctt$statistic), unname(ctt$parameter), unname(ctt$p.value), cramers_v(tt), expected_check(tt)$share_lt5 * 100)
  # 層の合計が 1 人以下の層は CMH 検定から除く
  keepz <- apply(t3, 3, sum) > 1; t3c <- t3[, , keepz, drop = FALSE]
  mh <- tryCatch(stats::mantelhaen.test(t3c, correct = FALSE), error = function(e) NULL)
  if (is.null(mh)) stop_user("層ごとの人数が少なすぎて、コクラン・マンテル・ヘンツェル検定を計算できません。層や選択肢をまとめてください。")
  is22 <- nlevels(a) == 2 && nlevels(b) == 2
  stat <- paste0("CMH χ²(", mh$parameter, ") = ", fmt_num(mh$statistic))
  res <- list(test = "コクラン・マンテル・ヘンツェル検定", p = unname(mh$p.value), n = n_info(nrow(df), n), fig = list(type = "xtab"), notes = character(0))
  res$headline <- list(hl(paste0("CMH χ²(", mh$parameter, ")"), fmt_num(mh$statistic)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("層", as.character(sum(keepz))), hl("n", as.character(n)))
  res$row <- list(stat = stat, effect = "–")
  # 図と表：行を「層・行の選択肢」にしたクロス集計表
  big <- do.call(rbind, lapply(seq_along(lay), function(k) { m <- matrix(t3[, , k], nrow = nlevels(a)); rownames(m) <- paste0(lay[k], "・", levels(a)); m }))
  colnames(big) <- levels(b); big <- big[rowSums(big) > 0, , drop = FALSE]
  res$xtab <- xtab_obj(as.table(big), paste0(sz$label, "・", sa$label), sb$label, resid = FALSE)
  res$grids <- list(list(kind = "grid", title = "層ごとのカイ二乗検定", corner = sz$label, rows = A(c(lay, "全体（層を分けない）")),
                         cols = A(c("n", "χ²", "自由度", "p", "Cramér's V", "期待度数 5 未満 (%)")), fmt = A(c("int", "num2", "int", "p", "num2", "num1")),
                         cells = lapply(rows, function(r) lapply(r, function(x) if (is.na(x)) NULL else x)),
                         note = "層ごとの検定はイェーツの補正なしのカイ二乗検定です。層の数だけ検定を繰り返すので、層ごとの p 値は参考として扱ってください。"))
  extra <- ""
  if (is22) {
    or <- unname(mh$estimate); ci <- mh$conf.int; bd <- breslow_day(t3c, or)
    res$headline <- append(res$headline, list(hl("共通オッズ比", paste0(fmt_num(or), " [", fmt_num(ci[1]), ", ", fmt_num(ci[2]), "]"))), 2)
    res$row$effect <- paste0("共通オッズ比 = ", fmt_num(or))
    orl <- vapply(dimnames(t3c)[[3]], function(k) { tk <- t3c[, , k]; (tk[1, 1] + .5) * (tk[2, 2] + .5) / ((tk[1, 2] + .5) * (tk[2, 1] + .5)) }, 0)
    res$grids <- c(res$grids, list(list(kind = "grid", title = "オッズ比", corner = "", rows = A(c(names(orl), "共通（マンテル・ヘンツェル）")),
                                        cols = A(c("オッズ比", "95% 信頼区間の下限", "上限")), fmt = A(c("num2", "num2", "num2")),
                                        cells = c(lapply(orl, function(x) list(x, NULL, NULL)), list(list(or, ci[1], ci[2]))),
                                        note = paste0("オッズ比は、", q(levels(a)[1]), "が", q(levels(a)[2]), "に比べて", q(levels(b)[1]), "と答えるオッズの比です。層ごとの値は、0 のセルがあっても計算できるよう各セルに 0.5 を足した値です。")),
                                   list(kind = "grid", title = "オッズ比の均一性（ブレスロー・デイ検定、タロンの補正）", corner = "", rows = A("層の間の違い"), cols = A(c("χ²", "自由度", "p")),
                                        fmt = A(c("num2", "int", "p")), cells = list(list(bd$stat, bd$df, bd$p)))))
    extra <- paste0("層の影響を調整した共通オッズ比は ", fmt_num(or), "（95%CI [", fmt_num(ci[1]), ", ", fmt_num(ci[2]), "]）であった。",
                    "ブレスロー・デイ検定では、層の間でオッズ比が", if (bd$p < alpha) paste0("異なっていた（χ²(", bd$df, ") = ", fmt_num(bd$stat), ", ", fmt_p(bd$p, lead0), "）。層によって関連の強さが違うため、共通オッズ比より層ごとの結果を重視して解釈する必要がある。") else paste0("異なるとはいえなかった（χ²(", bd$df, ") = ", fmt_num(bd$stat), ", ", fmt_p(bd$p, lead0), "）。"))
    res$notes <- c(res$notes, "2×2 の表の CMH 検定は、連続性の補正なしで計算しています。")
  } else res$notes <- c(res$notes, "行または列の選択肢が 3 つ以上なので、一般化した CMH 検定（一般の関連）で計算しています。共通オッズ比は 2×2 の表のときだけ出します。")
  if (length(dropped)) res$notes <- c(res$notes, paste0("層「", paste(dropped, collapse = "」「"), "」は、回答のある選択肢が 1 つしかないため、層ごとの検定ができませんでした。"))
  if (any(!keepz)) res$notes <- c(res$notes, paste0("回答が 1 人以下の層（", paste(lay[!keepz], collapse = "、"), "）は CMH 検定から除きました。"))
  sig <- res$p < alpha
  res$report <- paste0(q(sz$label), "で層に分け（", sum(keepz), " 層）、", q(sa$label), "と", q(sb$label), "の関連を調べた（有効回答 n = ", n, "）。",
                       "層を分けないカイ二乗検定では", if (ctt$p.value < alpha) "有意な関連が認められ" else "有意な関連は認められず",
                       "（χ²(", ctt$parameter, ") = ", fmt_num(ctt$statistic), ", ", fmt_p(ctt$p.value, lead0), "）、層ごとの検定で有意であった層は",
                       if (length(sigl)) paste0(paste(sigl, collapse = "、"), "であった。") else "なかった。",
                       "コクラン・マンテル・ヘンツェル検定の結果、", q(sz$label), "の影響を調整しても両者の関連は",
                       if (sig) "統計的に有意であった" else "統計的に有意ではなかった", "（", stat, ", ", fmt_p(res$p, lead0), "）。", extra)
  finish(res, alpha)
}
register("strat", "層別のクロス集計（CMH 検定）", "2 つの質問の関連", "3 つ目の質問（例：学年）で層に分けてクロス集計し、層の影響を調整しても 2 つの質問に関連があるかを調べます（コクラン・マンテル・ヘンツェル検定）。",
         list(S("a", "行（比べるグループ）", cat_types, "例：性別"), S("b", "列（回答）", cat_types, "例：ある・ない"), S("z", "層（分けて見る質問）", cat_types, "例：学年、部活動")),
         list(), an_strat)

registry_json <- function() {
  lapply(ANALYSES, function(a) a[c("id", "name", "group", "desc", "slots", "opts")])
}

run_analysis <- function(df, req) {
  a <- ANALYSES[[req$type]]
  if (is.null(a)) stop_user("この解析の種類（", req$type, "）は登録されていません。")
  # 同じ設問を 2 つの欄に選んでいるときは、計算せずに知らせる
  cols <- vapply(req$slots, function(s) if (is.null(s$col) || isTRUE(s$many)) NA_integer_ else as.integer(s$col), 0L)
  dup <- which(duplicated(cols) & !is.na(cols))
  if (length(dup)) {
    s <- req$slots[[dup[1]]]; nm <- if (is.character(s[["full"]]) && nzchar(s[["full"]])) s[["full"]] else s[["label"]]
    stop_user("同じ設問（", q(nm), "）が 2 つの欄で選ばれています。2 つの設問の関係を調べる解析なので、別の設問を選んでください。")
  }
  keep <- apply_filter(df, req$filter)
  d <- df[keep, , drop = FALSE]
  if (nrow(d) == 0) stop_user("絞り込みの条件に合う回答がありません。絞り込みを見直してください。")
  res <- a$fun(d, req)
  res$type <- req$type
  # 報告書の文では、設問を省略しない名前（full）で書く。表示名を短く書き換えていても、文は元の質問に沿う
  sl <- req$slots; if (!is.null(req$filter) && length(req$filter$values)) sl <- c(sl, list(req$filter))
  sl <- Filter(function(s) !isTRUE(s[["many"]]) && is.character(s[["full"]]) && length(s[["full"]]) == 1 && nzchar(s[["full"]]) && is.character(s[["label"]]) && s[["full"]] != s[["label"]], sl)
  if (length(sl)) for (s in sl[order(-nchar(vapply(sl, function(s) s[["label"]], "")))]) res$report <- gsub(q(s[["label"]]), q(s[["full"]]), res$report, fixed = TRUE)
  if (!is.null(req$filter) && length(req$filter$values)) {
    fl <- if (is.character(req$filter[["full"]]) && nzchar(req$filter[["full"]])) req$filter[["full"]] else req$filter$label
    res$filterText <- paste0(q(fl), "が", paste0(q(unlist(req$filter$values)), collapse = "・"), "の回答者（", nrow(d), "人）に限って集計した。")
    res$report <- paste0(res$filterText, res$report)
  }
  res
}
