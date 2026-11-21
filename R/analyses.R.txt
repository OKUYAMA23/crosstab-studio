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
OPT_RESID <- O_select("residMode", tr_("残差の判定基準"), list(z = tr_("有意水準どおり（補正なし）"), bonf = tr_("ボンフェローニ法で補正")), "z",
                      tr_("どのセルが多い・少ないかを判定する基準です。補正すると、セルの数が多い表で誤って有意と判定することが減ります。"))

hl <- function(k, v) list(k = k, v = v)
cat_types <- c("nominal", "ordinal")
rank_types <- c("ordinal", "numeric")

finish <- function(res, alpha) {
  res$ok <- TRUE
  res$alpha <- alpha
  if (!is.null(res$p) && !is.na(res$p)) res$sig <- res$p < alpha
  if (is.null(res$notes)) res$notes <- character(0)
  if (identical(LANG, "en") && !is.null(res$report)) res$report <- gsub(" {2,}", " ", trimws(res$report))   # 英語の文は ". " で終えて続けるため、最後の空白と重なりを除く
  res$notes <- A(res$notes)
  res
}

small_n_note <- function(chk) {
  if (chk$share_lt5 > 0.2 || chk$min < 1) {
    trf("期待度数が 5 未満のセルが %d 個（%s%%）あります%s。カイ二乗検定の近似が不正確になる目安（5 未満が 20%% 超、または 1 未満がある）に当たります。フィッシャーの正確確率検定を使うか、選択肢をまとめてください。",
        chk$n_lt5, fmt_pct(chk$share_lt5, 0), if (chk$min < 1) tr_("（1 未満のセルを含む）") else "")
  } else NULL
}

# ---------------------------------------------------------------- 単純集計
an_freq <- function(df, req) {
  s <- req$slots$a; alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  res <- list(test = tr_("単純集計"))
  if (identical(s$type, "multi")) {
    M <- slot_multi(df, s); ok <- !is.na(M[, 1]); M <- M[ok, , drop = FALSE]
    n <- nrow(M); if (n == 0) stop_user(q(s$label), tr_(" に回答がありません。"))
    cnt <- colSums(M); pct <- cnt / n
    res$n <- n_info(nrow(df), n)
    res$grids <- list(list(kind = "grid", title = tr_("選択肢ごとの回答数（複数回答）"), corner = s$label, rows = A(colnames(M)),
                           cols = A(c(tr_("回答数"), tr_("回答者に占める割合 (%)"))), fmt = A(c("int", "pct")),
                           cells = mat(cbind(cnt, pct)), note = paste0(tr_("割合の分母は回答者数（n = "), n, tr_("）。複数回答のため合計は 100% を超えることがある。"))))
    res$fig <- list(type = "freq", cats = A(colnames(M)), series = A(s$label), count = mat(cnt), base = n,
                    catTitle = s$label, multi = TRUE)
    o <- order(-cnt)
    res$headline <- list(hl(tr_("回答者"), as.character(n)), hl(tr_("最多"), colnames(M)[o[1]]))
    res$report <- paste0(trf("%s（複数回答、回答者 n = %d）で最も多く選ばれたのは%s（%d人、%s%%）", q(s$label), n, q(colnames(M)[o[1]]), cnt[o[1]], fmt_pct(pct[o[1]])),
                         if (length(o) > 1) trf("で、次いで%s（%d人、%s%%）であった。", q(colnames(M)[o[2]]), cnt[o[2]], fmt_pct(pct[o[2]])) else tr_("であった。"))
    return(finish(res, alpha))
  }
  f <- slot_factor(df, s); f <- f[!is.na(f)]; n <- length(f)
  if (n == 0) stop_user(q(s$label), tr_(" に回答がありません。"))
  tb <- table(f); cnt <- as.integer(tb); pct <- cnt / n
  res$n <- n_info(nrow(df), n)
  res$grids <- list(list(kind = "grid", title = tr_("度数分布"), corner = s$label, rows = A(names(tb)), cols = A(c(tr_("度数"), tr_("割合 (%)"))),
                         fmt = A(c("int", "pct")), cells = mat(cbind(cnt, pct)), total = A(c(n, 1))))
  res$fig <- list(type = "freq", cats = A(names(tb)), series = A(s$label), count = mat(cnt), base = n, catTitle = s$label)
  o <- order(-cnt)
  txt <- paste0(trf("%sの回答（n = %d）で最も多かったのは%s（%d人、%s%%）", q(s$label), n, q(names(tb)[o[1]]), cnt[o[1]], fmt_pct(pct[o[1]])),
                if (length(o) > 1) trf("で、次いで%s（%d人、%s%%）であった。", q(names(tb)[o[2]]), cnt[o[2]], fmt_pct(pct[o[2]])) else tr_("であった。"))
  res$headline <- list(hl("n", as.character(n)), hl(tr_("最多"), paste0(names(tb)[o[1]], tr_("（"), fmt_pct(pct[o[1]]), tr_("%）"))))
  if (isTRUE(opt(req, "gof", FALSE))) {
    if (length(tb) < 2) stop_user(tr_("適合度の検定には選択肢が 2 つ以上必要です。"))
    ct <- suppressWarnings(stats::chisq.test(tb))
    res$p <- unname(ct$p.value); res$test <- tr_("適合度のカイ二乗検定")
    st <- paste0("χ²(", ct$parameter, ") = ", fmt_num(ct$statistic), ", ", fmt_p(res$p, lead0))
    res$headline <- c(res$headline, list(hl(paste0("χ²(", ct$parameter, ")"), fmt_num(ct$statistic)), hl("p", fmt_p(res$p, lead0, FALSE))))
    res$row <- list(stat = paste0("χ²(", ct$parameter, ") = ", fmt_num(ct$statistic)), effect = "")
    if (any(ct$expected < 5)) res$notes <- tr_("期待度数が 5 未満の選択肢があります。適合度の検定の近似が不正確な可能性があります。")
    txt <- paste0(txt, trf("すべての選択肢が同じ割合で選ばれるという仮定に対する適合度のカイ二乗検定の結果、回答の偏りは%s（%s）。",
                  if (res$p < alpha) tr_("統計的に有意であった") else tr_("統計的に有意ではなかった"), st))
  }
  res$report <- txt
  finish(res, alpha)
}
register("freq", tr_("単純集計"), tr_("集計"), tr_("1 つの質問について、選択肢ごとの人数と割合を出します。複数回答にも使えます。"),
         list(S("a", tr_("集計する質問"), c("nominal", "ordinal", "multi"))),
         list(O_check("gof", tr_("回答の偏りを検定する（適合度の検定）"), FALSE, tr_("すべての選択肢が同じ割合で選ばれる、という仮定と比べます。単一回答のときだけ使えます。"))),
         an_freq)

# ---------------------------------------------------------------- 複数の設問の単純集計
# 同じ選択肢で答える複数の設問（例：サービスごとの利用頻度）を並べて集計する。
# 表は「行＝設問、列＝回答」。図の既定は、回答ごとの棒を設問ごとに色を変えて並べる形。
items_setup <- function(df, slot) {
  cols <- unlist(slot$cols); k <- length(cols)
  if (length(slot$pointsList) > 1 && length(unique(lapply(slot$pointsList, unlist))) > 1)
    stop_user(tr_("選んだ項目の配点がそろっていません。同じ配点にしてください（逆の向きの項目は「逆転項目」で指定します）。"))
  if (k < 2) stop_user(tr_("比べる設問を 2 つ以上選んでください（現在 "), k, tr_(" 個）。"))
  if (anyDuplicated(cols)) stop_user(tr_("同じ設問が 2 回選ばれています。別の設問を選んでください。"))
  if (length(slot$cats) == 0) stop_user(tr_("選択肢がすべて外されています。「選択肢の設定」で 1 つ以上にチェックを入れてください。"))
  lev <- unique(slot_labels(slot)); keys <- character(0); vals <- character(0)
  for (c in slot$cats) for (v in unlist(c$values)) { keys <- c(keys, v); vals <- c(vals, as.character(c$label)) }
  F <- lapply(cols, function(j) factor(vals[match(df[[j]], keys)], levels = lev))
  items <- unlist(slot$items); stems <- unlist(slot$stems); labels <- unlist(slot$labels); fulls <- unlist(slot$fulls)
  same_stem <- length(items) == k && all(nzchar(items)) && length(unique(stems)) == 1 && nzchar(stems[1])
  names <- if (same_stem) items else labels
  list(cols = cols, k = k, lev = lev, F = F, names = make.unique(names, sep = " "), same_stem = same_stem, stem = if (same_stem) stems[1] else "", fulls = fulls)
}
# 選択肢が「1 位」「2 位」…（または 1, 2, …、第 1 位、1st など）で、1〜k（k は項目の数）の順位になっているとき、選択肢ごとの順位の数を返す。そうでなければ NULL
rank_levels <- function(lev, k) {
  z <- chartr("０１２３４５６７８９", "0123456789", as.character(lev))
  m <- regmatches(z, regexec("^\\s*(?:第\\s*|[Rr]ank\\s*)?([0-9]+)\\s*(?:位|番目|番|st|nd|rd|th)?\\s*$", z, perl = TRUE))
  if (!length(lev) || any(vapply(m, length, 0L) != 2)) return(NULL)
  r <- as.numeric(vapply(m, `[`, "", 2))
  if (anyDuplicated(r) || !setequal(r, seq_len(k))) return(NULL)
  r
}
an_freqmulti <- function(df, req) {
  s <- req$slots$items; alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  it <- items_setup(df, s); k <- it$k
  # 順位を付ける設問（選択肢が 1〜k 位）の点検：同じ順位を複数の項目に付けた回答（同順位）を数え、指定があれば除く
  rk <- rank_levels(it$lev, k); tienote <- NULL; tiedrop <- 0L
  if (!is.null(rk)) {
    R <- vapply(it$F, function(f) rk[as.integer(f)], numeric(nrow(df))); if (!is.matrix(R)) R <- matrix(R, nrow = 1)
    tie <- apply(R, 1, function(r) { r <- r[!is.na(r)]; length(r) > 1 && any(duplicated(r)) })
    if (any(tie)) {
      if (isTRUE(opt(req, "excltie", FALSE))) {
        tiedrop <- sum(tie); df <- df[!tie, , drop = FALSE]; it <- items_setup(df, s); k <- it$k
        tienote <- trf("同じ順位を複数の項目に付けた回答 %d 件を除いて集計した。", tiedrop)
      } else tienote <- trf("同じ順位を複数の項目に付けた回答が %d 件あります（順位の並べ替えになっていない回答）。除くときは、オプションの「同順位を含む回答を除く」を選んでください。", sum(tie))
    }
  } else if (isTRUE(opt(req, "excltie", FALSE))) tienote <- tr_("選択肢が「1 位」「2 位」…の順位ではないため、「同順位を含む回答を除く」は使っていません。")
  cnt <- t(vapply(it$F, function(f) as.integer(table(f)), integer(length(it$lev))))
  if (length(it$lev) == 1) cnt <- matrix(cnt, ncol = 1)
  dimnames(cnt) <- list(it$names, it$lev)
  nn <- rowSums(cnt)
  if (any(nn == 0)) stop_user(tr_("回答のない設問があります（"), paste(it$names[nn == 0], collapse = tr_("、")), tr_("）。その設問を外してください。"))
  keepc <- colSums(cnt) > 0; tab <- as.table(cnt[, keepc, drop = FALSE])
  res <- list(test = tr_("複数の設問の単純集計"), n = n_info(nrow(df), max(nn)))
  res$xtab <- xtab_obj(tab, tr_("設問"), tr_("回答"), resid = FALSE)
  res$fig <- list(type = "items", prefer = "stack100")
  rp <- prop.table(tab, 1); mi <- apply(tab, 1, which.max)
  subj <- if (it$same_stem) trf("%sの各項目（%s）", q(it$stem), paste(it$names, collapse = tr_("、"))) else paste(vapply(it$fulls, q, ""), collapse = tr_("、"))
  first <- colnames(tab)[1]
  txt <- trf("%sの回答を集計した（回答者 %s）。", subj, paste0(it$names, " n = ", nn, collapse = tr_("、")))
  txt <- paste0(txt, trf("最も多い回答は、%sであった。", paste(trf("%sでは%s（%s%%）", it$names, q(colnames(tab)[mi]), vapply(seq_len(k), function(i) fmt_pct(rp[i, mi[i]]), "")), collapse = tr_("、"))))
  txt <- paste0(txt, trf("%sと答えた割合は、%sであった。", q(first), paste0(it$names, " ", fmt_pct(rp[, 1]), "%", collapse = tr_("、"))))
  res$headline <- list(hl(tr_("設問"), as.character(k)), hl(tr_("回答者"), if (length(unique(nn)) == 1) as.character(nn[1]) else paste0(min(nn), tr_("〜"), max(nn))))
  res$row <- list(stat = "", effect = "")
  res$notes <- c(tr_("割合の分母は、設問ごとの回答者数です。"), tienote)
  if (tiedrop > 0) txt <- paste0(tienote, txt)
  if (isTRUE(opt(req, "friedman", FALSE))) {
    sc <- sapply(seq_along(it$F), function(j) lev_points(req$slots$items, it$lev, j)[as.integer(it$F[[j]])]); ok <- stats::complete.cases(sc); m <- sc[ok, , drop = FALSE]
    if (nrow(m) < 2) stop_user(tr_("すべての設問に答えた人が 2 人未満のため、フリードマン検定ができません。"))
    ft <- stats::friedman.test(m); Q <- unname(ft$statistic); W <- Q / (nrow(m) * (k - 1))
    res$chk <- list(Q = Q, W = W, n = nrow(m))
    res$p <- unname(ft$p.value); res$test <- tr_("複数の設問の単純集計（フリードマン検定）")
    mr <- colMeans(t(apply(m, 1, rank)))
    res$headline <- c(res$headline, list(hl(paste0("χ²(", ft$parameter, ")"), fmt_num(Q)), hl("p", fmt_p(res$p, lead0, FALSE)), hl(tr_("Kendall の W"), fmt_num(W))))
    res$row <- list(stat = paste0("χ²(", ft$parameter, ") = ", fmt_num(Q)), effect = paste0("W = ", fmt_num(W)))
    res$grids <- list(list(kind = "grid", title = tr_("設問ごとの平均順位（フリードマン検定）"), corner = tr_("設問"), rows = A(it$names), cols = A(tr_("平均順位")), fmt = A("num2"), cells = mat(matrix(mr, ncol = 1)),
                           note = trf("すべての設問に答えた %d 人で計算した。順位は%sの側を上位とし、1 人の中で設問に順位を付けた。", nrow(m), q(top_label(it$lev, lev_points(req$slots$items, it$lev, 1))))))
    o <- order(-mr); sig <- res$p < alpha
    txt <- paste0(txt, trf("すべての設問に答えた %d 人についてフリードマン検定を行った結果、設問の間の回答の傾向の差は%s（χ²(%s) = %s, %s, Kendall の W = %s）。",
                  nrow(m), if (sig) tr_("統計的に有意であった") else tr_("統計的に有意ではなかった"), ft$parameter, fmt_num(Q), fmt_p(res$p, lead0), fmt_num(W)),
                  if (sig) trf("平均順位は%sの順に高かった。", paste0(it$names[o], tr_("（"), fmt_num(mr[o]), tr_("）"), collapse = tr_("、"))) else "")
    res$notes <- c(res$notes, tr_("フリードマン検定は、同じ人が答えた複数の設問の間で回答の傾向に差があるかを調べる検定です。Kendall の W は効果量（0〜1）です。"))
  }
  res$report <- txt
  finish(res, alpha)
}
register("freqmulti", tr_("複数の設問の単純集計"), tr_("集計"), tr_("同じ選択肢で答える複数の設問（例：サービスごとの利用頻度）を並べて集計し、設問ごとに色を変えたグラフにします。"),
         list(S("items", tr_("比べる設問（同じ選択肢）"), cat_types, tr_("例：Q4 の各サービス"), FALSE, TRUE)),
         list(O_check("friedman", tr_("設問の間で回答の傾向を比べる（フリードマン検定）"), FALSE, tr_("順序のある回答で、同じ人が答えた複数の設問の間に差があるかを調べます。すべての設問に答えた人だけで計算します。")),
              O_check("excltie", tr_("同順位を含む回答を除く"), FALSE, tr_("項目に 1 位、2 位…と順位を付ける設問で、同じ順位を複数の項目に付けた回答を除きます。選択肢が順位のときだけ使います。"))),
         an_freqmulti)

# ---------------------------------------------------------------- クロス集計（カイ二乗・フィッシャー）
crosstab_core <- function(df, req, method) {
  sa <- req$slots$a; sb <- req$slots$b
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  mt <- make_table(slot_factor(df, sa), slot_factor(df, sb)); tab <- mt$tab
  if (mt$n == 0) stop_user(tr_("2 つの質問の両方に答えた人がいません。列の組み合わせや絞り込みを見直してください。"))
  if (nrow(tab) < 2 || ncol(tab) < 2)
    stop_user(tr_("検定には、行と列のそれぞれに回答のある選択肢が 2 つ以上必要です（現在 "), nrow(tab), tr_(" 行 × "), ncol(tab), tr_(" 列）。選択肢の設定や絞り込みを見直してください。"))
  xt <- xtab_obj(tab, sa$label, sb$label, alpha, opt(req, "residMode", "z"))
  chk <- expected_check(tab); V <- cramers_v(tab); is22 <- all(dim(tab) == 2)
  res <- list(xtab = xt, n = n_info(nrow(df), mt$n), fig = list(type = "xtab"), notes = character(0))
  if (length(mt$dropped)) res$notes <- c(res$notes, paste0(tr_("回答が 0 人の選択肢（"), paste(mt$dropped, collapse = tr_("、")), tr_("）は表から除きました。")))
  vname <- if (is22) "φ" else "Cramér's V"
  if (method == "chisq") {
    yates <- isTRUE(opt(req, "yates", FALSE)) && is22
    ct <- suppressWarnings(stats::chisq.test(tab, correct = yates))
    res$p <- unname(ct$p.value)
    res$test <- if (yates) tr_("カイ二乗検定（イェーツの補正）") else tr_("カイ二乗検定")
    stat <- paste0("χ²(", ct$parameter, ") = ", fmt_num(ct$statistic))
    stats_txt <- paste0(stat, ", ", fmt_p(res$p, lead0), ", ", vname, " = ", fmt_num(V))
    res$headline <- list(hl(paste0("χ²(", ct$parameter, ")"), fmt_num(ct$statistic)), hl("p", fmt_p(res$p, lead0, FALSE)),
                         hl(vname, fmt_num(V)), hl("n", as.character(mt$n)))
    res$row <- list(stat = stat, effect = paste0(vname, " = ", fmt_num(V)))
    sn <- small_n_note(chk)
    if (!is.null(sn)) { res$notes <- c(res$notes, sn); res$suggest <- "fisher" }
    caution <- if (!is.null(sn)) trf("なお、期待度数が 5 未満のセルが %d 個（全 %d セルの %s%%）あり、カイ二乗検定の近似が不正確な可能性がある。", chk$n_lt5, chk$cells, fmt_pct(chk$share_lt5, 0)) else ""
  } else {
    sim <- FALSE
    # 大きな表の正確な計算は数分かかることがあるため、既定の作業領域で解けないときは
    # モンテカルロ法（20 万回、乱数の種は固定）に切り替える。
    ft <- tryCatch(stats::fisher.test(tab, workspace = 2e6), error = function(e) NULL)
    if (is.null(ft)) { set.seed(20240101); ft <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 200000); sim <- TRUE }
    res$p <- unname(ft$p.value)
    res$test <- tr_("フィッシャーの正確確率検定")
    stats_txt <- paste0(fmt_p(res$p, lead0), ", ", vname, " = ", fmt_num(V))
    res$headline <- list(hl("p", fmt_p(res$p, lead0, FALSE)), hl(vname, fmt_num(V)), hl("n", as.character(mt$n)))
    res$row <- list(stat = "–", effect = paste0(vname, " = ", fmt_num(V)))
    if (is22) {
      or <- unname(ft$estimate); ci <- ft$conf.int
      or_txt <- paste0(tr_("オッズ比 = "), fmt_num(or), ", 95%CI [", fmt_num(ci[1]), ", ", fmt_num(ci[2]), "]")
      stats_txt <- paste0(stats_txt, ", ", or_txt)
      res$headline <- append(res$headline, list(hl(tr_("オッズ比"), paste0(fmt_num(or), " [", fmt_num(ci[1]), ", ", fmt_num(ci[2]), "]"))), 2)
      res$row$effect <- paste0(tr_("オッズ比 = "), fmt_num(or))
      res$notes <- c(res$notes, trf("オッズ比は、%sが%sに比べて%sと答えるオッズの比（条件付き最尤推定値）です。", rownames(tab)[1], rownames(tab)[2], q(colnames(tab)[1])))
    }
    if (sim) res$notes <- c(res$notes, tr_("表が大きく正確な計算に時間がかかりすぎるため、p 値はモンテカルロ法（20 万回、乱数の種は固定）による推定値です。"))
    caution <- if (sim) tr_("p 値はモンテカルロ法（20 万回）による推定値である。") else ""
  }
  sig <- res$p < alpha
  txt <- paste0(trf("%sと%sのクロス集計を行った（有効回答 n = %d）。", q(sa$label), q(sb$label), mt$n), modal_sentence(tab, sb$label),
                trf("%sの結果、%s（%s）。", lc_(res$test), if (sig) tr_("両者の関連は統計的に有意であった") else tr_("両者の間に統計的に有意な関連は認められなかった"), stats_txt),
                if (sig) resid_sentence(xt, tab) else "", caution)
  res$report <- txt
  finish(res, alpha)
}
register("chisq", tr_("カイ二乗検定"), tr_("2 つの質問の関連"), tr_("2 つの質問の答え方に関連があるかを調べます。クロス集計の基本の検定です。"),
         list(S("a", tr_("行（比べるグループ）"), cat_types, tr_("例：性別、学年")), S("b", tr_("列（回答）"), cat_types, tr_("例：ある・ない"))),
         list(O_check("yates", tr_("イェーツの補正を使う（2×2 の表のとき）"), FALSE, tr_("2×2 の表で p 値を控えめにする補正です。人数が少ない 2×2 の表では、フィッシャーの正確確率検定のほうが適しています。")), OPT_RESID),
         function(df, req) crosstab_core(df, req, "chisq"))
register("fisher", tr_("フィッシャーの正確確率検定"), tr_("2 つの質問の関連"), tr_("人数が少ないセルがあるときに使う、関連の検定です。2×2 の表ではオッズ比も出します。"),
         list(S("a", tr_("行（比べるグループ）"), cat_types, tr_("例：性別、学年")), S("b", tr_("列（回答）"), cat_types, tr_("例：ある・ない"))),
         list(OPT_RESID),
         function(df, req) crosstab_core(df, req, "fisher"))

# ---------------------------------------------------------------- 順位による群の比較
rank_setup <- function(df, req) {
  sg <- req$slots$g; sy <- req$slots$y
  g <- slot_factor(df, sg); y <- slot_score(df, sy)
  ok <- !is.na(g) & !is.na(y)
  g <- droplevels(g[ok]); y <- y[ok]
  list(sg = sg, sy = sy, g = g, y = y, n = sum(ok), numeric = identical(sy$type, "numeric"),
       ylev = if (identical(sy$type, "numeric")) NULL else unique(slot_labels(sy)),
       ypts = if (identical(sy$type, "numeric")) NULL else lev_points(sy, unique(slot_labels(sy))),
       yf = if (identical(sy$type, "numeric")) NULL else slot_factor(df, sy)[ok])
}
rank_tables <- function(rs, df) {
  g <- rs$g; y <- rs$y; r <- rank(y)
  ng <- as.integer(table(g)); mr <- as.numeric(tapply(r, g, mean))
  out <- list()
  if (rs$numeric) {
    sm <- t(sapply(levels(g), function(l) { v <- y[g == l]; c(length(v), mean(v), stats::sd(v), stats::median(v), stats::quantile(v, .25), stats::quantile(v, .75), mean(r[g == l])) }))
    out$grids <- list(list(kind = "grid", title = tr_("群ごとの要約"), corner = rs$sg$label, rows = A(levels(g)),
                           cols = A(c("n", tr_("平均"), tr_("標準偏差"), tr_("中央値"), tr_("第1四分位"), tr_("第3四分位"), tr_("平均順位"))),
                           fmt = A(c("int", "num2", "num2", "num2", "num2", "num2", "num1")), cells = mat(sm)))
    bx <- lapply(levels(g), function(l) { v <- y[g == l]; b <- grDevices::boxplot.stats(v); list(stats = A(b$stats), out = A(b$out), n = length(v)) })
    out$fig <- list(type = "box", cats = A(levels(g)), box = bx, catTitle = rs$sg$label, valueTitle = rs$sy$label)
  } else {
    yf <- rs$yf
    tab <- table(g, yf); tab <- tab[, colSums(tab) > 0, drop = FALSE]
    out$xtab <- xtab_obj(tab, rs$sg$label, rs$sy$label, resid = FALSE)
    med <- vapply(levels(g), function(l) median_label(y[g == l], rs$ylev, rs$ypts), "")
    out$grids <- list(list(kind = "grid", title = tr_("群ごとの順位の要約"), corner = rs$sg$label, rows = A(levels(g)),
                           cols = A(c("n", tr_("平均順位"), tr_("中央値にあたる回答"))), fmt = A(c("int", "num1", "text")),
                           cells = lapply(seq_along(ng), function(i) list(ng[i], mr[i], gsub("[「」]", "", med[i]))),
                           note = trf("順位は%sの側を上位として付けた。", q(top_label(rs$ylev, rs$ypts)))))
    out$fig <- list(type = "xtab"); out$med <- med
  }
  out$ng <- ng; out$mr <- mr
  out
}
direction_phrase <- function(rs, hi, lo) {
  if (rs$numeric) trf("%sのほうが%sより値が大きい傾向にあった", gq(hi), gq(lo))
  else trf("%sのほうが%sより%sの側の回答が多かった", gq(hi), gq(lo), q(top_label(rs$ylev, rs$ypts)))
}

an_mwu <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  rs <- rank_setup(df, req); g <- rs$g; y <- rs$y
  if (nlevels(g) != 2) stop_user(tr_("マン・ホイットニーの U 検定は 2 つの群の比較です。"), q(rs$sg$label), tr_("は現在 "), nlevels(g), tr_(" 群です。「選択肢の設定」で 2 つにする（まとめる・チェックを外す）か、3 群以上のときはクラスカル・ウォリス検定を使ってください。"))
  if (length(unique(y)) < 2) stop_user(tr_("すべての回答が同じ値のため、検定できません。"))
  l <- levels(g); x1 <- y[g == l[1]]; x2 <- y[g == l[2]]; n1 <- length(x1); n2 <- length(x2); N <- n1 + n2
  wt <- wilcox_two(x1, x2)
  U1 <- unname(wt$statistic); U <- min(U1, n1 * n2 - U1)
  tt <- table(c(x1, x2)); sigma <- sqrt(n1 * n2 / 12 * ((N + 1) - sum(tt^3 - tt) / (N * (N - 1))))
  d <- U1 - n1 * n2 / 2; Z <- (d - sign(d) * 0.5) / sigma; r_eff <- abs(Z) / sqrt(N)
  tb <- rank_tables(rs, df)
  res <- list(test = tr_("マン・ホイットニーの U 検定"), p = unname(wt$p.value), n = n_info(nrow(df), N), xtab = tb$xtab, grids = tb$grids, fig = tb$fig)
  stats_txt <- paste0("U = ", fmt_num(U, 1), ", |Z| = ", fmt_num(abs(Z)), ", ", fmt_p(res$p, lead0), ", r = ", fmt_num(r_eff))
  res$headline <- list(hl("U", fmt_num(U, 1)), hl("|Z|", fmt_num(abs(Z))), hl("p", fmt_p(res$p, lead0, FALSE)), hl("r", fmt_num(r_eff)), hl("n", as.character(N)))
  res$row <- list(stat = paste0("U = ", fmt_num(U, 1)), effect = paste0("r = ", fmt_num(r_eff)))
  hi <- if (tb$mr[1] >= tb$mr[2]) 1 else 2; lo <- 3 - hi
  sig <- res$p < alpha
  res$notes <- tr_("p 値は正規近似（同順位の補正と連続性の補正あり）によります。効果量 r は |Z| / √N です。")
  res$report <- paste0(trf("%sの 2 群（%s n = %d、%s n = %d）で%sの回答を比較した。", q(rs$sg$label), l[1], n1, l[2], n2, q(rs$sy$label)),
                       trf("マン・ホイットニーの U 検定の結果、2 群の回答の分布には%s（%s）。", if (sig) tr_("統計的に有意な差が認められた") else tr_("統計的に有意な差は認められなかった"), stats_txt),
                       if (sig) trf("平均順位は%s（%s）が%s（%s）より高く、%s。", gq(l[hi]), fmt_num(tb$mr[hi], 1), gq(l[lo]), fmt_num(tb$mr[lo], 1), direction_phrase(rs, l[hi], l[lo])) else "",
                       if (!rs$numeric) trf("中央値にあたる回答は、%sが%s、%sが%sであった。", gq(l[1]), tb$med[1], gq(l[2]), tb$med[2]) else "")
  finish(res, alpha)
}
register("mwu", tr_("マン・ホイットニーの U 検定"), tr_("順序のある回答の比較"), tr_("2 つのグループで、段階のある回答（あてはまる〜あてはまらない など）の傾向が違うかを調べます。"),
         list(S("g", tr_("グループ（2 群）"), cat_types, tr_("例：性別"), TRUE), S("y", tr_("比べる回答（順序あり）"), rank_types, tr_("段階で答える質問、または数値"), TRUE)),
         list(), an_mwu)

an_kw <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  rs <- rank_setup(df, req); g <- rs$g; y <- rs$y; k <- nlevels(g); N <- rs$n
  if (k < 2) stop_user(tr_("比較するには、"), q(rs$sg$label), tr_("に回答のある群が 2 つ以上必要です。"))
  if (length(unique(y)) < 2) stop_user(tr_("すべての回答が同じ値のため、検定できません。"))
  kt <- stats::kruskal.test(y, g); H <- unname(kt$statistic); eps <- H / (N - 1)
  tb <- rank_tables(rs, df)
  res <- list(test = tr_("クラスカル・ウォリス検定"), p = unname(kt$p.value), n = n_info(nrow(df), N), xtab = tb$xtab, grids = tb$grids, fig = tb$fig)
  stats_txt <- paste0("H(", kt$parameter, ") = ", fmt_num(H), ", ", fmt_p(res$p, lead0), ", ε² = ", fmt_num(eps))
  res$headline <- list(hl(paste0("H(", kt$parameter, ")"), fmt_num(H)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("ε²", fmt_num(eps)), hl("n", as.character(N)))
  res$row <- list(stat = paste0("H(", kt$parameter, ") = ", fmt_num(H)), effect = paste0("ε² = ", fmt_num(eps)))
  sig <- res$p < alpha; post <- ""
  if (k >= 3) {
    pw <- pairwise_wilcox_holm(y, g)
    pairs <- list(); pv <- numeric(0)
    for (j in seq_len(ncol(pw))) for (i in seq_len(nrow(pw))) if (!is.na(pw[i, j])) {
      pairs[[length(pairs) + 1]] <- paste0(colnames(pw)[j], " – ", rownames(pw)[i]); pv <- c(pv, pw[i, j]) }
    res$grids <- c(res$grids, list(list(kind = "grid", title = tr_("多重比較（マン・ホイットニーの U 検定、ホルム法で補正）"), corner = tr_("組み合わせ"),
                                        rows = A(unlist(pairs)), cols = A(tr_("補正後の p")), fmt = A("p"), cells = mat(matrix(pv, ncol = 1)),
                                        mark = mat(matrix(as.numeric(pv < alpha), ncol = 1)))))
    if (sig) {
      sp <- which(pv < alpha)
      post <- if (length(sp)) trf("多重比較（ホルム法で補正したマン・ホイットニーの U 検定）では、%sの間に有意な差が認められた。", paste0(unlist(pairs)[sp], tr_("（"), vapply(pv[sp], fmt_p, "", lead0 = lead0), tr_("）"), collapse = tr_("、")))
              else tr_("多重比較（ホルム法で補正したマン・ホイットニーの U 検定）では、有意な差のある組み合わせは認められなかった。")
    }
  }
  o <- order(-tb$mr)
  res$notes <- tr_("効果量 ε² は H / (N − 1) です。")
  res$report <- paste0(trf("%sの %d 群で%sの回答を比較した（n = %d）。", q(rs$sg$label), k, q(rs$sy$label), N),
                       trf("クラスカル・ウォリス検定の結果、群の間に%s（%s）。", if (sig) tr_("統計的に有意な差が認められた") else tr_("統計的に有意な差は認められなかった"), stats_txt),
                       if (sig) trf("平均順位は%sの順に高かった。", paste0(vapply(levels(g)[o], q, ""), tr_("（"), vapply(tb$mr[o], fmt_num, "", d = 1), tr_("）"), collapse = tr_("、"))) else "", post)
  finish(res, alpha)
}
register("kw", tr_("クラスカル・ウォリス検定"), tr_("順序のある回答の比較"), tr_("3 つ以上のグループで、段階のある回答の傾向が違うかを調べます。どの組み合わせに差があるかも出します。"),
         list(S("g", tr_("グループ（3 群以上）"), cat_types, tr_("例：学年、利用頻度"), TRUE), S("y", tr_("比べる回答（順序あり）"), rank_types, tr_("段階で答える質問、または数値"), TRUE)),
         list(), an_kw)

# ---------------------------------------------------------------- スピアマンの順位相関
an_spearman <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  sx <- req$slots$x; sy <- req$slots$y
  x <- slot_score(df, sx); y <- slot_score(df, sy); ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; n <- sum(ok)
  if (n < 4) stop_user(tr_("相関を求めるには、両方に答えた人が 4 人以上必要です（現在 "), n, tr_(" 人）。"))
  if (length(unique(x)) < 2 || length(unique(y)) < 2) stop_user(tr_("どちらかの回答がすべて同じ値のため、相関を求められません。"))
  ct <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE)); rho <- unname(ct$estimate)
  res <- list(test = tr_("スピアマンの順位相関"), p = unname(ct$p.value), n = n_info(nrow(df), n))
  numx <- identical(sx$type, "numeric"); numy <- identical(sy$type, "numeric")
  if (!numx && !numy) {
    lx <- unique(slot_labels(sx)); ly <- unique(slot_labels(sy))
    tab <- table(slot_factor(df, sx)[ok], slot_factor(df, sy)[ok])
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
    res$xtab <- xtab_obj(tab, sx$label, sy$label, resid = FALSE); res$fig <- list(type = "xtab", prefer = "heat")
  } else {
    # 一方が順序のある回答のとき：散布図の軸に選択肢の名前を出す（lev：名前と点）。
    #   選択肢が 7 個以下で、もう一方の値の種類が 12 個以下なら、既定の図を「もう一方の値ごとの 100% 帯グラフ」にする（点が重なって人数が読めないため）
    levOf <- function(s) if (identical(s$type, "numeric")) NULL else { lv <- unique(slot_labels(s)); list(labels = A(lv), points = A(lev_points(s, lv))) }
    sc <- list(type = "scatter", x = A(x), y = A(y), xTitle = sx$label, yTitle = sy$label, xLev = levOf(sx), yLev = levOf(sy))
    res$fig <- sc
    if (xor(numx, numy)) {
      so <- if (numx) sy else sx; nv <- if (numx) x else y; uo <- sort(unique(nv)); lo <- unique(slot_labels(so))
      if (length(lo) <= 7 && length(uo) <= 12) {
        fo <- slot_factor(df, so)[ok]
        tab <- table(factor(format(nv, trim = TRUE, drop0trailing = TRUE, scientific = FALSE), levels = format(uo, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)), fo)
        tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
        res$xtab <- xtab_obj(tab, (if (numx) sx else sy)$label, so$label, resid = FALSE)
        res$fig <- list(type = "xtab", prefer = "stack100", sc = sc)
      }
    }
  }
  a <- abs(rho); strength <- if (a < 0.2) tr_("ごく弱い") else if (a < 0.4) tr_("弱い") else if (a < 0.7) tr_("中程度の") else tr_("強い")
  stats_txt <- paste0("ρ = ", fmt_num(rho), ", ", fmt_p(res$p, lead0), ", n = ", n)
  res$headline <- list(hl("ρ", fmt_num(rho)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("n", as.character(n)))
  res$row <- list(stat = paste0("ρ = ", fmt_num(rho)), effect = "–")
  sig <- res$p < alpha
  top <- function(s) if (identical(s$type, "numeric")) tr_("値が大きい人") else { lv <- unique(slot_labels(s)); trf("%sの側の回答をした人", q(top_label(lv, lev_points(s, lv)))) }
  res$notes <- c(tr_("相関の強さの表現（|ρ| が 0.2 未満：ごく弱い、0.4 未満：弱い、0.7 未満：中程度、0.7 以上：強い）は目安であり、分野によって基準が異なります。"),
                 tr_("相関は 2 つの回答の関連を示すもので、一方が他方の原因であることを示すものではありません。"))
  res$report <- paste0(trf("%sと%sの関連をスピアマンの順位相関係数で調べた。その結果、%s（%s）。", q(sx$label), q(sy$label),
                       if (sig) trf("%s%sの相関が認められた", strength, if (rho > 0) tr_("正") else tr_("負")) else tr_("統計的に有意な相関は認められなかった"), stats_txt),
                       if (!sig) "" else if (rho > 0) trf("%sで%sほど、%sで%sが多い傾向にあった。", q(sx$label), top(sx), q(sy$label), top(sy))
                       else trf("%sで%sほど、%sで%sが少ない傾向にあった。", q(sx$label), top(sx), q(sy$label), top(sy)))
  finish(res, alpha)
}
register("spearman", tr_("スピアマンの順位相関"), tr_("順序のある回答の比較"), tr_("段階のある 2 つの回答が、一方が高いほど他方も高い（または低い）関係にあるかを調べます。"),
         list(S("x", tr_("回答 1（順序あり）"), rank_types, "", TRUE), S("y", tr_("回答 2（順序あり）"), rank_types, "", TRUE)), list(), an_spearman)

# ---------------------------------------------------------------- 複数回答 × グループ
an_multi <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  sm <- req$slots$m; sg <- req$slots$g
  M <- slot_multi(df, sm); g <- slot_factor(df, sg); ok <- !is.na(M[, 1]) & !is.na(g)
  M <- M[ok, , drop = FALSE]; g <- droplevels(g[ok]); n <- sum(ok)
  if (n == 0) stop_user(tr_("両方の質問に答えた人がいません。"))
  if (nlevels(g) < 2) stop_user(tr_("比較するには、"), q(sg$label), tr_("に回答のある群が 2 つ以上必要です。"))
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
      pv[j] <- ft$p.value; meth[j] <- tr_("フィッシャーの正確確率検定")
    } else { pv[j] <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))$p.value; meth[j] <- tr_("カイ二乗検定") }
  }
  padj <- stats::p.adjust(pv, method = "holm")
  cells <- lapply(seq_along(items), function(j) c(as.list(unlist(lapply(seq_along(ng), function(i) list(cnt[j, i], pct[j, i])))), list(meth[j], pv[j], padj[j])))
  cols <- c(unlist(lapply(levels(g), function(l) c(paste0(l, tr_(" 人数")), paste0(l, " %")))), tr_("検定"), "p", tr_("補正後の p"))
  fm <- c(rep(c("int", "pct"), length(ng)), "text", "p", "p")
  res <- list(test = tr_("複数回答のクロス集計"), n = n_info(nrow(df), n))
  res$grids <- list(list(kind = "grid", title = tr_("選択肢ごとの選択率と検定"), corner = sm$label, rows = A(items), cols = A(cols), fmt = A(fm), cells = cells,
                         markCol = length(cols), markRows = A(as.numeric(!is.na(padj) & padj < alpha)),
                         note = paste0(tr_("割合の分母は各群の回答者数（"), paste0(levels(g), " n = ", ng, collapse = tr_("、")), tr_("）。補正後の p はホルム法による。"))))
  res$fig <- list(type = "multi", cats = A(items), series = A(levels(g)), count = mat(cnt), pct = mat(pct), catTitle = sm$label, seriesTitle = sg$label)
  sgn <- which(!is.na(padj) & padj < alpha)
  res$p <- if (all(is.na(padj))) NULL else min(padj, na.rm = TRUE)
  res$headline <- list(hl(tr_("選択肢"), as.character(length(items))), hl(tr_("有意な選択肢"), as.character(length(sgn))), hl(tr_("最小の補正後 p"), fmt_p(res$p, lead0, FALSE)), hl("n", as.character(n)))
  res$row <- list(stat = paste0(tr_("有意な選択肢 "), length(sgn), " / ", length(items)), effect = "")
  used <- unique(meth[meth != ""])
  res$notes <- c(tr_("選択肢の数だけ検定を繰り返すため、p 値はホルム法で補正しています。"), tr_("期待度数が 5 未満のセルを含む選択肢にはフィッシャーの正確確率検定、それ以外にはカイ二乗検定を使っています。"))
  res$report <- paste0(trf("%s（複数回答）の各選択肢が選ばれた割合を、%sの群別に集計した（n = %d）。", q(sm$label), q(sg$label), n),
                       trf("選択肢ごとに%sを行い、p 値をホルム法で補正した結果、%s", paste0(lc_(used), collapse = tr_("または")),
                       if (length(sgn)) trf("%sで群の間に有意な差が認められた。", paste0(q(items[sgn]), tr_("（"), vapply(sgn, function(j) paste0(levels(g), " ", fmt_pct(pct[j, ]), "%", collapse = tr_("、")), ""), tr_("、補正後 "), vapply(padj[sgn], fmt_p, "", lead0 = lead0), tr_("）"), collapse = tr_("、")))
                       else tr_("いずれの選択肢でも群の間に有意な差は認められなかった。")))
  finish(res, alpha)
}
register("multi", tr_("複数回答のクロス集計"), tr_("複数回答"), tr_("「あてはまるものをすべて」の質問で、選択肢ごとの選ばれた割合をグループ別に比べます。"),
         list(S("m", tr_("複数回答の質問"), "multi"), S("g", tr_("グループ"), cat_types, tr_("例：性別、学年"), TRUE)), list(), an_multi)

# ---------------------------------------------------------------- 同じ人の 2 つの回答の比較
paired_setup <- function(df, req, need2 = FALSE) {
  sx <- req$slots$x; sy <- req$slots$y
  lx <- unique(slot_labels(sx)); ly <- unique(slot_labels(sy))
  if (!identical(lx, ly)) stop_user(tr_("この検定では、2 つの質問の選択肢（名前と並び）が同じである必要があります。「選択肢の設定」でそろえてください（現在："), paste(lx, collapse = tr_("／")), tr_(" と "), paste(ly, collapse = tr_("／")), tr_("）。"))
  if (need2 && length(lx) != 2) stop_user(tr_("マクネマー検定は 2 択どうしの比較です。「選択肢の設定」で選択肢を 2 つにまとめてください（現在 "), length(lx), tr_(" 個）。"))
  fx <- slot_factor(df, sx); fy <- slot_factor(df, sy); ok <- !is.na(fx) & !is.na(fy)
  list(sx = sx, sy = sy, fx = fx[ok], fy = fy[ok], lev = lx, n = sum(ok), px = lev_points(sx, lx), py = lev_points(sy, lx))
}
an_wsr <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  ps <- paired_setup(df, req); k <- length(ps$lev)
  if (ps$n < 1) stop_user(tr_("両方の質問に答えた人がいません。"))
  if (!identical(ps$px, ps$py)) stop_user(tr_("この検定では、2 つの質問の配点が同じである必要があります。「データ」の列の設定で配点をそろえてください。"))
  x <- ps$px[as.integer(ps$fx)]; y <- ps$py[as.integer(ps$fy)]; d <- x - y; toplab <- top_label(ps$lev, ps$px)
  nz <- sum(d != 0); if (nz == 0) stop_user(tr_("2 つの回答がすべての人で同じため、検定できません。"))
  wt <- wilcox_paired(x, y)
  p <- unname(wt$p.value); Z <- stats::qnorm(p / 2, lower.tail = FALSE); r_eff <- Z / sqrt(nz)
  tab <- table(ps$fx, ps$fy)
  res <- list(test = tr_("ウィルコクソンの符号付順位検定"), p = p, n = n_info(nrow(df), ps$n),
              xtab = xtab_obj(tab, ps$sx$label, ps$sy$label, resid = FALSE), fig = list(type = "xtab", prefer = "heat"))
  up <- sum(d > 0); dn <- sum(d < 0); eq <- sum(d == 0)
  res$grids <- list(list(kind = "grid", title = tr_("回答の変化"), corner = "", rows = A(c(paste0(ps$sx$label, tr_(" のほうが上位")), paste0(ps$sy$label, tr_(" のほうが上位")), tr_("同じ"))),
                         cols = A(c(tr_("人数"), tr_("割合 (%)"))), fmt = A(c("int", "pct")), cells = mat(cbind(c(up, dn, eq), c(up, dn, eq) / ps$n)),
                         note = trf("上位は%sの側。", q(toplab))))
  stats_txt <- paste0("V = ", fmt_num(wt$statistic, 1), ", |Z| = ", fmt_num(Z), ", ", fmt_p(p, lead0), ", r = ", fmt_num(r_eff))
  res$headline <- list(hl("V", fmt_num(wt$statistic, 1)), hl("|Z|", fmt_num(Z)), hl("p", fmt_p(p, lead0, FALSE)), hl("r", fmt_num(r_eff)), hl("n", as.character(ps$n)))
  res$row <- list(stat = paste0("V = ", fmt_num(wt$statistic, 1)), effect = paste0("r = ", fmt_num(r_eff)))
  sig <- p < alpha
  res$notes <- tr_("p 値は正規近似によります。|Z| は p 値から求めた値、効果量 r は |Z| / √（差が 0 でない人数）です。")
  res$report <- paste0(trf("同じ回答者の%sと%sの回答を比較した（n = %d）。", q(ps$sx$label), q(ps$sy$label), ps$n),
                       trf("ウィルコクソンの符号付順位検定の結果、2 つの回答には%s（%s）。", if (sig) tr_("統計的に有意な差が認められた") else tr_("統計的に有意な差は認められなかった"), stats_txt),
                       trf("%sのほうが%sの側の回答だった人は %d 人、%sのほうがそうだった人は %d 人、同じ回答の人は %d 人であった。", q(ps$sx$label), q(toplab), up, q(ps$sy$label), dn, eq))
  finish(res, alpha)
}
register("wsr", tr_("ウィルコクソンの符号付順位検定"), tr_("同じ人の 2 つの回答"), tr_("同じ選択肢で答える 2 つの質問（例：2 つのサービスの利用頻度）を、同じ人の中で比べます。"),
         list(S("x", tr_("回答 1（順序あり）"), "ordinal", "", TRUE), S("y", tr_("回答 2（同じ選択肢）"), "ordinal", "", TRUE)), list(), an_wsr)

an_mcnemar <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  ps <- paired_setup(df, req, need2 = TRUE)
  if (ps$n < 1) stop_user(tr_("両方の質問に答えた人がいません。"))
  tab <- table(ps$fx, ps$fy); b <- tab[1, 2]; c <- tab[2, 1]
  if (b + c == 0) stop_user(tr_("2 つの回答がすべての人で同じため、検定できません。"))
  exact <- (b + c) < 25
  if (exact) { p <- stats::binom.test(b, b + c, 0.5)$p.value; stat <- paste0(tr_("不一致 "), b, tr_(" 対 "), c); tname <- tr_("マクネマー検定（正確二項検定）") }
  else { mt <- stats::mcnemar.test(tab, correct = TRUE); p <- unname(mt$p.value); stat <- paste0("χ²(1) = ", fmt_num(mt$statistic)); tname <- tr_("マクネマー検定") }
  px <- mean(as.integer(ps$fx) == 1); py <- mean(as.integer(ps$fy) == 1)
  res <- list(test = tname, p = p, n = n_info(nrow(df), ps$n), xtab = xtab_obj(tab, ps$sx$label, ps$sy$label, resid = FALSE), fig = list(type = "xtab", prefer = "heat"))
  res$headline <- list(hl(if (exact) tr_("不一致の組") else "χ²(1)", if (exact) paste0(b, tr_(" 対 "), c) else fmt_num(mt$statistic)), hl("p", fmt_p(p, lead0, FALSE)), hl("n", as.character(ps$n)))
  res$row <- list(stat = stat, effect = "")
  res$notes <- if (exact) tr_("回答が食い違った人が 25 人未満のため、正確二項検定を使いました。") else tr_("連続性の補正を行っています。")
  sig <- p < alpha
  res$report <- paste0(trf("同じ回答者について、%sと%sで%sと答えた割合を比較した（n = %d）。", q(ps$sx$label), q(ps$sy$label), q(ps$lev[1]), ps$n),
                       trf("割合はそれぞれ %s%% と %s%% で、%sの結果、その差は%s（%s, %s）。", fmt_pct(px), fmt_pct(py), lc_(tname),
                           if (sig) tr_("統計的に有意であった") else tr_("統計的に有意ではなかった"), stat, fmt_p(p, lead0)))
  finish(res, alpha)
}
register("mcnemar", tr_("マクネマー検定"), tr_("同じ人の 2 つの回答"), tr_("同じ 2 択で答える 2 つの質問を、同じ人の中で比べます（例：A は「はい」だが B は「いいえ」の人が多いか）。"),
         list(S("x", tr_("回答 1（2 択）"), cat_types, "", TRUE), S("y", tr_("回答 2（同じ 2 択）"), cat_types, "", TRUE)), list(), an_mcnemar)

# ---------------------------------------------------------------- 割合の傾向検定
an_trend <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  sb <- req$slots$b; so <- req$slots$o
  lb <- unique(slot_labels(sb))
  if (length(lb) != 2) stop_user(tr_("傾向検定では、"), q(sb$label), tr_("を 2 つの選択肢にする必要があります。「選択肢の設定」でまとめてください（現在 "), length(lb), tr_(" 個）。"))
  mt <- make_table(slot_factor(df, so), slot_factor(df, sb)); tab <- mt$tab
  if (nrow(tab) < 3) stop_user(tr_("傾向検定には、"), q(so$label), tr_("に回答のある段階が 3 つ以上必要です（現在 "), nrow(tab), tr_(" 段階）。2 段階のときはカイ二乗検定を使ってください。"))
  if (ncol(tab) < 2) stop_user(q(sb$label), tr_("の回答が 1 種類しかないため、検定できません。"))
  x <- tab[, 1]; nn <- rowSums(tab); pr <- x / nn
  sc <- if (is.null(so[["points"]])) seq_len(nrow(tab)) else lev_points(so, rownames(tab))     # 段階の得点（配点を選んだときはその点）
  pt <- stats::prop.trend.test(x, nn, score = sc)
  res <- list(test = tr_("コクラン・アーミテージの傾向検定"), p = unname(pt$p.value), n = n_info(nrow(df), mt$n),
              xtab = xtab_obj(tab, so$label, sb$label, resid = FALSE), fig = list(type = "xtab"))
  stat <- paste0("χ²(1) = ", fmt_num(pt$statistic))
  res$headline <- list(hl("χ²(1)", fmt_num(pt$statistic)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("n", as.character(mt$n)))
  res$row <- list(stat = stat, effect = "")
  sig <- res$p < alpha
  res$notes <- if (is.null(so[["points"]])) tr_("段階には並びの順に 1, 2, 3, … の等間隔の得点を与えています。") else trf("段階には、選んだ配点（%s）を得点として与えています。", points_text(rownames(tab), sc))
  res$report <- paste0(trf("%sの段階ごとに、%sで%sと答えた割合を求めた（n = %d）。", q(so$label), q(sb$label), q(colnames(tab)[1]), mt$n),
                       trf("割合は%sであった。", paste0(rownames(tab), " ", fmt_pct(pr), "%", collapse = tr_("、"))),
                       trf("コクラン・アーミテージの傾向検定の結果、段階にそった割合の直線的な増減は%s（%s, %s）。", if (sig) tr_("統計的に有意であった") else tr_("統計的に有意ではなかった"), stat, fmt_p(res$p, lead0)))
  finish(res, alpha)
}
register("trend", tr_("割合の傾向検定（コクラン・アーミテージ）"), tr_("2 つの質問の関連"), tr_("段階が上がるほど「はい」の割合が増える（減る）という一方向の傾向があるかを調べます。"),
         list(S("b", tr_("2 択の回答"), cat_types, tr_("例：ある・ない"), TRUE), S("o", tr_("段階（順序あり）"), "ordinal", tr_("例：学年、利用頻度"), TRUE)), list(), an_trend)

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
  if (!any(ok)) stop_user(tr_("3 つの質問すべてに答えた人がいません。列の組み合わせや絞り込みを見直してください。"))
  a <- droplevels(a[ok]); b <- droplevels(b[ok]); z <- droplevels(z[ok])
  if (nlevels(a) < 2 || nlevels(b) < 2) stop_user(tr_("行と列のそれぞれに、回答のある選択肢が 2 つ以上必要です。選択肢の設定や絞り込みを見直してください。"))
  if (nlevels(z) < 2) stop_user(tr_("層には、回答のある選択肢が 2 つ以上必要です。層を分けない場合は、カイ二乗検定を使ってください。"))
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
  if (is.null(mh)) stop_user(tr_("層ごとの人数が少なすぎて、コクラン・マンテル・ヘンツェル検定を計算できません。層や選択肢をまとめてください。"))
  is22 <- nlevels(a) == 2 && nlevels(b) == 2
  stat <- paste0("CMH χ²(", mh$parameter, ") = ", fmt_num(mh$statistic))
  res <- list(test = tr_("コクラン・マンテル・ヘンツェル検定"), p = unname(mh$p.value), n = n_info(nrow(df), n), fig = list(type = "xtab"), notes = character(0))
  res$headline <- list(hl(paste0("CMH χ²(", mh$parameter, ")"), fmt_num(mh$statistic)), hl("p", fmt_p(res$p, lead0, FALSE)), hl(tr_("層"), as.character(sum(keepz))), hl("n", as.character(n)))
  res$row <- list(stat = stat, effect = "–")
  # 図と表：行を「層・行の選択肢」にしたクロス集計表
  big <- do.call(rbind, lapply(seq_along(lay), function(k) { m <- matrix(t3[, , k], nrow = nlevels(a)); rownames(m) <- paste0(lay[k], tr_("・"), levels(a)); m }))
  colnames(big) <- levels(b); big <- big[rowSums(big) > 0, , drop = FALSE]
  res$xtab <- xtab_obj(as.table(big), paste0(sz$label, if (identical(LANG, "en")) " × " else "・", sa$label), sb$label, resid = FALSE)
  res$grids <- list(list(kind = "grid", title = tr_("層ごとのカイ二乗検定"), corner = sz$label, rows = A(c(lay, tr_("全体（層を分けない）"))),
                         cols = A(c("n", "χ²", tr_("自由度"), "p", "Cramér's V", tr_("期待度数 5 未満 (%)"))), fmt = A(c("int", "num2", "int", "p", "num2", "num1")),
                         cells = lapply(rows, function(r) lapply(r, function(x) if (is.na(x)) NULL else x)),
                         note = tr_("層ごとの検定はイェーツの補正なしのカイ二乗検定です。層の数だけ検定を繰り返すので、層ごとの p 値は参考として扱ってください。")))
  extra <- ""
  if (is22) {
    or <- unname(mh$estimate); ci <- mh$conf.int; bd <- breslow_day(t3c, or)
    res$headline <- append(res$headline, list(hl(tr_("共通オッズ比"), paste0(fmt_num(or), " [", fmt_num(ci[1]), ", ", fmt_num(ci[2]), "]"))), 2)
    res$row$effect <- paste0(tr_("共通オッズ比 = "), fmt_num(or))
    orl <- vapply(dimnames(t3c)[[3]], function(k) { tk <- t3c[, , k]; (tk[1, 1] + .5) * (tk[2, 2] + .5) / ((tk[1, 2] + .5) * (tk[2, 1] + .5)) }, 0)
    res$grids <- c(res$grids, list(list(kind = "grid", title = tr_("オッズ比"), corner = "", rows = A(c(names(orl), tr_("共通（マンテル・ヘンツェル）"))),
                                        cols = A(c(tr_("オッズ比"), tr_("95% 信頼区間の下限"), tr_("上限"))), fmt = A(c("num2", "num2", "num2")),
                                        cells = c(lapply(orl, function(x) list(x, NULL, NULL)), list(list(or, ci[1], ci[2]))),
                                        note = trf("オッズ比は、%sが%sに比べて%sと答えるオッズの比です。層ごとの値は、0 のセルがあっても計算できるよう各セルに 0.5 を足した値です。", q(levels(a)[1]), q(levels(a)[2]), q(levels(b)[1]))),
                                   list(kind = "grid", title = tr_("オッズ比の均一性（ブレスロー・デイ検定、タロンの補正）"), corner = "", rows = A(tr_("層の間の違い")), cols = A(c("χ²", tr_("自由度"), "p")),
                                        fmt = A(c("num2", "int", "p")), cells = list(list(bd$stat, bd$df, bd$p)))))
    extra <- paste0(trf("層の影響を調整した共通オッズ比は %s（95%%CI [%s, %s]）であった。", fmt_num(or), fmt_num(ci[1]), fmt_num(ci[2])),
                    if (bd$p < alpha) trf("ブレスロー・デイ検定では、層の間でオッズ比が異なっていた（χ²(%s) = %s, %s）。層によって関連の強さが違うため、共通オッズ比より層ごとの結果を重視して解釈する必要がある。", bd$df, fmt_num(bd$stat), fmt_p(bd$p, lead0))
                    else trf("ブレスロー・デイ検定では、層の間でオッズ比が異なるとはいえなかった（χ²(%s) = %s, %s）。", bd$df, fmt_num(bd$stat), fmt_p(bd$p, lead0)))
    res$notes <- c(res$notes, tr_("2×2 の表の CMH 検定は、連続性の補正なしで計算しています。"))
  } else res$notes <- c(res$notes, tr_("行または列の選択肢が 3 つ以上なので、一般化した CMH 検定（一般の関連）で計算しています。共通オッズ比は 2×2 の表のときだけ出します。"))
  if (length(dropped)) res$notes <- c(res$notes, trf("層「%s」は、回答のある選択肢が 1 つしかないため、層ごとの検定ができませんでした。", paste(dropped, collapse = tr_("」「"))))
  if (any(!keepz)) res$notes <- c(res$notes, trf("回答が 1 人以下の層（%s）は CMH 検定から除きました。", paste(lay[!keepz], collapse = tr_("、"))))
  sig <- res$p < alpha
  res$report <- paste0(trf("%sで層に分け（%d 層）、%sと%sの関連を調べた（有効回答 n = %d）。", q(sz$label), sum(keepz), q(sa$label), q(sb$label), n),
                       trf("層を分けないカイ二乗検定では%s（χ²(%s) = %s, %s）、層ごとの検定で有意であった層は%s", if (ctt$p.value < alpha) tr_("有意な関連が認められ") else tr_("有意な関連は認められず"),
                           ctt$parameter, fmt_num(ctt$statistic), fmt_p(ctt$p.value, lead0), if (length(sigl)) trf("%sであった。", paste(sigl, collapse = tr_("、"))) else tr_("なかった。")),
                       trf("コクラン・マンテル・ヘンツェル検定の結果、%sの影響を調整しても両者の関連は%s（%s, %s）。", q(sz$label), if (sig) tr_("統計的に有意であった") else tr_("統計的に有意ではなかった"), stat, fmt_p(res$p, lead0)), extra)
  finish(res, alpha)
}
register("strat", tr_("層別のクロス集計（CMH 検定）"), tr_("2 つの質問の関連"), tr_("3 つ目の質問（例：学年）で層に分けてクロス集計し、層の影響を調整しても 2 つの質問に関連があるかを調べます（コクラン・マンテル・ヘンツェル検定）。"),
         list(S("a", tr_("行（比べるグループ）"), cat_types, tr_("例：性別")), S("b", tr_("列（回答）"), cat_types, tr_("例：ある・ない")), S("z", tr_("層（分けて見る質問）"), cat_types, tr_("例：学年、部活動"))),
         list(), an_strat)

registry_json <- function() {
  # 名前・説明・欄の名前は登録したとき（日本語）のまま持ち、要求の言語（LANG）に合わせてここで訳す
  tl <- function(x) if (is.character(x) && length(x) == 1 && nzchar(x)) tr_(x) else x
  lapply(ANALYSES, function(a) {
    a <- a[c("id", "name", "group", "desc", "slots", "opts")]
    a$name <- tl(a$name); a$group <- tl(a$group); a$desc <- tl(a$desc)
    a$slots <- lapply(a$slots, function(s) { s$label <- tl(s$label); s$hint <- tl(s$hint); s })
    a$opts <- lapply(a$opts, function(o) {
      o$label <- tl(o$label); o$hint <- tl(o$hint)
      if (!is.null(o$choices)) o$choices <- lapply(o$choices, function(ch) { ch$label <- tl(ch$label); ch })
      o
    })
    a
  })
}

run_analysis <- function(df, req) {
  a <- ANALYSES[[req$type]]
  if (is.null(a)) stop_user(tr_("この解析の種類（"), req$type, tr_("）は登録されていません。"))
  # 同じ設問を 2 つの欄に選んでいるときは、計算せずに知らせる
  cols <- vapply(req$slots, function(s) if (is.null(s$col) || isTRUE(s$many)) NA_integer_ else as.integer(s$col), 0L)
  dup <- which(duplicated(cols) & !is.na(cols))
  if (length(dup)) {
    s <- req$slots[[dup[1]]]; nm <- if (is.character(s[["full"]]) && nzchar(s[["full"]])) s[["full"]] else s[["label"]]
    stop_user(tr_("同じ設問（"), q(nm), tr_("）が 2 つの欄で選ばれています。2 つの設問の関係を調べる解析なので、別の設問を選んでください。"))
  }
  keep <- apply_filter(df, req$filter)
  d <- df[keep, , drop = FALSE]
  if (nrow(d) == 0) stop_user(tr_("絞り込みの条件に合う回答がありません。絞り込みを見直してください。"))
  options(cts.alpha = opt(req, "alpha", 0.05))
  res <- a$fun(d, req)
  res$type <- req$type
  # 報告書の文では、設問を省略しない名前（full）で書く。表示名を短く書き換えていても、文は元の質問に沿う
  sl <- req$slots; if (!is.null(req$filter) && length(req$filter$values)) sl <- c(sl, list(req$filter))
  sl <- Filter(function(s) !isTRUE(s[["many"]]) && is.character(s[["full"]]) && length(s[["full"]]) == 1 && nzchar(s[["full"]]) && is.character(s[["label"]]) && s[["full"]] != s[["label"]], sl)
  if (length(sl)) for (s in sl[order(-nchar(vapply(sl, function(s) s[["label"]], "")))]) res$report <- gsub(q(s[["label"]]), q(s[["full"]]), res$report, fixed = TRUE)
  # 利用者が名前を付けた列（qtext がある列）は、文でもその名前を使い、最初に出たところに 1 度だけ質問文を添える
  ql <- Filter(function(s) is.character(s[["qtext"]]) && length(s[["qtext"]]) == 1 && nzchar(s[["qtext"]]) && is.character(s[["label"]]), c(req$slots, if (!is.null(req$filter)) list(req$filter), unlist(lapply(req$slots, function(s) if (isTRUE(s[["many"]])) s[["vars"]]), recursive = FALSE)))
  for (s in ql) { k <- q(s[["label"]]); if (grepl(k, res$report, fixed = TRUE)) res$report <- sub(k, paste0(k, trf("（質問文：%s）", s[["qtext"]])), res$report, fixed = TRUE) }
  if (!is.null(req$filter) && length(req$filter$values)) {
    fl <- if (is.character(req$filter[["full"]]) && nzchar(req$filter[["full"]])) req$filter[["full"]] else req$filter$label
    res$filterText <- trf("%sが%sの回答者（%d人）に限って集計した。", q(fl), paste0(q(unlist(req$filter$values)), collapse = if (identical(LANG, "en")) " or " else "・"), nrow(d))
    res$report <- paste0(res$filterText, res$report)
  }
  res
}
