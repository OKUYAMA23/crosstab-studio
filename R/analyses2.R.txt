# analyses2.R ― 数値・得点を扱う解析と、多変量の解析（0.7.0 で追加）。
#   数値の要約とヒストグラム／t 検定／一元配置分散分析／対応のある t 検定／ピアソンの相関／
#   尺度の信頼性（クロンバックの α）／探索的因子分析／コレスポンデンス分析／回答日時の推移

num_types <- c("numeric", "ordinal")
score_note <- function(s) if (identical(s$type, "ordinal")) paste0(q(s$label), "は、選択肢に", q(unique(slot_labels(s))[1]), "の側から高い得点（", length(unique(slot_labels(s))), "〜1 点）を付けて計算した。") else NULL
clean_note <- function(df, s) {
  if (!identical(s$type, "numeric")) return(NULL)
  st <- clean_number(df[[s$col]], s$unit)$status; v <- slot_score(df, s)
  nf <- sum(st %in% c("fixed", "range") & !is.na(v)); nx <- sum(st == "fail" & is.na(v))
  if (nf + nx == 0) return(NULL)
  paste0(q(s$label), "は、", if (nf) paste0("単位や全角文字などを直して数値にした回答 ", nf, " 件を含む") else "", if (nf && nx) "。" else "",
         if (nx) paste0("数値として読めない回答 ", nx, " 件は除外した") else "", "。")
}
desc8 <- function(v) c(length(v), mean(v), if (length(v) > 1) stats::sd(v) else NA, min(v), unname(stats::quantile(v, .25)), stats::median(v), unname(stats::quantile(v, .75)), max(v))
DESC_COLS <- c("n", "平均", "標準偏差", "最小", "第1四分位", "中央値", "第3四分位", "最大")
# 箱ひげ図の 5 数と外れ値に加えて、平均と 95% 信頼区間（t 分布）も持たせる（「平均と信頼区間」の図に使う）
box_of <- function(v) {
  b <- grDevices::boxplot.stats(v); n <- length(v); m <- mean(v)
  h <- if (n > 1) stats::qt(0.975, n - 1) * stats::sd(v) / sqrt(n) else NA_real_
  list(stats = A(b$stats), out = A(utils::head(b$out, 300)), n = n, mean = m, lo = if (is.na(h)) NULL else m - h, hi = if (is.na(h)) NULL else m + h)
}
fmt_ci <- function(ci, d = 2) paste0("[", fmt_num(ci[1], d), ", ", fmt_num(ci[2], d), "]")

# ---------------------------------------------------------------- 数値の要約とヒストグラム
hist_breaks <- function(y, how) {
  rng <- range(y)
  if (rng[1] == rng[2]) return(c(rng[1] - 0.5, rng[1] + 0.5))
  isint <- all(abs(y - round(y)) < 1e-9)
  if ((how == "auto" && isint && diff(rng) <= 30) || how == "int") return(seq(floor(rng[1]) - 0.5, ceiling(rng[2]) + 0.5, by = 1))
  k <- switch(how, fd = grDevices::nclass.FD(y), grDevices::nclass.Sturges(y))
  br <- pretty(rng, n = max(1, k))
  if (max(br) <= rng[2]) br <- c(br, max(br) + diff(br[1:2]))
  br
}
bin_labels <- function(br, intbins) {
  k <- length(br) - 1
  if (intbins) return(vapply(seq_len(k), function(i) fmt_num(br[i] + 0.5, 0), ""))
  d <- max(0, min(3, -floor(log10(diff(br[1:2]))) + 1))
  vapply(seq_len(k), function(i) paste0(fmt_num(br[i], d), " 以上 ", fmt_num(br[i + 1], d), if (i == k) " 以下" else " 未満"), "")
}
an_numsum <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); sy <- req$slots$y; sg <- req$slots$g
  y <- slot_score(df, sy); g <- if (!is.null(sg)) slot_factor(df, sg) else NULL
  ok <- !is.na(y) & (if (is.null(g)) TRUE else !is.na(g))
  y <- y[ok]; if (!is.null(g)) g <- droplevels(g[ok])
  n <- length(y); if (n < 2) stop_user(q(sy$label), "で数値として読める回答が 2 件未満です。")
  groups <- if (is.null(g)) list(全体 = y) else c(split(y, g), list(全体 = y))
  M <- t(vapply(groups, desc8, numeric(8)))
  res <- list(test = "数値の要約とヒストグラム", n = n_info(nrow(df), n))
  how <- opt(req, "bins", "auto"); br <- hist_breaks(y, how); intbins <- all(abs(diff(br) - 1) < 1e-9) && all(abs(br %% 1 - 0.5) < 1e-9)
  ser <- if (is.null(g)) "度数" else levels(g)
  cnt <- vapply(if (is.null(g)) list(y) else split(y, g), function(v) as.integer(table(cut(v, br, right = FALSE, include.lowest = TRUE))), integer(length(br) - 1))
  cnt <- matrix(cnt, ncol = length(ser)); labs <- bin_labels(br, intbins)
  res$grids <- list(
    list(kind = "grid", title = "要約統計量", corner = if (is.null(g)) sy$label else sg$label, rows = A(names(groups)), cols = A(DESC_COLS),
         fmt = A(c("int", rep("num2", 7))), cells = mat(M), note = "標準偏差は n − 1 で割った値（不偏標準偏差）。四分位は R の既定（type 7）で求めた。"),
    list(kind = "grid", title = "度数分布", corner = if (intbins) sy$label else paste0(sy$label, "（階級）"), rows = A(labs),
         cols = A(if (is.null(g)) c("度数", "割合 (%)") else ser), fmt = A(if (is.null(g)) c("int", "pct") else rep("int", length(ser))),
         cells = mat(if (is.null(g)) cbind(cnt[, 1], cnt[, 1] / n) else cnt), note = if (intbins) "値ごとの度数。" else "階級は「以上・未満」で区切った（最後の階級だけ上端を含む）。"))
  res$fig <- list(type = "hist", edges = A(br), counts = mat(cnt), series = A(ser), xTitle = sy$label, seriesTitle = if (is.null(g)) "" else sg$label, intBins = intbins,
                  boxCats = A(if (is.null(g)) sy$label else levels(g)), box = unname(lapply(if (is.null(g)) list(y) else split(y, g), box_of)), catTitle = if (is.null(g)) "" else sg$label)
  tot <- M[nrow(M), ]
  res$headline <- list(hl("n", as.character(n)), hl("平均", fmt_num(tot[2])), hl("標準偏差", fmt_num(tot[3])), hl("中央値", fmt_num(tot[6])))
  res$row <- list(stat = "", effect = "")
  txt <- paste0(q(sy$label), "（n = ", n, "）の平均は ", fmt_num(tot[2]), "（SD = ", fmt_num(tot[3]), "）、中央値は ", fmt_num(tot[6]),
                "（第1四分位 ", fmt_num(tot[5]), "、第3四分位 ", fmt_num(tot[7]), "）、最小値は ", fmt_num(tot[4]), "、最大値は ", fmt_num(tot[8]), "であった。")
  if (!is.null(g)) txt <- paste0(txt, q(sg$label), "別の平均は、", paste0(levels(g), " ", fmt_num(M[seq_len(nlevels(g)), 2]), "（SD = ", fmt_num(M[seq_len(nlevels(g)), 3]), "、n = ", M[seq_len(nlevels(g)), 1], "）", collapse = "、"), "であった。")
  skew <- if (n > 2 && tot[3] > 0) mean((y - tot[2])^3) / tot[3]^3 else 0
  if (abs(skew) >= 1) txt <- paste0(txt, "分布は", if (skew > 0) "右" else "左", "に裾の長い形（歪度 ", fmt_num(skew), "）で、平均は外れた値の影響を受けやすいため、中央値も併せて示す。")
  res$report <- paste0(txt, paste(c(clean_note(df, sy), score_note(sy)), collapse = ""))
  res$notes <- c(clean_note(df, sy), "図は「ヒストグラム」と「箱ひげ図」を切り替えられます。")
  finish(res, alpha)
}
register("numsum", "数値の要約とヒストグラム", "集計", "数字で答える質問（年齢・時間・回数など）の平均・標準偏差・中央値と度数分布を出し、ヒストグラムで示します。グループ別にも比べられます。",
         list(S("y", "数値の質問", c("numeric", "ordinal"), "例：年齢、1 日の勉強時間"), S("g", "グループ（任意）", cat_types, "選ぶとグループ別に要約します", TRUE, optional = TRUE)),
         list(O_select("bins", "ヒストグラムの区切り方", list(auto = "自動（整数で範囲が狭ければ値ごと）", sturges = "スタージェスの公式", fd = "フリードマン＝ダイアコニスの方法", int = "値（整数）ごと"), "auto",
                       "棒の数の決め方です。データが多いときは、フリードマン＝ダイアコニスの方法のほうが細かく区切ります。")),
         an_numsum)

# ---------------------------------------------------------------- 平均の比較の準備（グループ × 数値）
mean_setup <- function(df, req) {
  sg <- req$slots$g; sy <- req$slots$y
  g <- slot_factor(df, sg); y <- slot_score(df, sy); ok <- !is.na(g) & !is.na(y)
  g <- droplevels(g[ok]); y <- y[ok]
  sm <- t(vapply(levels(g), function(l) { v <- y[g == l]; c(length(v), mean(v), if (length(v) > 1) stats::sd(v) else NA, stats::median(v)) }, numeric(4)))
  list(sg = sg, sy = sy, g = g, y = y, n = length(y), sm = sm,
       grid = list(kind = "grid", title = "群ごとの平均", corner = sg$label, rows = A(levels(g)), cols = A(c("n", "平均", "標準偏差", "中央値")), fmt = A(c("int", "num2", "num2", "num2")), cells = mat(sm)),
       fig = list(type = "box", cats = A(levels(g)), box = unname(lapply(split(y, g), box_of)), catTitle = sg$label, valueTitle = sy$label, means = A(sm[, 2]), prefer = "mean"))
}
grp_txt <- function(ms) paste0(rownames(ms$sm), "：M = ", fmt_num(ms$sm[, 2]), ", SD = ", fmt_num(ms$sm[, 3]), ", n = ", ms$sm[, 1], collapse = "、")

# ---------------------------------------------------------------- t 検定（2 群）
an_ttest <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE); eqv <- isTRUE(opt(req, "student", FALSE))
  ms <- mean_setup(df, req); g <- ms$g; y <- ms$y
  if (nlevels(g) != 2) stop_user("t 検定は 2 つの群の比較です。", q(ms$sg$label), "は現在 ", nlevels(g), " 群です。「選択肢の設定」で 2 つにするか、一元配置分散分析を使ってください。")
  l <- levels(g); x1 <- y[g == l[1]]; x2 <- y[g == l[2]]; n1 <- length(x1); n2 <- length(x2)
  if (n1 < 2 || n2 < 2) stop_user("各群に 2 人以上の回答が必要です（", l[1], " ", n1, " 人、", l[2], " ", n2, " 人）。")
  if (stats::sd(c(x1, x2)) == 0) stop_user("すべての回答が同じ値のため、検定できません。")
  tt <- stats::t.test(x1, x2, var.equal = eqv)
  sp <- sqrt(((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / (n1 + n2 - 2))
  d <- (mean(x1) - mean(x2)) / sp; J <- 1 - 3 / (4 * (n1 + n2) - 9); hg <- d * J
  nm <- if (eqv) "スチューデントの t 検定" else "ウェルチの t 検定"
  res <- list(test = nm, p = unname(tt$p.value), n = n_info(nrow(df), n1 + n2), grids = list(ms$grid), fig = ms$fig)
  dfv <- unname(tt$parameter); tv <- unname(tt$statistic); ci <- unname(tt$conf.int)
  res$chk <- list(t = tv, df = dfv, p = res$p, g = hg, lo = ci[1], hi = ci[2])     # Excel の数式と照合するための値
  st <- paste0("t(", fmt_num(dfv, if (eqv) 0 else 2), ") = ", fmt_num(tv), ", ", fmt_p(res$p, lead0), ", g = ", fmt_num(hg))
  res$headline <- list(hl(paste0("t(", fmt_num(dfv, if (eqv) 0 else 2), ")"), fmt_num(tv)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("Hedges の g", fmt_num(hg)), hl("平均の差", fmt_num(mean(x1) - mean(x2))))
  res$row <- list(stat = paste0("t(", fmt_num(dfv, if (eqv) 0 else 2), ") = ", fmt_num(tv)), effect = paste0("g = ", fmt_num(hg)))
  res$grids <- c(res$grids, list(list(kind = "grid", title = "平均の差", corner = "", rows = A(paste0(l[1], " − ", l[2])), cols = A(c("平均の差", "95% 信頼区間の下限", "上限", "t", "自由度", "p", "Hedges の g")),
                                      fmt = A(c("num2", "num2", "num2", "num2", "num2", "p", "num2")), cells = mat(matrix(c(mean(x1) - mean(x2), ci, tv, dfv, res$p, hg), nrow = 1)))))
  sig <- res$p < alpha; hi <- if (mean(x1) >= mean(x2)) l[1] else l[2]; lo <- setdiff(l, hi)
  sz <- abs(hg); sz_w <- if (sz < 0.2) "ごく小さい" else if (sz < 0.5) "小さい" else if (sz < 0.8) "中程度の" else "大きい"
  res$report <- paste0(q(ms$sg$label), "の 2 群で", q(ms$sy$label), "の平均を比較した（", grp_txt(ms), "）。", nm, "の結果、平均の差は",
                       if (sig) "統計的に有意であった" else "統計的に有意ではなかった", "（", st, "、平均の差 ", fmt_num(mean(x1) - mean(x2)), "、95% CI ", fmt_ci(ci), "）。",
                       if (sig) paste0(hi, "のほうが", lo, "より平均が高く、効果量は", sz_w, "大きさであった。") else "", score_note(ms$sy), clean_note(df, ms$sy))
  res$notes <- c(if (!eqv) "ウェルチの t 検定は、2 群の分散が等しいことを仮定しない t 検定です。分散が等しいと仮定するときは、オプションでスチューデントの t 検定に切り替えられます。" else NULL,
                 "効果量 g は平均の差を併合標準偏差で割り、小標本の補正をした値です（|g| が 0.2：小、0.5：中、0.8：大 が目安）。",
                 if (identical(ms$sy$type, "ordinal")) "段階で答える回答の得点を平均しています。選択肢の間隔が等しいと見なせないときは、マン・ホイットニーの U 検定が適しています。" else NULL)
  finish(res, alpha)
}
register("ttest", "t 検定（2 群の平均の比較）", "平均の比較", "2 つのグループで、数値（または段階の得点）の平均に差があるかを調べます。効果量と平均の差の信頼区間も出します。",
         list(S("g", "グループ（2 群）", cat_types, "例：性別", TRUE), S("y", "比べる値", num_types, "数値、または段階で答える質問", TRUE)),
         list(O_check("student", "分散が等しいと仮定する（スチューデントの t 検定）", FALSE, "通常は、分散が等しいことを仮定しないウェルチの t 検定（既定）を使います。")), an_ttest)

# ---------------------------------------------------------------- 一元配置分散分析（3 群以上）
games_howell <- function(y, g) {
  l <- levels(g); k <- length(l); m <- tapply(y, g, mean); v <- tapply(y, g, stats::var); n <- tapply(y, g, length)
  pr <- utils::combn(k, 2); out <- matrix(NA, ncol(pr), 4)
  for (c in seq_len(ncol(pr))) {
    i <- pr[1, c]; j <- pr[2, c]; se2 <- v[i] / n[i] + v[j] / n[j]
    tv <- (m[i] - m[j]) / sqrt(se2); dfv <- se2^2 / ((v[i] / n[i])^2 / (n[i] - 1) + (v[j] / n[j])^2 / (n[j] - 1))
    out[c, ] <- c(m[i] - m[j], abs(tv), dfv, stats::ptukey(abs(tv) * sqrt(2), k, dfv, lower.tail = FALSE))
  }
  list(names = paste0(l[pr[1, ]], " − ", l[pr[2, ]]), M = out)
}
an_anova <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE); eqv <- isTRUE(opt(req, "classic", FALSE))
  ms <- mean_setup(df, req); g <- ms$g; y <- ms$y; k <- nlevels(g)
  if (k < 3) stop_user("一元配置分散分析は 3 つ以上の群の比較です。2 群のときは t 検定を使ってください。")
  if (any(ms$sm[, 1] < 2)) stop_user("回答が 1 人以下の群があります（", paste(rownames(ms$sm)[ms$sm[, 1] < 2], collapse = "、"), "）。「選択肢の設定」でまとめるか外してください。")
  if (stats::sd(y) == 0) stop_user("すべての回答が同じ値のため、検定できません。")
  ow <- stats::oneway.test(y ~ g, var.equal = eqv)
  ssb <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2); sst <- sum((y - mean(y))^2); eta2 <- ssb / sst
  nm <- if (eqv) "一元配置分散分析" else "ウェルチの一元配置分散分析"
  res <- list(test = nm, p = unname(ow$p.value), n = n_info(nrow(df), length(y)), grids = list(ms$grid), fig = ms$fig)
  F <- unname(ow$statistic); d1 <- unname(ow$parameter[1]); d2 <- unname(ow$parameter[2])
  res$chk <- list(F = F, d1 = d1, d2 = d2, eta2 = eta2, p = res$p)
  dtx <- paste0("F(", fmt_num(d1, 0), ", ", fmt_num(d2, if (eqv) 0 else 2), ")")
  st <- paste0(dtx, " = ", fmt_num(F), ", ", fmt_p(res$p, lead0), ", η² = ", fmt_num(eta2))
  res$headline <- list(hl(dtx, fmt_num(F)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("η²", fmt_num(eta2)), hl("n", as.character(length(y))))
  res$row <- list(stat = paste0(dtx, " = ", fmt_num(F)), effect = paste0("η² = ", fmt_num(eta2)))
  if (eqv) {
    th <- stats::TukeyHSD(stats::aov(y ~ g))$g; pn <- rownames(th)
    post <- list(names = vapply(strsplit(pn, "-", fixed = TRUE), function(a) paste(rev(a), collapse = " − "), ""), M = cbind(-th[, 1], NA, NA, th[, 4]))
    ptitle <- "多重比較（テューキーの方法）"
  } else { post <- games_howell(y, g); ptitle <- "多重比較（ゲームス・ハウエルの方法）" }
  res$grids <- c(res$grids, list(list(kind = "grid", title = ptitle, corner = "組み合わせ", rows = A(post$names), cols = A(c("平均の差", "調整後の p")), fmt = A(c("num2", "p")),
                                      cells = mat(post$M[, c(1, 4), drop = FALSE]), mark = lapply(post$M[, 4] < alpha, function(b) A(c(FALSE, b))))))
  sig <- res$p < alpha; sigp <- post$names[post$M[, 4] < alpha]
  o <- order(-ms$sm[, 2])
  res$report <- paste0(q(ms$sg$label), "の ", k, " 群で", q(ms$sy$label), "の平均を比較した（", grp_txt(ms), "）。", nm, "の結果、群の間の平均の差は",
                       if (sig) "統計的に有意であった" else "統計的に有意ではなかった", "（", st, "）。",
                       if (sig) paste0("平均は", paste(rownames(ms$sm)[o], collapse = "、"), "の順に高かった。", if (eqv) "テューキーの方法" else "ゲームス・ハウエルの方法", "による多重比較では、",
                                       if (length(sigp)) paste0(paste(sigp, collapse = "、"), "の間に有意な差が認められた。") else "どの組み合わせにも有意な差は認められなかった。") else "",
                       score_note(ms$sy), clean_note(df, ms$sy))
  res$notes <- c(if (!eqv) "ウェルチの一元配置分散分析は、群の分散が等しいことを仮定しません。多重比較も分散の違いを考えるゲームス・ハウエルの方法で行います。" else "分散が等しいと仮定する一元配置分散分析と、テューキーの方法による多重比較です。",
                 "η²（イータ二乗）は群の違いで説明される分散の割合です（0.01：小、0.06：中、0.14：大 が目安）。",
                 if (identical(ms$sy$type, "ordinal")) "段階で答える回答の得点を平均しています。選択肢の間隔が等しいと見なせないときは、クラスカル・ウォリス検定が適しています。" else NULL)
  finish(res, alpha)
}
register("anova", "一元配置分散分析（3 群以上の平均の比較）", "平均の比較", "3 つ以上のグループで、数値（または段階の得点）の平均に差があるかを調べ、どの組み合わせに差があるかも出します。",
         list(S("g", "グループ（3 群以上）", cat_types, "例：学年", TRUE), S("y", "比べる値", num_types, "数値、または段階で答える質問", TRUE)),
         list(O_check("classic", "分散が等しいと仮定する（通常の分散分析とテューキーの方法）", FALSE, "通常は、分散が等しいことを仮定しないウェルチの方法（既定）を使います。")), an_anova)

# ---------------------------------------------------------------- 対応のある t 検定
an_pairedt <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE); sx <- req$slots$x; sy <- req$slots$y
  x <- slot_score(df, sx); y <- slot_score(df, sy); ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 3) stop_user("両方に答えた人が 3 人以上必要です（現在 ", n, " 人）。")
  d <- x - y; if (stats::sd(d) == 0) stop_user("すべての人で差が同じ値のため、検定できません。")
  tt <- stats::t.test(x, y, paired = TRUE); dz <- mean(d) / stats::sd(d); ci <- unname(tt$conf.int)
  chk <- list(t = unname(tt$statistic), p = unname(tt$p.value), lo = ci[1], hi = ci[2], dz = dz)
  res <- list(test = "対応のある t 検定", p = unname(tt$p.value), n = n_info(nrow(df), n))
  sm <- rbind(c(n, mean(x), stats::sd(x)), c(n, mean(y), stats::sd(y)), c(n, mean(d), stats::sd(d)))
  res$grids <- list(list(kind = "grid", title = "平均と差", corner = "", rows = A(c(sx$label, sy$label, "差（1 − 2）")), cols = A(c("n", "平均", "標準偏差")), fmt = A(c("int", "num2", "num2")), cells = mat(sm),
                         note = paste0("両方に答えた ", n, " 人で計算した。")))
  res$fig <- list(type = "box", cats = A(c(sx$label, sy$label)), box = list(box_of(x), box_of(y)), catTitle = "", valueTitle = "値", means = A(c(mean(x), mean(y))), prefer = "mean")
  tv <- unname(tt$statistic); dfv <- unname(tt$parameter)
  st <- paste0("t(", dfv, ") = ", fmt_num(tv), ", ", fmt_p(res$p, lead0), ", dz = ", fmt_num(dz))
  res$headline <- list(hl(paste0("t(", dfv, ")"), fmt_num(tv)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("dz", fmt_num(dz)), hl("n", as.character(n)))
  res$row <- list(stat = paste0("t(", dfv, ") = ", fmt_num(tv)), effect = paste0("dz = ", fmt_num(dz)))
  sig <- res$p < alpha
  res$report <- paste0(q(sx$label), "（M = ", fmt_num(mean(x)), ", SD = ", fmt_num(stats::sd(x)), "）と", q(sy$label), "（M = ", fmt_num(mean(y)), ", SD = ", fmt_num(stats::sd(y)),
                       "）の平均を、両方に答えた ", n, " 人で比較した。対応のある t 検定の結果、平均の差は", if (sig) "統計的に有意であった" else "統計的に有意ではなかった",
                       "（", st, "、平均の差 ", fmt_num(mean(d)), "、95% CI ", fmt_ci(ci), "）。",
                       if (sig) paste0(if (mean(d) > 0) q(sx$label) else q(sy$label), "のほうが平均が高かった。") else "", score_note(sx), score_note(sy))
  res$notes <- c("同じ人が答えた 2 つの値の差の平均が 0 と異なるかを調べます。効果量 dz は差の平均を差の標準偏差で割った値です。",
                 if (identical(sx$type, "ordinal") || identical(sy$type, "ordinal")) "段階の得点を平均しています。間隔が等しいと見なせないときは、ウィルコクソンの符号付順位検定が適しています。" else NULL)
  res$chk <- chk
  finish(res, alpha)
}
register("pairedt", "対応のある t 検定", "平均の比較", "同じ人が答えた 2 つの値（事前と事後、2 つの項目の得点など）の平均に差があるかを調べます。",
         list(S("x", "値 1", num_types, "例：授業前の得点", TRUE), S("y", "値 2", num_types, "例：授業後の得点", TRUE)), list(), an_pairedt)

# ---------------------------------------------------------------- ピアソンの相関
an_pearson <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE); sx <- req$slots$x; sy <- req$slots$y
  x <- slot_score(df, sx); y <- slot_score(df, sy); ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 4) stop_user("相関を求めるには、両方に答えた人が 4 人以上必要です（現在 ", n, " 人）。")
  if (stats::sd(x) == 0 || stats::sd(y) == 0) stop_user("どちらかの回答がすべて同じ値のため、相関を求められません。")
  ct <- stats::cor.test(x, y, method = "pearson"); r <- unname(ct$estimate); ci <- unname(ct$conf.int)
  fit <- stats::lm(y ~ x); b <- unname(stats::coef(fit))
  chk <- list(r = r, p = unname(ct$p.value), lo = ci[1], hi = ci[2], b0 = b[1], b1 = b[2])
  res <- list(test = "ピアソンの相関係数", p = unname(ct$p.value), n = n_info(nrow(df), n))
  res$fig <- list(type = "scatter", x = A(x), y = A(y), xTitle = sx$label, yTitle = sy$label, fit = A(b))
  res$grids <- list(list(kind = "grid", title = "相関と回帰直線", corner = "", rows = A(paste0(sx$label, " と ", sy$label)), cols = A(c("n", "r", "95% 信頼区間の下限", "上限", "p", "切片", "傾き")),
                         fmt = A(c("int", "num2", "num2", "num2", "p", "num2", "num2")), cells = mat(matrix(c(n, r, ci, res$p, b), nrow = 1)),
                         note = paste0("回帰直線は ", sy$label, " = ", fmt_num(b[1]), " + ", fmt_num(b[2]), " × ", sx$label, "。")))
  a <- abs(r); strength <- if (a < 0.2) "ごく弱い" else if (a < 0.4) "弱い" else if (a < 0.7) "中程度の" else "強い"
  st <- paste0("r = ", fmt_num(r), ", 95% CI ", fmt_ci(ci), ", ", fmt_p(res$p, lead0), ", n = ", n)
  res$headline <- list(hl("r", fmt_num(r)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("n", as.character(n)))
  res$row <- list(stat = paste0("r = ", fmt_num(r)), effect = paste0("r = ", fmt_num(r)))
  sig <- res$p < alpha
  res$report <- paste0(q(sx$label), "と", q(sy$label), "の関連をピアソンの相関係数で調べた。その結果、",
                       if (sig) paste0(strength, if (r > 0) "正" else "負", "の相関が認められた") else "統計的に有意な相関は認められなかった", "（", st, "）。",
                       score_note(sx), score_note(sy), clean_note(df, sx), clean_note(df, sy))
  res$notes <- c("ピアソンの相関係数は直線的な関連の強さです。外れた値や、段階の数が少ない回答の影響を受けやすいため、そのときはスピアマンの順位相関も確かめてください。",
                 "相関は 2 つの値の関連を示すもので、一方が他方の原因であることを示すものではありません。")
  res$chk <- chk
  finish(res, alpha)
}
register("pearson", "ピアソンの相関係数", "関連・相関", "2 つの数値（または段階の得点）の直線的な関連の強さを調べ、散布図と回帰直線を描きます。",
         list(S("x", "値 1（横軸）", num_types, "", TRUE), S("y", "値 2（縦軸）", num_types, "", TRUE)), list(), an_pearson)

# ---------------------------------------------------------------- 尺度の信頼性・因子分析の準備（複数の段階の項目）
scale_matrix <- function(df, s, min_items) {
  it <- items_setup(df, s)
  if (it$k < min_items) stop_user("項目を ", min_items, " つ以上選んでください（現在 ", it$k, " 個）。")
  X <- sapply(it$F, function(f) length(it$lev) + 1L - as.integer(f)); colnames(X) <- it$names
  answered <- vapply(it$cols, function(j) sum(!is.na(df[[j]]) & df[[j]] != ""), 0)
  bad <- colSums(!is.na(X)) < 0.5 * pmax(1, answered)
  if (any(bad)) stop_user("1 つ目の項目と選択肢が違う項目があります（", paste(colnames(X)[bad], collapse = "、"), "）。同じ選択肢で答える項目だけを選んでください。")
  rev <- unlist(s$reverse); if (length(rev)) for (j in which(it$cols %in% rev)) X[, j] <- length(it$lev) + 1L - X[, j]
  ok <- stats::complete.cases(X); X <- X[ok, , drop = FALSE]
  if (nrow(X) < it$k + 2) stop_user("すべての項目に答えた人が少なすぎます（", nrow(X), " 人）。")
  sdz <- apply(X, 2, stats::sd); if (any(sdz == 0)) stop_user("全員が同じ回答をした項目があります（", paste(colnames(X)[sdz == 0], collapse = "、"), "）。その項目を外してください。")
  list(X = X, it = it, nrev = sum(it$cols %in% rev), revnames = colnames(X)[it$cols %in% rev])
}
cron_alpha <- function(X) { k <- ncol(X); k / (k - 1) * (1 - sum(apply(X, 2, stats::var)) / stats::var(rowSums(X))) }
an_alpha <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); sm <- scale_matrix(df, req$slots$items, 2); X <- sm$X; k <- ncol(X); n <- nrow(X)
  a <- cron_alpha(X); R <- stats::cor(X); rb <- mean(R[upper.tri(R)]); astd <- k * rb / (1 + (k - 1) * rb)
  tot <- rowSums(X)
  itc <- vapply(seq_len(k), function(j) stats::cor(X[, j], tot - X[, j]), 0)
  adel <- if (k > 2) vapply(seq_len(k), function(j) cron_alpha(X[, -j, drop = FALSE]), 0) else rep(NA, k)
  chk <- list(alpha = a, astd = astd, itc = itc, adel = adel)
  res <- list(test = "尺度の信頼性（クロンバックの α）", n = n_info(nrow(df), n))
  res$grids <- list(list(kind = "grid", title = "項目ごとの統計量", corner = "項目", rows = A(colnames(X)), cols = A(c("平均", "標準偏差", "修正済み項目−合計相関", "その項目を除いたときの α")),
                         fmt = A(c("num2", "num2", "num2", "num2")), cells = mat(cbind(colMeans(X), apply(X, 2, stats::sd), itc, adel)),
                         mark = lapply(seq_len(k), function(j) A(c(FALSE, FALSE, itc[j] < 0.3, !is.na(adel[j]) && adel[j] > a))),
                         note = "修正済み項目−合計相関は、その項目と残りの項目の合計点との相関。0.3 未満、または除くと α が上がる項目を太字にした。"),
                    list(kind = "grid", title = "尺度全体", corner = "", rows = A("合計点"), cols = A(c("項目数", "n", "α", "標準化 α", "項目間相関の平均", "合計点の平均", "合計点の標準偏差")),
                         fmt = A(c("int", "int", "num2", "num2", "num2", "num2", "num2")), cells = mat(matrix(c(k, n, a, astd, rb, mean(tot), stats::sd(tot)), nrow = 1))))
  res$headline <- list(hl("α", fmt_num(a)), hl("項目", as.character(k)), hl("n", as.character(n)))
  res$row <- list(stat = paste0("α = ", fmt_num(a)), effect = "")
  lvl <- if (a >= 0.9) "非常に高い" else if (a >= 0.8) "高い" else if (a >= 0.7) "許容できる" else if (a >= 0.6) "やや低い" else "低い"
  low <- colnames(X)[itc < 0.3]; up <- colnames(X)[!is.na(adel) & adel > a]
  res$report <- paste0(k, " 項目（", paste(colnames(X), collapse = "、"), "）の内的一貫性をクロンバックの α 係数で調べた。すべての項目に答えた ", n, " 人で、α = ", fmt_num(a),
                       "（標準化 α = ", fmt_num(astd), "）であり、内的一貫性は", lvl, "水準であった。",
                       if (length(low)) paste0("修正済み項目−合計相関が 0.3 未満の項目は", paste(low, collapse = "、"), "であった。") else "",
                       if (length(up)) paste0(paste(up, collapse = "、"), "を除くと α が上がる。") else "",
                       if (sm$nrev) paste0(paste(sm$revnames, collapse = "、"), "は逆転項目として得点を反転した。") else "",
                       "得点は、選択肢に", q(sm$it$lev[1]), "の側から ", length(sm$it$lev), "〜1 点を付けた。")
  res$notes <- c("α の目安は 0.7 以上（0.8 以上なら高い）とされることが多いですが、項目数が多いほど高くなりやすい点に注意してください。",
                 "逆の向きの項目（逆転項目）は、作業ウィンドウの「逆転項目」で指定すると、得点を反転して計算します。")
  res$chk <- chk
  finish(res, alpha)
}
register("alpha", "尺度の信頼性（クロンバックの α）", "尺度・多変量", "同じ内容を測る複数の段階の質問（尺度）について、まとまりの良さ（内的一貫性）を α 係数で調べ、項目ごとの問題も示します。",
         list(S("items", "尺度の項目（同じ選択肢）", "ordinal", "例：海への関心の 8 項目", FALSE, TRUE)), list(), an_alpha)

# ---------------------------------------------------------------- 探索的因子分析
an_factor <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); sm <- scale_matrix(df, req$slots$items, 3); X <- sm$X; p <- ncol(X); n <- nrow(X)
  R <- stats::cor(X); ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  fmax <- max(1, max(which(vapply(1:p, function(f) ((p - f)^2 - (p + f)) / 2 >= 0, TRUE))))
  nf0 <- opt(req, "nf", "auto"); rot <- opt(req, "rotation", "promax")
  nf <- if (identical(nf0, "auto")) max(1, sum(ev > 1)) else as.integer(nf0)
  if (nf > fmax) { if (identical(nf0, "auto")) nf <- fmax else stop_user(p, " 項目で求められる因子は ", fmax, " 個までです。項目を増やすか、因子の数を減らしてください。") }
  fa <- tryCatch(stats::factanal(covmat = R, factors = nf, n.obs = n, rotation = if (nf > 1) rot else "none"),
                 error = function(e) stop_user("因子分析の計算が収束しませんでした（", conditionMessage(e), "）。因子の数を変えるか、項目を見直してください。"))
  L <- unclass(fa$loadings); L <- L[, seq_len(nf), drop = FALSE]
  for (j in seq_len(nf)) if (sum(L[, j]) < 0) L[, j] <- -L[, j]
  prim <- apply(abs(L), 1, which.max); o <- order(prim, -abs(L[cbind(seq_len(p), prim)])); L <- L[o, , drop = FALSE]
  h2 <- 1 - fa$uniquenesses[o]
  fn <- paste0("第", seq_len(nf), "因子")
  res <- list(test = paste0("探索的因子分析（最尤法", if (nf > 1) paste0("、", if (rot == "promax") "プロマックス回転" else "バリマックス回転") else "", "）"), n = n_info(nrow(df), n))
  res$grids <- list(list(kind = "grid", title = "因子負荷量", corner = "項目", rows = A(rownames(L)), cols = A(c(fn, "共通性")), fmt = A(rep("num2", nf + 1)), cells = mat(cbind(L, h2)),
                         mark = lapply(seq_len(p), function(i) A(c(abs(L[i, ]) >= 0.4, FALSE))), note = "絶対値が 0.40 以上の負荷量を太字にした。項目は最も大きく負荷する因子ごとに並べた。"),
                    list(kind = "grid", title = "相関行列の固有値", corner = "", rows = A(paste0(seq_len(min(p, 10)))), cols = A(c("固有値", "累積寄与率 (%)")), fmt = A(c("num2", "num1")),
                         cells = mat(cbind(ev[seq_len(min(p, 10))], 100 * cumsum(ev)[seq_len(min(p, 10))] / p)), note = "固有値が 1 を超える数（カイザー基準）を、因子の数の目安とした（「自動」のとき）。"))
  if (nf > 1 && rot == "promax" && !is.null(fa$rotmat)) {
    Ti <- solve(fa$rotmat); Phi <- Ti %*% t(Ti)
    res$grids <- c(res$grids, list(list(kind = "grid", title = "因子間相関", corner = "", rows = A(fn), cols = A(fn), fmt = A(rep("num2", nf)), cells = mat(Phi))))
  }
  pv <- if (rot == "promax" && nf > 1) NULL else colSums(L^2) / p
  res$headline <- list(hl("因子", as.character(nf)), hl("項目", as.character(p)), hl("n", as.character(n)))
  res$row <- list(stat = "", effect = "")
  grp <- vapply(seq_len(nf), function(j) { it <- rownames(L)[apply(abs(L), 1, which.max) == j & apply(abs(L), 1, max) >= 0.4]; if (length(it)) paste0(fn[j], "には", paste(it, collapse = "、"), "が高く負荷した") else paste0(fn[j], "に高く負荷する項目はなかった") }, "")
  weak <- rownames(L)[apply(abs(L), 1, max) < 0.4]
  res$report <- paste0(p, " 項目について、すべての項目に答えた ", n, " 人のデータで探索的因子分析（最尤法", if (nf > 1) paste0("、", if (rot == "promax") "プロマックス回転" else "バリマックス回転") else "",
                       "）を行った。", if (identical(nf0, "auto")) paste0("相関行列の固有値が 1 を超える数から因子の数を ", nf, " とした。") else paste0("因子の数を ", nf, " とした。"),
                       paste(grp, collapse = "。"), "。", if (length(weak)) paste0("どの因子にも 0.40 以上負荷しなかった項目は", paste(weak, collapse = "、"), "であった。") else "",
                       if (!is.null(pv)) paste0("因子の寄与率は", paste0(fn, " ", fmt_num(100 * pv, 1), "%", collapse = "、"), "であった。") else "")
  res$notes <- c("段階の得点のピアソン相関行列から、最尤法で因子を求めています。因子の数は、固有値・解釈のしやすさ・先行研究を合わせて決めてください。",
                 "因子に名前を付けるときは、高く負荷した項目の内容を読んで判断します。尺度として使う項目が決まったら、クロンバックの α で信頼性を確かめてください。")
  finish(res, alpha)
}
register("factor", "探索的因子分析", "尺度・多変量", "多くの段階の質問の背後にある共通の要因（因子）を探します。質問紙の尺度づくりに使います。",
         list(S("items", "分析する項目（同じ選択肢）", "ordinal", "例：海への関心の項目すべて", FALSE, TRUE)),
         list(O_select("nf", "因子の数", list(auto = "自動（固有値が 1 を超える数）", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5"), "auto"),
              O_select("rotation", "回転", list(promax = "プロマックス（因子間の相関を認める）", varimax = "バリマックス（因子を独立とする）"), "promax",
                       "心理・教育の尺度では、因子どうしに相関があると考えるプロマックス回転がよく使われます。")), an_factor)

# ---------------------------------------------------------------- コレスポンデンス分析
an_ca <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); sa <- req$slots$a; sb <- req$slots$b
  a <- slot_factor(df, sa); b <- slot_factor(df, sb); ok <- !is.na(a) & !is.na(b)
  N <- table(droplevels(a[ok]), droplevels(b[ok])); N <- N[rowSums(N) > 0, colSums(N) > 0, drop = FALSE]
  if (nrow(N) < 3 || ncol(N) < 3) stop_user("コレスポンデンス分析には、行・列とも 3 つ以上の選択肢が必要です（現在 ", nrow(N), " × ", ncol(N), "）。")
  n <- sum(N); P <- N / n; r <- rowSums(P); cc <- colSums(P)
  Sm <- diag(1 / sqrt(r)) %*% (P - r %o% cc) %*% diag(1 / sqrt(cc))
  sv <- svd(Sm); K <- min(nrow(N), ncol(N)) - 1; d <- sv$d[seq_len(K)]; inert <- d^2; share <- inert / sum(inert)
  Fr <- diag(1 / sqrt(r)) %*% sv$u[, 1:2] %*% diag(d[1:2]); Gc <- diag(1 / sqrt(cc)) %*% sv$v[, 1:2] %*% diag(d[1:2])
  for (j in 1:2) if (sum(Fr[, j] * r) == 0 && Fr[which.max(abs(Fr[, j])), j] < 0) { Fr[, j] <- -Fr[, j]; Gc[, j] <- -Gc[, j] }
  chi <- suppressWarnings(stats::chisq.test(N, correct = FALSE))
  res <- list(test = "コレスポンデンス分析", p = unname(chi$p.value), n = n_info(nrow(df), n))
  res$xtab <- xtab_obj(N, sa$label, sb$label, resid = FALSE)
  res$grids <- list(list(kind = "grid", title = "次元ごとの寄与率", corner = "次元", rows = A(paste0(seq_len(K))), cols = A(c("特異値", "慣性（固有値）", "寄与率 (%)", "累積寄与率 (%)")),
                         fmt = A(c("num2", "num2", "num1", "num1")), cells = mat(cbind(d, inert, 100 * share, 100 * cumsum(share)))),
                    list(kind = "grid", title = "座標（第1・第2次元）", corner = "", rows = A(c(paste0(sa$label, "：", rownames(N)), paste0(sb$label, "：", colnames(N)))),
                         cols = A(c("第1次元", "第2次元")), fmt = A(c("num2", "num2")), cells = mat(rbind(Fr, Gc)), note = "行と列の両方を主座標で示した（対称マップ）。"))
  res$fig <- list(type = "map", pts = list(list(name = sa$label, labels = A(rownames(N)), x = A(Fr[, 1]), y = A(Fr[, 2])), list(name = sb$label, labels = A(colnames(N)), x = A(Gc[, 1]), y = A(Gc[, 2]))),
                  xTitle = paste0("第1次元（", fmt_num(100 * share[1], 1), "%）"), yTitle = paste0("第2次元（", fmt_num(100 * share[2], 1), "%）"))
  res$headline <- list(hl("第1・第2次元の累積寄与率", paste0(fmt_num(100 * sum(share[1:2]), 1), "%")), hl("n", as.character(n)))
  res$row <- list(stat = paste0("χ²(", unname(chi$parameter), ") = ", fmt_num(unname(chi$statistic))), effect = "")
  near <- function(i) { dd <- sqrt((Gc[, 1] - Fr[i, 1])^2 + (Gc[, 2] - Fr[i, 2])^2); colnames(N)[which.min(dd)] }
  res$report <- paste0(q(sa$label), "と", q(sb$label), "のクロス集計表（n = ", n, "）にコレスポンデンス分析を行った。第1次元と第2次元の寄与率はそれぞれ ", fmt_num(100 * share[1], 1), "%、",
                       fmt_num(100 * share[2], 1), "%で、2 つの次元で全体の ", fmt_num(100 * sum(share[1:2]), 1), "% を説明した。図上で近くに布置された組み合わせは、",
                       paste0(rownames(N), "と", vapply(seq_len(nrow(N)), near, ""), collapse = "、"), "であった。")
  res$notes <- c("図上の距離は目安です。行どうし・列どうしの近さは似た回答のしかたを示しますが、行と列の点の距離は厳密な意味を持たないため、元のクロス集計表と合わせて解釈してください。",
                 paste0("p 値は、表全体の独立性のカイ二乗検定（χ²(", unname(chi$parameter), ") = ", fmt_num(unname(chi$statistic)), "）によります。"))
  finish(res, alpha)
}
register("ca", "コレスポンデンス分析", "尺度・多変量", "選択肢の多い 2 つの質問のクロス集計表を、似た回答をする選択肢どうしが近くに来る 1 枚の図（布置図）にまとめます。",
         list(S("a", "質問 1（行）", cat_types, "例：好きな食べ物"), S("b", "質問 2（列）", cat_types, "例：学年、部活動")), list(), an_ca)

# ---------------------------------------------------------------- 回答日時の推移
# 日付の計算は、webR でも時刻帯のデータを読まずに済むよう、整数の暦計算で行う（1970-01-01 からの日数）
days_from_civil <- function(y, m, d) { y <- y - (m <= 2); era <- floor(y / 400); yoe <- y - era * 400; mp <- (m + 9) %% 12
  doy <- floor((153 * mp + 2) / 5) + d - 1; doe <- yoe * 365 + floor(yoe / 4) - floor(yoe / 100) + doy; era * 146097 + doe - 719468 }
civil_from_days <- function(z) { z <- z + 719468; era <- floor(z / 146097); doe <- z - era * 146097; yoe <- floor((doe - floor(doe / 1460) + floor(doe / 36524) - floor(doe / 146096)) / 365)
  y <- yoe + era * 400; doy <- doe - (365 * yoe + floor(yoe / 4) - floor(yoe / 100)); mp <- floor((5 * doy + 2) / 153); d <- doy - floor((153 * mp + 2) / 5) + 1
  m <- mp + ifelse(mp < 10, 3, -9); cbind(y = y + (m <= 2), m = m, d = d) }
fmt_day <- function(z, jp = FALSE) { c <- civil_from_days(z); if (jp) sprintf("%d年%d月%d日", c[, 1], c[, 2], c[, 3]) else sprintf("%d/%02d/%02d", c[, 1], c[, 2], c[, 3]) }
an_timeline <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); s <- req$slots$t; x <- as.character(df[[s$col]])
  m <- regmatches(x, regexec("^([0-9]{4})[/-]([0-9]{1,2})[/-]([0-9]{1,2})", x))
  okm <- lengths(m) >= 4
  if (!any(okm)) stop_user(q(s$label), "から日付を読み取れませんでした。")
  mm <- do.call(rbind, lapply(m[okm], function(v) as.numeric(v[2:4])))
  z <- days_from_civil(mm[, 1], mm[, 2], mm[, 3])
  per <- opt(req, "per", "day")
  if (per == "week") { zk <- z - ((z + 3) %% 7); all <- seq(min(zk), max(zk), by = 7); key <- match(zk, all); lab <- paste0(fmt_day(all), " 週") }
  else if (per == "month") { mk <- mm[, 1] * 12 + mm[, 2] - 1; all <- seq(min(mk), max(mk)); key <- match(mk, all); lab <- sprintf("%d/%02d", all %/% 12, all %% 12 + 1) }
  else { all <- seq(min(z), max(z)); key <- match(z, all); lab <- fmt_day(all) }
  cnt <- tabulate(key, nbins = length(all)); n <- sum(cnt)
  res <- list(test = "回答日時の推移", n = n_info(nrow(df), n))
  res$grids <- list(list(kind = "grid", title = "期間ごとの回答数", corner = c(day = "日付", week = "週（月曜始まり）", month = "月")[[per]], rows = A(lab), cols = A(c("回答数", "累計", "累計の割合 (%)")),
                         fmt = A(c("int", "int", "pct")), cells = mat(cbind(cnt, cumsum(cnt), cumsum(cnt) / n))))
  res$fig <- list(type = "freq", cats = A(lab), series = A("回答数"), count = mat(cnt), base = n, catTitle = c(day = "日付", week = "週", month = "月")[[per]], time = TRUE, prefer = "line")
  pk <- which.max(cnt); half <- lab[which(cumsum(cnt) >= n / 2)[1]]
  res$headline <- list(hl("回答", as.character(n)), hl("期間", paste0(lab[1], "〜", lab[length(lab)])), hl("最多", paste0(lab[pk], "（", cnt[pk], " 件）")))
  res$row <- list(stat = "", effect = "")
  res$report <- paste0("回答（n = ", n, "）は ", fmt_day(min(z), TRUE), "から ", fmt_day(max(z), TRUE), "までに寄せられた。回答が最も多かったのは", lab[pk], "（", cnt[pk],
                       " 件）で、", half, "の時点で回答の半数に達した。")
  finish(res, alpha)
}
register("timeline", "回答日時の推移", "集計", "タイムスタンプから、日・週・月ごとの回答数と累計を出します。回答の集まり方を確かめるときに使います。",
         list(S("t", "日時の列", "time", "Google フォームのタイムスタンプ")),
         list(O_select("per", "まとめる単位", list(day = "日", week = "週", month = "月"), "day")), an_timeline)

# ---------------------------------------------------------------- テキストマイニング（自由記述）
# 単語への分割は画面側（ブラウザの Intl.Segmenter）で行い、req$docs（回答ごとの単語の並び）と req$grp（群）を受け取る。
# ここでは、頻出語・群ごとの特徴語・共起ネットワーク（Jaccard 係数と配置）を求める。
layout_fr <- function(n, E, W, iter = 400) {                # Fruchterman–Reingold 法（乱数の種を固定）。離れたまとまりは中心へ引き寄せる
  set.seed(7); pos <- matrix(stats::runif(2 * n, -1, 1), n, 2); if (n == 1) return(matrix(0, 1, 2))
  k <- sqrt(6 / n); t <- 0.3
  for (it in seq_len(iter)) {
    dx <- outer(pos[, 1], pos[, 1], "-"); dy <- outer(pos[, 2], pos[, 2], "-"); d <- sqrt(dx^2 + dy^2) + 1e-6
    fr <- k^2 / d; diag(fr) <- 0
    disp <- cbind(rowSums(dx / d * fr), rowSums(dy / d * fr))
    if (nrow(E)) for (e in seq_len(nrow(E))) {
      i <- E[e, 1]; j <- E[e, 2]; v <- pos[i, ] - pos[j, ]; dd <- sqrt(sum(v^2)) + 1e-6; fa <- dd^2 / k * (0.6 + 0.8 * W[e])
      disp[i, ] <- disp[i, ] - v / dd * fa; disp[j, ] <- disp[j, ] + v / dd * fa
    }
    disp <- disp - pos * 1.2 * k * sqrt(rowSums(pos^2) + 0.01)
    len <- sqrt(rowSums(disp^2)) + 1e-9; pos <- pos + disp / len * pmin(len, t); t <- max(0.005, t * 0.985)
  }
  pos
}
communities <- function(n, E, W) {                           # 重み付きのラベル伝播（順序を固定）でまとまりを分ける
  lab <- seq_len(n); if (!nrow(E)) return(lab)
  for (r in 1:30) {
    ch <- FALSE
    for (i in seq_len(n)) {
      ix <- which(E[, 1] == i | E[, 2] == i); if (!length(ix)) next
      nb <- ifelse(E[ix, 1] == i, E[ix, 2], E[ix, 1]); sc <- tapply(W[ix], lab[nb], sum); best <- as.integer(names(sc)[which.max(sc)])
      if (best != lab[i]) { lab[i] <- best; ch <- TRUE }
    }
    if (!ch) break
  }
  as.integer(factor(lab, levels = unique(lab)))
}
an_textmine <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); st <- req$slots$t; sg <- req$slots$g
  docs <- lapply(req$docs, function(d) unlist(d)); grp <- if (!is.null(req$grp)) unlist(lapply(req$grp, function(x) if (is.null(x)) "" else x)) else NULL
  if (is.null(req$docs)) stop_user("単語に分けた結果がありません。")
  has <- lengths(docs) > 0; nd <- sum(has)
  if (nd < 3) stop_user("単語を取り出せた回答が 3 件未満です（", nd, " 件）。")
  docs <- docs[has]; if (!is.null(grp)) grp <- grp[has]
  topN <- as.integer(opt(req, "top", "60")); minf <- max(1L, as.integer(opt(req, "minf", "2")))
  tf <- table(unlist(docs)); dfq <- table(unlist(lapply(docs, unique)))
  w <- names(dfq)[order(-as.integer(dfq), -as.integer(tf[names(dfq)]))]; w <- w[as.integer(dfq[w]) >= minf]
  if (!length(w)) stop_user(minf, " 件以上の回答に出てくる語がありません。「語の最低出現数」を下げてください。")
  w <- utils::head(w, topN); Encoding(w) <- "UTF-8"
  dfw <- as.integer(dfq[w]); tfw <- as.integer(tf[w])
  res <- list(test = "テキストマイニング（頻出語と共起）", n = n_info(nrow(df), nd))
  res$grids <- list(list(kind = "grid", title = "頻出語", corner = "語", rows = A(w), cols = A(c("出現した回答数", "回答に占める割合 (%)", "出現回数")), fmt = A(c("int", "pct", "int")),
                         cells = mat(cbind(dfw, dfw / nd, tfw)), note = paste0("単語を取り出せた ", nd, " 件の回答で数えた。「出現した回答数」は、1 つの回答に何回出ても 1 と数える。")))
  # 共起：回答ごとの出現（0/1）から Jaccard 係数
  M <- t(vapply(docs, function(d) w %in% d, logical(length(w)))); if (length(w) == 1) M <- matrix(M, ncol = 1)
  co <- crossprod(M * 1); dg <- diag(co); J <- co / (outer(dg, dg, "+") - co); diag(J) <- 0
  ed <- which(upper.tri(J) & co >= 2, arr.ind = TRUE); ed <- ed[order(-J[ed]), , drop = FALSE]
  nE <- min(nrow(ed), as.integer(opt(req, "edges", "60"))); ed <- ed[seq_len(nE), , drop = FALSE]; Wt <- J[ed]
  used <- sort(unique(c(ed))); if (!length(used)) used <- seq_len(min(length(w), 20))
  map <- match(seq_along(w), used); E2 <- if (nE) cbind(map[ed[, 1]], map[ed[, 2]]) else matrix(0L, 0, 2)
  pos <- layout_fr(length(used), E2, if (nE) Wt / max(Wt) else numeric(0)); cm <- communities(length(used), E2, Wt)
  net <- list(nodes = lapply(seq_along(used), function(i) list(label = w[used[i]], n = dfw[used[i]], x = pos[i, 1], y = pos[i, 2], c = cm[i])),
              edges = if (nE) lapply(seq_len(nE), function(e) list(a = E2[e, 1] - 1L, b = E2[e, 2] - 1L, w = Wt[e])) else list())
  res$fig <- list(type = "words", words = A(w), n = A(dfw), base = nd, net = net, prefer = "wbar", group = if (is.null(grp)) NULL else NULL)
  top <- utils::head(w, 5)
  txt <- paste0(q(st$label), "の自由記述（単語を取り出せた ", nd, " 件）を単語に分けて集計した。多く出現した語は、", paste0(q(top), "（", utils::head(dfw, 5), " 件）", collapse = "、"), "であった。")
  if (nE) {
    o <- order(-Wt)[seq_len(min(3, nE))]
    txt <- paste0(txt, "同じ回答の中で一緒に使われやすい語の組み合わせ（Jaccard 係数が高い組）は、", paste0(q(w[ed[o, 1]]), "と", q(w[ed[o, 2]]), "（", fmt_num(Wt[o]), "）", collapse = "、"), "であった。")
  }
  # 群ごとの特徴語：各語を使った回答の割合を群で比べる（カイ二乗検定、語の数でホルム法により補正）
  ngrp <- 0L
  if (!is.null(grp)) {
    gf <- factor(grp[grp != ""]); Mg <- M[grp != "", , drop = FALSE]; L <- levels(gf)
    if (nlevels(gf) >= 2) {
      ngrp <- nlevels(gf)
      cnt <- vapply(L, function(l) colSums(Mg[gf == l, , drop = FALSE]), numeric(length(w))); if (length(w) == 1) cnt <- matrix(cnt, nrow = 1)
      ng <- as.integer(table(gf)); pr <- sweep(cnt, 2, ng, "/")
      pv <- vapply(seq_along(w), function(i) { tb <- rbind(cnt[i, ], ng - cnt[i, ]); if (any(rowSums(tb) == 0)) NA else suppressWarnings(stats::chisq.test(tb)$p.value) }, 0)
      pa <- stats::p.adjust(pv, "holm")
      res$grids <- c(res$grids, list(list(kind = "grid", title = "群ごとの語の使用割合 (%)", corner = "語", rows = A(w),
                                          cols = A(c(paste0(L, "（n = ", ng, "）"), "補正後の p")), fmt = A(c(rep("pct", length(L)), "p")), cells = mat(cbind(pr, pa)),
                                          mark = lapply(seq_along(w), function(i) A(c(if (!is.na(pa[i]) && pa[i] < alpha) pr[i, ] == max(pr[i, ]) else rep(FALSE, length(L)), FALSE))),
                                          note = "語ごとに、使った回答の割合を群で比べた（カイ二乗検定。p は語の数でホルム法により補正）。有意な語は、割合が最も高い群を太字にした。")))
      sigw <- which(!is.na(pa) & pa < alpha)
      txt <- paste0(txt, q(sg$label), "で比べると、", if (length(sigw)) paste0(paste0(q(w[sigw]), "は", L[apply(pr[sigw, , drop = FALSE], 1, which.max)], collapse = "、"), "で多く使われていた（ホルム法で補正した p < ", alpha, "）。") else "群の間で使用の割合に有意な差のある語はなかった。")
      res$fig$groups <- list(names = A(L), share = mat(pr))
    }
  }
  res$headline <- list(hl("回答", as.character(nd)), hl("語の種類", as.character(length(dfq))), hl("最多", paste0(w[1], "（", dfw[1], "）")))
  res$row <- list(stat = "", effect = "")
  res$report <- txt
  # 報告書の「方法」：単語の分け方、除いた語、数え方、対象にした語の条件、共起とグループ比較の方法を、実際の設定で書く
  ans <- { v <- as.character(df[[st$col]]); sum(!is.na(v) & nzchar(trimws(v))) }
  us <- unlist(req$stopUser); cmp <- !isFALSE(opt(req, "compound", TRUE)); emax <- as.integer(opt(req, "edges", "60"))
  m <- paste0(q(st$label), "の自由記述 ", ans, " 件のうち、語を取り出せた ", nd, " 件を分析した。",
              "文字を Unicode の NFKC で正規化し、英字を小文字にそろえたうえで、",
              if (identical(req$seg, "chartype")) "文字の種類（漢字・カタカナ・ひらがな・英数字）の切れ目で語に分けた（使用したブラウザが日本語の単語分割に対応していなかったため）。"
              else "ブラウザに組み込まれた日本語の単語分割（Intl.Segmenter、ICU の辞書による）で語に分けた。",
              if (cmp) "漢字・カタカナの語が続く場合は、つなげて 1 語とした（例：自然＋環境 → 自然環境）。" else "",
              "助詞・助動詞や「こと」「思う」のような意味の薄い語（あらかじめ用意した ", if (is.null(req$nstop)) "" else paste0(req$nstop, " 語の"), "一覧）、数字だけの語、2 文字以下のひらがなの語、漢字以外の 1 文字の語を除いた。",
              if (length(us)) paste0("さらに、", paste0(q(us), collapse = "、"), "を除いた。") else "",
              "語の出現は、1 つの回答に何回出ても 1 回と数え、", minf, " 件以上の回答に出た語のうち、出現した回答数の多い順に上位 ", topN, " 語（同数のときは出現回数の多い順）を分析の対象とした（今回は ", length(w), " 語）。",
              "2 語の結び付きの強さは、両方が出た回答数を、どちらか一方でも出た回答数で割った Jaccard 係数で表した。両方が出た回答が 2 件以上の組のうち、係数の大きい組から最大 ", emax, " 組（今回は ", nE, " 組）を共起ネットワークの線とした。語の配置にはフルクターマン・ラインゴールド法を、まとまりの色分けにはラベル伝播法を用いた。")
  if (ngrp >= 2)
    m <- paste0(m, q(sg$label), "による違いは、語ごとに、その語を使った回答と使わなかった回答の数を群で比べるカイ二乗検定（2 × ", ngrp, " の表）で調べ、p 値は分析した語の数（", length(w), " 語）でホルム法により補正した。有意水準は ", alpha, " とした。")
  res$method <- m
  res$notes <- c("単語の分割には、ブラウザに組み込まれた日本語の分かち書き（Intl.Segmenter）を使い、助詞や「思う」「こと」のような意味の薄い語を除いています。",
                 "図は「ワードクラウド」「頻出語の棒グラフ」「共起ネットワーク」を切り替えられます。共起ネットワークの線は、同じ回答に一緒に出る語どうしを結び、色は結び付きの強いまとまりを表します。")
  finish(res, alpha)
}
register("textmine", "テキストマイニング（自由記述）", "自由記述", "自由記述の回答を単語に分け、よく出てくる語、一緒に使われる語の組み合わせ（共起ネットワーク）、グループごとに多く使われる語を調べます。",
         list(S("t", "自由記述の質問", c("text", "nominal"), "例：感想、意見"), S("g", "グループ（任意）", cat_types, "選ぶとグループごとに多く使われる語を比べます", TRUE, optional = TRUE)),
         list(O_select("top", "取り出す語の数", list("30" = "30 語", "60" = "60 語", "100" = "100 語"), "60"),
              O_select("minf", "語の最低出現数（回答数）", list("1" = "1", "2" = "2", "3" = "3", "5" = "5", "10" = "10"), "2", "この数より少ない回答にしか出てこない語は除きます。"),
              O_select("edges", "共起ネットワークの線の数", list("30" = "30 本", "60" = "60 本", "100" = "100 本"), "60"),
              O_check("compound", "漢字・カタカナの続く語をつなげる（例：自然＋環境 → 自然環境）", TRUE),
              list(id = "stop", kind = "text", label = "除く語（読点「、」で区切る）", default = "", hint = "集計から外したい語を書きます。例：特に、なし、自然")), an_textmine)
