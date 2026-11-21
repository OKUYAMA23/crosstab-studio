# analyses2.R ― 数値・得点を扱う解析と、多変量の解析（0.7.0 で追加）。
#   数値の要約とヒストグラム／t 検定／一元配置分散分析／対応のある t 検定／ピアソンの相関／
#   尺度の信頼性（クロンバックの α）／探索的因子分析／コレスポンデンス分析／回答日時の推移

num_types <- c("numeric", "ordinal")
score_note <- function(s) if (identical(s$type, "ordinal")) { lev <- unique(slot_labels(s))
  if (is.null(s[["points"]])) trf("%sは、選択肢に%sの側から高い得点（%d〜1 点）を付けて計算した。", q(s$label), q(lev[1]), length(lev))
  else trf("%sは、選択肢に次の得点を付けて計算した（%s）。", q(s$label), points_text(lev, lev_points(s, lev))) } else NULL
points_text <- function(lev, pts) paste(paste0(lev, " = ", fmt_pts(pts)), collapse = tr_("、"))
fmt_pts <- function(p) ifelse(p == round(p), format(round(p), trim = TRUE), format(p, trim = TRUE))
clean_note <- function(df, s) {
  if (!identical(s$type, "numeric")) return(NULL)
  st <- clean_number(df[[s$col]], s$unit)$status; v <- slot_score(df, s)
  nf <- sum(st %in% c("fixed", "range") & !is.na(v)); nx <- sum(st == "fail" & is.na(v))
  if (nf + nx == 0) return(NULL)
  trf("%sは、%s。", q(s$label), paste(c(if (nf) trf("単位や全角文字などを直して数値にした回答 %d 件を含む", nf), if (nx) trf("数値として読めない回答 %d 件は除外した", nx)), collapse = tr_("。")))
}
desc8 <- function(v) c(length(v), mean(v), if (length(v) > 1) stats::sd(v) else NA, min(v), unname(stats::quantile(v, .25)), stats::median(v), unname(stats::quantile(v, .75)), max(v))
desc_cols <- function() c("n", tr_("平均"), tr_("標準偏差"), tr_("最小"), tr_("第1四分位"), tr_("中央値"), tr_("第3四分位"), tr_("最大"))
# 箱ひげ図の 5 数と外れ値に加えて、平均と 95% 信頼区間（t 分布）も持たせる（「平均と信頼区間」の図に使う）
box_of <- function(v) {
  b <- grDevices::boxplot.stats(v); n <- length(v); m <- mean(v)
  h <- if (n > 1) stats::qt(0.975, n - 1) * stats::sd(v) / sqrt(n) else NA_real_
  list(stats = A(b$stats), out = A(utils::head(b$out, 300)), n = n, mean = m, lo = if (is.na(h)) NULL else m - h, hi = if (is.na(h)) NULL else m + h)
}
fmt_ci <- function(ci, d = 2, s2 = FALSE) if (s2) paste0("[", fmt_num2s(ci[1]), ", ", fmt_num2s(ci[2]), "]") else paste0("[", fmt_num(ci[1], d), ", ", fmt_num(ci[2], d), "]")

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
  vapply(seq_len(k), function(i) paste0(fmt_num(br[i], d), tr_(" 以上 "), fmt_num(br[i + 1], d), if (i == k) tr_(" 以下") else tr_(" 未満")), "")
}
an_numsum <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); sy <- req$slots$y; sg <- req$slots$g
  y <- slot_score(df, sy); g <- if (!is.null(sg)) slot_factor(df, sg) else NULL
  ok <- !is.na(y) & (if (is.null(g)) TRUE else !is.na(g))
  y <- y[ok]; if (!is.null(g)) g <- droplevels(g[ok])
  n <- length(y); if (n < 2) stop_user(q(sy$label), tr_("で数値として読める回答が 2 件未満です。"))
  groups <- if (is.null(g)) setNames(list(y), tr_("全体")) else c(split(y, g), setNames(list(y), tr_("全体")))
  M <- t(vapply(groups, desc8, numeric(8)))
  res <- list(test = tr_("数値の要約とヒストグラム"), n = n_info(nrow(df), n))
  how <- opt(req, "bins", "auto"); br <- hist_breaks(y, how); intbins <- all(abs(diff(br) - 1) < 1e-9) && all(abs(br %% 1 - 0.5) < 1e-9)
  ser <- if (is.null(g)) tr_("度数") else levels(g)
  cnt <- vapply(if (is.null(g)) list(y) else split(y, g), function(v) as.integer(table(cut(v, br, right = FALSE, include.lowest = TRUE))), integer(length(br) - 1))
  cnt <- matrix(cnt, ncol = length(ser)); labs <- bin_labels(br, intbins)
  res$grids <- list(
    list(kind = "grid", title = tr_("要約統計量"), corner = if (is.null(g)) sy$label else sg$label, rows = A(names(groups)), cols = A(desc_cols()),
         fmt = A(c("int", rep("num2", 7))), cells = mat(M), note = tr_("標準偏差は n − 1 で割った値（不偏標準偏差）。四分位は R の既定（type 7）で求めた。")),
    list(kind = "grid", title = tr_("度数分布"), corner = if (intbins) sy$label else paste0(sy$label, tr_("（階級）")), rows = A(labs),
         cols = A(if (is.null(g)) c(tr_("度数"), tr_("割合 (%)")) else ser), fmt = A(if (is.null(g)) c("int", "pct") else rep("int", length(ser))),
         cells = mat(if (is.null(g)) cbind(cnt[, 1], cnt[, 1] / n) else cnt), note = if (intbins) tr_("値ごとの度数。") else tr_("階級は「以上・未満」で区切った（最後の階級だけ上端を含む）。")))
  res$fig <- list(type = "hist", edges = A(br), counts = mat(cnt), series = A(ser), xTitle = sy$label, seriesTitle = if (is.null(g)) "" else sg$label, intBins = intbins,
                  boxCats = A(if (is.null(g)) sy$label else levels(g)), box = unname(lapply(if (is.null(g)) list(y) else split(y, g), box_of)), catTitle = if (is.null(g)) "" else sg$label)
  tot <- M[nrow(M), ]
  res$headline <- list(hl("n", as.character(n)), hl(tr_("平均"), fmt_num(tot[2])), hl(tr_("標準偏差"), fmt_num(tot[3])), hl(tr_("中央値"), fmt_num(tot[6])))
  res$row <- list(stat = "", effect = "")
  txt <- trf("%s（n = %d）の平均は %s（SD = %s）、中央値は %s（第1四分位 %s、第3四分位 %s）、最小値は %s、最大値は %sであった。", q(sy$label), n, fmt_num(tot[2]), fmt_num(tot[3]), fmt_num(tot[6]),
             fmt_num(tot[5]), fmt_num(tot[7]), fmt_num(tot[4]), fmt_num(tot[8]))
  if (!is.null(g)) txt <- paste0(txt, trf("%s別の平均は、%sであった。", q(sg$label), paste0(levels(g), " ", fmt_num(M[seq_len(nlevels(g)), 2]), tr_("（SD = "), fmt_num(M[seq_len(nlevels(g)), 3]), tr_("、n = "), M[seq_len(nlevels(g)), 1], tr_("）"), collapse = tr_("、"))))
  skew <- if (n > 2 && tot[3] > 0) mean((y - tot[2])^3) / tot[3]^3 else 0
  if (abs(skew) >= 1) txt <- paste0(txt, if (skew > 0) trf("分布は右に裾の長い形（歪度 %s）で、平均は外れた値の影響を受けやすいため、中央値も併せて示す。", fmt_num(skew))
                                        else trf("分布は左に裾の長い形（歪度 %s）で、平均は外れた値の影響を受けやすいため、中央値も併せて示す。", fmt_num(skew)))
  res$report <- paste0(txt, paste(c(clean_note(df, sy), score_note(sy)), collapse = ""))
  res$notes <- c(clean_note(df, sy), tr_("図は「ヒストグラム」と「箱ひげ図」を切り替えられます。"))
  finish(res, alpha)
}
register("numsum", tr_("数値の要約とヒストグラム"), tr_("集計"), tr_("数字で答える質問（年齢・時間・回数など）の平均・標準偏差・中央値と度数分布を出し、ヒストグラムで示します。グループ別にも比べられます。"),
         list(S("y", tr_("数値の質問"), c("numeric", "ordinal"), tr_("例：年齢、1 日の勉強時間")), S("g", tr_("グループ（任意）"), cat_types, tr_("選ぶとグループ別に要約します"), TRUE, optional = TRUE)),
         list(O_select("bins", tr_("ヒストグラムの区切り方"), list(auto = tr_("自動（整数で範囲が狭ければ値ごと）"), sturges = tr_("スタージェスの公式"), fd = tr_("フリードマン＝ダイアコニスの方法"), int = tr_("値（整数）ごと")), "auto",
                       tr_("棒の数の決め方です。データが多いときは、フリードマン＝ダイアコニスの方法のほうが細かく区切ります。"))),
         an_numsum)

# ---------------------------------------------------------------- 平均の比較の準備（グループ × 数値）
mean_setup <- function(df, req) {
  sg <- req$slots$g; sy <- req$slots$y
  g <- slot_factor(df, sg); y <- slot_score(df, sy); ok <- !is.na(g) & !is.na(y)
  g <- droplevels(g[ok]); y <- y[ok]
  sm <- t(vapply(levels(g), function(l) { v <- y[g == l]; c(length(v), mean(v), if (length(v) > 1) stats::sd(v) else NA, stats::median(v)) }, numeric(4)))
  list(sg = sg, sy = sy, g = g, y = y, n = length(y), sm = sm,
       grid = list(kind = "grid", title = tr_("群ごとの平均"), corner = sg$label, rows = A(levels(g)), cols = A(c("n", tr_("平均"), tr_("標準偏差"), tr_("中央値"))), fmt = A(c("int", "num2", "num2", "num2")), cells = mat(sm)),
       fig = list(type = "box", cats = A(levels(g)), box = unname(lapply(split(y, g), box_of)), catTitle = sg$label, valueTitle = sy$label, means = A(sm[, 2]), prefer = "mean"))
}
grp_txt <- function(ms) paste0(rownames(ms$sm), tr_("：M = "), fmt_num(ms$sm[, 2]), ", SD = ", fmt_num(ms$sm[, 3]), ", n = ", ms$sm[, 1], collapse = tr_("、"))

# ---------------------------------------------------------------- t 検定（2 群）
an_ttest <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE); eqv <- isTRUE(opt(req, "student", FALSE))
  ms <- mean_setup(df, req); g <- ms$g; y <- ms$y
  if (nlevels(g) != 2) stop_user(tr_("t 検定は 2 つの群の比較です。"), q(ms$sg$label), tr_("は現在 "), nlevels(g), tr_(" 群です。「選択肢の設定」で 2 つにするか、一元配置分散分析を使ってください。"))
  l <- levels(g); x1 <- y[g == l[1]]; x2 <- y[g == l[2]]; n1 <- length(x1); n2 <- length(x2)
  if (n1 < 2 || n2 < 2) stop_user(tr_("各群に 2 人以上の回答が必要です（"), l[1], " ", n1, tr_(" 人、"), l[2], " ", n2, tr_(" 人）。"))
  if (stats::sd(c(x1, x2)) == 0) stop_user(tr_("すべての回答が同じ値のため、検定できません。"))
  tt <- stats::t.test(x1, x2, var.equal = eqv)
  sp <- sqrt(((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / (n1 + n2 - 2))
  d <- (mean(x1) - mean(x2)) / sp; J <- 1 - 3 / (4 * (n1 + n2) - 9); hg <- d * J
  nm <- if (eqv) tr_("スチューデントの t 検定") else tr_("ウェルチの t 検定")
  res <- list(test = nm, p = unname(tt$p.value), n = n_info(nrow(df), n1 + n2), grids = list(ms$grid), fig = ms$fig)
  dfv <- unname(tt$parameter); tv <- unname(tt$statistic); ci <- unname(tt$conf.int)
  res$chk <- list(t = tv, df = dfv, p = res$p, g = hg, lo = ci[1], hi = ci[2])     # Excel の数式と照合するための値
  st <- paste0("t(", fmt_num(dfv, if (eqv) 0 else 2), ") = ", fmt_num(tv), ", ", fmt_p(res$p, lead0), ", g = ", fmt_num(hg))
  res$headline <- list(hl(paste0("t(", fmt_num(dfv, if (eqv) 0 else 2), ")"), fmt_num(tv)), hl("p", fmt_p(res$p, lead0, FALSE)), hl(tr_("Hedges の g"), fmt_num(hg)), hl(tr_("平均の差"), fmt_num(mean(x1) - mean(x2))))
  res$row <- list(stat = paste0("t(", fmt_num(dfv, if (eqv) 0 else 2), ") = ", fmt_num(tv)), effect = paste0("g = ", fmt_num(hg)))
  res$grids <- c(res$grids, list(list(kind = "grid", title = tr_("平均の差"), corner = "", rows = A(paste0(l[1], " − ", l[2])), cols = A(c(tr_("平均の差"), tr_("95% 信頼区間の下限"), tr_("上限"), "t", tr_("自由度"), "p", tr_("Hedges の g"))),
                                      fmt = A(c("num2", "num2", "num2", "num2", "num2", "p", "num2")), cells = mat(matrix(c(mean(x1) - mean(x2), ci, tv, dfv, res$p, hg), nrow = 1)))))
  sig <- res$p < alpha; hi <- if (mean(x1) >= mean(x2)) l[1] else l[2]; lo <- setdiff(l, hi)
  sz <- abs(hg); sz_w <- if (sz < 0.2) tr_("ごく小さい") else if (sz < 0.5) tr_("小さい") else if (sz < 0.8) tr_("中程度の") else tr_("大きい")
  res$report <- paste0(trf("%sの 2 群で%sの平均を比較した（%s）。", q(ms$sg$label), q(ms$sy$label), grp_txt(ms)),
                       trf("%sの結果、平均の差は%s（%s、平均の差 %s、95%% CI %s）。", lc_(nm), if (sig) tr_("統計的に有意であった") else tr_("統計的に有意ではなかった"), st, fmt_num(mean(x1) - mean(x2)), fmt_ci(ci)),
                       if (sig) trf("%sのほうが%sより平均が高く、効果量は%s大きさであった。", gq(hi), gq(lo), sz_w) else "", score_note(ms$sy), clean_note(df, ms$sy))
  res$notes <- c(if (!eqv) tr_("ウェルチの t 検定は、2 群の分散が等しいことを仮定しない t 検定です。分散が等しいと仮定するときは、オプションでスチューデントの t 検定に切り替えられます。") else NULL,
                 tr_("効果量 g は平均の差を併合標準偏差で割り、小標本の補正をした値です（|g| が 0.2：小、0.5：中、0.8：大 が目安）。"),
                 if (identical(ms$sy$type, "ordinal")) tr_("段階で答える回答の得点を平均しています。選択肢の間隔が等しいと見なせないときは、マン・ホイットニーの U 検定が適しています。") else NULL)
  finish(res, alpha)
}
register("ttest", tr_("t 検定（2 群の平均の比較）"), tr_("平均の比較"), tr_("2 つのグループで、数値（または段階の得点）の平均に差があるかを調べます。効果量と平均の差の信頼区間も出します。"),
         list(S("g", tr_("グループ（2 群）"), cat_types, tr_("例：性別"), TRUE), S("y", tr_("比べる値"), num_types, tr_("数値、または段階で答える質問"), TRUE)),
         list(O_check("student", tr_("分散が等しいと仮定する（スチューデントの t 検定）"), FALSE, tr_("通常は、分散が等しいことを仮定しないウェルチの t 検定（既定）を使います。"))), an_ttest)

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
  if (k < 3) stop_user(tr_("一元配置分散分析は 3 つ以上の群の比較です。2 群のときは t 検定を使ってください。"))
  if (any(ms$sm[, 1] < 2)) stop_user(tr_("回答が 1 人以下の群があります（"), paste(rownames(ms$sm)[ms$sm[, 1] < 2], collapse = tr_("、")), tr_("）。「選択肢の設定」でまとめるか外してください。"))
  if (stats::sd(y) == 0) stop_user(tr_("すべての回答が同じ値のため、検定できません。"))
  ow <- stats::oneway.test(y ~ g, var.equal = eqv)
  ssb <- sum(tapply(y, g, length) * (tapply(y, g, mean) - mean(y))^2); sst <- sum((y - mean(y))^2); eta2 <- ssb / sst
  nm <- if (eqv) tr_("一元配置分散分析") else tr_("ウェルチの一元配置分散分析")
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
    ptitle <- tr_("多重比較（テューキーの方法）")
  } else { post <- games_howell(y, g); ptitle <- tr_("多重比較（ゲームス・ハウエルの方法）") }
  res$grids <- c(res$grids, list(list(kind = "grid", title = ptitle, corner = tr_("組み合わせ"), rows = A(post$names), cols = A(c(tr_("平均の差"), tr_("調整後の p"))), fmt = A(c("num2", "p")),
                                      cells = mat(post$M[, c(1, 4), drop = FALSE]), mark = lapply(post$M[, 4] < alpha, function(b) A(c(FALSE, b))))))
  sig <- res$p < alpha; sigp <- post$names[post$M[, 4] < alpha]
  o <- order(-ms$sm[, 2])
  res$report <- paste0(trf("%sの %d 群で%sの平均を比較した（%s）。", q(ms$sg$label), k, q(ms$sy$label), grp_txt(ms)),
                       trf("%sの結果、群の間の平均の差は%s（%s）。", lc_(nm), if (sig) tr_("統計的に有意であった") else tr_("統計的に有意ではなかった"), st),
                       if (sig) paste0(trf("平均は%sの順に高かった。", paste(rownames(ms$sm)[o], collapse = tr_("、"))),
                                       trf("%sによる多重比較では、%s", if (eqv) tr_("テューキーの方法") else tr_("ゲームス・ハウエルの方法"),
                                           if (length(sigp)) trf("%sの間に有意な差が認められた。", paste(sigp, collapse = tr_("、"))) else tr_("どの組み合わせにも有意な差は認められなかった。"))) else "",
                       score_note(ms$sy), clean_note(df, ms$sy))
  res$notes <- c(if (!eqv) tr_("ウェルチの一元配置分散分析は、群の分散が等しいことを仮定しません。多重比較も分散の違いを考えるゲームス・ハウエルの方法で行います。") else tr_("分散が等しいと仮定する一元配置分散分析と、テューキーの方法による多重比較です。"),
                 tr_("η²（イータ二乗）は群の違いで説明される分散の割合です（0.01：小、0.06：中、0.14：大 が目安）。"),
                 if (identical(ms$sy$type, "ordinal")) tr_("段階で答える回答の得点を平均しています。選択肢の間隔が等しいと見なせないときは、クラスカル・ウォリス検定が適しています。") else NULL)
  finish(res, alpha)
}
register("anova", tr_("一元配置分散分析（3 群以上の平均の比較）"), tr_("平均の比較"), tr_("3 つ以上のグループで、数値（または段階の得点）の平均に差があるかを調べ、どの組み合わせに差があるかも出します。"),
         list(S("g", tr_("グループ（3 群以上）"), cat_types, tr_("例：学年"), TRUE), S("y", tr_("比べる値"), num_types, tr_("数値、または段階で答える質問"), TRUE)),
         list(O_check("classic", tr_("分散が等しいと仮定する（通常の分散分析とテューキーの方法）"), FALSE, tr_("通常は、分散が等しいことを仮定しないウェルチの方法（既定）を使います。"))), an_anova)

# ---------------------------------------------------------------- 対応のある t 検定
an_pairedt <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE); sx <- req$slots$x; sy <- req$slots$y
  x <- slot_score(df, sx); y <- slot_score(df, sy); ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 3) stop_user(tr_("両方に答えた人が 3 人以上必要です（現在 "), n, tr_(" 人）。"))
  d <- x - y; if (stats::sd(d) == 0) stop_user(tr_("すべての人で差が同じ値のため、検定できません。"))
  tt <- stats::t.test(x, y, paired = TRUE); dz <- mean(d) / stats::sd(d); ci <- unname(tt$conf.int)
  chk <- list(t = unname(tt$statistic), p = unname(tt$p.value), lo = ci[1], hi = ci[2], dz = dz)
  res <- list(test = tr_("対応のある t 検定"), p = unname(tt$p.value), n = n_info(nrow(df), n))
  sm <- rbind(c(n, mean(x), stats::sd(x)), c(n, mean(y), stats::sd(y)), c(n, mean(d), stats::sd(d)))
  res$grids <- list(list(kind = "grid", title = tr_("平均と差"), corner = "", rows = A(c(sx$label, sy$label, tr_("差（1 − 2）"))), cols = A(c("n", tr_("平均"), tr_("標準偏差"))), fmt = A(c("int", "num2", "num2")), cells = mat(sm),
                         note = paste0(tr_("両方に答えた "), n, tr_(" 人で計算した。"))))
  res$fig <- list(type = "box", cats = A(c(sx$label, sy$label)), box = list(box_of(x), box_of(y)), catTitle = "", valueTitle = tr_("値"), means = A(c(mean(x), mean(y))), prefer = "mean")
  tv <- unname(tt$statistic); dfv <- unname(tt$parameter)
  st <- paste0("t(", dfv, ") = ", fmt_num(tv), ", ", fmt_p(res$p, lead0), ", dz = ", fmt_num(dz))
  res$headline <- list(hl(paste0("t(", dfv, ")"), fmt_num(tv)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("dz", fmt_num(dz)), hl("n", as.character(n)))
  res$row <- list(stat = paste0("t(", dfv, ") = ", fmt_num(tv)), effect = paste0("dz = ", fmt_num(dz)))
  sig <- res$p < alpha
  res$report <- paste0(trf("%s（M = %s, SD = %s）と%s（M = %s, SD = %s）の平均を、両方に答えた %d 人で比較した。", q(sx$label), fmt_num(mean(x)), fmt_num(stats::sd(x)), q(sy$label), fmt_num(mean(y)), fmt_num(stats::sd(y)), n),
                       trf("対応のある t 検定の結果、平均の差は%s（%s、平均の差 %s、95%% CI %s）。", if (sig) tr_("統計的に有意であった") else tr_("統計的に有意ではなかった"), st, fmt_num(mean(d)), fmt_ci(ci)),
                       if (sig) trf("%sのほうが平均が高かった。", if (mean(d) > 0) q(sx$label) else q(sy$label)) else "", score_note(sx), score_note(sy))
  res$notes <- c(tr_("同じ人が答えた 2 つの値の差の平均が 0 と異なるかを調べます。効果量 dz は差の平均を差の標準偏差で割った値です。"),
                 if (identical(sx$type, "ordinal") || identical(sy$type, "ordinal")) tr_("段階の得点を平均しています。間隔が等しいと見なせないときは、ウィルコクソンの符号付順位検定が適しています。") else NULL)
  res$chk <- chk
  finish(res, alpha)
}
register("pairedt", tr_("対応のある t 検定"), tr_("平均の比較"), tr_("同じ人が答えた 2 つの値（事前と事後、2 つの項目の得点など）の平均に差があるかを調べます。"),
         list(S("x", tr_("値 1"), num_types, tr_("例：授業前の得点"), TRUE), S("y", tr_("値 2"), num_types, tr_("例：授業後の得点"), TRUE)), list(), an_pairedt)

# ---------------------------------------------------------------- ピアソンの相関
an_pearson <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE); sx <- req$slots$x; sy <- req$slots$y
  x <- slot_score(df, sx); y <- slot_score(df, sy); ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 4) stop_user(tr_("相関を求めるには、両方に答えた人が 4 人以上必要です（現在 "), n, tr_(" 人）。"))
  if (stats::sd(x) == 0 || stats::sd(y) == 0) stop_user(tr_("どちらかの回答がすべて同じ値のため、相関を求められません。"))
  ct <- stats::cor.test(x, y, method = "pearson"); r <- unname(ct$estimate); ci <- unname(ct$conf.int)
  fit <- stats::lm(y ~ x); b <- unname(stats::coef(fit))
  chk <- list(r = r, p = unname(ct$p.value), lo = ci[1], hi = ci[2], b0 = b[1], b1 = b[2])
  res <- list(test = tr_("ピアソンの相関係数"), p = unname(ct$p.value), n = n_info(nrow(df), n))
  res$fig <- list(type = "scatter", x = A(x), y = A(y), xTitle = sx$label, yTitle = sy$label, fit = A(b))
  res$grids <- list(list(kind = "grid", title = tr_("相関と回帰直線"), corner = "", rows = A(paste0(sx$label, tr_(" と "), sy$label)), cols = A(c("n", "r", tr_("95% 信頼区間の下限"), tr_("上限"), "p", tr_("切片"), tr_("傾き"))),
                         fmt = A(c("int", "num2", "num2", "num2", "p", "num2", "num2")), cells = mat(matrix(c(n, r, ci, res$p, b), nrow = 1)),
                         note = paste0(tr_("回帰直線は "), sy$label, " = ", fmt_num(b[1]), " + ", fmt_num(b[2]), " × ", sx$label, tr_("。"))))
  a <- abs(r); strength <- if (a < 0.2) tr_("ごく弱い") else if (a < 0.4) tr_("弱い") else if (a < 0.7) tr_("中程度の") else tr_("強い")
  st <- paste0("r = ", fmt_num(r), ", 95% CI ", fmt_ci(ci), ", ", fmt_p(res$p, lead0), ", n = ", n)
  res$headline <- list(hl("r", fmt_num(r)), hl("p", fmt_p(res$p, lead0, FALSE)), hl("n", as.character(n)))
  res$row <- list(stat = paste0("r = ", fmt_num(r)), effect = "–")
  sig <- res$p < alpha
  res$report <- paste0(trf("%sと%sの関連をピアソンの相関係数で調べた。その結果、%s（%s）。", q(sx$label), q(sy$label),
                       if (sig) trf("%s%sの相関が認められた", strength, if (r > 0) tr_("正") else tr_("負")) else tr_("統計的に有意な相関は認められなかった"), st),
                       score_note(sx), score_note(sy), clean_note(df, sx), clean_note(df, sy))
  res$notes <- c(tr_("ピアソンの相関係数は直線的な関連の強さです。外れた値や、段階の数が少ない回答の影響を受けやすいため、そのときはスピアマンの順位相関も確かめてください。"),
                 tr_("相関は 2 つの値の関連を示すもので、一方が他方の原因であることを示すものではありません。"))
  res$chk <- chk
  finish(res, alpha)
}
register("pearson", tr_("ピアソンの相関係数"), tr_("関連・相関"), tr_("2 つの数値（または段階の得点）の直線的な関連の強さを調べ、散布図と回帰直線を描きます。"),
         list(S("x", tr_("値 1（横軸）"), num_types, "", TRUE), S("y", tr_("値 2（縦軸）"), num_types, "", TRUE)), list(), an_pearson)

# ---------------------------------------------------------------- 尺度の信頼性・因子分析の準備（複数の段階の項目）
scale_matrix <- function(df, s, min_items) {
  it <- items_setup(df, s)
  if (it$k < min_items) stop_user(tr_("項目を "), min_items, tr_(" つ以上選んでください（現在 "), it$k, tr_(" 個）。"))
  PT <- lapply(seq_along(it$F), function(j) lev_points(s, it$lev, j))
  X <- sapply(seq_along(it$F), function(j) PT[[j]][as.integer(it$F[[j]])]); colnames(X) <- it$names
  answered <- vapply(it$cols, function(j) sum(!is.na(df[[j]]) & df[[j]] != ""), 0)
  bad <- colSums(!is.na(X)) < 0.5 * pmax(1, answered)
  if (any(bad)) stop_user(tr_("1 つ目の項目と選択肢が違う項目があります（"), paste(colnames(X)[bad], collapse = tr_("、")), tr_("）。同じ選択肢で答える項目だけを選んでください。"))
  rev <- unlist(s$reverse); if (length(rev)) for (j in which(it$cols %in% rev)) X[, j] <- min(PT[[j]]) + max(PT[[j]]) - X[, j]
  ok <- stats::complete.cases(X); X <- X[ok, , drop = FALSE]
  if (nrow(X) < it$k + 2) stop_user(tr_("すべての項目に答えた人が少なすぎます（"), nrow(X), tr_(" 人）。"))
  sdz <- apply(X, 2, stats::sd); if (any(sdz == 0)) stop_user(tr_("全員が同じ回答をした項目があります（"), paste(colnames(X)[sdz == 0], collapse = tr_("、")), tr_("）。その項目を外してください。"))
  list(X = X, it = it, nrev = sum(it$cols %in% rev), revnames = colnames(X)[it$cols %in% rev], pts = PT)
}
cron_alpha <- function(X) { k <- ncol(X); k / (k - 1) * (1 - sum(apply(X, 2, stats::var)) / stats::var(rowSums(X))) }
an_alpha <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); sm <- scale_matrix(df, req$slots$items, 2); X <- sm$X; k <- ncol(X); n <- nrow(X)
  a <- cron_alpha(X); R <- stats::cor(X); rb <- mean(R[upper.tri(R)]); astd <- k * rb / (1 + (k - 1) * rb)
  tot <- rowSums(X)
  itc <- vapply(seq_len(k), function(j) stats::cor(X[, j], tot - X[, j]), 0)
  adel <- if (k > 2) vapply(seq_len(k), function(j) cron_alpha(X[, -j, drop = FALSE]), 0) else rep(NA, k)
  chk <- list(alpha = a, astd = astd, itc = itc, adel = adel)
  # ω 係数（1 因子の最尤法の因子分析の負荷量から。3 項目以上のとき）
  om <- NA_real_
  if (k >= 3) { fa1 <- tryCatch(suppressWarnings(stats::factanal(covmat = R, factors = 1, n.obs = n)), error = function(e) NULL)
    if (!is.null(fa1)) { lam <- as.numeric(fa1$loadings[, 1]); if (sum(lam) < 0) lam <- -lam; om <- sum(lam)^2 / (sum(lam)^2 + sum(fa1$uniquenesses)) } }
  # 天井効果・床効果：平均 ± 標準偏差が、選択肢の得点の範囲（既定は 1〜段階の数）を超える項目
  pmin_ <- min(unlist(sm$pts)); pmax_ <- max(unlist(sm$pts)); mu <- colMeans(X); sdv <- apply(X, 2, stats::sd)
  cf <- ifelse(mu + sdv > pmax_, tr_("天井効果"), ifelse(mu - sdv < pmin_, tr_("床効果"), ""))
  res <- list(test = tr_("尺度の信頼性（α 係数・ω 係数）"), n = n_info(nrow(df), n))
  res$grids <- list(list(kind = "grid", title = tr_("項目ごとの統計量（項目分析）"), corner = tr_("項目"), rows = A(colnames(X)), cols = A(c(tr_("平均"), tr_("標準偏差"), tr_("天井・床効果"), tr_("修正済み項目−合計相関"), tr_("その項目を除いたときの α"))),
                         fmt = A(c("num2", "num2", "text", "num2", "num2")), cells = lapply(seq_len(k), function(j) list(mu[j], sdv[j], cf[j], itc[j], adel[j])),
                         mark = lapply(seq_len(k), function(j) A(c(FALSE, FALSE, nzchar(cf[j]), itc[j] < 0.3, !is.na(adel[j]) && adel[j] > a))),
                         note = trf("得点は %1$s〜%2$s 点。天井効果は平均 + 標準偏差が %2$s を超える項目、床効果は平均 − 標準偏差が %1$s を下回る項目。修正済み項目−合計相関は、その項目と残りの項目の合計点との相関。0.3 未満、または除くと α が上がる項目を太字にした。", fmt_pts(pmin_), fmt_pts(pmax_))),
                    list(kind = "grid", title = tr_("尺度全体"), corner = "", rows = A(tr_("合計点")), cols = A(c(tr_("項目数"), "n", "α", tr_("標準化 α"), "ω", tr_("項目間相関の平均"), tr_("合計点の平均"), tr_("合計点の標準偏差"))),
                         fmt = A(c("int", "int", "num2", "num2", "num2", "num2", "num2", "num2")), cells = list(list(k, n, a, astd, if (is.na(om)) NULL else om, rb, mean(tot), stats::sd(tot)))))
  res$headline <- c(list(hl("α", fmt_num(a))), if (!is.na(om)) list(hl("ω", fmt_num(om))), list(hl(if (identical(LANG, "en")) "Items" else "項目", as.character(k)), hl("n", as.character(n))))
  res$row <- list(stat = paste0("α = ", fmt_num(a)), effect = "")
  lvl <- if (a >= 0.9) tr_("非常に高い") else if (a >= 0.8) tr_("高い") else if (a >= 0.7) tr_("許容できる") else if (a >= 0.6) tr_("やや低い") else tr_("低い")
  low <- colnames(X)[itc < 0.3]; up <- colnames(X)[!is.na(adel) & adel > a]
  res$report <- paste0(trf("%d 項目（%s）の内的一貫性をクロンバックの α 係数で調べた。", k, paste(colnames(X), collapse = tr_("、"))),
                       trf("すべての項目に答えた %d 人で、α = %s（標準化 α = %s%s）であり、内的一貫性は%s水準であった。", n, fmt_num(a), fmt_num(astd), if (!is.na(om)) paste0(tr_("、ω = "), fmt_num(om)) else "", lvl),
                       if (any(nzchar(cf))) trf("%sは、回答が選択肢の端に偏っていた。", paste(paste0(colnames(X)[nzchar(cf)], tr_("（"), cf[nzchar(cf)], tr_("）")), collapse = tr_("、"))) else "",
                       if (length(low)) trf("修正済み項目−合計相関が 0.3 未満の項目は%sであった。", paste(low, collapse = tr_("、"))) else "",
                       if (length(up)) trf("%sを除くと α が上がる。", paste(up, collapse = tr_("、"))) else "",
                       if (sm$nrev) trf("%sは逆転項目として得点を反転した。", paste(sm$revnames, collapse = tr_("、"))) else "",
                       if (is.null(req$slots$items$pointsList)) trf("得点は、選択肢に%sの側から %d〜1 点を付けた。", q(sm$it$lev[1]), length(sm$it$lev))
                       else if (length(unique(lapply(sm$pts, unname))) == 1) trf("得点は、選択肢に次の点を付けた（%s）。", points_text(sm$it$lev, sm$pts[[1]]))
                       else trf("得点は、項目ごとに次の点を付けた（%s）。", paste(paste0(colnames(X), tr_("："), vapply(sm$pts, function(p) points_text(sm$it$lev, p), "")), collapse = tr_("；"))))
  res$notes <- c(tr_("α の目安は 0.7 以上（0.8 以上なら高い）とされることが多いですが、項目数が多いほど高くなりやすい点に注意してください。"),
                 if (is.na(om)) tr_("ω 係数は、項目が 3 つ以上のときに計算します。") else tr_("ω 係数は、1 因子の因子分析（最尤法）の負荷量から求めた値です（項目ごとに負荷量の大きさが違ってもよい点で、α より前提が緩い）。"),
                 tr_("逆の向きの項目（逆転項目）は、作業ウィンドウの「逆転項目」で指定すると、得点を反転して計算します。"))
  # α が負：多くは逆転項目の指定漏れ（項目間の相関が負）
  if (a < 0) {
    neg <- colnames(X)[itc < 0]
    res$notes <- c(paste0(tr_("α が負の値です。項目どうしの相関が負になっていて、多くは逆の向きの項目（逆転項目）を指定していないことが原因です。"),
                          if (length(neg)) trf("残りの項目の合計と負の相関を持つ項目（%s）が逆転項目でないか、質問文を確かめてください。", paste(neg, collapse = tr_("、"))) else ""), res$notes)
    res$report <- paste0(res$report, tr_("なお、α は負の値であり、逆転項目の扱いを確認する必要がある。"))
  }
  res$chk <- chk
  finish(res, alpha)
}
register("alpha", tr_("尺度の信頼性（α 係数・ω 係数）"), tr_("尺度・多変量"), tr_("同じ内容を測る複数の段階の質問（尺度）について、まとまりの良さ（内的一貫性）を α 係数と ω 係数で調べ、項目ごとの平均・天井効果・床効果・項目と合計の相関などの問題も示します。"),
         list(S("items", tr_("尺度の項目（同じ選択肢）"), "ordinal", tr_("例：海への関心の 8 項目"), FALSE, TRUE)), list(), an_alpha)

# ---------------------------------------------------------------- 探索的因子分析
an_factor <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); sm <- scale_matrix(df, req$slots$items, 3); X <- sm$X; p <- ncol(X); n <- nrow(X)
  R <- stats::cor(X); ev <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  fmax <- max(1, max(which(vapply(1:p, function(f) ((p - f)^2 - (p + f)) / 2 >= 0, TRUE))))
  nf0 <- opt(req, "nf", "auto"); rot <- opt(req, "rotation", "promax")
  nf <- if (identical(nf0, "auto")) max(1, sum(ev > 1)) else as.integer(nf0)
  if (nf > fmax) { if (identical(nf0, "auto")) nf <- fmax else stop_user(p, tr_(" 項目で求められる因子は "), fmax, tr_(" 個までです。項目を増やすか、因子の数を減らしてください。")) }
  fa <- tryCatch(stats::factanal(covmat = R, factors = nf, n.obs = n, rotation = if (nf > 1) rot else "none"),
                 error = function(e) stop_user(tr_("因子分析の計算が収束しませんでした（"), conditionMessage(e), tr_("）。因子の数を変えるか、項目を見直してください。")))
  L <- unclass(fa$loadings); L <- L[, seq_len(nf), drop = FALSE]
  sgn <- ifelse(colSums(L) < 0, -1, 1); L <- sweep(L, 2, sgn, "*")      # 負荷量の合計が正になるよう因子の向きをそろえる
  prim <- apply(abs(L), 1, which.max); o <- order(prim, -abs(L[cbind(seq_len(p), prim)])); L <- L[o, , drop = FALSE]
  h2 <- 1 - fa$uniquenesses[o]
  fn <- trf("第%d因子", seq_len(nf))
  res <- list(test = paste0(tr_("探索的因子分析（最尤法"), if (nf > 1) paste0(tr_("、"), if (rot == "promax") tr_("プロマックス回転") else tr_("バリマックス回転")) else "", tr_("）")), n = n_info(nrow(df), n))
  res$grids <- list(list(kind = "grid", title = tr_("因子負荷量"), corner = tr_("項目"), rows = A(rownames(L)), cols = A(c(fn, tr_("共通性"))), fmt = A(rep("num2", nf + 1)), cells = mat(cbind(L, h2)),
                         mark = lapply(seq_len(p), function(i) A(c(abs(L[i, ]) >= 0.4, FALSE))), note = tr_("絶対値が 0.40 以上の負荷量を太字にした。項目は最も大きく負荷する因子ごとに並べた。")),
                    list(kind = "grid", title = tr_("相関行列の固有値"), corner = "", rows = A(paste0(seq_len(min(p, 10)))), cols = A(c(tr_("固有値"), tr_("累積寄与率 (%)"))), fmt = A(c("num2", "num1")),
                         cells = mat(cbind(ev[seq_len(min(p, 10))], 100 * cumsum(ev)[seq_len(min(p, 10))] / p)), note = tr_("固有値が 1 を超える数（カイザー基準）を、因子の数の目安とした（「自動」のとき）。")))
  if (nf > 1 && rot == "promax" && !is.null(fa$rotmat)) {
    # 因子間相関：回転前の負荷量 L0 と回転後の負荷量 L から、L0 L0' = L Φ L' を解いて求める
    #   （factanal の rotmat は、返す負荷量と向きがそろっていないことがあるため使わない）
    L0 <- unclass(stats::factanal(covmat = R, factors = nf, n.obs = n, rotation = "none")$loadings)[o, , drop = FALSE]
    Lp <- solve(crossprod(L), t(L)); Phi <- Lp %*% L0 %*% t(L0) %*% t(Lp); Phi <- (Phi + t(Phi)) / 2; Phi <- Phi / sqrt(outer(diag(Phi), diag(Phi)))
    res$grids <- c(res$grids, list(list(kind = "grid", title = tr_("因子間相関"), corner = "", rows = A(fn), cols = A(fn), fmt = A(rep("num2", nf)), cells = mat(Phi))))
  }
  pv <- if (rot == "promax" && nf > 1) NULL else colSums(L^2) / p
  res$headline <- list(hl(tr_("因子"), as.character(nf)), hl(if (identical(LANG, "en")) "Items" else "項目", as.character(p)), hl("n", as.character(n)))
  res$row <- list(stat = "", effect = "")
  grp <- vapply(seq_len(nf), function(j) { it <- rownames(L)[apply(abs(L), 1, which.max) == j & apply(abs(L), 1, max) >= 0.4]; if (length(it)) trf("%sには%sが高く負荷した", fn[j], paste(it, collapse = tr_("、"))) else trf("%sに高く負荷する項目はなかった", fn[j]) }, "")
  weak <- rownames(L)[apply(abs(L), 1, max) < 0.4]
  res$report <- paste0(trf("%d 項目について、すべての項目に答えた %d 人のデータで探索的因子分析（最尤法%s）を行った。", p, n, if (nf > 1) paste0(tr_("、"), if (rot == "promax") tr_("プロマックス回転") else tr_("バリマックス回転")) else ""),
                       if (identical(nf0, "auto")) trf("相関行列の固有値が 1 を超える数から因子の数を %d とした。", nf) else trf("因子の数を %d とした。", nf),
                       paste(grp, collapse = tr_("。")), tr_("。"), if (length(weak)) trf("どの因子にも 0.40 以上負荷しなかった項目は%sであった。", paste(weak, collapse = tr_("、"))) else "",
                       if (!is.null(pv)) trf("因子の寄与率は%sであった。", paste0(fn, " ", fmt_num(100 * pv, 1), "%", collapse = tr_("、"))) else "")
  # 共通性が 1 に近い（0.99 以上）項目は、推定が不安定な状態（ヘイウッドケース）の可能性がある
  hey <- rownames(L)[h2 >= 0.99]
  heyNote <- if (length(hey)) trf("共通性が 1 に近い項目（%s）があります。推定が不安定になっている（ヘイウッドケース）可能性が高く、その項目の負荷量は信頼できません。因子の数を減らすか、その項目を除いて計算し直してください。", paste(hey, collapse = tr_("、"))) else NULL
  if (length(hey)) res$report <- paste0(res$report, trf("ただし、%sの共通性が 1 に近く（ヘイウッドケース）、解は不安定である可能性がある。", paste(hey, collapse = tr_("、"))))
  res$notes <- c(heyNote, tr_("段階の得点のピアソン相関行列から、最尤法で因子を求めています。因子の数は、固有値・解釈のしやすさ・先行研究を合わせて決めてください。"),
                 tr_("因子に名前を付けるときは、高く負荷した項目の内容を読んで判断します。尺度として使う項目が決まったら、クロンバックの α で信頼性を確かめてください。"))
  finish(res, alpha)
}
register("factor", tr_("探索的因子分析"), tr_("尺度・多変量"), tr_("多くの段階の質問の背後にある共通の要因（因子）を探します。質問紙の尺度づくりに使います。"),
         list(S("items", tr_("分析する項目（同じ選択肢）"), "ordinal", tr_("例：海への関心の項目すべて"), FALSE, TRUE)),
         list(O_select("nf", tr_("因子の数"), list(auto = tr_("自動（固有値が 1 を超える数）"), "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5"), "auto"),
              O_select("rotation", tr_("回転"), list(promax = tr_("プロマックス（因子間の相関を認める）"), varimax = tr_("バリマックス（因子を独立とする）")), "promax",
                       tr_("心理・教育の尺度では、因子どうしに相関があると考えるプロマックス回転がよく使われます。"))), an_factor)

# ---------------------------------------------------------------- コレスポンデンス分析
an_ca <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); sa <- req$slots$a; sb <- req$slots$b
  a <- slot_factor(df, sa); b <- slot_factor(df, sb); ok <- !is.na(a) & !is.na(b)
  N <- table(droplevels(a[ok]), droplevels(b[ok])); N <- N[rowSums(N) > 0, colSums(N) > 0, drop = FALSE]
  if (nrow(N) < 3 || ncol(N) < 3) stop_user(tr_("コレスポンデンス分析には、行・列とも 3 つ以上の選択肢が必要です（現在 "), nrow(N), " × ", ncol(N), tr_("）。"))
  n <- sum(N); P <- N / n; r <- rowSums(P); cc <- colSums(P)
  Sm <- diag(1 / sqrt(r)) %*% (P - r %o% cc) %*% diag(1 / sqrt(cc))
  sv <- svd(Sm); K <- min(nrow(N), ncol(N)) - 1; d <- sv$d[seq_len(K)]; inert <- d^2; share <- inert / sum(inert)
  Fr <- diag(1 / sqrt(r)) %*% sv$u[, 1:2] %*% diag(d[1:2]); Gc <- diag(1 / sqrt(cc)) %*% sv$v[, 1:2] %*% diag(d[1:2])
  for (j in 1:2) if (sum(Fr[, j] * r) == 0 && Fr[which.max(abs(Fr[, j])), j] < 0) { Fr[, j] <- -Fr[, j]; Gc[, j] <- -Gc[, j] }
  chi <- suppressWarnings(stats::chisq.test(N, correct = FALSE))
  res <- list(test = tr_("コレスポンデンス分析"), p = unname(chi$p.value), n = n_info(nrow(df), n))
  res$xtab <- xtab_obj(N, sa$label, sb$label, resid = FALSE)
  res$grids <- list(list(kind = "grid", title = tr_("次元ごとの寄与率"), corner = tr_("次元"), rows = A(paste0(seq_len(K))), cols = A(c(tr_("特異値"), tr_("慣性（固有値）"), tr_("寄与率 (%)"), tr_("累積寄与率 (%)"))),
                         fmt = A(c("num2", "num2", "num1", "num1")), cells = mat(cbind(d, inert, 100 * share, 100 * cumsum(share)))),
                    list(kind = "grid", title = tr_("座標（第1・第2次元）"), corner = "", rows = A(c(paste0(sa$label, tr_("："), rownames(N)), paste0(sb$label, tr_("："), colnames(N)))),
                         cols = A(c(tr_("第1次元"), tr_("第2次元"))), fmt = A(c("num2", "num2")), cells = mat(rbind(Fr, Gc)), note = tr_("行と列の両方を主座標で示した（対称マップ）。")))
  res$fig <- list(type = "map", pts = list(list(name = sa$label, labels = A(rownames(N)), x = A(Fr[, 1]), y = A(Fr[, 2])), list(name = sb$label, labels = A(colnames(N)), x = A(Gc[, 1]), y = A(Gc[, 2]))),
                  xTitle = paste0(tr_("第1次元（"), fmt_num(100 * share[1], 1), tr_("%）")), yTitle = paste0(tr_("第2次元（"), fmt_num(100 * share[2], 1), tr_("%）")))
  res$headline <- list(hl(tr_("第1・第2次元の累積寄与率"), paste0(fmt_num(100 * sum(share[1:2]), 1), "%")), hl("n", as.character(n)))
  res$row <- list(stat = paste0("χ²(", unname(chi$parameter), ") = ", fmt_num(unname(chi$statistic))), effect = "")
  near <- function(i) { dd <- sqrt((Gc[, 1] - Fr[i, 1])^2 + (Gc[, 2] - Fr[i, 2])^2); colnames(N)[which.min(dd)] }
  res$report <- paste0(trf("%sと%sのクロス集計表（n = %d）にコレスポンデンス分析を行った。", q(sa$label), q(sb$label), n),
                       trf("第1次元と第2次元の寄与率はそれぞれ %s%%、%s%%で、2 つの次元で全体の %s%% を説明した。", fmt_num(100 * share[1], 1), fmt_num(100 * share[2], 1), fmt_num(100 * sum(share[1:2]), 1)),
                       trf("図上で近くに布置された組み合わせは、%sであった。", paste(trf("%sと%s", rownames(N), vapply(seq_len(nrow(N)), near, "")), collapse = tr_("、"))))
  res$notes <- c(tr_("図上の距離は目安です。行どうし・列どうしの近さは似た回答のしかたを示しますが、行と列の点の距離は厳密な意味を持たないため、元のクロス集計表と合わせて解釈してください。"),
                 trf("p 値は、表全体の独立性のカイ二乗検定（χ²(%s) = %s）によります。", unname(chi$parameter), fmt_num(unname(chi$statistic))))
  finish(res, alpha)
}
register("ca", tr_("コレスポンデンス分析"), tr_("尺度・多変量"), tr_("選択肢の多い 2 つの質問のクロス集計表を、似た回答をする選択肢どうしが近くに来る 1 枚の図（布置図）にまとめます。"),
         list(S("a", tr_("質問 1（行）"), cat_types, tr_("例：好きな食べ物")), S("b", tr_("質問 2（列）"), cat_types, tr_("例：学年、部活動"))), list(), an_ca)

# ---------------------------------------------------------------- 回答日時の推移
# 日付の計算は、webR でも時刻帯のデータを読まずに済むよう、整数の暦計算で行う（1970-01-01 からの日数）
days_from_civil <- function(y, m, d) { y <- y - (m <= 2); era <- floor(y / 400); yoe <- y - era * 400; mp <- (m + 9) %% 12
  doy <- floor((153 * mp + 2) / 5) + d - 1; doe <- yoe * 365 + floor(yoe / 4) - floor(yoe / 100) + doy; era * 146097 + doe - 719468 }
civil_from_days <- function(z) { z <- z + 719468; era <- floor(z / 146097); doe <- z - era * 146097; yoe <- floor((doe - floor(doe / 1460) + floor(doe / 36524) - floor(doe / 146096)) / 365)
  y <- yoe + era * 400; doy <- doe - (365 * yoe + floor(yoe / 4) - floor(yoe / 100)); mp <- floor((5 * doy + 2) / 153); d <- doy - floor((153 * mp + 2) / 5) + 1
  m <- mp + ifelse(mp < 10, 3, -9); cbind(y = y + (m <= 2), m = m, d = d) }
fmt_day <- function(z, jp = FALSE) { c <- civil_from_days(z); if (jp) sprintf(tr_("%d年%d月%d日"), c[, 1], c[, 2], c[, 3]) else sprintf("%d/%02d/%02d", c[, 1], c[, 2], c[, 3]) }
an_timeline <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); s <- req$slots$t; x <- as.character(df[[s$col]])
  tm <- time_minutes(x); okm <- !is.na(tm)
  if (!any(okm)) stop_user(q(s$label), tr_("から日付を読み取れませんでした。"))
  z <- floor(tm[okm] / 1440); mm <- civil_from_days(z)
  per <- opt(req, "per", "day")
  if (per == "week") { zk <- z - ((z + 3) %% 7); all <- seq(min(zk), max(zk), by = 7); key <- match(zk, all); lab <- trf("%s 週", fmt_day(all)) }
  else if (per == "month") { mk <- mm[, 1] * 12 + mm[, 2] - 1; all <- seq(min(mk), max(mk)); key <- match(mk, all); lab <- sprintf("%d/%02d", all %/% 12, all %% 12 + 1) }
  else { all <- seq(min(z), max(z)); key <- match(z, all); lab <- fmt_day(all) }
  cnt <- tabulate(key, nbins = length(all)); n <- sum(cnt)
  res <- list(test = tr_("回答日時の推移"), n = n_info(nrow(df), n))
  res$grids <- list(list(kind = "grid", title = tr_("期間ごとの回答数"), corner = c(day = tr_("日付"), week = tr_("週（月曜始まり）"), month = tr_("月"))[[per]], rows = A(lab), cols = A(c(tr_("回答数"), tr_("累計"), tr_("累計の割合 (%)"))),
                         fmt = A(c("int", "int", "pct")), cells = mat(cbind(cnt, cumsum(cnt), cumsum(cnt) / n))))
  res$fig <- list(type = "freq", cats = A(lab), series = A(tr_("回答数")), count = mat(cnt), base = n, catTitle = c(day = tr_("日付"), week = tr_("週"), month = tr_("月"))[[per]], time = TRUE, prefer = "line")
  pk <- which.max(cnt); half <- lab[which(cumsum(cnt) >= n / 2)[1]]
  res$headline <- list(hl(tr_("回答"), as.character(n)), hl(tr_("期間"), paste0(lab[1], tr_("〜"), lab[length(lab)])), hl(tr_("最多"), paste0(lab[pk], tr_("（"), cnt[pk], tr_(" 件）"))))
  res$row <- list(stat = "", effect = "")
  res$report <- paste0(trf("回答（n = %d）は %sから %sまでに寄せられた。", n, fmt_day(min(z), TRUE), fmt_day(max(z), TRUE)),
                       trf("回答が最も多かったのは%s（%d 件）で、%sの時点で回答の半数に達した。", lab[pk], cnt[pk], half))
  finish(res, alpha)
}
register("timeline", tr_("回答日時の推移"), tr_("集計"), tr_("タイムスタンプから、日・週・月ごとの回答数と累計を出します。回答の集まり方を確かめるときに使います。"),
         list(S("t", tr_("日時の列"), "time", tr_("Google フォームのタイムスタンプ"))),
         list(O_select("per", tr_("まとめる単位"), list(day = tr_("日"), week = tr_("週"), month = tr_("月")), "day")), an_timeline)

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
  if (is.null(req$docs)) stop_user(tr_("単語に分けた結果がありません。"))
  has <- lengths(docs) > 0; nd <- sum(has)
  if (nd < 3) stop_user(tr_("単語を取り出せた回答が 3 件未満です（"), nd, tr_(" 件）。"))
  docs <- docs[has]; if (!is.null(grp)) grp <- grp[has]
  topN <- as.integer(opt(req, "top", "60")); minf <- max(1L, as.integer(opt(req, "minf", "2")))
  tf <- table(unlist(docs)); dfq <- table(unlist(lapply(docs, unique)))
  w <- names(dfq)[order(-as.integer(dfq), -as.integer(tf[names(dfq)]))]; w <- w[as.integer(dfq[w]) >= minf]
  if (!length(w)) stop_user(minf, tr_(" 件以上の回答に出てくる語がありません。「語の最低出現数」を下げてください。"))
  w <- utils::head(w, topN); Encoding(w) <- "UTF-8"
  dfw <- as.integer(dfq[w]); tfw <- as.integer(tf[w])
  res <- list(test = tr_("テキストマイニング（頻出語と共起）"), n = n_info(nrow(df), nd))
  res$grids <- list(list(kind = "grid", title = tr_("頻出語"), corner = tr_("語"), rows = A(w), cols = A(c(tr_("出現した回答数"), tr_("回答に占める割合 (%)"), tr_("出現回数"))), fmt = A(c("int", "pct", "int")),
                         cells = mat(cbind(dfw, dfw / nd, tfw)), note = trf("単語を取り出せた %d 件の回答で数えた。「出現した回答数」は、1 つの回答に何回出ても 1 と数える。", nd)))
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
  txt <- paste0(trf("%sの自由記述（単語を取り出せた %d 件）を単語に分けて集計した。", q(st$label), nd),
                trf("多く出現した語は、%sであった。", paste0(q(top), tr_("（"), utils::head(dfw, 5), tr_(" 件）"), collapse = tr_("、"))))
  if (nE) {
    o <- order(-Wt)[seq_len(min(3, nE))]
    txt <- paste0(txt, trf("同じ回答の中で一緒に使われやすい語の組み合わせ（Jaccard 係数が高い組）は、%sであった。", paste0(trf("%sと%s", q(w[ed[o, 1]]), q(w[ed[o, 2]])), tr_("（"), fmt_num(Wt[o]), tr_("）"), collapse = tr_("、"))))
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
      res$grids <- c(res$grids, list(list(kind = "grid", title = tr_("群ごとの語の使用割合 (%)"), corner = tr_("語"), rows = A(w),
                                          cols = A(c(paste0(L, tr_("（n = "), ng, tr_("）")), tr_("補正後の p"))), fmt = A(c(rep("pct", length(L)), "p")), cells = mat(cbind(pr, pa)),
                                          mark = lapply(seq_along(w), function(i) A(c(if (!is.na(pa[i]) && pa[i] < alpha) pr[i, ] == max(pr[i, ]) else rep(FALSE, length(L)), FALSE))),
                                          note = tr_("語ごとに、使った回答の割合を群で比べた（カイ二乗検定。p は語の数でホルム法により補正）。有意な語は、割合が最も高い群を太字にした。"))))
      sigw <- which(!is.na(pa) & pa < alpha)
      txt <- paste0(txt, trf("%sで比べると、%s", q(sg$label), if (length(sigw)) trf("%sで多く使われていた（ホルム法で補正した p < %s）。", paste(trf("%sは%s", q(w[sigw]), L[apply(pr[sigw, , drop = FALSE], 1, which.max)]), collapse = tr_("、")), alpha)
                                                         else tr_("群の間で使用の割合に有意な差のある語はなかった。")))
      res$fig$groups <- list(names = A(L), share = mat(pr))
    }
  }
  res$headline <- list(hl(tr_("回答"), as.character(nd)), hl(tr_("語の種類"), as.character(length(dfq))), hl(tr_("最多"), paste0(w[1], tr_("（"), dfw[1], tr_("）"))))
  res$row <- list(stat = "", effect = "")
  res$report <- txt
  # 報告書の「方法」：単語の分け方、除いた語、数え方、対象にした語の条件、共起とグループ比較の方法を、実際の設定で書く
  ans <- { v <- as.character(df[[st$col]]); sum(!is.na(v) & nzchar(trimws(v))) }
  us <- unlist(req$stopUser); cmp <- !isFALSE(opt(req, "compound", TRUE)); emax <- as.integer(opt(req, "edges", "60"))
  m <- paste0(trf("%sの自由記述 %d 件のうち、語を取り出せた %d 件を分析した。", q(st$label), ans, nd),
              tr_("文字を Unicode の NFKC で正規化し、英字を小文字にそろえたうえで、"),
              if (identical(req$seg, "chartype")) tr_("文字の種類（漢字・カタカナ・ひらがな・英数字）の切れ目で語に分けた（使用したブラウザが日本語の単語分割に対応していなかったため）。")
              else tr_("ブラウザに組み込まれた日本語の単語分割（Intl.Segmenter、ICU の辞書による）で語に分けた。"),
              if (cmp) tr_("漢字・カタカナの語が続く場合は、つなげて 1 語とした（例：自然＋環境 → 自然環境）。") else "",
              trf("助詞・助動詞や「こと」「思う」のような意味の薄い語（あらかじめ用意した %s一覧）、数字だけの語、2 文字以下のひらがなの語、漢字以外の 1 文字の語を除いた。", if (is.null(req$nstop)) "" else trf("%s 語の", req$nstop)),
              if (length(us)) trf("さらに、%sを除いた。", paste0(q(us), collapse = tr_("、"))) else "",
              trf("語の出現は、1 つの回答に何回出ても 1 回と数え、%s 件以上の回答に出た語のうち、出現した回答数の多い順に上位 %s 語（同数のときは出現回数の多い順）を分析の対象とした（今回は %d 語）。", minf, topN, length(w)),
              trf("2 語の結び付きの強さは、両方が出た回答数を、どちらか一方でも出た回答数で割った Jaccard 係数で表した。両方が出た回答が 2 件以上の組のうち、係数の大きい組から最大 %s 組（今回は %s 組）を共起ネットワークの線とした。語の配置にはフルクターマン・ラインゴールド法を、まとまりの色分けにはラベル伝播法を用いた。", emax, nE))
  if (ngrp >= 2)
    m <- paste0(m, trf("%sによる違いは、語ごとに、その語を使った回答と使わなかった回答の数を群で比べるカイ二乗検定（2 × %d の表）で調べ、p 値は分析した語の数（%d 語）でホルム法により補正した。有意水準は %s とした。", q(sg$label), ngrp, length(w), alpha))
  res$method <- m
  res$notes <- c(tr_("単語の分割には、ブラウザに組み込まれた日本語の分かち書き（Intl.Segmenter）を使い、助詞や「思う」「こと」のような意味の薄い語を除いています。"),
                 tr_("図は「ワードクラウド」「頻出語の棒グラフ」「共起ネットワーク」を切り替えられます。共起ネットワークの線は、同じ回答に一緒に出る語どうしを結び、色は結び付きの強いまとまりを表します。"))
  finish(res, alpha)
}
register("textmine", tr_("テキストマイニング（自由記述）"), tr_("自由記述"), tr_("自由記述の回答を単語に分け、よく出てくる語、一緒に使われる語の組み合わせ（共起ネットワーク）、グループごとに多く使われる語を調べます。"),
         list(S("t", tr_("自由記述の質問"), c("text", "nominal"), tr_("例：感想、意見")), S("g", tr_("グループ（任意）"), cat_types, tr_("選ぶとグループごとに多く使われる語を比べます"), TRUE, optional = TRUE)),
         list(O_select("top", tr_("取り出す語の数"), list("30" = tr_("30 語"), "60" = tr_("60 語"), "100" = tr_("100 語")), "60"),
              O_select("minf", tr_("語の最低出現数（回答数）"), list("1" = "1", "2" = "2", "3" = "3", "5" = "5", "10" = "10"), "2", tr_("この数より少ない回答にしか出てこない語は除きます。")),
              O_select("edges", tr_("共起ネットワークの線の数"), list("30" = tr_("30 本"), "60" = tr_("60 本"), "100" = tr_("100 本")), "60"),
              O_check("compound", tr_("漢字・カタカナの続く語をつなげる（例：自然＋環境 → 自然環境）"), TRUE),
              list(id = "stop", kind = "text", label = tr_("除く語（読点「、」で区切る）"), default = "", hint = tr_("集計から外したい語を書きます。例：特に、なし、自然"))), an_textmine)

# ---------------------------------------------------------------- 確認的因子分析（最尤法。基本の R だけで計算する）
# 項目の得点（逆転項目は反転）を、欠損を残したまま返す
scale_raw <- function(df, s) {
  it <- items_setup(df, s)
  PT <- lapply(seq_along(it$F), function(j) lev_points(s, it$lev, j))
  X <- sapply(seq_along(it$F), function(j) PT[[j]][as.integer(it$F[[j]])]); if (!is.matrix(X)) X <- matrix(X, ncol = length(it$F)); colnames(X) <- it$names
  rev <- unlist(s$reverse); if (length(rev)) for (j in which(it$cols %in% rev)) X[, j] <- min(PT[[j]]) + max(PT[[j]]) - X[, j]
  list(X = X, it = it, revnames = colnames(X)[it$cols %in% rev])
}
# 各項目は 1 つの因子だけに負荷する。因子の分散は 1 に固定し、因子間の相関を推定する
cfa_fit <- function(S, n, grp) {
  p <- nrow(S); m <- max(grp); np <- m * (m - 1) / 2; ldS <- determinant(S, logarithm = TRUE)$modulus
  lw <- which(lower.tri(diag(m)), arr.ind = TRUE)
  unpack <- function(par) {
    lam <- par[1:p]; th <- exp(par[p + 1:p]); Ph <- diag(m)
    if (np) { r <- tanh(par[2 * p + seq_len(np)]); Ph[lw] <- r; Ph[lw[, 2:1, drop = FALSE]] <- r }
    L <- matrix(0, p, m); L[cbind(1:p, grp)] <- lam
    list(lam = lam, th = th, Ph = Ph, L = L, Sig = L %*% Ph %*% t(L) + diag(th, p))
  }
  fn <- function(par) { u <- unpack(par); ch <- tryCatch(chol(u$Sig), error = function(e) NULL); if (is.null(ch)) return(1e10)
    Si <- chol2inv(ch); 2 * sum(log(diag(ch))) + sum(S * Si) - ldS - p }
  gr <- function(par) {
    u <- unpack(par); ch <- tryCatch(chol(u$Sig), error = function(e) NULL); if (is.null(ch)) return(rep(0, length(par)))
    Si <- chol2inv(ch); G <- Si - Si %*% S %*% Si
    GL <- 2 * G %*% u$L %*% u$Ph; g <- c(GL[cbind(1:p, grp)], diag(G) * u$th)
    if (np) { M <- t(u$L) %*% G %*% u$L; g <- c(g, 2 * M[lw] * (1 - u$Ph[lw]^2)) }
    g
  }
  start <- c(sqrt(diag(S)) * 0.7, log(diag(S) * 0.5), rep(atanh(0.3), np))
  o <- stats::optim(start, fn, gr, method = "BFGS", control = list(maxit = 2000, reltol = 1e-12))
  u <- unpack(o$par)
  H <- tryCatch(stats::optimHess(o$par, fn, gr), error = function(e) NULL)
  V <- if (!is.null(H)) tryCatch(solve(H) * 2 / n, error = function(e) NULL) else NULL
  se <- if (!is.null(V)) sqrt(pmax(diag(V)[1:p], 0)) else rep(NA, p)
  list(F = o$value, conv = o$convergence == 0, lam = u$lam, th = u$th, Ph = u$Ph, Sig = u$Sig, se = se, np = 2 * p + np)
}
an_cfa <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE)
  ids <- c("f1", "f2", "f3", "f4"); used <- Filter(function(id) !is.null(req$slots[[id]]) && length(unlist(req$slots[[id]]$cols)), ids)
  if (!length(used)) stop_user(tr_("因子 1 の項目を選んでください。"))
  parts <- lapply(used, function(id) scale_raw(df, req$slots[[id]]))
  for (k in seq_along(parts)) if (ncol(parts[[k]]$X) < 2) stop_user(tr_("因子 "), k, tr_(" の項目が 1 つしかありません。1 つの因子に 2 つ以上（因子が 1 つだけのときは 3 つ以上）の項目を選んでください。"))
  X <- do.call(cbind, lapply(parts, function(z) z$X)); grp <- rep(seq_along(parts), vapply(parts, function(z) ncol(z$X), 1L))
  if (anyDuplicated(colnames(X))) stop_user(tr_("同じ項目が 2 つの因子に入っています。1 つの項目は 1 つの因子にだけ入れてください。"))
  m <- length(parts); p <- ncol(X)
  if (m == 1 && p < 3) stop_user(tr_("因子が 1 つのときは、項目を 3 つ以上選んでください（2 項目では当てはまりを調べられません）。"))
  ok <- stats::complete.cases(X); X <- X[ok, , drop = FALSE]; n <- nrow(X)
  if (n < p + 10) stop_user(tr_("すべての項目に答えた人が少なすぎます（"), n, tr_(" 人）。"))
  sdz <- apply(X, 2, stats::sd); if (any(sdz == 0)) stop_user(tr_("全員が同じ回答をした項目があります（"), paste(colnames(X)[sdz == 0], collapse = tr_("、")), tr_("）。"))
  S <- stats::cov(X) * (n - 1) / n
  fit <- cfa_fit(S, n, grp)
  dfm <- p * (p + 1) / 2 - fit$np; chi <- n * fit$F; pchi <- if (dfm > 0) stats::pchisq(chi, dfm, lower.tail = FALSE) else NA
  chib <- n * (sum(log(diag(S))) - determinant(S, logarithm = TRUE)$modulus); dfb <- p * (p - 1) / 2
  ident <- dfm > 0                                                    # 自由度 0（例：1 因子 3 項目）では当てはまりを評価できない
  cfi <- if (ident) 1 - max(chi - dfm, 0) / max(chib - dfb, chi - dfm, 0) else NA
  tli <- if (ident) ((chib / dfb) - (chi / dfm)) / ((chib / dfb) - 1) else NA
  rmsea <- if (ident) sqrt(max((chi - dfm) / (n * dfm), 0)) else NA
  ncp_at <- function(prob) { if (stats::pchisq(chi, dfm) < prob) return(0); stats::uniroot(function(l) stats::pchisq(chi, dfm, ncp = l) - prob, c(0, max(chi * 3, 10)))$root }
  rl <- if (ident) sqrt(ncp_at(0.95) / (n * dfm)) else NA; rh <- if (ident) sqrt(ncp_at(0.05) / (n * dfm)) else NA
  D <- sqrt(diag(S)); E <- (S - fit$Sig) / outer(D, D); srmr <- sqrt(mean(E[upper.tri(E, diag = TRUE)]^2))
  std <- fit$lam / sqrt(diag(fit$Sig)); z <- fit$lam / fit$se; pz <- 2 * stats::pnorm(-abs(z))
  fac <- paste0(tr_("因子 "), seq_len(m))
  rel <- t(vapply(seq_len(m), function(f) { j <- grp == f; c(sum(j), sum(fit$lam[j])^2 / (sum(fit$lam[j])^2 + sum(fit$th[j])), mean(std[j]^2)) }, numeric(3)))
  res <- list(test = tr_("確認的因子分析（最尤法）"), n = n_info(nrow(df), n), notes = character(0))   # χ² の p は当てはまりの表にだけ載せる（「有意」の判定はしない）
  res$grids <- list(
    list(kind = "grid", title = tr_("当てはまりの指標"), corner = "", rows = A(tr_("モデル")), cols = A(c("χ²", tr_("自由度"), "p", "CFI", "TLI", "RMSEA", tr_("RMSEA の 90% 信頼区間の下限"), tr_("上限"), "SRMR")),
         fmt = A(c("num2", "int", "p", "num3", "num3", "num3", "num3", "num3", "num3")), cells = list(lapply(list(chi, dfm, pchi, cfi, tli, rmsea, rl, rh, srmr), function(v) if (is.na(v)) NULL else v)),
         note = tr_("目安（Hu & Bentler, 1999）：CFI・TLI は 0.95 以上、RMSEA は 0.06 以下、SRMR は 0.08 以下で当てはまりがよいとされる。χ² 検定は人数が多いとわずかなずれでも有意になる。")),
    list(kind = "grid", title = tr_("因子負荷量"), corner = tr_("項目"), rows = A(colnames(X)), cols = A(c(tr_("因子"), tr_("標準化負荷量"), tr_("負荷量"), tr_("標準誤差"), "z", "p")),
         fmt = A(c("text", "num2", "num2", "num2", "num2", "p")), cells = lapply(seq_len(p), function(i) list(fac[grp[i]], std[i], fit$lam[i], fit$se[i], z[i], pz[i])),
         mark = lapply(seq_len(p), function(i) A(c(FALSE, std[i] < 0.4, FALSE, FALSE, FALSE, FALSE))),
         note = tr_("標準化負荷量が 0.4 未満の項目を太字にした。")),
    list(kind = "grid", title = tr_("因子ごとの信頼性"), corner = "", rows = A(fac), cols = A(c(tr_("項目数"), tr_("ω（合成信頼性）"), "AVE")), fmt = A(c("int", "num2", "num2")),
         cells = lapply(seq_len(m), function(f) as.list(rel[f, ])), note = tr_("AVE は標準化負荷量の 2 乗の平均（平均分散抽出）。0.5 以上が目安とされる。")))
  if (m > 1) res$grids <- c(res$grids, list(list(kind = "grid", title = tr_("因子間の相関"), corner = "", rows = A(fac), cols = A(fac), fmt = A(rep("num2", m)), cells = mat(fit$Ph))))
  res$headline <- if (ident) list(hl("CFI", fmt_num(cfi, 3)), hl("RMSEA", fmt_num(rmsea, 3)), hl("SRMR", fmt_num(srmr, 3)), hl(paste0("χ²(", dfm, ")"), fmt_num(chi)), hl("n", as.character(n))) else list(hl(tr_("自由度"), "0"), hl("n", as.character(n)))
  res$row <- list(stat = paste0("χ²(", dfm, ") = ", fmt_num(chi)), effect = paste0("CFI = ", fmt_num(cfi, 3), ", RMSEA = ", fmt_num(rmsea, 3)))
  res$fig <- list(type = "box", cats = A(colnames(X)), box = lapply(seq_len(p), function(i) list(stats = A(rep(std[i], 5)), out = A(numeric(0)), n = n, mean = std[i], lo = NULL, hi = NULL)),
                  catTitle = tr_("項目"), valueTitle = tr_("標準化負荷量"), coef = tr_("標準化負荷量"), ref = 0)
  good <- ident && cfi >= 0.95 && rmsea <= 0.06 && srmr <= 0.08; poor <- ident && (cfi < 0.90 || rmsea > 0.10)
  if (!ident) res$notes <- c(res$notes, tr_("モデルの自由度が 0 です（1 因子 3 項目など）。データに必ず完全に当てはまるので、当てはまりの指標では評価できません。負荷量と信頼性だけを見てください。"))
  res$report <- paste0(trf("%d 因子 %d 項目のモデルについて、最尤法による確認的因子分析を行った（n = %d）。", m, p, n),
                       if (!ident) tr_("モデルの自由度が 0 のため、当てはまりは評価できない。")
                       else trf("当てはまりは χ²(%s) = %s, %s、CFI = %s、TLI = %s、RMSEA = %s（90%%CI [%s, %s]）、SRMR = %sであり、%s", dfm, fmt_num(chi), fmt_p(pchi, lead0), fmt_num(cfi, 3), fmt_num(tli, 3), fmt_num(rmsea, 3),
                                fmt_num(rl, 3), fmt_num(rh, 3), fmt_num(srmr, 3), if (good) tr_("よい当てはまりを示した。") else if (poor) tr_("当てはまりは不十分であった。") else tr_("おおむね許容できる当てはまりであった。")),
                       trf("標準化負荷量は %s〜%s であった。", fmt_num(min(std)), fmt_num(max(std))),
                       if (any(std < 0.4)) trf("負荷量が 0.4 未満の項目は%sであった。", paste(colnames(X)[std < 0.4], collapse = tr_("、"))) else "")
  res$method <- paste0(tr_("各項目の得点（選択肢の並びの先頭を高い点とした段階の得点。逆転項目は反転）の分散共分散行列から、最尤法で推定した。各項目は 1 つの因子にだけ負荷するとし、因子の分散を 1 に固定して因子間の相関を推定した。"),
                       tr_("すべての項目に回答した人を対象とした（リストワイズ除去）。"))
  if (!fit$conv) res$notes <- c(res$notes, tr_("計算が十分に収束しませんでした。項目や因子の組み方を見直してください。"))
  if (any(fit$th < 1e-3 * diag(S))) res$notes <- c(res$notes, tr_("誤差分散がほぼ 0 の項目があります（不適解の可能性）。その項目を除くか、因子の組み方を見直してください。"))
  if (m > 1 && any(abs(fit$Ph[lower.tri(fit$Ph)]) > 0.9)) res$notes <- c(res$notes, tr_("相関が 0.9 を超える因子の組があります。2 つの因子を区別できていない可能性があります。"))
  res$notes <- c(res$notes, tr_("段階の回答を数値とみなして計算しています。段階が少ない（4 段階以下）ときや回答が偏っているときは、カテゴリカルなデータ向けの推定（WLSMV など）の方が適している場合があります。"))
  res$chk <- list(chi = chi, cfi = cfi, rmsea = rmsea, srmr = srmr, std = std)
  finish(res, alpha)
}
register("cfa", tr_("確認的因子分析"), tr_("尺度・多変量"), tr_("あらかじめ決めた「因子と項目の組み合わせ」が、データに当てはまっているかを調べます。当てはまりの指標（CFI・RMSEA など）と因子負荷量、因子ごとの信頼性を出します。"),
         list(S("f1", tr_("因子 1 の項目"), "ordinal", tr_("例：「海が好き」を測る項目"), FALSE, TRUE), S("f2", tr_("因子 2 の項目"), "ordinal", tr_("2 因子以上のときに選ぶ"), FALSE, TRUE, TRUE),
              S("f3", tr_("因子 3 の項目"), "ordinal", "", FALSE, TRUE, TRUE), S("f4", tr_("因子 4 の項目"), "ordinal", "", FALSE, TRUE, TRUE)),
         list(), an_cfa)

# ---------------------------------------------------------------- 回帰分析（重回帰・ロジスティック回帰）
# 説明変数の行列を作る。単一回答はダミー変数（先頭の選択肢を基準）、順序のある回答は段階の得点、数値はそのまま
reg_design <- function(df, xs) {
  if (is.null(xs) || length(xs) == 0) stop_user(tr_("説明変数を 1 つ以上選んでください。"))
  cols <- list(); info <- list(); notes <- character(0)
  for (v in xs) {
    if (identical(v$type, "nominal")) {
      f <- droplevels(slot_factor(df, v)); lv <- levels(f)
      if (length(lv) < 2 && length(unlist(v[["dropped"]]))) stop_user(trf("%sは、除いた選択肢のほかに回答のある選択肢が 1 つしかないため、説明変数にできません。除く選択肢を減らすか、この説明変数を外してください。", q(v$label)))
      if (length(lv) < 2) { notes <- c(notes, paste0(q(v$label), tr_("は回答のある選択肢が 1 つしかないため、説明変数から除きました。"))); next }
      for (l in lv[-1]) { nm <- paste0(v$label, tr_("："), l); cols[[nm]] <- ifelse(is.na(f), NA_real_, as.numeric(f == l)); info[[nm]] <- list(var = v$label, base = lv[1], dummy = TRUE, level = l, slot = v) }
    } else {
      x <- as.numeric(slot_score(df, v)); cols[[v$label]] <- x; info[[v$label]] <- list(var = v$label, dummy = FALSE, slot = v)
      if (identical(v$type, "ordinal")) { lv <- levels(slot_factor(df, v)); info[[v$label]]$scores <- trf("%sは%s", q(v$label), paste(paste0(lv, " = ", fmt_pts(lev_points(v, lv))), collapse = tr_("、"))) }
    }
  }
  if (!length(cols)) stop_user(tr_("使える説明変数がありません。"))
  list(X = do.call(cbind, cols), info = info, notes = notes)
}
reg_scorenote <- function(info) {
  sc <- unlist(lapply(info, function(z) z$scores)); if (!length(sc)) return("")
  trf("順序のある回答は段階の得点として入れています（%s）。", paste(sc, collapse = if (identical(LANG, "en")) "; " else "。"))
}
reg_vif <- function(X) {
  if (ncol(X) < 2) return(rep(NA_real_, ncol(X)))
  vapply(seq_len(ncol(X)), function(j) { r2 <- summary(stats::lm(X[, j] ~ X[, -j]))$r.squared; if (r2 >= 1) Inf else 1 / (1 - r2) }, 0)
}
reg_common <- function(df, req, y) {
  d <- reg_design(df, req$slots$x$vars)
  if (any(vapply(req$slots$x$vars, function(v) identical(as.integer(v$col), as.integer(req$slots$y$col)), TRUE))) stop_user(tr_("目的変数と同じ設問が説明変数に入っています。説明変数から外してください。"))
  y0 <- y; ok <- !is.na(y) & stats::complete.cases(d$X); X <- d$X[ok, , drop = FALSE]; y <- y[ok]
  # 説明変数の選択肢を除いた人（目的変数とほかの説明変数には答えている人）の数
  dropn <- 0L; droptxt <- character(0)
  for (v in req$slots$x$vars) {
    dv <- unlist(v[["dropped"]]); if (!length(dv)) next
    droptxt <- c(droptxt, trf("%sの%s", q(v$label), paste(vapply(dv, q, ""), collapse = tr_("・"))))
  }
  if (length(droptxt)) {
    # 除かなければ推定に入っていた人の数との差（除いた選択肢による欠測を、欠測でないとみなして数える）
    hitOf <- function(v) { dv <- unlist(v[["dropped"]]); if (!length(dv)) rep(FALSE, nrow(df)) else as.character(df[[v$col]]) %in% dv }
    ok0 <- !is.na(y0)
    for (nm in names(d$info)) { v <- d$info[[nm]]$slot; ok0 <- ok0 & (!is.na(d$X[, nm]) | hitOf(v)) }
    dropn <- sum(ok0) - sum(ok)
  }
  colnames(X) <- names(d$info)
  const <- apply(X, 2, function(x) length(unique(x)) < 2)
  if (any(const)) stop_user(paste0(q(colnames(X)[const]), collapse = tr_("、")), tr_("は、対象の人の中で値がすべて同じなので、説明変数にできません。"))
  if (nrow(X) <= ncol(X) + 1) stop_user(tr_("人数（"), nrow(X), tr_(" 人）が説明変数の数に比べて少なすぎます。"))
  dnote <- if (length(droptxt)) trf("説明変数の選択肢を除いた人（%s、%d 人）は分析から除外した。", paste(droptxt, collapse = tr_("、")), dropn) else ""
  list(X = X, y = y, info = d$info, notes = d$notes, n = nrow(X), k = ncol(X), dropnote = dnote)
}
reg_rankcheck <- function(fit, X) {
  bad <- is.na(stats::coef(fit))[-1]
  if (any(bad)) stop_user(trf("説明変数%sが、ほかの説明変数から完全に決まってしまうため（多重共線性）、係数を計算できません。どちらかを外してください。", paste0(q(colnames(X)[bad]), collapse = tr_("、"))))
}
an_reg <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE); sy <- req$slots$y
  rc <- reg_common(df, req, as.numeric(slot_score(df, sy))); X <- rc$X; y <- rc$y; n <- rc$n; k <- rc$k
  if (length(unique(y)) < 2) stop_user(tr_("目的変数の値がすべて同じなので、回帰分析ができません。"))
  fit <- stats::lm(y ~ X); reg_rankcheck(fit, X); sm <- summary(fit); co <- sm$coefficients; ci <- stats::confint(fit)
  fs <- sm$fstatistic; pF <- stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
  sdx <- apply(X, 2, stats::sd); sdy <- stats::sd(y); beta <- co[-1, 1] * sdx / sdy; bci <- ci[-1, , drop = FALSE] * sdx / sdy; vif <- reg_vif(X)
  rn <- c(tr_("切片"), colnames(X))
  cells <- lapply(seq_along(rn), function(i) list(co[i, 1], co[i, 2], if (i > 1) beta[i - 1] else NULL, ci[i, 1], ci[i, 2], co[i, 3], co[i, 4], if (i > 1 && !is.na(vif[i - 1])) vif[i - 1] else NULL))
  base <- unique(unlist(lapply(rc$info, function(z) if (isTRUE(z$dummy)) paste0(q(z$var), tr_("は「"), z$base, tr_("」"))))) 
  res <- list(test = tr_("重回帰分析"), p = unname(pF), n = n_info(nrow(df), n), notes = rc$notes)
  res$grids <- list(list(kind = "grid", title = tr_("回帰係数"), corner = "", rows = A(rn), cols = A(c("B", tr_("標準誤差"), "β", tr_("95% 信頼区間の下限"), tr_("上限"), "t", "p", "VIF")),
                         fmt = A(c("num2s", "num2s", "num2s", "num2s", "num2s", "num2", "p", "num2")), cells = cells,
                         note = paste0(tr_("B は偏回帰係数、β は標準化偏回帰係数です。"), reg_scorenote(rc$info),
                                       if (length(base)) paste0(tr_("単一回答はダミー変数にし、"), paste(base, collapse = tr_("、")), tr_("を基準（0）にしています。")) else "")),
                    list(kind = "grid", title = tr_("モデルの当てはまり"), corner = "", rows = A(tr_("モデル")), cols = A(c("R²", tr_("調整済み R²"), "F", tr_("自由度 1"), tr_("自由度 2"), "p", "n")),
                         fmt = A(c("num2", "num2", "num2", "int", "int", "p", "int")), cells = list(list(sm$r.squared, sm$adj.r.squared, unname(fs[1]), unname(fs[2]), unname(fs[3]), unname(pF), n))))
  stat <- paste0("F(", fs[2], ", ", fs[3], ") = ", fmt_num(fs[1]))
  res$headline <- list(hl("R²", fmt_num(sm$r.squared)), hl(tr_("調整済み R²"), fmt_num(sm$adj.r.squared)), hl(paste0("F(", fs[2], ", ", fs[3], ")"), fmt_num(fs[1])), hl("p", fmt_p(pF, lead0, FALSE)), hl("n", as.character(n)))
  res$row <- list(stat = stat, effect = paste0("R² = ", fmt_num(sm$r.squared)))
  res$fig <- list(type = "box", cats = A(colnames(X)), box = lapply(seq_len(k), function(j) list(stats = A(rep(beta[j], 5)), out = A(numeric(0)), n = n, mean = unname(beta[j]), lo = unname(bci[j, 1]), hi = unname(bci[j, 2]))),
                  catTitle = tr_("説明変数"), valueTitle = tr_("標準化係数 β（95% 信頼区間）"), coef = "β", ref = 0)
  if (any(vif > 10, na.rm = TRUE)) res$notes <- c(res$notes, trf("VIF が 10 を超える説明変数（%s）があります。説明変数どうしの相関が強く（多重共線性）、係数が不安定な可能性があります。", paste(colnames(X)[vif > 10 & !is.na(vif)], collapse = tr_("、"))))
  if (n < 10 * k) res$notes <- c(res$notes, paste0(tr_("説明変数 1 つあたりの人数が "), fmt_num(n / k, 1), tr_(" 人です。10 人以上が目安とされます。")))
  sigx <- which(co[-1, 4] < alpha)
  res$report <- paste0(trf("%sを目的変数、%sを説明変数とする重回帰分析（強制投入法）を行った（n = %d）。", q(sy$label), paste(q(unique(vapply(rc$info, function(z) z$var, ""))), collapse = tr_("・")), n), rc$dropnote,
                       trf("モデルは%s（%s, %s）、決定係数は R² = %s（調整済み R² = %s）であった。", if (pF < alpha) tr_("統計的に有意であり") else tr_("統計的に有意ではなく"), stat, fmt_p(pF, lead0), fmt_num(sm$r.squared), fmt_num(sm$adj.r.squared)),
                       if (length(sigx)) trf("統計的に有意な説明変数は、%sであった。", paste(vapply(sigx, function(j) paste0(q(colnames(X)[j]), tr_("（β = "), fmt_num2s(beta[j]), ", ", fmt_p(co[j + 1, 4], lead0), tr_("）")), ""), collapse = tr_("、")))
                       else tr_("統計的に有意な説明変数はなかった。"))
  finish(res, alpha)
}
register("reg", tr_("重回帰分析"), tr_("回帰分析"), tr_("数値（または段階の得点）を、複数の質問から説明・予測します。説明変数ごとの影響の大きさ（標準化係数 β）と、モデル全体の説明力（R²）を出します。"),
         list(S("y", tr_("目的変数（説明したい値）"), c("numeric", "ordinal"), tr_("例：勉強時間、満足度")), S("x", tr_("説明変数（複数選べます）"), c("numeric", "ordinal", "nominal"), tr_("例：学年、性別、通学時間"), many = TRUE)),
         list(), an_reg)

an_logit <- function(df, req) {
  alpha <- opt(req, "alpha", 0.05); lead0 <- opt(req, "lead0", TRUE); sy <- req$slots$y
  f <- droplevels(slot_factor(df, sy))
  if (nlevels(f) != 2) stop_user(tr_("ロジスティック回帰分析の目的変数は 2 択です。"), q(sy$label), tr_("は現在 "), nlevels(f), tr_(" 個の選択肢があります。「選択肢の設定」でまとめるか、チェックを外して 2 つにしてください。"))
  ev <- levels(f)[1]; rc <- reg_common(df, req, ifelse(is.na(f), NA_real_, as.numeric(f == ev))); X <- rc$X; y <- rc$y; n <- rc$n; k <- rc$k
  if (length(unique(y)) < 2) stop_user(tr_("対象の人の目的変数の回答がすべて同じなので、分析できません。"))
  fit <- suppressWarnings(stats::glm(y ~ X, family = stats::binomial(), control = stats::glm.control(epsilon = 1e-12, maxit = 100))); reg_rankcheck(fit, X)
  co <- summary(fit)$coefficients; z975 <- stats::qnorm(0.975)
  or <- exp(co[, 1]); lo <- exp(co[, 1] - z975 * co[, 2]); hi <- exp(co[, 1] + z975 * co[, 2]); vif <- reg_vif(X)
  lr <- fit$null.deviance - fit$deviance; dfl <- fit$df.null - fit$df.residual; pL <- stats::pchisq(lr, dfl, lower.tail = FALSE)
  cs <- 1 - exp(-lr / n); nk <- cs / (1 - exp(-fit$null.deviance / n)); acc <- mean((stats::fitted(fit) >= 0.5) == (y == 1)); nev <- sum(y)
  rn <- c(tr_("切片"), colnames(X))
  cells <- lapply(seq_along(rn), function(i) list(co[i, 1], co[i, 2], co[i, 3], co[i, 4], or[i], lo[i], hi[i], if (i > 1 && !is.na(vif[i - 1])) vif[i - 1] else NULL))
  base <- unique(unlist(lapply(rc$info, function(z) if (isTRUE(z$dummy)) paste0(q(z$var), tr_("は「"), z$base, tr_("」")))))
  res <- list(test = tr_("ロジスティック回帰分析"), p = unname(pL), n = n_info(nrow(df), n), notes = rc$notes)
  res$grids <- list(list(kind = "grid", title = paste0(tr_("回帰係数（"), q(ev), tr_("になる確率を予測）")), corner = "", rows = A(rn), cols = A(c("B", tr_("標準誤差"), "Wald z", "p", tr_("オッズ比"), tr_("95% 信頼区間の下限"), tr_("上限"), "VIF")),
                         fmt = A(c("num2s", "num2s", "num2", "p", "num2s", "num2s", "num2s", "num2")), cells = cells,
                         note = paste0(tr_("オッズ比は、説明変数が 1 上がる（ダミー変数では基準の選択肢と比べた）ときに、"), q(ev), tr_("と答えるオッズが何倍になるかを表します。信頼区間は Wald 法です。"), reg_scorenote(rc$info),
                                       if (length(base)) paste0(tr_("単一回答はダミー変数にし、"), paste(base, collapse = tr_("、")), tr_("を基準にしています。")) else "")),
                    list(kind = "grid", title = tr_("モデルの当てはまり"), corner = "", rows = A(tr_("モデル")), cols = A(c(tr_("尤度比 χ²"), tr_("自由度"), "p", "Cox-Snell R²", "Nagelkerke R²", "AIC", tr_("的中率 (%)"), "n", trf("%sの人数", ev))),
                         fmt = A(c("num2", "int", "p", "num2", "num2", "num1", "num1", "int", "int")), cells = list(list(lr, dfl, pL, cs, nk, fit$aic, acc * 100, n, nev))))
  stat <- paste0("χ²(", dfl, ") = ", fmt_num(lr))
  res$headline <- list(hl(paste0(tr_("尤度比 χ²("), dfl, ")"), fmt_num(lr)), hl("p", fmt_p(pL, lead0, FALSE)), hl("Nagelkerke R²", fmt_num(nk)), hl("n", as.character(n)))
  res$row <- list(stat = stat, effect = paste0("Nagelkerke R² = ", fmt_num(nk)))
  res$fig <- list(type = "box", cats = A(colnames(X)), box = lapply(seq_len(k), function(j) list(stats = A(rep(or[j + 1], 5)), out = A(numeric(0)), n = n, mean = unname(or[j + 1]), lo = unname(lo[j + 1]), hi = unname(hi[j + 1]))),
                  catTitle = tr_("説明変数"), valueTitle = tr_("オッズ比（95% 信頼区間）"), coef = tr_("オッズ比"), ref = 1)
  if (!fit$converged || any(abs(co[-1, 1]) > 10 | co[-1, 2] > 50)) res$notes <- c(res$notes, tr_("計算が安定しませんでした。ある説明変数の値で結果が完全に分かれている（完全分離）可能性があります。選択肢をまとめるか、その説明変数を外してください。"))
  if (any(vif > 10, na.rm = TRUE)) res$notes <- c(res$notes, trf("VIF が 10 を超える説明変数（%s）があります。説明変数どうしの相関が強く、係数が不安定な可能性があります。", paste(colnames(X)[vif > 10 & !is.na(vif)], collapse = tr_("、"))))
  epv <- min(nev, n - nev) / k
  if (epv < 10) res$notes <- c(res$notes, paste0(tr_("少ない方の結果の人数を説明変数の数で割った値（EPV）が "), fmt_num(epv, 1), tr_(" です。10 以上が目安とされます。")))
  sigx <- which(co[-1, 4] < alpha)
  res$report <- paste0(trf("%sが%sであるかを目的変数、%sを説明変数とするロジスティック回帰分析（強制投入法）を行った（n = %d、%s %d 人）。", q(sy$label), q(ev), paste(q(unique(vapply(rc$info, function(z) z$var, ""))), collapse = tr_("・")), n, ev, nev), rc$dropnote,
                       trf("モデルは%s（尤度比検定 %s, %s）、Nagelkerke R² = %s、的中率は %s%% であった。", if (pL < alpha) tr_("統計的に有意であり") else tr_("統計的に有意ではなく"), stat, fmt_p(pL, lead0), fmt_num(nk), fmt_num(acc * 100, 1)),
                       if (length(sigx)) trf("統計的に有意な説明変数は、%sであった。", paste(vapply(sigx, function(j) paste0(q(colnames(X)[j]), tr_("（オッズ比 = "), fmt_num2s(or[j + 1]), ", 95%CI ", fmt_ci(c(lo[j + 1], hi[j + 1]), s2 = TRUE), ", ", fmt_p(co[j + 1, 4], lead0), tr_("）")), ""), collapse = tr_("、")))
                       else tr_("統計的に有意な説明変数はなかった。"))
  finish(res, alpha)
}
register("logit", tr_("ロジスティック回帰分析"), tr_("回帰分析"), tr_("2 択の回答（例：部活動に入っているか）を、複数の質問から説明・予測します。説明変数ごとのオッズ比を出します。"),
         list(S("y", tr_("目的変数（2 択の回答）"), cat_types, tr_("例：はい・いいえ。先頭の選択肢になる確率を予測します")), S("x", tr_("説明変数（複数選べます）"), c("numeric", "ordinal", "nominal"), tr_("例：学年、性別、通学時間"), many = TRUE)),
         list(), an_logit)
