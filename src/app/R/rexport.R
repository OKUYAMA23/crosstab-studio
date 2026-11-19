# rexport.R ― アプリの解析を、R の標準の関数で計算し直すスクリプト（.R）を作る
#   Excel ブックと同じく「計算の確かめ」のためのもの。スクリプトと「解析用データ.csv」を同じフォルダーに置いて実行する。
#   データの前処理（選択肢のまとめ・絞り込み・尺度得点）も、スクリプトの中に R のコードとして書く。
#   数値の列は、アプリが直した値（「40分」→ 40 など）を「列番号_数値」の列としてデータに足す。

rx_s <- function(x) encodeString(enc2utf8(as.character(x)), quote = "\"")
rx_vec <- function(x) if (length(x) == 1) rx_s(x) else paste0("c(", paste(vapply(x, rx_s, ""), collapse = ", "), ")")
rx_map <- function(cats) paste0("list(", paste(vapply(cats, function(c) paste0(rx_s(c$label), " = ", rx_vec(unlist(c$values))), ""), collapse = ", "), ")")
rx_cmt <- function(x) gsub("[\r\n]+", " ", as.character(x))

# 変数を作るコード。名前 nm のベクトルを作る（順序の得点が要るときは score = TRUE）
rx_var <- function(nm, s, score = FALSE, numcols) {
  if (is.null(s)) return(character(0))
  lab <- paste0("  # ", rx_cmt(s$label))
  if (identical(s$type, "numeric")) {
    numcols[[as.character(s$col)]] <- s
    return(c(lab, sprintf("  %s <- as.numeric(dd[[\"%d_数値\"]])", nm, as.integer(s$col))))
  }
  if (identical(s$type, "multi")) return(c(lab, sprintf("  %s <- multi_mat(dd[[%d]], %s, %s)", nm, as.integer(s$col), rx_map(s$cats), rx_s(if (is.null(s$delim)) ";" else s$delim))))
  out <- c(lab, sprintf("  %s <- cat_var(dd[[%d]], %s)", nm, as.integer(s$col), rx_map(s$cats)))
  if (score) out <- c(out, sprintf("  %s <- score(%s)                       # 先頭の選択肢を高い点にした段階の得点", nm, nm))
  out
}
# 複数の項目（同じ選択肢）の得点の行列
rx_items <- function(nm, s, numcols) {
  cols <- unlist(s$cols); rev <- unlist(s$reverse)
  c(sprintf("  # %s", rx_cmt(paste(unlist(s$labels), collapse = " / "))),
    sprintf("  %s <- sapply(%s, function(j) score(cat_var(dd[[j]], %s)))", nm, paste0("c(", paste(cols, collapse = ", "), ")"), rx_map(s$cats)),
    sprintf("  colnames(%s) <- %s", nm, rx_vec(unlist(s$labels))),
    if (length(rev)) sprintf("  for (j in which(%s %%in%% %s)) %s[, j] <- %d + 1 - %s[, j]   # 逆転項目", paste0("c(", paste(cols, collapse = ", "), ")"), paste0("c(", paste(rev, collapse = ", "), ")"), nm, length(unique(vapply(s$cats, function(c) as.character(c$label), ""))), nm) else NULL)
}
rx_filter <- function(f) {
  if (is.null(f) || is.null(f$col) || !length(f$values)) return("  dd <- d")
  vals <- unlist(f$values)
  if (identical(f$type, "multi")) return(c(sprintf("  # 絞り込み：%s が %s のいずれかを含む人", rx_cmt(f$label), rx_cmt(paste(vals, collapse = "・"))),
                                         sprintf("  dd <- d[vapply(strsplit(d[[%d]], %s, fixed = TRUE), function(t) any(trimws(t) %%in%% %s), TRUE) & !is.na(d[[%d]]), , drop = FALSE]", as.integer(f$col), rx_s(if (is.null(f$delim)) ";" else f$delim), rx_vec(vals), as.integer(f$col))))
  c(sprintf("  # 絞り込み：%s が %s の人", rx_cmt(f$label), rx_cmt(paste(vals, collapse = "・"))), sprintf("  dd <- d[!is.na(d[[%d]]) & d[[%d]] %%in%% %s, , drop = FALSE]", as.integer(f$col), as.integer(f$col), rx_vec(vals)))
}
# 尺度得点（アプリで作った列）を、項目から計算するコード
rx_computed <- function(defs, numcols) {
  out <- character(0)
  for (d in defs) {
    rev <- unlist(d$reverse)
    out <- c(out, "", sprintf("# 尺度得点「%s」：%d 項目の%s点（すべての項目に答えた人だけ）", rx_cmt(d$name), length(d$items), if (identical(d$method, "sum")) "合計" else "平均"),
             sprintf("sc <- cbind(%s)", paste(vapply(d$items, function(s) {
               v <- if (identical(s$type, "numeric")) { numcols[[as.character(s$col)]] <- s; sprintf("as.numeric(d[[\"%d_数値\"]])", as.integer(s$col)) } else sprintf("score(cat_var(d[[%d]], %s))", as.integer(s$col), rx_map(s$cats))
               if (s$col %in% rev) sprintf("%d + 1 - %s", length(s$cats), v) else v
             }, ""), collapse = ",\n            ")),
             sprintf("d[[\"%d_数値\"]] <- ifelse(complete.cases(sc), %s(sc), NA)", as.integer(d$col), if (identical(d$method, "sum")) "rowSums" else "rowMeans"))
  }
  out
}
rx_appvals <- function(res) {
  if (!isTRUE(res$ok)) return(paste0("  cat(", rx_s(paste0(">> アプリ：計算できませんでした（", rx_cmt(res$message), "）\n")), ")"))
  hl <- vapply(res$headline %||% list(), function(h) paste0(h$k, " = ", h$v), "")
  paste0("  cat(", rx_s(paste0(">> アプリの値：", rx_cmt(paste(hl, collapse = "、")), "\n")), ")   # 上の結果と見比べる")
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# 解析ごとの計算のコード
rx_analysis <- function(req, res, numcols) {
  s <- req$slots; o <- req$opts; t <- req$type; L <- character(0); add <- function(...) L <<- c(L, ...)
  v <- function(nm, sl, score = FALSE) add(rx_var(nm, sl, score, numcols))
  tab2 <- "  tab <- table(a, b); tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]; print(tab)"
  switch(t,
    freq = { v("a", s$a)
      if (identical(s$a$type, "multi")) add("  ok <- !is.na(a[, 1]); print(colSums(a[ok, , drop = FALSE])); print(round(100 * colMeans(a[ok, , drop = FALSE]), 1))")
      else { add("  tab <- table(a); print(cbind(人数 = tab, 割合 = round(100 * prop.table(tab), 1)))")
        if (isTRUE(o$gof)) add("  print(chisq.test(tab))                         # 適合度検定（すべての選択肢が同じ割合か）") } },
    freqmulti = { add(rx_items("X", s$items, numcols)); add("  print(apply(X, 2, function(x) table(factor(x, levels = max(X, na.rm = TRUE):1))))")
      if (isTRUE(o$friedman)) add("  m <- X[complete.cases(X), , drop = FALSE]; print(friedman.test(m)); cat(\"Kendall の W =\", friedman.test(m)$statistic / (nrow(m) * (ncol(m) - 1)), \"\\n\")") },
    chisq = { v("a", s$a); v("b", s$b); add(tab2, sprintf("  ct <- chisq.test(tab, correct = %s); print(ct)", if (isTRUE(o$yates)) "TRUE" else "FALSE"),
      "  cat(\"Cramér の V =\", sqrt(unname(suppressWarnings(chisq.test(tab, correct = FALSE))$statistic) / (sum(tab) * (min(dim(tab)) - 1))), \"\\n\")",
      "  print(round(ct$stdres, 2))                     # 調整済み残差") },
    fisher = { v("a", s$a); v("b", s$b); add(tab2, "  print(tryCatch(fisher.test(tab, workspace = 2e6), error = function(e) { set.seed(20240101); fisher.test(tab, simulate.p.value = TRUE, B = 200000) }))") },
    mwu = { v("g", s$g); v("y", s$y, TRUE); add("  ok <- !is.na(g) & !is.na(y); g <- droplevels(g[ok]); y <- y[ok]",
      "  print(wilcox.test(y ~ g, exact = FALSE, correct = TRUE))   # 正規近似（同順位と連続性の補正あり）") },
    kw = { v("g", s$g); v("y", s$y, TRUE); add("  ok <- !is.na(g) & !is.na(y); g <- droplevels(g[ok]); y <- y[ok]", "  print(kruskal.test(y, g))",
      "  print(pairwise.wilcox.test(y, g, p.adjust.method = \"holm\", exact = FALSE))   # 多重比較") },
    spearman = { v("x", s$x, TRUE); v("y", s$y, TRUE); add("  print(cor.test(x, y, method = \"spearman\", exact = FALSE))") },
    multi = { v("M", s$m); v("g", s$g); add("  ok <- !is.na(M[, 1]) & !is.na(g); M <- M[ok, , drop = FALSE]; g <- droplevels(g[ok])",
      "  p <- sapply(colnames(M), function(k) { tab <- table(factor(M[, k], levels = c(TRUE, FALSE)), g); if (any(rowSums(tab) == 0)) return(NA)",
      "    E <- outer(rowSums(tab), colSums(tab)) / sum(tab)",
      "    if (any(E < 5)) fisher.test(tab, workspace = 2e6)$p.value else suppressWarnings(chisq.test(tab, correct = FALSE))$p.value })",
      "  print(cbind(p = p, 補正後の_p = p.adjust(p, \"holm\")))   # 選択肢ごと（期待度数 5 未満があればフィッシャー）") },
    wsr = { v("x", s$x, TRUE); v("y", s$y, TRUE); add("  ok <- !is.na(x) & !is.na(y)", "  print(wilcox.test(x[ok], y[ok], paired = TRUE, exact = FALSE, correct = TRUE))") },
    mcnemar = { v("a", s$x); v("b", s$y); add("  ok <- !is.na(a) & !is.na(b); tab <- table(a[ok], b[ok]); print(tab)",
      "  if (tab[1, 2] + tab[2, 1] < 25) print(binom.test(tab[1, 2], tab[1, 2] + tab[2, 1], 0.5)) else print(mcnemar.test(tab, correct = TRUE))   # 不一致が 25 未満なら正確二項検定") },
    trend = { v("b", s$b); v("o", s$o); add("  ok <- !is.na(b) & !is.na(o); tab <- table(droplevels(o[ok]), droplevels(b[ok])); print(tab)", "  print(prop.trend.test(tab[, 1], rowSums(tab)))") },
    strat = { v("a", s$a); v("b", s$b); v("z", s$z); add("  ok <- !is.na(a) & !is.na(b) & !is.na(z); t3 <- table(droplevels(a[ok]), droplevels(b[ok]), droplevels(z[ok]))",
      "  t3 <- t3[, , apply(t3, 3, sum) > 1, drop = FALSE]; print(t3)", "  print(mantelhaen.test(t3, correct = FALSE))") },
    numsum = { v("y", s$y, TRUE); if (!is.null(s$g)) { v("g", s$g); add("  ok <- !is.na(y) & !is.na(g)", "  print(do.call(rbind, tapply(y[ok], droplevels(g[ok]), function(x) c(n = length(x), 平均 = mean(x), 標準偏差 = sd(x), 中央値 = median(x), 最小 = min(x), 最大 = max(x)))))") }
      else add("  y <- y[!is.na(y)]; print(c(n = length(y), 平均 = mean(y), 標準偏差 = sd(y), 中央値 = median(y), 最小 = min(y), 最大 = max(y)))") },
    ttest = { v("g", s$g); v("y", s$y, TRUE); add("  ok <- !is.na(g) & !is.na(y); g <- droplevels(g[ok]); y <- y[ok]",
      sprintf("  print(t.test(y ~ g, var.equal = %s))   # %s", if (isTRUE(o$student)) "TRUE" else "FALSE", if (isTRUE(o$student)) "スチューデントの t 検定" else "ウェルチの t 検定")) },
    anova = { v("g", s$g); v("y", s$y, TRUE); add("  ok <- !is.na(g) & !is.na(y); g <- droplevels(g[ok]); y <- y[ok]",
      sprintf("  print(oneway.test(y ~ g, var.equal = %s))   # %s", if (isTRUE(o$classic)) "TRUE" else "FALSE", if (isTRUE(o$classic)) "一元配置分散分析" else "ウェルチの分散分析"),
      if (isTRUE(o$classic)) "  print(TukeyHSD(aov(y ~ g)))                    # テューキーの多重比較" else NULL) },
    pairedt = { v("x", s$x, TRUE); v("y", s$y, TRUE); add("  ok <- !is.na(x) & !is.na(y)", "  print(t.test(x[ok], y[ok], paired = TRUE))") },
    pearson = { v("x", s$x, TRUE); v("y", s$y, TRUE); add("  print(cor.test(x, y, method = \"pearson\"))") },
    alpha = { add(rx_items("X", s$items, numcols)); add("  X <- X[complete.cases(X), , drop = FALSE]; k <- ncol(X)",
      "  a <- k / (k - 1) * (1 - sum(apply(X, 2, var)) / var(rowSums(X))); cat(\"α =\", a, \"\\n\")",
      "  print(sapply(seq_len(k), function(j) c(項目と残りの合計の相関 = cor(X[, j], rowSums(X[, -j, drop = FALSE])), 除いたときの_α = if (k > 2) (k - 1) / (k - 2) * (1 - sum(apply(X[, -j], 2, var)) / var(rowSums(X[, -j]))) else NA)))",
      "  if (k >= 3) { f1 <- factanal(covmat = cor(X), factors = 1, n.obs = nrow(X)); l <- f1$loadings[, 1]; if (sum(l) < 0) l <- -l; cat(\"ω =\", sum(l)^2 / (sum(l)^2 + sum(f1$uniquenesses)), \"\\n\") }") },
    factor = { add(rx_items("X", s$items, numcols)); nf0 <- o$nf %||% "auto"
      add("  X <- X[complete.cases(X), , drop = FALSE]; p <- ncol(X)",
          if (identical(nf0, "auto")) "  nf <- max(1, sum(eigen(cor(X), only.values = TRUE)$values > 1))   # 固有値が 1 を超える数（カイザー基準）" else sprintf("  nf <- %d", as.integer(nf0)),
          "  nf <- min(nf, max(which(sapply(1:p, function(f) ((p - f)^2 - (p + f)) / 2 >= 0))))   # 項目の数で求められる因子の数まで",
          sprintf("  print(factanal(covmat = cor(X), factors = nf, n.obs = nrow(X), rotation = if (nf > 1) %s else \"none\"), cutoff = 0)", rx_s(o$rotation %||% "promax"))) },
    ca = { v("a", s$a); v("b", s$b); add(tab2, "  P <- tab / sum(tab); r <- rowSums(P); cc <- colSums(P); Z <- (P - outer(r, cc)) / sqrt(outer(r, cc))",
      "  sv <- svd(Z)$d; print(round(rbind(固有値 = sv^2, 寄与率 = 100 * sv^2 / sum(sv^2)), 4)[, seq_len(min(dim(tab)) - 1), drop = FALSE])") },
    cfa = { ids <- Filter(function(id) !is.null(s[[id]]) && length(unlist(s[[id]]$cols)), c("f1", "f2", "f3", "f4"))
      for (k in seq_along(ids)) add(rx_items(paste0("F", k), s[[ids[k]]], numcols))
      add(sprintf("  X <- cbind(%s); colnames(X) <- paste0(\"x\", seq_len(ncol(X))); X <- X[complete.cases(X), , drop = FALSE]", paste0("F", seq_along(ids), collapse = ", ")))
      nn <- vapply(ids, function(id) length(unlist(s[[id]]$cols)), 1L); idx <- split(seq_len(sum(nn)), rep(seq_along(nn), nn))
      add(sprintf("  model <- %s", rx_s(paste(vapply(seq_along(idx), function(k) paste0("F", k, " =~ ", paste0("x", idx[[k]], collapse = " + ")), ""), collapse = "\n"))),
          "  if (requireNamespace(\"lavaan\", quietly = TRUE)) {          # パッケージ lavaan があれば、それで計算し直す",
          "    fit <- lavaan::cfa(model, data = as.data.frame(X), std.lv = TRUE, estimator = \"ML\")",
          "    print(lavaan::fitMeasures(fit, c(\"chisq\", \"df\", \"pvalue\", \"cfi\", \"tli\", \"rmsea\", \"rmsea.ci.lower\", \"rmsea.ci.upper\", \"srmr\")))",
          "    print(lavaan::standardizedSolution(fit)[lavaan::standardizedSolution(fit)$op == \"=~\", ])",
          "  } else cat(\"確認的因子分析の計算し直しには、パッケージ lavaan が必要です（install.packages(\\\"lavaan\\\")）。\\n\")") },
    reg = , logit = { xs <- s$x$vars; nms <- paste0("x", seq_along(xs))
      if (identical(t, "reg")) v("y", s$y, TRUE) else { v("y", s$y); add("  y <- as.numeric(y == levels(y)[1])           # 先頭の選択肢になるか（1 / 0）") }
      for (k in seq_along(xs)) v(nms[k], xs[[k]], !identical(xs[[k]]$type, "nominal"))
      add(sprintf("  m <- data.frame(y = y, %s)", paste0(nms, " = ", nms, collapse = ", ")), "  m <- droplevels(m[complete.cases(m), ])",
          sprintf("  names(m) <- c(\"y\", %s)", paste(vapply(xs, function(x) rx_s(x$label), ""), collapse = ", ")),
          if (identical(t, "reg")) "  fit <- lm(y ~ ., data = m); print(summary(fit)); print(confint(fit))"
          else c("  fit <- glm(y ~ ., data = m, family = binomial); print(summary(fit)); print(exp(cbind(オッズ比 = coef(fit), confint.default(fit))))",
                 "  cat(\"尤度比 χ² =\", fit$null.deviance - fit$deviance, \" 自由度 =\", fit$df.null - fit$df.residual, \" p =\", pchisq(fit$null.deviance - fit$deviance, fit$df.null - fit$df.residual, lower.tail = FALSE), \"\\n\")")) },
    add(sprintf("  # この解析（%s）は、R の標準の関数で書き直していません。", rx_cmt(t))))
  L
}

build_rscript <- function(df, items, data_name, app_version, date = "") {
  numcols <- new.env()
  body <- character(0)
  computed <- NULL
  for (k in seq_along(items)) {
    it <- items[[k]]; req <- it$req
    if (length(req$computed)) computed <- req$computed
    if (req$type %in% c("textmine", "timeline")) {                     # アプリの中の処理（語の分割・日時の読み取り）を使う解析は、コードにしない
      body <- c(body, "", sprintf("# ---- %d. %s", k, rx_cmt(it$name)), sprintf("cat(%s)", rx_s(sprintf("\n==== %d. %s ====\nこの解析は、アプリの中の処理（%s）を使うため、R のコードにしていません。\n", k, rx_cmt(it$name), if (req$type == "textmine") "語の分割" else "回答日時の読み取り"))))
      next
    }
    res <- tryCatch({ d2 <- if (length(req$computed)) add_computed(df, req$computed) else df; run_analysis(d2, req) }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    code <- rx_analysis(req, res, numcols)
    body <- c(body, "", sprintf("# ---- %d. %s", k, rx_cmt(it$name)), sprintf("cat(%s)", rx_s(sprintf("\n==== %d. %s ====\n", k, rx_cmt(it$name)))), "local({", rx_filter(req$filter), code, rx_appvals(res), "})")
  }
  comp <- if (length(computed)) rx_computed(computed, numcols) else character(0)
  head <- c(sprintf("# CrossTab Studio %s で書き出した R のコード（%s）", app_version, date),
            "# アプリの解析を、R の標準の関数で計算し直して確かめるためのものです（Excel ブックと同じく、計算の確かめに使います）。",
            "# 使い方：このファイルと「解析用データ.csv」を同じフォルダーに置き、R（RStudio など）で作業フォルダーをそのフォルダーにしてから、全体を実行します。",
            "#        各解析の最後に「>> アプリの値」を表示するので、その上に出る R の結果と見比べてください。",
            "# 追加のパッケージは使いません（確認的因子分析だけ、パッケージ lavaan があれば使います）。",
            "",
            sprintf("d <- read.csv(%s, fileEncoding = \"UTF-8-BOM\", check.names = FALSE, na.strings = \"\", colClasses = \"character\")", rx_s(data_name)),
            "",
            "# 選択肢をまとめる：map は「表示する名前 = 元の回答」の一覧。一覧にない回答（外した選択肢・無回答）は欠損にする",
            "cat_var <- function(x, map) { out <- rep(NA_character_, length(x)); for (lb in names(map)) out[!is.na(x) & x %in% map[[lb]]] <- lb; factor(out, levels = names(map)) }",
            "# 段階の得点：先頭の選択肢を最も高い点にする（5 段階なら 5〜1 点）",
            "score <- function(f) nlevels(f) + 1L - as.integer(f)",
            "# 複数回答：回答者 × 選択肢の TRUE / FALSE の行列（無回答は NA）",
            "multi_mat <- function(x, map, delim) {",
            "  M <- matrix(FALSE, length(x), length(map), dimnames = list(NULL, names(map)))",
            "  for (i in seq_along(x)) { if (is.na(x[i])) { M[i, ] <- NA; next }; t <- trimws(strsplit(x[i], delim, fixed = TRUE)[[1]]); for (lb in names(map)) M[i, lb] <- any(t %in% map[[lb]]) }",
            "  M",
            "}")
  list(script = paste(c(head, comp, body, ""), collapse = "\n"), numcols = as.list(numcols))
}

# データ：元の列（文字のまま）に、数値の列の直した値（「列番号_数値」）を足した CSV
rscript_data <- function(df, numcols) {
  out <- df
  for (nm in names(numcols)) { s <- numcols[[nm]]; j <- as.integer(s$col); if (j <= ncol(df) && !is.numeric(df[[j]])) out[[paste0(j, "_数値")]] <- numeric_values(df[[j]], s$fix, s$unit) else if (j <= ncol(df)) out[[paste0(j, "_数値")]] <- df[[j]] }
  out
}
csv_text <- function(df) {
  esc <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; ifelse(grepl("[\",\r\n]", x), paste0("\"", gsub("\"", "\"\"", x), "\""), x) }
  lines <- c(paste(esc(names(df)), collapse = ","), do.call(paste, c(lapply(df, esc), sep = ",")))
  paste0("\ufeff", paste(lines, collapse = "\r\n"), "\r\n")
}
