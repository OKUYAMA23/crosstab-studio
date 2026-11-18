# stats.R ― 数値の書式、変数の組み立て、クロス集計表の共通部品

fmt_num <- function(x, d = 2) {
  if (is.null(x) || length(x) == 0) return("–")
  x <- round(as.numeric(x), d); x[!is.na(x) & x == 0] <- 0
  out <- formatC(x, format = "f", digits = d); out[is.na(x)] <- "–"
  out
}
fmt_pct <- function(x, d = 1) fmt_num(100 * x, d)
# p 値。lead0 = FALSE のときは「.012」のように 0 を省く
fmt_p <- function(p, lead0 = TRUE, with_p = TRUE) {
  if (is.null(p) || is.na(p)) return(if (with_p) "p = –" else "–")
  if (p < 0.001) { s <- "0.001"; op <- "<" } else { s <- formatC(p, format = "f", digits = 3); op <- "=" }
  if (!lead0) s <- sub("^0", "", s)
  if (with_p) paste("p", op, s) else if (op == "<") paste("<", s) else s
}
q <- function(s) paste0("「", s, "」")
A <- function(x) I(x)   # JSON で必ず配列にする
mat <- function(m) { m <- unname(as.matrix(m)); storage.mode(m) <- "double"; m }

opt <- function(req, name, default) {
  v <- req$opts[[name]]
  if (is.null(v)) default else v
}

slot_labels <- function(slot) vapply(slot$cats, function(c) as.character(c$label), "")

slot_factor <- function(df, slot) {
  if (is.null(slot) || is.null(slot$col)) stop_user("列が選ばれていません。右の作業ウィンドウで列を選んでください。")
  if (slot$col < 1 || slot$col > ncol(df)) stop_user("選んだ列がデータにありません。列を選び直してください。")
  x <- df[[slot$col]]
  if (length(slot$cats) == 0) stop_user(q(slot$label), " の選択肢がすべて外されています。「選択肢の設定」で 1 つ以上にチェックを入れてください。")
  lev <- unique(slot_labels(slot))
  keys <- character(0); vals <- character(0)
  for (c in slot$cats) for (v in unlist(c$values)) { keys <- c(keys, v); vals <- c(vals, as.character(c$label)) }
  factor(vals[match(x, keys)], levels = lev)
}

# 複数回答：行＝回答者、列＝選択肢の TRUE/FALSE 行列（無回答の行は NA）
slot_multi <- function(df, slot) {
  if (is.null(slot) || is.null(slot$col)) stop_user("複数回答の列が選ばれていません。")
  x <- df[[slot$col]]
  if (length(slot$cats) == 0) stop_user(q(slot$label), " の選択肢がすべて外されています。")
  lev <- unique(slot_labels(slot))
  keys <- character(0); vals <- character(0)
  for (c in slot$cats) for (v in unlist(c$values)) { keys <- c(keys, v); vals <- c(vals, as.character(c$label)) }
  tl <- split_multi(x, if (is.null(slot$delim)) ";" else slot$delim)
  M <- matrix(FALSE, length(x), length(lev), dimnames = list(NULL, lev))
  for (i in seq_along(tl)) {
    if (is.na(x[i])) { M[i, ] <- NA; next }
    j <- match(vals[match(tl[[i]], keys)], lev)
    M[i, j[!is.na(j)]] <- TRUE
  }
  M
}

# 順序の得点：先頭の選択肢を最も高い得点にする（数値の列はそのままの値）
slot_score <- function(df, slot) {
  if (identical(slot$type, "numeric")) return(numeric_values(df[[slot$col]], slot$fix, slot$unit))
  f <- slot_factor(df, slot)
  nlevels(f) + 1L - as.integer(f)
}

apply_filter <- function(df, flt) {
  if (is.null(flt) || is.null(flt$col) || length(flt$values) == 0) return(rep(TRUE, nrow(df)))
  x <- df[[flt$col]]; vals <- unlist(flt$values)
  if (identical(flt$type, "multi")) {
    tl <- split_multi(x, if (is.null(flt$delim)) ";" else flt$delim)
    vapply(tl, function(t) any(t %in% vals), TRUE) & !is.na(x)
  } else !is.na(x) & x %in% vals
}

# 度数のない行・列を落としたクロス集計表
make_table <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  tab <- table(a[ok], b[ok])
  dropped <- c(rownames(tab)[rowSums(tab) == 0], colnames(tab)[colSums(tab) == 0])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  list(tab = tab, n = sum(tab), dropped = dropped)
}

# クロス集計表（度数・期待度数・調整済み標準化残差）
xtab_obj <- function(tab, rowLabel, colLabel, alpha = 0.05, residMode = "z", resid = TRUE) {
  n <- sum(tab); rs <- rowSums(tab); cs <- colSums(tab)
  out <- list(kind = "xtab", rows = A(rownames(tab)), cols = A(colnames(tab)),
              rowLabel = rowLabel, colLabel = colLabel, count = mat(tab))
  if (resid && nrow(tab) >= 2 && ncol(tab) >= 2) {
    E <- outer(rs, cs) / n
    adj <- (tab - E) / sqrt(E * outer(1 - rs / n, 1 - cs / n))
    out$expected <- mat(E); out$resid <- mat(adj)
    out$crit <- if (identical(residMode, "bonf")) stats::qnorm(1 - alpha / (2 * length(tab))) else stats::qnorm(1 - alpha / 2)
    out$residMode <- residMode
  }
  out
}

cramers_v <- function(tab) {
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  as.numeric(sqrt(chi / (sum(tab) * (min(dim(tab)) - 1))))
}

expected_check <- function(tab) {
  E <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  list(n_lt5 = sum(E < 5), share_lt5 = mean(E < 5), min = min(E), cells = length(E))
}

# 群ごとに最も多い回答を述べる文
modal_sentence <- function(tab, colLabel) {
  rp <- prop.table(tab, 1)
  mi <- apply(tab, 1, which.max)
  if (length(unique(mi)) == 1) {
    c1 <- colnames(tab)[mi[1]]
    paste0(q(colLabel), "で最も多い回答は、いずれの群でも", q(c1), "であった（",
           paste0(rownames(tab), " ", fmt_pct(rp[cbind(seq_len(nrow(tab)), mi)]), "%", collapse = "、"), "）。")
  } else {
    paste0(q(colLabel), "で最も多い回答は、",
           paste0(rownames(tab), "では", q(colnames(tab)[mi]), "（", vapply(seq_len(nrow(tab)), function(i) fmt_pct(rp[i, mi[i]]), ""), "%）", collapse = "、"),
           "であった。")
  }
}

# 残差が基準を超えて大きいセルを述べる文（残差の大きい順、最大 6 件）
resid_sentence <- function(xt, tab) {
  if (is.null(xt$resid)) return("")
  R <- xt$resid; rp <- prop.table(tab, 1)
  idx <- which(R > xt$crit, arr.ind = TRUE)
  basis <- paste0("|調整済み標準化残差| > ", fmt_num(xt$crit, 2), if (identical(xt$residMode, "bonf")) "、ボンフェローニ法で補正" else "")
  if (nrow(idx) == 0) return(paste0("ただし、基準（", basis, "）を超えるセルはなかった。"))
  idx <- idx[order(-R[idx]), , drop = FALSE]
  more <- nrow(idx) > 6
  if (more) idx <- idx[1:6, , drop = FALSE]
  parts <- vapply(seq_len(nrow(idx)), function(k) {
    i <- idx[k, 1]; j <- idx[k, 2]
    paste0(rownames(tab)[i], "では", q(colnames(tab)[j]), "（", fmt_pct(rp[i, j]), "%）")
  }, "")
  paste0("残差分析（", basis, "）では、", paste(parts, collapse = "、"), "の割合が期待される値より有意に高かった",
         if (more) "（残差の大きい順に 6 件を記載）。" else "。")
}

median_label <- function(score, lev) {
  m <- stats::median(score); k <- length(lev)
  if (m == floor(m)) return(q(lev[k + 1 - m]))
  paste0(q(lev[k + 1 - ceiling(m)]), "と", q(lev[k + 1 - floor(m)]), "の間")
}

n_info <- function(total, used) list(total = total, used = used, excluded = total - used)

# ---- ウィルコクソン系の検定の p 値（正規近似。同順位の補正と連続性の補正あり）
# R 4.6 の wilcox.test は両側の p を 2 * min(p, 1 - p) で求めるため、p がとても小さいと 0 になる。
# R 4.5 以前の wilcox.test（exact = FALSE, correct = TRUE）と同じ式で、小さい p も失わないように自前で計算する。
.two_sided <- function(z) 2 * min(stats::pnorm(z), stats::pnorm(z, lower.tail = FALSE))
wilcox_two <- function(x1, x2) {
  n1 <- length(x1); n2 <- length(x2); N <- n1 + n2; r <- rank(c(x1, x2))
  W <- sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2; z <- W - n1 * n2 / 2
  tt <- table(r); sigma <- sqrt(n1 * n2 / 12 * ((N + 1) - sum(tt^3 - tt) / (N * (N - 1))))
  z <- (z - sign(z) * 0.5) / sigma
  list(statistic = W, p.value = .two_sided(z), z = z)
}
wilcox_paired <- function(x, y) {
  d <- x - y; d <- d[d != 0]; n <- length(d); r <- rank(abs(d)); V <- sum(r[d > 0])
  z <- V - n * (n + 1) / 4; tt <- table(r); sigma <- sqrt(n * (n + 1) * (2 * n + 1) / 24 - sum(tt^3 - tt) / 48)
  z <- (z - sign(z) * 0.5) / sigma
  list(statistic = V, p.value = .two_sided(z), z = z)
}
# pairwise.wilcox.test(y, g, p.adjust.method = "holm", exact = FALSE) と同じ形の行列（行＝2 番目以降の群、列＝最後を除く群）
pairwise_wilcox_holm <- function(y, g) {
  lv <- levels(g); k <- length(lv)
  pm <- matrix(NA_real_, k - 1, k - 1, dimnames = list(lv[-1], lv[-k]))
  for (i in 2:k) for (j in seq_len(i - 1)) {
    a <- y[g == lv[i]]; b <- y[g == lv[j]]
    if (length(a) && length(b)) pm[i - 1, j] <- wilcox_two(a, b)$p.value
  }
  ok <- !is.na(pm); pm[ok] <- stats::p.adjust(pm[ok], method = "holm")
  pm
}
