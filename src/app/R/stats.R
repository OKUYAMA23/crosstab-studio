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
q <- function(s) paste0(tr_("「"), s, tr_("」"))
A <- function(x) I(x)   # JSON で必ず配列にする
mat <- function(m) { m <- unname(as.matrix(m)); storage.mode(m) <- "double"; m }

opt <- function(req, name, default) {
  v <- req$opts[[name]]
  if (is.null(v)) default else v
}

slot_labels <- function(slot) vapply(slot$cats, function(c) as.character(c$label), "")

slot_factor <- function(df, slot) {
  if (is.null(slot) || is.null(slot$col)) stop_user(tr_("列が選ばれていません。右の作業ウィンドウで列を選んでください。"))
  if (slot$col < 1 || slot$col > ncol(df)) stop_user(tr_("選んだ列がデータにありません。列を選び直してください。"))
  x <- df[[slot$col]]
  if (length(slot$cats) == 0) stop_user(q(slot$label), tr_(" の選択肢がすべて外されています。「選択肢の設定」で 1 つ以上にチェックを入れてください。"))
  lev <- unique(slot_labels(slot))
  keys <- character(0); vals <- character(0)
  for (c in slot$cats) for (v in unlist(c$values)) { keys <- c(keys, v); vals <- c(vals, as.character(c$label)) }
  factor(vals[match(x, keys)], levels = lev)
}

# 複数回答：行＝回答者、列＝選択肢の TRUE/FALSE 行列（無回答の行は NA）
slot_multi <- function(df, slot) {
  if (is.null(slot) || is.null(slot$col)) stop_user(tr_("複数回答の列が選ばれていません。"))
  x <- df[[slot$col]]
  if (length(slot$cats) == 0) stop_user(q(slot$label), tr_(" の選択肢がすべて外されています。"))
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

# 順序の回答の配点：選択肢（lev の並び）ごとの点。指定がなければ先頭を最も高い点（k〜1 点）にする
#   slot[["points"]] は slot$cats と同じ並びの点。項目の組では slot$pointsList[[j]] が j 番目の項目の点
lev_points <- function(slot, lev, j = NULL) {
  pts <- if (!is.null(j) && !is.null(slot$pointsList)) slot$pointsList[[j]] else if (!is.null(slot[["points"]])) slot[["points"]] else if (length(slot$pointsList)) slot$pointsList[[1]]
  if (is.null(pts)) return(as.numeric(rev(seq_along(lev))))
  p <- vapply(pts, function(x) if (is.null(x)) NA_real_ else as.numeric(x), 0)
  out <- p[match(lev, slot_labels(slot))]
  if (anyNA(out)) stop_user(q(slot$label), tr_("の配点に、点が入っていない選択肢があります。「データ」の列の設定で、すべての選択肢に点を入れてください。"))
  out
}
# 点の高い側の選択肢（「上位」として文に書く）
top_label <- function(lev, pts) lev[which.max(pts)]
# 順序の得点（数値の列はそのままの値）
slot_score <- function(df, slot) {
  if (identical(slot$type, "numeric")) return(numeric_values(df[[slot$col]], slot$fix, slot$unit))
  f <- slot_factor(df, slot)
  lev_points(slot, levels(f))[as.integer(f)]
}

`%||%` <- function(a, b) if (is.null(a)) b else a
# 解析で尺度得点（または日時で分けた回の列）を使ったとき、その作り方を「方法」に書く
score_method <- function(res, body) {
  if (!isTRUE(res$ok) || !length(body$computed)) return(res)
  used <- unlist(lapply(body$slots, function(s) c(s$col, unlist(s$cols))))
  txt <- character(0)
  for (d in body$computed) if (as.integer(d$col) %in% used) {
    if (identical(d$kind, "period")) {
      nm <- unlist(d$names); br <- unlist(d$breaks)
      rng <- vapply(seq_along(nm), function(k) {
        if (k == 1) trf("%sより前", period_label(br[1])) else if (k == length(nm)) trf("%s以降", period_label(br[k - 1]))
        else trf("%sから%sの前まで", period_label(br[k - 1]), period_label(br[k]))
      }, "")
      txt <- c(txt, trf("%sは、%sの日時で回答を分けて作った（%s）。", q(d$name), q(d$srcLabel %||% ""), paste(paste0(nm, tr_("："), rng), collapse = tr_("、"))))
      if (isTRUE(d$same) && length(unique(used)) >= 2 && !is.null(res$p)) {   # 回どうしを比べる検定のときだけ書き添える
        res$report <- paste0(res$report, if (identical(LANG, "en")) " " else "", trf("なお、回答者を特定できず、同じ人が複数の回に回答している可能性があるため、%sの間の比較は各回の回答を独立した群とみなした近似である。", q(d$name)))
        res$notes <- c(res$notes, trf("%sは同じ人たちが複数の回に答えているため、検定は各回の回答を独立した群として扱った近似です。同じ人の回答どうしに正の相関があると、p 値は大きめに（有意差は出にくい側に）なります。集計の表や図はそのまま読めます。", q(d$name)))
      }
      next
    }
    rev <- unlist(d$reverse); its <- vapply(d$items, function(s) paste0(tr_("「"), s$label, tr_("」"), if (s$col %in% rev) tr_("（逆転）") else ""), "")
    ks <- unique(vapply(Filter(function(s) identical(s$type, "ordinal"), d$items), function(s) length(s$cats), 1L))
    sc <- if (length(ks)) sprintf(tr_("順序のある項目は選択肢の並びの先頭を高い点とし（%s）、"), paste(sprintf(tr_("%d 段階の項目は %d〜1 点"), ks, ks), collapse = tr_("、"))) else ""
    txt <- c(txt, sprintf(tr_("「%s」は、%sの %d 項目の%s点とした。%s%sすべての項目に回答した人だけを対象とした。"),
      d$name, paste(its, collapse = tr_("・")), length(its), tolower(if (identical(d$method, "sum")) tr_("合計") else tr_("平均")), sc,
      if (length(rev)) tr_("逆転項目は点数を反対にしてから計算した。") else ""))
  }
  if (length(txt)) res$method <- paste(c(txt, res$method), collapse = "\n")
  res$report <- trimws(res$report)
  if (length(res$notes)) res$notes <- A(res$notes)
  res
}
# 日時の文字を、1970-01-01 0:00 からの分に直す（時刻帯は見ない。読めないものは NA）
#   年/月/日（Google フォームの日本語の形、Excel を直した形）と、月/日/年・日/月/年（英語の形）を読む。午前・午後、AM・PM にも対応
time_minutes <- function(x) {
  x <- trimws(as.character(x)); n <- length(x); y <- mo <- d <- rep(NA_real_, n)
  m1 <- regmatches(x, regexec("^([0-9]{4})[/.-]([0-9]{1,2})[/.-]([0-9]{1,2})", x)); ok1 <- lengths(m1) >= 4
  if (any(ok1)) { v <- matrix(as.numeric(unlist(lapply(m1[ok1], function(z) z[2:4]))), ncol = 3, byrow = TRUE); y[ok1] <- v[, 1]; mo[ok1] <- v[, 2]; d[ok1] <- v[, 3] }
  m2 <- regmatches(x, regexec("^([0-9]{1,2})[/.-]([0-9]{1,2})[/.-]([0-9]{4})", x)); ok2 <- !ok1 & lengths(m2) >= 4
  if (any(ok2)) {
    v <- matrix(as.numeric(unlist(lapply(m2[ok2], function(z) z[2:4]))), ncol = 3, byrow = TRUE)
    dm <- any(v[, 1] > 12)                     # 先頭が 12 を超えるものがあれば 日/月/年、なければ 月/日/年（英語の Google フォーム）
    y[ok2] <- v[, 3]; mo[ok2] <- if (dm) v[, 2] else v[, 1]; d[ok2] <- if (dm) v[, 1] else v[, 2]
  }
  hh <- mi <- ss <- rep(0, n)
  m3 <- regmatches(x, regexec("([0-9]{1,2}):([0-9]{2})(:([0-9]{2}))?", x)); ok3 <- lengths(m3) >= 3
  if (any(ok3)) { v <- matrix(as.numeric(unlist(lapply(m3[ok3], function(z) c(z[2], z[3], if (nzchar(z[5])) z[5] else "0")))), ncol = 3, byrow = TRUE); hh[ok3] <- v[, 1]; mi[ok3] <- v[, 2]; ss[ok3] <- v[, 3] }
  pm <- grepl("午後|\\bPM\\b|\\bp\\.m\\.", x, ignore.case = TRUE); am <- grepl("午前|\\bAM\\b|\\ba\\.m\\.", x, ignore.case = TRUE)
  hh <- ifelse(pm & hh < 12, hh + 12, ifelse(am & hh == 12, 0, hh))
  ok <- !is.na(y) & !is.na(mo) & mo >= 1 & mo <= 12 & d >= 1 & d <= 31
  out <- rep(NA_real_, n); out[ok] <- days_from_civil(y[ok], mo[ok], d[ok]) * 1440 + hh[ok] * 60 + mi[ok] + ss[ok] / 60
  out
}
# 区切りの日時（"YYYY-MM-DDTHH:MM"）を、報告書の文で読む形にする
period_label <- function(b) {
  t <- time_minutes(b); if (is.na(t)) return(b)
  z <- floor(t / 1440); mn <- round(t - z * 1440); c <- civil_from_days(z)
  hm <- if (mn == 0) "" else sprintf(" %d:%02d", mn %/% 60, mn %% 60)    # 0:00 のときは日付だけにする
  if (identical(LANG, "en")) sprintf("%d/%02d/%02d%s", c[1, 1], c[1, 2], c[1, 3], hm) else sprintf("%d年%d月%d日%s", c[1, 1], c[1, 2], c[1, 3], hm)
}
# 尺度得点の列を、元のデータの右に足す（画面で作った定義を、依頼のたびに受け取って計算する）
#   def：col（足す列の番号）、name、method（"mean" か "sum"）、items（各項目の slot）、reverse（逆転する項目の列番号）
#   すべての項目に答えた人だけ得点を出し、ほかは欠損にする。逆転項目は（段階の数 + 1 − 得点）にする
add_computed <- function(df, defs) {
  for (d in defs) {
    if (identical(d$kind, "period")) {            # 日時で分けた回：区切りの日時の前後で、回の名前を付ける
      t <- time_minutes(df[[as.integer(d$src)]]); br <- time_minutes(unlist(d$breaks)); nm <- unlist(d$names)
      v <- ifelse(is.na(t), NA_character_, nm[findInterval(t, br) + 1L])
      col <- as.integer(d$col)
      while (ncol(df) < col - 1L) df[[paste0(".pad", ncol(df) + 1L)]] <- NA_character_
      df[[col]] <- v; names(df)[col] <- if (is.null(d$name)) paste0("period", col) else d$name
      next
    }
    items <- d$items; rev <- unlist(d$reverse)
    M <- vapply(items, function(s) {
      x <- slot_score(df, s)
      if (s$col %in% rev && !identical(s$type, "numeric")) { pp <- lev_points(s, unique(slot_labels(s))); x <- min(pp) + max(pp) - x }
      as.numeric(x)
    }, numeric(nrow(df)))
    if (!is.matrix(M)) M <- matrix(M, ncol = length(items))
    ok <- stats::complete.cases(M)
    v <- rep(NA_real_, nrow(df)); v[ok] <- if (identical(d$method, "sum")) rowSums(M[ok, , drop = FALSE]) else rowMeans(M[ok, , drop = FALSE])
    col <- as.integer(d$col)
    while (ncol(df) < col - 1L) df[[paste0(".pad", ncol(df) + 1L)]] <- NA_character_
    df[[col]] <- v; names(df)[col] <- if (is.null(d$name)) paste0("score", col) else d$name
  }
  df
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
    trf("%sで最も多い回答は、いずれの群でも%sであった（%s）。", q(colLabel), q(c1),
        paste0(rownames(tab), " ", fmt_pct(rp[cbind(seq_len(nrow(tab)), mi)]), "%", collapse = tr_("、")))
  } else {
    trf("%sで最も多い回答は、%sであった。", q(colLabel),
        paste(trf("%sでは%s（%s%%）", rownames(tab), q(colnames(tab)[mi]), vapply(seq_len(nrow(tab)), function(i) fmt_pct(rp[i, mi[i]]), "")), collapse = tr_("、")))
  }
}

# 残差が基準を超えて大きいセルを述べる文（残差の大きい順、最大 6 件）
resid_sentence <- function(xt, tab) {
  if (is.null(xt$resid)) return("")
  R <- xt$resid; rp <- prop.table(tab, 1)
  idx <- which(R > xt$crit, arr.ind = TRUE)
  basis <- paste0(tr_("|調整済み標準化残差| > "), fmt_num(xt$crit, 2), if (identical(xt$residMode, "bonf")) tr_("、ボンフェローニ法で補正") else "")
  if (nrow(idx) == 0) return(trf("ただし、基準（%s）を超えるセルはなかった。", basis))
  idx <- idx[order(-R[idx]), , drop = FALSE]
  more <- nrow(idx) > 6
  if (more) idx <- idx[1:6, , drop = FALSE]
  parts <- vapply(seq_len(nrow(idx)), function(k) {
    i <- idx[k, 1]; j <- idx[k, 2]
    trf("%sでは%s（%s%%）", rownames(tab)[i], q(colnames(tab)[j]), fmt_pct(rp[i, j]))
  }, "")
  trf("残差分析（%s）では、%sの割合が期待される値より有意に高かった%s", basis, paste(parts, collapse = tr_("、")),
      if (more) tr_("（残差の大きい順に 6 件を記載）。") else tr_("。"))
}

median_label <- function(score, lev, pts = rev(seq_along(lev))) {
  m <- stats::median(score)
  if (m %in% pts) return(q(lev[match(m, pts)]))
  lo <- max(pts[pts < m]); hi <- min(pts[pts > m])
  trf("%sと%sの間", q(lev[match(hi, pts)]), q(lev[match(lo, pts)]))
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
