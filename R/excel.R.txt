# excel.R ― 読み込んだ CSV を 1 枚のシートにし、その右側（1 列あけた先）に、
# 選んだ解析を Excel の関数で再現するセルを書く。計算結果の値は貼り付けず、すべて数式にする。
# R が作るのはブックの中身（XML）まで。ZIP にまとめて .xlsx にするのは画面側で行う。

XL_MARK <- "\u0001"                       # 行番号を入れる場所の目印
xl_L <- function(i) vapply(i, function(k) { s <- ""; while (k > 0) { r <- (k - 1) %% 26; s <- paste0(LETTERS[r + 1], s); k <- (k - 1) %/% 26 }; s }, "")
xl_esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
xl_q <- function(s) paste0('"', gsub('"', '""', s, fixed = TRUE), '"')          # 数式の中の文字列
ad <- function(r, c) paste0("$", xl_L(c), "$", r)
rg <- function(r1, c1, r2, c2) paste0(ad(r1, c1), ":", ad(r2, c2))
FN <- function(name) paste0("_xlfn.", name)                                     # Excel 2010 以降の関数に付ける接頭辞

xl_new <- function() { W <- new.env(); W$chunks <- list(); W$widths <- list(); W$checks <- list(); W$notes <- character(0); W }
xl_put <- function(W, row, col, kind, val, style = 0L) {
  n <- max(length(row), length(col), length(val)); if (n == 0) return(invisible())
  W$chunks[[length(W$chunks) + 1L]] <- data.frame(row = rep_len(as.integer(row), n), col = rep_len(as.integer(col), n), kind = rep_len(kind, n),
                                                  val = rep_len(as.character(val), n), style = rep_len(as.integer(style), n), stringsAsFactors = FALSE)
  invisible()
}
S_ <- function(W, r, c, text, style = 0L) xl_put(W, r, c, "s", text, style)
N_ <- function(W, r, c, num, style = 0L) xl_put(W, r, c, "n", num, style)
F_ <- function(W, r, c, f, style = 0L) xl_put(W, r, c, "f", f, style)
xl_width <- function(W, col, w) W$widths[[as.character(col)]] <- w
xl_check <- function(W, label, r, c, expect) W$checks[[length(W$checks) + 1L]] <- list(label = label, cell = paste0(xl_L(c), r), expect = expect)
# 行ごとの数式：目印を行番号に置き換えて、データの全行に書く
xl_rows <- function(W, X, col, tmpl, style = 0L) {
  pieces <- strsplit(paste0(tmpl, "\u0002"), XL_MARK, fixed = TRUE)[[1]]; rows <- 2:X$last   # 末尾に番兵を足して、最後の区切りが落ちないようにする
  out <- rep(pieces[1], length(rows)); for (p in pieces[-1]) out <- paste0(out, rows, p)
  F_(W, rows, col, substr(out, 1, nchar(out) - 1L), style)
}
rowref <- function(col) paste0("$", xl_L(col), XL_MARK)

xl_vals <- function(slot, label) { label <- as.character(label); unlist(lapply(slot$cats, function(c) if (identical(as.character(c$label), label)) as.character(unlist(c$values)))) }
xl_levels <- function(slot) unique(vapply(slot$cats, function(c) as.character(c$label), ""))
# 「この回答（まとめた選択肢）に当てはまるか」を表す式。ref は範囲でも 1 つのセルでもよい
xl_member <- function(ref, vals, multi = FALSE, delim = ";") {
  if (multi) {
    d <- gsub(" ", "", delim, fixed = TRUE); v <- gsub(" ", "", vals, fixed = TRUE)
    p <- sprintf('ISNUMBER(FIND(%s,%s&SUBSTITUTE(%s," ","")&%s))', xl_q(paste0(d, v, d)), xl_q(d), ref, xl_q(d))
    if (length(p) > 1) paste0("((", paste(p, collapse = "+"), ")>0)") else p
  } else {
    p <- sprintf("(%s=%s)", ref, xl_q(vals))
    if (length(p) > 1) paste0("(", paste(p, collapse = "+"), ")") else p
  }
}
xl_filter <- function(flt, ref) {
  if (is.null(flt) || length(flt$values) == 0) return(character(0))
  multi <- identical(flt$type, "multi"); e <- xl_member(ref, unlist(flt$values), multi, if (is.null(flt$delim)) ";" else flt$delim)
  if (!multi && length(flt$values) > 1) e <- paste0("(", e, ">0)")
  e
}
xl_count <- function(terms) paste0("SUMPRODUCT(", paste(c(terms, "1"), collapse = "*"), ")")
# 表を書く。fcell(i, j) がセルの数式を返す
xl_table <- function(W, r0, c0, corner, rown, coln, fcell, total = TRUE, style = 0L) {
  R <- length(rown); C <- length(coln)
  S_(W, r0, c0, corner, 6L); S_(W, r0, c0 + seq_len(C), coln, 6L); S_(W, r0 + seq_len(R), c0, rown, 6L)
  for (i in seq_len(R)) F_(W, r0 + i, c0 + seq_len(C), vapply(seq_len(C), function(j) fcell(i, j), ""), style)
  t <- list(r0 = r0, c0 = c0, R = R, C = C, rng = rg(r0 + 1, c0 + 1, r0 + R, c0 + C), cell = function(i, j) ad(r0 + i, c0 + j),
            row = function(i) rg(r0 + i, c0 + 1, r0 + i, c0 + C), colr = function(j) rg(r0 + 1, c0 + j, r0 + R, c0 + j), nxt = r0 + R + 2)
  if (total) {
    S_(W, r0, c0 + C + 1, "合計", 6L); S_(W, r0 + R + 1, c0, "合計", 6L)
    F_(W, r0 + seq_len(R), c0 + C + 1, vapply(seq_len(R), function(i) paste0("SUM(", t$row(i), ")"), ""), style)
    F_(W, r0 + R + 1, c0 + seq_len(C), vapply(seq_len(C), function(j) paste0("SUM(", t$colr(j), ")"), ""), style)
    F_(W, r0 + R + 1, c0 + C + 1, paste0("SUM(", t$rng, ")"), style)
    t$rtot <- function(i) ad(r0 + i, c0 + C + 1); t$ctot <- function(j) ad(r0 + R + 1, c0 + j); t$grand <- ad(r0 + R + 1, c0 + C + 1)
    t$rtots <- rg(r0 + 1, c0 + C + 1, r0 + R, c0 + C + 1); t$nxt <- r0 + R + 3
  }
  t
}
xl_stat <- function(W, r, c, label, f, style = 3L) { S_(W, r, c, label, 6L); F_(W, r, c + 1, f, style); ad(r, c + 1) }
xl_note <- function(W, r, c, text) S_(W, r, c, text, 7L)
xl_hyper_p <- function(a, r1, c1, n) {
  k <- sprintf('(ROW(INDIRECT((MAX(0,%s+%s-%s)+1)&":"&(MIN(%s,%s)+1)))-1)', r1, c1, n, r1, c1)
  h <- function(x) sprintf("%s(%s,%s,%s,%s,FALSE)", FN("HYPGEOM.DIST"), x, r1, c1, n)
  sprintf("SUMPRODUCT((%s<=%s*(1+0.0000001))*%s)", h(k), h(a), h(k))
}
# 元の回答と得点（または群の番号）の対応表を書く
xl_keys <- function(W, r, c, slot, title, levels = NULL, score = TRUE) {
  lev <- if (is.null(levels)) xl_levels(slot) else levels; k <- length(lev)
  vals <- character(0); labs <- character(0)
  for (ct in slot$cats) if (as.character(ct$label) %in% lev) for (v in unlist(ct$values)) { vals <- c(vals, v); labs <- c(labs, as.character(ct$label)) }
  code <- if (score) k + 1L - match(labs, lev) else match(labs, lev)
  S_(W, r, c, title, 1L); S_(W, r + 1, c + 0:2, c("元の回答", "まとめた名前", if (score) "得点" else "番号"), 6L)
  m <- length(vals); S_(W, r + 1 + seq_len(m), c, vals); S_(W, r + 1 + seq_len(m), c + 1, labs); N_(W, r + 1 + seq_len(m), c + 2, code)
  list(keys = rg(r + 2, c, r + 1 + m, c), codes = rg(r + 2, c + 2, r + 1 + m, c + 2), lev = lev, k = k, nxt = r + m + 3)
}
xl_valid <- function(slot, cell, keys) if (identical(slot$type, "numeric")) paste0("ISNUMBER(", cell, ")") else paste0("SUMPRODUCT((", keys$keys, "=", cell, ")*1)>0")
xl_value <- function(slot, cell, keys) if (identical(slot$type, "numeric")) cell else paste0("SUMPRODUCT((", keys$keys, "=", cell, ")*", keys$codes, ")")
xl_and <- function(...) { t <- c(...); if (length(t) == 1) t else paste0("AND(", paste(t, collapse = ","), ")") }

# ---------------------------------------------------------------- 解析ごとの書き方（戻り値は使った列の数）
xl_freq <- function(W, X, req, res, c0) {
  s <- req$slots$a; ref <- X$rng(s$col); flt <- xl_filter(req$filter, X$rng(req$filter$col)); multi <- identical(s$type, "multi")
  rown <- unlist(res$grids[[1]]$rows); R <- length(rown); r <- 3
  S_(W, r, c0 + 0:2, c(s$label, "人数", if (multi) "回答者に占める割合 (%)" else "割合 (%)"), 6L)
  base <- if (multi) ad(r + R + 1, c0 + 1) else ad(r + R + 1, c0 + 1)
  for (i in seq_len(R)) {
    S_(W, r + i, c0, rown[i], 6L)
    F_(W, r + i, c0 + 1, xl_count(c(xl_member(ref, xl_vals(s, rown[i]), multi, s$delim), flt)))
    F_(W, r + i, c0 + 2, paste0(ad(r + i, c0 + 1), "/", base, "*100"), 4L)
  }
  cnt <- rg(r + 1, c0 + 1, r + R, c0 + 1)
  S_(W, r + R + 1, c0, if (multi) "回答者数" else "合計", 6L)
  F_(W, r + R + 1, c0 + 1, if (multi) xl_count(c(paste0("(", ref, '<>"")'), flt)) else paste0("SUM(", cnt, ")"))
  r <- r + R + 3
  if (!multi && isTRUE(req$opts$gof)) {
    e <- paste0(base, "/", R)
    chi <- xl_stat(W, r, c0, "χ²（適合度）", sprintf("SUMPRODUCT((%s-%s)^2/(%s))", cnt, e, e), 5L)
    df <- xl_stat(W, r + 1, c0, "自由度", as.character(R - 1), 0L)
    xl_stat(W, r + 2, c0, "p", sprintf("%s(%s,%s)", FN("CHISQ.DIST.RT"), chi, df)); xl_check(W, "freq gof p", r + 2, c0 + 1, res$p)
  }
  xl_width(W, c0, 34); 3
}

xl_crosstab <- function(W, X, req, res, c0, method, extra = character(0)) {
  sa <- req$slots$a; sb <- req$slots$b; flt <- c(xl_filter(req$filter, X$rng(req$filter$col)), extra)
  rown <- unlist(res$xtab$rows); coln <- unlist(res$xtab$cols); R <- length(rown); C <- length(coln)
  A <- vapply(rown, function(l) xl_member(X$rng(sa$col), xl_vals(sa, l)), ""); B <- vapply(coln, function(l) xl_member(X$rng(sb$col), xl_vals(sb, l)), "")
  S_(W, 3, c0, "人数", 1L)
  t <- xl_table(W, 4, c0, paste0(sa$label, " ＼ ", sb$label), rown, coln, function(i, j) xl_count(c(A[i], B[j], flt)))
  r <- t$nxt; S_(W, r, c0, "行の割合 (%)", 1L)
  t2 <- xl_table(W, r + 1, c0, "", rown, coln, function(i, j) paste0(t$cell(i, j), "/", t$rtot(i), "*100"), FALSE, 4L)
  r <- t2$nxt; S_(W, r, c0, "期待度数", 1L)
  te <- xl_table(W, r + 1, c0, "", rown, coln, function(i, j) paste0(t$rtot(i), "*", t$ctot(j), "/", t$grand), FALSE, 4L)
  r <- te$nxt; S_(W, r, c0, "調整済み標準化残差", 1L)
  tr <- xl_table(W, r + 1, c0, "", rown, coln, function(i, j) sprintf("(%s-%s)/SQRT(%s*(1-%s/%s)*(1-%s/%s))", t$cell(i, j), te$cell(i, j), te$cell(i, j), t$rtot(i), t$grand, t$ctot(j), t$grand), FALSE, 5L)
  r <- tr$nxt
  n <- xl_stat(W, r, c0, "n", t$grand, 0L)
  chi0 <- xl_stat(W, r + 1, c0, "χ²", sprintf("SUMPRODUCT((%s-%s)^2/%s)", t$rng, te$rng, te$rng), 5L)
  df <- xl_stat(W, r + 2, c0, "自由度", sprintf("(ROWS(%s)-1)*(COLUMNS(%s)-1)", t$rng, t$rng), 0L)
  xl_stat(W, r + 3, c0, if (R == 2 && C == 2) "φ" else "Cramér の V", sprintf("SQRT(%s/(%s*MIN(ROWS(%s)-1,COLUMNS(%s)-1)))", chi0, n, t$rng, t$rng), 5L)
  r <- r + 4
  if (method == "chisq") {
    chi <- chi0
    if (grepl("イェーツ", res$test)) {
      chi <- xl_stat(W, r, c0, "χ²（イェーツの補正）", sprintf("SUMPRODUCT((ABS(%s-%s)-MIN(0.5,ABS(%s-%s)))^2/%s)", t$rng, te$rng, t$cell(1, 1), te$cell(1, 1), te$rng), 5L); r <- r + 1
    }
    xl_stat(W, r, c0, "p（カイ二乗検定）", sprintf("%s(%s,%s)", FN("CHISQ.DIST.RT"), chi, df)); xl_check(W, "chisq p", r, c0 + 1, res$p)
  } else if (R == 2 && C == 2) {
    xl_stat(W, r, c0, "p（フィッシャーの正確確率検定）", xl_hyper_p(t$cell(1, 1), t$rtot(1), t$ctot(1), t$grand)); xl_check(W, "fisher p", r, c0 + 1, res$p)
    xl_stat(W, r + 1, c0, "オッズ比（標本の値 ad/bc）", sprintf("%s*%s/(%s*%s)", t$cell(1, 1), t$cell(2, 2), t$cell(1, 2), t$cell(2, 1)), 5L)
    xl_note(W, r + 2, c0, "アプリのオッズ比は条件付き最尤推定値のため、標本の値とは少し異なります。")
  } else {
    xl_note(W, r, c0, "2×2 より大きい表のフィッシャーの正確確率検定は Excel の関数では再現できないため、p は載せていません（アプリの R で計算した値を使ってください）。")
    W$notes <- c(W$notes, "2×2 より大きい表のフィッシャーの正確確率検定")
  }
  xl_width(W, c0, 34); C + 2
}

# 順位による群の比較（マン・ホイットニー、クラスカル・ウォリス）
xl_rank <- function(W, X, req, res, c0, kw) {
  sg <- req$slots$g; sy <- req$slots$y; glev <- unlist(res$grids[[1]]$rows); G <- length(glev); num <- identical(sy$type, "numeric")
  h <- c0; s <- c0 + 3                                              # 補助の列 3 本（群・得点・順位）のあとに結果を書く
  S_(W, 1, h + 0:2, c("群の番号", if (num) "値" else "得点", "順位"), 6L)
  gk <- xl_keys(W, 3, s, sg, paste0("群：", sg$label), glev, FALSE); r <- gk$nxt
  yk <- NULL; if (!num) { yk <- xl_keys(W, r, s, sy, paste0("得点：", sy$label)); r <- yk$nxt }
  fl <- xl_filter(req$filter, rowref(req$filter$col))
  gexp <- paste0("SUMPRODUCT((", gk$keys, "=", rowref(sg$col), ")*", gk$codes, ")")
  xl_rows(W, X, h, if (length(fl)) paste0("IF(", fl, ",", gexp, ",0)") else gexp)
  gc <- paste0("$", xl_L(h), XL_MARK); sc <- paste0("$", xl_L(h + 1), XL_MARK)
  xl_rows(W, X, h + 1, paste0("IF(", xl_and(paste0(gc, ">0"), xl_valid(sy, rowref(sy$col), yk)), ",", xl_value(sy, rowref(sy$col), yk), ',"")'))
  scR <- rg(2, h + 1, X$last, h + 1); gcR <- rg(2, h, X$last, h); rkR <- rg(2, h + 2, X$last, h + 2)
  xl_rows(W, X, h + 2, paste0("IF(ISNUMBER(", sc, "),", FN("RANK.AVG"), "(", sc, ",", scR, ',1),"")'))
  S_(W, r, s + 0:3, c(sg$label, "n", "順位の合計", "平均順位"), 6L)
  for (i in seq_len(G)) {
    S_(W, r + i, s, glev[i], 6L)
    F_(W, r + i, s + 1, sprintf('COUNTIFS(%s,%d,%s,">0")', gcR, i, rkR)); F_(W, r + i, s + 2, sprintf("SUMIFS(%s,%s,%d)", rkR, gcR, i), 4L)
    F_(W, r + i, s + 3, paste0(ad(r + i, s + 2), "/", ad(r + i, s + 1)), 5L)
  }
  nR <- rg(r + 1, s + 1, r + G, s + 1); rsR <- rg(r + 1, s + 2, r + G, s + 2); r <- r + G + 2
  N <- xl_stat(W, r, s, "N", paste0("SUM(", nR, ")"), 0L)
  Tt <- xl_stat(W, r + 1, s, "同順位の補正項 Σ(t³−t)", sprintf("SUMPRODUCT(ISNUMBER(%s)*(COUNTIF(%s,%s)^2-1))", scR, scR, scR), 0L)
  r <- r + 2
  if (!kw) {
    n1 <- ad(r - G - 3, s + 1); n2 <- ad(r - G - 2, s + 1); R1 <- ad(r - G - 3, s + 2)
    U1 <- xl_stat(W, r, s, "U（1 つ目の群）", sprintf("%s-%s*(%s+1)/2", R1, n1, n1), 4L)
    xl_stat(W, r + 1, s, "U（小さいほう）", sprintf("MIN(%s,%s*%s-%s)", U1, n1, n2, U1), 4L)
    sg2 <- xl_stat(W, r + 2, s, "標準偏差", sprintf("SQRT(%s*%s/12*((%s+1)-%s/(%s*(%s-1))))", n1, n2, N, Tt, N, N), 5L)
    Z <- xl_stat(W, r + 3, s, "Z（連続性の補正あり）", sprintf("(%s-%s*%s/2-SIGN(%s-%s*%s/2)*0.5)/%s", U1, n1, n2, U1, n1, n2, sg2), 5L)
    xl_stat(W, r + 4, s, "p", sprintf("2*%s(-ABS(%s),TRUE)", FN("NORM.S.DIST"), Z)); xl_check(W, "mwu p", r + 4, s + 1, res$p)
    xl_stat(W, r + 5, s, "効果量 r", sprintf("ABS(%s)/SQRT(%s)", Z, N), 5L)
  } else {
    H0 <- xl_stat(W, r, s, "H（補正前）", sprintf("12/(%s*(%s+1))*SUMPRODUCT(%s^2/%s)-3*(%s+1)", N, N, rsR, nR, N), 5L)
    H <- xl_stat(W, r + 1, s, "H", sprintf("%s/(1-%s/(%s^3-%s))", H0, Tt, N, N), 5L)
    df <- xl_stat(W, r + 2, s, "自由度", as.character(G - 1), 0L)
    xl_stat(W, r + 3, s, "p", sprintf("%s(%s,%s)", FN("CHISQ.DIST.RT"), H, df)); xl_check(W, "kw p", r + 3, s + 1, res$p)
    xl_stat(W, r + 4, s, "効果量 ε²", sprintf("%s/(%s-1)", H, N), 5L)
    if (G >= 3) {                                               # 多重比較：2 群ずつ、その 2 群の中で順位を付け直してマン・ホイットニーの U 検定を行い、ホルム法で補正する
      rp <- r + 6; S_(W, rp, s, "多重比較（マン・ホイットニーの U 検定、ホルム法で補正）", 1L)
      S_(W, rp + 1, s + 0:5, c("組み合わせ", "順位の合計（前の群）", "同順位の補正項", "Z", "p", "補正後の p（ホルム法）"), 6L)
      pr <- utils::combn(G, 2); P <- ncol(pr); pR <- rg(rp + 2, s + 4, rp + 1 + P, s + 4); pv <- unlist(res$grids[[length(res$grids)]]$cells)
      ok <- paste0("ISNUMBER(", scR, ")")
      for (c in seq_len(P)) {
        a <- pr[1, c]; b <- pr[2, c]; rr <- rp + 1 + c; S_(W, rr, s, paste0(glev[a], " – ", glev[b]), 6L)
        lt <- sprintf('COUNTIFS(%s,"<"&%s,%s,%d)+COUNTIFS(%s,"<"&%s,%s,%d)', scR, scR, gcR, a, scR, scR, gcR, b)
        eq <- sprintf("(COUNTIFS(%s,%s,%s,%d)+COUNTIFS(%s,%s,%s,%d))", scR, scR, gcR, a, scR, scR, gcR, b)
        F_(W, rr, s + 1, sprintf("SUMPRODUCT((%s=%d)*%s*(%s+(%s+1)/2))", gcR, a, ok, lt, eq), 4L)
        F_(W, rr, s + 2, sprintf("SUMPRODUCT(((%s=%d)+(%s=%d))*%s*(%s^2-1))", gcR, a, gcR, b, ok, eq), 0L)
        na <- ad(r - G - 4 + a, s + 1); nb <- ad(r - G - 4 + b, s + 1); Ra <- ad(rr, s + 1); Tt2 <- ad(rr, s + 2)
        U <- sprintf("(%s-%s*(%s+1)/2-%s*%s/2)", Ra, na, na, na, nb); Nn <- sprintf("(%s+%s)", na, nb)
        F_(W, rr, s + 3, sprintf("(%s-SIGN(%s)*0.5)/SQRT(%s*%s/12*((%s+1)-%s/(%s*(%s-1))))", U, U, na, nb, Nn, Tt2, Nn, Nn), 5L)
        F_(W, rr, s + 4, sprintf("2*%s(-ABS(%s),TRUE)", FN("NORM.S.DIST"), ad(rr, s + 3)), 3L)
        F_(W, rr, s + 5, sprintf('MIN(1,SUMPRODUCT(MAX((%s<=%s)*(COUNT(%s)-COUNTIF(%s,"<"&%s))*%s)))', pR, ad(rr, s + 4), pR, pR, pR, pR), 3L)
        xl_check(W, paste0("kw holm[", c, "]"), rr, s + 5, pv[c])
      }
    }
  }
  xl_width(W, s, 30); if (kw && G >= 3) 9 else 7
}

xl_spearman <- function(W, X, req, res, c0) {
  sx <- req$slots$x; sy <- req$slots$y; h <- c0; s <- c0 + 4
  S_(W, 1, h + 0:3, c("回答 1 の値", "回答 2 の値", "回答 1 の順位", "回答 2 の順位"), 6L); r <- 3; xk <- NULL; yk <- NULL
  if (!identical(sx$type, "numeric")) { xk <- xl_keys(W, r, s, sx, paste0("得点：", sx$label)); r <- xk$nxt }
  if (!identical(sy$type, "numeric")) { yk <- xl_keys(W, r, s, sy, paste0("得点：", sy$label)); r <- yk$nxt }
  ok <- xl_and(xl_filter(req$filter, rowref(req$filter$col)), xl_valid(sx, rowref(sx$col), xk), xl_valid(sy, rowref(sy$col), yk))
  xl_rows(W, X, h, paste0("IF(", ok, ",", xl_value(sx, rowref(sx$col), xk), ',"")'))
  xl_rows(W, X, h + 1, paste0("IF(", ok, ",", xl_value(sy, rowref(sy$col), yk), ',"")'))
  for (k in 0:1) { v <- paste0("$", xl_L(h + k), XL_MARK); xl_rows(W, X, h + 2 + k, paste0("IF(ISNUMBER(", v, "),", FN("RANK.AVG"), "(", v, ",", rg(2, h + k, X$last, h + k), ',1),"")')) }
  rx <- rg(2, h + 2, X$last, h + 2); ry <- rg(2, h + 3, X$last, h + 3)
  n <- xl_stat(W, r, s, "n", paste0("COUNT(", rx, ")"), 0L)
  rho <- xl_stat(W, r + 1, s, "ρ（順位の相関係数）", sprintf("CORREL(%s,%s)", rx, ry), 5L)
  tt <- xl_stat(W, r + 2, s, "t", sprintf("%s*SQRT((%s-2)/(1-%s^2))", rho, n, rho), 5L)
  xl_stat(W, r + 3, s, "p", sprintf("%s(ABS(%s),%s-2)", FN("T.DIST.2T"), tt, n)); xl_check(W, "spearman p", r + 3, s + 1, res$p)
  xl_width(W, s, 30); 8
}

xl_wsr <- function(W, X, req, res, c0) {
  sx <- req$slots$x; sy <- req$slots$y; h <- c0; s <- c0 + 3
  S_(W, 1, h + 0:2, c("得点の差（回答 1 − 回答 2）", "差の絶対値（0 を除く）", "順位"), 6L)
  xk <- xl_keys(W, 3, s, sx, paste0("得点：", sx$label)); yk <- xl_keys(W, xk$nxt, s, sy, paste0("得点：", sy$label)); r <- yk$nxt
  ok <- xl_and(xl_filter(req$filter, rowref(req$filter$col)), xl_valid(sx, rowref(sx$col), xk), xl_valid(sy, rowref(sy$col), yk))
  xl_rows(W, X, h, paste0("IF(", ok, ",", xl_value(sx, rowref(sx$col), xk), "-", xl_value(sy, rowref(sy$col), yk), ',"")'))
  d <- paste0("$", xl_L(h), XL_MARK); a <- paste0("$", xl_L(h + 1), XL_MARK)
  xl_rows(W, X, h + 1, paste0("IF(ISNUMBER(", d, "),IF(", d, '=0,"",ABS(', d, ')),"")'))
  dR <- rg(2, h, X$last, h); aR <- rg(2, h + 1, X$last, h + 1); kR <- rg(2, h + 2, X$last, h + 2)
  xl_rows(W, X, h + 2, paste0("IF(ISNUMBER(", a, "),", FN("RANK.AVG"), "(", a, ",", aR, ',1),"")'))
  xl_stat(W, r, s, "n（両方に回答）", paste0("COUNT(", dR, ")"), 0L)
  nz <- xl_stat(W, r + 1, s, "差が 0 でない人数", paste0("COUNT(", aR, ")"), 0L)
  V <- xl_stat(W, r + 2, s, "V（差が正の順位の合計）", sprintf('SUMIFS(%s,%s,">0")', kR, dR), 4L)
  Tt <- xl_stat(W, r + 3, s, "同順位の補正項 Σ(t³−t)", sprintf("SUMPRODUCT(ISNUMBER(%s)*(COUNTIF(%s,%s)^2-1))", aR, aR, aR), 0L)
  sd <- xl_stat(W, r + 4, s, "標準偏差", sprintf("SQRT(%s*(%s+1)*(2*%s+1)/24-%s/48)", nz, nz, nz, Tt), 5L)
  Z <- xl_stat(W, r + 5, s, "Z（連続性の補正あり）", sprintf("(%s-%s*(%s+1)/4-SIGN(%s-%s*(%s+1)/4)*0.5)/%s", V, nz, nz, V, nz, nz, sd), 5L)
  xl_stat(W, r + 6, s, "p", sprintf("2*%s(-ABS(%s),TRUE)", FN("NORM.S.DIST"), Z)); xl_check(W, "wsr p", r + 6, s + 1, res$p)
  xl_width(W, s, 30); 7
}

xl_mcnemar <- function(W, X, req, res, c0) {
  sx <- req$slots$x; sy <- req$slots$y; lev <- xl_levels(sx); flt <- xl_filter(req$filter, X$rng(req$filter$col))
  A <- vapply(lev, function(l) xl_member(X$rng(sx$col), xl_vals(sx, l)), ""); B <- vapply(lev, function(l) xl_member(X$rng(sy$col), xl_vals(sy, l)), "")
  t <- xl_table(W, 3, c0, paste0(sx$label, " ＼ ", sy$label), lev, lev, function(i, j) xl_count(c(A[i], B[j], flt))); r <- t$nxt
  b <- t$cell(1, 2); cc <- t$cell(2, 1)
  if (grepl("正確", res$test)) xl_stat(W, r, c0, "p（正確二項検定）", sprintf("MIN(1,2*%s(MIN(%s,%s),%s+%s,0.5,TRUE))", FN("BINOM.DIST"), b, cc, b, cc))
  else { chi <- xl_stat(W, r, c0, "χ²（連続性の補正あり）", sprintf("(ABS(%s-%s)-1)^2/(%s+%s)", b, cc, b, cc), 5L); r <- r + 1; xl_stat(W, r, c0, "p", sprintf("%s(%s,1)", FN("CHISQ.DIST.RT"), chi)) }
  xl_check(W, "mcnemar p", r, c0 + 1, res$p); xl_width(W, c0, 34); 4
}

xl_trend <- function(W, X, req, res, c0) {
  sb <- req$slots$b; so <- req$slots$o; flt <- xl_filter(req$filter, X$rng(req$filter$col))
  rown <- unlist(res$xtab$rows); coln <- unlist(res$xtab$cols); R <- length(rown)
  A <- vapply(rown, function(l) xl_member(X$rng(so$col), xl_vals(so, l)), ""); B <- vapply(coln, function(l) xl_member(X$rng(sb$col), xl_vals(sb, l)), "")
  t <- xl_table(W, 3, c0, paste0(so$label, " ＼ ", sb$label), rown, coln, function(i, j) xl_count(c(A[i], B[j], flt)))
  cs <- c0 + length(coln) + 2; S_(W, 3, cs + 0:1, c("得点", paste0(coln[1], " の割合 (%)")), 6L)
  N_(W, 3 + seq_len(R), cs, seq_len(R)); F_(W, 3 + seq_len(R), cs + 1, vapply(seq_len(R), function(i) paste0(t$cell(i, 1), "/", t$rtot(i), "*100"), ""), 4L)
  sR <- rg(4, cs, 3 + R, cs); x <- t$colr(1); nn <- t$rtots; r <- t$nxt
  P <- xl_stat(W, r, c0, "全体の割合", paste0(t$ctot(1), "/", t$grand), 3L)
  sb2 <- xl_stat(W, r + 1, c0, "得点の平均", sprintf("SUMPRODUCT(%s,%s)/%s", nn, sR, t$grand), 5L)
  chi <- xl_stat(W, r + 2, c0, "χ²（傾向）", sprintf("SUMPRODUCT(%s*(%s-%s))^2/(%s*(1-%s)*SUMPRODUCT(%s*(%s-%s)^2))", x, sR, sb2, P, P, nn, sR, sb2), 5L)
  xl_stat(W, r + 3, c0, "p", sprintf("%s(%s,1)", FN("CHISQ.DIST.RT"), chi)); xl_check(W, "trend p", r + 3, c0 + 1, res$p)
  xl_width(W, c0, 34); length(coln) + 4
}

xl_multi <- function(W, X, req, res, c0) {
  sm <- req$slots$m; sg <- req$slots$g; g <- res$grids[[1]]; items <- unlist(g$rows); glev <- unlist(res$fig$series); G <- length(glev); K <- length(items)
  flt <- xl_filter(req$filter, X$rng(req$filter$col)); refM <- X$rng(sm$col)
  Gm <- vapply(glev, function(l) xl_member(X$rng(sg$col), xl_vals(sg, l)), "")
  r <- 3; S_(W, r, c0, sm$label, 6L); S_(W, r, c0 + seq_len(G), paste0(glev, " 人数"), 6L); S_(W, r, c0 + G + seq_len(G), paste0(glev, " %"), 6L)
  S_(W, r, c0 + 2 * G + 1:4, c("検定", "統計量 χ²", "p", "補正後の p（ホルム法）"), 6L)
  S_(W, r + 1, c0, "回答者数", 6L); F_(W, r + 1, c0 + seq_len(G), vapply(seq_len(G), function(i) xl_count(c(Gm[i], paste0("(", refM, '<>"")'), flt)), ""))
  ngR <- rg(r + 1, c0 + 1, r + 1, c0 + G); pcol <- c0 + 2 * G + 3; pR <- rg(r + 2, pcol, r + 1 + K, pcol); allnum <- TRUE; meth <- vapply(g$cells, function(c) as.character(c[[2 * G + 1]]), "")
  for (k in seq_len(K)) {
    rr <- r + 1 + k; S_(W, rr, c0, items[k], 6L); Mk <- xl_member(refM, xl_vals(sm, items[k]), TRUE, sm$delim)
    F_(W, rr, c0 + seq_len(G), vapply(seq_len(G), function(i) xl_count(c(Mk, Gm[i], flt)), ""))
    F_(W, rr, c0 + G + seq_len(G), vapply(seq_len(G), function(i) paste0(ad(rr, c0 + i), "/", ad(r + 1, c0 + i), "*100"), ""), 4L)
    cR <- rg(rr, c0 + 1, rr, c0 + G); S_(W, rr, c0 + 2 * G + 1, if (nzchar(meth[k])) meth[k] else "–")
    if (meth[k] == "カイ二乗検定") {
      P <- sprintf("(SUM(%s)/SUM(%s))", cR, ngR)
      F_(W, rr, c0 + 2 * G + 2, sprintf("SUMPRODUCT((%s-%s*%s)^2/(%s*%s*(1-%s)))", cR, ngR, P, ngR, P, P), 5L)
      F_(W, rr, pcol, sprintf("%s(%s,%d)", FN("CHISQ.DIST.RT"), ad(rr, c0 + 2 * G + 2), G - 1), 3L)
    } else if (meth[k] == "フィッシャーの正確確率検定" && G == 2) {
      F_(W, rr, pcol, xl_hyper_p(ad(rr, c0 + 1), ad(r + 1, c0 + 1), sprintf("SUM(%s)", cR), sprintf("SUM(%s)", ngR)), 3L)
    } else { allnum <- FALSE; if (nzchar(meth[k])) S_(W, rr, pcol, "Excel では再現できません", 7L) }
    if (nzchar(meth[k]) && (meth[k] == "カイ二乗検定" || G == 2)) xl_check(W, paste0("multi p[", k, "]"), rr, pcol, g$cells[[k]][[2 * G + 2]])
  }
  if (allnum) for (k in seq_len(K)) {
    rr <- r + 1 + k
    F_(W, rr, pcol + 1, sprintf('MIN(1,SUMPRODUCT(MAX((%s<=%s)*(COUNT(%s)-COUNTIF(%s,"<"&%s))*%s)))', pR, ad(rr, pcol), pR, pR, pR, pR), 3L)
    xl_check(W, paste0("multi holm[", k, "]"), rr, pcol + 1, g$cells[[k]][[2 * G + 3]])
  } else { xl_note(W, r + K + 3, c0, "一部の選択肢の検定を Excel の関数で再現できないため、ホルム法の補正は載せていません。"); W$notes <- c(W$notes, "複数回答の一部の選択肢の検定（3 群以上のフィッシャーの正確確率検定）") }
  xl_width(W, c0, 34); 2 * G + 5
}

# 複数の設問の単純集計：行＝設問、列＝回答の人数と割合
xl_freqmulti <- function(W, X, req, res, c0) {
  s <- req$slots$items; flt <- xl_filter(req$filter, X$rng(req$filter$col)); cols <- unlist(s$cols)
  rown <- unlist(res$xtab$rows); coln <- unlist(res$xtab$cols)
  t1 <- xl_table(W, 4, c0, "設問 ＼ 回答", rown, coln, function(i, j) xl_count(c(xl_member(X$rng(cols[i]), xl_vals(s, coln[j])), flt)))
  S_(W, 3, c0, "人数", 1L)
  r <- t1$nxt; S_(W, r, c0, "行の割合 (%)", 1L)
  xl_table(W, r + 1, c0, "", rown, coln, function(i, j) paste0(t1$cell(i, j), "/", t1$rtot(i), "*100"), FALSE, 4L)
  for (i in seq_along(rown)) xl_check(W, paste0("items n[", i, "]"), 4 + i, c0 + length(coln) + 1, sum(res$xtab$count[i, ]))
  used <- length(coln) + 3
  if (!is.null(res$p)) {                                            # フリードマン検定：すべての設問に答えた人について、1 人の中で設問に順位を付ける
    k <- length(cols); h <- c0 + used; r <- r + length(rown) + 3
    kk <- xl_keys(W, r, c0, s, "得点（すべての設問に共通）"); r <- kk$nxt
    xl_helphead(W, h, c(paste0("得点：", rown), paste0("順位：", rown), "同順位の補正項"))
    okall <- xl_and(xl_filter(req$filter, rowref(req$filter$col)), vapply(cols, function(j) xl_valid(s, rowref(j), kk), ""))
    for (j in seq_len(k)) xl_rows(W, X, h + j - 1, paste0("IF(", okall, ",", xl_value(s, rowref(cols[j]), kk), ',"")'))
    rowR <- paste0("$", xl_L(h), XL_MARK, ":$", xl_L(h + k - 1), XL_MARK)
    for (j in seq_len(k)) { v <- paste0("$", xl_L(h + j - 1), XL_MARK); xl_rows(W, X, h + k + j - 1, paste0("IF(ISNUMBER(", v, "),", FN("RANK.AVG"), "(", v, ",", rowR, ',1),"")')) }
    xl_rows(W, X, h + 2 * k, paste0("IF(ISNUMBER($", xl_L(h), XL_MARK, "),SUMPRODUCT(COUNTIF(", rowR, ",", rowR, ')^2-1),"")'))
    S_(W, r, c0 + 0:2, c("設問", "順位の合計", "平均順位"), 6L)
    n <- sprintf("COUNT(%s)", rg(2, h, X$last, h))
    for (j in seq_len(k)) { S_(W, r + j, c0, rown[j], 6L); F_(W, r + j, c0 + 1, sprintf("SUM(%s)", rg(2, h + k + j - 1, X$last, h + k + j - 1)), 4L); F_(W, r + j, c0 + 2, paste0(ad(r + j, c0 + 1), "/", n), 5L) }
    RjR <- rg(r + 1, c0 + 1, r + k, c0 + 1); r <- r + k + 2
    nn <- xl_stat(W, r, c0, "n（すべての設問に回答）", n, 0L)
    Tt <- xl_stat(W, r + 1, c0, "同順位の補正項 Σ(t³−t)", sprintf("SUM(%s)", rg(2, h + 2 * k, X$last, h + 2 * k)), 0L)
    Q <- xl_stat(W, r + 2, c0, "χ²（フリードマン）", sprintf("12*SUMPRODUCT((%s-%s*(%d+1)/2)^2)/(%s*%d*(%d+1)-%s/(%d-1))", RjR, nn, k, nn, k, k, Tt, k), 5L); xl_check(W, "friedman Q", r + 2, c0 + 1, res$chk$Q)
    df <- xl_stat(W, r + 3, c0, "自由度", as.character(k - 1), 0L)
    xl_stat(W, r + 4, c0, "p", sprintf("%s(%s,%s)", FN("CHISQ.DIST.RT"), Q, df)); xl_check(W, "friedman p", r + 4, c0 + 1, res$p)
    xl_stat(W, r + 5, c0, "Kendall の W", sprintf("%s/(%s*(%d-1))", Q, nn, k), 5L); xl_check(W, "friedman W", r + 5, c0 + 1, res$chk$W)
    used <- used + 2 * k + 2
  }
  xl_width(W, c0, 30); used
}
# ---------------------------------------------------------------- 数値（得点）を使う解析の共通部分
# 補助の列を書く：h 列＝群の番号（使わない行は 0）、h+1 列＝値（使わない行は ""）。群を選ばないときは、使う行を 1 にする
#   対応表（群・得点）は s 列の r 行目から書く
xl_gy <- function(W, X, req, h, s, sg, sy, glev, r) {
  gk <- NULL; if (!is.null(sg)) { gk <- xl_keys(W, r, s, sg, paste0("群：", sg$label), glev, FALSE); r <- gk$nxt }
  yk <- NULL; if (!identical(sy$type, "numeric")) { yk <- xl_keys(W, r, s, sy, paste0("得点：", sy$label)); r <- yk$nxt }
  ok <- xl_and(xl_filter(req$filter, rowref(req$filter$col)), xl_valid(sy, rowref(sy$col), yk))
  gexp <- if (is.null(sg)) "1" else paste0("SUMPRODUCT((", gk$keys, "=", rowref(sg$col), ")*", gk$codes, ")")
  xl_rows(W, X, h, paste0("IF(", ok, ",", gexp, ",0)"))
  gc <- paste0("$", xl_L(h), XL_MARK)
  xl_rows(W, X, h + 1, paste0("IF(", gc, ">0,", xl_value(sy, rowref(sy$col), yk), ',"")'))
  list(gk = gk, yk = yk, r = r, gc = gc, yc = paste0("$", xl_L(h + 1), XL_MARK), gR = rg(2, h, X$last, h), yR = rg(2, h + 1, X$last, h + 1))
}
# 群ごとの要約（n・平均・標準偏差など）。標準偏差は、偏差の 2 乗の列（dcol）を書いて求める（桁落ちを避けるため）
#   what：書く項目（n, mean, sd, min, q1, med, q3, max）。戻り値：各項目のセルを返す関数 cell(i, 項目) と次の行
xl_groupsum <- function(W, X, gy, r, s, glev, dcol, what, total = FALSE, corner = "") {
  G <- length(glev); nm <- c(n = "n", mean = "平均", sd = "標準偏差", min = "最小", q1 = "第1四分位", med = "中央値", q3 = "第3四分位", max = "最大")
  S_(W, r, s, corner, 6L); S_(W, r, s + seq_along(what), nm[what], 6L)
  col <- function(k) s + match(k, what)
  agg <- function(f, i, k) sprintf("%s(%d,6,%s/(%s=%d),%d)", FN("AGGREGATE"), f, gy$yR, gy$gR, i, k)
  for (i in seq_len(G)) {
    rr <- r + i; S_(W, rr, s, glev[i], 6L)
    f <- c(n = sprintf("COUNTIFS(%s,%d)", gy$gR, i), mean = sprintf("AVERAGEIFS(%s,%s,%d)", gy$yR, gy$gR, i),
           sd = sprintf("SQRT(SUMIFS(%s,%s,%d)/(%s-1))", rg(2, dcol, X$last, dcol), gy$gR, i, ad(rr, col("n"))),
           min = agg(15, i, 1), q1 = agg(17, i, 1), med = agg(17, i, 2), q3 = agg(17, i, 3), max = agg(14, i, 1))
    F_(W, rr, s + seq_along(what), f[what], ifelse(what == "n", 0L, 5L))
  }
  if (total) {
    rr <- r + G + 1; S_(W, rr, s, "全体", 6L); y <- gy$yR
    f <- c(n = paste0("COUNT(", y, ")"), mean = paste0("AVERAGE(", y, ")"), sd = paste0(FN("STDEV.S"), "(", y, ")"), min = paste0("MIN(", y, ")"),
           q1 = paste0(FN("QUARTILE.INC"), "(", y, ",1)"), med = paste0("MEDIAN(", y, ")"), q3 = paste0(FN("QUARTILE.INC"), "(", y, ",3)"), max = paste0("MAX(", y, ")"))
    F_(W, rr, s + seq_along(what), f[what], ifelse(what == "n", 0L, 5L))
  }
  # 偏差の 2 乗：その行の群の平均との差
  meanR <- rg(r + 1, col("mean"), r + G, col("mean"))
  xl_rows(W, X, dcol, paste0("IF(", gy$gc, ">0,(", gy$yc, "-INDEX(", meanR, ",", gy$gc, "))^2,\"\")"))
  list(cell = function(i, k) ad(r + i, col(k)), nxt = r + G + (if (total) 3 else 2), range = function(k) rg(r + 1, col(k), r + G, col(k)))
}
# t 分布・F 分布の p（自由度が整数でなくても正確に求めるため、ベータ分布で計算する。Excel の T.DIST.2T・F.DIST.RT は自由度の小数を切り捨てる）
xl_p_t <- function(t, df) sprintf("%s(%s/(%s+%s^2),%s/2,0.5,TRUE)", FN("BETA.DIST"), df, df, t, df)
xl_p_f <- function(F, d1, d2) sprintf("%s(%s/(%s+%s*%s),%s/2,%s/2,TRUE)", FN("BETA.DIST"), d2, d2, d1, F, d2, d1)
xl_t_crit <- function(df, a = "0.05") sprintf("SQRT(%s*(1-%s(%s,%s/2,0.5))/%s(%s,%s/2,0.5))", df, FN("BETA.INV"), a, df, FN("BETA.INV"), a, df)
xl_helphead <- function(W, h, labels) S_(W, 1, h + seq_along(labels) - 1L, labels, 6L)

# ---------------------------------------------------------------- 数値の要約とヒストグラム
xl_numsum <- function(W, X, req, res, c0) {
  sy <- req$slots$y; sg <- req$slots$g; glev <- if (is.null(sg)) "全体" else unlist(res$fig$series)
  h <- c0; s <- c0 + 4; xl_helphead(W, h, c("群の番号", "値", "偏差の 2 乗"))
  gy <- xl_gy(W, X, req, h, s, sg, sy, glev, 3); r <- gy$r
  S_(W, r, s, "要約統計量", 1L)
  what <- c("n", "mean", "sd", "min", "q1", "med", "q3", "max")
  gs <- xl_groupsum(W, X, gy, r + 1, s, if (is.null(sg)) sy$label else glev, h + 2, what, total = !is.null(sg), corner = if (is.null(sg)) "" else sg$label)
  M <- res$grids[[1]]$cells; G <- if (is.null(sg)) 1 else length(glev)
  for (i in seq_len(G)) for (k in seq_along(what)) xl_check(W, paste0("numsum ", what[k], "[", i, "]"), r + 1 + i, s + k, M[i, k])
  if (!is.null(sg)) for (k in seq_along(what)) xl_check(W, paste0("numsum total ", what[k]), r + 2 + G, s + k, M[G + 1, k])
  r <- gs$nxt; S_(W, r, s, "度数分布", 1L)
  br <- unlist(res$fig$edges); B <- length(br) - 1L; labs <- unlist(res$grids[[2]]$rows); cnt <- res$fig$counts
  S_(W, r + 1, s + 0:2, c("階級", "下端（以上）", "上端（未満）"), 6L); S_(W, r + 1, s + 2 + seq_len(G), if (is.null(sg)) "度数" else glev, 6L)
  for (b in seq_len(B)) {
    rr <- r + 1 + b; S_(W, rr, s, labs[b], 6L); N_(W, rr, s + 1, br[b]); N_(W, rr, s + 2, br[b + 1])
    up <- if (b == B) '"<="&' else '"<"&'
    F_(W, rr, s + 2 + seq_len(G), vapply(seq_len(G), function(i) sprintf('COUNTIFS(%s,">="&%s,%s,%s%s,%s,%d)', gy$yR, ad(rr, s + 1), gy$yR, up, ad(rr, s + 2), gy$gR, i), ""))
    for (i in seq_len(G)) xl_check(W, paste0("hist[", b, ",", i, "]"), rr, s + 2 + i, cnt[b, i])
  }
  xl_note(W, r + B + 2, s, "最後の階級だけ上端を含みます。")
  xl_width(W, s, 28); 4 + max(9, 3 + G)
}

# ---------------------------------------------------------------- t 検定（2 群）
xl_ttest <- function(W, X, req, res, c0) {
  sy <- req$slots$y; sg <- req$slots$g; glev <- unlist(res$grids[[1]]$rows); eqv <- grepl("スチューデント", res$test)
  h <- c0; s <- c0 + 4; xl_helphead(W, h, c("群の番号", "値", "偏差の 2 乗"))
  gy <- xl_gy(W, X, req, h, s, sg, sy, glev, 3); r <- gy$r
  gs <- xl_groupsum(W, X, gy, r, s, glev, h + 2, c("n", "mean", "sd", "med"), corner = sg$label)
  M <- res$grids[[1]]$cells; for (i in 1:2) for (k in 1:3) xl_check(W, paste0("ttest grp[", i, ",", k, "]"), r + i, s + k, M[i, k])
  n1 <- gs$cell(1, "n"); n2 <- gs$cell(2, "n"); m1 <- gs$cell(1, "mean"); m2 <- gs$cell(2, "mean"); s1 <- gs$cell(1, "sd"); s2 <- gs$cell(2, "sd")
  r <- gs$nxt
  dif <- xl_stat(W, r, s, paste0("平均の差（", glev[1], " − ", glev[2], "）"), paste0(m1, "-", m2), 5L)
  if (eqv) {
    sp <- xl_stat(W, r + 1, s, "合併した標準偏差", sprintf("SQRT(((%s-1)*%s^2+(%s-1)*%s^2)/(%s+%s-2))", n1, s1, n2, s2, n1, n2), 5L)
    se <- xl_stat(W, r + 2, s, "差の標準誤差", sprintf("%s*SQRT(1/%s+1/%s)", sp, n1, n2), 5L)
    df <- xl_stat(W, r + 3, s, "自由度", sprintf("%s+%s-2", n1, n2), 0L)
  } else {
    sp <- xl_stat(W, r + 1, s, "合併した標準偏差（効果量用）", sprintf("SQRT(((%s-1)*%s^2+(%s-1)*%s^2)/(%s+%s-2))", n1, s1, n2, s2, n1, n2), 5L)
    se <- xl_stat(W, r + 2, s, "差の標準誤差", sprintf("SQRT(%s^2/%s+%s^2/%s)", s1, n1, s2, n2), 5L)
    df <- xl_stat(W, r + 3, s, "自由度（ウェルチ）", sprintf("%s^4/((%s^2/%s)^2/(%s-1)+(%s^2/%s)^2/(%s-1))", se, s1, n1, n1, s2, n2, n2), 5L)
  }
  tv <- xl_stat(W, r + 4, s, "t", paste0(dif, "/", se), 5L)
  xl_stat(W, r + 5, s, "p", xl_p_t(tv, df)); xl_check(W, "ttest p", r + 5, s + 1, res$chk$p); xl_check(W, "ttest t", r + 4, s + 1, res$chk$t); xl_check(W, "ttest df", r + 3, s + 1, res$chk$df)
  tc <- xl_stat(W, r + 6, s, "t の臨界値（両側 5%）", xl_t_crit(df), 5L)
  xl_stat(W, r + 7, s, "95% 信頼区間の下限", paste0(dif, "-", tc, "*", se), 5L); xl_check(W, "ttest lo", r + 7, s + 1, res$chk$lo)
  xl_stat(W, r + 8, s, "95% 信頼区間の上限", paste0(dif, "+", tc, "*", se), 5L); xl_check(W, "ttest hi", r + 8, s + 1, res$chk$hi)
  xl_stat(W, r + 9, s, "効果量 Hedges の g", sprintf("%s/%s*(1-3/(4*(%s+%s)-9))", dif, sp, n1, n2), 5L); xl_check(W, "ttest g", r + 9, s + 1, res$chk$g)
  xl_width(W, s, 30); 4 + 5
}

# ---------------------------------------------------------------- 一元配置分散分析
xl_anova <- function(W, X, req, res, c0) {
  sy <- req$slots$y; sg <- req$slots$g; glev <- unlist(res$grids[[1]]$rows); G <- length(glev); eqv <- !grepl("ウェルチ", res$test)
  h <- c0; s <- c0 + 4; xl_helphead(W, h, c("群の番号", "値", "偏差の 2 乗"))
  gy <- xl_gy(W, X, req, h, s, sg, sy, glev, 3); r <- gy$r
  gs <- xl_groupsum(W, X, gy, r, s, glev, h + 2, c("n", "mean", "sd", "med"), corner = sg$label)
  M <- res$grids[[1]]$cells; for (i in seq_len(G)) for (k in 1:3) xl_check(W, paste0("anova grp[", i, ",", k, "]"), r + i, s + k, M[i, k])
  nR <- gs$range("n"); mR <- gs$range("mean"); sR <- gs$range("sd"); r <- gs$nxt
  N <- xl_stat(W, r, s, "N", paste0("SUM(", nR, ")"), 0L)
  gm <- xl_stat(W, r + 1, s, "全体の平均", sprintf("SUMPRODUCT(%s,%s)/%s", nR, mR, N), 5L)
  ssb <- xl_stat(W, r + 2, s, "群間の平方和", sprintf("SUMPRODUCT(%s,(%s-%s)^2)", nR, mR, gm), 5L)
  ssw <- xl_stat(W, r + 3, s, "群内の平方和", sprintf("SUMPRODUCT((%s-1),%s^2)", nR, sR), 5L)
  r <- r + 4
  if (eqv) {
    d1 <- xl_stat(W, r, s, "自由度 1", as.character(G - 1), 0L); d2 <- xl_stat(W, r + 1, s, "自由度 2", sprintf("%s-%d", N, G), 0L)
    Fv <- xl_stat(W, r + 2, s, "F", sprintf("(%s/%s)/(%s/%s)", ssb, d1, ssw, d2), 5L)
  } else {
    w <- sprintf("(%s/%s^2)", nR, sR); Wt <- xl_stat(W, r, s, "重みの合計 Σ(n/s²)", sprintf("SUMPRODUCT(%s)", w), 5L)
    mw <- xl_stat(W, r + 1, s, "重み付き平均", sprintf("SUMPRODUCT(%s,%s)/%s", w, mR, Wt), 5L)
    tmp <- xl_stat(W, r + 2, s, "補正の項", sprintf("SUMPRODUCT((1-%s/%s)^2/(%s-1))", w, Wt, nR), 5L)
    d1 <- xl_stat(W, r + 3, s, "自由度 1", as.character(G - 1), 0L)
    d2 <- xl_stat(W, r + 4, s, "自由度 2（ウェルチ）", sprintf("(%d^2-1)/(3*%s)", G, tmp), 5L)
    Fv <- xl_stat(W, r + 5, s, "F（ウェルチ）", sprintf("SUMPRODUCT(%s,(%s-%s)^2)/(%d-1)/(1+2*(%d-2)/(%d^2-1)*%s)", w, mR, mw, G, G, G, tmp), 5L)
    r <- r + 3
  }
  xl_check(W, "anova F", r + 2, s + 1, res$chk$F); xl_check(W, "anova df2", r + 1, s + 1, res$chk$d2)
  xl_stat(W, r + 3, s, "p", xl_p_f(Fv, d1, d2)); xl_check(W, "anova p", r + 3, s + 1, res$chk$p)
  xl_stat(W, r + 4, s, "効果量 η²", sprintf("%s/(%s+%s)", ssb, ssb, ssw), 5L); xl_check(W, "anova eta2", r + 4, s + 1, res$chk$eta2)
  r <- r + 6; post <- res$grids[[2]]; pn <- unlist(post$rows); PM <- post$cells
  S_(W, r, s, post$title, 1L); S_(W, r + 1, s + 0:3, c("組み合わせ", "平均の差", "t", "自由度"), 6L)
  pr <- utils::combn(G, 2)
  for (c in seq_len(ncol(pr))) {
    i <- pr[1, c]; j <- pr[2, c]; rr <- r + 1 + c; S_(W, rr, s, pn[c], 6L)
    F_(W, rr, s + 1, paste0(gs$cell(i, "mean"), "-", gs$cell(j, "mean")), 5L); xl_check(W, paste0("anova diff[", c, "]"), rr, s + 1, PM[c, 1])
    if (eqv) { msw <- sprintf("(%s/%s)", ssw, d2); F_(W, rr, s + 2, sprintf("ABS(%s)/SQRT(%s*(1/%s+1/%s))", ad(rr, s + 1), msw, gs$cell(i, "n"), gs$cell(j, "n")), 5L); F_(W, rr, s + 3, d2, 0L) }
    else {
      se2 <- sprintf("(%s^2/%s+%s^2/%s)", gs$cell(i, "sd"), gs$cell(i, "n"), gs$cell(j, "sd"), gs$cell(j, "n"))
      F_(W, rr, s + 2, sprintf("ABS(%s)/SQRT(%s)", ad(rr, s + 1), se2), 5L)
      F_(W, rr, s + 3, sprintf("%s^2/((%s^2/%s)^2/(%s-1)+(%s^2/%s)^2/(%s-1))", se2, gs$cell(i, "sd"), gs$cell(i, "n"), gs$cell(i, "n"), gs$cell(j, "sd"), gs$cell(j, "n"), gs$cell(j, "n")), 5L)
    }
  }
  xl_note(W, r + 2 + ncol(pr), s, "多重比較の p は、スチューデント化された範囲の分布を使うため Excel の関数では求められません（アプリの R で計算した値を使ってください）。")
  W$notes <- c(W$notes, "分散分析のあとの多重比較の p")
  xl_width(W, s, 30); 4 + 5
}

# ---------------------------------------------------------------- 対応のある t 検定
xl_pairedt <- function(W, X, req, res, c0) {
  sx <- req$slots$x; sy <- req$slots$y; h <- c0; s <- c0 + 4; r <- 3; xk <- NULL; yk <- NULL
  xl_helphead(W, h, c("回答 1 の値", "回答 2 の値", "差（1 − 2）"))
  if (!identical(sx$type, "numeric")) { xk <- xl_keys(W, r, s, sx, paste0("得点：", sx$label)); r <- xk$nxt }
  if (!identical(sy$type, "numeric")) { yk <- xl_keys(W, r, s, sy, paste0("得点：", sy$label)); r <- yk$nxt }
  ok <- xl_and(xl_filter(req$filter, rowref(req$filter$col)), xl_valid(sx, rowref(sx$col), xk), xl_valid(sy, rowref(sy$col), yk))
  xl_rows(W, X, h, paste0("IF(", ok, ",", xl_value(sx, rowref(sx$col), xk), ',"")'))
  xl_rows(W, X, h + 1, paste0("IF(", ok, ",", xl_value(sy, rowref(sy$col), yk), ',"")'))
  xl_rows(W, X, h + 2, paste0("IF(ISNUMBER($", xl_L(h), XL_MARK, "),$", xl_L(h), XL_MARK, "-$", xl_L(h + 1), XL_MARK, ',"")'))
  xR <- rg(2, h, X$last, h); yR <- rg(2, h + 1, X$last, h + 1); dR <- rg(2, h + 2, X$last, h + 2)
  S_(W, r, s + 0:3, c("", "n", "平均", "標準偏差"), 6L); lab <- c(sx$label, sy$label, "差（1 − 2）"); R3 <- list(xR, yR, dR)
  for (i in 1:3) { S_(W, r + i, s, lab[i], 6L); F_(W, r + i, s + 1:3, c(paste0("COUNT(", R3[[i]], ")"), paste0("AVERAGE(", R3[[i]], ")"), paste0(FN("STDEV.S"), "(", R3[[i]], ")")), c(0L, 5L, 5L)) }
  M <- res$grids[[1]]$cells; for (i in 1:3) for (k in 1:3) xl_check(W, paste0("pairedt [", i, ",", k, "]"), r + i, s + k, M[i, k])
  n <- ad(r + 3, s + 1); md <- ad(r + 3, s + 2); sd <- ad(r + 3, s + 3); r <- r + 5
  se <- xl_stat(W, r, s, "差の標準誤差", sprintf("%s/SQRT(%s)", sd, n), 5L)
  tv <- xl_stat(W, r + 1, s, "t", paste0(md, "/", se), 5L); df <- xl_stat(W, r + 2, s, "自由度", paste0(n, "-1"), 0L)
  xl_stat(W, r + 3, s, "p", sprintf("%s(ABS(%s),%s)", FN("T.DIST.2T"), tv, df)); xl_check(W, "pairedt t", r + 1, s + 1, res$chk$t); xl_check(W, "pairedt p", r + 3, s + 1, res$chk$p)
  tc <- xl_stat(W, r + 4, s, "t の臨界値（両側 5%）", sprintf("%s(0.05,%s)", FN("T.INV.2T"), df), 5L)
  xl_stat(W, r + 5, s, "差の 95% 信頼区間の下限", paste0(md, "-", tc, "*", se), 5L); xl_check(W, "pairedt lo", r + 5, s + 1, res$chk$lo)
  xl_stat(W, r + 6, s, "差の 95% 信頼区間の上限", paste0(md, "+", tc, "*", se), 5L); xl_check(W, "pairedt hi", r + 6, s + 1, res$chk$hi)
  xl_stat(W, r + 7, s, "効果量 dz", paste0(md, "/", sd), 5L); xl_check(W, "pairedt dz", r + 7, s + 1, res$chk$dz)
  xl_width(W, s, 30); 4 + 5
}

# ---------------------------------------------------------------- ピアソンの相関係数
xl_pearson <- function(W, X, req, res, c0) {
  sx <- req$slots$x; sy <- req$slots$y; h <- c0; s <- c0 + 3; r <- 3; xk <- NULL; yk <- NULL
  xl_helphead(W, h, c("回答 1 の値", "回答 2 の値"))
  if (!identical(sx$type, "numeric")) { xk <- xl_keys(W, r, s, sx, paste0("得点：", sx$label)); r <- xk$nxt }
  if (!identical(sy$type, "numeric")) { yk <- xl_keys(W, r, s, sy, paste0("得点：", sy$label)); r <- yk$nxt }
  ok <- xl_and(xl_filter(req$filter, rowref(req$filter$col)), xl_valid(sx, rowref(sx$col), xk), xl_valid(sy, rowref(sy$col), yk))
  xl_rows(W, X, h, paste0("IF(", ok, ",", xl_value(sx, rowref(sx$col), xk), ',"")'))
  xl_rows(W, X, h + 1, paste0("IF(", ok, ",", xl_value(sy, rowref(sy$col), yk), ',"")'))
  xR <- rg(2, h, X$last, h); yR <- rg(2, h + 1, X$last, h + 1)
  n <- xl_stat(W, r, s, "n", paste0("COUNT(", xR, ")"), 0L)
  rr <- xl_stat(W, r + 1, s, "r", sprintf("CORREL(%s,%s)", xR, yR), 5L); xl_check(W, "pearson r", r + 1, s + 1, res$chk$r)
  tv <- xl_stat(W, r + 2, s, "t", sprintf("%s*SQRT((%s-2)/(1-%s^2))", rr, n, rr), 5L)
  xl_stat(W, r + 3, s, "p", sprintf("%s(ABS(%s),%s-2)", FN("T.DIST.2T"), tv, n)); xl_check(W, "pearson p", r + 3, s + 1, res$chk$p)
  z <- sprintf("%s(%s)", "FISHER", rr); zc <- sprintf("%s(0.975)/SQRT(%s-3)", FN("NORM.S.INV"), n)
  xl_stat(W, r + 4, s, "95% 信頼区間の下限", sprintf("FISHERINV(%s-%s)", z, zc), 5L); xl_check(W, "pearson lo", r + 4, s + 1, res$chk$lo)
  xl_stat(W, r + 5, s, "95% 信頼区間の上限", sprintf("FISHERINV(%s+%s)", z, zc), 5L); xl_check(W, "pearson hi", r + 5, s + 1, res$chk$hi)
  xl_stat(W, r + 6, s, "回帰直線の切片", sprintf("INTERCEPT(%s,%s)", yR, xR), 5L); xl_check(W, "pearson b0", r + 6, s + 1, res$chk$b0)
  xl_stat(W, r + 7, s, "回帰直線の傾き", sprintf("SLOPE(%s,%s)", yR, xR), 5L); xl_check(W, "pearson b1", r + 7, s + 1, res$chk$b1)
  xl_width(W, s, 30); 3 + 3
}

# ---------------------------------------------------------------- 尺度の信頼性（クロンバックの α）
xl_alpha <- function(W, X, req, res, c0) {
  sl <- req$slots$items; cols <- unlist(sl$cols); k <- length(cols); rev <- unlist(sl$reverse); names <- unlist(res$grids[[1]]$rows)
  h <- c0; s <- c0 + k + 2
  xl_helphead(W, h, c(paste0("得点：", names), "合計"))
  kk <- xl_keys(W, 3, s, sl, "得点（すべての項目に共通）"); r <- kk$nxt; L <- length(kk$lev)
  okall <- xl_and(xl_filter(req$filter, rowref(req$filter$col)), vapply(cols, function(j) xl_valid(sl, rowref(j), kk), ""))
  for (j in seq_len(k)) {
    v <- xl_value(sl, rowref(cols[j]), kk); if (cols[j] %in% rev) v <- paste0("(", L + 1, "-", v, ")")
    xl_rows(W, X, h + j - 1, paste0("IF(", okall, ",", v, ',"")'))
  }
  xl_rows(W, X, h + k, paste0("IF(ISNUMBER($", xl_L(h), XL_MARK, "),SUM($", xl_L(h), XL_MARK, ":$", xl_L(h + k - 1), XL_MARK, '),"")'))
  IR <- function(j) rg(2, h + j - 1, X$last, h + j - 1); TR <- rg(2, h + k, X$last, h + k)
  if (length(rev)) { xl_note(W, r, s, paste0("逆転項目（", L + 1, " − 得点）：", paste(names[cols %in% rev], collapse = "、"))); r <- r + 1 }
  S_(W, r, s + 0:4, c("項目", "平均", "標準偏差", "残りの合計との相関", "除いたときの α"), 6L)
  n <- paste0("COUNT(", TR, ")"); vT <- paste0(FN("VAR.S"), "(", TR, ")"); sumv <- paste0("(", paste0(FN("VAR.S"), "(", vapply(seq_len(k), IR, ""), ")", collapse = "+"), ")")
  for (j in seq_len(k)) {
    rr <- r + j; vi <- paste0(FN("VAR.S"), "(", IR(j), ")"); cv <- paste0(FN("COVARIANCE.S"), "(", IR(j), ",", TR, ")")
    S_(W, rr, s, names[j], 6L)
    F_(W, rr, s + 1:4, c(paste0("AVERAGE(", IR(j), ")"), paste0(FN("STDEV.S"), "(", IR(j), ")"), sprintf("(%s-%s)/SQRT(%s*(%s+%s-2*%s))", cv, vi, vi, vT, vi, cv),
                         if (k > 2) sprintf("(%d-1)/(%d-2)*(1-(%s-%s)/(%s+%s-2*%s))", k, k, sumv, vi, vT, vi, cv) else '"–"'), 5L)
    xl_check(W, paste0("alpha itc[", j, "]"), rr, s + 3, res$chk$itc[j]); if (k > 2) xl_check(W, paste0("alpha del[", j, "]"), rr, s + 4, res$chk$adel[j])
  }
  r <- r + k + 2
  xl_stat(W, r, s, "n（すべての項目に回答）", n, 0L)
  xl_stat(W, r + 1, s, "クロンバックの α", sprintf("%d/(%d-1)*(1-%s/%s)", k, k, sumv, vT), 5L); xl_check(W, "alpha", r + 1, s + 1, res$chk$alpha)
  prs <- utils::combn(k, 2); rb <- paste0("(", paste(sprintf("CORREL(%s,%s)", vapply(prs[1, ], IR, ""), vapply(prs[2, ], IR, "")), collapse = "+"), ")/", ncol(prs))
  rbar <- xl_stat(W, r + 2, s, "項目間の相関の平均", rb, 5L)
  xl_stat(W, r + 3, s, "標準化した α", sprintf("%d*%s/(1+(%d-1)*%s)", k, rbar, k, rbar), 5L); xl_check(W, "alpha std", r + 3, s + 1, res$chk$astd)
  xl_width(W, s, 30); k + 2 + 6
}

# ---------------------------------------------------------------- 回答日時の推移
# 日付は、日時の文字の先頭（年/月/日）から DATE 関数で作る
xl_timeline <- function(W, X, req, res, c0) {
  st <- req$slots$t; per <- if (is.null(req$opts$per)) "day" else req$opts$per; h <- c0; s <- c0 + 2
  xl_helphead(W, h, "回答日")
  x <- rowref(st$col); a <- sprintf('SUBSTITUTE(LEFT(%s,FIND(" ",%s&" ")-1),"-","/")', x, x)
  d <- sprintf('DATE(VALUE(LEFT(%s,4)),VALUE(MID(%s,6,FIND("/",%s,6)-6)),VALUE(MID(%s,FIND("/",%s,6)+1,9)))', a, a, a, a, a)
  fl <- xl_filter(req$filter, rowref(req$filter$col))
  xl_rows(W, X, h, paste0("IFERROR(", if (length(fl)) paste0("IF(", fl, ",", d, ',"")') else d, ',"")'), 0L)
  dR <- rg(2, h, X$last, h); g <- res$grids[[1]]; lab <- unlist(g$rows); K <- length(lab); cells <- g$cells
  S_(W, 3, s + 0:5, c(g$corner, "開始日", "次の開始日", "回答数", "累計", "累計の割合 (%)"), 6L)
  for (i in seq_len(K)) {
    rr <- 3 + i; S_(W, rr, s, lab[i], 6L)
    st0 <- if (i > 1) ad(rr - 1, s + 2) else switch(per, week = sprintf("MIN(%s)-WEEKDAY(MIN(%s),2)+1", dR, dR), month = sprintf("DATE(YEAR(MIN(%s)),MONTH(MIN(%s)),1)", dR, dR), sprintf("MIN(%s)", dR))
    F_(W, rr, s + 1, st0, 0L); F_(W, rr, s + 2, switch(per, week = paste0(ad(rr, s + 1), "+7"), month = sprintf("%s(%s,1)", "EDATE", ad(rr, s + 1)), paste0(ad(rr, s + 1), "+1")), 0L)
    F_(W, rr, s + 3, sprintf('COUNTIFS(%s,">="&%s,%s,"<"&%s)', dR, ad(rr, s + 1), dR, ad(rr, s + 2)), 0L)
    F_(W, rr, s + 4, sprintf("SUM(%s:%s)", ad(4, s + 3), ad(rr, s + 3)), 0L)
    F_(W, rr, s + 5, sprintf("%s/COUNT(%s)*100", ad(rr, s + 4), dR), 4L)
    xl_check(W, paste0("timeline n[", i, "]"), rr, s + 3, cells[i, 1])
  }
  xl_note(W, 5 + K, s, "開始日・次の開始日は Excel の日付の値です（表示形式を日付にすると読めます）。")
  xl_width(W, h, 12); xl_width(W, s, 20); 2 + 6
}

# ---------------------------------------------------------------- コレスポンデンス分析（関連の検定まで）
xl_ca <- function(W, X, req, res, c0) {
  used <- xl_crosstab(W, X, req, res, c0, "chisq")
  W$notes <- c(W$notes, "コレスポンデンス分析の次元と座標（特異値分解）")
  used
}
# ---------------------------------------------------------------- 重回帰分析：正規方程式（X'X の逆行列）で係数を求める
xl_reg <- function(W, X, req, res, c0, df) {
  sy <- req$slots$y; d <- reg_design(df, req$slots$x$vars); info <- d$info; k <- length(info); p <- k + 1L
  h <- c0; s <- c0 + k + 3L; r <- 3L; keys <- list()
  xl_helphead(W, h, c("使う（1）", paste0("目的変数：", sy$label), paste0("説明変数：", names(info))))
  sl <- c(list(sy), lapply(info, function(z) z$slot))
  for (z in sl) if (identical(z$type, "ordinal") && is.null(keys[[as.character(z$col)]])) { kk <- xl_keys(W, r, s, z, paste0("得点：", z$label)); keys[[as.character(z$col)]] <- kk; r <- kk$nxt }
  valid <- function(z) if (identical(z$type, "nominal")) xl_member(rowref(z$col), unlist(lapply(z$cats, function(c) unlist(c$values)))) else xl_valid(z, rowref(z$col), keys[[as.character(z$col)]])
  vs <- unique(lapply(sl, function(z) z$col)); vz <- lapply(vs, function(cc) sl[[which(vapply(sl, function(z) identical(z$col, cc), TRUE))[1]]])
  ok <- xl_and(xl_filter(req$filter, rowref(req$filter$col)), vapply(vz, function(z) { v <- valid(z); if (identical(z$type, "nominal")) paste0(v, ">0") else v }, ""))
  xl_rows(W, X, h, paste0("IF(", ok, ",1,0)"), 0L)
  w <- paste0("$", xl_L(h), XL_MARK)
  xl_rows(W, X, h + 1, paste0("IF(", w, "=1,", xl_value(sy, rowref(sy$col), keys[[as.character(sy$col)]]), ",0)"))
  for (j in seq_len(k)) {
    z <- info[[j]]; v <- if (isTRUE(z$dummy)) paste0("IF(", xl_member(rowref(z$slot$col), xl_vals(z$slot, z$level)), ">0,1,0)") else xl_value(z$slot, rowref(z$slot$col), keys[[as.character(z$slot$col)]])
    xl_rows(W, X, h + 1 + j, paste0("IF(", w, "=1,", v, ",0)"))
  }
  colR <- function(j) rg(2, h + if (j == 0) 0 else 1 + j, X$last, h + if (j == 0) 0 else 1 + j)   # 0：切片（使う＝1 の列）、1〜k：説明変数
  yR <- rg(2, h + 1, X$last, h + 1)
  S_(W, r, s, "X'X（説明変数どうしの積の和。先頭は切片）", 1L); m0 <- r + 1L
  for (i in 0:k) for (j in 0:k) F_(W, m0 + i, s + j, sprintf("SUMPRODUCT(%s,%s)", colR(i), colR(j)), 4L)
  XtX <- rg(m0, s, m0 + k, s + k); r <- m0 + p + 1L
  S_(W, r, s, "(X'X) の逆行列", 1L); i0 <- r + 1L
  for (i in 0:k) for (j in 0:k) F_(W, i0 + i, s + j, sprintf("INDEX(MINVERSE(%s),%d,%d)", XtX, i + 1, j + 1), 5L)
  r <- i0 + p + 1L; S_(W, r, s, "X'y", 1L); y0 <- r + 1L
  for (j in 0:k) F_(W, y0, s + j, sprintf("SUMPRODUCT(%s,%s)", colR(j), yR), 4L)
  Xty <- rg(y0, s, y0, s + k); r <- y0 + 2L
  n <- xl_stat(W, r, s, "n", sprintf("SUM(%s)", colR(0)), 0L)
  yy <- xl_stat(W, r + 1, s, "Σy²", sprintf("SUMPRODUCT(%s,%s)", yR, yR), 4L)
  S_(W, r + 3, s, "回帰係数", 1L); hd <- c("", "B", "標準誤差", "β", "95% 信頼区間の下限", "上限", "t", "p", "VIF"); S_(W, r + 4, s + 0:8, hd, 6L)
  b0 <- r + 5L; Bc <- function(j) ad(b0 + j, s + 1)
  ct <- sprintf("%s(0.05,%s-%d)", FN("T.INV.2T"), n, p)
  rs <- b0 + p + 1L
  sseC <- xl_stat(W, rs, s, "残差平方和", paste0(yy, "-(", paste(vapply(0:k, function(j) paste0(Bc(j), "*", ad(y0, s + j)), ""), collapse = "+"), ")"), 4L)
  sstC <- xl_stat(W, rs + 1, s, "全体の平方和", sprintf("%s-SUM(%s)^2/%s", yy, yR, n), 4L)
  mse <- sprintf("(%s/(%s-%d))", sseC, n, p)
  sdy <- sprintf("SQRT(%s/(%s-1))", sstC, n)
  rn <- c("切片", names(info)); cells <- res$grids[[1]]$cells
  for (j in 0:k) {
    rr <- b0 + j; S_(W, rr, s, rn[j + 1], 6L)
    F_(W, rr, s + 1, sprintf("SUMPRODUCT(INDEX(%s,%d,0),%s)", rg(i0, s, i0 + k, s + k), j + 1, Xty), 5L); xl_check(W, paste0("reg B", j), rr, s + 1, cells[[j + 1]][[1]])
    se <- sprintf("SQRT(%s*%s)", ad(i0 + j, s + j), mse); F_(W, rr, s + 2, se, 5L)
    if (j > 0) {
      ssx <- sprintf("(SUMPRODUCT(%s,%s)-SUM(%s)^2/%s)", colR(j), colR(j), colR(j), n)
      F_(W, rr, s + 3, sprintf("%s*SQRT(%s/(%s-1))/%s", Bc(j), ssx, n, sdy), 5L); xl_check(W, paste0("reg beta", j), rr, s + 3, cells[[j + 1]][[3]])
      if (k > 1) { F_(W, rr, s + 8, sprintf("%s*%s", ad(i0 + j, s + j), ssx), 5L); xl_check(W, paste0("reg vif", j), rr, s + 8, cells[[j + 1]][[8]]) }
    }
    F_(W, rr, s + 4, sprintf("%s-%s*%s", Bc(j), ct, ad(rr, s + 2)), 5L); F_(W, rr, s + 5, sprintf("%s+%s*%s", Bc(j), ct, ad(rr, s + 2)), 5L)
    F_(W, rr, s + 6, sprintf("%s/%s", Bc(j), ad(rr, s + 2)), 5L)
    F_(W, rr, s + 7, sprintf("%s(ABS(%s),%s-%d)", FN("T.DIST.2T"), ad(rr, s + 6), n, p), 3L); xl_check(W, paste0("reg p", j), rr, s + 7, cells[[j + 1]][[7]])
  }
  r2 <- xl_stat(W, rs + 2, s, "R²", sprintf("1-%s/%s", sseC, sstC), 5L); xl_check(W, "reg R2", rs + 2, s + 1, res$grids[[2]]$cells[[1]][[1]])
  xl_stat(W, rs + 3, s, "調整済み R²", sprintf("1-(1-%s)*(%s-1)/(%s-%d)", r2, n, n, p), 5L)
  fv <- xl_stat(W, rs + 4, s, sprintf("F（自由度 %d と n−%d）", k, p), sprintf("((%s-%s)/%d)/%s", sstC, sseC, k, mse), 5L)
  xl_stat(W, rs + 5, s, "p", sprintf("%s(%s,%d,%s-%d)", FN("F.DIST.RT"), fv, k, n, p)); xl_check(W, "reg F p", rs + 5, s + 1, res$p)
  xl_note(W, rs + 6, s, "係数は正規方程式 B = (X'X)⁻¹X'y で求めています。単一回答はダミー変数（基準の選択肢は 0）、順序のある回答は得点の表で点数にしています。")
  xl_width(W, s, 34); k + 3L + 10L
}
xl_logit <- function(W, X, req, res, c0) { W$notes <- c(W$notes, "ロジスティック回帰分析（係数を反復計算で求めるため）"); 0L }

# ---------------------------------------------------------------- 層別のクロス集計：層ごとのカイ二乗検定と、2×2 のときの CMH 検定
xl_strat <- function(W, X, req, res, c0, df) {
  sa <- req$slots$a; sb <- req$slots$b; sz <- req$slots$z; g <- res$grids[[1]]
  lay <- unlist(g$rows); lay <- lay[-length(lay)]; c <- c0
  keep <- if (is.null(req$filter)) rep(TRUE, nrow(df)) else apply_filter(df, req$filter)
  for (k in seq_along(lay)) {
    if (is.null(g$cells[[k]][[4]])) next
    zt <- xl_member(X$rng(sz$col), xl_vals(sz, lay[k]))
    req2 <- list(type = "chisq", slots = list(a = sa, b = sb), filter = list(col = sz$col, type = sz$type, values = as.list(xl_vals(sz, lay[k]))), opts = req$opts)
    r2 <- tryCatch(run_analysis(df[keep, , drop = FALSE], req2), error = function(e) NULL); if (is.null(r2)) next
    S_(W, 2, c, paste0("層：", sz$label, " = ", lay[k]), 2L)
    c <- c + xl_crosstab(W, X, list(slots = list(a = sa, b = sb), filter = req$filter), r2, c, "chisq", zt) + 1L
  }
  rows <- unlist(res$xtab$rows); cols <- unlist(res$xtab$cols)
  if (length(unique(sub("^.*・", "", rows))) == 2 && length(cols) == 2) {
    alev <- xl_levels(sa); alev <- alev[alev %in% sub("^[^・]*・", "", rows)]; flt <- xl_filter(req$filter, X$rng(req$filter$col))
    A1 <- xl_member(X$rng(sa$col), xl_vals(sa, alev[1])); A2 <- xl_member(X$rng(sa$col), xl_vals(sa, alev[2]))
    B1 <- xl_member(X$rng(sb$col), xl_vals(sb, cols[1])); B2 <- xl_member(X$rng(sb$col), xl_vals(sb, cols[2]))
    S_(W, 3, c, "CMH 検定（2×2 の表）", 1L)
    hd <- c("層", "a", "b", "c", "d", "n", "E(a)", "V(a)", "ad/n", "bc/n"); S_(W, 4, c + 0:9, hd, 6L)
    r <- 5L
    for (k in seq_along(lay)) {
      zt <- xl_member(X$rng(sz$col), xl_vals(sz, lay[k])); S_(W, r, c, lay[k], 6L)
      F_(W, r, c + 1, xl_count(c(A1, B1, zt, flt)), 0L); F_(W, r, c + 2, xl_count(c(A1, B2, zt, flt)), 0L)
      F_(W, r, c + 3, xl_count(c(A2, B1, zt, flt)), 0L); F_(W, r, c + 4, xl_count(c(A2, B2, zt, flt)), 0L)
      a <- ad(r, c + 1); b <- ad(r, c + 2); cc <- ad(r, c + 3); d <- ad(r, c + 4); n <- ad(r, c + 5)
      F_(W, r, c + 5, sprintf("SUM(%s:%s)", a, d), 0L)
      F_(W, r, c + 6, sprintf("IF(%s>1,(%s+%s)*(%s+%s)/%s,0)", n, a, b, a, cc, n), 4L)
      F_(W, r, c + 7, sprintf("IF(%s>1,(%s+%s)*(%s+%s)*(%s+%s)*(%s+%s)/(%s^2*(%s-1)),0)", n, a, b, cc, d, a, cc, b, d, n, n), 4L)
      F_(W, r, c + 8, sprintf("IF(%s>1,%s*%s/%s,0)", n, a, d, n), 4L); F_(W, r, c + 9, sprintf("IF(%s>1,%s*%s/%s,0)", n, b, cc, n), 4L)
      r <- r + 1L
    }
    col <- function(j) rg(5, c + j, r - 1L, c + j)
    st <- xl_stat(W, r + 1L, c, "CMH χ²（補正なし）", sprintf("(SUM(%s)-SUM(%s))^2/SUM(%s)", col(1), col(6), col(7)), 5L)
    xl_stat(W, r + 2L, c, "自由度", "1", 0L)
    xl_stat(W, r + 3L, c, "p", sprintf("%s(%s,1)", FN("CHISQ.DIST.RT"), st)); xl_check(W, "cmh p", r + 3L, c + 1, res$p)
    xl_stat(W, r + 4L, c, "共通オッズ比（マンテル・ヘンツェル）", sprintf("SUM(%s)/SUM(%s)", col(8), col(9)), 5L)
    or <- res$grids[[2]]$cells; xl_check(W, "cmh or", r + 4L, c + 1, or[[length(or)]][[1]])
    xl_note(W, r + 5L, c, "a〜d は 2×2 の表の左上・右上・左下・右下の人数です。共通オッズ比の信頼区間とブレスロー・デイ検定は Excel では再現していません。")
    W$notes <- c(W$notes, "共通オッズ比の信頼区間とブレスロー・デイ検定")
    xl_width(W, c, 30); c <- c + 11L
  } else W$notes <- c(W$notes, "一般化したコクラン・マンテル・ヘンツェル検定（3×2 以上の表）")
  c - c0
}
XL_WRITERS <- list(freq = xl_freq, freqmulti = xl_freqmulti, chisq = function(W, X, req, res, c0) xl_crosstab(W, X, req, res, c0, "chisq"), fisher = function(W, X, req, res, c0) xl_crosstab(W, X, req, res, c0, "fisher"),
                   mwu = function(W, X, req, res, c0) xl_rank(W, X, req, res, c0, FALSE), kw = function(W, X, req, res, c0) xl_rank(W, X, req, res, c0, TRUE),
                   spearman = xl_spearman, wsr = xl_wsr, mcnemar = xl_mcnemar, trend = xl_trend, multi = xl_multi,
                   numsum = xl_numsum, ttest = xl_ttest, anova = xl_anova, pairedt = xl_pairedt, pearson = xl_pearson, alpha = xl_alpha, timeline = xl_timeline, ca = xl_ca, strat = xl_strat, reg = xl_reg, logit = xl_logit)

# items：list(list(name = 解析の名前, req = 解析の設定), …)
build_excel <- function(df, items) {
  n <- nrow(df); nc <- ncol(df); W <- xl_new()
  X <- list(n = n, last = n + 1L, nc = nc, rng = function(col) if (is.null(col)) "" else rg(2, col, n + 1L, col))
  numeric_cols <- integer(0); fixes <- list()
  for (it in items) for (s in it$req$slots) for (s2 in c(list(s), s$vars)) if (identical(s2$type, "numeric") && !is.null(s2$col)) { numeric_cols <- c(numeric_cols, s2$col); fixes[[as.character(s2$col)]] <- list(fix = s2$fix, unit = s2$unit) }
  S_(W, 1, seq_len(nc), gsub("[\r\n]+", " ", names(df)), 6L)
  for (j in seq_len(nc)) {
    v <- df[[j]]; ok <- which(!is.na(v)); if (!length(ok)) next
    if (is.numeric(v)) N_(W, ok + 1L, j, v[ok])                       # 尺度得点など、アプリで作った数値の列
    else if (j %in% numeric_cols) {                                 # 数値の列は、直した値を書く（数値にできない回答は空欄）
      fx <- fixes[[as.character(j)]]; num <- numeric_values(v[ok], fx$fix, fx$unit); g <- !is.na(num)
      if (any(g)) N_(W, ok[g] + 1L, j, num[g])
    } else S_(W, ok + 1L, j, v[ok])
    xl_width(W, j, 14)
  }
  c0 <- nc + 2L; skipped <- character(0)
  for (k in seq_along(items)) {
    it <- items[[k]]; fn <- XL_WRITERS[[it$req$type]]
    res <- tryCatch(run_analysis(df, it$req), error = function(e) NULL)
    if (is.null(fn) || is.null(res)) { skipped <- c(skipped, it$name); next }
    hcols <- switch(it$req$type, mwu = 3L, kw = 3L, spearman = 4L, wsr = 3L, 0L)
    used <- if ("df" %in% names(formals(fn))) fn(W, X, it$req, res, c0, df) else fn(W, X, it$req, res, c0)
    S_(W, 1, c0 + hcols, paste0("【", k, "】", it$name, "（", res$test, "）"), 2L)
    if (!is.null(res$filterText)) xl_note(W, 2, c0 + hcols, paste0("絞り込み：", res$filterText))
    if (hcols > 0) for (j in seq_len(hcols)) xl_width(W, c0 + j - 1L, 12)
    c0 <- c0 + used + 1L
  }
  list(files = xl_files(W), checks = W$checks, skipped = A(skipped), notReproduced = A(unique(W$notes)))
}

xl_sheet_xml <- function(W) {
  d <- do.call(rbind, W$chunks); d <- d[order(d$row, d$col), ]
  ref <- paste0(xl_L(d$col), d$row); st <- ifelse(d$style > 0, paste0(' s="', d$style, '"'), "")
  val <- xl_esc(d$val)
  cell <- ifelse(d$kind == "s", paste0('<c r="', ref, '"', st, ' t="inlineStr"><is><t xml:space="preserve">', val, "</t></is></c>"),
          ifelse(d$kind == "n", paste0('<c r="', ref, '"', st, "><v>", val, "</v></c>"), paste0('<c r="', ref, '"', st, "><f>", val, "</f></c>")))
  rows <- vapply(split(cell, d$row), paste, "", collapse = "")
  rn <- as.integer(names(rows)); o <- order(rn)
  cols <- ""
  if (length(W$widths)) { ci <- sort(as.integer(names(W$widths))); cols <- paste0("<cols>", paste0('<col min="', ci, '" max="', ci, '" width="', unlist(W$widths[as.character(ci)]), '" customWidth="1"/>', collapse = ""), "</cols>") }
  paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
         '<sheetViews><sheetView workbookViewId="0"><pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/></sheetView></sheetViews>', cols,
         "<sheetData>", paste0('<row r="', rn[o], '">', rows[o], "</row>", collapse = ""), "</sheetData></worksheet>")
}
xl_files <- function(W) {
  head <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
  ns <- 'xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"'
  list(
    "[Content_Types].xml" = paste0(head, '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/><Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/></Types>'),
    "_rels/.rels" = paste0(head, '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>'),
    "xl/workbook.xml" = paste0(head, "<workbook ", ns, ' xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="データと解析" sheetId="1" r:id="rId1"/></sheets><calcPr calcId="191029" fullCalcOnLoad="1"/></workbook>'),
    "xl/_rels/workbook.xml.rels" = paste0(head, '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/><Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/></Relationships>'),
    # 書式：0 標準、1 太字、2 解析の題（太字・緑の地）、3 小数 3 桁、4 小数 1 桁、5 小数 2 桁、6 見出し（太字・灰色の地）、7 注（灰色の文字）
    "xl/styles.xml" = paste0(head, "<styleSheet ", ns, '><numFmts count="3"><numFmt numFmtId="164" formatCode="0.000"/><numFmt numFmtId="165" formatCode="0.0"/><numFmt numFmtId="166" formatCode="0.00"/></numFmts>',
      '<fonts count="3"><font><sz val="11"/><name val="Yu Gothic"/></font><font><b/><sz val="11"/><name val="Yu Gothic"/></font><font><sz val="11"/><color rgb="FF595959"/><name val="Yu Gothic"/></font></fonts>',
      '<fills count="4"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill><fill><patternFill patternType="solid"><fgColor rgb="FFDDEFE3"/></patternFill></fill><fill><patternFill patternType="solid"><fgColor rgb="FFF2F2F2"/></patternFill></fill></fills>',
      '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders><cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
      '<cellXfs count="8"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/><xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/><xf numFmtId="0" fontId="1" fillId="2" borderId="0" xfId="0" applyFont="1" applyFill="1"/>',
      '<xf numFmtId="164" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/><xf numFmtId="165" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/><xf numFmtId="166" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',
      '<xf numFmtId="0" fontId="1" fillId="3" borderId="0" xfId="0" applyFont="1" applyFill="1"/><xf numFmtId="0" fontId="2" fillId="0" borderId="0" xfId="0" applyFont="1"/></cellXfs></styleSheet>'),
    "xl/worksheets/sheet1.xml" = xl_sheet_xml(W))
}
