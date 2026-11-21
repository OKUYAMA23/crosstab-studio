# server.R ― 画面（ブラウザ）と R をつなぐ小さなローカルサーバー。127.0.0.1 だけで待ち受ける。

APP_ID <- "CrossTabStudio"
APP_VERSION <- "0.12.0"
STORE <- new.env()          # 読み込んだデータ（id → data.frame）
STATE <- new.env()
STATE$last_ping <- Sys.time(); STATE$had_client <- FALSE; STATE$bye_at <- NULL

PROJECT_DIR <- file.path(tempdir(), "projects")      # run.R が、アプリのフォルダーの projects に置き換える
proj_name <- function(x) { if (is.null(x)) return(""); x <- gsub('[\\\\/:*?"<>|[:cntrl:]]', "_", x); x <- trimws(gsub("^\\.+", "", x)); substr(x, 1, 80) }
to_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA, force = TRUE)
json_resp <- function(x, status = 200L) {
  list(status = status,
       headers = list("Content-Type" = "application/json; charset=utf-8", "Cache-Control" = "no-store",
                      "Access-Control-Allow-Origin" = "*", "Access-Control-Allow-Private-Network" = "true"),
       body = charToRaw(enc2utf8(as.character(to_json(x)))))
}
read_body <- function(req) {
  inp <- req$rook.input
  if (is.null(inp)) return(raw(0))
  b <- inp$read(); if (is.null(b)) raw(0) else b
}
read_json <- function(req) {
  txt <- rawToChar(read_body(req)); Encoding(txt) <- "UTF-8"
  if (!nzchar(txt)) return(list())
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}
query <- function(req) {
  qs <- sub("^\\?", "", if (is.null(req$QUERY_STRING)) "" else req$QUERY_STRING)
  out <- list()
  for (kv in strsplit(qs, "&", fixed = TRUE)[[1]]) {
    p <- strsplit(kv, "=", fixed = TRUE)[[1]]
    if (length(p) == 2) { v <- utils::URLdecode(p[2]); Encoding(v) <- "UTF-8"; out[[p[1]]] <- v }
  }
  out
}
data_id <- function(raw) {
  f <- tempfile(); on.exit(unlink(f)); writeBin(raw, f)
  paste0(unname(tools::md5sum(f)), "-", length(raw))
}
get_data <- function(id) {
  if (is.null(id) || !exists(id, envir = STORE, inherits = FALSE)) return(NULL)
  get(id, envir = STORE)
}

# 同じパソコンのこのアプリの画面からの要求だけを受け付ける
same_origin <- function(req) {
  host <- if (is.null(req$HTTP_HOST)) "" else req$HTTP_HOST
  if (!grepl("^(127\\.0\\.0\\.1|localhost)(:[0-9]+)?$", host)) return(FALSE)
  origin <- req$HTTP_ORIGIN
  if (is.null(origin) || !nzchar(origin)) return(TRUE)
  grepl("^http://(127\\.0\\.0\\.1|localhost)(:[0-9]+)?$", origin)
}

route <- function(req) {
  path <- req$PATH_INFO; method <- req$REQUEST_METHOD
  # 起動画面（file:// で開く）からの問い合わせの事前確認。Edge・Chrome の「ローカル ネットワークへのアクセス」の確認にも答える
  if (identical(method, "OPTIONS"))
    return(list(status = 204L, headers = list("Access-Control-Allow-Origin" = "*", "Access-Control-Allow-Methods" = "GET, POST, OPTIONS",
                                               "Access-Control-Allow-Headers" = "Content-Type", "Access-Control-Allow-Private-Network" = "true",
                                               "Access-Control-Max-Age" = "600"), body = raw(0)))
  if (path == "/api/ping") {
    STATE$last_ping <- Sys.time(); STATE$had_client <- TRUE; STATE$bye_at <- NULL
    return(json_resp(list(ok = TRUE, app = APP_ID, version = APP_VERSION, r = paste(R.version$major, R.version$minor, sep = "."), ryear = R.version$year)))
  }
  if (!same_origin(req)) return(json_resp(list(ok = FALSE, message = tr_("この要求は受け付けられません。")), 403L))
  STATE$last_ping <- Sys.time()
  LANG <<- if (identical(query(req)$lang, "en")) "en" else "ja"
  if (path == "/api/bye") { STATE$bye_at <- Sys.time(); return(json_resp(list(ok = TRUE))) }
  if (path == "/api/registry") return(json_resp(list(ok = TRUE, analyses = unname(registry_json()))))
  if (path == "/api/load" && method == "POST") {
    raw <- read_body(req); qp <- query(req)
    df <- read_survey_csv(raw, if (is.null(qp$name)) "" else qp$name)
    id <- data_id(raw)
    for (k in ls(STORE)) rm(list = k, envir = STORE)       # 持つデータは 1 つだけ（メモリを増やさない）
    assign(id, df, envir = STORE)
    lim <- min(nrow(df), 2000L)
    rows <- lapply(seq_len(lim), function(i) unname(as.list(df[i, ])))
    return(json_resp(list(ok = TRUE, dataId = id, nrow = nrow(df), ncol = ncol(df), vars = describe_data(df),
                          rows = rows, rowsShown = lim)))
  }
  if (method != "POST") return(json_resp(list(ok = FALSE, message = tr_("見つかりません。")), 404L))
  # ---- プロジェクトの置き場（アプリのフォルダーの projects）
  if (path == "/api/proj/save") {
    qp <- query(req); nm <- proj_name(qp$name); raw <- read_body(req)
    if (!nzchar(nm)) stop_user(tr_("プロジェクトの名前を入れてください。"))
    dir.create(PROJECT_DIR, showWarnings = FALSE, recursive = TRUE)
    f <- file.path(PROJECT_DIR, paste0(nm, ".prj")); tmp <- paste0(f, ".tmp")
    writeBin(raw, tmp)
    if (!file.copy(tmp, f, overwrite = TRUE)) { unlink(tmp); stop_user(tr_("プロジェクトを保存できませんでした。フォルダーに書き込めるか確かめてください（"), PROJECT_DIR, tr_("）。")) }
    unlink(tmp); unlink(sub("\\.prj$", ".ctsproj", f))   # 古い拡張子の同じ名前のファイルは消す（一覧に 2 つ並ばないように）
    return(json_resp(list(ok = TRUE, name = nm, folder = normalizePath(PROJECT_DIR, winslash = "/", mustWork = FALSE))))
  }
  if (path == "/api/proj/list") {
    fs <- if (dir.exists(PROJECT_DIR)) list.files(PROJECT_DIR, pattern = "\\.(prj|ctsproj)$", full.names = TRUE) else character(0)
    info <- file.info(fs); o <- order(info$mtime, decreasing = TRUE)
    items <- lapply(o, function(i) { nm <- sub("\\.(prj|ctsproj)$", "", basename(fs[i])); Encoding(nm) <- "UTF-8"; list(name = nm, saved = format(info$mtime[i], "%Y-%m-%d %H:%M"), size = info$size[i]) })
    return(json_resp(list(ok = TRUE, items = items, folder = normalizePath(PROJECT_DIR, winslash = "/", mustWork = FALSE))))
  }
  if (path == "/api/proj/load" || path == "/api/proj/delete") {
    b <- read_json(req); f <- file.path(PROJECT_DIR, paste0(proj_name(b$name), ".prj")); if (!file.exists(f)) f <- sub("\\.prj$", ".ctsproj", f)
    if (!file.exists(f)) stop_user(tr_("プロジェクト「"), b$name, tr_("」が見つかりません。"))
    if (path == "/api/proj/delete") { file.remove(f); return(json_resp(list(ok = TRUE))) }
    return(list(status = 200L, headers = list("Content-Type" = "application/json; charset=utf-8", "Cache-Control" = "no-store"), body = readBin(f, "raw", file.info(f)$size)))
  }
  body <- read_json(req)
  df <- get_data(body$dataId)
  if (is.null(df)) return(json_resp(list(ok = FALSE, noData = TRUE, message = tr_("データが読み込まれていません。"))))
  if (length(body$computed)) df <- add_computed(df, body$computed)   # 画面で作った尺度得点の列
  ex <- as.integer(unlist(body$exclude)); ex <- ex[!is.na(ex) & ex >= 1 & ex <= nrow(df)]
  if (length(ex)) df <- df[-ex, , drop = FALSE]                       # 無効にした回答の行（元のデータの行番号）は、すべての計算から除く
  if (path == "/api/counts") {                      # 無効回答を除いた後の人数（「回答の内訳」と数値の要約）
    out <- lapply(body$vars, function(v) {
      x <- df[[as.integer(v$col)]]; nn <- x[!is.na(x)]; r <- list(col = v$col, n = length(nn))
      if (identical(v$type, "numeric")) {
        val <- numeric_values(x, v$fix, v$unit); val <- val[!is.na(val)]
        if (length(val)) r$summary <- list(min = min(val), max = max(val), mean = mean(val), median = stats::median(val))
      } else {
        tok <- if (identical(v$type, "multi")) unlist(split_multi(nn, if (is.null(v$delim)) ";" else v$delim)) else as.character(nn)
        tb <- table(tok); nm <- names(tb); Encoding(nm) <- "UTF-8"
        r$values <- A(nm); r$counts <- A(as.integer(tb))
      }
      r
    })
    return(json_resp(list(ok = TRUE, vars = out)))
  }
  if (path == "/api/varinfo") {
    x <- df[[body$col]]
    info <- category_info(x, body$type, if (is.null(body$delim)) ";" else body$delim)
    return(json_resp(list(ok = TRUE, cats = info$cats, ordinal = info$ordinal)))
  }
  if (path == "/api/timeinfo") {                    # 日時で回に分ける画面用：読み取った日時（1970-01-01 からの分）を古い順に返す
    t <- time_minutes(df[[body$col]]); bad <- sum(is.na(t) & !is.na(df[[body$col]]) & nzchar(trimws(as.character(df[[body$col]]))))
    return(json_resp(list(ok = TRUE, t = A(sort(t[!is.na(t)])), bad = bad)))
  }
  if (path == "/api/coltext") {                     # テキストマイニング用：絞り込み後の自由記述（と群）を画面へ返す。単語に分けるのは画面側
    keep <- apply_filter(df, body$filter); d <- df[keep, , drop = FALSE]
    tx <- as.character(d[[body$t$col]]); g <- if (!is.null(body$g)) as.character(slot_factor(d, body$g)) else NULL
    return(json_resp(list(ok = TRUE, text = A(ifelse(is.na(tx), "", tx)), group = if (is.null(g)) NULL else A(ifelse(is.na(g), "", g)))))
  }
  if (path == "/api/analyze") return(json_resp(score_method(run_analysis(df, body), body)))
  if (path == "/api/excel") return(json_resp(c(list(ok = TRUE), build_excel(df, body$items))))
  if (path == "/api/rscript") { r <- build_rscript(df, body$items, if (is.null(body$dataName)) tr_("解析用データ.csv") else body$dataName, APP_VERSION, if (is.null(body$date)) "" else body$date); return(json_resp(list(ok = TRUE, script = r$script, data = csv_text(rscript_data(df, r$numcols))))) }
  if (path == "/api/padjust") {
    p <- vapply(body$p, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0)
    return(json_resp(list(ok = TRUE, p = A(stats::p.adjust(p, method = "holm")))))
  }
  json_resp(list(ok = FALSE, message = tr_("見つかりません。")), 404L)
}

handle <- function(req) {
  tryCatch(route(req),
    user_error = function(e) json_resp(list(ok = FALSE, userError = TRUE, message = conditionMessage(e))),
    error = function(e) {
      message("[error] ", req$PATH_INFO, " : ", conditionMessage(e))
      json_resp(list(ok = FALSE, message = paste0(tr_("計算の途中でエラーが起きました（"), conditionMessage(e), tr_("）。列の組み合わせを変えてもう一度試してください。"))))
    })
}

start_app <- function(www, ports = 47815:47824, idle_exit = TRUE, log = function(...) NULL) {
  app <- list(call = handle,
              staticPaths = list("/" = httpuv::staticPath(www, indexhtml = TRUE, fallthrough = TRUE,
                                                          headers = list("Cache-Control" = "no-cache")),
                                 "/api" = httpuv::excludeStaticPath()))
  srv <- NULL; port <- NA
  for (p in ports) {
    srv <- tryCatch(httpuv::startServer("127.0.0.1", p, app), error = function(e) NULL)
    if (!is.null(srv)) { port <- p; break }
  }
  if (is.null(srv)) stop(tr_("使えるポートがありません（"), min(ports), tr_("〜"), max(ports), tr_("）。"))
  log("listening ", port)
  list(server = srv, port = port)
}

# 画面が閉じられたら終了する（最後の通信から 150 秒、または終了の合図から 4 秒）
serve_forever <- function(idle_exit = TRUE, fallback = NULL) {
  started <- Sys.time()
  repeat {
    httpuv::service(200)
    now <- Sys.time()
    if (!is.null(fallback) && !STATE$had_client && as.numeric(difftime(now, started, units = "secs")) > 25) { fallback(); fallback <- NULL }
    if (!idle_exit) next
    if (!is.null(STATE$bye_at) && as.numeric(difftime(now, STATE$bye_at, units = "secs")) > 4) break
    idle <- as.numeric(difftime(now, STATE$last_ping, units = "secs"))
    if (STATE$had_client && idle > 150) break
    if (!STATE$had_client && idle > 600) break
  }
}
