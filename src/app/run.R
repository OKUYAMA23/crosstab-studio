# run.R ― CrossTab Studio を起動する。
#   Rscript run.R            通常の起動（画面が閉じられたら自動で終了）
#   Rscript run.R --keep     画面を閉じても終了しない（確認用）
#   Rscript run.R --browser  アプリ用の窓ではなく、ふだんのブラウザで開く
#   Rscript run.R --noopen   画面を開かない（起動用ファイルが別に開くとき）
args <- commandArgs(trailingOnly = TRUE)
full <- commandArgs(FALSE)
this <- sub("^--file=", "", full[grep("^--file=", full)])
app_dir <- if (length(this)) dirname(normalizePath(this, winslash = "/")) else getwd()
log_file <- file.path(app_dir, "..", "startup.log")
logf <- function(...) {
  line <- paste0(format(Sys.time(), "%H:%M:%S"), " ", paste0(..., collapse = ""))
  cat(line, "\n"); try(cat(line, "\n", file = log_file, append = TRUE), silent = TRUE)
}
try(cat("", file = log_file), silent = TRUE)
logf("R ", R.version$major, ".", R.version$minor, " / ", app_dir)

if (getRversion() < "4.2.0") { logf("R 4.2 以上が必要です。"); quit(status = 2) }
lib <- file.path(app_dir, "library")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
need <- c("httpuv", "jsonlite")
miss <- need[!vapply(need, requireNamespace, TRUE, quietly = TRUE)]
if (length(miss)) {
  logf("パッケージを入れます: ", paste(miss, collapse = ", "))
  dir.create(lib, showWarnings = FALSE)
  ok <- tryCatch({ utils::install.packages(miss, lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE); TRUE }, error = function(e) FALSE)
  .libPaths(c(lib, .libPaths()))
  miss <- need[!vapply(need, requireNamespace, TRUE, quietly = TRUE)]
  if (length(miss)) { logf("パッケージ ", paste(miss, collapse = ", "), " を入れられませんでした。インターネットにつないでもう一度起動してください。"); quit(status = 3) }
}
# ソースは UTF-8。パソコンの文字コードの設定に左右されないよう、UTF-8 と明示して読み込む
for (f in c("data.R", "stats.R", "analyses.R", "analyses2.R", "excel.R", "rexport.R", "server.R")) {
  src <- readLines(file.path(app_dir, "R", f), encoding = "UTF-8", warn = FALSE)
  eval(parse(text = src, encoding = "UTF-8", keep.source = FALSE), envir = globalenv())
}

PROJECT_DIR <- file.path(dirname(app_dir), "projects")
st <- start_app(file.path(app_dir, "www"), log = logf)
url <- paste0("http://127.0.0.1:", st$port, "/")
logf("ready ", url)
# 起動画面（splash.html）に、準備ができたことと番号（ポート）を知らせる。起動画面は file:// で開くので、
# ブラウザの設定で 127.0.0.1 への問い合わせが止められても、同じフォルダーのこのファイルは読める
ready_file <- file.path(app_dir, "www", "ready.js")
tok <- sub("^--token=", "", grep("^--token=", args, value = TRUE)[1]); if (is.na(tok)) tok <- ""
tok <- gsub("[^0-9A-Za-z]", "", tok)
write_ready <- function(port) try(writeLines(sprintf("window.CTS = { port: %d, t: %.0f, k: '%s' };", port, as.numeric(Sys.time()) * 1000, tok), ready_file), silent = TRUE)
write_ready(st$port)

open_window <- function(url, as_app = TRUE) {
  if (.Platform$OS.type == "windows") {
    cand <- c(file.path(Sys.getenv("ProgramFiles(x86)"), "Microsoft/Edge/Application/msedge.exe"),
              file.path(Sys.getenv("ProgramFiles"), "Microsoft/Edge/Application/msedge.exe"),
              file.path(Sys.getenv("ProgramFiles"), "Google/Chrome/Application/chrome.exe"),
              file.path(Sys.getenv("ProgramFiles(x86)"), "Google/Chrome/Application/chrome.exe"),
              file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe"))
    exe <- cand[file.exists(cand)]
    # system2() は窓を隠す指定を相手に渡してしまうので、start 命令を通す
    if (as_app && length(exe)) shell(paste0('start "" "', normalizePath(exe[1]), '" --app=', url, ' --window-size=1320,860'), wait = FALSE)
    else shell(paste0('start "" "', url, '"'), wait = FALSE)
  } else utils::browseURL(url)
}
if (!("--noopen" %in% args)) try(open_window(url, as_app = !("--browser" %in% args)))
# 起動画面（splash.html）からつながらないときは、25 秒後にふだんのブラウザで開き直す
fallback <- if ("--fallback" %in% args) function() try(open_window(url, as_app = FALSE)) else NULL
serve_forever(idle_exit = !("--keep" %in% args), fallback = fallback)
write_ready(0)
logf("終了しました。")
