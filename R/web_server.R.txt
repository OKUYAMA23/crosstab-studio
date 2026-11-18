# web_server.R ― iPad 版（webR）用。パソコン版の server.R の route() をそのまま使い、
# 通信の代わりに、画面側（bridge.js）が書いたファイルを読み、結果をファイルに書く。
#   /tmp/cts_req.bin … 要求の中身（CSV のバイト列、または JSON の文字列）
#   /tmp/cts_req.R   … JSON の要求を R の式に直したもの（jsonlite::fromJSON(simplifyVector = FALSE) と同じ形）
#   /tmp/cts_res.bin … 返す JSON
read_json <- function(req) {
  f <- "/tmp/cts_req.R"
  if (!file.exists(f)) return(list())
  eval(parse(file = f, encoding = "UTF-8", keep.source = FALSE)[[1]], envir = baseenv())
}
web_handle <- function(path, qs, method) {
  req <- list(PATH_INFO = path, QUERY_STRING = qs, REQUEST_METHOD = method, HTTP_HOST = "127.0.0.1",
              rook.input = list(read = function() {
                f <- "/tmp/cts_req.bin"
                if (file.exists(f)) readBin(f, "raw", file.info(f)$size) else raw(0)
              }))
  r <- handle(req)
  writeBin(r$body, "/tmp/cts_res.bin")
  as.integer(r$status)
}
