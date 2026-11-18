# web_json.R ― iPad 版（webR）用。jsonlite を使わずに、R の値を JSON の文字列にする。
# パソコン版の to_json（jsonlite::toJSON(auto_unbox = TRUE, na = "null", null = "null", digits = NA)）と同じ形を出す。
.json_str <- function(s) {
  s <- enc2utf8(as.character(s))
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE); s <- gsub("\r", "\\r", s, fixed = TRUE); s <- gsub("\t", "\\t", s, fixed = TRUE)
  s <- gsub("\b", "\\b", s, fixed = TRUE); s <- gsub("\f", "\\f", s, fixed = TRUE)
  if (any(grepl("[\001-\037]", s, useBytes = TRUE)))
    s <- vapply(s, function(z) { ch <- strsplit(z, "", fixed = TRUE)[[1]]; cc <- vapply(ch, function(c) { u <- utf8ToInt(c); if (length(u) == 1 && !is.na(u) && u < 32) sprintf("\\u%04x", u) else c }, ""); paste(cc, collapse = "") }, "", USE.NAMES = FALSE)
  paste0("\"", s, "\"")
}
.json_atom <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, "Date") || inherits(x, "POSIXt")) x <- as.character(x)
  out <- if (is.logical(x)) ifelse(x, "true", "false")
         else if (is.character(x)) .json_str(x)
         else if (is.numeric(x)) { o <- if (is.integer(x)) as.character(x) else sprintf("%.15g", x); o[!is.finite(x)] <- "null"; o }
         else if (is.complex(x)) .json_str(as.character(x))
         else .json_str(as.character(x))
  out[is.na(x)] <- "null"
  out
}
to_json <- function(x) {
  if (is.null(x)) return("null")
  if (is.function(x) || is.environment(x)) return("null")
  if (is.data.frame(x)) {
    if (!nrow(x)) return("[]")
    rows <- lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
    return(paste0("[", paste(vapply(rows, function(r) to_json(lapply(r, function(v) if (length(v) == 1 && is.na(v)) NULL else unclass(v))), ""), collapse = ","), "]"))
  }
  if (is.list(x)) {
    nm <- names(x)
    parts <- vapply(x, to_json, "")
    if (!is.null(nm) && length(x)) {
      nm[is.na(nm) | nm == ""] <- as.character(seq_along(x))[is.na(nm) | nm == ""]
      return(paste0("{", paste0(.json_str(nm), ":", parts, collapse = ","), "}"))
    }
    if (!is.null(nm) && !length(x)) return("{}")
    return(paste0("[", paste(parts, collapse = ","), "]"))
  }
  d <- dim(x)
  if (length(d) == 2) {
    if (!d[1]) return("[]")
    a <- matrix(.json_atom(as.vector(x)), d[1], d[2])
    return(paste0("[", paste0("[", apply(a, 1, paste, collapse = ","), "]", collapse = ","), "]"))
  }
  if (is.factor(x)) x <- as.character(x)
  if (!length(x)) return("[]")
  a <- .json_atom(as.vector(unclass(x)))
  if (length(a) == 1 && !inherits(x, "AsIs")) return(a)
  paste0("[", paste(a, collapse = ","), "]")
}
