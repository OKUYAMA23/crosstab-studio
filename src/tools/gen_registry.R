# AGENTS.md 用：解析の一覧（id・名前・欄・オプション）を日本語と英語で JSON に書き出す
#   Rscript tools/gen_registry.R <app/R のフォルダー> <出力の JSON>
args <- commandArgs(TRUE); dir <- args[1]
for (f in c("i18n.R", "i18n_en.R", "data.R", "stats.R", "analyses.R", "analyses2.R"))
  eval(parse(text = readLines(file.path(dir, f), encoding = "UTF-8"), encoding = "UTF-8"), envir = globalenv())
out <- list()
for (lg in c("ja", "en")) { LANG <<- lg; out[[lg]] <- unname(registry_json()) }
writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = FALSE), args[2], useBytes = TRUE)
