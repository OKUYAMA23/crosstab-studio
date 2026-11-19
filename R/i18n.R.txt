# i18n.R ― 結果の文の言語。文字列は tr_("日本語") で包み、英語のときは辞書（i18n_en.R の EN_DICT）の訳を返す。
# 言語は、画面からの要求ごとに LANG に入れる（server.R の route）。
LANG <- "ja"
tr_ <- function(s) {
  if (identical(LANG, "en") && exists("EN_DICT", inherits = TRUE)) { v <- EN_DICT[[s]]; if (!is.null(v)) return(v) }
  s
}
# 語順が英語と違う文は、文全体を書式（%s、%1$s など）にして訳す。tr_ と同じ辞書を使う
trf <- function(fmt, ...) sprintf(tr_(fmt), ...)
# 英語の文の途中に置く検定名：固有名で始まらない名前は頭を小文字にする（Chi-square test → chi-square test）
lc_ <- function(x) {
  if (!identical(LANG, "en")) return(x)
  i <- grepl("^(Chi-square|Paired|One-way|Independent|Multiple|Logistic|Exploratory|Confirmatory|Stratified|Simple|Cross|Frequencies|Item|Scale|Text|Response|Trend|Correspondence|Analysis|Exact)", x)
  x[i] <- paste0(tolower(substr(x[i], 1, 1)), substring(x[i], 2)); x
}
