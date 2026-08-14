# 大阪府「感染症情報センター」週報HTML
# https://www.iph.pref.osaka.jp/infection/surv{YEAR2桁}/surv{WEEK}t.html
#
# Shift-JISエンコードのHTMLページに <table> が4つあり、
# 1: 定点医療機関数, 2: 定点あたり患者報告数(rate), 3: 患者報告数(count),
# 4: ブロック別年齢別発生状況
# table2・table3 は同じ疾患順・同じ11ブロック列（豊能/三島/北河内/
# 中河内/南河内/堺市/泉州/大阪市北部/大阪市西部/大阪市東部/大阪市南部）
# で構成されており、「合計」列・「合　計」行は除外して結合する。

fetch_osaka <- function(html_url) {
  if (missing(html_url) || is.null(html_url)) stop("html_url を指定してください")
  if (!requireNamespace("rvest", quietly = TRUE)) stop("rvest パッケージが必要です")
  tf <- tempfile(fileext = ".html")
  download.file(html_url, tf, mode = "wb", quiet = TRUE)
  raw <- readLines(tf, warn = FALSE, encoding = "unknown")
  raw <- iconv(raw, from = "shift-jis", to = "UTF-8", sub = "")
  txt <- paste(raw, collapse = "\n")
  html <- rvest::read_html(txt)
  tabs <- rvest::html_table(html, fill = TRUE)
  if (length(tabs) < 3) stop("想定するテーブル数(>=3)が見つかりません")

  alltext <- rvest::html_text(html)
  week_m <- regmatches(alltext, regexpr("20[0-9]{2}年 ?第[0-9]+週", alltext))
  week_label <- if (length(week_m) > 0) gsub(" ", "", week_m[1]) else NA_character_

  rate_tab <- as.data.frame(tabs[[2]])
  count_tab <- as.data.frame(tabs[[3]])

  hokenjo_cols <- c("豊能", "三島", "北河内", "中河内", "南河内", "堺市", "泉州",
                     "大阪市北部", "大阪市西部", "大阪市東部", "大阪市南部")

  melt_tab <- function(tab, value_name) {
    name_col <- names(tab)[1]
    tab <- tab[!grepl("^合\\s*計", tab[[name_col]]), ]
    out <- list()
    for (i in seq_len(nrow(tab))) {
      disease <- tab[[name_col]][i]
      for (h in hokenjo_cols) {
        if (!(h %in% names(tab))) next
        v <- tab[[h]][i]
        out[[length(out) + 1]] <- data.frame(hokenjo = h, disease = disease,
                                              value = suppressWarnings(as.numeric(v)),
                                              stringsAsFactors = FALSE)
      }
    }
    d <- do.call(rbind, out)
    names(d)[names(d) == "value"] <- value_name
    d
  }

  rate_long <- melt_tab(rate_tab, "rate")
  count_long <- melt_tab(count_tab, "count")

  merged <- merge(count_long, rate_long, by = c("hokenjo", "disease"), all = TRUE)
  merged$pref <- "大阪府"
  merged$week_label <- week_label
  merged[, c("pref", "week_label", "hokenjo", "disease", "count", "rate")]
}
