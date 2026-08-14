# ============================================================
# PDF表形式パーサー共通ユーティリティ（座標ベース）
# ------------------------------------------------------------
# pdftools::pdf_data() は単語ごとのバウンディングボックス（x, y座標）を
# 返すため、正規表現による行分割（pdf_text()の空白区切り）よりも
# 表構造（列位置のズレ、複数行にまたがる見出し等）に頑健に対応できる。
#
# 基本的な使い方：
#   words <- pdf_words(url, page = 1)
#   rows  <- group_words_into_rows(words, y_tol = 3)
#   # rows は 1行分の単語（x座標順）を格納したリストのリスト
# ============================================================

pdf_words <- function(url_or_path, page = 1) {
  if (!requireNamespace("pdftools", quietly = TRUE)) stop("pdftools パッケージが必要です")
  is_url <- grepl("^https?://", url_or_path)
  path <- url_or_path
  if (is_url) {
    path <- tempfile(fileext = ".pdf")
    download.file(url_or_path, path, mode = "wb", quiet = TRUE)
  }
  d <- pdftools::pdf_data(path)[[page]]
  d
}

# 同じ行とみなす単語をy座標（±y_tol）でグルーピングし、x座標順に並べる
group_words_into_rows <- function(words, y_tol = 3) {
  words <- words[order(words$y, words$x), ]
  rows <- list()
  cur_y <- NA
  cur <- list()
  for (i in seq_len(nrow(words))) {
    w <- words[i, ]
    if (is.na(cur_y) || abs(w$y - cur_y) > y_tol) {
      if (length(cur) > 0) rows[[length(rows) + 1]] <- do.call(rbind, cur)
      cur <- list(w)
      cur_y <- w$y
    } else {
      cur[[length(cur) + 1]] <- w
      cur_y <- mean(c(cur_y, w$y))
    }
  }
  if (length(cur) > 0) rows[[length(rows) + 1]] <- do.call(rbind, cur)
  rows
}

# 行（word data.frame、x昇順）からテキストを再構成
row_text <- function(row_df) paste(row_df$text, collapse = " ")

# 指定x範囲内の単語だけを連結して返す（列の切り出し用）
row_cell <- function(row_df, x_min, x_max) {
  sub <- row_df[row_df$x >= x_min & row_df$x < x_max, ]
  if (nrow(sub) == 0) return(NA_character_)
  paste(sub$text, collapse = "")
}
