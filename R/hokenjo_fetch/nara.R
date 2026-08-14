# 奈良県「奈良県感染症情報」週報PDF（定点把握感染症報告状況）
# https://www.pref.nara.lg.jp/documents/4352/0831.pdf 型（URLは週ごとに変わる）
#
# p.2（通常）の左側の表に「県全体(奈良県)/奈良市/郡山/中和（東）/
# 中和（西）/吉野」の6列×疾患別に、上段=報告数(count)・下段=(定点
# 当たり報告数)(rate) が2行1組で並ぶ。行はやや不規則（疾患名単独行、
# 数値行、括弧付き率行の順序が疾患により前後する）ため、行を走査
# しながら数値行(括弧なし)→疾患名→率行(括弧あり)の並びを検出する。
# 「中和（東）」「中和（西）」は境界データでは単一の「中和」保健所
# として扱われているため、呼び出し側で必要に応じて合算すること。

NARA_COLS <- c("県全体" = 98, "奈良市" = 118, "郡山" = 139,
               "中和（東）" = 157, "中和（西）" = 176, "吉野" = 196)

NARA_DISEASES <- c("インフルエンザ", "新型コロナウイルス感染症", "RSウイルス感染症",
                    "咽頭結膜熱", "A群溶連菌咽頭炎", "感染性胃腸炎", "水痘", "手足口病",
                    "伝染性紅斑", "突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎",
                    "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎", "無菌性髄膜炎",
                    "マイコプラズマ肺炎", "クラミジア肺炎", "感染性胃腸炎（ロタウイルス）")

fetch_nara <- function(pdf_url, page = 2) {
  if (missing(pdf_url) || is.null(pdf_url)) stop("pdf_url を指定してください")
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  words <- pdf_words(pdf_url, page = page)

  reiwa_hit <- words[words$text == "令和", ]
  week_label <- NA_character_
  if (nrow(reiwa_hit) > 0) {
    ry <- reiwa_hit$y[1]
    wline <- words[words$y > ry - 3 & words$y < ry + 3, ]
    wline <- wline[order(wline$x), ]
    m <- regmatches(paste(wline$text, collapse = ""),
                     regexpr("令和[0-9０-９]+年第[0-9０-９]+週", paste(wline$text, collapse = "")))
    if (length(m) > 0) week_label <- m[1]
  }

  sub <- words[words$x < 230 & words$y > 60 & words$y < 520, ]
  rows <- group_words_into_rows(sub, y_tol = 3)

  assign_col <- function(x) {
    d <- abs(NARA_COLS - x)
    if (min(d) > 12) return(NA_character_)
    names(NARA_COLS)[which.min(d)]
  }

  is_num <- function(t) grepl("^[0-9]+$", t)
  is_paren <- function(t) grepl("^\\([0-9]+\\.[0-9]+\\)$", t)

  # 「（ロタウイルス）」は前の「感染性胃腸炎」に結合させる特別処理のため事前結合
  seen_gi <- 0

  out <- list()
  pending_count <- NULL  # data.frame(x, text) 直近の数値のみの行
  i <- 1
  n <- length(rows)
  while (i <= n) {
    rdf <- rows[[i]]
    toks <- rdf$text
    num_mask <- is_num(toks)
    disease_here <- toks[!num_mask & !is_paren(toks)]
    disease_here <- disease_here[disease_here != ""]

    if (length(disease_here) == 0 && any(num_mask) && !any(is_paren(toks))) {
      # 純粋な数値のみの行 → 次の疾患名行のcountとして保持
      pending_count <- rdf[num_mask, ]
      i <- i + 1
      next
    }

    if (length(disease_here) > 0) {
      dname <- paste(disease_here, collapse = "")
      if (dname == "（ロタウイルス）" || dname == "(ロタウイルス)") {
        dname <- "感染性胃腸炎（ロタウイルス）"
      }
      if (!(dname %in% NARA_DISEASES)) { i <- i + 1; next }

      count_row <- if (any(num_mask)) rdf[num_mask, ] else pending_count
      pending_count <- NULL

      # 率行を探す（同一行内の括弧、または次の1-2行）
      rate_row <- NULL
      if (any(is_paren(toks))) {
        rate_row <- rdf[is_paren(toks), ]
      } else if (i + 1 <= n) {
        nxt <- rows[[i + 1]]
        if (any(is_paren(nxt$text))) rate_row <- nxt[is_paren(nxt$text), ]
      }

      if (!is.null(count_row)) {
        for (k in seq_len(nrow(count_row))) {
          col <- assign_col(count_row$x[k])
          if (is.na(col)) next
          out[[length(out) + 1]] <- data.frame(
            pref = "奈良県", week_label = week_label, hokenjo = col, disease = dname,
            count = parse_hokenjo_number(count_row$text[k]), rate = NA_real_,
            stringsAsFactors = FALSE
          )
        }
      }
      if (!is.null(rate_row)) {
        for (k in seq_len(nrow(rate_row))) {
          col <- assign_col(rate_row$x[k])
          if (is.na(col)) next
          rv <- parse_hokenjo_number(gsub("[()]", "", rate_row$text[k]))
          # 既存のcount行に対応する行があればrateを埋める。なければ新規行(count=NA)
          idx <- which(vapply(out, function(o) isTRUE(o$hokenjo == col) && isTRUE(o$disease == dname), logical(1)))
          if (length(idx) > 0) {
            out[[idx[length(idx)]]]$rate <- rv
          } else {
            out[[length(out) + 1]] <- data.frame(
              pref = "奈良県", week_label = week_label, hokenjo = col, disease = dname,
              count = NA_real_, rate = rv, stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    i <- i + 1
  }
  do.call(rbind, out)
}
