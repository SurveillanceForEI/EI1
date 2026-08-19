# 神奈川県「表１ 報告数・定点当たり報告数 疾病、政令市・保健所別」PDF
# https://www.pref.kanagawa.jp/sys/eiken/003_center/0001_weekly/pdf/wrR{YEAR2桁}_{WEEK}.pdf
# （例: 令和8年第31週 → wrR08_31.pdf）
#
# 各保健福祉事務所は1行1レコードで、行内のトークンはx座標順に
# そのまま疾病の列順と一致する（複数行に折り返す見出しはあるが、
# データ行自体は単純な横並び）。「秦野センター」等のサブセンターは
# 親事務所（平塚・鎌倉・小田原・厚木）に合算する。「全県」「県域」
# は集計値のため除外する。

.kanagawa_block1 <- c("急性呼吸器感染症(ARI)", "インフルエンザ(高病原性鳥インフルエンザを除く)",
                       "新型コロナウイルス感染症", "インフルエンザ（入院）",
                       "新型コロナウイルス感染症（入院）")
.kanagawa_block1_types <- c("cr", "cr", "cr", "c", "c")
.kanagawa_block2 <- c("RSウイルス感染症", "咽頭結膜熱", "Ａ群溶血性レンサ球菌咽頭炎",
                       "感染性胃腸炎", "水痘")
.kanagawa_block3 <- c("手足口病", "伝染性紅斑", "突発性発しん", "ヘルパンギーナ", "流行性耳下腺炎")
.kanagawa_block4 <- c("急性出血性結膜炎", "流行性角結膜炎")
.kanagawa_block5 <- c("細菌性髄膜炎", "無菌性髄膜炎", "マイコプラズマ肺炎",
                       "クラミジア肺炎（オウム病を除く）", "感染性胃腸炎（ロタウイルス）")

.kanagawa_exclude <- c("全県", "県域")
# センター（支所）は親事務所とは別の市区町村を管轄する別管区のため、
# 保健所境界データも親事務所とは別ポリゴンとして扱う（ユーザー指示）。
# 例:「平塚保健福祉事務所」は平塚市・大磯町・二宮町・寒川町、
# 「同 秦野センター」は秦野市・伊勢原市というように管轄が完全に分かれる。
.kanagawa_center_rename <- c(
  "平塚秦野センター" = "秦野",
  "鎌倉三崎センター" = "三浦",
  "小田原足柄上センター" = "足柄上",
  "厚木大和センター" = "大和"
)
.kanagawa_name_fix <- c("茅ケ崎市" = "茅ヶ崎市")

fetch_kanagawa <- function(pdf_url = NULL) {
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")
  if (is.null(pdf_url)) {
    pdf_url <- "https://www.pref.kanagawa.jp/sys/eiken/003_center/0001_weekly/pdf/wrR08_31.pdf"
  }

  path <- tempfile(fileext = ".pdf")
  download.file(pdf_url, path, mode = "wb", quiet = TRUE)
  n_pages <- length(pdftools::pdf_data(path))

  page1_txt <- NA_integer_
  page2_txt <- NA_integer_
  for (p in seq_len(n_pages)) {
    w <- pdftools::pdf_data(path)[[p]]
    txt <- paste(w$text, collapse = "")
    if (grepl("政令市・保健所別", txt) && grepl("（その１）", txt)) page1_txt <- p
    if (grepl("政令市・保健所別", txt) && grepl("（その２）", txt)) page2_txt <- p
  }
  if (is.na(page1_txt)) stop("表１（その１）のページが見つかりません")

  week_label <- NA_character_

  parse_block_page <- function(page, block_defs) {
    # block_defs: list(list(y_start, y_end, diseases, types=c("cr"|"c",...)))
    w <- pdftools::pdf_data(path)[[page]]
    wl <- w$text[grepl("^20[0-9]{2}年[0-9]+週", w$text)]
    if (length(wl) > 0) week_label <<- regmatches(wl[1], regexpr("^20[0-9]{2}年[0-9]+週", wl[1]))

    rows <- group_words_into_rows(w, y_tol = 3)
    results <- list()
    for (bd in block_defs) {
      types <- if (is.null(bd$types)) rep("cr", length(bd$diseases)) else bd$types
      n_needed <- sum(ifelse(types == "cr", 2, 1))
      out <- list()
      pending_name <- NULL
      for (i in seq_along(rows)) {
        rr <- rows[[i]]
        if (rr$y[1] < bd$y_start || rr$y[1] > bd$y_end) next
        rr <- rr[order(rr$x), ]
        name_tok <- rr$text[rr$x < 100]
        val_tok_df <- rr[rr$x >= 100, ]
        if (length(name_tok) == 0 && is.null(pending_name)) next
        hj_raw <- if (length(name_tok) > 0) gsub("\\s+", "", paste(name_tok, collapse = "")) else pending_name
        if (nrow(val_tok_df) == 0) { pending_name <- hj_raw; next }
        pending_name <- NULL
        if (hj_raw %in% .kanagawa_exclude) next
        vals <- val_tok_df$text
        if (length(vals) < n_needed) next
        vals <- vals[seq_len(n_needed)]
        pos <- 1
        for (di in seq_along(bd$diseases)) {
          if (types[di] == "c") {
            cnt <- parse_hokenjo_number(vals[pos])
            rt <- NA_real_
            pos <- pos + 1
          } else {
            cnt <- parse_hokenjo_number(vals[pos])
            rt <- parse_hokenjo_number(vals[pos + 1])
            pos <- pos + 2
          }
          out[[length(out) + 1]] <- data.frame(
            hokenjo = hj_raw, disease = bd$diseases[di], count = cnt, rate = rt,
            stringsAsFactors = FALSE
          )
        }
      }
      results[[length(results) + 1]] <- do.call(rbind, out)
    }
    do.call(rbind, results)
  }

  w1 <- pdftools::pdf_data(path)[[page1_txt]]
  y_all <- sort(unique(w1$y))
  # block1（急性呼吸器感染症/インフル/コロナ + 入院）はy=161(見出し)以降～258付近
  # block2, block3 はそれぞれ次の「報告数」ヘッダー行から
  hdr_ys <- w1$y[w1$text == "報告数"]
  hdr_ys <- sort(unique(hdr_ys))

  df1 <- parse_block_page(page1_txt, list(
    list(y_start = hdr_ys[1] - 2, y_end = hdr_ys[2] - 15, diseases = .kanagawa_block1, types = .kanagawa_block1_types)
  ))
  df2 <- parse_block_page(page1_txt, list(
    list(y_start = hdr_ys[2] - 2, y_end = hdr_ys[3] - 15, diseases = .kanagawa_block2)
  ))
  df3 <- parse_block_page(page1_txt, list(
    list(y_start = hdr_ys[3] - 2, y_end = 10000, diseases = .kanagawa_block3)
  ))

  df4 <- NULL
  df5 <- NULL
  if (!is.na(page2_txt)) {
    w2 <- pdftools::pdf_data(path)[[page2_txt]]
    hdr_ys2 <- sort(unique(w2$y[w2$text == "報告数"]))
    df4 <- parse_block_page(page2_txt, list(
      list(y_start = hdr_ys2[1] - 2, y_end = hdr_ys2[2] - 15, diseases = .kanagawa_block4)
    ))
    df5 <- parse_block_page(page2_txt, list(
      list(y_start = hdr_ys2[2] - 2, y_end = 10000, diseases = .kanagawa_block5)
    ))
  }

  df <- do.call(rbind, list(df1, df2, df3, df4, df5))
  df <- df[!is.null(df$hokenjo) & !is.na(df$hokenjo), ]

  # センターは親事務所とは別管区として扱う（合算しない）
  df$hokenjo <- ifelse(df$hokenjo %in% names(.kanagawa_center_rename),
                        .kanagawa_center_rename[df$hokenjo], df$hokenjo)
  df$hokenjo <- ifelse(df$hokenjo %in% names(.kanagawa_name_fix), .kanagawa_name_fix[df$hokenjo], df$hokenjo)

  agg <- aggregate(cbind(count, rate) ~ hokenjo + disease, data = df, FUN = function(x) sum(x, na.rm = TRUE), na.action = na.pass)
  # aggregateはNA同士の合計を0にしてしまうため、全てNAだった場合はNAに戻す
  has_any <- aggregate(count ~ hokenjo + disease, data = df, FUN = function(x) any(!is.na(x)), na.action = na.pass)
  agg <- merge(agg, has_any, by = c("hokenjo", "disease"), suffixes = c("", "_has"))
  agg$count[!agg$count_has] <- NA_real_
  agg$rate[!agg$count_has] <- NA_real_
  agg$count_has <- NULL
  agg$rate[agg$disease %in% c("インフルエンザ（入院）", "新型コロナウイルス感染症（入院）")] <- NA_real_

  agg$pref <- "神奈川県"
  agg$week_label <- week_label
  agg <- agg[, c("pref", "week_label", "hokenjo", "disease", "count", "rate")]
  rownames(agg) <- NULL
  agg
}
