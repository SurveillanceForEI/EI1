# 佐賀県「定点報告：五類感染症（週報分）」PDF（API経由生成、p.3）
# https://kansen.pref.saga.jp/api/report/openPdf?schemaname=public&yw={YEAR}{WEEK}
# 年週一覧: https://kansen.pref.saga.jp/api/report （JSON, list[].yw / y / w）
#
# レイアウト: 5保健福祉事務所（佐賀中部/鳥栖/唐津/伊万里/杵藤）×
# 各疾患の「報告数 定点当たり」ペアが横並び。0件のセルは空白（トークンが
# 存在しない）ため、x座標ベースの列判定が必須。また疾患名が2〜3行に
# 折り返されるため、y方向の間隔（同一疾患内は間隔が小さい、次の疾患との
# 間は間隔が大きい）で論理行にまとめてからキーワードで疾患を同定する。

.SAGA_HOKENJO <- c("佐賀中部", "鳥栖", "唐津", "伊万里", "杵藤")
.SAGA_X_BINS <- c(114, 168.5, 223.5, 278.5, 334, 387)

.SAGA_KEYWORDS <- list(
  list("インフルエンザ", "インフルエンザ", "コロナ"),
  list("新型コロナウイルス感染症(COVID-19)", "コロナ", NULL),
  list("ＲＳウイルス感染症", "ＲＳ", NULL),
  list("咽頭結膜熱", "咽頭結膜熱", NULL),
  list("A群溶血性レンサ球菌咽頭炎", "レン", NULL),
  list("感染性胃腸炎", "感染性胃腸炎", "ロタ"),
  list("水痘", "水痘", NULL),
  list("手足口病", "手足口病", NULL),
  list("伝染性紅斑", "伝染性紅斑", NULL),
  list("突発性発しん", "突発性発", NULL),
  list("ヘルパンギーナ", "ヘルパンギーナ", NULL),
  list("流行性耳下腺炎", "耳下腺", NULL),
  list("急性出血性結膜炎", "出血性結膜炎", NULL),
  list("流行性角結膜炎", "角結膜炎", NULL),
  list("細菌性髄膜炎", "細菌性髄膜炎", NULL),
  list("無菌性髄膜炎", "無菌性髄膜炎", NULL),
  list("マイコプラズマ肺炎", "マイコプラズマ", NULL),
  list("クラミジア肺炎(オウム病を除く)", "クラミジア", NULL),
  list("感染性胃腸炎(ロタウイルスに限る)", "ロタウイルス", NULL)
)

# [旧実装] yの間隔（固定閾値）だけで論理行をまとめる方式は、疾患名が2〜3行に
# 折り返される一方で、隣接する別疾患の見出し行との行間もほぼ同じ間隔になる
# ケースがあり（例: インフルエンザ／新型コロナ／ＲＳウイルス感染症が全て
# 1つの巨大グループに誤結合されていた）、疾患の取りこぼしを起こしていた。
# 代わりに、x座標で「疾患名欄」（表の定点数値列より左）と「数値欄」を区別し、
# 数値を含まない行（＝疾患名の続き）を数値行が現れるまでバッファに貯めて
# 結合する方式に変更する。数値行に到達したら、それまでのバッファ＋その行
# 自身の疾患名欄テキストを結合してキーワード照合し、数値はその行のx区間
# から抽出する。数値行を処理した後はバッファをクリアする
# （数値行の後に続く注記行、例:「（オウム病を除く）」等が次の疾患名に
# 混入するのを防ぐため）。

# 佐賀県公開のAPI（年週一覧）から実際に存在する最新のyw（例:"202633"）を
# 取得する。ハードコードした週を使うと、その週を過ぎた時点で自動更新が
# 止まってしまうため（実際に202631で固定されたまま動かなくなっていた）
resolve_saga_latest_yw <- function() {
  if (!requireNamespace("httr", quietly = TRUE)) stop("httr パッケージが必要です")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite パッケージが必要です")
  r <- httr::GET("https://kansen.pref.saga.jp/api/report")
  txt <- httr::content(r, as = "text", encoding = "UTF-8")
  d <- jsonlite::fromJSON(txt)
  sprintf("%06d", max(d$list$yw, na.rm = TRUE))
}

fetch_saga <- function(pdf_url = NULL, yw = NULL) {
  if (is.null(pdf_url)) {
    if (is.null(yw)) yw <- "202631"
    pdf_url <- paste0("https://kansen.pref.saga.jp/api/report/openPdf?schemaname=public&yw=", yw)
  }
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  words <- pdf_words(pdf_url, page = 3)
  is_num_count <- function(s) grepl("^[0-9,]+$", s)
  is_num_rate <- function(s) grepl("^[0-9]+\\.[0-9]+$", s)

  if (is.null(yw)) {
    m <- regmatches(pdf_url, regexpr("yw=[0-9]{6}", pdf_url))
    yw <- if (length(m) > 0) sub("yw=", "", m) else NA_character_
  }
  week_label <- if (!is.na(yw)) paste0(substr(yw, 1, 4), "年第", as.integer(substr(yw, 5, 6)), "週") else NA_character_

  rows <- group_words_into_rows(words, y_tol = 3)
  bins <- .SAGA_X_BINS
  hokenjo <- .SAGA_HOKENJO
  is_num_any <- function(s) grepl("^[0-9,]+(\\.[0-9]+)?$", s)

  # ヘッダー部（タイトル・列見出し等）には表の数値列と重なるx位置に
  # 偶然数字（週番号など）が入ることがあり、疾患データ行と誤認されうる。
  # 「病名」列見出し行より前は表本体ではないため処理対象から除外する。
  header_idx <- which(vapply(rows, function(r) any(r$text == "病名"), logical(1)))
  start_row <- if (length(header_idx) > 0) max(header_idx) + 1 else 1

  matched <- rep(FALSE, length(.SAGA_KEYWORDS))
  out <- list()
  pending_name <- ""
  n_rows <- length(rows)
  i <- start_row
  while (i <= n_rows) {
    r <- rows[[i]]
    is_name_col <- r$x < bins[1]
    name_tokens <- r$text[is_name_col]
    data_row <- r[!is_name_col, ]
    has_data <- nrow(data_row) > 0 && any(vapply(data_row$text, is_num_any, logical(1)))

    combined <- paste0(pending_name, paste(name_tokens, collapse = ""))

    if (!has_data) {
      # 疾患名（または注記）の続きの行。数値行が現れるまでバッファに貯める
      pending_name <- combined
      i <- i + 1
      next
    }

    # 一部の疾患（例: 「感染性胃腸炎(ロタウイルスに限る)」）は識別に必要な
    # 語（「ロタ」等）が数値行より後ろの注記行にしか現れないため、直後に
    # 続く疾患名のみの行（次の数値行が現れるまで）も先読みして結合する
    lookahead_text <- ""
    j <- i + 1
    while (j <= n_rows) {
      rj <- rows[[j]]
      is_name_col_j <- rj$x < bins[1]
      data_row_j <- rj[!is_name_col_j, ]
      has_data_j <- nrow(data_row_j) > 0 && any(vapply(data_row_j$text, is_num_any, logical(1)))
      if (has_data_j) break
      lookahead_text <- paste0(lookahead_text, paste(rj$text[is_name_col_j], collapse = ""))
      j <- j + 1
    }
    combined_full <- paste0(combined, lookahead_text)

    di_matched <- NA_integer_
    for (di in seq_along(.SAGA_KEYWORDS)) {
      if (matched[di]) next
      req <- .SAGA_KEYWORDS[[di]][[2]]
      excl <- .SAGA_KEYWORDS[[di]][[3]]
      # まず数値行までのテキストのみで照合し、マッチしなければ後続の
      # 注記行まで含めたテキストで再照合する
      hit <- grepl(req, combined, fixed = TRUE)
      if (hit && !is.null(excl)) hit <- hit && !grepl(excl, combined, fixed = TRUE)
      if (!hit) {
        hit <- grepl(req, combined_full, fixed = TRUE)
        if (hit && !is.null(excl)) hit <- hit && !grepl(excl, combined_full, fixed = TRUE)
      }
      if (hit) {
        di_matched <- di
        break
      }
    }

    if (!is.na(di_matched)) {
      matched[di_matched] <- TRUE
      dname <- .SAGA_KEYWORDS[[di_matched]][[1]]
      for (k in seq_along(hokenjo)) {
        cell <- data_row[data_row$x >= bins[k] & data_row$x < bins[k + 1], ]
        cnt_tok <- if (nrow(cell) > 0) cell$text[vapply(cell$text, is_num_count, logical(1))] else character(0)
        rate_tok <- if (nrow(cell) > 0) cell$text[vapply(cell$text, is_num_rate, logical(1))] else character(0)
        cnt <- if (length(cnt_tok) >= 1) parse_hokenjo_number(cnt_tok[1]) else NA_real_
        rate <- if (length(rate_tok) >= 1) parse_hokenjo_number(rate_tok[1]) else NA_real_
        out[[length(out) + 1]] <- data.frame(
          pref = "佐賀県", week_label = week_label, hokenjo = hokenjo[k],
          disease = dname, count = cnt, rate = rate, stringsAsFactors = FALSE
        )
      }
    }
    # 数値行を処理した後はバッファをクリアする（数値行の後に続く注記が
    # 次の疾患名に誤って混入するのを防ぐ）。先読みした行はそのまま
    # 次の疾患名バッファの先頭候補として温存せず、通常どおり次ループで
    # 名前のみ行として再処理させる（マッチ済みなら実害はない）
    pending_name <- ""
    i <- i + 1
  }
  do.call(rbind, out)
}
