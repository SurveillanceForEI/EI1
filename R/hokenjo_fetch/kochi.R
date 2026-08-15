# 高知県「疾病別・地域別報告数」PDF（週報 p.5）
# https://www.pref.kochi.lg.jp/doc/2026011400132/file_contents/file_2026864114034_1.pdf 型
# （URLは公開ページの添付ファイルIDが週ごとに変わるため、呼び出し側で最新PDFのURLを解決すること）
#
# レイアウト: p.5に「報告数」表（上段）と「定点当たり報告数」表（下段）が
# 縦に並ぶ。両表とも6保健所（安芸/中央東/高知市/中央西/須崎/幡多）×
# 20疾患（ARI, インフルエンザ, 新型コロナ, 咽頭結膜熱,
# Ａ群溶血性レンサ球菌咽頭炎, 感染性胃腸炎, 水痘, 手足口病, 伝染性紅斑,
# 突発性発疹, ヘルパンギーナ, 流行性耳下腺炎, RSウイルス感染症,
# 急性出血性結膜炎, 流行性角結膜炎, 細菌性髄膜炎, 無菌性髄膜炎,
# マイコプラズマ肺炎, クラミジア肺炎, 感染性胃腸炎(ロタウイルスに限る))
# の固定順で並ぶため、疾患名は座標抽出ではなく固定リストを用いる
# （多言語文字の複数行折返しでテキスト順が乱れ座標抽出が不安定なため）。
# 数値行は「保健所列のx範囲に数値トークンを含む行」として検出する。

.KOCHI_HOKENJO <- c("安芸", "中央東", "高知市", "中央西", "須崎", "幡多")
.KOCHI_X_BINS <- c(108, 143.5, 169.5, 196.5, 224.5, 252.5, 280)  # 6区間の境界

.KOCHI_DISEASES <- c(
  "急性呼吸器感染症(ARI)", "インフルエンザ", "新型コロナウイルス感染症", "咽頭結膜熱",
  "Ａ群溶血性レンサ球菌咽頭炎", "感染性胃腸炎", "水痘", "手足口病", "伝染性紅斑",
  "突発性発疹", "ヘルパンギーナ", "流行性耳下腺炎", "RSウイルス感染症",
  "急性出血性結膜炎", "流行性角結膜炎", "細菌性髄膜炎", "無菌性髄膜炎",
  "マイコプラズマ肺炎", "クラミジア肺炎(オウム病は除く)",
  "感染性胃腸炎(ロタウイルスに限る)"
)

# 疾患名は複数行に分割された全角文字トークンとして現れ、かつ数値が
# 全て空欄（報告数0）の行はテキスト順が乱れて座標抽出できないため、
# 各行に含まれる特徴的なキーワード（トークン部分一致）で疾患を同定する。
# 順序: (疾患名, 必須キーワード, 除外キーワード)
.KOCHI_KEYWORDS <- list(
  list("急性呼吸器感染症(ARI)", "ARI", NULL),
  list("インフルエンザ", "フル", NULL),
  list("新型コロナウイルス感染症", "コロナ", NULL),
  list("咽頭結膜熱", "熱", NULL),
  list("Ａ群溶血性レンサ球菌咽頭炎", "レンサ", NULL),
  list("感染性胃腸炎", "胃", "限"),
  list("水痘", "痘", NULL),
  list("手足口病", "足", NULL),
  list("伝染性紅斑", "紅", NULL),
  list("突発性発疹", "疹", NULL),
  list("ヘルパンギーナ", "ギー", NULL),
  list("流行性耳下腺炎", "腺", NULL),
  list("RSウイルス感染症", "RS", NULL),
  list("急性出血性結膜炎", "出血", NULL),
  list("流行性角結膜炎", "角", NULL),
  list("細菌性髄膜炎", "細菌", NULL),
  list("無菌性髄膜炎", "無菌", NULL),
  list("マイコプラズマ肺炎", "マイコ", NULL),
  list("クラミジア肺炎(オウム病は除く)", "クラミジア", NULL),
  list("感染性胃腸炎(ロタウイルスに限る)", "限", NULL)
)

# 1つの表ブロックからhokenjo×disease×valueのdata.frameを作る
.kochi_extract_block <- function(rows, bins = .KOCHI_X_BINS, hokenjo = .KOCHI_HOKENJO, keywords = .KOCHI_KEYWORDS) {
  is_num <- function(s) grepl("^[0-9,]+(\\.[0-9]+)?$", s)
  matched <- rep(FALSE, length(keywords))
  out <- list()
  for (r in rows) {
    txt <- r$text
    # 疾患名が縦書き風に1文字ずつ別トークンとして現れる行があるため、
    # 行内の全トークンを連結した文字列に対してキーワード照合を行う
    # （個々のトークン単位の照合では複数文字のキーワードが検出できない）。
    txt_joined <- paste(txt, collapse = "")
    for (di in seq_along(keywords)) {
      if (matched[di]) next
      req <- keywords[[di]][[2]]
      excl <- keywords[[di]][[3]]
      hit <- grepl(req, txt_joined, fixed = TRUE)
      if (hit && !is.null(excl)) hit <- hit && !grepl(excl, txt_joined, fixed = TRUE)
      if (hit) {
        matched[di] <- TRUE
        dname <- keywords[[di]][[1]]
        for (k in seq_along(hokenjo)) {
          sub <- r[r$x >= bins[k] & r$x < bins[k + 1] & sapply(r$text, is_num), ]
          val <- if (nrow(sub) >= 1) parse_hokenjo_number(sub$text[1]) else NA_real_
          out[[length(out) + 1]] <- data.frame(hokenjo = hokenjo[k], disease = dname, value = val, stringsAsFactors = FALSE)
        }
        break
      }
    }
  }
  do.call(rbind, out)
}

fetch_kochi <- function(pdf_url = NULL) {
  if (is.null(pdf_url)) {
    src <- hokenjo_source_for_pref("高知県")
    pdf_url <- src$sample_url
  }
  if (!exists("pdf_words")) stop("pdf_table_utils.R を先に source してください")

  words <- pdf_words(pdf_url, page = 5)
  rows <- group_words_into_rows(words, y_tol = 3)

  week_line <- Filter(function(r) grepl("20[0-9]{2}年第[0-9]+週", row_text(r)), rows)
  week_label <- if (length(week_line) > 0) {
    regmatches(row_text(week_line[[1]]), regexpr("20[0-9]{2}年第[0-9]+週", row_text(week_line[[1]])))
  } else NA_character_

  # 「定点当たり人数」の行を境に上段(報告数)/下段(定点当たり)を分割
  split_idx <- which(sapply(rows, function(r) grepl("定点当たり人数|定点当たり", row_text(r)) && grepl("第[0-9]+週|感染症情報", row_text(r))))
  boundary <- if (length(split_idx) > 0) min(split_idx) else floor(length(rows) / 2)

  rows_count <- rows[1:boundary]
  rows_rate <- rows[(boundary + 1):length(rows)]

  df_count <- .kochi_extract_block(rows_count)
  df_rate <- .kochi_extract_block(rows_rate)

  merged <- merge(df_count, df_rate, by = c("hokenjo", "disease"), suffixes = c("_count", "_rate"), all = TRUE)

  data.frame(
    pref = "高知県",
    week_label = week_label,
    hokenjo = merged$hokenjo,
    disease = merged$disease,
    count = merged$value_count,
    rate = merged$value_rate,
    stringsAsFactors = FALSE
  )
}
