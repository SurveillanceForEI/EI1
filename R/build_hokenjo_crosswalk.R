# ============================================================
# 保健所管轄区域クロスウォーク生成スクリプト
# ------------------------------------------------------------
# 厚生労働省公式データ「都道府県別市区町村符号及び保健所符号一覧」
# （e-Gov データポータル、2026年5月更新）を解析し、
# 市区町村コード(JIS5桁) → 保健所名 の対応表を全都道府県分生成する。
#
# 出典: 厚生労働省 人口動態・保健社会統計室
#       https://data.e-gov.go.jp/data/dataset/mhlw_20170316_0002
#
# シート構成: 都道府県ごとに1シート。各シート内に「市区町村符号」＋
# 「保健所名」の見出しペアが複数ブロック（政令市・郡ごとに列がずれて
# 配置）存在するため、シート全体から見出しペアを自動検出して読む。
#
# 実行すると data/geo/hokenjo_muni_crosswalk.csv を生成する。
#
# 注意（2026-08-24 ユーザー指摘・手動修正済み）: 厚労省マスターは支所を
# 本所と同じ「保健所名」で丸めてしまうため、和歌山県 新宮保健所 串本支所
# （串本町=30428、古座川町=30427が管轄。本所は新宮市=30207、
# 那智勝浦町=30421、太地町=30422、北山村=30424）のように、週報が支所単位
# で別々に数値を出す都道府県では実態と合わなくなる。このスクリプトを
# 再実行してCSVを再生成する場合は、上記2市町村の hokenjo 列を
# 「新宮」→「串本」に必ず手動で戻すこと（自動生成では復元されない）。
# ============================================================

library(readxl)

XLSX_PATH <- "data/geo/hokenjo_code_master.xlsx"

# 全角数字を半角に変換
.zen2han <- function(x) {
  chartr("０１２３４５６７８９", "0123456789", x)
}

# 1シート分を解析し、data.frame(pref, muni_code, hokenjo) を返す
parse_hokenjo_sheet <- function(sheet_name) {
  d <- suppressMessages(read_excel(XLSX_PATH, sheet = sheet_name, col_names = FALSE))
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  pref <- gsub("^[0-9０-９]+[　\\s]*", "", trimws(as.character(d[1, 1])))

  nr <- nrow(d); nc <- ncol(d)
  # 見出し行（「市区町村符号」を含む行）を探す
  header_rows <- which(apply(d, 1, function(r) any(grepl("市区町村符号", r), na.rm = TRUE)))

  out <- list()
  for (hr in header_rows) {
    code_header_cols <- which(apply(d[hr, , drop = FALSE], 2, function(v) grepl("市区町村符号", v)))
    for (cc_header in code_header_cols) {
      probe0 <- (hr + 1):min(hr + 30, nr)
      n_cc_header <- sum(grepl("^[0-9０-９]{5}$", trimws(as.character(d[probe0, cc_header]))))
      cc <- cc_header
      if (cc_header + 1 <= nc) {
        n_cc_next <- sum(grepl("^[0-9０-９]{5}$", trimws(as.character(d[probe0, cc_header + 1]))))
        if (n_cc_next > n_cc_header) cc <- cc_header + 1
      }
      # 同じ見出し行の右側〜6列以内に「保健所名」列があるはず
      search_range <- cc_header:min(cc_header + 6, nc)
      hj_header_col <- NA
      for (j in search_range) {
        if (isTRUE(grepl("保健所名", d[hr, j]))) { hj_header_col <- j; break }
      }
      if (is.na(hj_header_col)) next

      # セル結合の影響で見出しと実データが1列ずれる場合があるため、
      # 見出し列と右隣の列のうち、データが入っている方を採用する
      probe <- (hr + 1):min(hr + 30, nr)
      n_at_header <- sum(!is.na(d[probe, hj_header_col]) & nchar(trimws(as.character(d[probe, hj_header_col]))) > 0)
      hj_col <- hj_header_col
      if (hj_header_col + 1 <= nc) {
        n_at_next <- sum(!is.na(d[probe, hj_header_col + 1]) & nchar(trimws(as.character(d[probe, hj_header_col + 1]))) > 0)
        if (n_at_next > n_at_header) hj_col <- hj_header_col + 1
      }

      codes <- .zen2han(as.character(d[(hr + 1):nr, cc]))
      hjs   <- trimws(gsub("[　\\s]+$", "", as.character(d[(hr + 1):nr, hj_col])))
      keep  <- !is.na(codes) & grepl("^[0-9]{5}$", codes) & !is.na(hjs) & nchar(hjs) > 0
      if (any(keep)) {
        out[[length(out) + 1]] <- data.frame(
          pref = pref, muni_code = codes[keep], hokenjo = hjs[keep],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(out) == 0) return(data.frame(pref = character(0), muni_code = character(0), hokenjo = character(0)))
  do.call(rbind, out)
}

build_full_crosswalk <- function() {
  sheets <- excel_sheets(XLSX_PATH)
  # 北海道は「振興局別」「郡別」の2シートがあるが内容はほぼ同一のため振興局別のみ使用
  sheets <- sheets[!grepl("郡別", sheets)]

  all_rows <- lapply(sheets, function(s) {
    tryCatch(parse_hokenjo_sheet(s), error = function(e) {
      warning("シート解析失敗: ", s, " - ", conditionMessage(e))
      data.frame(pref = character(0), muni_code = character(0), hokenjo = character(0))
    })
  })
  cw <- do.call(rbind, all_rows)
  cw <- cw[!duplicated(cw[, c("muni_code", "hokenjo")]), ]
  cw <- cw[order(cw$muni_code), ]
  rownames(cw) <- NULL
  cw
}

if (sys.nframe() == 0 || identical(environment(), globalenv())) {
  # 直接実行された場合のみCSV出力する
}
