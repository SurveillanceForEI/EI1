setwd(if (basename(getwd()) == "scripts") ".." else ".")
h <- readRDS("data/hokenjo_history.rds")

# 18文字(全角/半角正規化後)を超える疾患名は、189件を全件目視確認した結果
# 大半がPDF本文の説明文の断片・表ヘッダー・複数疾患の結合ミスであり、
# 正規の疾患名は以下の9件のみだった
LEGIT_LONG_DISEASE_NAMES <- c(
  "感染性胃腸炎（ロタウイルスによるもの）",
  "インフルエンザ菌感染症(髄膜炎に限る。)",
  "新型コロナウイルス感染症（COVID-19）",
  "新型コロナウイルス感染症(COVID-19)",
  "感染性胃腸炎(ロタウイルスによるものに限る。)",
  "インフルエンザ(高病原性鳥インフルエンザを除く)",
  "感染性胃腸炎（病原体がロタウイルスであるものに限る。）",
  "感染性胃腸炎(病原体がロタウイルスであるものに限る。)",
  "インフルエンザ（鳥インフルエンザ及び新型インフルエンザ等感染症を除く）"
)

norm_len <- nchar(chartr("０１２３４５６７８９（）", "0123456789()", h$disease))
is_junk <- norm_len > 18 & !(h$disease %in% LEGIT_LONG_DISEASE_NAMES)

cat("junk行数:", sum(is_junk), " / total:", nrow(h), "\n")
cat("\npref別 削除内訳:\n")
print(sort(table(h$pref[is_junk]), decreasing = TRUE))

h_clean <- h[!is_junk, ]
saveRDS(h_clean, "data/hokenjo_history.rds")
cat("\n保存完了。削除後 total:", nrow(h_clean), "\n")
