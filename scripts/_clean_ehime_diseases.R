setwd(if (basename(getwd()) == "scripts") ".." else ".")
h <- readRDS("data/hokenjo_history.rds")
is_ehime_junk <- h$pref == "愛媛県" & grepl("入院|迅速検査|︵|︶", h$disease)
cat("削除対象行数:", sum(is_ehime_junk), "\n")
print(table(h$disease[is_ehime_junk]))
h_clean <- h[!is_ehime_junk, ]
saveRDS(h_clean, "data/hokenjo_history.rds")
cat("\n保存完了。削除後total:", nrow(h_clean), "(削除前:", nrow(h), ")\n")
