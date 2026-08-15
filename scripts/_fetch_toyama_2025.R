source("R/hokenjo_fetch/hokenjo_fetch_schema.R")

url <- "https://www.pref.toyama.jp/documents/32640/teiten_hc_202552w.zip"
tmp <- tempfile(fileext = ".zip")
download.file(url, tmp, mode = "wb", quiet = TRUE)
dir <- tempfile()
dir.create(dir)
files <- unzip(tmp, exdir = dir)

region_map <- c(chuubu = "中部", niikawa = "新川", takaoka = "高岡", tonami = "砺波", toyamashi = "富山市")

parse_one <- function(path, hokenjo_name) {
  d <- read.csv(path, fileEncoding = "shift-jis", check.names = FALSE, header = FALSE, stringsAsFactors = FALSE)
  disease_row <- unlist(d[2, ])
  disease_cols <- which(nchar(trimws(disease_row)) > 0 & seq_along(disease_row) > 2)
  diseases <- trimws(disease_row[disease_cols])

  data_rows <- which(!is.na(suppressWarnings(as.integer(d[[2]]))) & trimws(as.character(d[[1]])) == "2025")
  out <- list()
  for (ri in data_rows) {
    week <- as.integer(d[ri, 2])
    for (ci in seq_along(disease_cols)) {
      col <- disease_cols[ci]
      out[[length(out) + 1]] <- data.frame(
        pref = "富山県", week_label = sprintf("2025年第%d週", week), week_num = week,
        hokenjo = hokenjo_name, disease = diseases[ci],
        count = parse_hokenjo_number(d[ri, col]),
        rate  = parse_hokenjo_number(d[ri, col + 2]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

all_rows <- list()
for (f in files) {
  key <- names(region_map)[sapply(names(region_map), function(k) grepl(k, basename(f)))]
  if (length(key) == 0) next
  hj <- region_map[[key[1]]]
  res <- parse_one(f, hj)
  all_rows[[length(all_rows) + 1]] <- res
  cat("[OK]", hj, ":", nrow(res), "rows\n")
}

df <- do.call(rbind, all_rows)
df$fetched_at <- as.character(Sys.time())
cat("total:", nrow(df), "weeks:", paste(range(df$week_num), collapse="-"), "\n")

h <- readRDS("data/hokenjo_history.rds")
common_cols <- intersect(names(h), names(df))
extract_year_num <- function(week_label) {
  if (is.na(week_label)) return(NA_integer_)
  wl <- chartr("０１２３４５６７８９", "0123456789", week_label)
  m <- regmatches(wl, regexec("([0-9]{4})年", wl))[[1]]
  if (length(m) >= 2) return(as.integer(m[2]))
  NA_integer_
}
h_yr <- vapply(h$week_label, function(x) as.integer(extract_year_num(x)), integer(1))
is_old_toyama_2025 <- h$pref == "富山県" & h_yr == 2025 & !is.na(h$week_num)
cat("removing", sum(is_old_toyama_2025), "old rows\n")
h <- h[!is_old_toyama_2025, ]
combined <- rbind(h[, common_cols], df[, common_cols])
saveRDS(combined, "data/hokenjo_history.rds")
cat("saved. total:", nrow(combined), "\n")
