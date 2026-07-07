# 必要パッケージのインストール
pkgs <- c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "tidyr",
  "ggplot2",
  "plotly",
  "leaflet",
  "sf",
  "DT",
  "lubridate",
  "scales",
  "RColorBrewer",
  "rnaturalearth",
  "rnaturalearthdata"
)

missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  cat("インストール中:", paste(missing, collapse = ", "), "\n")
  install.packages(missing)
} else {
  cat("すべてのパッケージがインストール済みです\n")
}

# rnaturalearthdata は大きいので別途
if (!"rnaturalearthdata" %in% installed.packages()[, "Package"]) {
  install.packages("rnaturalearthdata", repos = "https://ropensci.r-universe.dev")
}
