library(taskscheduleR)

taskscheduler_create(
  taskname = "google_news_daily_task",
  rscript = "C:/Users/yusuke-k/Documents/R/EI1/test1.R",
  schedule = "DAILY",
  starttime = "00:00",
  startdate = format(Sys.Date(), "%m/%d/%Y"),  # "07/30/2025" の形式
  Rexe = file.path(R.home("bin"), "Rscript.exe"),
  logfile = "C:/Users/yusuke-k/Documents/R/EI1/test1.log"
)