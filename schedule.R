library(taskscheduleR)

taskscheduler_create(
  taskname   = "google_news_daily_task",
  rscript    = "C:/Users/kobayashi/Documents/R/EI1/run_test1.bat",  # ← RスクリプトでなくてもOK
  schedule   = "DAILY",
  starttime  = "17:55",
  startdate  = format(Sys.Date(), "%m/%d/%Y"),
  Rexe       = "cmd.exe"  # Rexe を cmd.exe にすると `.bat` が動く（非公式技）
)
##解除##
taskscheduler_delete(taskname = "google_news_daily_task")
