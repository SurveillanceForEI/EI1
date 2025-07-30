library(taskscheduleR)


taskscheduler_create(
  taskname = "google_news_daily_task",
  rscript = "C:/Users/kobayashi/Documents/R/EI1/test1.R",
  schedule = "DAILY",
  starttime = "16:00",
  startdate = format(Sys.Date(), "%m/%d/%Y"),  # 形式注意
  Rexe = file.path(R.home("bin"), "Rscript.exe")
)
##解除##
taskscheduler_delete(taskname = "google_news_daily_task")
