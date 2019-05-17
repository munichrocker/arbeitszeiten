# This Script checks, if there is already a `leaving` value for the day and if it did already send an email.
# If not, it runs Arbeitszeit.R
exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}

drive_url <- "https://docs.google.com/spreadsheets/d/1ANRzIfTUVGjmXzspuY9fdTILJcYoDUOY7MFkIpSJVN4/export?format=csv"

current_date <- as.Date(Sys.time())
log_con <- file("/home/benedict/skripte/2019_Arbeitszeit/mail.log")

if (identical(tail(readLines(log_con), n = 1), as.character(as.numeric(current_date)))) {
  print("IDENTICAL")
  exit()
} else {
  download.file(drive_url, "arbeitszeit.csv")
  
  source("/home/benedict/skripte/2019_Arbeitszeit/Arbeitszeit.R")
  
  cat(current_date, file = "/home/benedict/skripte/2019_Arbeitszeitmail.log", append = TRUE, sep="\n")
}