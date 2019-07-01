# This script downloads the current Arbeitszeiten-Sheet from Google Drive analyzes it and sends an email
library("stringr")
library("lubridate")
library("dplyr")
library("ggplot2")
library("mailR")

# Read in current Data

d <- read.csv("/home/benedict/skripte/2019_Arbeitszeit/arbeitszeit.csv", col.names = c("status", "date", "location"))

# change data format in date, time, status
Sys.setlocale("LC_TIME", "C")

d %>% 
  mutate(date = as.POSIXct(date, format = "%B %d, %Y at %I:%M%p"),
         week_number = isoweek(date),
        weekday_number = factor(weekdays(date, FALSE), levels = c("Monday", "Tuesday", 
                                                                  "Wednesday", "Thursday", 
                                                                  "Friday", "Saturday", "Sunday"))) -> d

# Analyze shift length: filter out multiple entries on one day to first and last entry

d %>% 
  arrange(date) %>% 
  group_by(date(date)) %>% 
  slice(c(1, n())) %>% 
  ungroup() %>% 
  distinct() -> d_distinct

## calculate working times
d_distinct %>% 
  arrange(date) %>% 
  group_by(date(date)) %>% 
  tidyr::spread(status, date) %>% 
  mutate(working_hours = as.duration(interval(entered, exited))) %>% 
  ungroup() %>% 
  group_by(week_number) %>% 
  summarize(no_workingdays = n(),
            sum_workinghours = sum(as.numeric(working_hours), na.rm = TRUE) / 3600,
            workingtime_todo = no_workingdays * 7.3,
            overtime = round(sum_workinghours - workingtime_todo, 2)) %>% 
  arrange(week_number) -> d_results

# Total Sum of Overtime
d_distinct %>% 
  arrange(date) %>% 
  group_by(date(date)) %>% 
  tidyr::spread(status, date) %>% 
  mutate(working_hours = as.duration(interval(entered, exited))) %>% 
  ungroup() %>% 
  summarise(sum_workinghours = sum(as.numeric(working_hours), na.rm = TRUE) / 3600,
         sum_workingdays = n()) -> total_overtime

overtime_total <- total_overtime$sum_workinghours - total_overtime$sum_workingdays * 7.3
total_number_workingdays <- total_overtime$sum_workingdays

# Building text blocks from result
d_results %>% 
  filter(week_number == max(d_results$week_number)) %>% 
  select(overtime) %>% 
  .[[1]] -> this_week_overtime

d_results %>% 
  filter(week_number == max(d_results$week_number)) %>% 
  select(no_workingdays, sum_workinghours, workingtime_todo) -> d_text_information

over_under_time <- if_else(this_week_overtime >= 0, "zu viel", "zu wenig")

text_result <- paste0("Du hast in der vergangenen Arbeitswoche bei <b>",
       d_text_information$no_workingdays, " Arbeitstagen (", round(d_text_information$workingtime_todo, 2), 
       " Sollstunden)</b> insgesamt <b>", round(abs(this_week_overtime), 2), " Stunden ", over_under_time, "</b> gearbeitet.<br>",
       "Die Gesamtüberstundenzahl seit Januar 2019 beträgt: ", round(overtime_total, 2), " Stunden. <br>Bei einer Mittagspause von 45 Minuten pro Tag wären das immer noch <b>",
       round(overtime_total - total_overtime$sum_workingdays * 0.75, 2), " Stunden</b>.")

# Vizualizing results
d_results %>% 
  do(tail(., n=4)) %>% 
  ggplot(., aes(week_number, overtime)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Überstunden der vergangenen Wochen", x = "Kalenderwoche", y = "Überstunden") +
  theme_minimal()

ggsave("last_weeks.png")

# ideas: plot overtime per day for the last week
d_distinct %>% 
  arrange(date) %>% 
  group_by(date(date)) %>% 
  tidyr::spread(status, date) %>% 
  mutate(working_hours = as.duration(interval(entered, exited))) %>% 
  ungroup() %>% 
  filter(week_number == max(week_number)) %>% 
  mutate(working_hours = as.numeric(working_hours) / 3600,
         workingtime_todo = 7.3,
         overtime = round(working_hours - workingtime_todo, 2)) %>% 
  rename(date = `date(date)`) %>% 
  arrange(date) %>% 
  ggplot(., aes(weekday_number, working_hours)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_hline(mapping = aes(yintercept = workingtime_todo)) +
  geom_text(aes(label = overtime), nudge_y = 0.5) +
  labs(x = "Wochentag", y = "Arbeitsstunden") +
  theme_minimal()

ggsave("this_week.png")

# Create Mail from here

body_text <- paste0('<html>','<p>Hallo Benedict, <br>hier kommt dein freitägliches Arbeitszeitupdate.</p>', 
                    '<p>', text_result, '</p>', '<h3>So verteilen sich die Arbeitsstunden in dieser Woche:</h3><br>',
                    '<img src="', here::here(), '/this_week.png", style = "width: 100%; height: auto"><br>', 
                    '<h3>So viele Überstunden gab es in den vergangenen Wochen</h3><br>',
                    '<img src="', here::here(), '/last_weeks.png", style = "width: 100%; height: auto"><br>',
                    '</html>')

# needed: App-COde from Google, as Two Factor Authentication is enabled.
# load credentials
source("creds.R")

send.mail(from = mailadress,
          to = mailadress,
          subject = "Arbeitszeiten",
          body = body_text,
          html = TRUE,
          inline = TRUE,
          smtp = list(host.name = "smtp.gmail.com",
                      port = 465,
                      user.name = username,
                      passwd = password,
                      ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
