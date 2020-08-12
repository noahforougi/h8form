rm(list = ls())

library(mFilter)
library(lubridate)
library(forecast)

h8form_c <- read_csv(file = here("proc", "h8form_c.csv"))

h8form_h <- read_csv(file = here("data", "data.csv"))


dat <- h8form_h %>%
  filter(id == "all commercial banks" &
           variable == "Bank credit" & 
           adjusted == "seasonally adjusted" &
           growth == "level") %>%
  mutate(date = myd(paste(date, "01"))) %>%
  select(date, value)


obj <- ts(dat[,2], start = ymd("19800101"), end = ymd("2019sep01"), frequency = 12)

hpfilter(obj, freq = "144")

