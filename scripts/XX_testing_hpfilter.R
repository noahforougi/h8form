rm(list = ls())

library(mFilter)
library(lubridate)
library(forecast)
library(tidyverse)
library(here)
library(data.table)
library(plotly)
library(tseries)


h8form_h <- fread(file = here("proc", "h8form_h.csv"))

h8form_c <- fread(file = here("proc", "h8form_c.csv"))

# Time Series Decomposition

dat <- h8form_h %>%
  filter(id == "all commercial banks" &
           variable == "Bank credit" & 
           adjusted == "seasonally adjusted" &
           growth == "level") %>%
  mutate(date = myd(paste(date, "01")))

filtered <- dat %>% 
  select(value) %>%
  ts(., frequency = 144) %>% 
  hpfilter(., freq = 12)

ready <- tibble(
  dat, 
  trend = filtered$trend, 
  cycle = filtered$cycle
)






ready %>% 
  mutate(trend = as.numeric(as.character(trend))) %>%
  select(value, trend, date) %>%
  pivot_longer(cols = c("value", "trend"), names_to = "Component") %>% 
  ggplot(aes(x = date, y = value, color = Component)) + 
  geom_line() + 
  ggthemes::theme_clean() 


# Aggregate plotting
h8form_c <- fread(file = here("proc", "h8form_c.csv"))

h8form_c %>%
  filter(Type == "Total assets") %>%
  ggplot(aes(x = date, y = value, fill = `Bank Type`, group = `Bank Type`)) + geom_area()


