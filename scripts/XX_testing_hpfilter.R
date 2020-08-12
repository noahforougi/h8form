rm(list = ls())

library(mFilter)
library(lubridate)
library(forecast)
library(tidyverse)
library(here)
library(data.table)
library(plotly)


h8form_h <- fread(file = here("proc", "h8form_h.csv"))


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

as.vector(filtered$cycle)




ready <- tibble(
  dat, 
  trend = filtered$trend, 
  cycle = filtered$cycle
)

class(ready)



ready <- data.frame(cbind(dat,
                 trend = filtered$trend, 
                 cycle =filtered$cycle))

ggplotly(ggplot(data = ready, aes(x = date)) + 
  geom_line(aes(y = value, color = "red")) + 
  geom_line(aes(y = trend.value, color = "blue")))

data.frame(filtered[c("trend", "cycle")])
