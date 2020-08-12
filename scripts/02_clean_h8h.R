rm(list = ls())

library(tidyverse)
library(zoo)

feddata <- read_csv("Downloads/feddata.csv", col_names = T)[-c(1:5),]

feddata <- feddata %>%
  pivot_longer(names(feddata)[2:length(feddata)], names_to = "variable", values_to = "value") %>%
  separate( col = variable, into = c("variable", "id", "adjusted", "growth"), sep = ",") %>%
  rename(date = `Series Description`) %>% 
  mutate(date = as.yearmon(date, "%Y-%m"), 
         variable = as.factor(variable), 
         id = as.factor(id), 
         adjusted = as.factor(adjusted), 
         growth = as.factor(growth), 
         value = as.numeric(value)) %>%
  mutate_if(is.factor, str_trim) %>%
  mutate_each(funs=funs(ifelse(is.na(.),"level",.)),matches("growth"))

write.csv(feddata, "~/Desktop/foreignbank/data.csv", row.names = F)


feddata %>%
  filter(
    variable == "Total liabilities", 
    adjusted == "seasonally adjusted", 
    growth == "annual growth rate (break adjusted)"
  ) %>% 
  ggplotly(aes(x = date, y = value, group = id)) + geom_line(aes(color = id))






