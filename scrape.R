# Scrape the most current release of H.8 form
library(tidyverse)
library(rvest)

h8 <- "https://www.federalreserve.gov/releases/h8/current/"
page <- read_html(h8)

pages <- list()

pages[[1]] <- page %>%
  html_nodes(xpath = "/html/body/div[3]/div[4]/table") %>%
  html_table(fill = T)

for (i in 2:21) {
  pages[[i]] <- page %>%
    html_nodes(xpath = paste0("/html/body/div[3]/div[", i + 4, "]/table")) %>%
    html_table(fill = T)
  pages[[i]] <- as.data.frame(pages[[i]])
}

names <- names(pages[[2]])
str_remove(names[3:10], "..\\.")

# Each page is either seasonally adjusted or not-seasonally adjusted, and represents a subset of all commercial banks. We need to clear that up here.
SA_pages <- pages[c(2, 3, 6, 7, 10, 11, 14, 15, 18, 19)]
NSA_pages <- pages[c(4, 5, 8, 9, 12, 13, 16, 17, 20, 21)]

names(SA_pages) <- c(
  "all", "all",
  "dom", "dom",
  "largedom", "largedom",
  "smalldom", "smalldom",
  "foreign", "foreign"
)
dat <- bind_rows(SA_pages, .id = "id")

# Clean data frame.
dat <- dat %>%
  filter(Account != "Account")
dat$Account[dat$Account %in% paste(seq(1:50))] <- NA
dat <- fill(data = dat, Account)
dat$Account <- str_replace(dat$Account, pattern = " \\(continued\\)", "")

dat <- dat[!(dat$Account.1 %in% c("Assets", "Assets (continued)", "Liabilities", "Memoranda")), ]
dat[] <- sapply(dat, gsub, pattern = "\\,", replacement = "")
dat[, 4:15] <- sapply(dat[4:15], as.numeric)

names(dat) <- gsub("X|\\.", "", names(dat))
names(dat)[4:11] <- paste0(names(dat)[4:11], "01")
# names(dat)[4:11] <- zoo::as.yearmon(names(dat)[4:11], "%Y%b")

names(dat)[12:15] <- paste(seq(from = as.Date(pages[[2]]$Week.ending.3[1], "%b %d"), by = "week", length.out = 4))

# Pivot to long format
dat %<>% 
  pivot_longer(cols = 4:15, names_to = "date") %>%
  mutate(date = ymd(date)) %>%
  rename("Asset or Liability" = Account, 
         "Type" = Account1, 
         "Bank Type" = id)
