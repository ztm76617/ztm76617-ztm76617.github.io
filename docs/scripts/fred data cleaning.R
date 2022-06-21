#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots)   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven)
library(lubridate); library(scales); library(IndexNumR); library(wid); library(datawrangling)
library(stats); library(smooth); library(plm); library(WDI); library(lmtest); library(sandwich)
library(interactions); library(rmarkdown); library(captioner); library(fredr)
#--------------------------------------------------------------------------------------
# Set API Key
#--------------------------------------------------------------------------------------
fredr_set_key("4ea646af5b37ba1c45a0890c7a04f2dc")
#--------------------------------------------------------------------------------------
# Retrieve Series Observations
#--------------------------------------------------------------------------------------
fredr(series_id = c("DCPBSV"),
      observation_start = as.Date("1947-01-01"),
      observation_end = as.Date("2022-01-01"),
      frequency = "q",
      units = "pch",
      aggregation_method = "avg")
#--------------------------------------------------------------------------------------
# fedr ggplot
#--------------------------------------------------------------------------------------
map_dfr(c("SMU11000000500000003",
          "DCUCSFRCONDOSMSAMID",
          "LBSSA11"),
        fredr,
        frequency = "m",
        units = "pch") %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "SMU11000000500000003" ~ "Avg. Hourly Earnings (All Private Employees)",
                               series_id == "DCUCSFRCONDOSMSAMID" ~ "Zillow Home Value Index)",
                               series_id == "LBSSA11" ~ "Labor Force Participation Rate")) %>%
  filter(date >= "2019-01-01") %>%
  ggplot(mapping = aes(x = date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_bw() +
  labs(x = "Date",
       y = "Percent Change (%)",
       title = "Washington, DC Area",
       subtitle = "Monthly Economic Data (2019 - 2022)") +
  theme(text = element_text(face = 'bold'),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  facet_grid(vars(series_id))
    
