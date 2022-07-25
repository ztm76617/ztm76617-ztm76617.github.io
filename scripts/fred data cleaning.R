#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(gplots);   
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven);
library(lubridate); library(scales); library(IndexNumR); library(wid); library(datawrangling);
library(stats); library(smooth); library(plm); library(WDI); library(lmtest); library(sandwich);
library(interactions); library(rmarkdown); library(captioner); library(fredr); library(countrycode)
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
         series_id = case_when(series_id == "SMU11000000500000003" ~ "Average Hourly Earnings",
                               series_id == "DCUCSFRCONDOSMSAMID" ~ "Zillow Home Value Index",
                               series_id == "LBSSA11" ~ "Labor Force Participation Rate")) %>%
  filter(date >= "2019-01-01") %>%
  ggplot(mapping = aes(x = date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_bw() +
  labs(x = "Month-Year",
       y = "Percent Change (%)",
       title = "Washington, DC Area",
       subtitle = "Monthly Economic Data (2019 - 2022)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8),
        text = element_text(face = 'bold'),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "4 month") +
  facet_wrap(vars(series_id))
#--------------------------------------------------------------------------------------
# fedr ggplot
#--------------------------------------------------------------------------------------
map_dfr(c("DCBPPRIVSA"),
        fredr,
        frequency = "m",
        units = "pch") %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "DCBPPRIVSA" ~ "New Housing Permits")) %>%
  filter(date >= "2019-01-01") %>%
  ggplot(mapping = aes(x = date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_bw() +
  labs(x = "Month-Year",
       y = "Monthly Change",
       title = "Washington, DC Area",
       subtitle = "Monthly Economic Data (2019 - 2022)") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8),
        text = element_text(face = 'bold'),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "4 month") +
  scale_y_continuous(limits = c(-900, 2850),
                     labels = label_percent(big.mark = ",")) +
  facet_wrap(vars(series_id))
#--------------------------------------------------------------------------------------
map_dfr(c("SMS11000009091000001",
          "DCLEIH",
          "SMU11000006056130001SA"),
        fredr,
        frequency = "m",
        units = "lin") %>%
  mutate(date = ymd(date),
         series_id = case_when(series_id == "SMS11000009091000001" ~ "Federal Governmentt",
                               series_id == "DCLEIH" ~ "Leisure/Hospitality",
                               series_id == "SMU11000006056130001SA" ~ "Business/Professional Services")) %>%
  filter(date == "2022-05-01") %>%
  view()
  
  
  ggplot(aes(y=value, x=series_id)) + 
  geom_bar(position="dodge", stat="identity", width = 0.4) +
  theme_bw() +
  labs(x = "",
       y = "Number of Employees (Thousands)",
       title = "Washington, DC Area (May 2022)") +
  theme(text = element_text(face = 'bold'))


    
