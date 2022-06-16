#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggthemes); library(ggthemr); library(plm)
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(IndexNumR); library(wid); library(scales);library(ISOcodes); library(labelled); library(wesanderson); library(captioner)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(countrycode); library(RColorBrewer)
library(WDI); library(lmtest); library(sandwich); library(interactions); library(ggpubr); library(gplots); library(purrr)
#--------------------------------------------------------------------------------------
names(wes_palettes)

scale_fill_manual(values = wes_palette("Royal1"))
scale_colour_brewer(palette = "Set1")

# fav brewer pallettes
  #1 "Set1"
  #2 "Set2"
  #3 "Set3"
  #4 "Dark2"
  #5 "Accent"
  #6 "Spectral"
#--------------------------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", 'DEU')) %>%
  group_by(country_code_ISO3, year) %>%
  mutate(pwt_human_capital_index = mean(pwt_human_capital_index),
         WID_pre_tax_income_share_top_10pct = mean(WID_pre_tax_income_share_top_10pct)) %>%
  distinct(country_code_ISO3, year, .keep_all = TRUE) %>%
  ggplot(aes(x = pwt_human_capital_index,
             y = WID_pre_tax_income_share_top_10pct,
             color = country_name)) +
  geom_point() +
  labs(title = "Income Inequality vs. Human Capital Index",
       subtitle = "Country-Year Averages (1960 - 2020)",
       x = "Human Capital Index",
       y = "Income Share of Top 10%",
       caption = "
       Data source(s): Penn World Table 10.0 & World Inequality Database") +
  theme_bw() +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank())
#--------------------------------------------------------------------------------------------------
names(new_mega_edits_df)

new_mega_edits_df %>%
  filter(iso3 %in% c("USA", "GBR", "FRA", 'DEU')) %>%
  group_by(iso3, year) %>%
  mutate(oecd_union_density = mean(oecd_union_density),
         oecd_unit_labor_costs = mean(oecd_unit_labor_costs)) %>%
  distinct(iso3, year, .keep_all = TRUE) %>%
  ggplot(aes(x = oecd_union_density,
             y = oecd_unit_labor_costs,
             color = country_name)) +
  geom_point() +
  labs(title = "Union Density vs. Unit Labor Costs",
       subtitle = "Country-Year Averages (1960 - 2020)",
       x = "Union Density",
       y = "Unit Labor Costs",
       caption = "
       Data source(s): OECD Data Warehouse") +
  theme_bw() +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank())
#--------------------------------------------------------------------------------------------------
names(new_mega_edits_df)

new_mega_edits_df %>%
  select(contains("labor")) %>%
  names()


new_mega_edits_df %>%
  filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>%
  ggplot(aes(x = oecd_union_density,
             y = oecd_unit_labor_costs)) +
  geom_point() +
  labs(title = "Union Density vs. Unit Labor Costs",
       x = "Union Density",
       y = "Unit Labor Costs",
       caption = "
       Data source(s): OECD Data Warehouse") +
  theme_bw() +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) +
  facet_wrap(vars(year))
#--------------------------------------------------------------------------------------------------
names(new_mega_edits_df)

new_mega_edits_df %>%
  select(contains("labor")) %>%
  names()

new_mega_edits_df %>%
  filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>%
  ggplot(aes(x = trade_union_density,
             y = pct_gdp_labor_compensation)) +
  geom_point() +
  labs(title = "Union Density vs. Labor Comnpensation",
       x = "Union Density",
       y = "Labor Share of Comnpensation (% GDP)",
       caption = "
       Data source(s): CPDS") +
  theme_bw() +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) +
  facet_wrap(vars(year))

new_mega_edits_df %>%
  filter(iso3 %in% c("USA", "GBR", "FRA", 'DEU', "JPN", "CAN")) %>%
  ggplot(aes(x = oecd_union_density,
             y = pct_gdp_labor_compensation)) +
  geom_point(size = 0.75) +
  labs(title = "Union Density vs. Labor Comnpensation (1960-2020)",
       x = "Union Density (% Working Population)",
       y = "Labor Share of Comnpensation (% GDP)",
       caption = "
       Data source(s): CPDS") +
  theme_bw() +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) +
  facet_wrap(vars(country_name))


