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
#--------------------------------------------------------------------------------------
names(mega_combined_vars_df_SUBSET_added_vars)

mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_marx_occ,
         pwt_marx_ROSV) %>%
  mutate(pwt_marx_occ = scale(pwt_marx_occ),
         pwt_marx_ROSV = scale(pwt_marx_ROSV)) %>%
  filter(country_code_ISO3 %in% c("CHN", "USA", "GBR", "FRA", "JPN", "CAN")) %>%
  rename("OCC" = pwt_marx_occ,
         "ROSV" = pwt_marx_ROSV) %>%
  gather(key = "Legend", value = "value", -country_code_ISO3, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = `Legend`)) +
  theme_bw() +
  geom_line(aes(color = `Legend`), size = 1) +
  labs(y = "",
       x = "Year",
       title = "Comparing Economic Conditions Across Countries",
       caption = "Data source(s): Penn World Tables") +
  theme(axis.text.x = element_text(angle=50, hjust=1),
        text = element_text(size=14, face = 'bold'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        plot.caption = element_text(hjust = 0, face = "bold.italic", size=12),
        plot.caption.position = "plot",
        plot.margin = unit(c(0.5,0.5,0.75,0.5), "cm"),
        legend.position="right",
        strip.text.x = element_text(size = 15, face = "bold"),
        legend.title = element_text(size=0),
        legend.text = element_text(size=12)) +
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1970, 2020, by = 10)) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(vars(country_code_ISO3))
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         pwt_icor_number) %>%
  filter(country_code_ISO3 %in% c("CHN", "USA", "GBR", "FRA", "JPN", "CAN")) %>%
  rename("ICOR" = pwt_icor_number) %>%
  gather(key = "Legend", value = "value", -country_code_ISO3, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = `Legend`)) +
  theme_bw() +
  geom_line(aes(color = `Legend`), size = 1) +
  labs(y = "",
       x = "Year",
       title = "Comparing Economic Conditions Across Countries",
       caption = "Data source(s): Penn World Tables") +
  theme(axis.text.x = element_text(angle=50, hjust=1),
        text = element_text(size=14, face = 'bold'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        plot.caption = element_text(hjust = 0, face = "bold.italic", size=12),
        plot.caption.position = "plot",
        plot.margin = unit(c(0.5,0.5,0.75,0.5), "cm"),
        legend.position="right",
        strip.text.x = element_text(size = 15, face = "bold"),
        legend.title = element_text(size=0),
        legend.text = element_text(size=12)) +
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1970, 2020, by = 10)) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(vars(country_code_ISO3))
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  select(country_code_ISO3, year,
         WID_share_pre_tax_income_bottom_50pct,
         WID_share_pre_tax_income_top_1pct) %>%
  filter(country_code_ISO3 %in% c("CHN", "USA", "GBR", "FRA", "JPN", "CAN")) %>%
  rename("Bottom 50%" = WID_share_pre_tax_income_bottom_50pct,
         "Top 1%" = WID_share_pre_tax_income_top_1pct) %>%
  gather(key = "Legend", value = "value", -country_code_ISO3, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = `Legend`)) +
  theme_grey() +
  geom_line(aes(color = `Legend`), size = 1) +
  labs(y = "",
       x = "Year",
       title = "Comparing Economic Conditions Across Countries",
       caption = "Data source(s): World Inequality Database") +
  theme(axis.text.x = element_text(angle = 50, hjust=1),
        text = element_text(size = 14, face = 'bold'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        plot.caption = element_text(hjust = 0, face = "bold.italic", size=12),
        plot.caption.position = "plot",
        plot.margin = unit(c(0.5,0.5,0.75,0.5), "cm"),
        legend.position="right",
        strip.text.x = element_text(size = 15, face = "bold"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1970, 2020, by = 5)) +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(vars(country_code_ISO3))
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "FRA", "DEU", "JPN")) %>%
  select(country_name, year,
         pwt_gdp_growth_2017_ppp_5yr_MA) %>%
  rename("GDP Growth (5yr MA)" = pwt_gdp_growth_2017_ppp_5yr_MA) %>%
  gather(key = "gdp", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  theme_bw() +
  geom_smooth(method = 'lm') +
  geom_line() +
  labs(y = "Percent (%)",
       x = "Year",
       title = "GDP Growth (5yr Moving Average)",
       subtitle = "G6 Countries",
       caption = "Data source(s): Penn World Tables") +
  theme(axis.text.x = element_text(angle=35, hjust=1),
        text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 0, face = "bold.italic"),
        plot.caption.position = "plot",
        legend.position="bottom",
        strip.text.x = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN")) %>%
  select(country_name, year,
         pwt_gdp_growth_2017_USD_5yr_MA) %>%
  rename("GDP Growth (Annual)" = pwt_gdp_growth_2017_USD_5yr_MA) %>%
  gather(key = "Legend", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Legend)) +
  theme_linedraw() +
  geom_smooth(method = 'lm') +
  labs(title = "Comparing Economic Conditions Across Countries",
       x = "Year",
       y = "Percent (%)",
       caption = "Data source(s): Penn World Table 10.0") +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "right",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "FRA")) %>%
  rename(Private = oecd_total_private_socspend_full_pop_pct_gdp,
         Public = oecd_public_socspend_full_pop_pct_gdp) %>%
  select(country_name, year, Private, Public) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_linedraw() +
  labs(title = "Cross-National Social Spending (% GDP)",
       x = "Year",
       y = "Percent (%)",
       caption = "Data source(s): Penn World Tables") +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "right",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Private Social spending (% Total Spending) ggplot
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "NOR")) %>%
  ggplot(aes(x = year, y = pct_total_socspend_private)) +
  geom_line() +
  theme_linedraw() +
  labs(title = "Cross-National Welfare Spending",
       subtitle = "Private Spending (% Total Spending)",
       x = "Year",
       y = "Percent (%)",
       caption = "Data source(s): OECD Social Protection Database") +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "right",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Private vs Public Welfare Spending
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "NOR")) %>%
  ggplot(aes(x = year, y = faricy_ratio)) +
  geom_line() +
  theme_linedraw() +
  labs(x = "Year",
       y = "Percent (%)",
       caption = "Data source(s): OECD Social Protection Database") +
  theme(text = element_text(face = 'bold'),
        axis.text.x = element_text(angle=30, hjust=1),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "right",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Average profit rates ggplot
mega_combined_vars_df_SUBSET_added_vars %>%
  group_by(year) %>%
  mutate(IRR = mean(pwt_irr, na.rm = TRUE),
         ROP = mean(pwt_marx_rop, na.rm = TRUE)) %>%
  distinct(year, .keep_all = TRUE) %>%
  select(year, ROP, IRR) %>%
  gather(key = "Type", value = "value", -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_linedraw() +
  labs(x = "Year",
       y = "Percent (%)",
       caption = "Data Source(s): Penn World Tables") +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        strip.text.x = element_text(face = "bold"),
        legend.position = "right",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10))
#--------------------------------------------------------------------------------
# Inequality vs Private Welfare Spending
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "NOR")) %>%
  rename("Private Social Spending (% GDP)" = oecd_total_private_socspend_full_pop_pct_gdp,
         "Top 0.1% Income Share" = WID_pre_tax_income_share_top_0.1pct) %>%
  select(country_name, year, "Private Social Spending (% GDP)", "Top 0.1% Income Share") %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_linedraw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): OECD Social Protection Database & World Inequality Database") +
  theme(text = element_text(face = 'bold'),
        panel.grid = element_line(color = "#8ccde3", size = 0.75, linetype = 2),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  scale_y_continuous(limits = c(0, 12.5), breaks = seq(0, 12.5, by = 2.5)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Financial vs Nonfinancial Assets
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "NOR")) %>%
  select(country_name, year,
         WID_pct_gdp_national_financial_assets,
         WID_pct_gdp_national_nonfinancial_assets) %>%
  rename("Financial Assets" = WID_pct_gdp_national_financial_assets,
         "Non-Financial Assets" = WID_pct_gdp_national_nonfinancial_assets) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): World Inequality Database") +
  theme(text = element_text(face = 'bold', size = 10),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Financial vs Nonfinancial Assets Average
mega_combined_vars_df_SUBSET_added_vars %>%
  select(year,
         WID_pct_gdp_national_financial_assets,
         WID_pct_gdp_national_nonfinancial_assets) %>%
  group_by(year) %>%
  mutate(WID_pct_gdp_national_financial_assets = mean(WID_pct_gdp_national_financial_assets, na.rm = TRUE),
         WID_pct_gdp_national_nonfinancial_assets = mean(WID_pct_gdp_national_nonfinancial_assets, na.rm = TRUE)) %>%
  ungroup(year) %>%
  rename("Financial Assets" = WID_pct_gdp_national_financial_assets,
         "Non-Financial Assets" = WID_pct_gdp_national_nonfinancial_assets) %>%
  gather(key = "Type", value = "value", -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): World Inequality Database") +
  theme(text = element_text(face = 'bold', size = 10),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10))
#--------------------------------------------------------------------------------
# Financial vs Nonfinancial Assets Average
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "FRA")) %>%
  select(country_name, year,
         pwt_pct_gdp_productive_investment,
         pwt_marx_rop) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): World Inequality Database") +
  theme(text = element_text(face = 'bold', size = 10),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Financial vs Nonfinancial Assets Average
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "FRA", "JPN", "DEU")) %>%
  select(country_name, year,
         WID_pct_gdp_national_financial_assets,
         WID_pct_gdp_national_nonfinancial_assets) %>%
  rename(Financial = WID_pct_gdp_national_financial_assets,
         Nonfinancial = WID_pct_gdp_national_nonfinancial_assets) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): World Inequality Database") +
  theme(text = element_text(face = 'bold', size = 10),
        axis.text.x = element_text(angle = 30, hjust = 0.5),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1970, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Debts as % GDP
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR",
                                  "FRA", "JPN")) %>%
  select(country_name, year,
         WID_pct_gdp_corporate_debt,
         WID_pct_gdp_govt_debt) %>%
  rename("Corporate Debt" = WID_pct_gdp_corporate_debt,
         "Government Debt" = WID_pct_gdp_govt_debt) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): World Inequality Database") +
  theme(text = element_text(face = 'bold', size = 10),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Financial Assets + Debts as % GDP
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "FRA", "JPN", "DEU")) %>%
  select(country_name, year,
         pwt_pct_gdp_net_fixed_capital_stock,
         WID_pct_gdp_corporate_financial_assets) %>%
  rename("Corporate Financial Assets" = WID_pct_gdp_corporate_financial_assets,
         "Net Fixed Capital Stock" = pwt_pct_gdp_net_fixed_capital_stock) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): Penn World Tables & World Inequality Database") +
  theme(text = element_text(face = 'bold', size = 10),
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Financial Assets + Debts as % GDP
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "FRA", "JPN", "DEU")) %>%
  select(country_name, year,
         pwt_pct_gdp_net_fixed_capital_stock,
         WID_pct_gdp_corporate_financial_assets) %>%
  rename("Corporate Financial Assets" = WID_pct_gdp_corporate_financial_assets,
         "Net Fixed Capital Stock" = pwt_pct_gdp_net_fixed_capital_stock) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): Penn World Tables & World Inequality Database") +
  theme(text = element_text(face = 'bold', size = 10),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Financial vs. Nonfinancial Assets  % GDP
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "FRA", "JPN", "DEU")) %>%
  select(country_name, year,
         WID_pct_gdp_corporate_financial_assets,
         WID_pct_gdp_corporate_nonfinancial_assets) %>%
  rename("Financial Assets" = WID_pct_gdp_corporate_financial_assets,
         "Nonfinancial Assets" = WID_pct_gdp_corporate_nonfinancial_assets) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): World Inequality Database") +
  theme(text = element_text(face = 'bold', size = 10),
        axis.text.x = element_text(angle = 35, hjust = 0.75),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  scale_y_continuous(limits = c(100, 1750), breaks = seq(100, 1750, by = 375)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Financial vs. Nonfinancial Assets Ratio
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "FRA", "JPN", "DEU")) %>%
  select(country_name, year,
         WID_ratio_corporate_financial_vs_nonfinancial_assets) %>%
  rename("Ratio: Financial to Non-Financial Assets" = WID_ratio_corporate_financial_vs_nonfinancial_assets) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): World Inequality Database") +
  theme(text = element_text(face = 'bold', size = 10),
        axis.text.x = element_text(angle = 30, hjust = 0.5),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Population Comparisons
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "NOR")) %>%
  select(country_name, year, pct_gdp_pc_private_welfare_work_pc, pct_gdp_pc_private_welfare_pc) %>%
  rename("Working Population" = pct_gdp_pc_private_welfare_work_pc,
         "Total Population" = pct_gdp_pc_private_welfare_pc) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): OECD Social Protection Database & World Inequality Database") +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        panel.grid = element_line(color = "gray", size = 0.1),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# ROP vs Private Welfare Spending
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "NOR", "DEU", "FRA")) %>%
  select(country_name, year, pct_gdp_private_welfare, pwt_marx_rop) %>%
  rename("Private Welfare (% GDP)" = pct_gdp_private_welfare,
         "ROP" = pwt_marx_rop) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "Data source(s): OECD Social Protection Database & Penn World Tables") +
  theme(text = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 35, hjust = 0.75),
        panel.background = element_rect(colour = "black"),
        panel.grid = element_line(color = "gray", size = 0.1),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# IRR vs Private Welfare Spending
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "NOR", "DEU", "FRA")) %>%
  select(country_name, year, pct_gdp_private_welfare, pwt_irr) %>%
  rename("Private Welfare (% GDP)" = pct_gdp_private_welfare,
         "IRR" = pwt_irr) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "Data source(s): OECD Social Protection Database & Penn World Tables") +
  theme(text = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 35, hjust = 0.75),
        panel.background = element_rect(colour = "black"),
        panel.grid = element_line(color = "gray", size = 0.1),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Average profit rates vs. private welfare ggplot
mega_combined_vars_df_SUBSET_added_vars %>%
  group_by(year) %>%
  mutate("Private Welfare (% GDP)" = mean(pct_gdp_private_welfare, na.rm = TRUE),
         "Marxist ROP" = mean(pwt_marx_rop, na.rm = TRUE)) %>%
  distinct(year, .keep_all = TRUE) %>%
  select(year, `Marxist ROP`, `Private Welfare (% GDP)`) %>%
  gather(key = "Type", value = "value", -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "Data Source(s): OECD Social Protection Database & Penn World Tables") +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        strip.text.x = element_text(face = "bold"),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10))
#--------------------------------------------------------------------------------
# Average profit rates ggplot
mega_combined_vars_df_SUBSET_added_vars %>%
  group_by(year) %>%
  mutate(pwt_marx_rop = mean(pwt_marx_rop, na.rm = TRUE)) %>%
  distinct(year, .keep_all = TRUE) %>%
  select(year, pwt_marx_rop) %>%
  rename("Average Rate of Profit" = pwt_marx_rop) %>%
  gather(key = "Type", value = "value", -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "Data Source(s): Penn World Table 10.0") +
  theme(text = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 35, hjust = 0.75),
        panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10))

ggsave("rop_small.png", width = 4, height = 3)
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "ITA", "DEU", "FRA")) %>%
  select(country_name, year,
         pct_gdp_private_welfare,
         pct_gdp_public_welfare) %>%
  rename("Private" = pct_gdp_private_welfare,
         "Public" = pct_gdp_public_welfare) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "Data source(s): OECD Social Protection Database") +
  theme(text = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 35, hjust = 0.75),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.15, "cm"),
        axis.ticks.length.x = unit(.15, "cm"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Cross-National Debt Levels (% GDP) v1
p1 <- mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR", "FRA", "JPN", "DEU")) %>%
  select(country_name, year, WID_pct_gdp_corporate_debt, WID_pct_gdp_govt_debt) %>%
  rename("Corporate Debt" = WID_pct_gdp_corporate_debt,
         "Government Debt" = WID_pct_gdp_govt_debt) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent of GDP (%)",
       linetype = "",
       caption = "Data source(s): World Inequality Database",
       title = "Cross-National Debt Levels (% GDP)") +
  theme(text = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 40, hjust = 1),
        axis.title.x = element_text(vjust = -0.75),
        axis.title.y = element_text(vjust = 0.75),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
# Cross-National Debt Levels (% GDP) v2
p2 <- mega_combined_vars_df_SUBSET_added_vars %>%
  group_by(year) %>%
  filter(country_code_ISO3 %in% g7_countries) %>%
  mutate(pct_gdp_private_welfare = mean(pct_gdp_private_welfare, na.rm = TRUE),
         pct_gdp_public_welfare = mean(pct_gdp_public_welfare, na.rm = TRUE)) %>%
  distinct(year, .keep_all = TRUE) %>%
  select(year, pct_gdp_public_welfare, pct_gdp_private_welfare) %>%
  rename("Private" = pct_gdp_private_welfare,
         "Public" = pct_gdp_public_welfare) %>%
  gather(key = "variable", value = "value", -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = variable)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent of GDP (%)",
       caption = "
       Data source(s): OECD Social Protection Database") +
  theme(text = element_text(face = 'bold'),
        axis.title.x = element_text(vjust = -0.75),
        axis.title.y = element_text(vjust = 0.75),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10))
#--------------------------------------------------------------------------------
# Cross-National Debt Levels (% GDP) v3
p3 <- mega_combined_vars_df_SUBSET_added_vars %>%
  group_by(year) %>%
  filter(country_code_ISO3 %in% g7_countries) %>%
  mutate(WID_pct_gdp_corporate_debt = mean(WID_pct_gdp_corporate_debt, na.rm = TRUE),
         WID_pct_gdp_govt_debt = mean(WID_pct_gdp_govt_debt, na.rm = TRUE)) %>%
  distinct(year, .keep_all = TRUE) %>%
  select(year, WID_pct_gdp_corporate_debt, WID_pct_gdp_govt_debt) %>%
  rename("Corporate Debt" = WID_pct_gdp_corporate_debt,
         "Government Debt" = WID_pct_gdp_govt_debt) %>%
  gather(key = "variable", value = "value", -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = variable)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent of GDP (%)",
       caption = "
       Data source(s): World Inequality Database") +
  theme(text = element_text(face = 'bold'),
        axis.title.x = element_text(vjust = -0.75),
        axis.title.y = element_text(vjust = 0.75),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10))
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  group_by(year) %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "JPN", "DEU", "FRA", "ITA")) %>%
  mutate(WID_pre_tax_income_share_bottom_50pct = mean(WID_pre_tax_income_share_bottom_50pct, na.rm = TRUE),
         WID_pre_tax_income_share_top_1pct = mean(WID_pre_tax_income_share_top_1pct, na.rm = TRUE)) %>%
  distinct(year, .keep_all = TRUE) %>%
  select(year, WID_pre_tax_income_share_top_1pct, WID_pre_tax_income_share_bottom_50pct) %>%
  rename("Bottom 50%" = WID_pre_tax_income_share_bottom_50pct,
         "Top 1%" = WID_pre_tax_income_share_top_1pct) %>%
  gather(key = "variable", value = "value", -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = variable)) +
  theme_bw() +
  labs(x = "Year",
       y = "Share of National Income (%)",
       caption = "
       Data source(s): World Inequality Database",
       title = "Average National Income Share in G7 Countries") +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1970, 2020, by = 10))

ggsave("income_ineq_g7.png", width = 8, height = 6)
#--------------------------------------------------------------------------------
# Inequality vs Private Welfare Spending
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "NOR", "DEU", "FRA")) %>%
  rename("Private Social Spending (% GDP)" = oecd_total_private_socspend_full_pop_pct_gdp,
         "Top 1% Income Share" = WID_pre_tax_income_share_top_1pct) %>%
  select(country_name, year, "Private Social Spending (% GDP)", "Top 1% Income Share") %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw(base_size = 5, base_rect_size = 0.35, base_line_size = 0.2) +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): OECD Social Protection Database & World Inequality Database") +
  theme(text = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 35, hjust = 1),
        axis.title.x = element_text(vjust = -0.75),
        axis.title.y = element_text(vjust = 0.75),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))

ggsave("private_socx_ineq.png", width = 12, height = 8)
ggsave("private_socx_ineq-v2.png", width = 4, height = 3)
#--------------------------------------------------------------------------------
new_mega_edits_df %>%
  filter(iso3 %in% c("CAN", "USA", "GBR",
                                  "NOR", "DEU", "FRA")) %>%
  select(country_name, year, marx_rop, total_private_socx_fullpop_pct_gdp) %>%
  rename("Marxist ROP" = marx_rop,
         "Private Welfare (% GDP)" = total_private_socx_fullpop_pct_gdp) %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data Source(s): OECD Social Protection Database & Penn World Table 10.0") +
  theme(text = element_text(face = 'bold', size = 10),
        axis.text.x = element_text(angle = 35, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1970, 2020, by = 10)) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 5)) +
  facet_wrap(vars(country_name))
#--------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("CAN", "USA", "GBR",
                                  "NOR", "DEU", "FRA")) %>%
  rename("Private Social Spending (% GDP)" = oecd_total_private_socspend_full_pop_pct_gdp,
         "Top 1% Income Share" = WID_pre_tax_income_share_top_1pct) %>%
  select(country_name, year, "Private Social Spending (% GDP)", "Top 1% Income Share") %>%
  gather(key = "Type", value = "value", -country_name, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = Type)) +
  theme_bw() +
  labs(x = "Year",
       y = "Percent (%)",
       linetype = "",
       caption = "
       Data source(s): OECD Social Protection Database & World Inequality Database") +
  theme(text = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980, 2020, by = 10)) +
  facet_wrap(vars(country_name))

theme_set(theme_bw(base_size = 10))







