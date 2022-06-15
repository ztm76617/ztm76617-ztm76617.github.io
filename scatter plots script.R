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
# Marx ROP x GDP Growth (YOY) scatterplot
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN', 'DEU')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth,
         marx_rop_total_economy_v1) %>%
  rename("Real GDP Growth (Annual)" = pwt_real_gdp_growth,
         'Marx ROP' = marx_rop_total_economy_v1,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP`, y = `Real GDP Growth (Annual)`)) +
  geom_point() +
  facet_wrap(vars(`Country`))
#--------------------------------------------------------------------------------------------------
# Marx ROP x GDP Growth (YOY) scatterplot w/ smoothing 'lm' line
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN', 'DEU')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth,
         marx_rop_total_economy_v1) %>%
  rename("Real GDP Growth (Annual)" = pwt_real_gdp_growth,
         'Marx ROP' = marx_rop_total_economy_v1,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP`, y = `Real GDP Growth (Annual)`)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(vars(`Country`))
#--------------------------------------------------------------------------------------------------
# Marx ROP x GDP Growth (5yr MA) scatterplot
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR")) %>%
  ggplot(aes(x = marx_rop_total_economy_v2,
             y = pct_total_socspend_private_socspend_FULLPOP)) +
  geom_text(aes(label = year)) +
  facet_wrap(vars(country_code_ISO3))
#--------------------------------------------------------------------------------------------------
# Marx ROP x GDP Growth (5yr MA) scatterplot w/ smoothing 'lm' line
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN', 'DEU')) %>%
  select(country_code_ISO3,
         pct_total_socspend_private_socspend_FULLPOP_5yr_MA,
         marx_rop_total_economy_v2_5yr_MA) %>%
  rename("Private Social Spending" = pct_total_socspend_private_socspend_FULLPOP_5yr_MA,
         'Marx ROP' = marx_rop_total_economy_v2_5yr_MA,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP`, y = `Private Social Spending`, color = Country)) +
  geom_point()
#--------------------------------------------------------------------------------------------------
# Marx ROP x GDP Growth (5yr MA) scatterplot w/ smoothing 'lm' line w/o CI
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN', 'DEU')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_5yr_moving_avg,
         marx_rop_total_economy_v1) %>%
  rename("Real GDP Growth (5yr MA)" = pwt_real_gdp_growth_5yr_moving_avg,
         'Marx ROP' = marx_rop_total_economy_v1,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP`, y = `Real GDP Growth (5yr MA)`, color = `Country`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(vars(`Country`))
#--------------------------------------------------------------------------------------------------
# Marx ROP x GDP Growth (5yr MA) scatterplot w/ smoothing 'loess' line
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN', 'DEU')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_5yr_moving_avg,
         marx_rop_total_economy_v1) %>%
  rename("Real GDP Growth (5yr MA)" = pwt_real_gdp_growth_5yr_moving_avg,
         'Marx ROP' = marx_rop_total_economy_v1,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP`, y = `Real GDP Growth (5yr MA)`, color = `Country`)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(`Country`))
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x GDP Growth (5yr MA) scatterplot 
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_5yr_moving_avg,
         pct_change_marx_rop_total_economy_v1_5yr_ma) %>%
  rename("Real GDP Growth (5yr MA)" = pwt_real_gdp_growth_5yr_moving_avg,
         'Marx ROP (5yr MA)' = pct_change_marx_rop_total_economy_v1_5yr_ma,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP (5yr MA)`, y = `Real GDP Growth (5yr MA)`, color = `Country`)) +
  scale_y_continuous(limits = c(-2, 10), breaks = seq(-2, 10, by = 2)) +
  geom_point()
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x GDP Growth (5yr MA) scatterplot w/ smoothing 'loess' line
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_5yr_moving_avg,
         pct_change_marx_rop_total_economy_v1_5yr_ma) %>%
  rename("Real GDP Growth (5yr MA)" = pwt_real_gdp_growth_5yr_moving_avg,
         'Marx ROP (5yr MA)' = pct_change_marx_rop_total_economy_v1_5yr_ma,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP (5yr MA)`, y = `Real GDP Growth (5yr MA)`, color = `Country`)) +
  scale_y_continuous(limits = c(-2, 10), breaks = seq(-2, 10, by = 2)) +
  geom_point() +
  geom_smooth()
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x GDP Growth (5yr MA) scatterplot w/ smoothing 'lm' line
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_5yr_moving_avg,
         pct_change_marx_rop_total_economy_v1_5yr_ma) %>%
  rename("Real GDP Growth (5yr MA)" = pwt_real_gdp_growth_5yr_moving_avg,
         'Marx ROP (5yr MA)' = pct_change_marx_rop_total_economy_v1_5yr_ma,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP (5yr MA)`, y = `Real GDP Growth (5yr MA)`, color = `Country`)) +
  scale_y_continuous(limits = c(-2, 10), breaks = seq(-2, 10, by = 2)) +
  geom_point() +
  geom_smooth(method = 'lm')
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x GDP Growth (5yr MA) scatterplot w/ smoothing 'lm' line, w/o CI
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_5yr_moving_avg,
         pct_change_marx_rop_total_economy_v1_5yr_ma) %>%
  rename("Real GDP Growth (5yr MA)" = pwt_real_gdp_growth_5yr_moving_avg,
         'Marx ROP (5yr MA)' = pct_change_marx_rop_total_economy_v1_5yr_ma,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP (5yr MA)`, y = `Real GDP Growth (5yr MA)`, color = `Country`)) +
  scale_y_continuous(limits = c(-2, 10), breaks = seq(-2, 10, by = 2)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x GDP Growth (10yr MA) scatterplot 
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_10yr_moving_avg,
         pct_change_marx_rop_total_economy_v1_5yr_ma) %>%
  rename("Real GDP Growth (10yr MA)" = pwt_real_gdp_growth_10yr_moving_avg,
         'Marx ROP (5yr MA)' = pct_change_marx_rop_total_economy_v1_5yr_ma,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP (5yr MA)`, y = `Real GDP Growth (10yr MA)`, color = `Country`)) +
  scale_y_continuous(limits = c(-2, 10), breaks = seq(-2, 10, by = 2)) +
  geom_point()
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x GDP Growth (5yr MA) scatterplot w/ smoothing 'loess' line
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_5yr_moving_avg,
         pct_change_marx_rop_total_economy_v1_5yr_ma) %>%
  rename("Real GDP Growth (5yr MA)" = pwt_real_gdp_growth_5yr_moving_avg,
         'Marx ROP (5yr MA)' = pct_change_marx_rop_total_economy_v1_5yr_ma,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP (5yr MA)`, y = `Real GDP Growth (5yr MA)`, color = `Country`)) +
  scale_y_continuous(limits = c(-2, 10), breaks = seq(-2, 10, by = 2)) +
  geom_point() +
  geom_smooth()
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x GDP Growth (5yr MA) scatterplot w/ smoothing 'lm' line
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_5yr_moving_avg,
         pct_change_marx_rop_total_economy_v1_5yr_ma) %>%
  rename("Real GDP Growth (5yr MA)" = pwt_real_gdp_growth_5yr_moving_avg,
         'Marx ROP (5yr MA)' = pct_change_marx_rop_total_economy_v1_5yr_ma,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP (5yr MA)`, y = `Real GDP Growth (5yr MA)`, color = `Country`)) +
  scale_y_continuous(limits = c(-2, 10), breaks = seq(-2, 10, by = 2)) +
  geom_point() +
  geom_smooth(method = 'lm')
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x GDP Growth (5yr MA) scatterplot w/ smoothing 'lm' line, w/o CI
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_5yr_moving_avg,
         pct_change_marx_rop_total_economy_v1_5yr_ma) %>%
  rename("Real GDP Growth (5yr MA)" = pwt_real_gdp_growth_5yr_moving_avg,
         'Marx ROP (5yr MA)' = pct_change_marx_rop_total_economy_v1_5yr_ma,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP (5yr MA)`, y = `Real GDP Growth (5yr MA)`, color = `Country`)) +
  scale_y_continuous(limits = c(-2, 10), breaks = seq(-2, 10, by = 2)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x GDP Growth (10yr MA) scatterplot w/ smoothing 'lm' line
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN', 'DEU')) %>%
  select(country_code_ISO3,
         pwt_real_gdp_growth_10yr_moving_avg,
         marx_rop_total_economy_v1_10yr_ma) %>%
  rename("Real GDP Growth (10yr MA)" = pwt_real_gdp_growth_10yr_moving_avg,
         'Marx ROP (10yr MA)' = marx_rop_total_economy_v1_10yr_ma,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = `Marx ROP (10yr MA)`, y = `Real GDP Growth (10yr MA)`, color = `Country`)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(vars(`Country`))
#--------------------------------------------------------------------------------------------------
# Marx ROP (5yr MA) x Private Social Spending
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA", "JPN", 'CAN', 'DEU')) %>%
  rename("Private Social Spending" = pct_total_socspend_private_socspend_FULLPOP,
         'Marx ROP' = marx_rop_total_economy_v2,
         'Country' = country_code_ISO3) %>%
  ggplot(aes(x = 'Marx ROP', y = "Private Social Spending", color = Country)) +
  geom_point() +
  facet_wrap(vars(Country))
#--------------------------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA",
                                  "JPN", 'CAN', 'DEU',
                                  "AUS", "ESP")) %>%
  ggplot(aes(x = pwt_GDP_per_capita_2017_price_2017_ppp,
             y = WID_share_pre_tax_income_top_10pct,
             color = country_code_ISO3)) +
  geom_point()
#--------------------------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA",
                                  "JPN", 'CAN', 'DEU',
                                  "AUS", "ESP")) %>%
  ggplot(aes(x = pwt_GDP_per_capita_2017_USD,
             y = WID_share_pre_tax_income_top_10pct,
             color = country_code_ISO3)) +
  geom_point()
#--------------------------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  filter(country_code_ISO3 %in% c("USA", "GBR", "FRA",
                                  "JPN", 'CAN', 'DEU',
                                  "AUS", "ESP")) %>%
  ggplot(aes(x = pwt_human_capital_index,
             y = WID_share_pre_tax_income_top_10pct,
             color = country_code_ISO3)) +
  geom_point()
#--------------------------------------------------------------------------------------------------
mega_combined_vars_df_SUBSET_added_vars %>%
  group_by(year) %>%
  mutate(year_GDP_rank = dense_rank(desc(pwt_GDP_per_capita_2017_price_2017_ppp))) %>%
  arrange(year, year_GDP_rank) %>%
  mutate(avg_pwt_human_capital_index_wt = weighted.mean(pwt_human_capital_index, year_GDP_rank, na.rm = TRUE),
         avg_WID_share_pre_tax_income_top_10pct_wt = weighted.mean(WID_share_pre_tax_income_top_10pct, year_GDP_rank, na.rm = TRUE),
         avg_pwt_human_capital_index = mean(pwt_human_capital_index, na.rm = TRUE),
         avg_WID_share_pre_tax_income_top_10pct = mean(WID_share_pre_tax_income_top_10pct, na.rm = TRUE)) %>%
  distinct(year, .keep_all = TRUE) %>%
  ggplot(aes(x = avg_pwt_human_capital_index_wt,
             y = avg_WID_share_pre_tax_income_top_10pct_wt)) +
  geom_point()

