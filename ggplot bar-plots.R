#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggthemes); library(ggthemr); library(plm)
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(IndexNumR); library(wid); library(scales);library(ISOcodes); library(labelled); library(wesanderson); library(captioner)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(countrycode); library(RColorBrewer)
library(WDI); library(lmtest); library(sandwich); library(interactions); library(ggpubr); library(gplots); library(purrr)

install.packages("postcards")
library(postcards)
#--------------------------------------------------------------------------------------
devtools::install_github('Mikata-Project/ggthemr')

names(wes_palettes)

scale_fill_manual(values = wes_palette("Royal1"))

scale_colour_brewer(palette = "Set1")

theme_update()

# fav brewer pallettes
#1 "Set1"
#2 "Set2"
#3 "Set3"
#4 "Dark2"
#5 "Accent"
#6 "Spectral"
#--------------------------------------------------------------------------------
tax_expenditure_distriibution_df <- read_excel("tax expenditure distriibution.xlsx")
#--------------------------------------------------------------------------------
# ggthemr themes
#1 fresh
#2 grape
#3 greyscale
#4 light
#5 sky
#6 solarized
# dust

ggthemr('greyscale')
swatch()
ggthemr_reset()

tax_expenditure_distriibution_df
#--------------------------------------------------------------------------------
swatch()

#--------------------------------------------------------------------------------
tableau_colours <- c("#111111", "#65ADC2", "#233B43", "#E84646", "#C29365", "#362C21", "#316675", "#168E7F", "#b2432f")
ggthemr_reset()
tableau <- define_palette(
  swatch = tableau_colours, 
  gradient = c(lower = tableau_colours[1L], upper = tableau_colours[2L]))
ggthemr(tableau)
#--------------------------------------------------------------------------------

names(tax_expenditure_distriibution_df)

tax_expenditure_distriibution_df %>%
  mutate(income_quintile = case_when(income_quintile == q5 ~ sum(income_quintile),))

tax_expenditure_distriibution_df %>%
  group_by(income_quintile) %>%
  summarise(income_tax_expenditure_share = sum(income_tax_expenditure_share)) %>%
  filter(income_quintile == "q5")

tax_expenditure_distriibution_df %>%
  group_by(income_quintile) %>%
  summarise(payroll_tax_expenditure_share = sum(payroll_tax_expenditure_share)) %>%
  filter(income_quintile == "q5")

new_row <- data.frame(income_group = "top_quintile",
           income_quintile = "q5",
           income_tax_expenditure_share = 50,
           payroll_tax_expenditure_share = 34.2)

tax_expenditure_distriibution_df_v2 <- rbind(tax_expenditure_distriibution_df, new_row)

write_rds(tax_expenditure_distriibution_df_v2, "tax_expenditure_distriibution_df_v2.rds")
#--------------------------------------------------------------------------------
# Solid Barplot
#--------------------------------------------------------------------------------
tax_expenditure_distriibution_df_v2 %>%
  mutate(income_group = factor(income_group, c("first_quintile", "second_quintile", "middle_quintile", "fourth_quintile", "top_quintile", "top_1pct"))) %>%
  filter(income_group %in% c("first_quintile", "second_quintile", "middle_quintile", "fourth_quintile", "top_quintile", "top_1pct")) %>%
  ggplot(aes(income_group, income_tax_expenditure_share)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.5) +
  theme_bw() +
  labs(x = "Income Group",
       y = "Share of Income Tax Expenditures (%)",
       caption = "
       Data source(s): Congressional Budget Office (2019)") +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.box.background = element_rect(colour = "black")) +
  scale_x_discrete(breaks = c("first_quintile", "second_quintile", "middle_quintile", "fourth_quintile", "top_quintile", "top_1pct"),
                   labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Top 1%"))

tax_expenditure_distriibution_df_v2 %>%
  mutate(income_group = factor(income_group, c("first_quintile", "second_quintile", "middle_quintile", "fourth_quintile", "top_quintile", "top_1pct"))) %>%
  filter(income_group %in% c("first_quintile", "second_quintile", "middle_quintile", "fourth_quintile", "top_quintile", "top_1pct")) %>%
  view()


#--------------------------------------------------------------------------------
names(new_mega_edits_df)

new_mega_edits_df %>%
  select(contains("debt")) %>%
  names()

new_mega_edits_df %>%
  filter(iso3 %in% c("USA", "GBR", "DEU", "JPN"),
         year == 2015) %>%
  select(country_name,
         pct_gdp_corporate_debt,
         pct_gdp_govt_debt,
         pct_gdp_total_household_debt) %>%
  rename("Corporate" = pct_gdp_corporate_debt,
         "Household" = pct_gdp_total_household_debt,
         "Government" = pct_gdp_govt_debt) %>%
  gather(key = "debt", value = "value", -country_name) %>%
  ggplot(aes(fill=debt, y=value, x=country_name)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  labs(x = "Country",
       y = "Debt Total (% GDP)",
       title = "Cross-National Debt Totals (2015)",
       caption = "Data Source(s): Global Debt Database") +
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.position = "right",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank())





