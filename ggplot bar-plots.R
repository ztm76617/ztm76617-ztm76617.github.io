#--------------------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------------------
library(tidyverse); library(stargazer); library(papeR); library(ggrepel); library(ggthemes); library(ggthemr); library(plm)
library(kableExtra); library(car); library(zoo); library(rio); library(readxl); library(haven); library(lubridate)
library(IndexNumR); library(wid); library(scales);library(ISOcodes); library(labelled); library(wesanderson); library(captioner)
library(stats); library(smooth); library(tm); library(TTR); library(naniar); library(countrycode); library(RColorBrewer)
library(WDI); library(lmtest); library(sandwich); library(interactions); library(ggpubr); library(gplots); library(purrr)
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

ggsave("tax_expend_dist.png", width = 8, height = 6)
#--------------------------------------------------------------------------------
# Stacked Barplot 
#--------------------------------------------------------------------------------
ggthemr('tableau')

tax_expenditure_distriibution_df %>%
  mutate(income_group = factor(income_group, c("first_quintile", "second_quintile", "middle_quintile", "fourth_quintile",
                                               "81_pctile_90pctile", "91st_pctile_95th_pctile", "96th_pctile_99th_pctile", "top_1pct"))) %>%
  ggplot(aes(income_quintile, income_tax_expenditure_share, fill = income_group)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.5) +
  labs(x = "Income Quintile",
       y = "Share of Income Tax Expenditures (%)",
       caption = "
       Data source(s): Congressional Budget Office",
       fill = "Income Percentiles",
       title = "Distribution of Income Tax Expenditures by Income Quintile") +
  theme(text = element_text(face = 'bold', size = 8),
        axis.title.x = element_text(vjust = -0.75),
        axis.title.y = element_text(vjust = 0.75),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.background = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.box.background = element_rect(colour = "black"),
        legend.box.margin = margin(2, 2, 2, 2)) +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4", "Q5")) +
  scale_fill_discrete(breaks = c("81_pctile_90pctile", "91st_pctile_95th_pctile", "96th_pctile_99th_pctile", "top_1pct"),
                      labels = c("81st to 90th",
                                 "91st to 95th",
                                 "96th to 99th",
                                 "Top 1%"))

ggsave("tax_expend_dist2.png", width = 6, height = 5)
#--------------------------------------------------------------------------------


