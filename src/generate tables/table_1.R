#-------------------------------------------------------------------------
# AUTHOR:             Jacob Jameson
# PURPOSE:            Generate table 1
#-------------------------------------------------------------------------
#
# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "haven", 'scales', 'survey', 'gridExtra',
          'gtsummary', 'labelled', 'broom', 'ggpubr', 'sandwich',
          'lmtest', 'marginaleffects', 'webshot2', 'gt',
          'kableExtra')

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

source('src/prepare data/Construct Analytical Dataset.R')
#-------------------------------------------------------------------------
# Table 1 ----------------------------------------------------------------

# Unweighted -------------------------------------------------------------

t1.vars <- c("race", 'pseudo.gpa', 'sespc_al', 'nhood1_d', 'edu', 'insurance',
             "w1.cigarettes", "w4.cigarettes.bin.30",
             "w1.marijuana", "w4.marijuana.bin.year", "w4.marijuana.bin.30",
             "w1.drunk", "w4.drunk.bin.year", "w4.drunk.bin.30",
             "w1.recreational", "w4.fav.bin.year", "w4.fav.bin.30",
             "w4.prescription")

sum <- final.df %>% filter(in_sample == 1) %>%
  select(one_of(t1.vars), above_school_avg) %>%
  tbl_summary(by = above_school_avg,
              type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})"),
              missing_text = "(Missing)") %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2),
        test = list(all_continuous() ~ "aov",
                    all_categorical() ~ "chisq.test")) %>%
  add_overall() %>% modify_header(label ~ "**Variable**") %>%
  modify_caption("**Patient Characteristics (unweighted)**") %>%
  bold_labels() %>% as_gt()

print(sum)
# Weighted -------------------------------------------------------------
final.weighted <- svydesign(id=~psuscid, strata=~region,
                            weights=~gswgt4_2, data=final.df, nest=TRUE)

sum <- subset(final.weighted, in_sample == 1) %>%
  tbl_svysummary(
    by = above_school_avg, 
    type = all_continuous() ~ "continuous2",
    statistic = 
      all_continuous() ~ c("{mean} ({sd})"),
    missing = "always",
    include = c(one_of(t1.vars), above_school_avg)) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2),
        test = list(all_continuous() ~ "svy.kruskal.test",
                    all_categorical() ~ "svy.chisq.test")) %>%
  add_overall() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1. Patient Characteristics (weighted)**") %>%
  bold_labels() %>% as_gt()

print(sum)
