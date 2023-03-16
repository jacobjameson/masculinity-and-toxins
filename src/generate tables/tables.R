#-------------------------------------------------------------------------
# AUTHOR:             Jacob Jameson
# PURPOSE:            Generate tables
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
             "w1.cigarettes", "w1.marijuana", 
             "w1.drunk", "w1.recreational", "w4.cigarettes", "w4.marijuana", 
             "w4.drunk", "w4.recreational", "w4.prescription")

sum <- final.df %>% filter(in_sample == 1) %>%
  select(one_of(t1.vars), school_self) %>%
  tbl_summary(by = school_self,
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
    by = school_self, 
    type = all_continuous() ~ "continuous2",
    statistic = 
      all_continuous() ~ c("{mean} ({sd})"),
    missing = "always",
    include = c(one_of(t1.vars), school_self)) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2),
        test = list(all_continuous() ~ "svy.kruskal.test",
                    all_categorical() ~ "svy.chisq.test")) %>%
  add_overall() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1. Patient Characteristics (weighted)**") %>%
  bold_labels() %>% as_gt()

print(sum)

# TABLE 2 --------------------------------------------------------------------------

### Create function for regression analysis
reg_analysis <- function(frml, df, vcv, log=T){
  
  #### INPUTS
  ##   frml= string containing formula of lm model
  ##   df  = data.frame
  ##   vcv = type of SEm.
  #### OUTPUTS
  ##  A data.frame with the estimated regression coefficient
  ## and heteroskedastic robust SE (also 95% Confidence Intervals).
  
  ####Transform string into formula 
  frml1 <- as.formula(frml)
  ####Estimate regression model
  if (log == T){
    m0 <- svyglm(formula = frml1, design = df, family = 'quasibinomial')
  }
  else{
    m0 <- svyglm(formula = frml1, design = df)
  }
  ####Robust variance-covariance matrix
  vcv_robust <- vcovHC(m0, type = vcv)
  ####Calculate new SEs
  #robust <- coeftest(x = m0, vcov. = vcv_robust)
  robust <- coeftest(x = m0)
  ####Calculate Confindence Intervals (95%)
  ci     <- confint(robust)
  ####Definitive format
  robust %>% broom::tidy() %>% as.data.frame() %>%
    dplyr::mutate(outcome = str_squish(word(frml,1,sep = "\\~")),
                  lwr = as.data.frame(ci)[,1],
                  upr = as.data.frame(ci)[,2]) %>%
    dplyr::select(outcome,term:p.value,lwr,upr) -> robust_df
  ####Return output
  return(robust_df)
}


# AVERAGE MARGINAL EFFECTS
### Create function for regression analysis
ape_analysis <- function(frml, df, vcv){
  
  #### INPUTS
  ##   frml= string containing formula of lm model
  ##   df  = data.frame
  ##   vcv = type of SEm.
  #### OUTPUTS
  ##  A data.frame with the estimated regression coefficient 
  ## and heteroskedastic robust SE (also 95% Confidence Intervals).
  ####Transform string into formula 
  frml1 <- as.formula(frml)
  ####Estimate regression model
  m0 <- svyglm(formula = frml1, design = df, family = 'quasibinomial')
  ####Robust variance-covariance matrix
  vcv_robust <- vcovHC(m0, type = vcv)
  ####Calculate APE
  #ape <- slopes(model = m0, vcov = vcv_robust, by = "above_school_avg")
  ape <- slopes(model = m0, by = "above_school_avg")
  ####Definitive format
  ape %>% tidy() %>% as.data.frame() %>%
    mutate(outcome = str_squish(word(frml,1,sep = "\\~")) ) %>%
    select(outcome,term:conf.high) %>%
    filter(term == 'delta_w1_w4_GE')-> ape_df
  ####Return output
  return(ape_df)
}

# Smoking
s.0 <- reg_analysis(
  frml = "w4.cigarettes ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") %>%
  mutate(model = 'model 0')

s.1 <- reg_analysis(
  frml = "w4.cigarettes ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.cigarettes", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 1')

s.2 <- reg_analysis(
  frml = "w4.cigarettes ~ w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.cigarettes", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 2')

s.3 <- reg_analysis(
  frml = "w4.cigarettes ~ delta_w1_w4_GE + race + pseudo.gpa + sespc_al + nhood1_d + w1.cigarettes", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 3')

s.4 <- reg_analysis(
  frml = "w4.cigarettes ~ w1.GE_male_std + w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.cigarettes", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 4')

s.5 <- reg_analysis(
  frml = "w4.cigarettes ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa + sespc_al + nhood1_d + w1.cigarettes", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 5')

s.5.ame <- ape_analysis(
  frml = "w4.cigarettes ~ delta_w1_w4_GE*factor(above_school_avg) + race + pseudo.gpa + sespc_al + nhood1_d + w1.cigarettes", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") 

s.results <- rbind(s.0, '', s.1, '', s.2, '',  s.3, '',  s.4, '',  s.5)

# Marijuana
m.0 <- reg_analysis(
  frml = "w4.marijuana ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") %>%
  mutate(model = 'model 0')

m.1 <- reg_analysis(
  frml = "w4.marijuana ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.marijuana", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 1')

m.2 <- reg_analysis(
  frml = "w4.marijuana ~ w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.marijuana", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 2')

m.3 <- reg_analysis(
  frml = "w4.marijuana ~ delta_w1_w4_GE + race + pseudo.gpa + sespc_al + nhood1_d + w1.marijuana", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 3')

m.4 <- reg_analysis(
  frml = "w4.marijuana ~ w1.GE_male_std + w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.marijuana", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 4')

m.5 <- reg_analysis(
  frml = "w4.marijuana ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa + sespc_al + nhood1_d + w1.marijuana", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 5')

m.5.ame <- ape_analysis(
  frml = "w4.marijuana ~ delta_w1_w4_GE*factor(above_school_avg) + race + pseudo.gpa + sespc_al + nhood1_d + w1.marijuana", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") 

m.results <- rbind(m.0, '', m.1, '', m.2, '',  m.3, '',  m.4, '',  m.5)

names(final.df)
# Alcohol
a.0 <- reg_analysis(
  frml = "w4.drunk ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") %>%
  mutate(model = 'model 0')

a.1 <- reg_analysis(
  frml = "w4.drunk ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.drunk", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 1')

a.2 <- reg_analysis(
  frml = "w4.drunk ~ w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.drunk", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 2')

a.3 <- reg_analysis(
  frml = "w4.drunk ~ delta_w1_w4_GE + race + pseudo.gpa + sespc_al + nhood1_d + w1.drunk", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 3')

a.4 <- reg_analysis(
  frml = "w4.drunk ~ w1.GE_male_std + w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.drunk", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 4')

a.5 <- reg_analysis(
  frml = "w4.drunk ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa + sespc_al + nhood1_d + w1.drunk", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 5')

a.5.ame <- ape_analysis(
  frml = "w4.drunk ~ delta_w1_w4_GE*factor(above_school_avg) + race + pseudo.gpa + sespc_al + nhood1_d + w1.drunk", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") 

a.results <- rbind(a.0, '', a.1, '', a.2, '',  a.3, '',  a.4, '',  a.5)


# Recreational
r.0 <- reg_analysis(
  frml = "w4.recreational ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") %>%
  mutate(model = 'model 0')

r.1 <- reg_analysis(
  frml = "w4.recreational ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.recreational", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 1')

r.2 <- reg_analysis(
  frml = "w4.recreational ~ w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.recreational", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 2')

r.3 <- reg_analysis(
  frml = "w4.recreational ~ delta_w1_w4_GE + race + pseudo.gpa + sespc_al + nhood1_d + w1.recreational", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 3')

r.4 <- reg_analysis(
  frml = "w4.recreational ~ w1.GE_male_std + w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d + w1.recreational", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 4')

r.5 <- reg_analysis(
  frml = "w4.recreational ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa + sespc_al + nhood1_d + w1.recreational", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 5')

r.5.ame <- ape_analysis(
  frml = "w4.recreational ~ delta_w1_w4_GE*factor(above_school_avg) + race + pseudo.gpa + sespc_al + nhood1_d + w1.recreational", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") 

r.results <- rbind(r.0, '', r.1, '', r.2, '',  r.3, '',  r.4, '',  r.5)


# Prescription
p.0 <- reg_analysis(
  frml = "w4.prescription ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") %>%
  mutate(model = 'model 0')

p.1 <- reg_analysis(
  frml = "w4.prescription ~ w1.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d ", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 1')

p.2 <- reg_analysis(
  frml = "w4.prescription ~ w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 2')

p.3 <- reg_analysis(
  frml = "w4.prescription ~ delta_w1_w4_GE + race + pseudo.gpa + sespc_al + nhood1_d ", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 3')

p.4 <- reg_analysis(
  frml = "w4.prescription ~ w1.GE_male_std + w4.GE_male_std + race + pseudo.gpa + sespc_al + nhood1_d ", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 4')

p.5 <- reg_analysis(
  frml = "w4.prescription ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa + sespc_al + nhood1_d", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0")  %>%
  mutate(model = 'model 5')

p.5.ame <- ape_analysis(
  frml = "w4.prescription ~ delta_w1_w4_GE*factor(above_school_avg) + race + pseudo.gpa + sespc_al + nhood1_d ", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0") 

p.results <- rbind(p.0, '', p.1, '', p.2, '',  p.3, '',  p.4, '',  p.5)



# Delta GE ------------------------------------------------------------------

m0 <- reg_analysis(
  frml = "delta_w1_w4_GE ~ race + pseudo.gpa +
               sespc_al + nhood1_d + w1.GE_male_std_school", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0", F)  %>%
  mutate(model = 'model 0')

m1 <- reg_analysis(
  frml = "delta_w1_w4_GE ~ race + pseudo.gpa +
               sespc_al + nhood1_d + katz_centrality_R", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0", F)  %>%
  mutate(model = 'model 1')

m2 <- reg_analysis(
  frml = "delta_w1_w4_GE ~ race + pseudo.gpa +
               sespc_al + nhood1_d + mbff_reciprocity", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0", F)  %>%
  mutate(model = 'model 2')

m3 <- reg_analysis(
  frml = "delta_w1_w4_GE ~ race + pseudo.gpa +
               sespc_al + nhood1_d + fbff_reciprocity", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0", F)  %>%
  mutate(model = 'model 3')

m4 <- reg_analysis(
  frml = "delta_w1_w4_GE ~ race + pseudo.gpa +
               sespc_al + nhood1_d + nominations", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0", F)  %>%
  mutate(model = 'model 4')

m5 <- reg_analysis(
  frml = "delta_w1_w4_GE ~ race + pseudo.gpa +
               sespc_al + nhood1_d + num_bff_noms", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0", F)  %>%
  mutate(model = 'model 5')

m6 <- reg_analysis(
  frml = "delta_w1_w4_GE ~ race + pseudo.gpa +
               sespc_al + nhood1_d + katz_centrality_R + 
               num_bff_noms + nominations +
               mbff_reciprocity + fbff_reciprocity", 
  df = subset(final.weighted, in_sample == 1), vcv = "HC0", F)  %>%
  mutate(model = 'model 6')

result.net <- rbind(m0, '', m1, '', m2, '',  m3, '',  m4, '',  m5, '', m6)

results <- rbind(s.results, '', m.results, '', a.results, '',  
                 r.results, '',  p.results, '', result.net)

results.ame <- rbind(s.5.ame, '', m.5.ame, '', 
                     a.5.ame, '',  r.5.ame, '',  p.5.ame)

write_csv(results, 'tables:figures/table2_regression_results.csv')
write_csv(results.ame, 'tables:figures/table2_marginaleffects_results.csv')


