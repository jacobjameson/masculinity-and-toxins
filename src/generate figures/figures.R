#-------------------------------------------------------------------------
# AUTHOR:             Jacob Jameson
# PURPOSE:            Generate figures
#-------------------------------------------------------------------------
#
# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "haven", "scales", "survey", "gridExtra",
          "gtsummary", "labelled", "broom", "ggpubr", "sandwich",
          "lmtest", "marginaleffects", "webshot2", "systemfit",
          "stargazer", "pander", "sjPlot", "pollster", "lavaan",
          "qwraps2", "arsenal", "ggsci", "ggeffects", "margins", 
          "ggplot2")


installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))


source('src/prepare data/Construct Analytical Dataset.R')
#-------------------------------------------------------------------------
# Figure 1 ---------------------------------------------------------------
final.weighted <- svydesign(id=~psuscid, strata=~region,
                            weights=~gswgt4_2, data=final.df, nest=TRUE)


s.1 <- svyglm(w4.cigarettes ~ w1.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

m.1 <- svyglm(w4.marijuana ~ w1.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

d.1 <- svyglm(w4.drunk ~ w1.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

r.1 <- svyglm(w4.recreational ~ w1.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

p.1 <- svyglm(w4.prescription ~ w1.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')


df.1 <- ggpredict(s.1, terms = c('w1.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Cigarette Smoking') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.2 <- ggpredict(m.1, terms = c('w1.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Marijuana Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.3 <- ggpredict(d.1, terms = c('w1.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Excessive Alcohol Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.4 <- ggpredict(r.1, terms = c('w1.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Recreational Drug Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.5 <- ggpredict(p.1, terms = c('w1.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Prescription Drug Misuse') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)


data <- rbind(df.1, df.2)
data <- rbind(data, df.3)
data <- rbind(data, df.4)
data <- rbind(data, df.5)


data <- data %>%
  mutate(group = 'Model 0: Adolescent (Wave I) GE')


s.1 <- svyglm(w4.cigarettes ~ w4.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.cigarettes, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

m.1 <- svyglm(w4.marijuana ~ w4.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.marijuana, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

d.1 <- svyglm(w4.drunk ~ w4.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.drunk, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

r.1 <- svyglm(w4.recreational ~ w4.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.recreational, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

p.1 <- svyglm(w4.prescription ~ w4.GE_male_std + race + pseudo.gpa +
                sespc_al + nhood1_d, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')


df.1 <- ggpredict(s.1, terms = c('w4.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Cigarette Smoking', 
         group ='Model 2: Young Adult (Wave IV) GE') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables, group)

df.2 <- ggpredict(m.1, terms = c('w4.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Marijuana Use', 
         group ='Model 2: Young Adult (Wave IV) GE') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables, group)

df.3 <- ggpredict(d.1, terms = c('w4.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Excessive Alcohol Use', 
         group ='Model 2: Young Adult (Wave IV) GE') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables, group)

df.4 <- ggpredict(r.1, terms = c('w4.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Recreational Drug Use', 
         group ='Model 2: Young Adult (Wave IV) GE') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables, group)

df.5 <- ggpredict(p.1, terms = c('w4.GE_male_std [-2:2]')) %>%
  mutate(Variables = 'Prescription Drug Misuse',  
         group ='Model 2: Young Adult (Wave IV) GE') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables, group)


data <- rbind(data, df.1)
data <- rbind(data, df.2)
data <- rbind(data, df.3)
data <- rbind(data, df.4)
data <- rbind(data, df.5)

data$Variables <- factor(data$Variables, 
                         levels = c("Excessive Alcohol Use", 'Marijuana Use',
                                    "Cigarette Smoking", 'Recreational Drug Use', 
                                    'Prescription Drug Misuse'))

ggplot(data, aes(x=xvals,fill=Variables)) +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.2) +
  geom_line(aes(y=coef)) + 
  theme_minimal() + theme(plot.caption = element_text(hjust = 0),
                          text = element_text(size = 20)) +
  facet_wrap(~group, nrow = 2) + geom_vline(xintercept =0, color='darkred') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits =c(0.1, 0.9)) +
  labs(fill = "Wave IV Behavior",
       y = "Predicted Probability\n", 
       x='\nStandardized Gender Expression\n',
       title = 'Marginal Effects of Gender Expression on Predicted Substance Use\n',
       caption = str_wrap('\n\nResults show that increases in adolescent GE 
                          and young adult GE are associated with greater predicted 
                          likelihood of young adult substance use. 
                          The effect of GE on substance use behaviors is stronger in young adults. 
                          Model 0 does not control for baseline substance use because of 
                          the associations between baseline substance use and adolescent GE', 140)) +
  theme(plot.caption = element_text(hjust = 0, size = 11))

ggsave("tables:figures/figure_1.png", width = 12, height = 10, bg = 'white')


# Figure 2 ---------------------------------------------------------------

s.5 <- svyglm(w4.cigarettes ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.cigarettes, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

m.5 <- svyglm(w4.marijuana ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.marijuana, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

d.5 <- svyglm(w4.drunk ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.drunk, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

r.5 <- svyglm(w4.recreational ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.recreational, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')


p.5 <- svyglm(w4.prescription ~ delta_w1_w4_GE*above_school_avg + race + pseudo.gpa +
                sespc_al + nhood1_d, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

df.1 <- ggpredict(s.5, terms = c('delta_w1_w4_GE [-2:2 by=0.1]', 'above_school_avg')) %>%
  mutate(Variables = 'Cigarette Smoking') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, group, Variables)

df.2 <- ggpredict(m.5, terms = c('delta_w1_w4_GE [-2:2 by=0.1]', 'above_school_avg')) %>%
  mutate(Variables = 'Marijuana Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, group, Variables)

df.3 <- ggpredict(d.5, terms = c('delta_w1_w4_GE [-2:2 by=0.1]', 'above_school_avg')) %>%
  mutate(Variables = 'Excessive Alcohol Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, group, Variables)

df.4 <- ggpredict(r.5, terms = c('delta_w1_w4_GE [-2:2 by=0.1]', 'above_school_avg')) %>%
  mutate(Variables = 'Recreational Drug Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, group, Variables)

df.5 <- ggpredict(p.5, terms = c('delta_w1_w4_GE [-2:2 by=0.1]', 'above_school_avg')) %>%
  mutate(Variables = 'Prescription Drug Misuse') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, group, Variables)

data <- rbind(df.1, df.2)
data <- rbind(data, df.3)
data <- rbind(data, df.4)
data <- rbind(data, df.5)

data$group = ifelse(data$group == 1, 'Above School GE Average', 'Below School GE Average')

ggplot(data, aes(x=xvals,fill=group)) +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.2) +
  geom_line(aes(y=coef))  +
  theme_minimal() + theme(text = element_text(size = 20)) + 
  scale_y_continuous(labels = scales::percent) + geom_vline(xintercept =0, color='darkred') +
  facet_wrap(~Variables) + 
  labs(x = "\nChange in Relative GE from Adolescence to Adulthood",
       y = "Predicted Probability of Behavior in Adulthood\n", 
       fill='Gender Expression in Adolescence',
       title = 'Mariginal Effects of Adolescent to Young Adult GE Change on Predicted Substance Use\n',
       caption = str_wrap('\n\n\n\n Results suggest that increases in relative GE from adolescence to adulthood 
                          are associated with greater predicted probability of young adult substance use 
                          behavior. Graphs depict the predicted probability of behavior in adulthood, 
                          split by whether or not the adolescent had a higher GE than their school average.
                          This was done to see if starting off with a higher GE than your peers modifies the effect of an increasing GE.
                          For mariijuana and recreational drug use, 
                          the impact of relative changes in GE are sharper for 
                          respondents who had greater GE than the average male of 
                          their school during adolescence.', 180)) +
  theme(legend.position="bottom", plot.caption = element_text(hjust = 0, size = 13))  

ggsave("tables:figures/figure_2.png", width = 16, height = 10, bg = 'white')

# Figure 3 ---------------------------------------------------------------

s.1 <- svyglm(w4.cigarettes ~ delta_w1_w4_GE + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.cigarettes, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

m.1 <- svyglm(w4.marijuana ~ delta_w1_w4_GE + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.marijuana, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

d.1 <- svyglm(w4.drunk ~ delta_w1_w4_GE + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.drunk, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

r.1 <- svyglm(w4.recreational ~ delta_w1_w4_GE + race + pseudo.gpa +
                sespc_al + nhood1_d + w1.recreational, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')

p.1 <- svyglm(w4.prescription ~ delta_w1_w4_GE + race + pseudo.gpa +
                sespc_al + nhood1_d, 
              design=subset(final.weighted, in_sample == 1), family = 'quasibinomial')


df.1 <- ggpredict(s.1, terms = c('delta_w1_w4_GE [-2:2]')) %>%
  mutate(Variables = 'Cigarette Smoking') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.2 <- ggpredict(m.1, terms = c('delta_w1_w4_GE [-2:2]')) %>%
  mutate(Variables = 'Marijuana Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.3 <- ggpredict(d.1, terms = c('delta_w1_w4_GE [-2:2]')) %>%
  mutate(Variables = 'Excessive Alcohol Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.4 <- ggpredict(r.1, terms = c('delta_w1_w4_GE [-2:2]')) %>%
  mutate(Variables = 'Recreational Drug Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.5 <- ggpredict(p.1, terms = c('delta_w1_w4_GE [-2:2]')) %>%
  mutate(Variables = 'Prescription Drug Misuse') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)


data <- rbind(df.1, df.2)
data <- rbind(data, df.3)
data <- rbind(data, df.4)
data <- rbind(data, df.5)

data <- data %>%
  mutate(group = 'Model 3: Chance in Adolescent to Adult GE')

library(ggsci)
ggplot(data, aes(x=xvals,fill=Variables)) +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.2) +
  geom_line(aes(y=coef)) + 
  theme_minimal() + theme(plot.caption = element_text(hjust = 0),
                          text = element_text(size = 18)) +
  geom_vline(xintercept =0, color='darkred') +
  annotate("text", x = -1, y = 0.8, color ='black',
           label = "GE Decreases from Adolescence \nto Young Adulthood") +
  annotate("text", x = 1, y = 0.8, color ='black',
           label = "GE Increases from Adolescence \nto Young Adulthood") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits =c(0.1, 0.9)) +
  labs(fill = "Wave IV Behavior",
       y = "Predicted Probability\n", 
       x='\nChange in Standardized Gender Expression \nfrom Adolescence to Young Adulthood\n',
       title = 'Marginal Effects of Change in Gender Expression on Predicted Substance Use\n',
       caption = str_wrap('\n\nResults show that increases in adolescent GE 
                          and young adult GE are associated with greater predicted 
                          likelihood of young adult use of marijuana. 
                          The effect of GE on substance use behaviors is stronger in young adults. 
                          Model 0 does not control for baseline substance use because of 
                          the associations between baseline substance use and adolescent GE', 150)) +
  theme(plot.caption = element_text(hjust = 0, size = 11))

ggsave("tables:figures/figure_3.png", width = 14, height = 10, bg = 'white')


