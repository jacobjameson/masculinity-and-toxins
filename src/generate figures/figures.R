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
          "ggplot2", 'MASS', 'lfe', 'sjstats', 'ggthemes')


installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))


source('src/prepare data/Construct Analytical Dataset.R')

#-------------------------------------------------------------------------
# Model 1 + 2 ------------------------------------------------------------
s.1 <- glm(w4.cigarettes.bin.30 ~ w4.GE_male_std + race + pseudo.gpa +
      sespc_al + nhood1_d + w1.cigarettes, 
    data=subset(final.df, in_sample == 1),
    weights = weights, family = 'quasibinomial')

m.1 <- glm(w4.marijuana.bin.30 ~ w4.GE_male_std + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.marijuana, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

d.1 <- glm(w4.drunk.bin.30 ~ w4.GE_male_std + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.drunk, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

r.1 <- glm(w4.fav.bin.30 ~ w4.GE_male_std + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.recreational, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

p.1 <- glm(w4.prescription ~ w4.GE_male_std + race + pseudo.gpa +
             sespc_al + nhood1_d, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

df.1 <- ggpredict(s.1, terms = c('w4.GE_male_std [-2:2]'),
          vcov.type = "HC0", 
          vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Cigarette Smoking') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.2 <- ggpredict(m.1, terms = c('w4.GE_male_std [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Marijuana Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.3 <- ggpredict(d.1, terms = c('w4.GE_male_std [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Excessive Alcohol Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.4 <- ggpredict(r.1, terms = c('w4.GE_male_std [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Recreational Drug Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.5 <- ggpredict(p.1, terms = c('w4.GE_male_std [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Prescription Drug Misuse') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)


data <- rbind(df.1, df.2)
data <- rbind(data, df.3)
data <- rbind(data, df.4)
data <- rbind(data, df.5)

data <- data %>%
  mutate(group = 'Model 2: Young Adult (Wave IV) GE')

s.2 <- glm(w4.cigarettes.bin.30 ~ w1.GE_male_std + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.cigarettes, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

m.2 <- glm(w4.marijuana.bin.30 ~ w1.GE_male_std + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.marijuana, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

d.2 <- glm(w4.drunk.bin.30 ~ w1.GE_male_std + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.drunk, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

r.2 <- glm(w4.fav.bin.30 ~ w1.GE_male_std + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.recreational, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

p.2 <- glm(w4.prescription ~ w1.GE_male_std + race + pseudo.gpa +
             sespc_al + nhood1_d, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

df.2 <- ggpredict(s.2, terms = c('w1.GE_male_std [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Cigarette Smoking') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.2 <- ggpredict(m.2, terms = c('w1.GE_male_std [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Marijuana Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.3 <- ggpredict(d.2, terms = c('w1.GE_male_std [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Excessive Alcohol Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.4 <- ggpredict(r.2, terms = c('w1.GE_male_std [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Recreational Drug Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.5 <- ggpredict(p.2, terms = c('w1.GE_male_std [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Prescription Drug Misuse') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)


data.2 <- rbind(df.1, df.2)
data.2 <- rbind(data.2, df.3)
data.2 <- rbind(data.2, df.4)
data.2 <- rbind(data.2, df.5)

data.2 <- data.2 %>%
  mutate(group = 'Model I: Adolsecent (Wave I) GE')

data <- rbind(data.2, data)

data$Variables <- factor(data$Variables, 
                         levels = c("Excessive Alcohol Use", 
                                    "Cigarette Smoking",
                                    'Prescription Drug Misuse',
                                    'Marijuana Use',
                                    'Recreational Drug Use'))

data$group <- factor(data$group, 
                         levels = c('Model I: Adolsecent (Wave I) GE',
                                    'Model 2: Young Adult (Wave IV) GE'))

ggplot(data, aes(x=xvals,fill=Variables)) +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.2) +
  geom_line(aes(y=coef)) + 
  scale_fill_d3() +
  facet_wrap(~group, ncol = 2) +
  theme_minimal() + theme(plot.caption = element_text(hjust = 0),
                          text = element_text(size = 20)) +
   geom_vline(xintercept =0, color='darkred') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits =c(0, 0.9)) +
  labs(fill = "Wave IV Behavior",
       y = "Predicted Probability\n", 
       x='\nStandardized Gender Expression\n',
       title = 'Marginal Effects of Gender Expression on Predicted Substance Use\n',
       caption = str_wrap('\n\nResults show that increases in adolescent and young adult GE are associated with greater predicted 
                          probability of young adult substance use. Predicted probabilities are calculated from the logit results of Model 1 and Model 2.', 140)) +
  theme(plot.caption = element_text(hjust = 0, size = 13))

ggsave("tables:figures/Model1+2_margins.png", width = 14, height = 10, bg = 'white')

# Model 3 ---------------------------------------------------------------

s.1 <- glm(w4.cigarettes.bin.30 ~ delta_w1_w4_GE + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.cigarettes, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

m.1 <- glm(w4.marijuana.bin.30 ~ delta_w1_w4_GE + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.marijuana, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

d.1 <- glm(w4.drunk.bin.30 ~ delta_w1_w4_GE + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.drunk, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

r.1 <- glm(w4.fav.bin.30 ~ delta_w1_w4_GE + race + pseudo.gpa +
             sespc_al + nhood1_d + w1.recreational, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

p.1 <- glm(w4.prescription ~ delta_w1_w4_GE + race + pseudo.gpa +
             sespc_al + nhood1_d, 
           data=subset(final.df, in_sample == 1),
           weights = weights, family = 'quasibinomial')

df.1 <- ggpredict(s.1, terms = c('delta_w1_w4_GE [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Cigarette Smoking') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.2 <- ggpredict(m.1, terms = c('delta_w1_w4_GE [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Marijuana Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.3 <- ggpredict(d.1, terms = c('delta_w1_w4_GE [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Excessive Alcohol Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.4 <- ggpredict(r.1, terms = c('delta_w1_w4_GE [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Recreational Drug Use') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

df.5 <- ggpredict(p.1, terms = c('delta_w1_w4_GE [-2:2]'),
                  vcov.type = "HC0", 
                  vcov.args = list(cluster = subset(final.df, in_sample == 1)$cluster))  %>%
  mutate(Variables = 'Prescription Drug Misuse') %>%
  select(xvals=x, coef=predicted, lower=conf.low, upper=conf.high, Variables)

data <- rbind(df.1, df.2)
data <- rbind(data, df.3)
data <- rbind(data, df.4)
data <- rbind(data, df.5)

data <- data %>%
  mutate(group = 'Model 3: Chance in Adolescent to Adult GE')

ggplot(data, aes(x=xvals,fill=Variables)) +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.2) +
  geom_line(aes(y=coef)) + 
  theme_minimal() + theme(plot.caption = element_text(hjust = 0),
                          text = element_text(size = 18)) +
  geom_vline(xintercept =0, color='darkred') +
  annotate("text", x = -1, y = 0.85, color ='black', fontface =2,
           label = "GE Decreases from Adolescence \nto Young Adulthood") +
  annotate("text", x = 1, y = 0.85, color ='black', fontface =2,
           label = "GE Increases from Adolescence \nto Young Adulthood") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits =c(0.02, 0.85)) +
  labs(fill = "Wave IV Behavior",
       y = "Predicted Probability\n", 
       x='\nChange in Standardized Gender Expression \nfrom Adolescence to Young Adulthood\n',
       title = 'Marginal Effects of Change in Gender Expression on Predicted Substance Use\n',
       caption = str_wrap('\n\nResults show that changes in adolescent to young adult GE 
                          are associated with greater predicted 
                          probability of young adult substance use.', 150)) +
  theme(plot.caption = element_text(hjust = 0, size = 13))

ggsave("tables:figures/delta_margins.png", width = 14, height = 10, bg = 'white')



# IRR ---------------------------------------------------------------

nbregs <-  read_csv('tables:figures/Negative Binomial IRRs 1,2,3.csv')

nbregs %>%
  mutate(vars = factor(case_when(
    term == 'w1.GE_male_std' ~ 'Wave I GE Measure',
    term == 'w4.GE_male_std' ~ 'Wave IV GE Measure',
    term == 'delta_w1_w4_GE' ~ 'Change in Wave I to Wave IV GE'),
    levels = c('Change in Wave I to Wave IV GE', 'Wave IV GE Measure', 'Wave I GE Measure')),
         outcomes = factor(case_when(
           outcome == 'w4.cigarettes.cont.30' ~ 'Wave IV 30 Day Cigarette Use',
           outcome == 'w4.marijuana.cont.30' ~ 'Wave IV 30 Day Marijuana Use',
           outcome == 'w4.drunk.cont.30' ~ 'Wave IV 30 Day Alcohol Use',
           outcome == 'w4.fav.cont.30' ~ 'Wave IV 30 Day Recreational Drug Use'),
    levels = c('Wave IV 30 Day Cigarette Use', 'Wave IV 30 Day Marijuana Use',
               'Wave IV 30 Day Alcohol Use', 'Wave IV 30 Day Recreational Drug Use'))) %>%
  mutate(zero = ifelse(lwr<1 & upr>1, "p-value > 0.05", "p-value < 0.05" )) %>% 
  ggplot() + 
  geom_linerange(mapping = aes(x = vars, ymin = lwr, ymax = upr), size = 0.75) +
  geom_point(mapping = aes(x = vars, y = estimate, color = zero), size = 3.5) +
  geom_hline(mapping = aes(yintercept = 1), linetype = "dashed", color = 'darkred') + 
  facet_wrap(~outcomes, nrow=4) + coord_flip() + 
  labs(x = "Gender Expression Variables\n", 
       y = "\nIncidence Rate Ratio from Negative Binomial Model", 
       color = "Significance") + 
  scale_color_d3() + theme_minimal() + theme_bw() +
  theme(strip.text.x = element_text(face ="bold", size =11),
        axis.text = element_text(colour = "black"),
        legend.position = 'bottom',
        axis.title = element_text(size = 16),
        legend.title = element_text (colour ="black", size =10 ,
                                     face ="bold"),
        legend.background = element_rect (size =0.5 , linetype ="solid",
                                          colour ="black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) 


ggsave("tables:figures/IRR.png", width = 10, height = 10, bg = 'white')

