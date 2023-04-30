#-------------------------------------------------------------------------
# AUTHOR:             Jacob Jameson
# PURPOSE:            Generate main regression results
#-------------------------------------------------------------------------
#
# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "haven", 'scales', 'survey', 'gridExtra',
          'gtsummary', 'labelled', 'broom', 'ggpubr', 'sandwich',
          'lmtest', 'webshot2', 'kableExtra', 'sjstats')

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))

source('src/prepare data/Construct Analytical Dataset.R')
#----------------------------------------------------------------------------------
# logit analysis of 3 models ------------------------------------------------------

### Create function for regression analysis
logit_analysis <- function(frml, df, j){
  
  frml1 <- as.formula(frml)
  m0 <- glm(formula = frml1, data = df, weights = weights, family = "binomial")
  cluster <- df$cluster 
  robust <- coeftest(x = m0, vcov = vcovCL(m0, type = "HC0", cluster = ~ cluster))
  ci     <- confint(robust)
  
  robust %>% broom::tidy() %>% as.data.frame() %>%
    mutate(outcome = str_squish(word(frml,1,sep = "\\~")),
           lwr = as.data.frame(ci)[,1],
           upr = as.data.frame(ci)[,2]) %>%
    select(outcome,term:p.value,lwr,upr) %>%
    mutate(model = paste('Model ', j)) -> robust_df
  
  return(robust_df)
}

# Create a vector of outcome variables
outcomes <- list(c("w4.cigarettes.bin.30",'w1.cigarettes'), 
                 c("w4.marijuana.bin.30",'w1.marijuana'), 
                 c("w4.drunk.bin.30", 'w1.drunk'),
                 c("w4.fav.bin.30",'w1.recreational'),
                 c("w4.prescription", ""))

# Create a vector of predictor variables
predictors <- c("w1.GE_male_std", "w4.GE_male_std", "delta_w1_w4_GE")

suppressWarnings({
# Loop through each combination of outcome and predictor variables
for (i in seq_along(outcomes)) {
  for (j in seq_along(predictors)) {
    # Generate a unique name for the model based on the outcome and predictor variables
    model_name <- paste0('logit', outcomes[i][[1]][1], j)
    
    # Fit the model and save it in a unique object with the generated name
    if (outcomes[i][[1]][2] != ''){
    assign(model_name, logit_analysis(
      frml = paste(outcomes[i][[1]][1], 
                   "~ race + pseudo.gpa + sespc_al + nhood1_d +", 
                   predictors[j],'+', outcomes[i][[1]][2]),
      df = subset(final.df, in_sample == 1), j))
    }
    else{
      assign(model_name, logit_analysis(
        frml = paste(outcomes[i][[1]][1], 
                     "~ race + pseudo.gpa + sespc_al + nhood1_d +", 
                     predictors[j]),
        df = subset(final.df, in_sample == 1), j))
    }
  }
}}
)

logit.models <- ls()[sapply(ls(), function(x) is.data.frame(get(x))) & grepl("^logit", ls())]
logit.models <- do.call(rbind, mget(logit.models))[-1,]

write_csv(logit.models, 'tables:figures/Logit Models 1,2,3.csv')

# Negative Binomial Models ----------------------------------------------------------

### Create function for nbreg regression analysis

nbreg_analysis <- function(frml, df, j){
  
  frml1 <- as.formula(frml)
  m0 <- glm.nb(formula = frml1, data = df, weights = weights)
  cluster <- df$cluster 
  robust <- coeftest(x = m0, vcov = vcovCL(m0, type = "HC0", cluster = ~ cluster))
  ci     <- confint(robust)
  
  robust %>% tidy() %>% as.data.frame() %>%
    mutate(outcome = str_squish(word(frml,1,sep = "\\~")),
           lwr = as.data.frame(ci)[,1],
           upr = as.data.frame(ci)[,2]) %>%
    select(outcome,term:p.value,lwr,upr) %>%
    filter(term %in% c("w1.GE_male_std", 
                       "w4.GE_male_std", 
                       "delta_w1_w4_GE")) %>%
    mutate(model = paste('Model ', j)) -> robust_df
  
  # Get IRR
  robust_df$estimate <- exp(robust_df$estimate)
  robust_df$std.error <- exp(robust_df$std.error)
  robust_df$lwr <- exp(robust_df$lwr)
  robust_df$upr <- exp(robust_df$upr)
  
  return(robust_df)
}


# Create a vector of outcome variables
outcomes <- list(c("w4.cigarettes.cont.30",'w1.cigarettes'), 
                 c("w4.marijuana.cont.30",'w1.marijuana'), 
                 c("w4.drunk.cont.30", 'w1.drunk'),
                 c("w4.fav.cont.30",'w1.recreational'))

# Create a vector of predictor variables
predictors <- c("w1.GE_male_std", "w4.GE_male_std", "delta_w1_w4_GE")

suppressWarnings({
# Loop through each combination of outcome and predictor variables
for (i in seq_along(outcomes)) {
  for (j in seq_along(predictors)) {
    # Generate a unique name for the model based on the outcome and predictor variables
    model_name <- paste0('nbreg', outcomes[i][[1]][1], j)
    # Fit the model and save it in a unique object with the generated name
    assign(model_name, nbreg_analysis(
      frml = paste(outcomes[i][[1]][1], 
                   "~ race + pseudo.gpa + sespc_al + nhood1_d +", 
                   predictors[j],'+', outcomes[i][[1]][2]),
      df = subset(final.df, in_sample == 1), j))
  }
}
})

nbreg.model <- ls()[sapply(ls(), function(x) is.data.frame(get(x))) & grepl("^nbreg", ls())]
nbreg.model <- do.call(rbind, mget(nbreg.model))[-1,]

write_csv(nbreg.model, 'tables:figures/Negative Binomial IRRs 1,2,3.csv')

view(logit.models)
