#-------------------------------------------------------------------------
# AUTHOR:             Jacob Jameson
# PURPOSE:            Construct W1 to W4 Analytical Dataset
#-------------------------------------------------------------------------
#
# load packages ----------------------------------------------------------
rm(list = ls())

libs <- c("tidyverse", "haven", "labelled", 'scales')

installed_libs <- libs %in% rownames (installed.packages ())
if (any (installed_libs == F)) {
  install.packages (libs[!installed_libs])
}

invisible(lapply (libs, library, character.only = T))


# Create and prepare network, wave 1, and wave 4 data --------------------

#source('src/prepare data/Prepare Network.R')
source('src/prepare data/Prepare W1.R')
source('src/prepare data/Prepare W4.R')

# Get SES data  ----------------------------------------------------------
ses_path <- '~/Add Health Data/Constructed SES Variables'
ses.data <- read_xpt(paste0(ses_path, '/conses4.xpt'))
names(ses.data) <- tolower(names(ses.data))


# Merge wave 1, wave 4, and SES data together ----------------------------
final.df <- merge(wave.1, 
                  wave.4, 
                  by=c('aid', 'psuscid', 'region'), 
                  all=TRUE)

final.df <- merge(final.df, 
                  ses.data,
                  by='aid')


# Clear environment
rm(list=setdiff(ls(), 'final.df'))

#-------------------------------------------------------------------------
# Create variables that will be used in the analysis
#
#   - school_self: Where does the adolescent GE start relative to school
#                  and what is their trajectory going into adulthood?
#-------------------------------------------------------------------------

final.df <- final.df %>%
  mutate(
    above_school_avg = case_when(
      w1.GE_male >= school_avg_GE ~ 1,
      w1.GE_male < school_avg_GE ~ 0,
      TRUE ~ NaN
    ),
    delta_w1_w4_GE = w4.GE_male_std - w1.GE_male_std,
    increasing = ifelse(delta_w1_w4_GE > 0, 1, 0),
    school_self = case_when(
      above_school_avg == 1 & delta_w1_w4_GE > 0 ~ 1,
      above_school_avg == 1 & delta_w1_w4_GE < 0 ~ 2,
      above_school_avg == 0 & delta_w1_w4_GE > 0 ~ 3,
      above_school_avg == 0 & delta_w1_w4_GE < 0 ~ 4
    ),
    self_self = case_when(
      w1.GE_male_std >= 0 & delta_w1_w4_GE > 0 ~ 1,
      w1.GE_male_std >= 0 & delta_w1_w4_GE < 0 ~ 2,
      w1.GE_male_std < 0 & delta_w1_w4_GE > 0 ~ 3,
      w1.GE_male_std < 0 & delta_w1_w4_GE < 0 ~ 4
    )
  )

# Rename factor levels for school_self and self_self variables
final.df$school_self <- factor(
  final.df$school_self,
  levels = c(1, 2, 3, 4),
  labels = c(
    "Above School Avg and Increasing",
    "Above School Avg and Decreasing",
    "Below School Avg and Increasing",
    "Below School Avg and Decreasing"
  )
)

final.df$self_self <- factor(
  final.df$self_self,
  levels = c(1, 2, 3, 4),
  labels = c(
    "Above Adolescent Male Avg and Increasing",
    "Above Adolescent Male Avg and Decreasing",
    "Below Adolescent Male Avg and Increasing",
    "Below Adolescent Male Avg and Decreasing"
  )
)


## Outcomes and controls for substance use ---------------------------------
final.df <- final.df %>%
  mutate(
    w1.cigarettes = case_when(h1to5 > 0 & h1to5 < 96 ~ 1,
                              h1to5 == 0 | h1to5 == 97 ~ 0,
                              TRUE ~ NA),
    w1.marijuana = case_when(h1to30 > 0 & h1to30 < 96 ~ 1,
                             h1to30 == 0 | h1to30 == 98 ~ 0,
                             TRUE ~ NA),
    w1.recreational = case_when(h1to34 > 0 & h1to34 < 96 ~ 1,
                                h1to38 > 0 & h1to38 < 996 ~ 1,
                                h1to41 > 0 & h1to41 < 996 ~ 1,
                                h1to43 == 1 ~ 1,
                                (h1to34 == 0 & h1to38 == 997 &
                                   h1to41 == 997 &
                                   (h1to43 == 0 | h1to43 == 7)) ~ 0,
                                TRUE ~ NA),
    w1.drunk = case_when(h1to18 < 7 & h1to18 >= 1 ~ 1,
                         h1to18 == 7 | h1to18 == 97 ~ 0,
                         TRUE ~ NA),
    w4.recreational = case_when(h4to65a == 1 ~ 1,
                                h4to65c == 1 ~ 1,
                                h4to65d == 1 ~ 1,
                                h4to65e == 1 ~ 1,
                                h4to66 == 1 ~ 1,
                                (h4to65a == 0 & h4to65c == 0 &
                                   h4to65d == 0 & h4to65e == 0 &
                                   h4to66 == 0) ~ 0,
                                TRUE ~ 0),
    w4.marijuana = case_when(h4to65b == 1 ~ 1,
                             h4to65b == 0 ~ 0,
                             TRUE ~ NA),
    w4.drunk = case_when(h4to38 > 0 & h4to38 < 96 ~ 1,
                         h4to38 == 0 | h4to38 == 97 ~ 0,
                         TRUE ~ NA),
    w4.cigarettes = case_when(h4to3 == 1 ~ 1,
                              h4to3 == 0 | h4to3 == 7 ~ 0,
                              TRUE ~ NA),
    w4.prescription = case_when(h4to63 == 1 ~ 1,
                                h4to63 == 0 ~ 0,
                                TRUE ~ NA),
    w4.cigarettes.cont.30 = case_when(h4to5 < 96 ~ h4to5,
                                      h4to5 == 97 ~ 0,
                                      TRUE ~ NA),
    w4.cigarettes.bin.30 = case_when(w4.cigarettes.cont.30 > 0 ~ 1,
                                     w4.cigarettes.cont.30 == 0 ~ 0),
    w4.drunk.cont.year = case_when(h4to38 == 97 ~ 0,
                                 h4to38 >= 96 ~ NA,
                                 TRUE ~ h4to38),
    w4.drunk.bin.year = case_when(w4.drunk.cont.year == 0 ~ 0,
                                   w4.drunk.cont.year >= 1 ~ 1),
    w4.drunk.cont.30 = case_when(h4to39 == 97 ~ 0,
                                   h4to39 >= 96 ~ NA,
                                 TRUE ~ h4to39),
    w4.drunk.bin.30 = case_when(w4.drunk.cont.30 == 0 ~ 0,
                                w4.drunk.cont.30 >= 1 ~ 1),
    w4.marijuana.cont.year = case_when(h4to70 == 97 ~ 0,
                                   h4to70 >= 96 ~ NA,
                                   TRUE ~ h4to70),
    w4.marijuana.cont.30 = case_when(h4to71 == 97 ~ 0,
                                 h4to71 >= 96 ~ NA,
                                 TRUE ~ h4to71),
    w4.marijuana.bin.year = case_when(w4.marijuana.cont.year == 0 ~ 0,
                                      w4.marijuana.cont.year >= 1 ~ 1),
    w4.marijuana.bin.30 = case_when(w4.marijuana.cont.30 == 0 ~ 0,
                                    w4.marijuana.cont.30 >= 1 ~ 1),
    w4.fav.cont.year = case_when(h4to98 == 97 ~ 0,
                                       h4to98 >= 96 ~ NA,
                                 TRUE ~ h4to98),
    w4.fav.cont.30 = case_when(h4to99 == 97 ~ 0,
                                     h4to99 >= 96 ~ NA,
                               TRUE ~ h4to99),
    w4.fav.bin.year = case_when(w4.fav.cont.year == 0 ~ 0,
                                w4.fav.cont.year >= 1 ~ 1),
    w4.fav.bin.30 = case_when(w4.fav.cont.30 == 0 ~ 0,
                              w4.fav.cont.30 >= 1 ~ 1))

# Define variables to keep -----------------------------------------------
vars.keep <- c('w1.cigarettes', 'w1.marijuana', 'w1.recreational',
               'w1.drunk', 'w4.recreational', 'w4.marijuana',
               'w4.drunk', 'w4.cigarettes', 'w4.prescription',
               'aid', 'region', 'psuscid', 'gswgt4_2', 'sschlcde',
               'w1_male', 'self_self', 'school_self', 'increasing',
               'delta_w1_w4_GE', 'w1.GE_male', 'w4.GE_male',
               'school_avg_GE', 'w1.GE_male_std', 'w4.GE_male_std',
               'sespc_al', 'nhood1_d', 'pseudo.gpa', 'race', 'edu', 'insurance',
               'w4_male', 'above_school_avg', 'w1.GE_male_std_school',
               'w4.cigarettes.bin.30', 'w4.cigarettes.cont.30',
               'w4.drunk.cont.year', 'w4.drunk.bin.30', 'w4.drunk.bin.year',
               'w4.drunk.cont.30', 'w4.marijuana.cont.year', 
               'w4.marijuana.cont.30', 'w4.fav.cont.year', 
               'w4.fav.cont.30', 'w4.fav.bin.year', 'w4.fav.bin.30',
               'w4.marijuana.bin.year', 'w4.marijuana.bin.30')

# Select only the variables to keep and filter out any missing weight values
final.df <- final.df[, vars.keep]
final.df <- filter(final.df, is.na(gswgt4_2) == FALSE)

nrow(final.df[is.na(final.df$w1.GE_male) == FALSE & final.df$w1_male == 1,])

# Create a binary variable indicating whether a respondent is in the sample
final.df <- final.df %>%
  mutate(in_sample = ifelse(w1_male == w4_male & w1_male == 1, 1, 0),
         in_sample = ifelse(is.na(w1.GE_male) == FALSE & is.na(w4.GE_male) == FALSE, 
                            in_sample, 0))

#create cluster var
final.df$cluster <- paste(final.df$region,final.df$psuscid)
final.df$weights <- final.df$gswgt4_2 / mean(final.df$gswgt4_2)

#-------------------------------------------------------------------------
network <- read_csv('data/network.csv')
final.df <- merge(final.df, network, on='aid')

