################################################################################
# AUTHOR:             JACOB JAMESON
# LAST UPDATED:       8/7/2022
# PURPOSE:            CONSTRUCT NETWORK VARIABLES
################################################################################
# Load packages necessary for preparing final data
library(tidyverse)
library(haven)
library(labelled)
library(reshape)
library(scales)
library(igraph)
library(centiserve)


data_path <- '~/Desktop/Add Health Substance Use Project/Data'
inschool_path <-  paste0(data_path, '/Wave I In-School Questionnaire Data')

#
inschool <- read_xpt(paste0(inschool_path, '/Inschool.xpt'))
inschool <- inschool[,c('AID', 'SQID', 'SSCHLCDE')]

inschool <- inschool %>%
  filter(SQID != '', AID != '')

#
friend.df <- read_xpt(paste0(data_path,
                    '/Wave I In-School Friendship Nominations/sfriend.xpt'))

friend.df <- friend.df %>%
  filter(SQID != '999999')


#
friend.df <- merge(friend.df, inschool, by='SQID')
friend.df[] <- lapply(friend.df, as.character)

# Rename variables lowercase
names(friend.df) <- tolower(names(friend.df))

# Clear environment
rm(list=setdiff(ls(), 'friend.df'))

df <-  merge(friend.df, wave.1, on='aid')
################################################################################
# CREATE VARIABLES THAT WILL BE USED IN ANALYSIS:
#
#   - num_friend_noms: Number of friendship nominations
#   - num_bff_noms: Number of BFF friendship nominations
#   - fbff_reciprocity: Female BFF reciprocity
#   - mbff_reciprocity: Male BFF reciprocity
#   - katz_centrality: Katz centrality score

################################################################################


# Replace friendship nominations that are 77777777, 99999999, 88888888, 99959995
# with missing nomination. EXPLAIN REASONING

friend.vars <- c('mf1aid', 'mf2aid', 'mf3aid', 'mf4aid', 'mf5aid', 'ff1aid',
                 'ff2aid', 'ff3aid', 'ff4aid', 'ff5aid')

id.replace <- c('77777777', '99999999', '88888888', '99959995')


friend.df <- friend.df %>%
  mutate(mf1aid = ifelse(mf1aid %in% id.replace, NA, mf1aid),
         mf2aid = ifelse(mf2aid %in% id.replace, NA, mf2aid),
         mf3aid = ifelse(mf3aid %in% id.replace, NA, mf3aid),
         mf4aid = ifelse(mf4aid %in% id.replace, NA, mf4aid),
         mf5aid = ifelse(mf5aid %in% id.replace, NA, mf5aid),
         ff1aid = ifelse(ff1aid %in% id.replace, NA, ff1aid),
         ff2aid = ifelse(ff2aid %in% id.replace, NA, ff2aid),
         ff3aid = ifelse(ff3aid %in% id.replace, NA, ff3aid),
         ff4aid = ifelse(ff4aid %in% id.replace, NA, ff4aid),
         ff5aid = ifelse(ff5aid %in% id.replace, NA, ff5aid))

network <- friend.df
################################################################################
# Number of BFF nominations dataframe

bff <- data.frame()
for (school in unique(friend.df$sschlcde)){

  friend.df.2 <- friend.df[friend.df$sschlcde == school,]

  bff_noms <- data.frame(list('aid' = c(friend.df.2$mf1aid, friend.df.2$ff1aid)))

  bff_noms <- filter(bff_noms, is.na(aid) == F)
  bff_noms <- merge(bff_noms, friend.df[, c('aid', 'sschlcde')], by='aid')
  bff_noms <- filter(bff_noms, sschlcde == school)

  bff_noms <- bff_noms %>%
    group_by(aid) %>% summarize(num_bff_noms = n())

  bff <- rbind(bff, bff_noms)
}



rm(list=setdiff(ls(), c('bff', 'friend.df')))

################################################################################
# Number of friend nominations dataframe

friend <- data.frame()
for (school in unique(friend.df$sschlcde)){

  friend.df.2 <- friend.df[friend.df$sschlcde == school,]

  friend_noms <- data.frame(list('aid' = c(friend.df.2$mf1aid, friend.df.2$mf2aid,
                                           friend.df.2$mf3aid, friend.df.2$mf4aid,
                                           friend.df.2$mf5aid, friend.df.2$ff1aid,
                                           friend.df.2$ff2aid, friend.df.2$ff3aid,
                                           friend.df.2$ff4aid, friend.df.2$ff5aid)))

  friend_noms <- filter(friend_noms, is.na(aid) == F)
  friend_noms <- merge(friend_noms, friend.df[, c('aid', 'sschlcde')], by='aid')
  friend_noms <- filter(friend_noms, sschlcde == school)

  friend_noms <- friend_noms %>%
    group_by(aid) %>% summarize(num_friend_noms = n())

  friend <- rbind(friend, friend_noms)
}


rm(list=setdiff(ls(), c('friend', 'bff', 'friend.df')))
################################################################################
# Determine best friend reciprocity
rec <- data.frame()
for (school in unique(friend.df$sschlcde)){

  friend.df.2 <- friend.df[friend.df$sschlcde == school,]

  male_bff_df <- friend.df.2[, c('aid', 'mf1aid')] %>% filter(is.na(mf1aid) == F)
  female_bff_df <- friend.df.2[, c('aid', 'ff1aid')] %>% filter(is.na(ff1aid) == F)

  bff_df <- merge(male_bff_df, female_bff_df, by='aid', all= TRUE )

  mbff_list <- c(bff_df$mf1aid)
  mbff_list <- mbff_list[!is.na(mbff_list)]

  mbff_df <- bff_df[bff_df$aid %in% mbff_list,] %>%
    select(mf1aid = aid, mf1aid_mf1aid = mf1aid, mf1aid_ff1aid = ff1aid)

  fbff_list <- c(bff_df$ff1aid)
  fbff_list <- fbff_list[!is.na(fbff_list)]

  fbff_df <- bff_df[bff_df$aid %in% fbff_list,] %>%
    select(ff1aid = aid, ff1aid_mf1aid = mf1aid, ff1aid_ff1aid = ff1aid)

  bff_df <- merge(bff_df, fbff_df, by='ff1aid', all=T)
  bff_df <- merge(bff_df, mbff_df, by='mf1aid', all=T)

  bff_df <- bff_df %>%
    mutate(mbff_reciprocity = case_when(mf1aid_mf1aid == aid ~ 1,
                                        mf1aid_ff1aid == aid ~ 1,
                                        TRUE ~ 0),
           fbff_reciprocity = case_when(ff1aid_mf1aid == aid ~ 1,
                                        ff1aid_ff1aid == aid ~ 1,
                                        TRUE ~ 0)) %>%
    select(aid, mbff_reciprocity, fbff_reciprocity)


  rec <- rbind(rec, bff_df)
}


rm(list=setdiff(ls(), c('rec', 'friend', 'bff', 'friend.df')))
################################################################################
friend.df <- merge(friend, friend.df, by='aid', all=T)
friend.df <- merge(rec, friend.df, by='aid', all=T)
friend.df <- merge(bff, friend.df, by='aid', all=T)

final <- friend.df[,c('aid', 'sqid', 'num_bff_noms', 'num_friend_noms',
                          'mbff_reciprocity', 'fbff_reciprocity')]

friend.df <- friend.df[,c('aid', 'sschlcde', 'mf1aid', 'mf2aid', 'mf3aid', 'mf4aid',
                          'mf5aid', 'ff1aid','ff2aid', 'ff3aid', 'ff4aid',
                          'ff5aid')]

final <- final %>%
  mutate(num_bff_noms = ifelse(is.na(num_bff_noms) == T, 0, num_bff_noms),
         num_friend_noms = ifelse(is.na(num_friend_noms) == T, 0, num_friend_noms),
         mbff_reciprocity = ifelse(is.na(mbff_reciprocity) == T, 0, mbff_reciprocity),
         fbff_reciprocity = ifelse(is.na(fbff_reciprocity) == T, 0, fbff_reciprocity))


################################################################################


friend_long <- melt(friend.df, id=c("aid","sschlcde"))

friend_long <-  friend_long[,c('aid', 'sschlcde', 'value')] %>%
  select(aid, sschlcde, friend=value)

friend_long <- friend_long[is.na(friend_long$friend) == F, ]

katz.centrality = function(g, alpha, beta, t) {
  n = vcount(g);
  A = get.adjacency(g);
  x0 = rep(0, n);
  x1 = rep(1/n, n);
  eps = 1/10^t;
  iter = 0;
  while (sum(abs(x0 - x1)) > eps) {
    x0 = x1;
    x1 = as.vector(alpha * x1 %*% A) + beta;
    iter = iter + 1;
  }
  return(list(aid = x0, vector = x1, iter = iter))
}


data_path <- '~/Desktop/Add Health Substance Use Project/Data'
inschool_path <-  paste0(data_path, '/Wave I In-School Questionnaire Data')

#
inschool <- read_xpt(paste0(inschool_path, '/Inschool.xpt'))
inschool <- inschool[,c('AID', 'SQID', 'SSCHLCDE')]

inschool <- inschool %>%
  filter(SQID != '', AID != '') %>%
  select(friend = AID,  sschlcde.f = SSCHLCDE)

friend_long <- merge(friend_long, inschool, by='friend', all = T)
friend_long <- friend_long[is.na(friend_long$aid) == F, ]


netx <- data.frame()
for (school in unique(friend_long$sschlcde)){

  sample <- filter(friend_long, sschlcde == school & sschlcde.f == school)

  net <- graph_from_data_frame(sample[,c('aid','friend')],
                               directed = TRUE, vertices = NULL)
  A = get.adjacency(net)
  k = katz.centrality(net, 0.1, 1, 0.001)$vector
  aids <- A@Dimnames[[1]]

  netx <- rbind(netx, data.frame(list('aid' = aids, 'katz_centrality_R' = k)))
}

friend.df <- merge(netx, final, by='aid')
################################################################################
# Save data
write_csv(friend.df, '~/Desktop/Add Health Substance Use Project/Data/network.csv')
################################################################################
