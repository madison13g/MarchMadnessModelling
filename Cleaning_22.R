# Cleaning 2021-2022 Data

# Joining Dataframes

# Dataframes here:

### NCAA-Statistics -> Conference, Net Rankings, Quadrants, etc.
### field-goal-pct-opponents -> Rank, OPP FG, OPP FGA,	OPP FG%
### field-goal-pct -> Rank, FGM, FGA, FG%
### Free-throws -> Rank, FT, FTA, Avg
### Three-FG-Perc -> Rank, GM, 3FG, 3FGA, 3FG%
### Scoring-Margin -> Rank, PTS, PPG, OPP PTS, OPP PPG, SCR, MAR
### Rebounds-D-and-O -> Rank, ORebs, DRebs, REB, RPG
### Assist-Turnover-Ratio -> Rank, AST, TO, Ratio
### bracket-rankings -> Bracket Ranking, Region

## Loading Dataframes

library("readxl")
ncaa_stats_22 <- read_excel("Data22\\NCAA-Statistics-22.xlsx")
fg_opp_22 <- read_excel("data22\\field-goal-pct-opponents-22.xlsx")
fg_22 <- read_excel("data22\\field-goal-pct-22.xlsx")
free_22 <- read_excel("data22\\free-throws-22.xlsx")
threes_22 <- read_excel("data22\\Three-FG-Perc-22.xlsx")
scoring_margin_22 <- read_excel("data22\\Scoring-Margin-22.xlsx")
rebounds_22 <- read.csv("data22\\Rebounds-D-and-O-22.csv")
assist_turnover_22 <- read_excel("data22\\Assist-Turnover-Ratio-22.xlsx")
bracket_rank_22 <- read_excel("data22\\bracket-rankings-results-22.xlsx")

# Beginning to merge dataframes

# Left join - select all rows from the left table, and matching rows from right
#### USING THE bracket-rankings as the primary LHS join, as it has the 
#### March Madness teams here, to save on storage

bracket_rank_22 # the teams in March Madness and their bracket seed


df22 <- merge(x = bracket_rank_22, y = ncaa_stats_22, by = 'Team', all.x=TRUE)
df22 <- merge( x = df22, y = fg_opp_22, by = 'Team', all.x=TRUE)
df22 <- merge(x = df22, y = fg_22, by = 'Team', all.x = TRUE) # adding a row is happening here
df22 <- merge(x = df22, y = free_22, by = 'Team', all.x = TRUE)
df22 <- merge(x = df22, y = threes_22, by = 'Team', all.x = TRUE)
df22 <- merge(x = df22, y = scoring_margin_22, by = 'Team', all.x = TRUE)
df22 <- merge(x = df22, y = rebounds_22, by = 'Team', all.x = TRUE)
df22 <- merge(x = df22, y = assist_turnover_22, by = 'Team', all.x = TRUE)


# now df22 is our entire 2022 df
df22$Conference <- as.factor(df22$Conference)
df22$Region <- as.factor(df22$Region)
summary(df22$Region)
summary(df22$Conference)
summary(df22)

#write.csv(df1, "Data22\\combined_unclean_df22.csv", row.names=FALSE)


# - check for missing values and ensure there aren't any
which(is.na(df22))


# - make the various df's to work with the models (i.e. no character, numeric only, etc.)
df22_clean <- subset(df22, select = -c(GM))

# testing collinearity
library(car)
library(psych)
#filter new df for numeric values only
numeric_df22 <- subset(df22_clean, select = -c(Team, Region, Conference))

# correlation
res22 <- cor(numeric_df22)
round(res22,2)

# Fitting an lm as a base model, or 'punching bag' as John calls it
lmfit22 <- lm(Finish~., data = numeric_df22)
summary(lmfit22)
plot(lmfit22)


# Fitting subset of lm
lm22 <- lm(Finish~ NET+Avg_Opp_NET_Rank+WinPct+Fied_Goal_Rank+numeric_df22$`OPP FG%` + 
             numeric_df22$`FG%`+Free_Throw_Rank+Threes_Rank+numeric_df22$`3FG%`+Scoring_Margin_Rank+
            PPG+SCR_MAR+Rebound_Rank+ORebs+DRebs+numeric_df22$`Assist-Turnover_Rank`, data =numeric_df22)
summary(lm22)
plot(lm22)



# backwards selection lm
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
step.model <- stepAIC(lmfit22, direction = "both", 
                      trace = FALSE)
summary(step.model)
plot(step.model)  ### pretty good!! 



# fitting glm
gau.glm <- glm(Finish~., data = numeric_df22, family = "gaussian")
summary(gau.glm)



# cleaning names

library(janitor)

#can be done by simply
numeric_df22_clean <- clean_names(numeric_df22)
df22_clean <- clean_names(df22)

# RF
library(ranger)
rfrun <- ranger(finish~., data=df22_clean, num.trees=5000, 
                importance="impurity")
rfrun # not great


# PCA
pca_vec <- prcomp(numeric_df22, scale.=TRUE)
dim(pca_vec$rotation) # dimension is 51, for 51 variables
dim(pca_vec$x)
summary(pca_vec)
plot(pca_vec, type="lines")
load <- round(pca_vec$rotation[,1:3],2)
load[abs(load)<0.2] <- NA
load








# Overall, the step.model is the best


# Let's load in 2023 data


## Loading Dataframes

library("readxl")
ncaa_stats <- read_excel("data\\NCAA-Statistics.xlsx")
fg_opp <- read_excel("data\\field-goal-pct-opponents.xlsx")
fg <- read_excel("data\\field-goal-pct.xlsx")
free <- read_excel("data\\free-throws.xlsx")
threes <- read_excel("data\\Three-FG-Perc.xlsx")
scoring_margin <- read_excel("data\\Scoring-Margin.xlsx")
rebounds <- read_excel("data\\Rebounds-D-and-O.xlsx")
assist_turnover <- read_excel("data\\Assist-Turnover-Ratio.xlsx")
bracket_rank <- read_excel("data\\bracket-rankings.xlsx")

# Beginning to merge dataframes

# Left join - select all rows from the left table, and matching rows from right
#### USING THE bracket-rankings as the primary LHS join, as it has the 
#### March Madness teams here, to save on storage

bracket_rank # the teams in March Madness and their bracket seed

df23 <- merge(x = bracket_rank, y = ncaa_stats, by = 'Team', all.x=TRUE)
df23 <- merge( x = df23, y = fg_opp, by = 'Team', all.x=TRUE)
df23 <- merge(x = df23, y = fg, by = 'Team', all.x = TRUE) # adding a row is happening here
df23 <- merge(x = df23, y = free, by = 'Team', all.x = TRUE)
df23 <- merge(x = df23, y = threes, by = 'Team', all.x = TRUE)
df23 <- merge(x = df23, y = scoring_margin, by = 'Team', all.x = TRUE)
df23 <- merge(x = df23, y = rebounds, by = 'Team', all.x = TRUE)
df23 <- merge(x = df23, y = assist_turnover, by = 'Team', all.x = TRUE)


# now df is our entire 
df23$Conference <- as.factor(df23$Conference)
df23$Region <- as.factor(df23$Region)
summary(df23$Region)
summary(df23$Conference)
summary(df23)

# export df23
write.csv(df23, "Data22\\df23_2.csv", row.names=FALSE)
df23 <- read.csv("data22\\df23_2.csv")




# PREDICT

df23_clean <- clean_names(df23)
df23_clean <- df23_clean[-13,] #dropping farleigh dickinson because NA
df23 <- df23[-13,] #dropping farleigh dickinson because NA

df23_clean

## random forest prediction
rownames(df23_clean) <- df23_clean$team


rfpreds <- predict(rfrun, data=df23_clean)
rfpreds2 <- as.data.frame(rfpreds$predictions)
rownames(rfpreds2) <- rownames(df23_clean)
rfpreds2[order(rfpreds2[,1]),]
write.csv(rfpreds2, "rfpred1.csv")


# linear model prediction



lmpreds <- predict(step.model, data = df23)
names(lmpreds) <- rownames(na.omit(df23_clean))
write.csv(lmpreds, "lmpred.csv")

### Next steps:
# - check for missing values and ensure there aren't any
# - make the various df's to work with the models (i.e. no character, numeric only, etc.)
# - run the same models on this data with changing y to finish or place or whatever
# - run a pca?????
# - use the models and predict Y (placing in tourney) for the 2023 data 