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

df <- merge(x = bracket_rank, y = ncaa_stats, by = 'Team', all.x=TRUE)
df1 <- merge( x = df, y = fg_opp, by = 'Team', all.x=TRUE)
df1 <- merge(x = df1, y = fg, by = 'Team', all.x = TRUE) # adding a row is happening here
df1 <- merge(x = df1, y = free, by = 'Team', all.x = TRUE)
df1 <- merge(x = df1, y = threes, by = 'Team', all.x = TRUE)
df1 <- merge(x = df1, y = scoring_margin, by = 'Team', all.x = TRUE)
df1 <- merge(x = df1, y = rebounds, by = 'Team', all.x = TRUE)
df1 <- merge(x = df1, y = assist_turnover, by = 'Team', all.x = TRUE)


# now df is our entire 
df1$Conference <- as.factor(df1$Conference)
df1$Region <- as.factor(df1$Region)
summary(df1$Region)
summary(df1$Conference)
summary(df1)

#write.csv(df1, "Data\\combined_unclean_df.csv", row.names=FALSE)

df1 <- read.csv("Data\\combined_unclean_df.csv", sep = ',')
  
# Dropping duplicate columns
df_clean <- subset(df1, select = -c(GM))

# testing collinearity because the lm won't run
library(car)
library(psych)
#filter new df for numeric values only
numeric_df <- subset(df_clean, select = -c(Team, Region, Conference))



#pairs.panels(numeric_df, method = "pearson")
res <- cor(numeric_df)
round(res,2)


df_no_team_name <- subset(df_clean, select = -c(Team, Conference))
# Fitting an lm as a base model, or 'punching bag' as John calls it
lmfit <- lm(Ranking~., data = df_no_team_name)
summary(lmfit)

lm2 <- lm(Ranking~ NET+Avg_Opp_NET_Rank+WinPct+Fied_Goal_Rank+df_no_team_name$OPP_FG.+ 
            df_no_team_name$FG.+Free_Throw_Rank+Threes_Rank+df_no_team_name$X3FG.+Scoring_Margin_Rank+
            PPG+SCR_MAR+Rebound_Rank+ORebs+DRebs+df_no_team_name$Assist.Turnover_Rank, data = df_no_team_name)
summary(lm2)
plot(lm2)

 


# backwards selection lm
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
step.model <- stepAIC(lmfit, direction = "both", 
                      trace = FALSE)
summary(step.model)
plot(step.model)  ### pretty good!!



# fitting glm
gau.glm <- glm(Ranking~., data = df_no_team_name, family = "gaussian")
summary(gau.glm)



data1 <- read.csv("data\\clean_df_all.csv")
head(data1)
data1 <- data1[-13,] #dropping farleigh dickinson because NA

# cleaning names

library(janitor)

#can be done by simply
####data1 <- clean_names(data1)


# RF
library(ranger)
rfrun <- ranger(Ranking~., data=data1, num.trees=500, 
                importance="impurity")
rfrun



### idea!!!!!!!! get last year's data (same as above) and then create the model
### with the same info but let Y=march madness finish place, then test the model
### with this year's data from above, to get the prediction values for MM finish place


# Last year's data