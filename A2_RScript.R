matches <- read.csv("Matches.csv")
stadiums <- read.csv("Stadiums.csv")
teams <- read.csv("Teams.csv")
tournaments <- read.csv("Tournaments.csv")

## Question 1

# View the structure of each dataset
str(matches)
str(stadiums)
str(teams)
str(tournaments)

# Display the first six observations to quickly view the layout
head(matches)
head(stadiums)
head(teams)
head(tournaments)

# Run summary to present the class and statistical distribution of every column in each dataset
summary(matches)
summary(stadiums)
summary(teams)
summary(tournaments)

# Since only the matches dataset has missing values, replace "?" with NA
matches[matches == "?"] <- NA
str(matches) # Check if the missing values ? is replaced with NA

# Convert categorical variables to factors
matches$Result <- as.factor(matches$Result)
matches$Stage <- as.factor(matches$Stage)
stadiums$Country <- as.factor(stadiums$Country)
matches$ExtraTime <- as.factor(matches$ExtraTime)

# Convert penalties to numeric format and replace the missing values with 0
matches$HomePenalty <- as.numeric(matches$HomePenalty)
matches$AwayPenalty <- as.numeric(matches$AwayPenalty)
# Replace the missing value in penalties with 0
matches$HomePenalty[is.na(matches$HomePenalty)] <- 0
matches$AwayPenalty[is.na(matches$AwayPenalty)] <- 0

# Validate the pre-processing results
colSums(is.na(matches)) # Check remaining missing values
str(matches) # Check variable types in the matches dataset
str(stadiums) # Check variable types in the stadium dataset
sum(is.na(matches$HomePenalty)) # Confirm no missing value in HomePenalty
sum(is.na(matches$AwayPenalty)) # Confirm no missing value in AwayPenalty

# Filter matches with a penalty shootout
library(dplyr) # Use the dplyr library for filtering
unique(matches$PenaltyShootout) # Check the value of PenaltyShootout
penalty_matches <- matches %>%
  filter(PenaltyShootout == 1)
penalty_matches # Check if the penalty_matches is filtered correctly

# Since HomePenalty is a quantitative numeric variable, a histogram is 
# selected for analyzing its distribution across matches involving a penalty shootout.
table(penalty_matches$HomePenalty)
library(ggplot2) # Use the ggplot2 library to produces the graphic with better customization
ggplot(penalty_matches, aes(x = HomePenalty)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Home Penalty Goals in Penalty Shootout Matches",
       x = "Number of Home Penalty Goals",
       y = "Number of Matches") +
  theme_minimal()
# Interpretation: This histogram indicates that most home teams score 3 or 4 penalty goals during the penalty shootouts.
# In contrast, lower scores (0-2 goals) and the maximum observed score (5 goals) occurred less often, suggesting
# that home teams generally centered around a moderate number of penalties during shootouts.


## Question 2
# TournamentName is stored in the tournaments dataset, while HomeTeamScore and Stage
# are stored in the matches dataset. Therefore, the two datasets are joined using
# TournamentID to analyse HomeTeamScore across different tournaments.
matches_tournaments <- matches %>%
  left_join(tournaments, by = "TournamentID") # Join TournamentName
str(matches_tournaments) # Validate if the TournamentName join successfully

# Create histogram to analyze the distribution of HomeTeamScore across different competition stages.
ggplot(matches, aes(x = HomeTeamScore)) +
  geom_histogram(binwidth = 1, fill = "lightyellow", color = "black") +
  scale_x_continuous(breaks = seq(0, max(matches$HomeTeamScore), by = 1)) + # Display integer goals on the x-axis
  facet_wrap(~ Stage, scales = "free_y", ncol = 2) + # Compare score distribution across competition stages
  labs(title = "Distribution of Home Team Scores across Competition Stages",
       x = "Number of Goals Scored by the Home Team",
       y = "Number of Matches") +
  theme_minimal()
# Interpretation: The overall distribution of home team scores across competition stages is mainly concentrated around 1-2 scores.
# Since the Group Stage contains the largest number of matches, it shows the widespread of scores and a right-skewed distribution,
# indicating high-score performances from home team occurred occasionally. In contrast, the knockout stages, particularly final
# and final round, contain fewer matches and their distributions more concentrated at lower scores from the home teams. Additionally, 
# across the remaining knockout stages, most home teams scored between 1 and 3 goals, while scoring 5 goals or more were relatively uncommon.

# Create histogram to analyze the distribution of HomeTeamScore across different tournaments
ggplot(matches_tournaments, aes(x = HomeTeamScore)) +
  geom_histogram(binwidth = 1, fill = "lightpink", color = "black") +
  scale_x_continuous(breaks = seq(0, max(matches$HomeTeamScore), by = 1)) + # Display integer goals on the x-axis
  facet_wrap(~ TournamentName, scales = "free_y", ncol = 4) + # Compare score distribution across tournaments
  labs(title = "Distribution of Home Team Scores across FIFA World Cup Tournaments",
       x = "Number of Goals Scored by the Home Team",
       y = "Number of Matches") +
  theme_minimal()
# Interpretation: Overall, the distribution of home team scores across FIFA World Cup tournaments is concentrated between 0 and
# 3 goals, with most matches recording 1 to 2 goals. Additionally, later FIFA World Cup tournaments, particularly from 1998 onwards,
# contain more matches that earlier editions, resulting in taller histograms while maintaining similar home team scoring distribution.
# Although a few tournaments include matches where the home scored six or more goals, these high-scoring matches occurred only occasionally.

# To categorize the AwayTeamScore across following groups: 0-1 goals, 2-3 goals, and 4 or more goals
matches <- matches %>%
  mutate(AwayScoreGroup = case_when(
    AwayTeamScore <= 1 ~ "0-1 goals",
    AwayTeamScore > 1 & AwayTeamScore <= 3 ~ "2-3 goals",
    AwayTeamScore > 3 ~ "4+ goals"
  ))
# Convert AwayScoreGroup to an ordered factor for consistent group ordering in plots
matches$AwayScoreGroup <- factor (
  matches$AwayScoreGroup,
  levels = c("0-1 goals", "2-3 goals", "4+ goals")
)
# Validate the grouping of AwayScoreGroup before visualizing
table(matches$AwayScoreGroup)
# Create histogram to analyze the distribution of HomeTeamScore across AwayTeamScore groups
ggplot(matches, aes(x = HomeTeamScore)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  scale_x_continuous(breaks = seq(0, max(matches$HomeTeamScore), by = 1)) + # Display integer goals on the x-axis
  facet_wrap(~ AwayScoreGroup, scales = "free_y") + # Compare score distribution across Away Team Score groups
  labs(title = "Distribution of Home Team Scores by Away Team Score groups",
       x = "Number of Goals Scored by the Home Team",
       y = "Number of Matches") +
  theme_minimal()
# Interpretation: The distribution of goals scored by Home Team is similar across the three Away Team Score groups,
# with most matches concentrated between 0 and 3 home goals regardless of the away team's scoring groups. From the distribution,
# while the 0-1 goals contains significant number of observations, the 4+ goals contains fewer observations, indicating
# that matches in which the away team scores 4 or more goals are relatively uncommon. Moreover, although there are few matches
# recorded five or more goals, these high-scoring goals are occasionally distributed across different groups of away team score.