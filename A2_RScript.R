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