##########################################################
# Installing and loading necessary packages
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

# Loading libraries
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(knitr)

##########################################################
# Dataset Exploration
##########################################################

# Reading data from CSV file into a variable
vgSales <- read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
summary(vgSales)

# Checking for and eliminating N/A values
colSums(is.na(vgSales))
vgSales <- vgSales[complete.cases(vgSales), ]

str(vgSales)

# Checking for and eliminating string N/A values
sum(vgSales$Year_of_Release != "N/A")
sum(vgSales$Publisher != "N/A")
sum(vgSales$Developer != "N/A")
sum(vgSales$Rating != "N/A")

vgSales_YOR <- vgSales[vgSales$Year_of_Release != "N/A", ]
vgSales_Publisher <- vgSales[vgSales$Publisher != "N/A", ]
unique(vgSales$Year_of_Release)
unique(vgSales$Publisher)

# Changing critic score, user score, critic count, and user count variables to numeric value
vgSales$Critic_Score <- as.numeric(as.character(vgSales$Critic_Score))
vgSales$Critic_Score <- vgSales$Critic_Score / 10

vgSales$User_Score <- as.numeric(as.character(vgSales$User_Score))

vgSales$Critic_Count <- as.numeric(vgSales$Critic_Count)

vgSales$User_Count <- as.numeric(vgSales$User_Count)

# Checking for extreme values in sales variables
summary(vgSales$EU_Sales)
summary(vgSales$JP_Sales)
summary(vgSales$NA_Sales)
summary(vgSales$Global_Sales)
summary(vgSales$Other_Sales)

# Categorizing rating groups together to prevent extremes in results
table(vgSales$Rating)

vgSales <- vgSales %>%
  mutate(Rating = ifelse(Rating == "AO", "M", Rating))
vgSales <- vgSales %>%
  mutate(Rating = ifelse(Rating == "K-A", "E", Rating))
vgSales <- vgSales %>%
  mutate(Rating = ifelse(Rating == "RP", "E", Rating))

##########################################################
# Modeling Analysis and Results
##########################################################

# Analyzing games released by genre
gameGenre <- ggplot(vgSales_YOR, aes(Year_of_Release)) +
  geom_bar(stat = "count", aes(fill = Genre)) +
  labs(title = "Total Count of Games Released/Year Sorted by Genre", x = "Year", y = "# of Games") +
  theme(legend.position = "left", axis.text.x = element_text(angle = 90, hjust = 0.8, size = 6))
gameGenre

# Analyzing game sales worldwide by region
vgSales_wwSales <- vgSales_YOR %>%
  select(Year_of_Release, EU_Sales, JP_Sales, NA_Sales, Other_Sales)

vgSales_byRegion <- gather(vgSales_wwSales, Region, TotalGameSales, EU_Sales:Other_Sales)

wwSales <- ggplot(vgSales_byRegion, aes(x = Year_of_Release, y = TotalGameSales, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Game Sales/Year Sorted by Region", x = "Year", y = "Total Game Sales") +
  theme(legend.position = "left", axis.text.x = element_text(angle = 90, hjust = 0.8, size = 6))
wwSales

# Analyzing correlation between critic and user score
scoresPlot <- ggplot() + 
  geom_density(data = vgSales, aes(x = Critic_Score, y = ..scaled..), color = "purple4", fill = "purple", alpha = 0.3) + 
  geom_density(data = vgSales, aes(x = User_Score, y = ..scaled..), color = "darkorange3", fill = "orange", alpha = 0.3) +
  labs(title = "Density Correlation between Critic Score (Purple) and User Score (Orange)", x = "Critic_Score, User_Score")
scoresPlot

##########################################################
# Conclusion
##########################################################

# Final plot of game sales by publisher
salesbyPublisher <- vgSales %>%
  group_by(Publisher) %>%
  summarise(pubSales = sum(Global_Sales)) %>%
  arrange(desc(pubSales)) %>%
  head(n = 25)

pubPlot <- ggplot(salesbyPublisher, aes(x = reorder(Publisher, pubSales), y = pubSales)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Total Game Sales by Publisher", x = "Publisher", y = "Total Sales") +
  theme_bw() +
  coord_flip()
pubPlot

# Final plot of game sales by platform
salesbyPlatform <- vgSales %>%
  group_by(Platform) %>%
  summarise(platSales = sum(Global_Sales)) %>%
  arrange(desc(platSales)) %>%
  head(n = 25)

platPlot <- ggplot(salesbyPlatform, aes(x = reorder(Platform, platSales), y = platSales)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Total Game Sales by Platform", x = "Platform", y = "Total Sales") +
  theme_bw() +
  coord_flip()
platPlot

# Final t-test analysis of critic score and user score data revealing correlation 
t.test(vgSales$Critic_Score, vgSales$User_Score)