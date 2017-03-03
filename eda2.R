library(ggplot2)
library(gapminder)
library(lubridate)
library(gganimate)
library(ggthemes)
library(dplyr)

full <- read.csv("2016FullPractice.csv")

mikayla <- full %>%
  filter(Player.Name == "Mikayla Lasky", Session.Type != "MatchDay") %>%
  mutate(week = week(dmy(Session.Date)))

matchdays <- full %>%
  filter(Session.Type == "MatchDay") %>%
  mutate(week = week(dmy(Session.Date)))

mikpractice <- mikayla %>%
  select(week, Distance.Total) %>%
  group_by(week) %>%
  summarise(practice = mean(Distance.Total))

mikmatchday <- matchdays %>%
  filter(Player.Name == "Mikayla Lasky")

matchdaybyweek <- mikmatchday %>%
  select(week, Distance.Total) %>%
  group_by(week) %>%
  summarise(matchday = mean(Distance.Total))

mikFinal <- merge(mikpractice, matchdaybyweek, by=c("week"), all=T)



full$Player.Position[full$Player.Position == "Midfielders"] = "Midfield" 
full$Player.Position[full$Player.Position == "Center Midfielder"] = "Central Midfielder" 
full$Player.Position[full$Player.Position == "Forwards"] = "Forward"


write.csv(full, "practice and full.csv", row.names = F)
