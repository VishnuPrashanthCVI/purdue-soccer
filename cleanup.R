library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(scales)

setwd("C:/Users/Kanishka/purdue-soccer/")

yellow <- "#f1c40f"
blue <- "#30a2da"
red <- "#fc4f30"
green <- "#77ab43"
purple <- "#9b59b6"
orange <- "#FF8B2E"
darkpurp <- "#461A82"

practice <- read.csv("practice fixed.csv")
full <- read.csv("practice and full fixed.csv")

practice$Session.Type = factor(practice$Session.Type, c("Minus 5", "Minus 3", "Minus 2", "Minus 1"))
levels(full$Session.Type) <- c("Match Day", "Minus 1", "Minus 2", "Minus 3")
full$Session.Type = factor (full$Session.Type, c("Minus 3", "Minus 2", "Minus 1", "Match Day"))
full$Player.Position[full$Player.Position == "21"] = "Outside Midfielder"


midfielders <- practice %>%
  filter(grepl("midfield", Player.Position, ignore.case = T)) %>% 
  select(Player.Name, Session.Date, Player.Position, Distance.Total:Decelerations) %>%
  arrange(Player.Position, Player.Name) %>%
  mutate(Session.Date = dmy(Session.Date)) %>%
  arrange(Session.Date)

defenders <- practice %>%
  filter(grepl("defender", Player.Position, ignore.case = T)) %>% 
  select(Player.Name, Session.Date, Player.Position, Distance.Total:Decelerations) %>%
  arrange(Player.Position, Player.Name) %>%
  mutate(Session.Date = dmy(Session.Date)) %>%
  arrange(Session.Date)

forwards <- practice %>%
  filter(grepl("forward", Player.Position, ignore.case = T)) %>% 
  select(Player.Name, Session.Date, Player.Position, Distance.Total:Decelerations) %>%
  arrange(Player.Position, Player.Name) %>%
  mutate(Session.Date = dmy(Session.Date)) %>%
  arrange(Session.Date)

matchdays <- full %>%
  filter(Session.Type == "Match Day") %>%
  mutate(week = week(dmy(Session.Date)))
