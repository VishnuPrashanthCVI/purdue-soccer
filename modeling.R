library(dplyr)
library(ggplot2)
library(MASS)
library(pander)
library(ALSM)

practice <- read.csv("practice fixed.csv")
practice$Session.Type = factor(practice$Session.Type, c("Minus 5", "Minus 3", "Minus 2", "Minus 1"))

positionsPracticeDays <- full %>%
  filter(Player.Name != "Holly Gregory", Session.Type != "Minus 5") %>%
  select(Player.Position, Session.Type, Distance.Total:Decelerations) %>%
  group_by(Player.Position, Session.Type) %>%
  summarise_all(mean)

model <- lm(Fatigue.Factor ~ Distance.Total + High.Speed.Running + HML.Distance + Heart.Rate.Exertion + Sprints + Accelerations + Decelerations, data = practice)
summary(model)


modeling <- practice %>%
  dplyr::select(-c(Player.Name:Session.Date, Dynamic.Stress.Load, Speed.Intensity)) %>%
  dplyr::select(Session.Type, Fatigue.Factor, Distance.Total:Decelerations)

minus1 <- modeling %>%
  filter(Session.Type == "Minus 1")
minus2 <- modeling %>%
  filter(Session.Type == "Minus 2")
minus3 <- modeling %>%
  filter(Session.Type == "Minus 3")
 
best <- BestSub(minus3[,3:9], minus3[,2], method = 'r2adj')
pander(best)
