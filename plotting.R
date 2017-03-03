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


library(reshape2)
forwards3 <- forwards %>%
  select(Distance.Total:Decelerations)
melted_forwards <- melt(forwards3)

cormat <- round(cor(forwards3),2)
cormat
melted_cormat <- melt(cormat)

ggplot(melted_cormat, aes(Var1, Var2, fill = value))+
  geom_tile() + 
  theme_fivethirtyeight()

ggplot(positionsPracticeDays, aes(Session.Type, Distance.Total, color = blue)) +
  geom_line(aes(group = Player.Position), size = 1.5, color = yellow) +
  facet_wrap(~Player.Position) + 
  theme_fivethirtyeight() +
  ggtitle("Mean Distance total for each position - practice")


levels(full$Player.Name)
players <- full %>%
  unique(Player.Name)




averages <- full %>%
  select(Player.Name, Session.Type, Player.Position, Distance.Total:Decelerations) %>%
  filter(Session.Type %in% c("Minus 1", "Minus 2", "Minus 3", "Match Day")) %>%
  arrange(Player.Position) %>%
  group_by(Player.Name, Player.Position, Session.Type) %>%
  summarise_all(mean)


ggplot(averages, aes(Session.Type,round(Fatigue.Factor, 2), fill = Player.Position)) +
  # geom_line(aes(group = Player.Name), size = 1)+
  geom_bar(stat = "identity") + 
  facet_wrap(~Player.Name) + 
  xlab("Practice Session") +
  ylab("Distance Covered") + 
  scale_color_discrete(name = "Player\nPosition") + 
  # theme_fivethirtyeight() +
  ggtitle("Average Total distance(m) covered for practice days")+
  geom_text(aes(label = round(Fatigue.Factor,2)), vjust = -0.5)

