library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(scales)
library(extrafont)

setwd("C:/Users/Kanishka/purdue-soccer/")

yellow <- "#f1c40f"
blue <- "#30a2da"
red <- "#fc4f30"
green <- "#77ab43"
purple <- "#9b59b6"
orange <- "#FF8B2E"
darkpurp <- "#461A82"
bg_text = '#969696'

practice <- read.csv("practice fixed.csv")
full <- read.csv("practice and full fixed.csv")

practice$Session.Type = factor(practice$Session.Type, c("Minus 5", "Minus 3", "Minus 2", "Minus 1"))
levels(full$Session.Type) <- c("Match Day", "Minus 1", "Minus 2", "Minus 3")
full$Session.Type = factor (full$Session.Type, c("Minus 3", "Minus 2", "Minus 1", "Match Day"))
full$Player.Position[full$Player.Position == "21"] = "Outside Midfielder"

levels(full$Player.Position) <- c(levels(full$Player.Position), "Midfielder")
full$Player.Position[full$Player.Position == "Outside Midfielder"] = "Midfielder"
full$Player.Position[full$Player.Position == "Central Midfielder"] = "Midfielder"
full$Player.Position[full$Player.Position == "Midfield"] = "Midfielder"

full$Player.Position[full$Player.Position == "Outside Defender"] = "Defender"
full$Player.Position[full$Player.Position == "Center Defender"] = "Defender"

rejects <- c("Claire Albertz", "Grace Dunker", "Juliana Hairston", "Ryan Dudycha", "Holly Gregory", "Kyrie Seying", "Lydia Brosnahan", "Paige Bourne")
regulars <- full %>%
  filter(!(Player.Name %in% rejects))
regulars <- regulars[, !(names(regulars) %in% c("X"))]

midfielders <- regulars %>%
  filter(grepl("midfield", Player.Position, ignore.case = T)) %>% 
  select(Player.Name, Session.Date, Player.Position, Distance.Total:Decelerations) %>%
  arrange(Player.Position, Player.Name) %>%
  mutate(Session.Date = dmy(Session.Date)) %>%
  arrange(Session.Date)

defenders <- regulars %>%
  filter(grepl("defender", Player.Position, ignore.case = T)) %>% 
  select(Player.Name, Session.Date, Player.Position, Distance.Total:Decelerations) %>%
  arrange(Player.Position, Player.Name) %>%
  mutate(Session.Date = dmy(Session.Date)) %>%
  arrange(Session.Date)

forwards <- regulars %>%
  filter(grepl("forward", Player.Position, ignore.case = T)) %>% 
  select(Player.Name, Session.Date, Player.Position, Distance.Total:Decelerations) %>%
  arrange(Player.Position, Player.Name) %>%
  mutate(Session.Date = dmy(Session.Date)) %>%
  arrange(Session.Date)

matchdays <- regulars %>%
  filter(Session.Type == "Match Day") %>%
  mutate(week = week(dmy(Session.Date)))


averages <- regulars %>%
  select(Player.Name, Session.Type, Player.Position, Distance.Total:Decelerations) %>%
  filter(Session.Type %in% c("Minus 1", "Minus 2", "Minus 3", "Match Day")) %>%
  arrange(Player.Position) %>%
  group_by(Player.Name, Player.Position, Session.Type) %>%
  summarise_all(mean)

averages$color[averages$Session.Type %in% c("Minus 1", "Minus 2", "Minus 3")] <- blue
averages$color[averages$Session.Type %in% c("Match Day")] <- yellow


fatigue <- ggplot(averages, aes(Session.Type,round(Fatigue.Factor, 2), fill = color)) +
  # geom_line(aes(group = Player.Name), size = 1)+
  geom_bar(stat = "identity") + 
  facet_wrap(~Player.Name, scales = "free_x") + 
  ggtitle("Average Fatigue Factor for the season")+
  geom_text(aes(label = round(Fatigue.Factor,2)), vjust = 1.5, color = "white") + 
  scale_fill_hue(labels = c("Practice Days", "Match Day")) +
  theme_fivethirtyeight() + 
  theme(legend.title=element_blank()) + 
  theme(panel.background = element_rect(fill = '#f8f8f8'),
        strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(fill="#f9f9f9"))

fatigue

kkplot <- function(metric) {
  text = metric
  metric = eval(parse(text = paste("round(averages$", metric, ", 2)")))
  ggplot(averages, aes(Session.Type,metric, fill = color)) +
    geom_bar(stat = "identity") + 
    facet_wrap(~Player.Name, scales = "free_x") + 
    ggtitle(paste("Average", gsub("\\.", " ", text), "for the season"))+
    geom_text(aes(label = metric), vjust = 1.5, color = "white") + 
    scale_fill_hue(labels = c("Practice Days", "Match Day")) +
    theme_fivethirtyeight() + 
    theme(legend.title=element_blank()) + 
    theme(panel.background = element_rect(fill = '#f8f8f8'),
          strip.text.x = element_text(face = "bold"),
          strip.background = element_rect(fill="#f9f9f9"))
}

kkplot("Distance.Total")

colnames(averages)[4:14]

for (i in colnames(averages)[4:14]) {
  ggsave(paste("plots/Metrics/", gsub("\\.", " ", i), ".png"), kkplot(i), width = 13, height = 7.55)
}
ggsave("plots/metrics.png", fatigue, width = 13.44, height = 7.55)



