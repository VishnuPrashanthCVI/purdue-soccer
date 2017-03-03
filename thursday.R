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

write.csv(full, "practice and full fixed.csv", row.names = F)

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

kplot <- function(dfe,xmetric, ymetric, colormetric) {
  ggplot(dfe, aes_string(x = xmetric, y = ymetric, fill = colormetric)) +
    # geom_line(aes_string(group = colormetric), size = 1.5)+
    geom_bar(stat="identity") +
    facet_wrap(~Player.Name) + 
    xlab(xmetric) +
    ylab(ymetric) + 
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_color_discrete(name = gsub("\\.", "\n", colormetric)) +
    ggtitle(paste(gsub("\\.", " ", ymetric), "for practice days - ", dfe$Player.Name[1])) +
    scale_x_date(date_breaks = "2 days", date_labels = "%b %d")
}

kplot(forwards, "Session.Date", "Distance.Total", "Player.Position")

kplot(defenders, "Session.Date", "Distance.Total", "Player.Position")

kplot(midfielders, "Session.Date", "Distance.Total", "Player.Position")

kmplot <- function(player, xmetric) {
  # for(i in unique(position$Player.Name)) {
  #   
  # }
  playerdata <- full %>%
    filter(Player.Name == player) %>%
    arrange(Player.Position, Player.Name) %>%
    mutate(Session.Date = dmy(Session.Date)) %>%
    arrange(Session.Date)
  
  kplot(playerdata, "Session.Date", xmetric, "Session.Type")
  # rm(playerdata)
}

kmplot("Brady Riley", "Distance.Total")

unique(practice$Player.Name)

for (i in unique(full$Player.Name)) {
  ggsave(paste("plots/Distance Total/", i, ".png"), kmplot(i, "Distance.Total"), width = 11, height = 5)
}

for (i in unique(practice$Player.Name)) {
  ggsave(paste("plots/Fatigue Factor//", i, ".png"), kmplot(i, "Fatigue.Factor"), width = 11, height = 5)
}

positions <- full %>%
  filter(Player.Name != "Holly Gregory") %>%
  select(Player.Position, Distance.Total:Decelerations) %>%
  group_by(Player.Position) %>%
  summarise_all(mean)

positionsPracticeDays <- full %>%
  filter(Player.Name != "Holly Gregory", Session.Type != "Minus 5") %>%
  select(Player.Position, Session.Type, Distance.Total:Decelerations) %>%
  group_by(Player.Position, Session.Type) %>%
  summarise_all(mean)

ggplot(positionsPracticeDays, aes(Session.Type, Distance.Total, color = blue)) +
# ggplot(positionsPracticeDays, aes(Session.Type, Distance.Total, fill = blue)) +
  geom_line(aes(group = Player.Position), size = 1.5, color = blue) +
  # geom_bar(stat = "identity", fill = blue) + 
  facet_wrap(~Player.Position) + 
  theme_fivethirtyeight() +
  ggtitle("Mean Distance total for each position - practice")
  # scale_color_manual(values = c(red, blue, orange, green, darkpurp, yellow, purple))

# ggplot(forwards, aes())

ggplot(defenders, aes(Distance.Total,Fatigue.Factor, color = Player.Name)) +
  geom_line(aes(group = Player.Name), size = 1) + 
  facet_wrap(~Player.Position) + 
  scale_color_brewer(palette = "Paired")


ggplot(forwards, aes( Speed.Intensity, Fatigue.Factor, color = Player.Name)) +
  geom_line(aes(group = Player.Name), size = 1)+
  facet_wrap(~Player.Position) 
  # xlab("Pracice Session") +
  # ylab("Distance Covered") + 
  # scale_color_discrete(name = "Player\nPosition") + 
  # theme_fivethirtyeight() +
  # ggtitle("Mean HSR(m) vs mean total distance covered(m) for practice days") + 
  # scale_color_brewer(palette = "Paired")

cor(forwards$Fatigue.Factor, forwards$Speed.Intensity)
cor(forwards2$Fatigue.Factor, forwards2$Speed.Intensity)

forwards2 <- forwards %>%
  filter(Player.Name != "Hannah Leinert")




ggplot(forwards, aes())





ggplot(positionsPracticeDays, aes(Session.Type, Distance.Total, color = blue)) +
  geom_line(aes(group = Player.Position), size = 1.5, color = yellow) +
  facet_wrap(~Player.Position) + 
  theme_fivethirtyeight() +
  ggtitle("Mean Distance total for each position - practice")


levels(full$Player.Name)
players <- full %>%
  unique(Player.Name)











