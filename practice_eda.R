library(dplyr)
library(ggplot2)
library(gapminder)
library(lubridate)
library(gganimate)
library(ggthemes)
library(plyr)

yellow <- "#f1c40f"
blue <- "#30a2da"
red <- "#fc4f30"
green <- "#77ab43"
purple <- "#9b59b6"

practice <- read.csv("2016Practice.csv")

practice$Player.Position[practice$Player.Position == "Midfielders"] = "Midfield" 
practice$Player.Position[practice$Player.Position == "Center Midfielder"] = "Central Midfielder" 
practice$Player.Position[practice$Player.Position == "Forwards"] = "Forward"

# practice2 <- practice %>%
#   mutate(Session.Date = dmy(Session.Date)) %>%
#   arrange(Player.Name, Session.Date)

# p3 <- ggplot(gapminder, aes(gdpPercap, lifeExp, frame = year)) +
#   geom_path(aes(cumulative = TRUE, group = country)) +
#   scale_x_log10() +
#   facet_wrap(~continent)


levels(practice$Session.Type) <- c("Minus 5", "Minus 1", "Minus 3", "Minus 2")
levels(practice$Session.Type)

averages <- practice %>%
  select(Player.Name, Session.Type, Player.Position, Distance.Total:Decelerations) %>%
  filter(Session.Type %in% c("Minus 1", "Minus 2", "Minus 3")) %>%
  arrange(Player.Position) %>%
  group_by(Player.Name, Player.Position, Session.Type) %>%
  summarise_all(mean)

averages$Session.Type <- factor(averages$Session.Type, c("Minus 1", "Minus 2", "Minus 3", "Minus 5"))
averages$Session.Type

# averages$Player.Position[averages$Player.Position == "Midfielders"] = "Midfield" 
# averages$Player.Position[averages$Player.Position == "Forwards"] = "Forward"

distance <- ggplot(averages, aes(Session.Type, round(High.Speed.Running,2), fill = Player.Position)) +
  # geom_line(aes(group = Player.Name), size = 1)+
  geom_bar(stat = "identity") + 
  facet_wrap(~Player.Name) + 
  xlab("Practice Session") +
  ylab("Distance Covered") + 
  scale_color_discrete(name = "Player\nPosition") + 
  # theme_fivethirtyeight() +
  ggtitle("Average Total distance(m) covered for practice days")+
  geom_text(aes(label = round(High.Speed.Running,2)), vjust = -0.5)
            
            

distance
library(RColorBrewer)

RColorBrewer::display.brewer.all()
hsrunning <- ggplot(defenders, aes(Fatigue.Factor, Distance.Total, color = Player.Name)) +
  geom_line(aes(group = Player.Name), size = 1)+
  facet_wrap(~Player.Position) + 
  xlab("Pracice Session") +
  ylab("Distance Covered") + 
  scale_color_discrete(name = "Player\nPosition") + 
  theme_fivethirtyeight() +
  ggtitle("Mean HSR(m) vs mean total distance covered(m) for practice days") + 
  scale_color_brewer(palette = "Paired")
  

  


hsrunning
gganimate(hsrunning)

ggplot(averages, aes(Session.Type, High.Speed.Running, color = Player.Position, frame = )) +
  geom_line(aes(group = Player.Name), size = 1)+
  facet_wrap(~Player.Name) + 
  xlab("Pracice Session") +
  ylab("Distance Covered") + 
  scale_color_discrete(name = "Player\nPosition") + 
  theme_fivethirtyeight() +
  ggtitle("Average High Speed Running(m) for practice days")

distance

hmldistance <- ggplot(averages, aes(Session.Type, HML.Distance, color = Player.Position)) +
  geom_line(aes(group = Player.Name), size = 1)+
  facet_wrap(~Player.Name) + 
  xlab("Pracice Session") +
  ylab("Distance Covered") + 
  scale_color_discrete(name = "Player\nPosition") + 
  theme_fivethirtyeight() +
  ggtitle("Average HML Distance(m) for practice days")

dsl <- ggplot(averages, aes(Session.Type, Dynamic.Stress.Load, color = Player.Position)) +
  geom_line(aes(group = Player.Name), size = 1)+
  facet_wrap(~Player.Name) + 
  xlab("Pracice Session") +
  ylab("Load") + 
  scale_color_discrete(name = "Player\nPosition") + 
  theme_fivethirtyeight() +
  ggtitle("Mean Dynamic Stress Load for practice days")

distance
hsrunning
hmldistance
dsl


midfielders <- practice %>%
  filter(grepl("midfield", Player.Position, ignore.case = T)) %>% 
  select(Player.Name, Session.Date, Player.Position, Distance.Total:Decelerations) %>%
  arrange(Player.Position, Player.Name)

defenders <- practice %>%
  filter(grepl("defender", Player.Position, ignore.case = T)) %>% 
  select(Player.Name, Session.Date, Player.Position, Distance.Total:Decelerations) %>%
  arrange(Player.Position, Player.Name)

forwards <- practice %>%
  filter(grepl("forward", Player.Position, ignore.case = T)) %>% 
  select(Player.Name, Session.Date, Player.Position, Distance.Total:Decelerations) %>%
  arrange(Player.Position, Player.Name)

kplot <- function(dfe,xmetric, ymetric, colormetric) {
  ggplot(dfe, aes_string(x = xmetric, y = ymetric, color = colormetric)) +
    geom_line(aes_string(group = colormetric), size = 1.25)+
    facet_wrap(~Player.Name) + 
    xlab(xmetric) +
    ylab(ymetric) + 
    theme_fivethirtyeight() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    scale_color_discrete(name = gsub("\\.", "\n", colormetric)) +
    ggtitle(paste("Mean", gsub("\\.", " ", ymetric), "for practice days - ", deparse(substitute(dfe))))
}

gsub("\\.", " ", "Distance Total")

kplot(forwards, "Session.Date", "Distance.Total", "Player.Position")
kplot(forwards, "Session.Date", "Fatigue.Factor", "Player.Position")
kplot(forwards, "Session.Date", "Decelerations", "Player.Position")
kplot(forwards, "Session.Date", "Sprints", "Player.Position")

kplot(midfielders, "Session.Date", "Distance.Total", "Player.Position")
kplot(midfielders, "Session.Date", "Fatigue.Factor", "Player.Position")
kplot(midfielders, "Session.Date", "Decelerations", "Player.Position")
kplot(midfielders, "Session.Date", "Sprints", "Player.Position")

kplot(defenders, "Session.Date", "Distance.Total", "Player.Position")
kplot(defenders, "Session.Date", "Fatigue.Factor", "Player.Position")
kplot(defenders, "Session.Date", "Decelerations", "Player.Position")


kplot(averages, "Session.Type", "Distance.Total", "Player.Position")
kplot(averages, "Session.Type", "Fatigue.Factor", "Player.Position")

ggplot(forwards, aes(Session.Date, Distance.Total, color = Player.Position)) +
  geom_point()+
  facet_wrap(~Player.Name) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


full <- read.csv("2016FullPractice.csv")


class(defenders$Session.Date)
defenders$Session.Date


defenders <- defenders %>%
  mutate(Session.Date = dmy(Session.Date))

forwards <- forwards %>%
  mutate(Session.Date = dmy(Session.Date))

midfielders <- midfielders %>%
  mutate(Session.Date = dmy(Session.Date))


defenders$Session.Date

View(as.data.frame(order(dmy(levels(defenders$Session.Date)))))

dates <- levels(defenders$Session.Date) %>%
  dmy() %>%
  as.data.frame() %>%
  arrange(.)


kplot(defenders, "Session.Date", "Sprints", "Player.Position")
kplot(defenders, "Session.Date", "Fatigue.Factor", "Player.Position")

mikayla <- full %>%
  filter(Player.Name == "Mikayla Lasky")

mikayla <- mikayla %>%
  mutate(week = week(dmy(Session.Date)))

matchdays <- full %>%
  filter(Session.Type == "MatchDay") %>%
  mutate(week = week(dmy(Session.Date)))

mikDist <- mikayla %>%
  select(week, Distance.Total) %>%
  group_by(week) %>%
  summarise(averageDistance = mean(Distance.Total))

  
unique(full$Player.Name)
length(matchdays$Distance.Total[matchdays$Player.Name == "Mikayla Lasky"])

byweek <- mikayla %>%
  select(week, Distance.Total) %>%
  group_by(week) %>%
  summarise(mean(Distance.Total))

ggplot(mikayla, aes(Session.Date, Fatigue.Factor)) +
  geom_line(aes(group = Player.Name)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

levels(defenders$Session.Date)

week("2016-08-16")
week("2016-08-23")


mikFinal <- merge(mikDist, byweek, by=c("week"), all=T)




practice$Player.Position[practice$Player.Name == "Hannah Mussallem"] = "Outside Midfielder"




write.csv(practice, "practice.csv", row.names = F)



