library(dplyr)
library(readr)
library(ggplot2)
fires <- read_csv("forestfires.csv")

fires <- fires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))) 
fires <- fires %>%
  mutate(day = factor(day, levels = c("mon", "tue", "wed", "thur", "fri","sat", "sun")))


fires_by_month <- fires %>%
  group_by(month) %>%
  summarize(total = n())

ggplot(data = fires_by_month) +
  aes(x = month, y = total) +
  geom_bar(stat = "identity") +
  ggtitle("Forest Fires By Month") +
  xlab("Month") + ylab("Number of Fires") +
  theme(plot.title = element_text(hjust = 0.5))

fires_by_day <- fires %>% 
  group_by(day) %>%
  summarize(total = n())

ggplot(data = fires_by_day) +
  aes(x = day, y = total) +
  geom_bar(stat = "identity") +
  ggtitle("Forest Fires by \n Day of the Week") +
  xlab("Day of the Week") + ylab("Number of Fires") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(fires) +
  aes(x = month, y = FFMC) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("FFMC per Month") +
  xlab("Month") + ylab("FFMC") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = month, y = area) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("Area of Fires per Month") +
  xlab("Month") + ylab("FFMC") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = month, y = DMC) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("DMC per Month") +
  xlab("Month") + ylab("DMC") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = month, y = DC) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("DC per Month") +
  xlab("Month") + ylab("DC") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = month, y = ISI) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("ISI per Month") +
  xlab("Month") + ylab("ISI") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = month, y = temp) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("Temperature per Month") +
  xlab("Month") + ylab("Temp (in C)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = month, y = RH) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("RH per Month") +
  xlab("Month") + ylab("RH") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = month, y = wind) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("Wind speed per Month") +
  xlab("Month") + ylab("Wind speed (km/h)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = month, y = rain) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("Rain per Month") +
  xlab("Month") + ylab("rain (in cm)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = day, y = FFMC) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("FFMC per Day") +
  xlab("Day") + ylab("FFMC") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = day, y = DMC) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("DMC per Day") +
  xlab("Day") + ylab("DMC") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = day, y = DC) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("DC per Day") +
  xlab("Day") + ylab("DC") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = day, y = ISI) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("ISI per Day") +
  xlab("Day") + ylab("ISI") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = day, y = temp) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("Temperatures per Day") +
  xlab("Day") + ylab("Temp (in C)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = day, y = RH) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("RH per Day") +
  xlab("Day") + ylab("RH") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = day, y = wind) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("Wind Speeds per Day") +
  xlab("Day") + ylab("Wind Speed (in km/hr") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(fires) +
  aes(x = day, y = rain) + 
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("Rain per Day") +
  xlab("Day") + ylab("RAin (in cm)") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = fires) +
  aes(x = FFMC, y = area) +
  geom_point(alpha=0.3) +
  xlim(18.7,96.2) +
  ggtitle("FFMC per Area") +
  xlab("FFMC") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = fires) +
  aes(x = DMC, y = area) +
  geom_point(alpha=0.3) +
  xlim(1.1,291.3) +
  ggtitle("DMC per Area") +
  xlab("DMC") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
  
ggplot(data = fires) +
  aes(x = DC, y = area) +
  geom_point(alpha=0.3) +
  xlim(7.9,891.6) +
  ggtitle("DC per Area") +
  xlab("DC") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = ISI, y = area) +
  geom_point(alpha=0.3) +
  xlim(0,56.1) +
  ggtitle("ISI per Area") +
  xlab("ISI") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = temp, y = area) +
  geom_point(alpha=0.3) +
  xlim(2.2,33.3) + 
  ggtitle("Temperature per Area") +
  xlab("Temperature (in C)") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
  
ggplot(data = fires) +
  aes(x = RH, y = area) +
  geom_point(alpha=0.3) +
  xlim(15,100) +
  ggtitle("RH per Area") +
  xlab("RH") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = wind, y = area) +
  geom_point(alpha=0.3) +
  xlim(0.40,9.40) +
  ggtitle("Wind per Area") +
  xlab("Wind (in kmh)") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = rain, y = area) +
  geom_point(alpha=0.3)
  xlim(0.0,6.4) +
  ggtitle("Rain per Area") +
  xlab("Rain (in cm)") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
# look at top 25 % largest fires by area
  
#Create set with top 25%

areas = fires$area
quantile(areas)

# the 75th percentile is 6.57. We will look at all the data points above this value

ggplot(data = fires) +
  aes(x = FFMC, y = area) +
  geom_point(alpha=0.3) +
  xlim(18.7,96.2) +
  ylim(6,1200) +
  ggtitle("FFMC per Area \n Top 25%") +
  xlab("FFMC") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
  
ggplot(data = fires) +
  aes(x = DMC, y = area) +
  geom_point(alpha=0.3) +
  xlim(1.1,291.3) +
  ylim(6,1200) +
  ggtitle("DMC per Area \n Top 25%") +
  xlab("DMC") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = DC, y = area) +
  geom_point(alpha=0.3) +
  xlim(7.9,891.6) +
  ylim(6,1200) +
  ggtitle("DC per Area \n Top 25%") +
  xlab("DC") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = ISI, y = area) +
  geom_point(alpha=0.3) +
  xlim(0,56.1) +
  ylim(6,1200) +
  ggtitle("ISI per Area \n Top 25%") +
  xlab("ISI") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = temp, y = area) +
  geom_point(alpha=0.3) +
  xlim(2.2,33.3) +
  ylim(6,1200) +
  ggtitle("Temperature during fire \n per Area \n Top 25%") +
  xlab("Temperature (in C)") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = RH, y = area) +
  geom_point(alpha=0.3) +
  xlim(15,100) +
  ylim(6,1200) +
  ggtitle("RH per Area \n Top 25%") +
  xlab("RH") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = wind, y = area) +
  geom_point(alpha=0.3) +
  xlim(0.40,9.40) +
  ylim(6,1200) + 
  ggtitle("Wind per Area \n Top 25%") +
  xlab("Wind (in kmh)") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(data = fires) +
  aes(x = rain, y = area) +
  geom_point(alpha=0.3) +
  xlim(0.0,6.4) +
  ylim(6,1200) +
  ggtitle("Rain per Area \n Top 25%") +
  xlab("Rain (in cm)") + ylab("Area") +
  theme(plot.title = element_text(hjust = 0.5))

