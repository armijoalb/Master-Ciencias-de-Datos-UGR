# Librerias necesarias
library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(VIM)
df.car_spec_data <- read_excel("~/Universidad/Introduccion a la ciencia de datos/Introduccion a la ciencia de datos/car_example.xls")
#View(df)

aggr_plot <- aggr(df.car_spec_data, col=c('blue','red'), numbers=TRUE,
                  sortVars=TRUE, labels=names(df.car_spec_data), cex.axis=.7, gap=3,
                  ylab=c("Histogram of missing data","Pattern"))

ggplot(data=df.car_spec_data, aes(x=horsepower_bhp, y=top_speed_mph)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("Horsepower vs. Top Speed") +
  labs(x="Horsepower, bhp", y="Top Speed,\n mph")

ggplot(data=df.car_spec_data, aes(x=top_speed_mph)) +
  geom_histogram(fill="blue") +
  ggtitle("Histogram of Top Speed") +
  labs(x="Top Speed, mph", y="Count\nof Records")

ggplot(data=df.car_spec_data, aes(x=top_speed_mph)) +
  geom_histogram(fill="blue") +
  ggtitle("Histogram of Top Speed\nby decade") +
  labs(x="Top Speed, mph", y="Count\nof Records") +
  facet_wrap(~decade)

df.car_spec_data %>%
  filter(top_speed_mph == 155 & year>=1990) %>%
  group_by(make_nm) %>%
  summarize(count_speed_controlled = n()) %>%
  arrange(desc(count_speed_controlled))

ggplot(data=df.car_spec_data, aes(x=horsepower_bhp,
                                  y=top_speed_mph)) +
  geom_point(alpha=.6,color="blue") +
  facet_wrap(~decade) +
  ggtitle("Horsepower vs Top Speed\nby decade") +
  labs(x="Horsepower, bhp", y="Top Speed\n mph")

ggplot(data=df.car_spec_data, aes(x=year,
                                  y=df.car_spec_data$top_speed_mph)) +
  geom_point(alpha=.35, size=4.5, color="#880011", position =
               position_jitter()) +
  scale_x_discrete(breaks =
                     c("1950","1960","1970","1980","1990","2000","2010")) +
  ggtitle("Car Top Speeds by Year") +
  labs(x="Year" ,y="Top Speed\nmph")

df.car_spec_data %>%
  group_by(year) %>%
  summarize(max_speed = max(top_speed_mph, na.rm=TRUE))%>%
  ggplot(aes(x=year,y=max_speed,group=1)) +
  geom_point(size=5, alpha=.8, color="#880011") +
  stat_smooth(method="auto",size=1.5) +
  scale_x_discrete(breaks =
                     c("1950","1960","1970","1980","1990","2000","2010")) +
  ggtitle("Speed of Year's Fastest Car by Year") +
  labs(x="Year",y="Top Speed\n(fastest car)")

