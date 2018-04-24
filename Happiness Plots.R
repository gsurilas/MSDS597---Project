library(tidyverse)
library(reshape2)

happy17 <- read_csv("C:/Users/George/Documents/Rutgers/Data Wrangling/Project/2017_happiness.csv")
happy16 <- read_csv("C:/Users/George/Documents/Rutgers/Data Wrangling/Project/2016_happiness.csv")

#Join happy16 and happy17 to get Regions from 2016 data which is missing in 2017 data. And clean column names
happy <- happy17 %>% 
  left_join(select(happy16, Country, Region))

happy <- happy %>% 
  rename(GDP = Economy..GDP.per.Capita.,
         Health = Health..Life.Expectancy.,
         `Government Corruption` = Trust..Government.Corruption.
         )

glimpse(happy)

#Look at 10 happiest and 10 saddest countries
happy %>% 
  head(n=10) %>% 
  select(Happiness.Rank, Country, Region, Happiness.Score)

top10 <- c("Norway", "Denmark", "Iceland", "Switzerland", "Finland", "Netherlands", "Canada", "New Zealand", "Sweden", "Australia")

#Melt variables by country so we can plot
happy_by_country <- happy %>% 
  select(Country, GDP:`Government Corruption`) %>% 
  melt(id = c("Country"))

#Plot top 10 countries
happy_by_country %>% 
  filter(Country %in% top10) %>% 
  ggplot(mapping = aes(variable, value)) +
  geom_line(aes(color = Country, group = Country), size = 2) +
  xlab("Happiness Variables") + ylab("Score Contribution") + ggtitle("Happiness Variable Score Contribution for Top 10 Countries") +
  theme_grey(base_size = 22)

happy %>% 
  tail(n=10) %>% 
  select(Happiness.Rank, Country, Region, Happiness.Score)

low10 <- c("Yemen", "South Sudan", "Liberia", "Guinea", "Togo", "Rwanda", "Syria", "Tanzania", "Burundi", "Central African Republic")

#Plot low 10 countries
happy_by_country %>% 
  filter(Country %in% low10) %>% 
  ggplot(mapping = aes(variable, value)) +
  geom_line(aes(color = Country, group = Country), size = 2) +
  xlab("Happiness Variables") + ylab("Score Contribution") + ggtitle("Happiness Variable Score Contribution for Low 10 Countries") +
  theme_grey(base_size = 22)


#Happiness scores by region
happy %>% 
  filter(is.na(Region) != TRUE) %>% 
  ggplot(mapping = aes(x = reorder(Region, Happiness.Score, mean), Happiness.Score)) +
  geom_boxplot(aes(fill = Region)) +
  xlab("World Region") +ggtitle("Happiness Score by Region") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme_grey(base_size = 22) +
  scale_x_discrete(labels = function(Region) str_wrap(Region, width = 5))

#Melt happiness variables into region so we can plot
happy_by_region <- happy %>% 
  select(Region, GDP:`Government Corruption`) %>% 
  melt(id = c("Region"))

#Happiness Variable contribution by Region
happy_by_region %>% 
  filter(is.na(Region) != TRUE) %>% 
  ggplot(mapping = aes(variable, value)) +
  geom_boxplot(aes(fill = Region)) +
  scale_x_discrete(labels = function(variable) str_wrap(variable, width = 10)) +
  xlab("Happiness Variables") + ylab("Score Contribution") + ggtitle("Happiness Variable Contribution Levels by World Region") +
  theme_grey(base_size = 17) +
  theme(legend.position = "none") + 
  facet_wrap(~ Region)

#Boxplots of Overall happiness variables contributions
happy_by_country %>% 
  ggplot(mapping= aes(variable,value)) +
  geom_boxplot(aes(fill = variable)) +
  xlab("Happiness Varirables") + ylab("Score Contribution") + ggtitle("Happiness Variable Contributions") +
  theme(legend.position = "none")

#Plot the 5 countries we're looking at
country.list <- c("Russia", "United States", "Greece", "France", "United Kingdom")


happy_by_country %>% 
  filter(Country %in% country.list) %>% 
  ggplot(mapping = aes(variable, value)) +
  geom_line(aes(color = Country, group = Country), size = 2) +
  xlab("Happiness Variable") + ylab("Score Contribution") + ggtitle("Happiness Variable Contributions") +
  theme_grey(base_size = 22)



    
