library(tidyverse)
library(reshape2)
library(ggrepel)

##Join authors, biographies and authors.nrc table into one large table
authors_and_bios <- biographies %>% 
  left_join(authors) %>% 
  left_join(authors.nrc)

#Clean up e with accents in Authors column of authors_and_bios
authors_and_bios$author <- str_replace_all(authors_and_bios$author, "%.*%[A-Z|1-9][A-Z|1-9]", "e")
#Clean up O in Flannery O Connor
authors_and_bios$author <- str_replace_all(authors_and_bios$author, "O%[1-9]{2}", "O'")
#Get rid of underscores in names
authors_and_bios$author <- str_replace_all(authors_and_bios$author, "_", " ")

authors_and_bios %>%
  filter(is.na(Mean_Book_Score) == FALSE) %>% 
  ggplot(mapping = aes(Mean_Bio_Score, Mean_Book_Score)) +
  geom_point(aes(color = country), size=4) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label=author), size = 6, nudge_y = .025) +
  theme_grey(base_size = 22) +
  xlab("Mean Biography Score") + ylab("Mean Book Score")


authors_and_bios %>% 
  ggplot(mapping = aes(country,Mean_Bio_Score)) +
  geom_boxplot(aes(fill = country)) +
  xlab("Country") + ylab("Mean Biography Score") + ggtitle("Author's Happiness by Country") +
  theme_grey(base_size = 22) +
  theme(legend.position = "none") 

authors_and_bios %>% 
  ggplot(mapping = aes(country,Mean_Book_Score)) +
  geom_boxplot(aes(fill = country)) +
  xlab("Country") + ylab("Mean Book Score") + ggtitle("Average Book Sentiment by Country") +
  theme_grey(base_size = 22) +
  theme(legend.position = "none") 

print(authors_and_bios, n=Inf)

#Create a new tibble with NRC emotions by country stacked on each other so boxplots can be made
emotions_by_country <- authors_and_bios %>% 
  select(country, anger.prop:trust.prop) %>% 
  melt(id = "country")

#Emotion names list to use in emotions_labeller function
emotion_names <- list(
  "anger.prop" = "Anger",
  "anticipation.prop" = "Anticipation",
  "disgust.prop" = "Disgust",
  "fear.prop" = "Fear",
  "joy.prop" = "Joy",
  "sadness.prop" = "Sadness",
  "surprise.prop" = "Surprise",
  "trust.prop" = "Trust"
)

#Emotions labeller function to use in re-writing facet wrap labels so the titles look better than anger.prop
emotions_labeller <- function(variable,value){
  return (emotion_names[value])
}

#Boxplots of NRC emotions by country
emotions_by_country %>% 
  ggplot(mapping = aes(country, value)) +
  geom_boxplot(aes(fill = country)) +
  facet_wrap(~variable, labeller = emotions_labeller) +
  xlab("Country") + ylab("Proportion") + ggtitle("Proportion of NRC Words used by Country") +
  theme_grey(base_size = 20)

#Create a new tibble with NRC emotions by author stacked on each other so dotplots can be made
emotions_by_author <- authors_and_bios %>% 
  select(author, country, anger.prop:trust.prop) %>% 
  melt(id = c("author", "country"))

#Emotions by Russian Authors
emotions_by_author %>% 
  filter(country == "Russia") %>% 
  ggplot(mapping = aes(author, value)) +
  geom_point(aes(fill = country)) +
  geom_label_repel(aes(label = author), segment.color = NA) +
  facet_wrap(~variable, labeller = emotions_labeller) +
  theme_grey(base_size = 22) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  xlab("Author") + ylab("Proportion") + ggtitle("NRC Words used by Russian Authors")

#Emotions by British Authors
emotions_by_author %>% 
  filter(country == "United Kingdom") %>% 
  ggplot(mapping = aes(author, value)) +
  geom_point(aes(fill = country)) +
  geom_label_repel(aes(label = author), segment.color = NA) +
  facet_wrap(~variable, labeller = emotions_labeller) +
  theme_grey(base_size = 22) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  xlab("Author") + ylab("Proportion") + ggtitle("NRC Words used by UK Authors") 

#Emotions by French Authors
emotions_by_author %>% 
  filter(country == "France") %>% 
  ggplot(mapping = aes(author, value)) +
  geom_point(aes(fill = country)) +
  geom_label_repel(aes(label = author), segment.color = NA) +
  facet_wrap(~variable, labeller = emotions_labeller) +
  theme_grey(base_size = 22) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  xlab("Author") + ylab("Proportion") + ggtitle("NRC Words used by French Authors") 

#Emotions by Greek Authors
emotions_by_author %>% 
  filter(country == "Greece") %>% 
  ggplot(mapping = aes(author, value)) +
  geom_point(aes(fill = country)) +
  geom_label_repel(aes(label = author), segment.color = NA) +
  facet_wrap(~variable, labeller = emotions_labeller) +
  theme_grey(base_size = 22) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  xlab("Author") + ylab("Proportion") + ggtitle("NRC Words used by Greek Authors")

#Emotions by American Authors
emotions_by_author %>% 
  filter(country == "United States") %>% 
  ggplot(mapping = aes(author, value)) +
  geom_point(aes(fill = country)) +
  geom_label_repel(aes(label = author), segment.color = NA) +
  facet_wrap(~variable, labeller = emotions_labeller) +
  theme_grey(base_size = 22) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  xlab("Author") + ylab("Proportion") + ggtitle("NRC Words used by American Authors") 
