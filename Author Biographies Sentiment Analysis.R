library(tidyverse)
library(tidytext)
library(tokenizers)
library(gutenbergr)
library(rvest)

gutenberg_works(str_detect(author, "Bunin"))

urls <- c("https://en.wikipedia.org/wiki/Fyodor_Dostoevsky",
          "https://en.wikipedia.org/wiki/Leo_Tolstoy",
          "https://en.wikipedia.org/wiki/Nikolai_Gogol",
          "https://en.wikipedia.org/wiki/Ivan_Turgenev",
          "https://en.wikipedia.org/wiki/Anton_Chekhov",
          "https://en.wikipedia.org/wiki/Maxim_Gorky",
          "https://en.wikipedia.org/wiki/Yevgeny_Zamyatin",
          "https://en.wikipedia.org/wiki/Alexander_Pushkin",
          "https://en.wikipedia.org/wiki/Aleksandr_Solzhenitsyn",
          "https://en.wikipedia.org/wiki/Vladimir_Nabokov",
          "https://en.wikipedia.org/wiki/Mikhail_Bulgakov",
          "https://en.wikipedia.org/wiki/Ivan_Bunin",
          "https://en.wikipedia.org/wiki/Ivan_Goncharov",
          "https://en.wikipedia.org/wiki/Varlam_Shalamov",
          "https://en.wikipedia.org/wiki/Boris_Pasternak",
          "https://en.wikipedia.org/wiki/Mikhail_Lermontov",
          "https://en.wikipedia.org/wiki/Mikhail_Sholokhov",
          "https://en.wikipedia.org/wiki/Vasily_Grossman",
          "https://en.wikipedia.org/wiki/Lyudmila_Ulitskaya",
          "https://en.wikipedia.org/wiki/Marina_Tsvetaeva",
          "https://en.wikipedia.org/wiki/Evgeny_Chirikov",
          "https://en.wikipedia.org/wiki/Sergei_Yesenin",
          "https://en.wikipedia.org/wiki/Anna_Akhmatova",
          "https://en.wikipedia.org/wiki/Sophia_Parnok",
          "https://en.wikipedia.org/wiki/Maria_Rybakova",
          "https://en.wikipedia.org/wiki/Ernest_Hemingway",
          "https://en.wikipedia.org/wiki/William_Faulkner",
          "https://en.wikipedia.org/wiki/Toni_Morrison",
          "https://en.wikipedia.org/wiki/Maya_Angelou",
          "https://en.wikipedia.org/wiki/F._Scott_Fitzgerald",
          "https://en.wikipedia.org/wiki/Mark_Twain",
          "https://en.wikipedia.org/wiki/John_Steinbeck",
          "https://en.wikipedia.org/wiki/Nathaniel_Hawthorne",
          "https://en.wikipedia.org/wiki/Ralph_Ellison",
          "https://en.wikipedia.org/wiki/Herman_Melville",
          "https://en.wikipedia.org/wiki/Philip_Roth",
          "https://en.wikipedia.org/wiki/Edith_Wharton",
          "https://en.wikipedia.org/wiki/Harper_Lee",
          "https://en.wikipedia.org/wiki/Joseph_Heller",
          "https://en.wikipedia.org/wiki/Kurt_Vonnegut",
          "https://en.wikipedia.org/wiki/Emily_Dickinson",
          "https://en.wikipedia.org/wiki/J._D._Salinger",
          "https://en.wikipedia.org/wiki/James_Baldwin",
          "https://en.wikipedia.org/wiki/Sylvia_Plath",
          "https://en.wikipedia.org/wiki/Thomas_Pynchon",
          "https://en.wikipedia.org/wiki/Ray_Bradbury",
          "https://en.wikipedia.org/wiki/Truman_Capote",
          "https://en.wikipedia.org/wiki/Sinclair_Lewis",
          "https://en.wikipedia.org/wiki/Tennessee_Williams",
          "https://en.wikipedia.org/wiki/Sherwood_Anderson",
          "https://en.wikipedia.org/wiki/Raymond_Chandler",
          "https://en.wikipedia.org/wiki/Henry_David_Thoreau",
          "https://en.wikipedia.org/wiki/Flannery_O%27Connor",
          "https://en.wikipedia.org/wiki/Albert_Camus",
          "https://en.wikipedia.org/wiki/Marcel_Proust",
          "https://en.wikipedia.org/wiki/Gustave_Flaubert",
          "https://en.wikipedia.org/wiki/Victor_Hugo",
          "https://en.wikipedia.org/wiki/%C3%89mile_Zola",
          "https://en.wikipedia.org/wiki/Honor%C3%A9_de_Balzac",
          "https://en.wikipedia.org/wiki/Voltaire",
          "https://en.wikipedia.org/wiki/Jean-Paul_Sartre",
          "https://en.wikipedia.org/wiki/Moli%C3%A8re",
          "https://en.wikipedia.org/wiki/Simone_de_Beauvoir",
          "https://en.wikipedia.org/wiki/Alexandre_Dumas",
          "https://en.wikipedia.org/wiki/Charles_Baudelaire",
          "https://en.wikipedia.org/wiki/Jules_Verne",
          "https://en.wikipedia.org/wiki/Guy_de_Maupassant",
          "https://en.wikipedia.org/wiki/Arthur_Rimbaud",
          "https://en.wikipedia.org/wiki/Fran%C3%A7ois_Rabelais",
          "https://en.wikipedia.org/wiki/Denis_Diderot",
          "https://en.wikipedia.org/wiki/Ren%C3%A9_Descartes",
          "https://en.wikipedia.org/wiki/Georges_Perec",
          "https://en.wikipedia.org/wiki/Marquis_de_Sade",
          "https://en.wikipedia.org/wiki/Stendhal",
          "https://en.wikipedia.org/wiki/Charles_Dickens",
          "https://en.wikipedia.org/wiki/Jane_Austen",
          "https://en.wikipedia.org/wiki/Charlotte_Bront%C3%AB",
          "https://en.wikipedia.org/wiki/George_Eliot",
          "https://en.wikipedia.org/wiki/J._R._R._Tolkien",
          "https://en.wikipedia.org/wiki/J._K._Rowling",
          "https://en.wikipedia.org/wiki/George_Orwell",
          "https://en.wikipedia.org/wiki/Thomas_Hardy",
          "https://en.wikipedia.org/wiki/William_Shakespeare",
          "https://en.wikipedia.org/wiki/Virginia_Woolf",
          "https://en.wikipedia.org/wiki/Joseph_Conrad",
          "https://en.wikipedia.org/wiki/C._S._Lewis",
          "https://en.wikipedia.org/wiki/Roald_Dahl",
          "https://en.wikipedia.org/wiki/Lewis_Carroll",
          "https://en.wikipedia.org/wiki/William_Wordsworth",
          "https://en.wikipedia.org/wiki/Geoffrey_Chaucer",
          "https://en.wikipedia.org/wiki/Mary_Shelley",
          "https://en.wikipedia.org/wiki/Samuel_Taylor_Coleridge",
          "https://en.wikipedia.org/wiki/Aldous_Huxley",
          "https://en.wikipedia.org/wiki/Rudyard_Kipling",
          "https://en.wikipedia.org/wiki/Francis_Bacon",
          "https://en.wikipedia.org/wiki/Emily_Bront%C3%AB",
          "https://en.wikipedia.org/wiki/D._H._Lawrence",
          "https://en.wikipedia.org/wiki/Jonathan_Swift",
          "https://en.wikipedia.org/wiki/T._S._Eliot",
          "https://en.wikipedia.org/wiki/John_Milton",
          "https://en.wikipedia.org/wiki/Samuel_Johnson",
          "https://en.wikipedia.org/wiki/H._G._Wells",
          "https://en.wikipedia.org/wiki/Alfred,_Lord_Tennyson",
          "https://en.wikipedia.org/wiki/E._M._Forster",
          "https://en.wikipedia.org/wiki/Wilfred_Owen",
          "https://en.wikipedia.org/wiki/Evelyn_Waugh",
          "https://en.wikipedia.org/wiki/Agatha_Christie",
          "https://en.wikipedia.org/wiki/Homer",
          "https://en.wikipedia.org/wiki/Euripides",
          "https://en.wikipedia.org/wiki/Aristotle",
          "https://en.wikipedia.org/wiki/Plato",
          "https://en.wikipedia.org/wiki/Sophocles",
          "https://en.wikipedia.org/wiki/Aristophanes",
          "https://en.wikipedia.org/wiki/Herodotus",
          "https://en.wikipedia.org/wiki/Aeschylus",
          "https://en.wikipedia.org/wiki/Thucydides",
          "https://en.wikipedia.org/wiki/Nikos_Kazantzakis",
          "https://en.wikipedia.org/wiki/Sappho",
          "https://en.wikipedia.org/wiki/Plutarch",
          "https://en.wikipedia.org/wiki/Hesiod",
          "https://en.wikipedia.org/wiki/Xenophon",
          "https://en.wikipedia.org/wiki/Demosthenes",
          "https://en.wikipedia.org/wiki/Aesop"
          )


countries <- c(rep("Russia", 25), rep("United States",28), rep("France", 21), rep("United Kingdom", 33), rep("Greece", 16))
       
##Initialize tibble that bio_sentiments table will fill
biographies <- as.tibble(data.frame(author=character(0), country = character(0), Mean_Bio_Score=numeric(0), stringsAsFactors = FALSE))

##Function that produces a tibble of Sentiments of Author Biographies
bio_sentiments <- function(urls, countries){

  
  if(length(urls) != length(countries)){
    stop ("Both lists must be of equal length")
  }
  for(i in 1:length(urls)){
    
    #Convert webpage text from list of lists, to one large list of all text
    url<- urls[i]
    bio <- url %>% 
      read_html() %>% 
      html_nodes("p") %>% 
      html_text() %>% 
      unlist() 
    
    #Convert list of text into a tibble of words with sentiment score
    bio <- tibble(text = bio) %>% 
      mutate(paragraph = row_number()) %>% 
      unnest_tokens(word,text) %>% 
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("afinn")) 
    
    author <- str_split(url,"/")[[1]][5]
    result <- mean(bio$score)
    country <- countries[i]
    
    ##Add row of data to tibble
    biographies <- add_row(biographies, author = author, country = country, Mean_Bio_Score = result)
  }
  
  ##Return filled out tibble as result
  biographies
}

##Create Biography Sentiments table
biographies <- bio_sentiments(urls,countries)


print(biographies, n = Inf)
