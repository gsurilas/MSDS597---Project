library(tidyverse)
library(tidytext)
library(tokenizers)
library(gutenbergr)
library(rvest)


authors <- as.tibble(data.frame(author=character(0), Mean_Book_Score=numeric(0), stringsAsFactors = FALSE))

##Sentiments by Authors function

book_sentiments <- function(author, book_ids){
  
  books <-gutenberg_download(gutenberg_id = book_ids) %>% 
    unnest_tokens(word,text) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("afinn"))
    
  result <- mean(books$score)
  authors <- add_row(authors, author = author, Mean_Book_Score = result)
}

#Russian Authors
authors <- book_sentiments("Leo_Tolstoy", book_ids = c(1399,2600))
authors <- book_sentiments("Fyodor_Dostoevsky", book_ids = c(2197,2554,2638,28054,8117))
authors <- book_sentiments("Nikolai_Gogol", book_ids = c(1081,36238))
authors <- book_sentiments("Ivan_Turgenev", book_ids = c(30723,8597,8744))
authors <- book_sentiments("Anton_Chekhov", book_ids = c(1753,1732,13414))
authors <- book_sentiments("Maxim_Gorky", book_ids = c(681,2288,2709,3783,51094))
authors <- book_sentiments("Alexander_Pushkin", book_ids = c(4344,5089,13511,23058))
authors <- book_sentiments("Ivan_Bunin", book_ids = c(44998))
authors <- book_sentiments("Mikhail_Lermontov", book_ids = c(913))
authors <- book_sentiments("F._Scott_Fitzgerald", book_ids = c(805,6695,9830))

#American Authors
authors <- book_sentiments("Mark_Twain", book_ids = c(74,76,86,1837))
authors <- book_sentiments("Nathaniel_Hawthorne", book_ids = c(33,77))
authors <- book_sentiments("Herman_Melville", book_ids = c(15))
authors <- book_sentiments("Edith_Wharton", book_ids = c(284,541))
authors <- book_sentiments("Kurt_Vonnegut", book_ids = c(21279,30240))
authors <- book_sentiments("Emily_Dickinson", book_ids = c(2678,2679,12242))
authors <- book_sentiments("Ray_Bradbury", book_ids = c(41622,41624,41627,51171))
authors <- book_sentiments("Sinclair_Lewis", book_ids = c(543,1156))
authors <- book_sentiments("Sherwood_Anderson", book_ids = c(416,7048))
authors <- book_sentiments("Henry_David_Thoreau", book_ids = c(205))

#French Authors
authors <- book_sentiments("Marcel_Proust", book_ids = c(7178))
authors <- book_sentiments("Gustave_Flaubert", book_ids = c(25053,25014,27537,27575,34828,4609))
authors <- book_sentiments("Victor_Hugo", book_ids = c(2610))
authors <- book_sentiments("%C3%89mile_Zola ", book_ids = c(1069,5135,5744,6626,7011,9499,10330,10720,13851,13695))
authors <- book_sentiments("Honor%C3%A9_de_Balzac", book_ids = c(1237,469,1189,1196,1215,1223,1230,
                                                                 1242,1277,1294,1305,1307,1343,1344,1345,
                                                                 1350,1352,1357,1369,1373,1374,1380,1389,1403,1405,
                                                                 1410,1411,1425,1426,1427,1428,1432,1433,1437,1443,
                                                                 1453,1454,1455,1456,1474,1475,1481,1482,1553,1554,
                                                                 1555,1556,1559,1569,1639,1641,1649,1659,1680,1683,1704,1710,
                                                                 1714,1715,1729,1737,1749,1810,1811,1813,1826,1841,1854,
                                                                 1856,1871,1873,1884,1899,1912,1921,1940,1941,1942,1943,
                                                                 1950,1954,1957,1967,5873,7416,7927,7929,7950,7958,8079,
                                                                 12900,13159))
authors <- book_sentiments("Voltaire", book_ids = c(18569,19942,18972))
authors <- book_sentiments("Alexandre_Dumas", book_ids = c(965,1257,1258,1259,2759,1189))
authors <- book_sentiments("Charles_Bauderlaire", book_ids = c(36098))
authors <- book_sentiments("Jules_Verne", book_ids = c(83,103,164,3748))
authors <- book_sentiments("Guy_de_Maupassant", book_ids = c(7114,7549,3090))
authors <- book_sentiments("Denis_Diderot", book_ids = c(34544))
authors <- book_sentiments("Ren%C3%A9_Descartes", book_ids = c(59,4391))
authors <- book_sentiments("Stendhal", book_ids = c(44747))

#British Authors
authors <- book_sentiments("Charles_Dickens", book_ids = c(46,98,564,580,700,730,766,883,963,967,968,1023,1400))
authors <- book_sentiments("Jane_Austen", book_ids = c(105,121,141,158,161,1342,946))
authors <- book_sentiments("Charlotte_Bront%C3%AB", book_ids = c(1028,1260,9182,30486))
authors <- book_sentiments("George_Eliot", book_ids = c(145,507,550,6688))
authors <- book_sentiments("Thomas_Hardy", book_ids = c(110,153))
authors <- book_sentiments("William_Shakespeare", book_ids = c(1041,1045,seq(1500,1544),2247,2250,seq(2253,2260),2262,2265,2266,2267))
authors <- book_sentiments("Virginia_Woolf", book_ids = c(144,1245,5670,29220))
authors <- book_sentiments("Joseph_Conrad", book_ids = c(219,220,1083,974,2021,5658))
authors <- book_sentiments("Lewis_Carroll", book_ids = c(11,12,13))
authors <- book_sentiments("William_Wordsworth", book_ids = c(8774,8824,8905,8912,10219,12145,12383,32459))
authors <- book_sentiments("Geoffrey_Chaucer", book_ids = c(257,2383))
authors <- book_sentiments("Mary_Shelley", book_ids = c(61))
authors <- book_sentiments("Rudyard_Kipling", book_ids = c(236,21777,1937))
authors <- book_sentiments("Franics_Bacon", book_ids = c(575,2434,5500))
authors <- book_sentiments("Emily_Bront%C3%AB", book_ids = c(768))

#Greek Authors
authors <- book_sentiments("HomerB", book_ids = c(2199,3160))
authors <- book_sentiments("Euripides", book_ids = c(15081,35170,35171,35173,35451,10523))
authors <- book_sentiments("Aristotle", book_ids = c(1974,2412,6762,6763))
authors <- book_sentiments("Plato", book_ids = c(150))
authors <- book_sentiments("Sophocles", book_ids = c(31))
authors <- book_sentiments("Aristophanes", book_ids = c(2562,2571,3012,3013,7700,7998))
authors <- book_sentiments("Thucydides", book_ids = c(9074))
authors <- book_sentiments("Sappho", book_ids = c(42166))
authors <- book_sentiments("Hesiod", book_ids = c(348))
authors <- book_sentiments("Demosthenes", book_ids = c(9060,9061))
authors <- book_sentiments("Aesop", book_ids = c(21))

