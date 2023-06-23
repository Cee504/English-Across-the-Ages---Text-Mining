#Created with:
  
#Silge, Julia, and David Robinson. Text Mining with R. “O’Reilly Media, Inc.,” 12 June 2017.
#Wickham, Hadley, and Garrett Grolemund. R for Data Science. “O’Reilly Media, Inc.,” 12 Dec. 2016.


#library items

install.packages("gutenbergr")
devtools::install_github("ropensci/gutenbergr")

library(gutenbergr)
library(janeaustenr)
library(tidytext)
library(tidyr)
library(dplyr)
library(stringi)
library(stringr)
library(qdap)
library(rJava)
library(tidyverse)
library(tm)

#Organizing - Beowolf
#link  https://www.gutenberg.org/files/16328/16328-h/16328-h.htm

beowulf <- gutenberg_download(16328, mirror =
                                "http://mirrors.xmission.com/gutenberg/")

#Removing intro and addenda
beowulf_two <- beowulf[-c(1:907),]
beowulf_clean <- beowulf_two[-c(5623:5696),]

#Removing blank rows
beowulf_clean <- beowulf_clean[!beowulf_clean$text == "", ]

#Add author and epoch column
beowulf_clean$Author <- "Beowulf"
beowulf_clean$Epoch <- "Old"
beowulf_clean

#Organizing - Chaucer
#link https://www.gutenberg.org/cache/epub/2383/pg2383.html

chaucer <- gutenberg_download(2383, mirror =
                                "http://mirrors.xmission.com/gutenberg/")
head(chaucer)
tail(chaucer)

#Removing intro and other poems
chaucer_two <- chaucer[-c(1:1290),]
chaucer_clean <- chaucer_two[-c(23546:35220),]

#Removing blank rows
chaucer_clean <- chaucer_clean[!chaucer_clean$text == "", ]

#Add author column
chaucer_clean$Author <- "Chaucer"
chaucer_clean$Epoch <- "Middle"
chaucer_clean

#Organizing - Shakespeare
#link https://www.gutenberg.org/files/1524/1524-h/1524-h.htm

hamlet <- gutenberg_download(1524, mirror =
                                "http://mirrors.xmission.com/gutenberg/")

#Removing intro
hamlet_clean <- hamlet[-c(1:70),]


#Removing blank rows
hamlet_clean <- hamlet_clean[!hamlet_clean$text == "", ]

#Add author column
hamlet_clean$Author <- "Shakespeare"
hamlet_clean$Epoch <- "Modern"
hamlet_clean


#Organizing - Jane Austen
jane_books <- austen_books() %>%
  group_by(book) %>% mutate(linenumber=row_number(),
                            chapter=cumsum(str_detect(text,regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()

jane_clean <- jane_books[!jane_books$text == "", ]
jane_clean

#Add author column
jane_clean$Author <- "Austen"
jane_clean$Epoch <- "Modern"
jane_clean

#remove extra columns
jane_bind <- jane_clean %>% select(-c(book,linenumber, chapter))


#combine into 1

book_corpus <- rbind(beowulf_clean, chaucer_clean, hamlet_clean)

book_corpus_bind <- book_corpus %>% select(-gutenberg_id)

full_corpus <- rbind(book_corpus_bind, jane_bind)

#No author

no_author <- full_corpus %>% select(-c(Author, Epoch))


#creating custom stopwords

word <- c("hamlet","claudius","the ghost","gertrude",
          "polonius", "laertes","ophelia","horatio","fortinbras","voltemand",
          "cornelius", "rosencrantz", "guildenstern", "marcellus", "barnardo",
          "francisco", "osric", "reynaldo", "otway", "owens", "grandison", "admiral brand", "agatha", "alicia", "anderson"
          , "andrews", "anne", "augusta", "austen", "bates", "bennet", "bennets", "benwick", "bertram",
          "bertrams", "bickerton", "bingley", "brandon", "byron", "campbell", "carter",
          "carteret", "catherine", "chapter", "charles", "charlotte", "christopher",
          "churchill", "colonel fitzwilliam", "count cassel", "cox", "crawford", "dalrymple",
          "darcy", "dashwood", "dashwoods", "dr grant", "drummond", "edmund", "edward",
          "elinor", "eliza", "elizabeth", "elizabeth", "fanny", "elinor", "emma", "elliot", "fairfax", "fanny", "fanny price",
          "ferrars", "fitzwilliam", "forster", "frasers", "frederick", "george", "hamilton", "harriet", "elton",
          "harriet smith", "harville", "hawkins", "hayter", "henry", "jackson", "james",
          "jane", "john", "julia", "knightley", "lacey", "larolles", "lee", "longtown",
          "louisa", "lucas", "lucy", "lydia", "maddox", "maria", "marianne", "martin",
          "mary", "mary king", "middleton", "miss de", "miss frances", "miss grey","miss price",
          "miss smith", "miss sparks", "miss steele", "miss steeles", "miss ward", "morland",
          "morton", "musgrove", "musgroves", "nash", "norris", "phillips", "ravenshaw",
          "robert", "rooke", "russell", "shirley", "sneyd", "taylor", "thomas", "thornton",
          "thorpe", "thorpes", "tilney", "tom", "wallis", "walter", "wentworth", "weston",
          "wickham", "william", "williams", "woodhouse", "miss smith", "miss crawford's", "dr grant", "miss price", "sir thomas's", 
          "de bourgh", "mansfield park", "lady russell's", "harriet smith", "miss steeles", "captain wentworth's", 
          "kellynch", "miss steele", "colonel brandon's", "maple grove", "scene ii", "scene", "beowulf", "hrothgar",
          "jennings", "isabella", "rushworth", "grendel", "beowulf's", "higelac", "_exit", "_exeunt", "chaucer", "mansfield", "willoughby",
          "crawford's", "smith", "dr", "thomas's", "russell's", "wentworth's", "steeles", "bourgh",
          "brandon's", "steele", "churchill's", "price")
lexicon <-  rep("custom.stop", times=length(word))


custom.stopwords <- data.frame(word, lexicon)
names(custom.stopwords) <- c("word", "lexicon")

stop_words <-  dplyr::bind_rows(stop_words, custom.stopwords)
stop_words



#Question 1 - Word Frequencies
#Most Common Words - tf

#unnest tokens
full_word <- full_corpus %>% unnest_tokens(word, text)
noauthor_word <- no_author %>% unnest_tokens(word, text)

#remove stop words
tidy_fullword <- full_word %>% anti_join(stop_words) 
tidy_noauthor <- noauthor_word %>% anti_join(stop_words)

#common words

tidy_fullword %>% count(word, sort = TRUE)
tidy_noauthor %>% count(word, sort = TRUE) 
 
#count books by author - tf

count_fullword <- tidy_fullword %>%
  count(Author, word,sort = TRUE) %>%
  ungroup()

total_fullword <- count_fullword %>%
  group_by(Author) %>%
  summarize(total = sum(n))

book_words_author <- left_join(count_fullword, total_fullword)

freq_byauthor <- book_words_author %>%
  group_by(Author) %>%
  mutate(rank = row_number(),
         'term frequency' = n/total)

#graphing counts by author - tf

freq_byauthor %>%
  arrange(desc(n)) %>%
  mutate(word = factor (word, levels = rev(unique(word)))) %>%
  group_by(Author) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, n, bill = Author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~Author, ncol = 2, scales = "free") +
  coord_flip()

# graph - no author

tidy_noauthor %>%
  count(word, sort = TRUE) %>% filter(n > 600) %>% mutate(word=reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) + coord_flip() 

#Most Common Words by author - tf_dif

no_epoch<- tidy_fullword %>% 
  select(-Epoch) 

no_epoch %>%
  count(word, sort = TRUE)

no_epoch_counts <- no_epoch %>%
  count(Author, word, sort = TRUE)

book_word_idf <- no_epoch_counts %>%
  bind_tf_idf(word, Author, n)

book_word_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(Author) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, n, fill = Author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~Author, ncol = 2, scales = "free") +
  coord_flip()

# word frequency - no author tf_idf

tidy_noauthor2 <- tidy_noauthor %>%
  count(word, sort = TRUE)

tidy_noauthor_idf <- tidy_noauthor2 %>%
  bind_tf_idf(word, word, n)

tidy_noauthor_idf %>%
  filter(n > 600) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) + coord_flip()  


#Question 2 - Most common phrases
#Bigrams by author tf_idf

books_bigrams <- full_corpus %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

books_bigrams

books_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- books_bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(Author, word1, word2, sort = TRUE)

bigram_counts
bigram_counts = bigram_counts[-1:-4,]

#plotting bigrams by author - tf_idf

bigram_plot <- bigram_counts

bigram_plot$bigram <- paste(bigram_plot$word1, bigram_plot$word2, sep = " ")

bigram_plot_author <- bigram_plot %>%
  bind_tf_idf(bigram, Author, n)

bigram_plot_author %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor (bigram, levels = rev(unique(bigram)))) %>%
  group_by(Author) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(bigram, n, bill = Author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~Author, ncol = 2, scales = "free") +
  coord_flip()

#bigrams - no author tf_idf

noauthor_bigrams <- no_author %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

noauthor_bigrams %>%
  count(bigram, sort = TRUE)

noauthor_separated <- noauthor_bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ")

noauthor_filtered <- noauthor_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

noauthor_counts <- noauthor_filtered %>%
  count(word1, word2, sort = TRUE)

noauthor_counts

# graphing bigrams - no author td_idf

bigram_plot2 <- noauthor_counts

bigram_plot2$bigram <- paste(bigram_plot2$word1, bigram_plot2$word2, sep = " ")

bigram_plot2 %>% select(-c(word1, word2))
bigram_plot2 = bigram_plot2[-1,]

bigram_plotplot <- bigram_plot2 %>%
  bind_tf_idf(word1, word2, n)

bigram_plotplot2 <- bigram_plotplot %>% filter(n > 20)

bigram_plotplot2 %>%
  arrange(desc(tf_idf)) %>%
  top_n(10) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) + coord_flip()


#Trigrams - by author, tf_idf

books_trigrams <- full_corpus %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

books_trigrams

books_trigrams %>%
  count(trigram, sort = TRUE)

trigrams_separated <- books_trigrams %>%
  separate(trigram, c("word1","word2","word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

trigram_counts <- trigrams_filtered %>%
  count(Author, word1, word2, word3, sort = TRUE)

trigram_counts
trigram_counts = trigram_counts[-1:-4,]

#plotting trigrams - by author tf_idf

trigram_plot <- trigram_counts

trigram_plot$trigram <- paste(trigram_plot$word1, trigram_plot$word2, trigram_plot$word3, sep = " ")

trigram_plot_author <- trigram_plot %>%
  bind_tf_idf(trigram, Author, n)

trigram_plot_author %>%
  arrange(desc(tf_idf)) %>%
  mutate(trigram = factor (trigram, levels = rev(unique(trigram)))) %>%
  group_by(Author) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(trigram, n, bill = Author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~Author, ncol = 2, scales = "free") +
  coord_flip()

#trigrams, no author tf_idf

noauthor_trigrams <- no_author %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

noauthor_trigrams %>%
  count(trigram, sort = TRUE)

noauthor_separated_tri <- noauthor_trigrams %>%
  separate(trigram, c("word1","word2", "word3"), sep = " ")

noauthor_filtered_tri <- noauthor_separated_tri %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

noauthor_counts_tri <- noauthor_filtered_tri %>%
  count(word1, word2, word3, sort = TRUE)

noauthor_counts_tri

# graphing trigrams no author

trigram_plot3 <- noauthor_counts_tri

trigram_plot3$trigram <- paste(trigram_plot3$word1, trigram_plot3$word2, trigram_plot3$word3, sep = " ")

trigram_plot3 %>% select(-c(word1, word2, word3))

trigram_plot3 = trigram_plot3[-1,]

trigram_plot4 <- trigram_plot3 %>%
  bind_tf_idf(trigram, trigram, n)

trigramplot_plot <- trigram_plot4 %>% filter(n > 4)

trigramplot_plot %>%
  arrange(desc(tf_idf))%>%
  ggplot(aes(trigram,n)) +
  geom_col() +
  xlab(NULL) + coord_flip() 


#Question 3 - Epoch Schism
#words by time  - tf_idf

#creating data set by epoch
full_tidy <- full_corpus %>% unnest_tokens(word, text) %>% select(-Author)
tidy_books_time <- full_tidy %>% anti_join(stop_words)

full_tidy2 <- tidy_books_time %>%
  count(Epoch, word, sort = TRUE)

tidy_books_time <- full_tidy2 %>%
  bind_tf_idf(word, Epoch, n) %>%
  arrange(desc(tf_idf)) 

frequency <- tidy_books_time %>%
  group_by(Epoch) %>%
  left_join(tidy_books_time %>%
             group_by(Epoch) %>%
             summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>%
  select(Epoch, word, freq) %>%
  spread(Epoch, freq) %>%
  arrange(Old, Middle, Modern)

#plot frequency by time

library(scales)

#old vs middle
ggplot(frequency, aes(Old, Middle)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "blue")

#middle vs modern

ggplot(frequency, aes(Middle, Modern)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "blue")

#old vs modern

ggplot(frequency, aes(Old, Modern)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "blue")

#Comparing word usage

#old vs middle
word_ratios_old <- tidy_books_time %>%
  arrange(desc(tf_idf)) %>%
  filter(sum(n) >= 300) %>%
  ungroup() %>%
  spread(Epoch, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. +1) / sum(. + 1))) %>%
  mutate(logratio = log(Old / Middle)) %>%
  arrange(desc(logratio))

word_ratios_old %>% arrange(abs(logratio))

word_ratios_old %>% group_by(logratio < 0 ) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0 )) +
  geom_col(sho.legend = FALSE) +
  coord_flip() +
  ylab("log odds of Old or Middle English") +
  scale_fill_discrete(name = "", labels = c("Old", "Middle"))

#Middle vs Modern

word_ratios_Middle <- tidy_books_time %>%
  arrange(desc(tf_idf)) %>%
  filter(sum(n) >= 300) %>%
  ungroup() %>%
  spread(Epoch, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. +1) / sum(. + 1))) %>%
  mutate(logratio = log(Middle / Modern)) %>%
  arrange(desc(logratio))

word_ratios_Middle %>% arrange(abs(logratio))

word_ratios_Middle %>% group_by(logratio < 0 ) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0 )) +
  geom_col(sho.legend = FALSE) +
  coord_flip() +
  ylab("log odds of Middle or Modern English") +
  scale_fill_discrete(name = "", labels = c("Middle", "Modern"))

#Old vs Modern
  
word_ratios_Modern <- tidy_books_time %>%
  arrange(desc(tf_idf)) %>%
  filter(sum(n) >= 300) %>%
  ungroup() %>%
  spread(Epoch, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. +1) / sum(. + 1))) %>%
  mutate(logratio = log(Modern / Old)) %>%
  arrange(desc(logratio))

word_ratios_Modern %>% arrange(abs(logratio))

word_ratios_Modern %>% group_by(logratio < 0 ) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0 )) +
  geom_col(sho.legend = FALSE) +
  coord_flip() +
  ylab("log odds of Modern or Old English") +
  scale_fill_discrete(name = "", labels = c("Modern", "Old"))

#correlation analysis

library(widyr)
library(ggraph)
library(igraph)

tidy_books_time %>%
  group_by(Epoch) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, n, fill = Epoch)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Epoch, scales = "free") +
  ylab("n") +
  coord_flip()

Epoch_cors <- tidy_books_time %>%
  pairwise_cor(Epoch, word, n, sort = TRUE)

Epoch_cors

set.seed(1234)

Epoch_cors %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "pink") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
