rm(list= ls())

library(stringr)
library(tidyverse)
library(tidytext)
library(textstem)
library(textdata)
library(tm)
library(MASS)
library(topicmodels)
library(keras)
library(tensorflow)
library(readr)
library(lubridate)
library(tfhub)
library(dplyr)
library(rpart)
library(caret)



######### DATA LOADING AND PROCESSING ##########
# read in text data files and organise these into a data frame
filenames <- c('1994_post_elections_Mandela.txt', 
               '1994_pre_elections_deKlerk.txt', 
               '1995_Mandela.txt', 
               '1996_Mandela.txt', 
               '1997_Mandela.txt',
               '1998_Mandela.txt', 
               '1999_post_elections_Mandela.txt', 
               '1999_pre_elections_Mandela.txt', 
               '2000_Mbeki.txt', 
               '2001_Mbeki.txt', 
               '2002_Mbeki.txt', 
               '2003_Mbeki.txt', 
               '2004_post_elections_Mbeki.txt', 
               '2004_pre_elections_Mbeki.txt', 
               '2005_Mbeki.txt', 
               '2006_Mbeki.txt', 
               '2007_Mbeki.txt', 
               '2008_Mbeki.txt', 
               '2009_post_elections_Zuma.txt', 
               '2009_pre_elections_ Motlanthe.txt', 
               '2010_Zuma.txt', 
               '2011_Zuma.txt', 
               '2012_Zuma.txt', 
               '2013_Zuma.txt', 
               '2014_post_elections_Zuma.txt', 
               '2014_pre_elections_Zuma.txt', 
               '2015_Zuma.txt', 
               '2016_Zuma.txt', 
               '2017_Zuma.txt', 
               '2018_Ramaphosa.txt', 
               '2019_post_elections_Ramaphosa.txt', 
               '2019_pre_elections_Ramaphosa.txt', 
               '2020_Ramaphosa.txt', 
               '2021_Ramaphosa.txt', 
               '2022_Ramaphosa.txt', 
               '2023_Ramaphosa.txt')


this_speech <- c()
this_speech[1] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1994_post_elections_Mandela.txt', nchars = 27050)
this_speech[2] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1994_pre_elections_deKlerk.txt', nchars = 12786)
this_speech[3] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1995_Mandela.txt', nchars = 39019)
this_speech[4] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1996_Mandela.txt', nchars = 39524)
this_speech[5] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1997_Mandela.txt', nchars = 37489)
this_speech[6] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1998_Mandela.txt', nchars = 45247)
this_speech[7] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1999_post_elections_Mandela.txt', nchars = 34674)
this_speech[8] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1999_pre_elections_Mandela.txt', nchars = 41225)
this_speech[9] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2000_Mbeki.txt', nchars = 37552)
this_speech[10] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2001_Mbeki.txt', nchars = 41719)
this_speech[11] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2002_Mbeki.txt', nchars = 50544)
this_speech[12] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2003_Mbeki.txt', nchars = 58284)
this_speech[13] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2004_post_elections_Mbeki.txt', nchars = 34590)
this_speech[14] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2004_pre_elections_Mbeki.txt', nchars = 39232)
this_speech[15] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2005_Mbeki.txt', nchars = 54635)
this_speech[16] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2006_Mbeki.txt', nchars = 48643)
this_speech[17] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2007_Mbeki.txt', nchars = 48641)
this_speech[18] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2008_Mbeki.txt', nchars = 44907)
this_speech[19] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2009_post_elections_Zuma.txt', nchars = 31101)
this_speech[20] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2009_pre_elections_Motlanthe.txt', nchars = 47157)
this_speech[21] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2010_Zuma.txt', nchars = 26384)
this_speech[22] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2011_Zuma.txt', nchars = 33281)
this_speech[23] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2012_Zuma.txt', nchars = 33376)
this_speech[24] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2013_Zuma.txt', nchars = 36006)
this_speech[25] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2014_post_elections_Zuma.txt', nchars = 29403)
this_speech[26] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2014_pre_elections_Zuma.txt', nchars = 36233)
this_speech[27] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2015_Zuma.txt', nchars = 32860)
this_speech[28] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2016_Zuma.txt', nchars = 32464)
this_speech[29] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2017_Zuma.txt', nchars = 35981)
this_speech[30] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2018_Ramaphosa.txt', nchars = 33290)
this_speech[31] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2019_post_elections_Ramaphosa.txt', nchars = 42112)
this_speech[32] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2019_pre_elections_Ramaphosa.txt', nchars = 56960)
this_speech[33] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2020_Ramaphosa.txt', nchars = 47910)
this_speech[34] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2021_Ramaphosa.txt', nchars = 43352)
this_speech[35] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2022_Ramaphosa.txt', nchars = 52972)
this_speech[36] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2022_Ramaphosa.txt', nchars = 53933)


sona <- data.frame(filename = filenames, speech = this_speech, stringsAsFactors = FALSE)

# extract year and president for each speech
sona$year <- str_sub(sona$filename, start = 1, end = 4)
sona$pres <- str_remove_all(str_extract(sona$filename, "[dA-Z].*\\."), "\\.")

# clean the sona dataset by adding the date and removing unnecessary text
replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;|\n%[0-9]'

sona <-sona %>%
  mutate(speech = lemmatize_strings(stringr::str_replace_all(speech, replace_reg , ' '))
         ,date = str_sub(speech, start=1, end=30)
         ,date = str_replace_all(date, "February", "02")
         ,date = str_replace_all(date, "June", "06")
         ,date = str_replace_all(date, "Feb", "02")
         ,date = str_replace_all(date, "May", "05")
         ,date = str_replace_all(date, "Jun", "06")
         ,date = str_replace_all(date, "Thursday, ","")
         ,date = str_replace_all(date, ' ', '-')        
         ,date = str_replace_all(date, "[A-z]",'')
         ,date = str_replace_all(date, '-----', '')
         ,date = str_replace_all(date, '----', '')
         ,date = str_replace_all(date, '---', '')
         ,date = str_replace_all(date, '--', '')
  )

# Convert to tibble
sona <- as.tibble(sona)

#tokenize into sentences 
tidy_sona <- sona %>% 
  mutate(speech = str_replace_all(speech, replace_reg, '')) %>% # remove links etx
  unnest_tokens(sentences, speech, token = 'sentences') %>%   # tokenize
  dplyr::select(sentences, pres, date, filename,year)

#tokenize into words 
tidy_sona.w <- sona %>% 
  mutate(speech = str_replace_all(speech, replace_reg, '')) %>% # remove links etx
  unnest_tokens(word, speech, token = 'regex') %>%   # tokenize
  filter(!word %in% stop_words$word, str_detect(word, '[A-Za-z]')) %>% #remove stop words
  dplyr::select(word, pres, date, filename,year)

# removing deKlerk and Motlanthe from the analysis as they made only 1 speech
tidy_sona  <- tidy_sona  %>%
  filter(!pres %in% c('deKlerk', 'Motlanthe')) # excluding 1 time president
tidy_sona.w  <- tidy_sona.w  %>%
  filter(!pres %in% c('deKlerk', 'Motlanthe')) # excluding 1 time president

pres_ind <- unique(tidy_sona$pres) # dealing with 4 presidents now

set.seed(123)
###### NN and CNN #########

########SPLIT DATA INTO TRAINING AND TESTING #########

#splitting sentences into testing and training  
sent_train<- tidy_sona %>%
  group_by("pres") %>%
  slice_sample(prop=0.7)%>%
  ungroup()%>%
  dplyr::select(pres,date,sentences)

sent_test <- tidy_sona %>%
  anti_join(sent_train)%>%
  dplyr::select(pres,date,sentences)

#separate features into y and x variables
x_train <- sent_train[3]
y_train <- sent_train[1]

x_test <- sent_test[3]
y_test <- sent_test[1]

####CONVERT TARGET INTO CAT ##########

# Create the mapping
mapping <- c("Ramaphosa" = 0, "Mandela" = 1, "Zuma" = 2, "Mbeki" = 3)

# Apply the mapping to the pres column and convert to categorical. 
y_train_cat <- y_train %>%
  mutate(pres = mapping[pres])%>%
  to_categorical()

y_test_cat <- y_test %>%
  mutate(pres = mapping[pres])%>%
  to_categorical()


###### CONVERT SENTENCES INTO SEQUENCE ##### 

sent.to.seq<- function(x,max_features,maxlen){
  # choose max_features most popular words
  tokenizer = text_tokenizer(num_words = max_features)
  fit_text_tokenizer(tokenizer,x$sentences)
  
  #convert text to sequence
  x.seq <- texts_to_sequences(tokenizer,x$sentences)

  # padding 
  x.seq <- x.seq %>% pad_sequences(maxlen = maxlen) 
  return(x.seq)
}

max_features <- 1500
maxlen <- 70
x_train.seq <-sent.to.seq(x_train,max_features,maxlen)
x_test.seq <- sent.to.seq(x_test,max_features,maxlen)



####### BUILD THE MODEL ######
set.seed(123)
nn_model <- keras_model_sequential() %>% 
  layer_embedding(max_features, output_dim = 10, input_length = maxlen) %>%
  layer_dropout(0.2) %>%
  layer_flatten() %>%
  layer_dense(100, activation = "relu") %>%
  layer_dense(4, activation = "softmax")


nn_model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(learning_rate = 0.01),
  metrics = c('accuracy'),
)

#impliment early stopping 
early_stopping <- callback_early_stopping(monitor = "val_loss", patience = 10)

nn_history <- nn_model %>% 
  fit(x_train.seq, y_train_cat, 
      epochs = 30, batch_size = 5, 
      validation_split = 0.2, shuffle = TRUE,
      callbacks = list(early_stopping)
  )

nn_results <- nn_model %>% evaluate(x_test.seq, y_test_cat, batch_size =5, verbose = 2)


#### CNN ######
set.seed(123)
cnn_model <- keras_model_sequential() %>% 
  layer_embedding(max_features, output_dim = 10, input_length = maxlen) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(filters = 64, kernel_size = 8, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(50, activation = "relu") %>%
  layer_dense(4, activation = "softmax")


cnn_model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(learning_rate = 0.01),
  metrics = c('accuracy'),
)

cnn_history <- cnn_model %>% 
  fit(x_train.seq, y_train_cat, 
      epochs = 30, batch_size = 5, 
      validation_split = 0.2, shuffle = TRUE,
      callbacks = list(early_stopping)
  )

cnn_results <- cnn_model %>% evaluate(x_test.seq, y_test_cat, batch_size = 64, verbose = 2)


#### Bag of words per speech ######

# select the top 500 words 
word_bag <- tidy_sona.w %>%
  group_by(word) %>%
  count() %>%
  ungroup() %>%
  top_n(500, wt = n)
 

sona.w_tdf <- tidy_sona.w %>%
  inner_join(word_bag) %>%
  group_by(pres,date,word) %>%
  count() %>%  
  group_by(pres) %>%
  mutate(total = sum(n)) %>%
  ungroup()
# show words in a wide format
bag_of_words <- sona.w_tdf %>% 
  dplyr::select(pres,date,word,n) %>% 
  pivot_wider(names_from = word, values_from = n, values_fill = 0) 
  
set.seed(321)
word_train <- bag_of_words %>% 
  group_by(pres) %>% 
  slice_sample(prop = 0.7) %>% 
  ungroup() 

word_test<- bag_of_words %>% 
  anti_join(training) 

x_train.w<-word_train%>%
  dplyr::select(-c(date))

y_train.w<-word_train%>%
  dplyr::select(pres)

x_test.w<-word_test%>%
  dplyr::select(-c(date,pres))

y_test.w <-word_test%>%
  dplyr::select(pres)

fit <- rpart(pres ~ .,x_train.w, method = 'class')
# options(repr.plot.width = 12, repr.plot.height = 10) # set plot size in the notebook
plot(fit, main = 'Full Classification Tree')
text(fit, use.n = TRUE, all = TRUE, cex=.8)

fittedtrain <- predict(fit, type = 'class')
predtrain <- table(x_train.w$pres, fittedtrain)
predtrain

round(sum(diag(predtrain))/sum(predtrain), 3) # training accuracy


fit.test<-predict(fit,newdata =x_test.w,type="class" )
predtrain.t <- table(y_test.w$pres, fit.test)
predtrain.t

round(sum(diag(predtrain.t))/sum(predtrain.t), 3) # training accuracy


#### Bag of words per sentence ######
#tokenize sentences in words
tidy_sona.sw <- tidy_sona %>% 
  mutate (sent_id = row_number())%>% #add sentence id for grouping 
  unnest_tokens(word, sentences, token = 'regex') %>%   # tokenize
  filter(!word %in% stop_words$word, str_detect(word, '[A-Za-z]')) %>% #remove stop words
  dplyr::select(pres,sent_id,word)


# select the top 500 words 
word_bag <- tidy_sona.sw %>%
  group_by(word) %>%
  count() %>%
  ungroup() %>%
  top_n(9000, wt = n)


sona.w_tdf <- tidy_sona.sw %>%
  inner_join(word_bag) %>%
  group_by(pres,sent_id,word) %>%
  count() %>%  
  group_by(pres) %>%
  mutate(total = sum(n)) %>%
  ungroup()
# show words in a wide format
bag_of_words <- sona.w_tdf %>% 
  dplyr::select(pres,sent_id,word,n) %>% 
  pivot_wider(names_from = word, values_from = n, values_fill = 0) 

set.seed(321)
word_train <- bag_of_words %>% 
  group_by(pres) %>% 
  slice_sample(prop = 0.7) %>% 
  ungroup() 

word_test<- bag_of_words %>% 
  anti_join(word_train) 

x_train.w<-word_train%>%
  dplyr::select(-c(sent_id))

y_train.w<-word_train%>%
  dplyr::select(pres)

x_test.w<-word_test%>%
  dplyr::select(-c(sent_id,pres))

y_test.w <-word_test%>%
  dplyr::select(pres)

fit <- rpart(pres ~ .,x_train.w, method = 'class')
# options(repr.plot.width = 12, repr.plot.height = 10) # set plot size in the notebook
plot(fit, main = 'Full Classification Tree')
text(fit, use.n = TRUE, all = TRUE, cex=.8)

fittedtrain <- predict(fit, type = 'class')
predtrain <- table(x_train.w$pres, fittedtrain)
predtrain

round(sum(diag(predtrain))/sum(predtrain), 3) # training accuracy


fit.test<-predict(fit,newdata =x_test.w,type="class" )
predtrain.t <- table(y_test.w$pres, fit.test)
predtrain.t

round(sum(diag(predtrain.t))/sum(predtrain.t), 3) # training accuracy
####### 

ndocs <- 36

idf <-  sona.w_tdf%>% 
  group_by(word) %>% 
  summarize(docs_with_word = n()) %>% 
  ungroup() %>%
  mutate(idf = log(ndocs / docs_with_word)) %>% arrange(desc(idf))

sona_tdf.w <- sona.w_tdf %>% 
  left_join(idf, by = 'word') %>% 
  mutate(tf = n/total, tf_idf = tf * idf)


tfidf <- sona_tdf.w %>% 
  dplyr::select(pres,word,tf_idf) %>%  # note the change, using tf-idf
  pivot_wider(names_from = word, values_from = tf_idf) %>%  
  left_join(tidy_sona %>% dplyr::select(pres,date))
########
tfidf <- sona.w_tdf %>%
  bind_tf_idf(word,pres,n)

tfidf <- sona_tdf.w %>% 
  dplyr::select(pres,word,tf_idf) %>%  # note the change, using tf-idf
  pivot_wider(names_from = word, values_from = tf_idf) %>%  
  left_join(tidy_sona %>% dplyr::select(pres,date))

training_tidf <- tfidf %>% 
  group_by(pres) %>% 
  slice_sample(prop = 0.7) %>% 
  ungroup() 

test_tidf <- tfidf %>% 
  anti_join(training_tidf, by = 'pres') 
########

fit2<- rpart(pres ~ sentences,sona_train_rn)

fittedtrain2 <- predict(fit2, type = 'class')
predtrain <- table(x_training$pres, fittedtrain)
predtrain

round(sum(diag(predtrain))/sum(predtrain), 3) # training accuracy


fit.test<-predict(fit,newdata =x_testing,type="class" )
predtrain.t <- table(x_testing$pres, fit.test)
predtrain.t

round(sum(diag(predtrain.t))/sum(predtrain.t), 3) # training accuracy
