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
this_speech[36] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2022_Ramaphosa.txt', nchars = 52972)


sona <- data.frame(filename = filenames, speech = this_speech, stringsAsFactors = FALSE)

# extract year and president for each speech
sona$year <- str_sub(sona$filename, start = 1, end = 4)
sona$pres <- str_remove_all(str_extract(sona$filename, "[dA-Z].*\\."), "\\.")

# clean the sona dataset by adding the date and removing unnecessary text
replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;|\n'

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

# removing deKlerk and Motlanthe from the analysis as they made only 1 speech
tidy_sona  <- tidy_sona  %>%
  filter(!pres %in% c('deKlerk', 'Motlanthe')) # excluding 1 time president

pres_ind <- unique(tidy_sona$pres) # dealing with 4 presidents now

set.seed(123)

########SEPARATE PRESIDENT SENTENCES#########
mandela_sentences <- filter(tidy_sona,pres == pres_ind[1])
mbeki_sentences <- filter(tidy_sona,pres == pres_ind[2])
zuma_sentences <- filter(tidy_sona,pres == pres_ind[3])
ramaphosa_sentences <- filter(tidy_sona,pres == pres_ind[4])


########SPLIT DATA INTO TRAINING AND TESTING #########
splt <- function(name){
  #separate sentences into 67% and 33% for each president
  ind <- sample(1:2, nrow(name), replace= TRUE, prob=c(0.70,0.30))
  
  # Split features
  train <- name[ind==1, ]
  test <- name[ind==2, ]
  return(list(train=train,test=test))
}

#split speeches into 67/33 for each pres
mandela_train <- splt(mandela_sentences)[[1]]
mandela_test <- splt(mandela_sentences)[[2]]

mbeki_train <- splt(mbeki_sentences)[[1]]
mbeki_test <- splt(mbeki_sentences)[[2]]

zuma_train <- splt(zuma_sentences)[[1]]
zuma_test <- splt(zuma_sentences)[[2]]

ramaphosa_train <- splt(ramaphosa_sentences)[[1]]
ramaphosa_test <- splt(ramaphosa_sentences)[[2]]


#joining the speeches together
rand <- sample((dim(ramaphosa_train)[1]+dim(zuma_train)[1]+dim(mbeki_train)[1]+dim(mandela_train)[1]),replace = F)
sona_train <- rbind(mandela_train,mbeki_train,zuma_train,ramaphosa_train)
sona_train_rn <- sona_train[rand,] #randomised train dataset 

rand1 <- sample((dim(ramaphosa_test)[1]+dim(zuma_test)[1]+dim(mbeki_test)[1]+dim(mandela_test)[1]),replace = F)
sona_test <- rbind(mandela_test,mbeki_test,zuma_test,ramaphosa_test)
sona_test_rn <- sona_test[rand1,] #randomised test data set

#separate features into y and x variables
x_train <- sona_train_rn[1]
y_train <- sona_train_rn[2]

x_test <- sona_test_rn[1]
y_test <- sona_test_rn[2]

####CONVERT TARGET INTO CAT ##########

tag.to.num <- function(y){
  y_train_num <- c()
  for (i in 1:nrow(y)){
    if (y[i,]== "Ramaphosa"){
      y_train_num[i]<-0
    }else
      if (y[i,]== "Zuma"){
        y_train_num[i]<-1
      }else
        if (y[i,]== "Mbeki"){
          y_train_num[i]<-2
        }else
          if (y[i,]== "Mandela"){
            y_train_num[i]<-3
          }
  }
  return(y_train_num)
}

#convert the numeric classes to categorical variables
y_train_cat <- to_categorical(tag.to.num(y_train))
y_test_cat <- to_categorical(tag.to.num(y_test))

###### CONVERT SENTENCES INTO SEQUENCE ##### 

sent.to.seq<- function(x,max_features,maxlen){
  # choose max_features most popular words
  tokenizer = text_tokenizer(num_words = max_features)
  fit_text_tokenizer(tokenizer,x$sentences)
  
  #convert text to sequence
  x.seq <- texts_to_sequences(tokenizer,x$sentences)
  
  #check word index
  #tokenizer$index_word[x.seq[[1]]]
  
  # investigate the max length you want your sequence to be so that you pad with zeros 
  #hist(unlist(lapply(x.seq, length)), main = "Sequence length after tokenization")
  
  # padding 
  x.seq <- x.seq %>% pad_sequences(maxlen = maxlen) 
  return(x.seq)
}

max_features <- 1000
maxlen <- 70
x_train.seq <-sent.to.seq(x_train,max_features,maxlen)
x_test.seq <- sent.to.seq(x_test,max_features,maxlen)



####### BUILD THE MODEL ######

model <- keras_model_sequential() %>% 
  layer_embedding(max_features, output_dim = 10, input_length = maxlen) %>%
  layer_dropout(0.2) %>%
  layer_flatten() %>%
  layer_dense(50, activation = "relu") %>%
  layer_dense(4, activation = "softmax")


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(learning_rate = 0.01),
  metrics = c('accuracy'),
)

history <- model %>% 
  fit(x_train.seq, y_train_cat, 
      epochs = 30, batch_size = 5, 
      validation_split = 0.2, shuffle = TRUE
  )

results <- model %>% evaluate(x_test.seq, y_test_cat, batch_size = 64, verbose = 2)


#### CNN ######

model1 <- keras_model_sequential() %>% 
  layer_embedding(max_features, output_dim = 10, input_length = maxlen) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(filters = 64, kernel_size = 8, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(50, activation = "relu") %>%
  layer_dense(4, activation = "softmax")


model1 %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(learning_rate = 0.01),
  metrics = c('accuracy'),
)

history1 <- model1 %>% 
  fit(x_train.seq, y_train_cat, 
      epochs = 30, batch_size = 5, 
      validation_split = 0.2, shuffle = TRUE
  )

results1 <- model1 %>% evaluate(x_test.seq, y_test_cat, batch_size = 64, verbose = 2)

######## USING TRANSFER LEARNING MODELS ######
embedding <- "https://tfhub.dev/google/nnlm-en-dim50/2"
hub_layer <- tfhub::layer_hub(handle = embedding, trainable = TRUE)
hub_layer(x_train[1,])

model2 <- keras_model_sequential() %>%
  hub_layer() %>%
  layer_dense(16, activation = "relu") %>%
  layer_dense(4, activation = "softmax")

history2 <- model2 %>% 
  fit(x_train.seq, y_train_cat, 
      epochs = 30, batch_size = 5, 
      validation_split = 0.2, shuffle = TRUE
  )

results2 <- model2 %>% evaluate(x_test.seq, y_test_cat, batch_size = 64, verbose = 2)

####
