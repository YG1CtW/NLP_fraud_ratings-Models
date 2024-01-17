data <- read.csv('/Users/yilingao/Desktop/APAN5205/review.csv')
library(dplyr); library(magrittr); library(stringr)
head(data)

# Omit NA values
data <- na.omit(data)

# Reassign data's id
data$ID <- seq.int(nrow(data))

# Check scores of seat_comfort, cabin_service, food_bev, 
# entertainment, ground_service, value_for_money's association between recommend
# This will help to determine what coefficient to use for each variable when computing the overall weighted score
data$rec_num <- ifelse(data$recommended == "yes", 1, 0)
data$cabin_service <- as.numeric(data$cabin_service)
data$food_bev <- as.numeric(data$food_bev)
data$entertainment <- as.numeric(data$entertainment)
data$ground_service <- as.numeric(data$ground_service)
data$value_for_money <- as.numeric(data$value_for_money)
correlation <- cor(data[, c("seat_comfort", "cabin_service", "food_bev", "entertainment", "ground_service", "value_for_money", "rec_num")])
correlation <- correlation["rec_num", ]

# Calculate weighted correlation of each and generate one representative score of all rating variable
sum<- sum(c(0.7272627, 0.7446106, 0.7376767, 0.6518008, 0.7512807, 0.8384546))
weighted_coefficient <- correlation/sum

# Compute representative score suing weighted correlation 
score <- c(data$cabin_service*weighted_coefficient[1] + data$seat_comfort*weighted_coefficient[2] + data$food_bev*weighted_coefficient[3]
            + data$entertainment*weighted_coefficient[4] + data$ground_service*weighted_coefficient[5] + data$value_for_money*weighted_coefficient[6])
# Select columns relevant with text mining
data <- data %>%
          mutate(score = score) %>%
          select(ID, customer_review, score) %>%
          rename(review = customer_review)


#Review Data Overview
# 1.summary of sentences
summary(str_count(string = data$review,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"))

# 2.summary of words
summary(str_count(string = data$review,pattern = '\\S+'))

# 3.longest review
longest_review_index = which.max(str_count(string = data$review,pattern = '\\S+'))
data$review[longest_review_index]

# most common words(25)
library(tidytext); library(magrittr)
data%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

# most common words after removing stoop words
data%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

# most common words after removing stoop words frequency diagram
library(ggplot2)
data%>%
  unnest_tokens(input = review, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()

# examine word counts
data %>%
  select(ID,review)%>%
  group_by(ID)%>%
  unnest_tokens(output = word,input=review)%>%
  ungroup()%>%
  group_by(ID)%>%
  summarize(count = n()) %>%
  ggplot(aes(x=count))+geom_histogram(bins = 40)+xlab('Number of Words')

# Use Bing to examine words' attributes
data %>%
  select(ID,review)%>%
  group_by(ID)%>%
  unnest_tokens(output=word,input=review)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

# Compute Average Rating Score which is 2.99
data_mean <- mean(data$score)
data_mean
 # There are 20% more of positive words compared to negative words using Bing method
 # Mean Score: 2.99
 # Conclusion: Ratings may be not reliable

# Examine positive and negative proportion of each review along with the score
proportion <- data %>%
  select(ID,review,score)%>%
  group_by(ID,score)%>%
  unnest_tokens(output=word,input=review)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(score,sentiment)%>%
  summarize(n = n())%>% 
  mutate(proportion = n/sum(n))

# Check what ID might be given fake ratings
# If a review has scored over 3, but more negative porportions, it will be marked as 'fake' for further investigation
num_rows <- nrow(proportion)
proportion <- proportion %>%
  ungroup() %>%
  mutate(object = rep(1:ceiling(num_rows/2), each = 2, length.out = num_rows))

# Row 162 missing due to no positive words
row_162 <- data.frame(
  score = 1.542471,
  sentiment = "positive",
  n = 0,
  proportion = 0,
  object = 81
)

# Modify dataframe so all reviews has both negative and positive sentiment properly
before_162 <- proportion[1:161, ]
after_162 <- proportion[162:nrow(proportion), ]
proportion <- rbind(before_162, row_162, after_162)
proportion[16, 'object'] <- 82
before_163 <- proportion[1:162, ]
after_163 <- proportion[163:nrow(proportion), ]
sequence <- rep(82:4733, each = 2)
new_dataframe <- data.frame(object = sequence)
after_163$object <- new_dataframe$object
proportion <- rbind(before_163, after_163)
tail(proportion)

# Mark potential fake reviews
fake_reviews <- proportion %>%
  filter(row_number() %% 2 == 1 &
           n > lead(n) &
         score > 3)
# Display fake reviews and total number
fake_reviews$object
count(fake_reviews)

# Exaime all review's affin sentiment, median is 0.429, and mean is 0.491
afinn = get_sentiments('afinn')
data  %>%
  select(ID,review)%>%
  group_by(ID)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),
            max=max(reviewSentiment),
            median=median(reviewSentiment),
            mean=mean(reviewSentiment))
#plot 
library(ggplot2)
data %>%
  select(ID,review)%>%
  group_by(ID)%>%
  unnest_tokens(output=word,input=review)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+
  scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_minimal()

#wordcloud
library(wordcloud)
wordcloudData = 
data%>%
  group_by(ID)%>%
  unnest_tokens(output=word,input=review)%>%
  ungroup()%>%
  select(ID,word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()


library(wordcloud)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

# Make a predictive model based on the reviews
library(tm)
corpus = Corpus(VectorSource(data$review))

#Clean text
 #remove uppercase
corpus = tm_map(corpus,FUN = content_transformer(tolower))
 #remove urls
corpus = tm_map(corpus,
                FUN = content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*',
                                                                replacement = ' ',x = x)))
 #remove punctuation
corpus = tm_map(corpus,FUN = removePunctuation)
 #remove stopwords
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
 #strip white space
corpus = tm_map(corpus,FUN = stripWhitespace)

#Create dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(data$review))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

#Stem document
corpus = tm_map(corpus,FUN = stemDocument)

#Create matrix(tokenize)
dtm = DocumentTermMatrix(corpus)
dtm

#Remove infrequent items and complete stems
xdtm = removeSparseTerms(dtm,sparse = 0.8)
xdtm

xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))
sort(colSums(xdtm),decreasing = T)

#Using tfidf
dtm_tfidf = DocumentTermMatrix(x=corpus,
                               control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.8)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),
                                      dictionary = dict_corpus,
                                      type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)

# Building predictive models
data_xdtm = cbind(review_rating = data$score,xdtm)
data_tfidf = cbind(review_rating = data$score,xdtm_tfidf)

split = sample(1:nrow(data_xdtm),size = 0.7*nrow(data_xdtm))
train = data_xdtm[split,]
test = data_xdtm[-split,]

#Cart model:1.156
library(rpart); library(rpart.plot)
tree = rpart(review_rating~.,train)
rpart.plot(tree)
pred_tree = predict(tree,newdata=test)
rmse_tree = sqrt(mean((pred_tree - test$review_rating)^2)); rmse_tree

#Linear Regression Model:1.10
reg = lm(review_rating~.,train)
summary(reg)
pred_reg = predict(reg, newdata=test)
rmse_reg = sqrt(mean((pred_reg-test$review_rating)^2)); rmse_reg

#Using TFIDF
split = sample(1:nrow(data_tfidf),size = 0.7*nrow(data_tfidf))
train_tfidf = data_tfidf[split,]
test_tfidf = data_tfidf[-split,]

#CART TIDF:1.398
tree_tfidf = rpart(review_rating~.,train_tfidf)
rpart.plot(tree_tfidf)
pred_tree_tfidf = predict(tree_tfidf,newdata=test)
rmse_tree_tfidf = sqrt(mean((pred_tree_tfidf - test_tfidf$review_rating)^2)); rmse_tree_tfidf

#Linear:1.098
reg_tfidf = lm(review_rating~.,train_tfidf)
summary(reg_tfidf)
pred_reg_tfidf = predict(reg_tfidf, newdata=test_tfidf)
rmse_reg_tfidf = sqrt(mean((pred_reg_tfidf-test_tfidf$review_rating)^2)); rmse_reg_tfidf

