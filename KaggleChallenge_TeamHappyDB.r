
library(wordcloud)
library(plyr)
library(cluster)
library(RColorBrewer)
library(tm)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ca)
library(fpc)
library(party)

#data_HM <- read.csv("C:/Users/Antoine Lain/Desktop/cleaned_hm.csv")
data_HM <- read.csv("/Users/jacquelineneef/Desktop/Kaggle/data/cleaned_hm.csv")

head(data_HM)

summary(data_HM[,c(3,6,7,9)])

rapply(data_HM,function(x)length(unique(x)))

sapply(data_HM, class)

opt <- options("scipen" = 20)
op <- par(mar = c(5,7,4,2) + 0.1) #extra margin to accommodate tick labs
par(mfrow = c(2,1))

plot(data_HM$reflection_period, main="Happy Moments by Reflection Period", col =c('red', 'blue'), las = 1, ylab = "")
title(ylab = "count", line = 4)

plot(data_HM$predicted_category, main="Predicted Categories", col = rainbow(7), las = 1, ylab = "")
title(ylab = "count", line = 4)

par(op)
options(opt)

length(data_HM$cleaned_hm[])
wordcountcleaned = gsub(' {2,}',' ',data_HM$cleaned_hm)
summary(sapply(strsplit(wordcountcleaned[],' '), length))

for(i in 1:length(data_HM$cleaned_hm[])){
if(sapply(strsplit(wordcountcleaned[i],' '), length)==2){print(toString(data_HM$cleaned_hm[i]))}}

word_0_4= c()
word_05_09= c()
word_10_14= c()
word_15_19= c()
word_20_24= c()
word_25_29= c()
word_30_34= c()
word_35_39= c()
word_40_44= c()
word_45_49= c()
word_50= c()
for(i in 1:length(data_HM$cleaned_hm[])){
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 5){word_0_4 <<- c(word_0_4,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 10 && sapply(strsplit(wordcountcleaned[i],' '), length) > 4){word_05_09 <<- c(word_05_09,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 15 && sapply(strsplit(wordcountcleaned[i],' '), length) > 9){word_10_14 <<- c(word_10_14,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 20 && sapply(strsplit(wordcountcleaned[i],' '), length) > 14){word_15_19 <<- c(word_15_19,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 25 && sapply(strsplit(wordcountcleaned[i],' '), length) > 19){word_20_24 <<- c(word_20_24,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 30 && sapply(strsplit(wordcountcleaned[i],' '), length) > 24){word_25_29 <<- c(word_25_29,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 35 && sapply(strsplit(wordcountcleaned[i],' '), length) > 29){word_30_34 <<- c(word_30_34,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 40 && sapply(strsplit(wordcountcleaned[i],' '), length) > 34){word_35_39 <<- c(word_35_39,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 45 && sapply(strsplit(wordcountcleaned[i],' '), length) > 39){word_40_44 <<- c(word_40_44,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) < 50 && sapply(strsplit(wordcountcleaned[i],' '), length) > 44){word_45_49 <<- c(word_45_49,i)}
  if(sapply(strsplit(wordcountcleaned[i],' '), length) > 49){word_50 <<- c(word_50,i)}
}
distribution <- c(rep("0-4", length(word_0_4)),
                  rep("05-09", length(word_05_09)),
                  rep("10-14", length(word_10_14)),
                  rep("15-19", length(word_15_19)),
                  rep("20-24", length(word_20_24)),
                  rep("25-29", length(word_25_29)),
                  rep("30-34", length(word_30_34)),
                  rep("35-39", length(word_35_39)),
                  rep("40-44", length(word_40_44)),
                  rep("45-49", length(word_45_49)),
                  rep(">50", length(word_50)))
#plot the results
opt <- options("scipen" = 20)
op <- par(mar = c(5,7,4,2) + 0.1) #extra margin to accommodate tick labs
barplot(table(distribution), main="Distribution of Word Counts within the Happy Moments",
        xlab="Word Count Range",las = 1, ylab="", col = rainbow(11))
title(ylab = "Word Count", line = 4)
par(op)
options(opt)

First_range <- c(word_0_4, word_05_09)
Second_range <- c(word_10_14)
Third_range <- c(word_15_19, word_20_24, word_25_29, word_30_34, word_35_39, word_40_44, word_45_49, word_50)

data_occurency <- c()
for(i in 1:length(First_range)){data_occurency <- c(toString(data_occurency), toString(data_HM$cleaned_hm[i]))}
myCorpus <- Corpus(VectorSource(data_occurency))
tdm <- TermDocumentMatrix( myCorpus,
                          control = list(removePunctuation = TRUE ,
                          stopwords = c(stopwords("english")),
                          removeNumbers = TRUE ,
                          tolower = TRUE )
                                           )
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing =TRUE )
dm_First_range <- data.frame(word= names( word_freqs), freq = word_freqs)

data_occurency <- c()
for(i in 1:length(Second_range)){data_occurency <- c(toString(data_occurency), toString(data_HM$cleaned_hm[i]))}
myCorpus <- Corpus(VectorSource(data_occurency))
tdm <- TermDocumentMatrix( myCorpus,
                            control = list(removePunctuation = TRUE ,
                            stopwords = c(stopwords("english")),
                            removeNumbers = TRUE ,
                            tolower = TRUE )
 )
 m <- as.matrix(tdm)
 word_freqs <- sort(rowSums(m), decreasing =TRUE )
 dm_Second_range <- data.frame(word= names( word_freqs), freq = word_freqs)

data_occurency <- c()
for(i in 1:length(Third_range)){data_occurency <- c(toString(data_occurency), toString(data_HM$cleaned_hm[i]))}
myCorpus <- Corpus(VectorSource(data_occurency))
tdm <- TermDocumentMatrix( myCorpus,
                            control = list(removePunctuation = TRUE ,
                            stopwords = c(stopwords("english")),
                            removeNumbers = TRUE ,
                            tolower = TRUE )
 )
 m <- as.matrix(tdm)
 word_freqs <- sort(rowSums(m), decreasing =TRUE )
 dm_Third_range <- data.frame(word= names( word_freqs), freq = word_freqs)

Top_word <- merge(dm_First_range, dm_Second_range, by="word", all = TRUE)
Top_word[is.na(Top_word)] <- 0
Top_word <- transform(Top_word, summation.col = freq.x + freq.y)
Top_word$freq.x <- NULL
Top_word$freq.y <- NULL
Top_word <- merge(Top_word, dm_Third_range, by="word", all = TRUE)
Top_word[is.na(Top_word)] <- 0
Top_word <- transform(Top_word, summation = summation.col + freq)
Top_word <- arrange(Top_word,desc(summation))
head(Top_word)

wordcloud(words = Top_word$word, freq = Top_word$summation, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) 

occurence <- c()
word_to_remove = c('happy', 'got', 'went', 'today', 'made', 'one', 'two', 'time', 'last', 'first', 'going', 'getting', 'took', 'found', 'lot', 'really', 'saw', 'see', 'month', 'week', 'day', 'yesterday', 'year', 'ago', 'now', 'still', 'since', 'something', 'great', 'good', 'long', 'thing','without')
for(i in 1:nrow(Top_word)){
  for(j in 1:length(word_to_remove)){
    if(word_to_remove[j] == Top_word[i,1]){
      occurence <- c(occurence, i)
    }
  }
}
Top_cleanword <- Top_word[-c(occurence), ]
wordcloud(words = Top_cleanword$word, freq = Top_cleanword$summation, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) 

seasons <- c('spring', 'summer', 'fall', 'winter')
occurence <- c()
for(i in 1:nrow(Top_cleanword)){
  for(j in 1:length(seasons)){
    if(seasons[j] == Top_cleanword[i,1]){
      occurence <- c(occurence, i)
    }
  }
}

for(i in occurence){
  print(Top_cleanword[i,c(1,4)])
}


brand <- c('facebook', 'amazon', 'apple', 'google', 'samsung')
occurence <- c()
for(i in 1:nrow(Top_cleanword)){
  for(j in 1:length(brand)){
    if(brand[j] == Top_cleanword[i,1]){
      occurence <- c(occurence, i)
    }
  }
}

for(i in occurence){
  print(Top_cleanword[i,c(1,4)])
}

sentences <- strsplit(wordcountcleaned,' ')
get <- c('purchase', 'buy', 'purchasing', 'buying', 'purchases', 'purchased', 'sell', 'discount', 'discounted', 'order')
occurence <- c()
for(i in 1:length(sentences)){
  for (v in 1:length(sentences[[i]])) {
    for(j in 1:length(get)){
      if(get[j] == sentences[[i]][v]){
        occurence <- c(occurence, i)
      }
      
    }
  }
}

get_sentences <- c()
for(i in occurence){
  get_sentences <- c(get_sentences, toString(data_HM$cleaned_hm[[i]]))
}

myCorpus <- Corpus(VectorSource(get_sentences))
tdm <- TermDocumentMatrix( myCorpus,
                           control = list(removePunctuation = TRUE ,
                                          stopwords = c(stopwords("english")),
                                          removeNumbers = TRUE ,
                                          tolower = TRUE )
)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing =TRUE )
dm_get <- data.frame(word= names( word_freqs), freq = word_freqs)
head(dm_get)

wordcloud(words = dm_get$word, freq = dm_get$freq, min.freq = 1,
          max.words=30, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) 

occurence <- c()
word_to_remove = c('purchase', 'buy', 'purchasing', 'buying', 'purchases', 'purchased', 'sell', 'discount', 'discounted', 'order', 'happy', 'got', 'went', 'today', 'made', 'one', 'two', 'time', 'last', 'first', 'going', 'getting', 'took', 'found', 'lot', 'really', 'saw', 'see', 'month', 'week', 'day', 'yesterday', 'year', 'ago', 'now', 'still', 'since', 'something', 'great', 'good', 'long', 'thing','without')
for(i in 1:nrow(dm_get)){
  for(j in 1:length(word_to_remove)){
    if(word_to_remove[j] == dm_get[i,1]){
      occurence <- c(occurence, i)
    }
  }
}
dm_cleanget <- dm_get[-c(occurence), ]
wordcloud(words = dm_cleanget$word, freq = dm_cleanget$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) 

category <- table(data_HM$predicted_category)
category_df<-as.data.frame(category)
colnames(category_df) <- c("category", "count")
category_df<-category_df %>% ungroup() %>% mutate(percent = count/sum(count) * 100) %>% arrange(desc(count))
colnames(category_df) <- c("category", "count", "percent")

bar <- ggplot(data = category_df) +
    geom_bar(mapping = aes(x = reorder(category_df$category, -count), y=category_df$count, fill = category_df$category), stat="identity", width = 1) +
    theme(aspect.ratio = 1) +
    labs(x = "count", y = NULL, fill = "Predicted Category") +
    ggtitle("The Predicted Categories of the Happy Moments ")

bar + coord_flip()
bar + coord_polar()


barplot_predicted <- ggplot(category_df, aes(x = "", y = percent, fill = category))+
                        geom_bar(stat = "identity")+labs( x = "", y = "Percentage", fill = "Category")+
                        geom_text(aes(label = paste0(round(category_df$percent),"%")),
                                  color = "white", family = "Georgia", position = position_stack(vjust = 0.5))+
                    ggtitle("The Predicted Categories of the Happy Moments ")
barplot_predicted

wordcount <- sapply(strsplit(wordcountcleaned[],' '), length)
data_HM <- data.frame(data_HM, wordcount)
summary(data_HM$wordcount)

data_HM$wordcount <- cut(data_HM$wordcount, breaks=c(quantile(data_HM$wordcount, probs = seq(0, 1, by = 0.25))), 
      labels=c("0-9","10-14","15-21","22-1155"), include.lowest=TRUE)

summary(data_HM$wordcount)

qplot(data_HM$reflection_period, data_HM$predicted_category, data=data_HM,
      color=data_HM$wordcount)+ geom_jitter(width = 0.2, height = 0.2)+
    labs(x = "Reflection Period", y = "Predicted Category", colour = "Word Count Range")+
ggtitle("The Predicted Categories of the Happy Moments")

ggplot( data = data_HM ) + 
    geom_bar(mapping = aes( x = data_HM$predicted_category, fill = data_HM$wordcount),position = "fill")+ 
    labs(x = "Predicted Category",y = "Percentage",fill = "Word Count Range") + 
    theme(axis.text.x=element_text(size=7))+
    ggtitle("The Predicted Categories of the Happy Moments ")

#data_Demographic <- read.csv("C:/Users/Antoine Lain/Desktop/demographic.csv")
data_demographic <- read.csv("/Users/jacquelineneef/Desktop/Kaggle/demographic.csv")

head(data_demographic)

summary(data_demographic)

rapply(data_demographic,function(x)length(unique(x)))

sapply(data_demographic, class)

'%!in%' <- function(x,y)!('%in%'(x,y))
# data prep countries
data_demographic <- data_demographic[data_demographic$country %!in% c(""), ]
data_demographic$country <- droplevels.data.frame(data_demographic)$country
# data prep gender
data_demographic <- data_demographic[data_demographic$gender %!in% c("", "o"), ]
data_demographic$gender <- droplevels.data.frame(data_demographic)$gender
#data prep martial status
data_demographic <- data_demographic[data_demographic$marital %!in% c(""), ]
data_demographic$marital <- droplevels.data.frame(data_demographic)$marital
#data prep parenthood
data_demographic <- data_demographic[data_demographic$parenthood %!in% c(""), ]
data_demographic$parenthood <- droplevels.data.frame(data_demographic)$parenthood

#get the percentages of the countries
counts_country <- table(data_demographic$country)
country_df<-as.data.frame(counts_country)
colnames(country_df) <- c("country", "count")
country_df<-country_df %>% ungroup() %>% mutate(percent = count/sum(count) * 100) %>% arrange(desc(count))
colnames(country_df) <- c("country", "count", "percent")

#get the percentages of the gender
gender <- table(data_demographic$gender)
gender_df<-as.data.frame(gender)
colnames(gender_df) <- c("gender", "count")
gender_df<-gender_df %>% ungroup() %>% mutate(percent = count/sum(count) * 100) %>% arrange(desc(count))
colnames(gender_df) <- c("gender", "count", "percent")

#get the percentages of marital status
marital <- table(data_demographic$marital)
marital_df<-as.data.frame(marital)
colnames(marital_df) <- c("marital", "count")
marital_df<-marital_df %>% ungroup() %>% mutate(percent = count/sum(count) * 100) %>% arrange(desc(count))
colnames(marital_df) <- c("marital", "count", "percent")

#get the percentages of parenthood
parenthood <- table(data_demographic$parenthood)
parenthood_df<-as.data.frame(parenthood)
colnames(parenthood_df) <- c("parenthood", "count")
parenthood_df<-parenthood_df %>% ungroup() %>% mutate(percent = count/sum(count) * 100) %>% arrange(desc(count))
colnames(parenthood_df) <- c("parenthood", "count", "percent")


# Barplots
barplot_gender <- ggplot(gender_df, aes(x = "", y = percent, fill = gender))+
                        geom_bar(stat = "identity")+labs( x = "", y = "Percentage", fill = "Gender")+
                        geom_text(aes(label = paste0(round(gender_df$percent),"%")), color = "white", position = position_stack(vjust = 0.5))+
                        coord_fixed(0.15)
barplot_marital <- ggplot(marital_df, aes(x = "", y = percent, fill = marital))+
                        geom_bar(stat = "identity")+labs( x = "", y="", fill = "Marital Status")+
                        geom_text(aes(label = paste0(round(marital_df$percent),"%")), color = "white", position = position_stack(vjust = 0.5))+
                        coord_fixed(0.15)
barplot_parenthood <- ggplot(parenthood_df, aes(x = "", y = percent, fill = parenthood))+
                        geom_bar(stat = "identity")+labs( x = "", y = "", fill = "Parenthood")+
                        geom_text(aes(label = paste0(round(parenthood_df$percent),"%")), color = "white", position = position_stack(vjust = 0.5))+
                        coord_fixed(0.15)

grid.arrange(barplot_gender, barplot_marital,barplot_parenthood, ncol=3, nrow = 1)

barplot_country <- ggplot(country_df, aes(x = "", y = percent, fill = country))+
                    geom_bar(stat = "identity")+
                    geom_text(aes(label =ifelse(country_df$percent >= 0.9, paste0(country_df$country),"")), color = "white", position = position_stack(vjust = 0.5))+
                    labs(title = "Workers by Country",
                         x = "", y = "Percentage", fill = "Country Code")
barplot_country

#label all countries that are not US and India as "Other"
levels(data_demographic$country)[levels(data_demographic$country) %!in% c("USA", "IND")] <- "Other"
data_demographic$country <- droplevels.data.frame(data_demographic)$country

summary(data_demographic)

sapply(data_demographic, class)

summary(data_demographic$age)

data_demographic$age <- as.numeric(as.character(data_demographic$age))
data_demographic$age[data_demographic$age < 18 | data_demographic$age > 65 ] <- NA #only condsider ages from 18-65
data_demographic$age[is.na(data_demographic$age)] <- mean(data_demographic$age, na.rm = TRUE) #replace NA values by the mean
data_demographic$age <- round(data_demographic$age,digits=0)#round all values to integers
summary(data_demographic$age)

sapply(data_demographic, class)

h <- hist(data_demographic$age, breaks = 10, density = 10,
          col = "red", xlab = "Age", main = "Distibution of the Age")

theoretical <- data.frame(age = runif(10000, min=18, max=65))
emprical <- data.frame(age = data_demographic$age)
theoretical$values <- 'theoretical'
emprical$values <- 'emprical'
comparison <- rbind(emprical, theoretical)

ggplot(comparison, aes(age, fill = values)) +
    geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=5, position = 'identity')+
    labs(title = "Emprical vs. Theoretical Distribution of the Age",x = "Age", y = "Density", fill = "")

age <- c()
a <- "[18.00,25.00]"
b <- "[26.00,30.00]"
c <- "[31.00,37.00]"
d <- "[38.00,65.00]"
for(i in 1:length(data_demographic$age[])){
  if(data_demographic$age[i] < 26 ){ age <- c(age, as.character(a))}
  if(data_demographic$age[i] > 25 && data_demographic$age[i] < 31){age <- c(age, as.character(b))}
  if(data_demographic$age[i] > 30 && data_demographic$age[i] < 38){age <- c(age, as.character(c))}
  if(data_demographic$age[i] > 37 ){age <- c(age, as.character(d))}
}
data_demographic <- data_demographic[,-2]
data_demographic <- cbind(data_demographic, age)
summary(data_demographic)

head(data_demographic)

#we compute a dissimilarity matrix using the Gower distance on the data_demographic dataframe without the wid
dmatrix <- daisy(data_demographic[2:6], metric = "gower")

# Compute and plot wss for k = 2 to k = 15.
wss <- sapply(1:15, 
              function(k){kmeans(dmatrix, k)$tot.withinss})
wss
plot(1:15, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

pamk.best <- pamk(dmatrix)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")

km.res6 <- kmeans(dmatrix, 6)
km.res7 <- kmeans(dmatrix, 7)
km.res8 <- kmeans(dmatrix, 8)
km.res9 <- kmeans(dmatrix, 9)
km.res10 <- kmeans(dmatrix, 10)
km.res11 <- kmeans(dmatrix, 11)
km.res12 <- kmeans(dmatrix, 12)
km.res13 <- kmeans(dmatrix, 13)
km.res14 <- kmeans(dmatrix, 14)
mkm.res6 <- mean(silhouette(km.res6$cluster, dmatrix)[, 3])
mkm.res7 <- mean(silhouette(km.res7$cluster, dmatrix)[, 3])
mkm.res8 <- mean(silhouette(km.res8$cluster, dmatrix)[, 3])
mkm.res9 <- mean(silhouette(km.res9$cluster, dmatrix)[, 3])
mkm.res10 <- mean(silhouette(km.res10$cluster, dmatrix)[, 3])
mkm.res11 <- mean(silhouette(km.res11$cluster, dmatrix)[, 3])
mkm.res12 <- mean(silhouette(km.res12$cluster, dmatrix)[, 3])
mkm.res13 <- mean(silhouette(km.res13$cluster, dmatrix)[, 3])
mkm.res14 <- mean(silhouette(km.res14$cluster, dmatrix)[, 3])
km_result <- c(mkm.res6,
mkm.res7,
mkm.res8,
mkm.res9,
mkm.res10,
mkm.res11,
mkm.res12,
mkm.res13,
mkm.res14)

plot(6:14, km_result, type="o", col="red", xlab = "Number of cluster", ylab = "Kmeans Silhouette average width")
abline(v = 13, lty =2)

silhouette(km.res13$cluster, dmatrix)

fviz_silhouette(silhouette(km.res13$cluster, dmatrix)) 

km13 <- kmeans(dmatrix, 13)

pam.res6 <- pam(dmatrix, 6)
pam.res7 <- pam(dmatrix, 7)
pam.res8 <- pam(dmatrix, 8)
pam.res9 <- pam(dmatrix, 9)
pam.res <- pam(dmatrix, 10)
pam.res11 <- pam(dmatrix, 11)
pam.res12 <- pam(dmatrix, 12)
pam.res13 <- pam(dmatrix, 13)
pam.res14 <- pam(dmatrix, 14)
pam_result <- c(pam.res6$silinfo$avg.width,
pam.res7$silinfo$avg.width,
pam.res8$silinfo$avg.width,
pam.res9$silinfo$avg.width,
pam.res$silinfo$avg.width,
pam.res11$silinfo$avg.width,
pam.res12$silinfo$avg.width,
pam.res13$silinfo$avg.width,
pam.res14$silinfo$avg.width)

plot(6:14, pam_result, type="o", col="red", xlab = "Number of cluster", ylab = "Pam Silhouette average width")

pam.res10$silinfo

fviz_silhouette(silhouette(pam.res10)) 

pam10 <- pam(dmatrix, 10)
summary(pam10)

#display the medoids of the clustering
medoids<-pam10$medoids
medoids

data_demographic[c(medoids),]

hc <- hclust(dmatrix, method="ward.D2")
ghc6 <- cutree(hc, k=6)
ghc7 <- cutree(hc, k=7)
ghc8 <- cutree(hc, k=8)
ghc9 <- cutree(hc, k=9)
ghc10 <- cutree(hc, k=10)
ghc11 <- cutree(hc, k=11)
ghc12 <- cutree(hc, k=12)
ghc13 <- cutree(hc, k=13)
ghc14 <- cutree(hc, k=14)
mghc6 <- mean(silhouette(ghc6, dmatrix)[, 3])
mghc7 <- mean(silhouette(ghc7, dmatrix)[, 3])
mghc8 <- mean(silhouette(ghc8, dmatrix)[, 3])
mghc9 <- mean(silhouette(ghc9, dmatrix)[, 3])
mghc10 <- mean(silhouette(ghc10, dmatrix)[, 3])
mghc11 <- mean(silhouette(ghc11, dmatrix)[, 3])
mghc12 <- mean(silhouette(ghc12, dmatrix)[, 3])
mghc13 <- mean(silhouette(ghc13, dmatrix)[, 3])
mghc14 <- mean(silhouette(ghc14, dmatrix)[, 3])
ghc_result <- c(mghc6,
mghc7,
mghc8,
mghc9,
mghc10,
mghc11,
mghc12,
mghc13,
mghc14)
plot(6:14, ghc_result, type="o", col="red", xlab = "Number of cluster", ylab = "Hierarchical Silhouette average width")
abline(v = 12, lty =2)

silhouette(ghc12, dmatrix)

fviz_silhouette(silhouette(ghc12, dmatrix))

hc <- hclust(dmatrix, method="ward.D2")
plot(hc)
rect.hclust(hc, k=8, border="skyblue")
rect.hclust(hc, k=10, border="red")
rect.hclust(hc, k=12, border="blue")
rect.hclust(hc, k=14, border="green")

ghc12 <- cutree(hc, k=12)

# add the results of the clustering to our demographic data
data_demographic_withclusters <- data.frame(data_demographic, km13$cluster, ghc12, pam10$clustering)
dem_wclus <-data.frame(data_demographic_withclusters)
head(dem_wclus)

a <- qplot(dem_wclus$km13, data=dem_wclus, fill=dem_wclus$marital, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Marital Status")+scale_x_continuous(breaks=c(seq(0:12)))
b <- qplot(dem_wclus$ghc12, data=dem_wclus, fill=dem_wclus$marital, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Marital Status")+scale_x_continuous(breaks=c(seq(0:11)))
c <- qplot(dem_wclus$pam10, data=dem_wclus, fill=dem_wclus$marital, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Marital Status")+scale_x_continuous(breaks=c(seq(0:9)))

d <- qplot(dem_wclus$km13, data=dem_wclus, fill=dem_wclus$parenthood, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Parenthood")+scale_x_continuous(breaks=c(seq(0:12)))
e <- qplot(dem_wclus$ghc12, data=dem_wclus, fill=dem_wclus$parenthood, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Parenthood")+scale_x_continuous(breaks=c(seq(0:11)))
f <- qplot(dem_wclus$pam10, data=dem_wclus, fill=dem_wclus$parenthood, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Parenthood")+scale_x_continuous(breaks=c(seq(0:9)))

g <- qplot(dem_wclus$km13, data=dem_wclus, fill=dem_wclus$country, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Country")+scale_x_continuous(breaks=c(seq(0:12)))
h <- qplot(dem_wclus$ghc12, data=dem_wclus, fill=dem_wclus$country, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Country")+scale_x_continuous(breaks=c(seq(0:11)))
i <- qplot(dem_wclus$pam10, data=dem_wclus, fill=dem_wclus$country, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Country")+scale_x_continuous(breaks=c(seq(0:9)))

j <- qplot(dem_wclus$km13, data=dem_wclus, fill=dem_wclus$gender, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Gender")+scale_x_continuous(breaks=c(seq(0:12)))
k <- qplot(dem_wclus$ghc12, data=dem_wclus, fill=dem_wclus$gender, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Gender")+scale_x_continuous(breaks=c(seq(0:11)))
l <- qplot(dem_wclus$pam10, data=dem_wclus, fill=dem_wclus$gender, binwidth = 1)+
            labs(x = "", y = "Count", fill = "Gender")+scale_x_continuous(breaks=c(seq(0:9)))

m <- qplot(dem_wclus$km13, data=dem_wclus, fill=as.character(dem_wclus$age), binwidth = 1)+
            labs(x = "", y = "Count", fill = "Age")+scale_x_continuous(breaks=c(seq(0:12)))
n <- qplot(dem_wclus$ghc12, data=dem_wclus, fill=as.character(dem_wclus$age), binwidth = 1)+
            labs(x = "", y = "Count", fill = "Age")+scale_x_continuous(breaks=c(seq(0:11)))
o <- qplot(dem_wclus$pam10, data=dem_wclus, fill=as.character(dem_wclus$age), binwidth = 1)+
            labs(x = "", y = "Count", fill = "Age")+scale_x_continuous(breaks=c(seq(0:9)))

grid.arrange(a,d,g,j,m, ncol=1, nrow = 5, top=("Result of Kmeans Clustering"))

grid.arrange(c,f,i,l,o, ncol=1, nrow = 5, top=("Result of Partioning Around Medoids Clustering"))

grid.arrange(b,e,h,k,n, ncol=1, nrow = 5, top=("Result of Hierarchical Clustering"))

qplot(dem_wclus$age, dem_wclus$km13.cluster, data=dem_wclus, color=dem_wclus$marital) +
        geom_jitter(width = 0.3, height = 0.3)+
        scale_y_continuous(breaks=c(seq(0:12)))+
        labs(title= "K-Means: Age Ranges and Marital Status by Cluster",x = "Age Ranges", y = "K-Means Clustering", color = "Marital Status")

qplot(dem_wclus$age, dem_wclus$pam10.clustering, data=dem_wclus, color=dem_wclus$marital) +
        geom_jitter(width = 0.3, height = 0.3)+
        scale_y_continuous(breaks=c(seq(0:9)))+
        labs(title= "PAM: Age Ranges and Marital Status by Cluster",x = "Age Ranges", y = "PAM", color = "Marital Status")

qplot(dem_wclus$age, dem_wclus$ghc12, data=dem_wclus, color=dem_wclus$marital) +
        geom_jitter(width = 0.3, height = 0.3)+
        scale_y_continuous(breaks=c(seq(0:11)))+
        labs(title= "Hierarchical Clust.: Age Ranges and Marital Status by Cluster",x = "Age Ranges", y = "Hierarchical Clustering", color = "Marital Status")

summary(data_demographic)

data_HM <- read.csv("/Users/jacquelineneef/Desktop/Kaggle/data/cleaned_hm.csv")
summary(data_HM)

data_joined <- join(x=data_HM, y=data_demographic, by = 'wid')
summary(data_joined)

data_joined <- data_joined[complete.cases(data_joined), ]

sapply(data_joined, class)

head(data_joined)

#get the joined data including the clustering
data_joined_CA <- join(x=data_HM, y=dem_wclus, by = 'wid')
data_joined_CA <- data_joined_CA[complete.cases(data_joined_CA), ]
head(data_joined_CA)

data_joined_CA <- data_joined_CA[,-c(3,4,5,6,7,8,10,11,12,13,14,15,16)]
head(data_joined_CA)

summary(data_joined_CA)

Contingency_Matrix <- as.matrix(cont_table)
Contingency_Matrix

n = sum(Contingency_Matrix)
Frequency_Matrix = Contingency_Matrix / n
Frequency_Matrix

column.masses = colSums(Frequency_Matrix)
row.masses = rowSums(Frequency_Matrix)
Expected_Frequencies = row.masses %o% column.masses
Expected_Frequencies

Residuals = Frequency_Matrix - Expected_Frequencies
Residuals

Indexed_Residuals = Residuals / Expected_Frequencies
Indexed_Residuals

# Correspondence Analysis
A <- data_joined_CA$predicted_category
B <- data_joined_CA$pam10.clustering
cont_table <- with(data_joined_CA, table(A,B)) # create a 2 way table
cat("1) Contingency Table")
print(cont_table)
cat("\n 2) Frequency Table of Row Profiles - Happy Moment Category is fixed")
prop.table(cont_table, 1) # row percentages
cat("\n 3) Frequency Table of Column Profiles - Clustering is fixed")
prop.table(cont_table, 2) # column percentages

# compute a simple correspondence analysis based on the singular value decomposition
ca_result <- ca(cont_table)
print(ca_result)

plot(ca_result, arrow=c(T,F), mass = TRUE) #symmetric plot - the rows and columns are in the principal coordinates

plot(ca_result, mass = TRUE, map ="colgreen") # asymmetric map

head(data_joined)

data_joined_tree <- data_joined[,-c(1,2,3,4,5,6,7,8)]
head(data_joined_tree)

train <-sample(1:length(data_joined_tree$predicted_category), 0.75*length(data_joined_tree$predicted_category), replace=F)
data_train <- data_joined_tree[train,]
data_test <- data_joined_tree[-train,]
target <- data_train$predicted_category ~ .
cdt <- partykit::ctree(data_train$predicted_category~., data=data_train)
cdt
table(predict(cdt,newdata=data_test), data_test$predicted_category)

plot(cdt, type = "simple")

confusion_matrix <-table(predict(cdt,newdata=data_test), data_test$predicted_category)
confusion_matrix

accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

head(predict(cdt, type = "prob"))

head(predict(cdt, type = "prob"))head(predict(cdt, type = "prob"))head(predict(cdt, type = "response"))

print(c(data_joined_tree[1,],predict(cdt, type = "prob")[[1]]))

response <- predict(cdt, type = "response")
prob <- predict(cdt, type = "prob")
rls <- partykit:::.list.rules.party(cdt)
rule <- rls[as.character(predict(cdt, type = "node"))]
data_train <- cbind(data_train,response,prob,rule)

data_train[!duplicated(data_train$rule),][,c(1,2,3,4,5,6,7)]
nrow(data_train[!duplicated(data_train$rule),])response <- predict(cdt, type = "response")
prob <- predict(cdt, type = "prob")
rls <- partykit:::.list.rules.party(cdt)
rule <- rls[as.character(predict(cdt, type = "node"))]
data_train <- cbind(data_train,response,prob,rule)

data_train <- cbind(data_train,data_joined[train,5])

names(data_train)[16] <- "cleaned_hm"
head(data_train)

WhatMakesYouHappy <- function(age , country, gender, marital , parenthood){
  a <- "[18.00,25.00]"
  b <- "[26.00,30.00]"
  c <- "[31.00,37.00]"
  d <- "[38.00,65.00]"
  if(age < 26 ){ age <-  as.character(a)}
  if(age > 25 && age < 31){age <- as.character(b)}
  if(age > 30 && age < 38){age <- as.character(c)}
  if(age > 37 ){age <- as.character(d)}
  for(i in 1:length(data_train$response)){
    if(age == data_train$age[i] && country == data_train$country[i] && gender  == data_train$gender[i] && marital == data_train$marital[i] && parenthood == data_train$parenthood[i]){
      predicted_prob <- 0
      if(data_train$response[i] == "achievement"){
          predicted_prob <- data_train$achievement[i]
      }
      if(data_train$response[i] == "affection"){
          predicted_prob <- data_train$affection[i]
      }
      if(data_train$response[i] == "bonding"){
          predicted_prob <- data_train$bonding[i]
      }
      if(data_train$response[i] == "enjoy_the_moment"){
          predicted_prob <- data_train$enjoy_the_moment[i]
      }
      if(data_train$response[i] == "exercise"){
          predicted_prob <- data_train$exercise[i]
      }
      if(data_train$response[i] == "leisure"){
          predicted_prob <- data_train$leisure[i]
      }
      if(data_train$response[i] == "nature"){
          predicted_prob <- data_train$nature[i]
      }
      cat(paste("Your predicted category is :",  data_train$response[i],  "with a probability of :", round(predicted_prob*100),"%"))
      cat("\nHere you can find some examples of what made other people sharing the same category happy:\n")  
      print(data_train$cleaned_hm[data_train$predicted_category == data_train$response[i]][sample(1:length(data_train$cleaned_hm[data_train$predicted_category == data_train$response[i]]), 5, replace=F)])
      break
    }
  }
}

WhatMakesYouHappy(22,"Other","f","single","n")

WhatMakesYouHappy(29,"USA","m","married","y")

WhatMakesYouHappy(40,"USA","m","married","y")

WhatMakesYouHappy(27,"IND","f","married","n")

WhatMakesYouHappy(21,"USA","m","single","n")
