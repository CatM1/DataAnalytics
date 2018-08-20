---
title: "HW2_McNabb"
author: "Catherine McNabb"
date: "August 20, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Problem 1) Flight Info at Austin Bergstrom

First, I'm going to read in the data.

```{r Warning=FALSE}
abia = read.csv("ABIA.csv")
```

Then, I am going to create a new column called "total delays", so we can look at all of the delays together. 

```{r}
abia$totaldelays <- rowSums(abia[,25:29])
attach(abia)
```

Let's compare total delays by origin and destination to and from Austin Bergstrom. 

```{r echo=FALSE}
par(mfrow=c(2,1)) #look at total delays
plot(Origin, totaldelays, main="Total Delays by Origin")
plot(Dest, totaldelays, main="Total Delays by Destination")
```

It appears that origin and destination delays are pretty comparable for each airport. 

Now, I'm going to make a graph that shows the network of airports with flights going TO AUS. The distance the node from AUS indicates how common that airport is, with the closer ones being more common, and the size of the node indicates how many delays, with bigger nodes indicating bigger delays. 

```{r echo=FALSE}
#make a network
library(igraph) 
#trim the list
par(mfrow=c(1,1))
alt.delays = abia[,c('Origin','Dest','totaldelays')]
alt.delays.new = alt.delays[which(alt.delays$totaldelays > 75),]
alt.delays.new2 = alt.delays.new[,c('Origin','Dest')]
alt.delays.matrix = as.matrix(alt.delays.new2)
alt.delays.graph = graph.edgelist(alt.delays.matrix,directed=TRUE)

plot(alt.delays.graph,edge.curved=FALSE,edge.arrow.size=scale(alt.delays.new$totaldelays)*.5,vertex.color="pink",margin=-.25)

```


## Problem 2) Predicting Authors

First, I need to load in the data and read it in. I will start with training data.

```{r echo=FALSE, warning=FALSE}
library(tm) 
library(magrittr)
library(slam)
library(proxy)


file_list = Sys.glob('ReutersC50/C50train/*/*.txt')
author_list = Sys.glob('ReutersC50/C50train/*')

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

reuters_train = lapply(file_list, readerPlain) 

```

Then, I need to clean up the names of the documents and authors. 

```{r echo=FALSE}
#create a file list
mynames = file_list %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=2) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist

#create a list of authors so we know who we are trying to predict
myauthors = author_list %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=1) } %>%
  unlist

```

Then, I will clean up the data, so get rid of common words, numbers, weird symbols, etc. 

```{r echo=FALSE}
names(reuters_train) = mynames
documents_raw = Corpus(VectorSource(reuters_train))

my_documents = documents_raw
my_documents = tm_map(my_documents, content_transformer(tolower)) # make everything lowercase
my_documents = tm_map(my_documents, content_transformer(removeNumbers)) # remove numbers
my_documents = tm_map(my_documents, content_transformer(removePunctuation)) # remove punctuation
my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) ## remove excess white-space

my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))
DTM_reuters_train = DocumentTermMatrix(my_documents)
DTM_reuters_train = removeSparseTerms(DTM_reuters_train, 0.95)

```

Then do the same with test, including taking care of any words that aren't in the training data. 

```{r echo=FALSE,warning=FALSE}
file_list2 = Sys.glob('ReutersC50/C50test/*/*.txt')
author_list2 = Sys.glob('ReutersC50/C50test/*')

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

reuters_test = lapply(file_list2, readerPlain) 

#create a file list
mynames2 = file_list2 %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=2) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist

#create a list of authors so we know who we are trying to predict
myauthors2 = author_list2 %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=1) } %>%
  unlist

#set up words from the whole corpus so they are usable
names(reuters_test) = mynames
documents_raw2 = Corpus(VectorSource(reuters_test))

my_documents2 = documents_raw2
my_documents2 = tm_map(my_documents2, content_transformer(tolower)) # make everything lowercase
my_documents2 = tm_map(my_documents2, content_transformer(removeNumbers)) # remove numbers
my_documents2 = tm_map(my_documents2, content_transformer(removePunctuation)) # remove punctuation
my_documents2 = tm_map(my_documents2, content_transformer(stripWhitespace)) ## remove excess white-space

my_documents2 = tm_map(my_documents2, content_transformer(removeWords), stopwords("en"))


DTM_reuters_test = DocumentTermMatrix(my_documents2,control = list(dictionary=Terms(DTM_reuters_train)))


```

Now, I need to do PCA to get the right amount of components. 

```{r echo=FALSE}
tfidf_train = weightTfIdf(DTM_reuters_train)
tfidf_test = weightTfIdf(DTM_reuters_test)

tfidf_train_df <- as.data.frame(as.matrix(tfidf_train))
tfidf_test_df <- as.data.frame(as.matrix(tfidf_test))

pc_author = prcomp(tfidf_train_df)
pc_author_test <- predict(pc_author, newdata = tfidf_test_df)
pc_author_test <- as.data.frame(pc_author_test)

pve = summary(pc_author)$importance[3,]
plot(pve)

```

This looks like cutting the data at 100 will do. 

Now, we need to turn this into models. 

First, random forest: 

```{r echo=FALSE}
library(randomForest)
set.seed(9)
n_cut = 100
X = pc_author$x[,1:n_cut]
y = myauthors
X_test = pc_author_test[,1:n_cut]
y_test = sapply(strsplit(names(reuters_test), "/"), "[", 3)

TrainSet <- cbind(as.data.frame(X),y)
ValidSet <- cbind(as.data.frame(X_test),y_test)

rffit <- randomForest(y~.,TrainSet,ntree=200)
mean(predict(rffit,TrainSet)== y )
mean(predict(rffit,ValidSet)== y )
```

Unfortunately, seems like we did not predict very well on the test set. We did a LOT better on the training set, which isn't really helpful. 

Now, I'll try naive bayes. 

```{r echo=FALSE}
#boosting
library(gbm)
#mode("X") = "numeric"
set.seed(9)

n_cut = 100

#already set up training and test set

boost.author <- gbm(y ~.,data=TrainSet, n.trees =100 ,shrinkage = 0.01,distribution = "multinomial",interaction.depth=4, cv.folds = 5)

#== checking accuracy on trainset
pred=as.data.frame(predict(boost.author,newdata =TrainSet,n.trees=100,type="response"))
pred_val = sub("*\\.[0-9]+", "", colnames(pred)[apply(pred,1,which.max)])
mean(pred_val== y )

#== checking accuracy on test data
pred=as.data.frame(predict(boost.author,newdata =ValidSet,n.trees=100,type="response"))
pred_val = sub("*\\.[0-9]+", "", colnames(pred)[apply(pred,1,which.max)])
mean(pred_val== y )

```

Again, very, very poor showing for the the testing set. I probably did something wrong, but this does not bode well for predicting authors. 

## Problem 3) Groceries List

First, I'm going to read in the data and correct libraries. 

```{r warning=FALSE}
library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)
groceries_raw = read.csv("groceries.txt",header=FALSE)
```

Then, I will set the data up to be in the correct format.

```{r}
x = matrix(0,nrow= nrow(groceries_raw)*4,ncol=2)

counter = 1
x = data.frame(x)
for (i in 1:15296) {
  g = groceries_raw[i,] 
  for (j in 1:length(g)) {
    x[counter,1] = i
    x[counter,2] = as.character(g[[j]])
    counter = counter+1
  }
}

```

Now, I will format the data so we can graph using different support and confidence rules. 

```{r}
x$X1 = factor(x$X1)
baskets = split(x=x$X2, f=x$X1)

## Remove duplicates ("de-dupe")
baskets = lapply(baskets, unique)

#make into special arules class
shoptrans = as(baskets, "transactions")

grocerylist = apriori(shoptrans, 
                     parameter=list(support=.005, confidence=.1, maxlen=5))

```

I wanted to look at the results for multiple combinations of support and confidence, so I tried several. 

```{r}
sub1 = subset(grocerylist, subset=confidence > 0.01 & support > 0.005)
sub2 = subset(grocerylist, subset=confidence > 0.05 & support > 0.01)
sub3 = subset(grocerylist, subset=confidence > 0.0075 & support > 0.0045)

```

And then I found that the one with the most lax rules - confidence > 0.05 and support > 0.01 seemed to be the ones with the best grouping. Perhaps because there are some items that are bought so often, 'strict' rules seem to overfit, and the 'looser' rules work better. 

```{r}
plot(sub2, method='graph')
```





