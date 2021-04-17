# Human Activity Recognition

This analysis is to build a model to predict how well certain activities is done.

Some exploratory data analysis shows that there is much of features that is full of NA's so we eliminate the features from 160 to 55.

```{r echo=TRUE}
library(caret)
library(parallel)
library(doParallel)
#reading data
setwd("D:/Others/R_Projects/Human-Activity-Recognition-PML-Course")
tr1 <- read.csv("pml-training.csv" ,na.strings = c("NA",""))
ts1 <- read.csv("pml-testing.csv")
#removing NAs
nar <- is.na(tr1)
del <- NULL
for(i in 1:length(names(tr1))){
        t <- table(nar[,i])
        if (t[1] < 5000) {del <- c(del,i)}
}
tr2 <- tr1[,-del]
tr3 <- tr2[,-c(1,3,4,5,6)]
tr3$user_name <- as.factor(tr3$user_name)
tr3$classe <- as.factor(tr3$classe)
#cross validation
n <- createDataPartition(tr3$classe, p=0.7, list = FALSE)
tr <- tr3[n,]
tc <- tr3[-n,]
names(tr)
table(tr$classe)
```

After trying a simple tree predictor using rpart method, the average accuracy was 0.63, so I tried boosting with trees. the model has been tested on cross validation set.

```{r echo=TRUE, cache=TRUE}
set.seed(33835)
f <- train(classe ~ ., method ="gbm", data = tr, verbose = FALSE)
p <- predict(f,tc)
confusionMatrix(p,tc$classe)
```



```{r include=FALSE}
ts2 <- ts1[,-del]
ts <- ts2[,-c(1,3,4,5,6)]
ts$user_name <- as.factor(ts$user_name)
```
