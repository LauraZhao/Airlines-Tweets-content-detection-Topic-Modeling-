rm(list=ls())
#####   Loading packages  #####
library(topicmodels)
library(tm)
library(e1071)
library(rpart)

#####   Load data   #####
setwd('C:/Simon/CIS434/Final Project')
neg_tweets <- read.csv("complaint1700.csv", header=TRUE, sep=',', quote='"')
neg_tweets$sentiment = 0
pos_tweets <- read.csv("noncomplaint1700.csv", header=TRUE, sep=',', quote='"')
pos_tweets$sentiment = 1
tweets = rbind(neg_tweets,pos_tweets)
##### data cleaning #####
cleaning <- function(df){
    df$tweet = gsub("&amp", "", df$tweet)
    df$tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$tweet)
    df$tweet = gsub("@\\w+", "", df$tweet)
    df$tweet = gsub("[[:punct:]]", "", df$tweet)
    df$tweet = gsub("[[:digit:]]", "", df$tweet)
    df$tweet = gsub("http\\w+", "", df$tweet)
    df$tweet = gsub("[ \t]{2,}", "", df$tweet)
    df$tweet = gsub("^\\s+|\\s+$", "", df$tweet)
    df$tweet <- iconv(df$tweet, "UTF-8", "ASCII", sub="")
    df$tweet <- tolower(df$tweet)
    return(df)
}

tweets = cleaning(tweets)
y = tweets$sentiment
docs = Corpus(VectorSource(tweets$tweet))

#####   build DTM   #####
mystop <- c("airline","flight")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, 
                   stopwords=c(stopwords("english"), mystop), 
                   stripWhitespace=T, stemming=T
                   )
dtm.full= DocumentTermMatrix(docs, control=dtm.control)
dtm = removeSparseTerms(dtm.full,0.99)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- docs[idx]
dtm = dtm[idx,]
X = as.matrix( dtm )
#dim(X)
y = y[idx]
#length(y)
dict_X = colnames(dtm)
colnames(X) = c(1:182)


# colnames(X)
# weight_more = c("thank","awful","bad","unpleasant",'unhappy','sucks','shit','shady','kick','kicked',
#                 'fuckup','poor','ruin','kicking','disturbing','frustrating','frustrated','horrible',
#                 'miss','missing','delay','delaying','delayed','cancel','cancelled','cancelling',"can",
# 
#                                 'thanks','disgusting','never','again')
# for (word in weight_more){
#     if (word %in% colnames(X)){
#         X[,word] = X[,word]*10
#     }
# }
    






svm.model <- svm(y ~ ., data = X, kernel="radial", degree = 3)



#####   Topic Modeling   #####
### tune the dimension ###

# lda.model = LDA(dtm.full, 40)
# 
# myposterior <- posterior(lda.model) # get the posterior of the model
# 
# # topic distribution of each document, one row per document, one column per topic
# coins = myposterior$topics
# dices = myposterior$terms  #defination of topic
# dict = colnames(dices)
# #barplot(coins[1:2,], beside=TRUE, col=c("red","blue")) # plot topic distribution of specific documents
# y = y[as.numeric(rownames(coins))]
# X = coins


###########################################
#############   Evaluation   ##############
###########################################

Evaluation <- function(pred, true, class)
{
    
    tp <- sum( pred==class & true==class)
    fp <- sum( pred==class & true!=class)
    tn <- sum( pred!=class & true!=class)
    fn <- sum( pred!=class & true==class)
    #accuracy = (tn+tp)/length(pred)
    precision <- tp/(tp+fp)
    recall <- tp/(tp+fn)
    F1 <- 2/(1/precision + 1/recall)
    #precision
    F1
}

#####  SVM   #####
# set.seed(1) # fixing the seed value for the random selection guarantees the same results in repeated runs
# n=length(y)
# n1=round(n*0.8)
# n2=n-n1
# train=sample(1:n,n1)

#svm.model <- svm(y ~ ., data = X, kernel="radial", degree = 3)

# X_val = X[-train,]
# dim(X_val)
# pred <- predict(svm.model, X[-train,])
# pred.class <- as.numeric( pred>0.35)
# #table(pred, y[-train])
# Evaluation(pred.class, y[-train], 1)
#Evaluation(pred.class, y[-train], 0)

##### rpart #####

fit = rpart(y ~ ., 
            data=df,
            control=rpart.control(xval=10, minsplit = 20),
            method = 'class')
##### Model Evaluation #####
set.seed(1) 
nFold = 10
#Step 1: Randomly choose which fold each row is in 
valNum = floor(runif(nrow(X))*nFold)+1
model_evaluation = function(xval,minsplit){
    modelPerformance = rep(NA,nFold)
    for(fold in 1:nFold){
        #print(paste('Fold Num',fold))
        #Step 2i: Get the training and validation data for this fold
        trainingX = subset(X,valNum!=fold)
        validationX = subset(X,valNum==fold)
        trainingy = y[valNum!=fold]
        validationy = y[valNum==fold]
        #Step 2ii: Estimate the models for this training data
        
        model = rpart(y ~ ., 
                    data=df,
                    control=rpart.control(xval=xval, minsplit = minsplit),
                    method = 'class')
        #Step 2iii: Calculate out of sample F1 for this validationData
        pred = predict(model, validationX)
        
        eva_pos = Evaluation(pred, validationy, 1)
        #Store model performance		
        modelPerformance[fold] = eva_pos
    }
    return(mean(modelPerformance))
}
model_evaluation(10,20)
model_evaluation(10,10)
model_evaluation(10,5)
model_evaluation(20,20)
model_evaluation(20,10)
model_evaluation(20,5)


##### nb #####
nb.model <- naiveBayes( X, factor(y),laplace = 1) # encode the response as a factor variable
##### Model Evaluation #####
set.seed(1) 
nFold = 10
#Step 1: Randomly choose which fold each row is in 
valNum = floor(runif(nrow(X))*nFold)+1
model_evaluation = function(laplace,threshold,threshold2){
    modelPerformance = rep(NA,nFold)
    
    for(fold in 1:nFold){
        #print(paste('Fold Num',fold))
        #Step 2i: Get the training and validation data for this fold
        trainingX = subset(X,valNum!=fold)
        validationX = subset(X,valNum==fold)
        trainingy = y[valNum!=fold]
        validationy = y[valNum==fold]
        #Step 2ii: Estimate the models for this training data
        nb.model = naiveBayes( X, factor(y),laplace = laplace)
        #Step 2iii: Calculate out of sample F1 for this validationData
        pred = predict(model, validationX,threshold = threshold,type = 'raw')
        pred.class <- as.numeric( pred>threshold2 )
        eva_pos = Evaluation(pred.class, validationy, 1)
        #Store model performance		
        modelPerformance[fold] = eva_pos
    }
    return(mean(modelPerformance))
}
for (i in c(0,1)){
    for (j in c(0.001,0.05)){
        for (p in c(0.5,0.7,0.8,0.9)){
            model_evaluation(i,j,p)
        }
    }
}





##### Model Evaluation #####
set.seed(1) 
nFold = 10
#Step 1: Randomly choose which fold each row is in 
valNum = floor(runif(nrow(X))*nFold)+1

model_evaluation = function(degree,gamma,threshold){
    modelPerformance = rep(NA,nFold)

    for(fold in 1:nFold){
        #print(paste('Fold Num',fold))
        #Step 2i: Get the training and validation data for this fold
        trainingX = subset(X,valNum!=fold)
        validationX = subset(X,valNum==fold)
        trainingy = y[valNum!=fold]
        validationy = y[valNum==fold]
        #Step 2ii: Estimate the models for this training data
        model = svm(trainingy ~ ., data = trainingX, kernel="radial", degree = degree,gamma = gamma)
        #Step 2iii: Calculate out of sample F1 for this validationData
        pred = predict(model, validationX)
        pred.class = as.numeric( pred>threshold )
        eva_pos = Evaluation(pred.class, validationy, 1)
        #Store model performance		
        modelPerformance[fold] = eva_pos
    }
    return(mean(modelPerformance))
}


model_evaluation(2,0.01)
model_evaluation(2,0.05)
model_evaluation(2,0.005,0.6)
model_evaluation(3,0.01)
model_evaluation(3,0.05)
model_evaluation(3,0.001,0.5)
model_evaluation(4,0.01)
model_evaluation(4,0.05)
model_evaluation(4,0.001,0.5)

############################################
############################################
# install.packages("SentimentAnalysis")
# library(SentimentAnalysis)
# 
# neg_tweets <- read.csv("complaint1700.csv", header=TRUE, sep=',', quote='"',stringsAsFactors = FALSE)
# ntweets= neg_tweets$tweet
# typeof(ntweets)
# neg_tweets$senti = NA
# 
# for(i in 1:length(ntweets)){
#     sentiment <- analyzeSentiment(ntweets[i])
#     senti = convertToBinaryResponse(sentiment)$SentimentGI
#     neg_tweets$senti[i] = senti
# }


##### loading test data #####

mydata <- read.csv("mydata.csv", header=TRUE, sep=',', quote='"')
docs_c <- Corpus(VectorSource(mydata$tweet))
mystop <- c("airline",'flight')
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, 
                   stopwords=c(stopwords("english"), mystop), 
                   stripWhitespace=T, stemming=T,dictionary = dict_X)
dtm.full_c = DocumentTermMatrix(docs_c, control=dtm.control)
idx_c <- rowSums(as.matrix(dtm.full_c))>0
newdocs_c <- docs_c[idx_c]
dtm.full_c = dtm.full_c[idx_c,]
# dim(dtm.full_c)
m_c = as.matrix(dtm.full_c)


# weight_more = c("thank","awful","bad","unpleasant",'unhappy','sucks','shit','shady','kick','kicked',
#                 'fuckup','poor','ruin','kicking','disturbing','frustrating','frustrated','horrible',
#                 'miss','missing','delay','delaying','delayed','cancel','cancelled','cancelling',"can",
#                 
#                 'thanks','disgusting','never','again')
# for (word in weight_more){
#     if (word %in% colnames(X)){
#         m_c[,word] = m_c[,word]*10
#     }
# }
# 
# 






#cosim = function(x,y) sum(x*y)/(norm(matrix(x,1),'f')*norm(matrix(y,1),'f'))

#dice_new = data.frame(matrix(NA,4555,20))
#for (i in 1:dim(m_c)[1]){
#     for (j in 1:20){
#         dice_new[i,j] = cosim(m_c[i,],dices[j,])  
#     }
# }

dim(m_c)
# df_c = data.frame(m_c)
# colnames(df_c) = c(1:182)
# df_c$y = 1

# dice_new = dice_new[1:3793,]
# colnames(dice_new) = c(1:20)

##### maaking prediction #####

colnames(m_c) = c(1:182)
pred_m = predict(svm.model,m_c)
res = mydata[idx_c,]
#res = res[pred_nb[,2] == 1,]
res = res[pred_m >0.98,]
write.csv(res,'Ruixuan_Zhao.csv')

#pred_nb <- predict( nb.model,m_c,type = 'raw',threshold = 0.5)
#pred_rp=predict(fit,df_c , type="prob")
#res = res[pred_rp[,2]>0.56215370,]

