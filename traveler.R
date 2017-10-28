
#import important libraries
install.packages("pROC")

library(pROC)
library(zipcode)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)  
library(mice)
library(VIM)
library(Amelia)
library(e1071)
library(rpart.plot) 
library(rpart) 
library(class)
library(neuralnet)
library(nnet)
library(xgboost)
library(Matrix)

df.train=read.csv('/Users/pawanshivhare/Desktop/Kaggle Competition/Travelers/data/train.csv')
df.test=read.csv('/Users/pawanshivhare/Desktop/Kaggle Competition/Travelers/data/test.csv')
str(df.train)

# Check Missing Values
any(is.na(df.train))

md.pattern(df.train)
aggr_plot = aggr(df.train, col=c('navyblue','red'), 
                 numbers=TRUE, sortVars=TRUE, labels=names(data), 
                 cex.axis=1, gap=3, ylab=c("Histogram of missing data","Pattern"))

# dropping NA values since 
df.policy=na.omit(df.train)
md.pattern(df.train)

odds=function(x){
  out=NULL
  
  for(i in 1:length(x)){
    y=x[1:i]
    p=mean(y==1)
    out[i]=p/(1-p)
  }
  return(out)
}

df.policy$odds=odds(df.policy[,"cancel"])
df.policy$l.odds=log(df.policy$odds)


#df.policy = df.policy %>% 

data(zipcode)
zipinfo= zipcode %>% mutate(zip.code=zip) %>% select(zip.code,latitude,longitude)

df.policy=merge(x=df.policy,y=zipinfo,by="zip.code",all.x = T)

df.policy[df.policy$ni.age >100,"ni.age"]

str(df.policy)

#Check Distribution of Num Variables

ggplot(df.policy,aes(l.odds,tenure)) + geom_point()
ggplot(df.policy,aes(ni.age)) + geom_histogram() #some customers are more than 100 years old
ggplot(df.policy,aes(premium)) + geom_histogram()
ggplot(df.policy,aes(tenure)) + geom_histogram()
ggplot(df.policy,aes(len.at.res)) + geom_histogram()
ggplot(df.policy,aes(latitude)) + geom_histogram()
ggplot(df.policy,aes(longitude)) + geom_histogram()

# Check Distribution of Classes

ggplot(df.policy,aes(ni.gender)) + geom_bar(stat = 'count')
ggplot(df.policy,aes(factor(ni.marital.status))) + geom_bar(stat = 'count')
ggplot(df.policy,aes(factor(claim.ind))) + geom_bar(stat = 'count')
ggplot(df.policy,aes(factor(n.adults))) + geom_bar(stat = 'count')
ggplot(df.policy,aes(factor(n.children))) + geom_bar(stat = 'count')
ggplot(df.policy,aes(sales.channel)) + geom_bar(stat = 'count')
ggplot(df.policy,aes(coverage.type)) + geom_bar(stat = 'count')
ggplot(df.policy,aes(dwelling.type)) + geom_bar(stat = 'count')
ggplot(df.policy,aes(house.color)) + geom_bar(stat = 'count')

ggplot(df.policy,aes(credit)) + geom_bar(stat = 'count')

ggplot(df.policy,aes(factor(cancel))) + geom_bar(stat = 'count') # contains -1



# check relationship with target

ggplot(df.policy,aes(factor(cancel),tenure)) + geom_boxplot() # not important
ggplot(df.policy,aes(factor(cancel),premium)) + geom_boxplot() # not important
ggplot(df.policy,aes(factor(cancel),ni.age)) + geom_boxplot() # not important
ggplot(df.policy,aes(factor(cancel),len.at.res)) + geom_boxplot() # not important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=claim.ind,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') # important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=n.adults,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') # important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=n.children,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') # important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=ni.gender,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') # not important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=ni.marital.status,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') # not important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=sales.channel,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') #  important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=coverage.type,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') # not important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=dwelling.type,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') # not important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=house.color,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') # not important

ggplot(df.policy[df.policy$cancel>=0,],aes(x=credit,y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') #important

ggplot(df.policy[df.policy$cancel>=0,],aes(latitude,longitude)) + 
  geom_point(aes(color=factor(cancel)),size=4,alpha=0.5)

ggplot(df.policy[df.policy$cancel>=0,],aes(x=factor(zip.code),y=cancel))+
  stat_summary(fun.y = mean,geom = 'bar',color='blue',fill='blue') #important


# data cleansing

# drop observations where cancel = -1, drop observations where age > 100

df.policy=filter(df.policy,(ni.age<= 100 & cancel != -1))
df.policy1=mutate(df.policy,zip.code=factor(zip.code),
                 claim.ind=factor(claim.ind),
                 n.adults=factor(n.adults),
                 n.children=factor(n.children),
                 ni.marital.status=factor(ni.marital.status),
                 cancel=factor(cancel))

str(df.policy1.train.2)

sample=sample.split(df.policy1$cancel,0.7)
df.policy1.train=df.policy1[sample==T,]
df.policy1.test=df.policy1[sample==F,]

mean(df.policy1$cancel==1)
mean(df.policy1.train$cancel==1)
mean(df.policy1.test$cancel==1)

# lets build models

df.policy1.train.2=select(df.policy1.train,-id,
                          -latitude,-longitude,
                          -year,-zip.code,-dwelling.type,
                          -premium,-house.color,-ni.gender,
                          -coverage.type, -ni.age,-tenure,-len.at.res, -ni.marital.status)

#,-coverage.type,
#-premium

#df.policy1.train.2=filter(df.policy1.train.2,(is.infinite(l.odds)==F))

#lm.model=lm(l.odds ~ .,data = df.policy1.train.2)
#summary(lm.model)


glm.model=glm(formula=cancel ~ ., family = binomial(link='logit'),data = df.policy1.train.2)
summary(glm.model)

# train Stats
predictions=predict(glm.model,df.policy1.train,type = "response")
predictions.train=ifelse(predictions>0.5,1,0)
mean(predictions.train != df.policy1.train$cancel)
table(df.policy1.train$cancel,predictions.train)

# test Stats
predictions=predict(glm.model,df.policy1.test,type = "response")
predictions.test=ifelse(predictions>0.5,1,0)
mean(predictions.test != df.policy1.test$cancel)
table(df.policy1.test$cancel,predictions.test)

auc(df.policy1.test$cancel,predictions)

# random forests model

rf.model=randomForest(cancel ~ ., data = df.policy1.train.2,importance = TRUE, ntree=200)
summary(rf.model) # this is an overfit model, you need to denoise the features

# train stats
rf.model$confusion
rf.model$importance

rf.train.error=mean(rf.model$predicted!=df.policy1.train.2$cancel)
1-rf.train.error

# test stats

rf.test.predictions=predict(rf.model,df.policy1.test)
rf.test.error=mean(rf.test.predictions!=df.policy1.test$cancel)
1-rf.test.error
table(df.policy1.test$cancel,rf.test.predictions)

#deciding the number of trees in forest

fixtree=function(ns,ne,train,test){
 
  ytr=train[,'cancel']
  yte=test[,'cancel']
   
error.train=NULL
error.test=NULL
nt=NULL

for (i in ns:ne){
  m1=randomForest(cancel ~ .,data=train,ntree=i)
  p2 = predict(m1,train)
  error.train[i]=mean(p2!=ytr)
  #error.train[i]=mean(m1$predicted!=ytr)
  p1 = predict(m1,test)
  error.test[i]=mean(p1!=yte)
  nt[i]=i
  m1=NULL
}
out=cbind.data.frame(nt=nt[ns:ne],error.train=error.train[ns:ne],error.test=error.test[ns:ne])
return(out)
}

y=fixtree(200,300,df.policy1.train.2,df.policy1.test)

ggplot(y,aes(x=as.numeric(nt))) + geom_line(aes(y=error.train),color='blue')+ 
  geom_line(aes(y=error.test),color='green') 

# building nearest neighbor model

df.policy1.train.3=select(df.policy1.train.2,-cancel)
df.label=df.policy1.train.2$cancel

df.test=select(df.policy1.test,-id,
               -latitude,-longitude,
               -year,-zip.code,-dwelling.type,
               -premium,-house.color,-ni.gender,-ni.marital.status,
               -coverage.type, -ni.age,-tenure,-len.at.res, -cancel)


df.policy1.train.3=model.matrix(~ .,df.policy1.train.3)
df.policy1.train.3=as.data.frame(df.policy1.train.3[,2:ncol(df.policy1.train.3)])

df.test=model.matrix(~ .,df.test)
df.test=as.data.frame(df.test[,2:ncol(df.test)])

predictions <- knn(df.policy1.train.3,df.test,df.label,k=22)
mean(df.policy1.test$cancel != predictions)


fix_k=function(train,test,train.class,test.class){
  prediction.k=NULL
  error=NULL
  predictions=NULL
  for(i in 20:40){
    predictions = knn(train,test,train.class,k=i)
    error[i]=as.numeric(mean(test.class != predictions))
    prediction.k[i]=i
  }
  
  out=cbind.data.frame(prediction.k,error)
  return(out)
}

y=fix_k(df.policy1.train.3,df.test,df.label,df.policy1.test$cancel)

ggplot(y,aes(prediction.k,error)) + geom_point(size=2) +
  geom_line(lty="dotted",color='red')+theme_classic()

auc(df.policy1.test$cancel, predictions)


# lets build neural net model

df.test=select(df.policy1.test,-id,
              -latitude,-longitude,
               -year,-zip.code,-dwelling.type,
               -premium,-house.color,-ni.gender, -ni.marital.status,
               -coverage.type, -ni.age,-tenure,-len.at.res,-cancel)

df.policy1.train.3=model.matrix(~ .,df.policy1.train.2)
df.policy1.train.3=df.policy1.train.3[,2:ncol(df.policy1.train.3)]

df.test=model.matrix(~ .,df.test)
df.test=df.test[,2:ncol(df.test)]

n <- names(as.data.frame(df.policy1.train.3))
f <- as.formula(paste("cancel1 ~", paste(n[!n %in% c("cancel1","(Intercept)")], collapse = " + ")))
f

nn.model=neuralnet(f,data = df.policy1.train.3, hidden = c(5,3),linear.output = FALSE,stepmax =1e6)
predicted.nn.values = neuralnet::compute(nn.model,df.policy1.train.3[,1:24])



predictions <- ifelse(predicted.nn.values$net.result>0.5,1,0)

misClasificError <- mean(predictions != df.policy1.train.2$cancel)
print(paste('Accuracy',1-misClasificError))
table(df.policy1.train.2$cancel,predictions)

predicted.nn.values = neuralnet::compute(nn.model,df.test)



predictions <- ifelse(predicted.nn.values$net.result>0.5,1,0)

misClasificError <- mean(predictions != df.policy1.test$cancel)
print(paste('Accuracy',1-misClasificError))
table(df.policy1.test$cancel,predictions)

# Random forests okay, Neural network over fit, logistic slightly overfit, Xgboost overfit


df.policy1.train.3=select(df.policy1.train.2,-cancel)

df.test=select(df.policy1.test,-id,
               -latitude,-longitude,
               -year,-zip.code,-dwelling.type,
               -premium,-house.color,-ni.gender,-ni.marital.status,
               -coverage.type, -ni.age,-tenure,-len.at.res, -cancel)



n.data=sparse.model.matrix(~ .,data=df.policy1.train.3)
n.label=as.numeric(levels(df.policy1.train.2$cancel))[df.policy1.train.2$cancel]

dtrain <- xgb.DMatrix(data = n.data, label = n.label)

bst <- xgb.cv(data = dtrain, nfold = 5, max_depth=15,
              nthread=2,
              nrounds = 20, objective = "binary:logistic",
              early_stopping_rounds = 3, maximize = FALSE)


#bst <- xgboost(data = dtrain, max_depth = 15, eta = 1, 
               #nthread = 2, nrounds = 30, objective = "binary:logistic")

