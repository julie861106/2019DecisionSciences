##Supreme Court Cases
#大法官決策預測
# Read in the data
Stevens = read.csv("/Users/julieyao/Documents/nccu/2019fall/DecisionSciences/R Codes/Stevens.csv")
str(Stevens)

# Split the data
library(caTools)
library(rpart)
library(rpart.plot)
library(partykit)
set.seed(9527)
spl = sample.split(Stevens$Reverse, SplitRatio = 0.7)
Train = subset(Stevens, spl==TRUE)
Test = subset(Stevens, spl==FALSE)


##Classification trees
Train.df=Train
Train.df$Reverse=ifelse(Train.df$Reverse==1,"yes","no")
Test.df=Test
Test.df$Reverse=ifelse(Test.df$Reverse==1,"yes","no")

# Build a decision tree model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + 
                      Respondent + LowerCourt + Unconst, 
                    data = Train.df, method="class", minbucket=15,
                    parms = list(split="information"))

plot(as.party(StevensTree),type="simple")
plot(as.party(StevensTree),type="extended")

#隨機森林
library(randomForest)
# Build a random forest model
StevensForest = randomForest(as.factor(Reverse) ~ Circuit + Issue + 
                               Petitioner + Respondent + LowerCourt + 
                               Unconst, data = Train, ntree=200, 
                             nodesize=25)




StevensForest = randomForest(as.factor(Reverse) ~ Circuit + Issue + 
                               Petitioner + Respondent + LowerCourt + Unconst,
                             data = Train, ntree=500, nodesize=25,
                             importance=TRUE)
varImpPlot(StevensForest)



##Logistic regression
StevensLogit=glm(Reverse ~ Circuit + Issue + Petitioner + 
                   Respondent + LowerCourt + Unconst,
                 data=Train, family=binomial(link="logit"))
summary(StevensLogit)

# ROC curve
library(ROCR)
PredictLogit=predict(StevensLogit,newdata=Test, type="response")
round(PredictLogit,3)
#閥值/決策臨界值
t=0.5
yhat=ifelse(PredictLogit>=t,1,0)
actual=Test$Reverse
table(actual,yhat)
predLogit = prediction(PredictLogit, Test$Reverse)
#
x11(width=8,height=5)
plot(performance(predLogit, "tpr","fpr"),col='blue',lty=3,lwd=3)
abline(0,1)
performance(predLogit, "auc")

ROCRperf=performance(predLogit, "tpr","fpr")

x11(width=8,height=5)
#threshold on the right
#Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))


# ROC curve
library(ROCR)
PredictLogit=predict(StevensLogit,newdata=Test, type="response")
PredictTree = predict(StevensTree, newdata = Test.df, type = "prob")
PredictForest = predict(StevensForest, newdata = Test, type = "prob")
predLogit = prediction(PredictLogit, Test$Reverse)
predTree = prediction(PredictTree[,2], Test$Reverse)
predForest = prediction(PredictForest[,2], Test$Reverse)
#
x11(width=8,height=5)
plot(performance(predLogit, "tpr","fpr"),col='blue',lty=3,lwd=3)
plot(performance(predTree, "tpr", "fpr"),col='green',add=T,lty=4,lwd=3)
plot(performance(predForest, "tpr", "fpr"),col='red',add=T,lty=3,lwd=3)
abline(0,1,lty=2)

performance(predLogit, "auc")
performance(predTree, "auc")
performance(predForest, "auc")
