#t-test DAta
x<-"D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/Tselected_training_merged_file.csv"
y<-"D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/Tselected_validation_merged_file.csv"


#RF data
#x<-"D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/selected_training_merged_file.csv"
#y<-"D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/selected_validation_merged_file.csv."
#selected_training_merged_file.csv and selected_validation_merged_file.csv. 


#predict_results<- function(x,y) {


#library(ROSE) 
library(e1071)
library(caret)
library(ROCR)
#library(pROC)
#library(klaR)
library(mltools)



file_n<-x
#file_n1<-x1
file_other<-y

data1 <- read.csv(file_n, header = TRUE)
data1$Output
#data12 <- read.csv(file_n1, header = TRUE)
#data1<-rbind(data1,data12)
cols<-ncol(data1)

data_backup<-data1

nrows_training<-nrow(data1)

data_other <- read.csv(file_other, header = TRUE)
data_other_backup<-data_other
data_other2<-data_other
cols2<-ncol(data_other)


which(apply(data_other, 2, var) == 0)

#data_other<-data_other[-c(cols2)]
DF2<-data_other
data_other_backup<-data_other
nrows_testing<-nrow(data_other)
nrows_testing

#print(nrows_training)
data1<-data1[-c(cols)]
DF1<-data1
#summary(data)




#if(nrows_testing<100){
#  data_other<-rbind(DF2,DF1)
#}

library(caret)
library(randomForest)
data1$dist_100

#data_norm<-as.data.frame(scale(data1))
data_norm<-preProcess(data1,method=c("center", "scale"))
#data_norm$mean
data1<-predict(data_norm, data1)
data1$dist_100
data1["Output"]<-data_backup[,cols]
#data1$Output <- as.factor(data1$Output)
data_other<-predict(data_norm, data_other)
data_other$dist_100


# if(nrows_testing<100){
#   data_other <- data_other[-((nrows_testing+1):nrows_testing_scale), ]
# }
# nrow(data_other)




train<-data1
test<-data_other

suraiya<-123
set.seed(suraiya)
train$Output
#train$Output[train$Output==1]<-"Yes"
train$Output<-as.factor(train$Output)
#tmodel2<-tune(svm, Output~., data = train, ranges = list(epsilon =seq(0,1,0.1), cost=2^(2:7)))
#tmodel2$best.parameters
tmodel2<- tune(randomForest, Output ~ ., data = train, ranges= list(ntree = c(500,400), mtry = c(6,5)))
tmodel2$best.parameters
mymodel2<-tmodel2$best.model
mymodel2


#scale(test$pseudo_10)
#setdiff(colnames(train),colnames(test))
#results<-predict(tmodel2, test)
#ind <- colSums(is.na(test)) == nrow(test)
#ind
#pos<-which(ind==TRUE)
#test[,pos]<-data_other_backup[,pos]
#names(test)[ind]

results<-predict(mymodel2, test)
test$Output<-as.factor(test$Output)
predict(mymodel2,test)
mcc(test$Output, results)
round(mcc(test$Output, results),2)
results
library(plyr)
data_other_backup$Output<-as.factor(data_other_backup$Output)

results<- revalue(results, c("1"="Yes"))
results<- revalue(results, c("-1"="No"))
sum(results=="Yes")

confusionMatrix(predict(mymodel2,test), test$Output, positive = '1')
y_pred = predict(mymodel2, newdata = test)
cm = table(data_other_backup$Output, y_pred)
cm[1,1] #TN
cm[2,1]
cm[2,2] #TP


