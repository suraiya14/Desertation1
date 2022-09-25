library(caret)
library(doMC)
#detectCores()
registerDoMC(cores = 10)

data(mdrr)
mdrrDescr

#MDGI data
#file_n<-"D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\selected_training_merged_file.csv"
#t-tset data
file_n<-"D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\Tselected_training_merged_file.csv"
xtdm <- read.csv(file_n, header = TRUE)
#ncol(xtdm)



#summary(xtdm$Output)
col_names = names(xtdm)
col_names
target = "Output"
xtdm$Output[xtdm$Output==1] <- "yes"
xtdm$Output[xtdm$Output==-1] <- "no"
xtdm$Output<-as.factor(xtdm$Output)
feature_names = col_names[col_names!=target]
feature_names


caretFuncs$summary <- twoClassSummary

set.seed(123)
ctrl <- rfeControl(functions=caretFuncs, 
                  method = "cv",
                  number=10,
                  repeats=5)
    


trainctrl <- trainControl(classProbs= TRUE,
                        summaryFunction = twoClassSummary)

# get results
rfe_fit = rfe(xtdm[,feature_names], xtdm[,target],
             sizes = c(1:140),#t-test=c(1:140), RF= c(1:44)
             rfeControl = ctrl,
             method="svmRadial",
             metric = "ROC",
              ## additional arguments to train method here
             trControl=trainctrl
)


# summarize the results
print(rfe_fit)
rfe_fit$results
rfe_fit$bestSubset
#df<-as.data.frame(rfe_fit$variables,rfe_fit$)
rfe_fit$optVariables
file<-"D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\RFE\\Result\\summary_T-TEST_SVM.csv"
write.csv(rfe_fit$results,file, row.names = FALSE)

file<-"D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\RFE\\Result\\optva_T-TEST_SVM.csv"
write.csv(rfe_fit$optVariables,file, row.names = FALSE)


rfe_fit$bestSubset
rfe_fit$optVariables
#vr<-rfe_fit$optvariables
feat<-rfe_fit$optVariables
feat

df<- data.frame(feat)

length(df)
df
df[nrow(df) + 1,] <- c("Output")
#add row
#final_df[nrow(final_df) + 1,] <- c("Output")
#print
print(df)

write.csv(df, "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\RFE/T-TEST/T-TEST_RF.csv", row.names = FALSE)

file1<-read.csv("D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/RFE/T-TEST/T-TEST_RF.csv", header = TRUE)
file1

final_df <- as.data.frame(t(file1))
final_df<-data.frame(final_df)

final_df
write.csv(final_df, "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\RFE/T-TEST/T-TEST_RF_NW.csv", row.names = FALSE)

res<-rfe_fit$results
res
#write.csv(res, "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\RFE/MDGI/results_svm_mdgi.csv", row.names = FALSE)
result<-res$ROC
result
varbles<-res$Variables

# list the chosen features
predictors(rfe_fit)
# plot the results
plot(rfe_fit, type=c("p", "l"))
#plot(varbles,result, xlab="Number of features", ylab="kk", type=c("l"))
plot(varbles,result, xlab="Number of features", ylab="AUC values", type=c("p"), col="black",pch = 16, cex = .8)
lines(varbles,result, col="red")




