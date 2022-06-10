
## =============read from the original feature set=============================##

file <-read.csv("D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\dataSplit/validate_train_data.csv", header = TRUE)
file

col <- ncol(file) 
col
file_backup<-file
length(file_backup$Output)
c(col)
file<-file[-c(col)]
file
file<-as.data.frame(scale(file))
length(file)

#file<-as.numeric(file$Output)
#normalize <- function(x) {
# return ((x - min(x)) / (max(x) - min(x)))
#}

#file <- as.data.frame(lapply(file, normalize))

col_backup<-col
#start_CN<-which(colnames(file)=="binaryC_1")
col_psc<-col-1
col_psc
# Put a flag with size of the input (col: number of columns)

flag = c(1:col)
flag
flag[1:col]=0
flag
#Rlist =list()
Rlist = c()
#index=1

Rlist

#============== Create a list of columns that we want to remove ==================
for(i in 1:col_psc)
{
  for(j in 1:col_psc)
  {
    
    if(flag[i]==0)
    {
      col2= file[[i]]
      col1 = file[[j]]
      
      corre<-abs (cor(col1,col2, method = "pearson"))
      
      res <- c(corre, i, j)
      if(corre >= 0.9 && i!=j)
      {
        flag[j]=1
        Rlist<-c(Rlist,j)
        Rlist
        #Rlist[index]=j
        #index=index+1
        # write(j, file = "E:\\Abu\\SanaAbu\\ACM-BCB\\1SetFeatureSet\\Cstcor95.txt", ncolumns =1, append = TRUE, sep = ",")
      }
      
    }
  }
  
  print(i)
}

length(Rlist)
##==================Remove the duplicates in our list====================================






##==========Remove the list from original file ============================



corrFST= file
if(length(Rlist)>0){
  Rlist <- unique(Rlist)
  
  Rlist<-sort(Rlist)
corrFST= corrFST[,-Rlist]
}
length(corrFST)
#row = length(Rlist)
#corrFST= file
#count=0
#x=0
#for(i in 1:row)
#{
#  x=as.numeric(Rlist[i])-count
#  corrFST= corrFST[-x]
#  count=count+1
#}




#write.csv(corrFST, file = "C:\\Soil ML works\\Merged_training_features\\CorrFeatureSet.csv", row.names = FALSE)

corrFST$Output<-file_backup$Output
ncol(corrFST)


#corrFST$aac_1








write.csv(corrFST, file = "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/RCorrFeatureSet.csv", row.names = FALSE)




corrFSTnum<-read.csv(file = "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/RCorrFeatureSet.csv", header = TRUE, sep = ",")
colnames(corrFSTnum)
ncol(corrFSTnum)
#ncol(corrFST$bi)
corrFSTnum$Output

len_num_after<-length(corrFSTnum)-1
#len_num_after
#linearMod <- bayesglm(corrFSTnum$Output ~., corrFSTnum,family = "binomial",maxit = 100)
#linearMod <- glm(corrFSTnum$Output ~., corrFSTnum,family = "binomial",maxit = 100)
#s<-summary(linearMod)
#s
#s$coefficients[,4]
#length(s$coefficients[,4])
#s$coefficients[len_num_after+1,4]
#Rlist<-c()
#for(i in 1:len_num_after){
#  if(round(s$coefficients[i+1,4],2)>0.05){
#    Rlist<-c(Rlist,i)
#  }
#}

#Rlist
#if(length(Rlist)>0){
#  corrFSTnum= corrFSTnum[,-Rlist]
#}

#write.csv(corrFSTnum, file = "C:\\Soil ML works\\Merged_training_features\\FinalFeatureSet.csv", row.names = FALSE)

corrFSTnum$Output<-as.factor(corrFSTnum$Output)

library(randomForest)
library(caret)

set.seed(123)
fit_rf = randomForest(corrFSTnum$Output~., data=corrFSTnum)
vi<-floor(importance(fit_rf))
#vi changed the floor to ceiling
#vi<-ceiling(importance(fit_rf))
length(vi)
vi
#varImp(fit_rf)
plot(vi)
#vi
Rlist<-c()
for(k in 1:len_num_after){
  if(vi[k]==0){
    Rlist<-c(Rlist,k)
    Rlist
  }
}

length(Rlist)

if(length(Rlist)>0){
  corrFSTnum= corrFSTnum[,-Rlist]
  length(Rlist)
  vi<-vi[-Rlist]
}
ncol(corrFSTnum)
lngt<-ncol(corrFSTnum)
corrFSTnum_vi<-corrFSTnum[,-lngt]
vi_col<-colnames(corrFSTnum_vi)
vi[1]
length(vi)
length(corrFSTnum)
dtfr<-data.frame(vi_col,vi)
write.csv(corrFSTnum, file = "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/FinalFeatureSet.csv", row.names = FALSE)
write.csv(dtfr, file = "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/MDGI_FinalFeatureSet.csv", row.names = FALSE)
write.csv(vi, file = "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction/MDGI_val_FinalFeatureSet.csv", row.names = FALSE)

file_read <-read.csv("D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\FinalFeatureSet.csv", header = TRUE, sep = ",")
length(file_read)
file_read$Output

