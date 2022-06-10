#file <-read.csv("D:\\Bacteriocin\\data\\featureExtraction\\CorrFeatureSet.csv", header = TRUE, sep = ",")
file <-read.csv("D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\TCorrFeatureSet.csv", header = TRUE, sep = ",")

#file<-file[,-1]
col <- ncol(file)
col
#colnames(file[1])
cl<-col-1
cl
#Rlist = list()
Rlist <- c()
selectedfeature <- c()
selectedfeature
pval<-c()
pval
#for(i in 3:col-1)
#options("scipen"=100, "digits"=4)

for(i in 1:cl){
  
  shp<-shapiro.test(file[,i])$p.value
  print(shp)
}

for(i in 1:10){
  
  qqnorm(file[,i]);qqline(file[,i], col = 2)
}


hist(file[,2],probability=T, main="Histogram of normal data",xlab="Approximately normally distributed data")
lines(density(file[,2]),col=2)



for(i in 1:cl)
{
  
x<- file[,i]
x
y<- file[[col]]
y
ttest = t.test(x,y)$p.value
print (ttest)

if(ttest > 0.05)
{  
  Rlist<-c(Rlist,i)
#  Rlist[index]=j
#index=index+1
}
else{
  selectedfeature <- c(selectedfeature,colnames(file[i]))
  pval <- c(pval, t.test(x,y)$p.value)
}

}
length(Rlist)
length(selectedfeature)
selectedfeature
pval

dframe<-data.frame(selectedfeature, pval)
#head(dframe)
dframesorted<-dframe[order(dframe$pval),]


##==========Remove the list from original file ============================

corrFST<- file
corrFST<- corrFST[,-Rlist]
corrFST
length(corrFST)
#corrFST<- corrFST[,-c(ncolumn)]
corrFST$Output[corrFST$Output==1]<-"yes"
corrFST$Output[corrFST$Output== -1]<-"no"
#corrFST$Output

write.csv(corrFST, file = "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\TtestFeatureSet.csv", row.names = FALSE)

write.csv(dframesorted, file = "D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\TRankedFeatures.csv", row.names = FALSE)

dframesorted<-read.csv("D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\TRankedFeatures.csv", header = TRUE)



pval<-dframesorted$pval
length(pval)


file_read <-read.csv("D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\TtestFeatureSet.csv", header = TRUE, sep = ",")
length(file_read)
file_read$Output


lpvalue<- -log10(pval)
xvalue<-c(1:length(pval))

tiff("D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\figure\\fig1_a_pdf_A4.tiff",width= 17, height=12, units="cm", res=300)
plot(xvalue,pval,xlab="Number of selected features", ylab = "p-value")
dev.off()

tiff("D:\\Research Work\\Disertation Project 1\\Raw Data\\featureExtraction\\figure\\fig_1b_pdf_A4.tiff",width= 17, height=12, units="cm", res=300)
plot(xvalue,lpvalue,xlab="Number of selected features", ylab = "-log10(p-value)")
dev.off()

