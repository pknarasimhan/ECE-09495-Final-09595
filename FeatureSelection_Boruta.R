rm(list=ls())

#Age, BMI, BSA, Height, Weight, Preferred Term, AE related (Y/N) and SAE related (Y/N)
#read data
d1<-read.csv("pretreated.csv", header=TRUE, sep=",", stringsAsFactors = FALSE) 
dim(d1) #1633 x 1315

excl_col<-c("COLITIS","HYPOTHYROIDISM","PNEUMONIA","HEPATITIS")
d1<-d1[,setdiff(colnames(d1),excl_col)]
dim(d1) #1633 x 1311

d2<-na.omit(d1[,c(1:9,1310)])
dim(d2) #1161 x10 
d3<-merge(d2, d1[,10:1311], by="Subject_ID")
dim(d3)  #1161 x 1311
table(d3$immflag) #1014/147

d3[,c(3:6,1311)]<-lapply(d3[,c(3:6,1311)],factor)
d3[,c(11:1310)]<-lapply(d3[,c(11:1310)],function(a){
  ifelse(is.na(a),0,ifelse(a=="GRADE 1",1,ifelse(a=="GRADE 2",2,ifelse(a=="GRADE 3",3,ifelse(a=="GRADE 4",4,5)))))
})

d3<-d3[,apply(d3[,c(11:1310)],2,sum)!=0]  #keep only the AE is recorded
dim(d3) #1161 x 1169
d3<-d3[apply(d3[,c(11:1168)],1,sum)!=0,]
dim(d3) #1153 x 1169
#write.csv(d3, file="pretreated2.csv",row.names = FALSE)
d4<-d3[,-1] #remove ID
colnames(d4)[10:1167]<-paste("A",seq(10:1167),sep="")

##################################################################
#Feature selection with Boruta
library(Boruta)
Boruta.test <- Boruta(immflag ~ ., data = d4, doTrace = 2, ntree = 500, maxRuns=500) #increase maxRun to reduce tentative
Boruta.test
attStats(Boruta.test)[attStats(Boruta.test)$meanZ>0.2,]
TentativeRoughFix(Boruta.test)
plot(Boruta.test)
Boruta.test$finalDecision[Boruta.test$finalDecision=="Confirmed"]