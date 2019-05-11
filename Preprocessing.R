rm(list=ls())

#Age, BMI, BSA, Height, Weight, Preferred Term, AE related (Y/N) and SAE related (Y/N)

library(haven)
d1 <- read_sas("imfinal.sas7bdat")
str(d1)
colnames(d1)
dim(d1) #14918 x 16
dim(unique(d1))  #13470x16  more than 1400 records are duplicated!
d1<-unique(d1)

#remove subject ID which is obviously assigning to the dfferent persoan and same ID!
#remove unnecessary columns incl immflag, reestablish later with the four AEs
d1<-d1[,-c(12,13:16)]
dim(d1)
d1<-unique(d1)
dim(d1)  #13097 x 11

str(d1)

#Change character to factor
d1[,which(sapply(d1, class)=="character")]<-lapply(d1[,which(sapply(d1, class)=="character")],as.factor)
str(d1)

#Unpivot to have each patient per row
library(tidyr)  #Used tidyr_0.8.1 before.  

#Current env used 0.3.1 version and spread function seems not working here!!

library(dplyr)

#Add row id to avoid duplicated cell.
d2<-d1 %>% mutate(row_id=1:n()) %>% spread(PT_NAME, AE_Intensity_L) %>% select(-row_id)  #This will stop collapsing the row due to the introduction of id!

#Create a unique ID by a group of biometric variables
comb <- with(d2, paste(Age_DRV,Country_DRV, baseline_weight, baseline_height))
d2 <- within(d2, Subject_ID <- match(comb, unique(comb)))

per_patient<-NULL
for (z in 1:length(unique(d2$Subject_ID))) {
  tmp<-subset(d2, Subject_ID %in% unique(d2$Subject_ID)[z])
  tmp<-as.data.frame(tmp)
  #Check each column's NA
  #Getting NA count for each column (each covariate)
  na_count2 <- data.frame(count = apply(tmp, 2, function(x) sum(is.na(x))))
  na_count2$na_percent <- (na_count2$count / nrow(tmp))*100
  non_NA<-rownames(na_count2[na_count2$count!=nrow(tmp),]) #find any column has non-all NA
  for (i in 1:length(non_NA)){
    for (j in 1:nrow(tmp)){
      if (is.na(tmp[j,non_NA[i]])) {
        tmp[j,non_NA[i]]<-dplyr::first(na.omit(tmp[,non_NA[i]]))
      } else if (tmp[j,non_NA[i]]!=dplyr::first(na.omit(tmp[,non_NA[i]]))){
        tmp[j,non_NA[i]]<-dplyr::first(na.omit(tmp[,non_NA[i]]))
      }
    }
  }
  tmp2<-unique(tmp)
  per_patient<-rbind(per_patient,tmp2)
}
dim(per_patient) #1633 x 1315 
per_patient<-per_patient[,-10] #remove V1

#AE im-related
col_names1<-c("COLITIS","HYPOTHYROIDISM","PNEUMONIA","HEPATITIS")
per_patient[,col_names1]<-lapply(per_patient[,col_names1],function(a) {ifelse(is.na(a),0,1)})
per_patient$immflag<-apply(per_patient[,col_names1],1,sum)
per_patient$immflag<-ifelse(per_patient$immflag==0,"no","yes")
table(per_patient$immflag) #1446/187  This is highly imbalanced
table(per_patient$COLITIS) #1608/25
table(per_patient$HYPOTHYROIDISM) #1516/117
table(per_patient$PNEUMONIA) #1586/47
table(per_patient$HEPATITIS) #1627/6

write.csv(per_patient,file="pretreated_test.csv",row.names = FALSE)
