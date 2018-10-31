library(plyr)
library(missForest)  


#### script for looping and imputing to complete samples
#### we can use all samples that are at least 50% present
####

#read full files with traits and taxonomy
setwd("E:/collaborations/tara/IUCNproject/newdatasets/noLivingSp")
AllDataGlobal=read.table(file="AllTaxaGlobalLandPlantsNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataAfr=read.table(file="AllTaxaAfricaLandPlantsNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataEur=read.table(file="AllTaxaEuropeLandPlantsNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataNAm=read.table(file="AllTaxaNAmLandPlantsNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataAsia=read.table(file="AllTaxaAsiaLandPlantsNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataAus=read.table(file="AllTaxaAusLandPlantsNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataCA=read.table(file="AllTaxaCALandPlantsNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataSA=read.table(file="AllTaxaSALandPlantsNew.txt",header=T, sep=";",row.names=NULL,fill=T)


#read files with proportions per Family, so we can select families we can use further
setwd("E:/collaborations/tara/outputs/PropPerFamily/noLivingSp")
GlobalProp=read.table(file="GlobalProportions.txt", sep=";", header=T)
AfricaProp=read.table(file="AfricaProportions.txt", sep=";", header=T)
EuropeProp=read.table(file="EuropeProportions.txt", sep=";", header = T)
NorthAmericaProp=read.table(file="NAmericaProportions.txt", sep=";", header=T)
AsiaProp=read.table(file="AsiaProportions.txt", sep=";", header=T)
AusProp=read.table(file="AusProportions.txt", sep=";", header=T)
CAProp=read.table(file="CAProportions.txt", sep=";", header=T)
SAProp=read.table(file="SAProportions.txt", sep=";", header=T)

#now, loop to select the lines and then traits that we're going to end up using in the RF.

setwd("E:/collaborations/tara/outputs/noLivingSp")

#Global
#traits to keep and impute for global
# Woodiness (1)
#select families with traits present at 50%.
values0.5Global=GlobalProp[(GlobalProp[,1:39]<0.5),]
familiesToImpute=row.names(values0.5Global[1:61,])
ImputeAllFamilies=c()
ImputingGlobal=c()

for(i in 1:length(familiesToImpute)) {   
  ToImpute=AllDataGlobal[AllDataGlobal$family==familiesToImpute[i],c(53,53)]  
  ToImpute=as.matrix(ToImpute[!grepl('NA', rownames(ToImpute)), ])
  ToImpute <- ToImpute[,colSums(is.na(ToImpute))<nrow(ToImpute)]
  ImputingAf=missForest(ToImpute, maxiter = 10, ntree = 100, 
                        variablewise = F, verbose = T, replace = T)
  ImputeAllFamilies=rbind.fill(as.data.frame(ImputeAllFamilies),as.data.frame(ImputingGlobal$ximp))
}  

ImputeGlobal=ImputeAllFamilies  
AllDataGlobal[rownames(ImputeGlobal),c(53,56)]<-ImputeGlobal[,c(1:2)]

DataGlobalImpuComplete=which(complete.cases(AllDataGlobal[,c(53,56)])) 
DataGlobalImpuComplete=AllDataGlobal[DataGlobalImpuComplete,c(2:53,56,59,92,93)]
write.table(DataGlobalImpuComplete, file="ImputedGlobalNew.csv", sep=',')


#Africa
#traits to keep and impute for Africa
# Woodiness (1), Leaf Phenology (7) and Height (4)
#select families with traits present at 50%.
values0.5Af=AfricaProp[(AfricaProp[,1:39]<0.5),]
familiesToImpute=row.names(values0.5Af[c(1:9,12:13,15),])
ImputeAllFamilies=c()
ImputingAf=c()

for(i in 1:length(familiesToImpute)) {   
  ToImpute=AllDataAfr[AllDataAfr$family==familiesToImpute[i],c(53,56,59)]  
  ToImpute=as.matrix(ToImpute[!grepl('NA', rownames(ToImpute)), ])
  ToImpute <- ToImpute[,colSums(is.na(ToImpute))<nrow(ToImpute)]
  ImputingAf=missForest(ToImpute, maxiter = 10, ntree = 100, 
                        variablewise = F, verbose = T, replace = T)
  ImputeAllFamilies=rbind.fill(as.data.frame(ImputeAllFamilies),as.data.frame(ImputingAf$ximp))
}  

ImputeAFrica=ImputeAllFamilies  
AllDataAfr[rownames(ImputeAFrica),c(53,56,59)]<-ImputeAFrica[,c(1:3)]

DataAfricaImpuComplete=which(complete.cases(AllDataAfr[,c(53,56,59)])) 
DataAfricaImpuComplete=AllDataAfr[DataAfricaImpuComplete,c(2:53,56,59,92,93)]
write.table(DataAfricaImpuComplete, file="ImputedAfricaNew.csv", sep=',')

#Europe
#traits to keep and impute for Europe
#Woodiness (1), Leaf Phenology (7) and Height (4), Disp Syndrome (13)
#select families with traits present at 50%.
values0.5Eur=EuropeProp[(EuropeProp[,1:39]<0.5),]
familiesToImpute=row.names(values0.5Eur[c(1:9,11),])
ImputeAllFamilies=c()
ImputingAf=c()

for(i in 1:10) {   
  ToImpute=AllDataEur[AllDataEur$family==familiesToImpute[i],c(53,56,59,65)]
  ToImpute=as.matrix(ToImpute[!grepl('NA', rownames(ToImpute)), ])
  ToImpute <- ToImpute[,colSums(is.na(ToImpute))<nrow(ToImpute)]
  ImputingAf=missForest(ToImpute, maxiter = 10, ntree = 100, 
                        variablewise = F, verbose = T, replace = T)
  ImputeAllFamilies=rbind.fill(as.data.frame(ImputeAllFamilies),as.data.frame(ImputingAf$ximp))
}  

ImputeEurope=ImputeAllFamilies  
AllDataEur[rownames(ImputeEurope),c(53,56,59,65)]<-ImputeEurope[,c(1:4)]


DataEuropeImpuComplete=which(complete.cases(AllDataEur[,c(53,56,59,65)])) 
DataEuropeImpuComplete=AllDataEur[DataEuropeImpuComplete,c(2:53,56,59,65,92:95)]
write.table(DataEuropeImpuComplete, file="ImputedEuropeNew.csv", sep=',')

#North America
#traits to keep and impute for NAmerica
# Woodiness (1), Leaf Phenology (7) and Height (4)

#select families with traits present at 50%.
values0.5NAm=NorthAmericaProp[(NorthAmericaProp[,c(1)]<0.5),]
familiesToImpute=row.names(values0.5NAm[,])
ImputeAllFamilies=c()
ImputingAf=c()

for(i in 1:52) {   
  ToImpute=AllDataNAm[AllDataNAm$family==familiesToImpute[i],c(53)] 
  ToImpute=as.matrix(ToImpute[!grepl('NA', rownames(ToImpute)), ])
  ToImpute <- ToImpute[,colSums(is.na(ToImpute))<nrow(ToImpute)]
  #ToImpute=as.data.frame(ToImpute)
  #ToImpute[,c(1,3)]<-as.factor((unlist(ToImpute[,c(1,3)])))
  #ToImpute=as.data.frame(unclass(ToImpute))
  ImputingAf=missForest(ToImpute, maxiter = 10, ntree = 100, sampsize = 10,
                        variablewise = F, verbose = T, replace = T)
  ImputeAllFamilies=rbind.fill(as.data.frame(ImputeAllFamilies),as.data.frame(ImputingAf$ximp))
}  

ImputeNAm=ImputeAllFamilies  
AllDataNAm[rownames(ImputeNAm),c(53,56,59)]<-ImputeNAm[,c(1:3)]

DataNAmImpuComplete=which(complete.cases(AllDataNAm[,c(53,56,59)])) 
DataNAmImpuComplete=AllDataNAm[DataNAmImpuComplete,c(2:53,56,59,92:95)]
write.table(DataNAmImpuComplete, file="ImputedNAmericaNew.csv", sep=',')

