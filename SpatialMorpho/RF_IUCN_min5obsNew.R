####read in data and merge what's needed

library(plyr)

##complete cases of spatial and morpho traits
setwd("E:/collaborations/tara/IUCNproject/newdatasets")
GlobalNoNA=read.table(file="AllTaxaGlobalLandPlantsNew.txt", sep=';', header=T)
GlobalNoNANoRL=na.omit(GlobalNoNA[,c(1:48,50,52,55,58)]) #select Leaf Phenology, Plant Height  and Woodiness
GlobalNoNARL=join(GlobalNoNANoRL,GlobalNoNA[,c(1,51)],by="Species", type="left")
write.table(GlobalNoNARL, file="LandPlantsGlobalNoNANew.txt", sep=";")

##complete cases of spatial and morpho traits
setwd("E:/collaborations/tara/IUCNproject/newdatasets")
NAmericaNoNA=read.table(file="AllTaxaNAmLandPlantsNew.txt", sep=';', header=T)
NAmericaNoNANoRL=na.omit(NAmericaNoNA[,c(1:48,50,52,55,58)]) #select Leaf Phenology, Plant Height  and Woodiness
NAmericaNoNARL=join(NAmericaNoNANoRL,NAmericaNoNA[,c(1,51)],by="Species", type="left")
#write.table(NAmericaNoNARL, file="LandPlantsNAmNoNANew.txt", sep=";")

EuropeNoNA=read.table(file="AllTaxaEuropeLandPlantsNew.txt", sep=';', header=T)
EuropeNoNANoRL=na.omit(EuropeNoNA[,c(1:48,50,52,55,58)]) #select Leaf Phenology, Plant Height and Woodiness
EuropeNoNARL=join(EuropeNoNANoRL,EuropeNoNA[,c(1,51)],by="Species", type="left")
#write.table(EuropeNoNARL, file="LandPlantsEuropeNoNANew.txt", sep=";")

AfricaNoNA=read.table(file="AllTaxaAfricaLandPlantsNew.txt", sep=';', header=T)
AfricaNoNANoRL=na.omit(AfricaNoNA[,c(1:48,50,52,58)]) #select Leaf Phenology, and Woodiness
AfricaNoNARL=join(AfricaNoNANoRL,AfricaNoNA[,c(1,51)],by="Species", type="left")
#write.table(AfricaNoNARL, file="LandPlantsAfricaNoNANew.txt", sep=";")

AsiaNoNA=read.table(file="AllTaxaAsiaLandPlantsNew.txt", sep=';', header=T)
AsiaNoNANoRL=na.omit(AsiaNoNA[,c(1:48,50,52,58)]) #select Leaf Phenology and Woodiness
AsiaNoNARL=join(AsiaNoNANoRL,AsiaNoNA[,c(1,51)],by="name", type="left")
write.table(AsiaNoNARL, file="LandPlantsAsiaNoNANew.txt", sep=";")

AusNoNA=read.table(file="AllTaxaAusLandPlantsNew.txt", sep=';', header=T)
AusNoNANoRL=na.omit(AusNoNA[,c(1:48,50,52)]) #select Woodiness
AusNoNARL=join(AusNoNANoRL,AusNoNA[,c(1,51)],by="name", type="left")
#write.table(AusNoNARL, file="LandPlantsAusNoNANew.txt", sep=";")

CANoNA=read.table(file="AllTaxaCALandPlantsNew.txt", sep=';', header=T)
CANoNANoRL=na.omit(CANoNA[,c(1:48,50,52,58)]) # select WoodinessTRYCA + LeafPhenoTRYCA
CANoNARL=join(CANoNANoRL,CANoNA[,c(1,51)],by="name", type="left")
write.table(CANoNARL, file="LandPlantsCANoNANew.txt", sep=";")

SANoNA=read.table(file="AllTaxaSALandPlantsNew.txt", sep=';', header=T)
SANoNANoRL=na.omit(SANoNA[,c(1:50,52)]) # select WoodinessTRYSA
SANoNARL=join(SANoNANoRL,SANoNA[,c(1,51)],by="name", type="left")
write.table(SANoNARL, file="LandPlantsSANoNANew.txt", sep=";")

#colheaders=as.matrix(read.table("column_headers.txt", sep="\t"))
#colnames(SpatialDataEurope)<-colheaders[1,]


##########################################################################
#run RF for all types of datasets and combinations
##########################################################################

library(randomForest)

######## GLOBAL ####################

#####read Imputed data
#setwd("E:/collaborations/tara/outputs")
#AfricaImpNoNA=read.table(file="ImputedAfricaNew.csv", sep=',', header=T)
#AfricaImpNoNANoRL=na.omit(AfricaImpNoNA[,c(3:48,52:54)]) #select Leaf Phenology, Plant Height + and Woodiness
#AfricaImpNoNARL=join(AfricaImpNoNANoRL,AfricaImpNoNA[,c(1,51)],by="Species", type="left")
#write.table(AfricaImpNoNARL, file="LandPlantsAfricaNoNANew.txt", sep=";")


#select the species that have at least 5 observations
GlobalNoNARL4=GlobalNoNARL[GlobalNoNARL$n.gps>=5,]
#AfricaNoNARL4=AfricaNoNARL4[AfricaNoNARL4$dist=='endemic',]
GlobalMorphoSpatial4=GlobalNoNARL4


setwd("E:/collaborations/tara/outputs/RFResults/Global")

#####################################################
#### USE IMPUTED DATASETS ###########################
####################################################



#adjust the RF by downsampling Majority class
#create a new column with new version of IUCN category
AfricaImputedSpatialToUse=AfricaMorphoSpatial4[,]
#AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

#AfricaImputedSpatialToUse=AfricaImputedSpatialToUse[!(AfricaImputedSpatialToUse$Red.List.status== 'EW'),]
#AfricaImputedSpatialToUse=AfricaImputedSpatialToUse[!(AfricaImputedSpatialToUse$Red.List.status== 'EX'),]

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

memory.limit(size=50000)

### this does the downsampling to a value of 42
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=42, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYAfr + LeafPhenoTRYAfr,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="DownsampleImputed42NoLCvLCNew.csv", sep=",", append=T)
  
  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesNew.csv", sep=",", append=T)
  
  
  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="DownsampleImputed42NoLCvLCErrorRatesNew.csv", sep=",", append=T)
  
  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataDownsampleImputed42NoLCvLCNew.csv", sep=",", append=T)
  
  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableDownsampleImputed42NoLCvLCNew.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$Red.List.status),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableDownsampleImputed42NoLCvLCNew.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
##dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=30, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=30, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYAfr + LeafPhenoTRYAfr,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="ResampleImputed30NoLCvLC.csv", sep=",", append=T)
  
  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="ResampleImputed30NoLCvLCErrorRates.csv", sep=",", append=T)
  
  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataResampleImputed30NoLCvLC.csv", sep=",", append=T)
  
  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,37)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBSpecies30.csv", sep=",", append=T)
  
  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableResampleImputed30NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$Red.list.status),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleImputed30NoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE COMPLETE DATASETS #########################
####################################################


#adjust the RF by downsampling Majority class
setwd("E:/collaborations/tara/outputs/RFResults/Global")
#create a new column with new version of IUCN category
AfricaMorphoSpatial4=GlobalNoNARL[GlobalNoNARL$n.gps>=5,]
AfricaImputedSpatialToUse=AfricaMorphoSpatial4[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

memory.limit(size=50000)

### this does the downsampling to a value of 20
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=20, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYGlobal + LeafPhenoTRYGlobal + PlantHeightTRYGlobal,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="FullGlobalNoLCvLC20.csv", sep=",", append=T)

  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,54)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesGlobalFull20New.csv", sep=",", append=T)

  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="FullGlobalNoLCvLCErrorRates20.csv", sep=",", append=T)

  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.AfrdataGlobalNoLCvLC20.csv", sep=",", append=T)

  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableFullNoLCvLCGlobal20.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCGlobal20.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling 40 of each

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=40, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=40, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYGlobal + LeafPhenoTRYGlobal + PlantHeightTRYGlobal,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="ResampleGlobalComplete40NoLCvLC.csv", sep=",", append=T)

  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="ResampleGlobalComplete40NoLCvLCErrorRates.csv", sep=",", append=T)

  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,54)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionGlobalResampleOfOOBSpecies40.csv", sep=",", append=T)


   PredictionPNW <- predict(fitRFPNW)
   binded.dataRFPNW=c()
   binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
   write.table(binded.dataRFPNW,file="binded.dataGlobalResampleComplete40NoLCvLC.csv", sep=",", append=T)

   ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableGlobalResampleComplete40NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleCompleteGlobal40NoLCvLC.csv", sep=',', append=T)
  
}





######## AFRICA ####################

#####read Imputed data
#setwd("E:/collaborations/tara/outputs")
#AfricaImpNoNA=read.table(file="ImputedAfricaNew.csv", sep=',', header=T)
#AfricaImpNoNANoRL=na.omit(AfricaImpNoNA[,c(3:48,52:54)]) #select Leaf Phenology, Plant Height + and Woodiness
#AfricaImpNoNARL=join(AfricaImpNoNANoRL,AfricaImpNoNA[,c(1,51)],by="Species", type="left")
#write.table(AfricaImpNoNARL, file="LandPlantsAfricaNoNANew.txt", sep=";")


#select the species that have at least 4 observations
AfricaNoNARL4=AfricaNoNARL[AfricaNoNARL$n.gps>=5,]
#AfricaNoNARL4=AfricaNoNARL4[AfricaNoNARL4$dist=='endemic',]
AfricaMorphoSpatial4=AfricaNoNARL4


setwd("E:/collaborations/tara/outputs/RFResults/Africa")

#####################################################
#### USE IMPUTED DATASETS ###########################
####################################################



#adjust the RF by downsampling Majority class
#create a new column with new version of IUCN category
AfricaImputedSpatialToUse=AfricaMorphoSpatial4[,]
#AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

#AfricaImputedSpatialToUse=AfricaImputedSpatialToUse[!(AfricaImputedSpatialToUse$Red.List.status== 'EW'),]
#AfricaImputedSpatialToUse=AfricaImputedSpatialToUse[!(AfricaImputedSpatialToUse$Red.List.status== 'EX'),]

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

memory.limit(size=50000)

### this does the downsampling to a value of 42
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=42, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYAfr + LeafPhenoTRYAfr,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="DownsampleImputed42NoLCvLCNew.csv", sep=",", append=T)
  
  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesNew.csv", sep=",", append=T)
  
  
  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="DownsampleImputed42NoLCvLCErrorRatesNew.csv", sep=",", append=T)
  
  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataDownsampleImputed42NoLCvLCNew.csv", sep=",", append=T)
  
  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableDownsampleImputed42NoLCvLCNew.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$Red.List.status),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableDownsampleImputed42NoLCvLCNew.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
##dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=30, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=30, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYAfr + LeafPhenoTRYAfr,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="ResampleImputed30NoLCvLC.csv", sep=",", append=T)
  
  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="ResampleImputed30NoLCvLCErrorRates.csv", sep=",", append=T)
  
  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataResampleImputed30NoLCvLC.csv", sep=",", append=T)
  
  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,37)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBSpecies30.csv", sep=",", append=T)
  
  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableResampleImputed30NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$Red.list.status),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleImputed30NoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE COMPLETE DATASETS #########################
####################################################


#adjust the RF by downsampling Majority class
setwd("E:/collaborations/tara/outputs/RFResults/Africa")
#create a new column with new version of IUCN category
AfricaMorphoSpatial4[,]=AfricaNoNARL[AfricaNoNARL$n.gps>=5,]
AfricaImputedSpatialToUse=AfricaMorphoSpatial4[,]
#AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

memory.limit(size=50000)

### this does the downsampling to a value of 42
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
    C=PNWRF[PNWRF$NewIUCN=="LC",]
    Csel=C[(sample(nrow(C), size=42, replace=T)),]
    NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
    PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYAfr + LeafPhenoTRYAfr,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="FullAfrNoLCvLC42.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesAfrFull42New.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="FullAfrNoLCvLCErrorRates42.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.AfrdataFullNoLCvLC42.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableFullNoLCvLCAfr42.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCAfr42.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling 84 of each

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=84, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=84, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYAfr + LeafPhenoTRYAfr,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleAfrComplete84NoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleAfrComplete84NoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionAfrResampleOfOOBSpecies84.csv", sep=",", append=T)
  # 
  
#  PredictionPNW <- predict(fitRFPNW)
#  binded.dataRFPNW=c()
#  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
#  write.table(binded.dataRFPNW,file="binded.dataAfrResampleComplete84NoLCvLC.csv", sep=",", append=T)
  
#  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
# diag(prop.table(ctRFPNW, 1))
# write.table(ctRFPNW, file="confusionTableAfrResampleComplete84NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleCompleteAfr84NoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE ENDEMICS ONLY #############################
####################################################

AfricaNoNARL4=AfricaNoNARL[AfricaNoNARL$n.gps>=5,]
AfricaNoNARL4=AfricaNoNARL4[AfricaNoNARL4$dist=='endemic',]
AfricaMorphoSpatial4=AfricaNoNARL4

#create a new column with new version of IUCN category
AfricaImputedSpatialToUse=AfricaMorphoSpatial4[,]
#AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

#AfricaImputedSpatialToUse=AfricaImputedSpatialToUse[!(AfricaImputedSpatialToUse$Red.List.status== 'EW'),]
#AfricaImputedSpatialToUse=AfricaImputedSpatialToUse[!(AfricaImputedSpatialToUse$Red.List.status== 'EX'),]

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

memory.limit(size=50000)

#adjust the RF by downsampling Majority class
setwd("E:/collaborations/tara/outputs/RFResults/Africa")

### this does the downsampling to a value of 15
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=15, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYAfr + LeafPhenoTRYAfr,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
#  Contribution=fitRFPNW$importance
#  write.table(Contribution,file="FullAfrNoLCvLC15Endem.csv", sep=",", append=T)
  
#  SpeciesPredicted = fitRFPNW$votes
#  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
#  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
#  write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesAfrFull15EndemNew.csv", sep=",", append=T)
  
#  errorRates=fitRFPNW$err.rate
#  write.table(errorRates, file="FullAfrNoLCvLCErrorRates28Endem.csv", sep=",", append=T)
  
#  PredictionPNW <- predict(fitRFPNW)
#  binded.dataRFPNW=c()
#  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
#  write.table(binded.dataRFPNW,file="binded.AfrdataFullNoLCvLC15Endem.csv", sep=",", append=T)
  
#  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
#  diag(prop.table(ctRFPNW, 1))
#  write.table(ctRFPNW, file="confusionTableFullNoLCvLCAfr15Endem.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCAfr15Endem.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling 28 of each

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=28, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=28, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd + WoodinessTRYAfr + LeafPhenoTRYAfr,
                           data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
#  Contribution=fitRFPNW$importance
#  write.table(Contribution,file="ResampleAfrComplete28EndemNoLCvLC.csv", sep=",", append=T)
  
#  errorRates=fitRFPNW$err.rate
#  write.table(errorRates, file="ResampleAfrComplete28EndemNoLCvLCErrorRates.csv", sep=",", append=T)
  
#  SpeciesPredicted = fitRFPNW$votes
#  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
#  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
#  write.table(SpeciesPredicted, file="PredictionAfrResampleOfOOBSpecies28Endem.csv", sep=",", append=T)
  
  
#  PredictionPNW <- predict(fitRFPNW)
#  binded.dataRFPNW=c()
#  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
#  write.table(binded.dataRFPNW,file="binded.dataAfrResampleComplete28EndemNoLCvLC.csv", sep=",", append=T)
  
#  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
#  diag(prop.table(ctRFPNW, 1))
#  write.table(ctRFPNW, file="confusionTableAfrResampleComplete28EndemNoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleCompleteAfr28EndemNoLCvLC.csv", sep=',', append=T)
  
}




######## NORTH AMERICA ####################


#####################################################
#### USE IMPUTED DATASETS ###########################
####################################################

#adjust the RF by downsampling Majority class
setwd("E:/collaborations/tara/outputs/RFResults/NAmerica")
#create a new column with new version of IUCN category
LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse=NAmericaImputedSpatial[,c(1,4:7,11:41)]
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.list.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

memory.limit(size=50000)

### this does the downsampling to a value of 29 (# of NoLC)
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=29, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYNAm + PlantHeightTRYNAm + LeafPhenoTRYNAm + abs_max_lat +
                             abs_min_lat + length_lat + median_lon + median_lat + area + bio1 + bio2 + bio3 + bio4 +
                             bio5 + bio6+ bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + bio16 +
                             bio17 + bio18 , data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="DownsampleImputed29NoLCvLC.csv", sep=",", append=T)
  
  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="DownsampleImputed29NoLCvLCErrorRates.csv", sep=",", append=T)
  
  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,54)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBSpecies29.csv", sep=",", append=T)
  
  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataDownsampleImputed29NoLCvLC.csv", sep=",", append=T)
  
  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableDownsampleImputed29NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$Red.list.status),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableDownsampleImputed29NoLCvLC.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=50, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=50, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYNAm + PlantHeightTRYNAm + LeafPhenoTRYNAm + abs_max_lat +
                             abs_min_lat + length_lat + median_lon + median_lat + area + bio1 + bio2 + bio3 + bio4 +
                             bio5 + bio6+ bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + bio16 +
                             bio17 + bio18 , data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="ResampleImputed50NoLCvLC.csv", sep=",", append=T)
  
  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="ResampleImputed50NoLCvLCErrorRates.csv", sep=",", append=T)
  
  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,37)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBSpecies50.csv", sep=",", append=T)
  
  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataResampleImputed50NoLCvLC.csv", sep=",", append=T)
  
  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableResampleImputed50NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$Red.list.status),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleImputed50NoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE COMPLETE DATASETS #########################
####################################################

#select species that have at least 4 observations
SpatialNorthAmerica=NAmericaNoNARL
SpatialNorthAmerica4=SpatialNorthAmerica[SpatialNorthAmerica$n.gps>=5,]

AfricaImputedSpatialToUse=SpatialNorthAmerica4[,]
#AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))


#adjust the RF by downsampling Majority class
setwd("E:/collaborations/tara/outputs/RFResults/NAmerica")
#create a new column with new version of IUCN category

memory.limit(size=50000)

### this does the downsampling to a value of 17
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=17, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYNAm + PlantHeightTRYNAm + LeafPhenoTRYNAm + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="DownsampleNAmComplete17NoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="DownsampleNamComplete17NoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,37)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOOBSpeciesDownsampleNAm17.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataNAmDownsampleImputed17NoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableNAmDownsampleComplete17NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableNAmDownsampleComplete17NoLCvLC.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling 34 of each

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=34, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=34, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYNAm + PlantHeightTRYNAm + LeafPhenoTRYNAm + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleNAmComplete34NoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleNAmComplete34NoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,37)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOOBSpeciesDownsampleNAm34.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleCompleteNAm34NoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleCompleteNAm34NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleCompleteNAm34NoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE ENDEMIC DATASETS ##########################
####################################################

#select species that have at least 4 observations
SpatialNorthAmerica=NAmericaNoNARL
SpatialNorthAmerica4=SpatialNorthAmerica[SpatialNorthAmerica$n.gps>=5,]
SpatialNorthAmerica4=SpatialNorthAmerica4[SpatialNorthAmerica4$dist=="endemic",]

AfricaImputedSpatialToUse=SpatialNorthAmerica4[,]
#AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))


#adjust the RF by downsampling Majority class
setwd("E:/collaborations/tara/outputs/RFResults/NAmerica")
#create a new column with new version of IUCN category

memory.limit(size=50000)

### this does the downsampling to a value of 5
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=5, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYNAm + PlantHeightTRYNAm + LeafPhenoTRYNAm + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="DownsampleNAmComplete5EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="DownsampleNamComplete5EndemNoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,37)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOOBSpeciesDownsampleNAm5Endem.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataNAmDownsampleImputed5EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableNAmDownsampleComplete5EndemNoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableNAmDownsampleComplete5EndemNoLCvLC.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling 37 of each

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=37, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=37, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYNAm + PlantHeightTRYNAm + LeafPhenoTRYNAm + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleNAmComplete37EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleNAmComplete37EndemNoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,37)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOOBSpeciesDownsampleNAm37Endem.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleCompleteNAm37EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleCompleteNAm37EndemNoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleCompleteNAm37EndemNoLCvLC.csv", sep=',', append=T)
  
}



######## EUROPE ####################



#####################################################
#### USE IMPUTED DATASETS ###########################
####################################################


#adjust the RF by downsampling Majority class
setwd("E:/collaborations/tara/outputs/RFResults/Europe")
#create a new column with new version of IUCN category


#select the species that have at least 4 observations

memory.limit(size=50000)

### this does the downsampling to a value of 17 (# of NoLC)
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=17, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYEur + PlantHeightTRYEur + LeafPhenoTRYEur + DisperSyndTRYEur +
                             abs_max_lat +
                             abs_min_lat + length_lat + median_lon + median_lat + area + bio1 + bio2 + bio3 + bio4 +
                             bio5 + bio6+ bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + bio16 +
                             bio17 + bio18 , data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="DownsampleImputed38NoLCvLC.csv", sep=",", append=T)
  
  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="DownsampleImputed38NoLCvLCErrorRates.csv", sep=",", append=T)
  
  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataDownsampleImputed38NoLCvLC.csv", sep=",", append=T)
  
  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,37)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBSpecies38.csv", sep=",", append=T)
  
  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableDownsampleImputed38NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$Red.list.status),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableDownsampleImputed38NoLCvLC.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=70, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=70, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYEur + PlantHeightTRYEur + LeafPhenoTRYEur + DisperSyndTRYEur +
                             abs_max_lat +
                             abs_min_lat + length_lat + median_lon + median_lat + area + bio1 + bio2 + bio3 + bio4 +
                             bio5 + bio6+ bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + bio16 +
                             bio17 + bio18 , data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  Contribution=fitRFPNW$importance
  write.table(Contribution,file="ResampleImputed70NoLCvLC.csv", sep=",", append=T)
  
  errorRates=fitRFPNW$err.rate
  write.table(errorRates, file="ResampleImputed70NoLCvLCErrorRates.csv", sep=",", append=T)
  
  SpeciesPredicted = fitRFPNW$votes
  SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,37)]
  SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  write.table(SpeciesPredicted, file="PredictionOfOOBSpecies70.csv", sep=",", append=T)
  
  
  PredictionPNW <- predict(fitRFPNW)
  binded.dataRFPNW=c()
  binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  write.table(binded.dataRFPNW,file="binded.dataResampleImputed70NoLCvLC.csv", sep=",", append=T)
  
  ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  diag(prop.table(ctRFPNW, 1))
  write.table(ctRFPNW, file="confusionTableResampleImputed70NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$Red.list.status),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleImputed70NoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE COMPLETE DATASETS #########################
####################################################
#select species that have at least 4 observations
SpatialDataEurope=EuropeNoNARL
SpatialDataEurope4=SpatialDataEurope[SpatialDataEurope$n.gps>=5,]
EuropeImputedSpatial=SpatialDataEurope4

AfricaImputedSpatialToUse=EuropeImputedSpatial[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

#adjust the RF by downsampling Majority class
setwd("E:/collaborations/tara/outputs/RFResults/Europe")
#create a new column with new version of IUCN category

memory.limit(size=50000)

###  get 4 samples each
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=4, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYEur + PlantHeightTRYEur + LeafPhenoTRYEur + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="DownsampleEurComplete4NoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="DownsampleEurComplete4NoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,54)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBEurDownsampleSpecies4.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataEurDownsampleImputed4NoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableEurDownsampleComplete4NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableEurDownsampleComplete4NoLCvLC.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling 8 of each 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=8, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=8, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYEur + PlantHeightTRYEur + LeafPhenoTRYEur + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleCompleteEur8NoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleCompleteEur8NoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,54)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOOBSpeciesDownsampleEur8.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleCompleteEur8NoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleEurComplete8NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableEurResampleComplete8NoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE ENDEMIC DATASETS #########################
####################################################
#select species that have at least 4 observations
SpatialDataEurope=EuropeNoNARL
SpatialDataEurope4=SpatialDataEurope[SpatialDataEurope$n.gps>=5,]
SpatialDataEurope4=SpatialDataEurope[SpatialDataEurope$dist=="endemic",]
EuropeImputedSpatial=SpatialDataEurope4

AfricaImputedSpatialToUse=EuropeImputedSpatial[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

#adjust the RF by downsampling Majority class
setwd("E:/collaborations/tara/outputs/RFResults/Europe")
#create a new column with new version of IUCN category

memory.limit(size=50000)

###  get 2 samples each
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=2, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYEur + PlantHeightTRYEur + LeafPhenoTRYEur + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="DownsampleEurComplete2EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="DownsampleEurComplete2EndemNoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,54)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBEurDownsampleSpecies2Endem.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataEurDownsampleImputed2EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableEurDownsampleComplete2EndemNoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableEurDownsampleComplete2EndemNoLCvLC.csv", sep=',', append=T)
  
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling 35 of each 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=35, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=35, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYEur + PlantHeightTRYEur + LeafPhenoTRYEur + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF, importance=TRUE, ntree=1000, replace=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleCompleteEur35EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleCompleteEur35EndemNoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,54)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOOBSpeciesDownsampleEur35Endem.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleCompleteEur35EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleEurComplete35EndemNoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableEurResampleComplete35EndemNoLCvLC.csv", sep=',', append=T)
  
}

#################### ASIA

####################################################
#### USE COMPLETE DATASETS #########################
####################################################

#select the species that have at least 4 observations
SpatialDataAsia=AsiaNoNARL
SpatialDataAsia4=SpatialDataAsia[SpatialDataAsia$n.gps>=5,]

AfricaImputedSpatialToUse=SpatialDataAsia4[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

setwd("E:/collaborations/tara/outputs/RFResults/Asia")

memory.limit(size=50000)

### this does the downsampling to a value of 43 (# obs in noLC category)
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=43, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYAsia + LeafPhenoTRYAsia + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="FullAsiaNoLCvLCDown43.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="FullAsiaNoLCvLCErrorRatesDown43.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataFullAsiaNoLCvLCDown43.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableFullNoLCvLCDownAsia43.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCDownAsia43.csv", sep=',', append=T)
  
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesDown43.csv", sep=",", append=T)
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling 86 of each ... 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=86, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=86, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYAsia + LeafPhenoTRYAsia + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleComplete86AsiaNoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleComplete86AsiaNoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="ResamplePredictionOfOOBSpeciesAsia86.csv", sep=",", append=T)
  # 
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleAsiaComplete86NoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleAsiaComplete86NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableAsiaResampleComplete86NoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE ENDEMIC DATASETS #########################
####################################################

#select the species that have at least 4 observations
SpatialDataAsia=AsiaNoNARL
SpatialDataAsia4=SpatialDataAsia[SpatialDataAsia$n.gps>=5,]
SpatialDataAsia4=SpatialDataAsia[SpatialDataAsia$dist=="endemic",]

AfricaImputedSpatialToUse=SpatialDataAsia4[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

setwd("E:/collaborations/tara/outputs/RFResults/Asia")

memory.limit(size=50000)

### this does the downsampling to a value of 36 (# obs in noLC category)
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=36, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYAsia + LeafPhenoTRYAsia + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="FullAsiaNoLCvLCDown36Endem.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="FullAsiaNoLCvLCErrorRatesDown36Endem.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataFullAsiaNoLCvLCDown36Endem.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableFullNoLCvLCDownAsia36Endem.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCDownAsia36Endem.csv", sep=',', append=T)
  
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesDown36Endem.csv", sep=",", append=T)
}

#x11()
#print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
#dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
#dev.off()

##### resampling 38 of each ... 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=38, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=38, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYAsia + LeafPhenoTRYAsia + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleComplete38EndemAsiaNoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleComplete38EndemAsiaNoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="ResamplePredictionOfOOBSpeciesAsia38Endem.csv", sep=",", append=T)
  # 
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleAsiaComplete38EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleAsiaComplete38EndemNoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableAsiaResampleComplete38EndemNoLCvLC.csv", sep=',', append=T)
  
}


#################### AUSTRALIA

####################################################
#### USE COMPLETE DATASETS #########################
####################################################
setwd("E:/collaborations/tara/outputs/RFResults/Australia")
#select the species that have at least 4 observations
SpatialDataAus=AusNoNARL
SpatialDataAus4=SpatialDataAus[SpatialDataAus$n.gps>=5,]


AfricaImputedSpatialToUse=SpatialDataAus4[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))


setwd("E:/collaborations/tara/outputs/RFResults/Australia")
#create a new column with new version of IUCN category

memory.limit(size=50000)

### Impossible. Only 40 NoLC observations :/
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=40, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYAus + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="FullNoLCvLCDownAusDown40.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="FullNoLCvLCErrorRatesAusDown40.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataFullNoLCvLCAusDown40.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableFullNoLCvLCAusDown40.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW[,c(1:2,51:55)],binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCAusDown40.csv", sep=',', append=T)
  
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,52)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesDownAus40.csv", sep=",", append=T)
}

# x11()
# print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
# #dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
# dev.off()

##### resampling 80 of each ... 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=80, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=80, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYAus + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleComplete80AusNoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleComplete80AusNoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,52)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesResampleAus80.csv", sep=",", append=T)
  # 
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleCompleteAus80NoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleCompleteAus80NoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW[,c(1:2,51:55)],binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleCompleteAus80NoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE ENDEMICS DATASETS #########################
####################################################
#select the species that have at least 4 observations
SpatialDataAus=AusNoNARL
SpatialDataAus4=SpatialDataAus[SpatialDataAus$n.gps>=5,]
SpatialDataAus4=SpatialDataAus[SpatialDataAus$dist=="endemic",]

AfricaImputedSpatialToUse=SpatialDataAus4[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))


setwd("E:/collaborations/tara/outputs/RFResults/Australia")
#create a new column with new version of IUCN category

memory.limit(size=50000)

### Only 23 NoLC observations 
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=23, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYAus + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="FullNoLCvLCDownAusDown23Endem.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="FullNoLCvLCErrorRatesAusDown23Endem.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataFullNoLCvLCAusDown23Endem.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableFullNoLCvLCAusDown23Endem.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW[,c(1:2,51:55)],binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCAusDown23Endem.csv", sep=',', append=T)
  
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,52)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesDownAus23Endem.csv", sep=",", append=T)
}

# x11()
# print(plot(binded.Predict$PredictionNEW, main="Prediction NA", beside=F) )
# #dev.copy(tiff,width = 1000, height = 1000, compression="none",file="Ascaphus.montanus.truei.tif")
# dev.off()

##### resampling 61 of each ... 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=61, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=61, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYAus + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleComplete61EndemAusNoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleComplete61EndemAusNoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,52)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesResampleAus61Endem.csv", sep=",", append=T)
  # 
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleCompleteAus61EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleCompleteAus61EndemNoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW[,c(1:2,51:55)],binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleCompleteAus61EndemNoLCvLC.csv", sep=',', append=T)
  
}



#################### CENTRAL AMERICA

####################################################
#### USE COMPLETE DATASETS #########################
####################################################
#select the species that have at least 4 observations
setwd("E:/collaborations/tara/outputs/RFResults/CentralAmerica")

CANoNARL4=CANoNARL[CANoNARL$n.gps>=5,]
CAMorphoSpatial4=CANoNARL4

AfricaImputedSpatialToUse=CAMorphoSpatial4[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

setwd("E:/collaborations/tara/outputs/RFResults/CentralAmerica")


memory.limit(size=50000)

### Downsampling to 52 in LC
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=52, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYCA + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="FullNoLCvLCDown52CANew.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="FullNoLCvLCErrorRatesDown52CANew.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataFullNoLCvLCDown52CANew.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableFullNoLCvLCDown52CANew.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCDown52CANew.csv", sep=',', append=T)
  
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesDown52CANew.csv", sep=",", append=T)
}



##### resampling 104 of each ... 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=104, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=104, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYCA + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleComplete104CANoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleComplete104CANoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="ResampleCompletePredictionOfOOBSpecies104CA.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleComplete104CANoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleComplete104CANoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleComplete104CANoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE ENDEMICS DATASETS #########################
####################################################
#select the species that have at least 4 observations
CANoNARL4=CANoNARL[CANoNARL$n.gps>=5,]
CANoNARL4=CANoNARL[CANoNARL$dist=="endemic",]

CAMorphoSpatial4=CANoNARL4

AfricaImputedSpatialToUse=CAMorphoSpatial4[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))

setwd("E:/collaborations/tara/outputs/RFResults/CentralAmerica")


memory.limit(size=50000)

### Downsampling to 23 in LC
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=23, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYCA + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="FullNoLCvLCDown23EndemCANew.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="FullNoLCvLCErrorRatesDown23EndemCANew.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataFullNoLCvLCDown23EndemCANew.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableFullNoLCvLCDown23EndemCANew.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCDown23EndemCANew.csv", sep=',', append=T)
  
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesDown23EndemCANew.csv", sep=",", append=T)
}



##### resampling 61 of each ... 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=61, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=61, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYCA + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleComplete61EndemCANoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleComplete61EndemCANoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="ResampleCompletePredictionOfOOBSpecies61EndemCA.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleComplete61EndemCANoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleComplete61EndemCANoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW,binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleComplete61EndemCANoLCvLC.csv", sep=',', append=T)
  
}



#################### SOUTH AMERICA

####################################################
#### USE COMPLETE DATASETS #########################
####################################################
#select the species that have at least 4 observations
setwd("E:/collaborations/tara/outputs/RFResults/SouthAmerica")

SpatialDataSA4=SANoNARL[SANoNARL$n.gps>=5,]

AfricaImputedSpatialToUse=SpatialDataSA4[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))



setwd("E:/collaborations/tara/outputs/RFResults/SouthAmerica")
#create a new column with new version of IUCN category -- > No LC :(
memory.limit(size=50000)

### Downsampling to 300 in LC
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=300, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYSA + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="FullNoLCvLCDownSA300.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="FullNoLCvLCErrorRatesDownSA300.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataFullNoLCvLCDownSA300.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableFullNoLCvLCDownSA300.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW[,c(1:2,52:56)],binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCDownSA300.csv", sep=',', append=T)
  
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesDownSA300.csv", sep=",", append=T)
}



##### resampling 459 of each ... 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=459, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=459, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYSA + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleCompleteSA459NoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleCompleteSA459NoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesSAResample459.csv", sep=",", append=T)
  # 
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleComplete459SANoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleComplete459SANoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW[,c(1:2,52:56)],binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleComplete459SANoLCvLC.csv", sep=',', append=T)
  
}

####################################################
#### USE ENDEMICS DATASETS #########################
####################################################
#select the species that have at least 4 observations
SpatialDataSA4=SANoNARL[SANoNARL$n.gps>=5,]
SpatialDataSA4=SANoNARL[SANoNARL$dist=="endemic",]

AfricaImputedSpatialToUse=SpatialDataSA4[,]
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'DD'] <- NA
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/lc'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'LR/nt'] <- "LC"
AfricaImputedSpatialToUse[AfricaImputedSpatialToUse == 'NT'] <- "LC"

LC="LC"
NoLC="NoLC"
AfricaImputedSpatialToUse = within(AfricaImputedSpatialToUse, {
  NewIUCN <- ifelse(Red.List.status == "LC", LC, NoLC)
})
AfricaImputedSpatialToUse$NewIUCN<-(as.factor(AfricaImputedSpatialToUse$NewIUCN))



setwd("E:/collaborations/tara/outputs/RFResults/SouthAmerica")
#create a new column with new version of IUCN category -- > No LC :(
memory.limit(size=50000)

### Downsampling to 220 in LC
for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="NoLC",]
  Csel=C[(sample(nrow(C), size=220, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="LC",]
  PNWRF=rbind(Csel,NC)
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYSA + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="FullNoLCvLCDownSA220Endem.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="FullNoLCvLCErrorRatesDownSA220Endem.csv", sep=",", append=T)
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataFullNoLCvLCDownSA220Endem.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableFullNoLCvLCDownSA220Endem.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW[,c(1:2,52:56)],binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableFullNoLCvLCDownSA220Endem.csv", sep=',', append=T)
  
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesDownSA220Endem.csv", sep=",", append=T)
}



##### resampling 281 of each ... 

for (i in 1:1000) {
  PNWRF=c()
  fitRFPNW=c()
  binded.Predict=c()
  PNWRF=na.omit(AfricaImputedSpatialToUse)
  C=PNWRF[PNWRF$NewIUCN=="LC",]
  Csel=C[(sample(nrow(C), size=281, replace=T)),]
  NC=PNWRF[PNWRF$NewIUCN=="NoLC",]
  NCsel=NC[(sample(nrow(NC), size=281, replace=T)),]
  PNWRF=rbind(Csel,NCsel)
  
  
  
  fitRFPNW <- randomForest(NewIUCN ~ WoodinessTRYSA + abs_max_lat 
                           + abs_min_lat + length_lat + median_lon + median_lat + area
                           + bio1m + bio1sd + bio2m + bio2sd + bio3m + bio3sd + bio4m + bio4sd + bio5m + bio5sd
                           + bio6m + bio6sd + bio7m + bio7sd + bio8m + bio8sd + bio9m + bio9sd + bio10m + bio10sd
                           + bio11m + bio11sd + bio12m + bio12sd + bio13m + bio13sd + bio14m + bio14sd
                           + bio15m + bio15sd + bio16m + bio16sd + bio17m + bio17sd + bio18m + bio18sd + bio19m
                           + bio19sd + elevm + elevsd, data=PNWRF,
                           importance=TRUE, ntree=1000, replace=T, err.rate=T, votes=T)
  
  # Contribution=fitRFPNW$importance
  # write.table(Contribution,file="ResampleCompleteSA281EndemNoLCvLC.csv", sep=",", append=T)
  # 
  # errorRates=fitRFPNW$err.rate
  # write.table(errorRates, file="ResampleCompleteSA281EndemNoLCvLCErrorRates.csv", sep=",", append=T)
  # 
  # SpeciesPredicted = fitRFPNW$votes
  # SpeciesInfo =  PNWRF[row.names(fitRFPNW$votes),c(1,53)]
  # SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
  # write.table(SpeciesPredicted, file="PredictionOfOOBSpeciesSAResample281Endem.csv", sep=",", append=T)
  # 
  # 
  # PredictionPNW <- predict(fitRFPNW)
  # binded.dataRFPNW=c()
  # binded.dataRFPNW=cbind(PNWRF, PredictionPNW) # bind initial data table with predicted values
  # write.table(binded.dataRFPNW,file="binded.dataResampleComplete281EndemSANoLCvLC.csv", sep=",", append=T)
  # 
  # ctRFPNW <- table(binded.dataRFPNW$NewIUCN, PredictionPNW)
  # diag(prop.table(ctRFPNW, 1))
  # write.table(ctRFPNW, file="confusionTableResampleComplete281EndemSANoLCvLC.csv", sep=",",append=T)
  
  NAData=AfricaImputedSpatialToUse[is.na(AfricaImputedSpatialToUse$NewIUCN),]
  PredictionNEWprob <- predict(fitRFPNW,NAData,type="prob") #calculates probability for each group
  PredictionNEW <- predict(fitRFPNW,NAData)
  binded.dataRFPNW=cbind(NAData, PredictionNEWprob,PredictionNEW)
  binded.Predict=rbind(binded.dataRFPNW[,c(1:2,52:56)],binded.Predict)
  write.table(binded.Predict, file="PredictNAData_confusionTableResampleComplete281EndemSANoLCvLC.csv", sep=',', append=T)
  
}
