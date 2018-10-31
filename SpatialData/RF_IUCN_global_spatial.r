#run RF for all continents using env/geo data for prediction.

library(randomForest)
library(plyr)

global_data<-read.table("global_data.txt", sep="\t", header=T)
global_data<-as.data.frame(global_data)

#############################DATA PREP########################################
#remove algae
ordersNLP=c("Acrosiphoniales","Bryopsidales","Cladophorales","Dasycladales","Ignatiales","Oltmansiellopsidales", "Trentepohliales","Ulotrichales","Ulvales","Oedogoniales","Chaetophorales","Chaetopeltidales", "Chlamydomonadales","Sphaeropleales","Chlorellales","Oocystaceae","Microthamniales","Trebouxiales","Pyramimonadales","Prasinococcales","Palmophyllales","Zygnematales","Desmidiales","Charales", "Coleochaetales","Pseudoscourfieldiales","Pedinomonadales","Volvocales")

#import each cont occurrence file and get algae species
temp<-DATA[which(DATA$order %in% ordersNLP),]
temp<-temp$species
#algaeList<-temp
algaeList<-append(algaeList, temp)
algaeList<-unique(algaeList)

global_data<-global_data[! global_data$name %in% algaeList,]

#change red List status to follow newest Listings
global_data$Red.List.status[global_data$Red.List.status == "LR/nt"] <-"NT"
global_data$Red.List.status[global_data$Red.List.status == "LR/cd"] <-"VU"
global_data$Red.List.status[global_data$Red.List.status == "LR/lc"] <-"LC"

#remove those with low sample size
global_data_3<-subset(global_data, n.gps > 4)

#get numbers for table
table(global_data_3$Red.List.status)
sum(is.na(global_data_3$Red.List.status))

#change DD to NA because there is not enough info and these should be predicted too
global_data_3$Red.List.status<-as.factor(global_data_3$Red.List.status)
global_data_3$Red.List.status[global_data_3$Red.List.status == "DD"] <-"NA"
table(global_data_3$Red.List.status)
sum(is.na(global_data_3$Red.List.status))

#to get rid of empty classes (there must be a better way to do this???)
global_data_3$Red.List.status<-as.character(global_data_3$Red.List.status)
global_data_3$Red.List.status<-as.factor(global_data_3$Red.List.status)
table(global_data_3$Red.List.status)


#############################random forest########################################

#build files for rf model prediction and na data
na_data<-global_data_3[is.na(global_data_3$Red.List.status),]
pred_data<-na.omit(global_data_3)

#remove EW and EX and empty classes
pred_data<-pred_data[pred_data$Red.List.status!='EW' & pred_data$Red.List.status!='EX' ,]
pred_data$Red.List.status<-as.character(pred_data$Red.List.status)
pred_data$Red.List.status<-as.factor(pred_data$Red.List.status)

#add column to label as LC or NoLC
NoLC<-c('CR','EN','EW','EX','VU')
for (i in 1:nrow(pred_data)) {
  if (pred_data$Red.List.status[i] %in% NoLC) {
    pred_data$rls_LC[i] <-'NoLC' 
  }
  else {
    pred_data$rls_LC[i] <-'LC'
  }
}
pred_data$rls_LC<-as.factor(pred_data$rls_LC)

#add column to label as CR or NoCR
NoCR<-c('LC','EN','EW','EX','NT','VU')
for (i in 1:nrow(pred_data)) {
  if (pred_data$Red.List.status[i] %in% NoCR) {
    pred_data$rls_CR[i] <-'NoCR' 
  }
  else {
    pred_data$rls_CR[i] <-'CR'
  }
}
pred_data$rls_CR<-as.factor(pred_data$rls_CR)


#explore data for rf input
apply(pred_data, 2, function(pred_data)length(unique(pred_data)))
table(pred_data$Red.List.status)
table(pred_data$rls_LC)
table(pred_data$rls_CR)
names(pred_data)

#################################
#1: ALL IUCN CATEGORIES FULL DATA
fitRF <- randomForest(Red.List.status ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area + bio1m + bio2m + bio3m + bio4m + bio5m + bio6m + bio7m + bio8m + bio9m + bio10m + bio11m + bio12m + bio13m + bio14m + bio15m + bio16m + bio17m + bio18m + bio19m + bio1sd + bio2sd + bio3sd + bio4sd + bio5sd + bio6sd + bio7sd + bio8sd + bio9sd + bio10sd + bio11sd + bio12sd + bio13sd + bio14sd + bio15sd + bio16sd + bio17sd + bio18sd + bio19sd + elevm + elevsd, data=pred_data, importance=TRUE, ntree=1000, replace=T)

fitRF
write.csv(fitRF$importance, file="global_AllFull_imp.csv")

predictRF<-predict(fitRF, newdata=na_data, type="prob")
df<-data.frame(na_data$name, predictRF)
write.csv(df, file="global_AllFull_pred.csv", row.names=FALSE)

##########################
#2: LC vs non-LC FULL DATA
fitRF <- randomForest(rls_LC ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area + bio1m + bio2m + bio3m + bio4m + bio5m + bio6m + bio7m + bio8m + bio9m + bio10m + bio11m + bio12m + bio13m + bio14m + bio15m + bio16m + bio17m + bio18m + bio19m + bio1sd + bio2sd + bio3sd + bio4sd + bio5sd + bio6sd + bio7sd + bio8sd + bio9sd + bio10sd + bio11sd + bio12sd + bio13sd + bio14sd + bio15sd + bio16sd + bio17sd + bio18sd + bio19sd + elevm + elevsd, data=pred_data, importance=TRUE, ntree=1000, replace=T, votes=T)

fitRF
write.csv(fitRF$importance, file="global_LCFull_imp.csv")

predictRF<-predict(fitRF, newdata=na_data, type="prob")
df<-data.frame(na_data$name, predictRF)
write.csv(df, file="global_LCFull_pred.csv", row.names=FALSE)

#get bad species
SpeciesPredicted = fitRF$votes
SpeciesInfo =  pred_data[c(1,52)]
SpeciesPredicted = cbind(SpeciesPredicted, SpeciesInfo)
write.csv(SpeciesPredicted, file="global_LCFull_OOBSpecies.csv", row.names=FALSE)

LC_species<-SpeciesPredicted[SpeciesPredicted$rls_LC == "LC",]
LC_species_9<-LC_species[LC_species$LC < 0.1,]
LC_species_8<-LC_species[LC_species$LC < 0.2,]
LC_species_9<-LC_species_9$name
LC_species_8<-LC_species_8$name

NoLC_species<-SpeciesPredicted[SpeciesPredicted$rls_LC == "NoLC",]
NoLC_species_9<-NoLC_species[NoLC_species$NoLC < 0.1,]
NoLC_species_8<-NoLC_species[NoLC_species$NoLC < 0.2,]
NoLC_species_9<-NoLC_species_9$name
NoLC_species_8<-NoLC_species_8$name

badspecies_8<-c(LC_species_8, NoLC_species_8)

badspecies_9<-c(LC_species_9, NoLC_species_9) 

##########################
#remove bad species at <0.9 of being wrong
pred_data_new<-pred_data[!pred_data$name %in% badspecies_9,]

fitRF <- randomForest(rls_LC ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area + bio1m + bio2m + bio3m + bio4m + bio5m + bio6m + bio7m + bio8m + bio9m + bio10m + bio11m + bio12m + bio13m + bio14m + bio15m + bio16m + bio17m + bio18m + bio19m + bio1sd + bio2sd + bio3sd + bio4sd + bio5sd + bio6sd + bio7sd + bio8sd + bio9sd + bio10sd + bio11sd + bio12sd + bio13sd + bio14sd + bio15sd + bio16sd + bio17sd + bio18sd + bio19sd + elevm + elevsd, data=pred_data_new, importance=TRUE, ntree=1000, replace=T, votes=T)

fitRF

write.csv(fitRF$importance, file="global_LCrm9_imp.csv")

predictRF<-predict(fitRF, newdata=na_data, type="prob")
df<-data.frame(na_data$name, predictRF)
write.csv(df, file="global_LCrm9_pred.csv", row.names=FALSE)

##########################
#remove bad species at <0.8 of being wrong
pred_data_new<-pred_data[!pred_data$name %in% badspecies_8,]

fitRF <- randomForest(rls_LC ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area + bio1m + bio2m + bio3m + bio4m + bio5m + bio6m + bio7m + bio8m + bio9m + bio10m + bio11m + bio12m + bio13m + bio14m + bio15m + bio16m + bio17m + bio18m + bio19m + bio1sd + bio2sd + bio3sd + bio4sd + bio5sd + bio6sd + bio7sd + bio8sd + bio9sd + bio10sd + bio11sd + bio12sd + bio13sd + bio14sd + bio15sd + bio16sd + bio17sd + bio18sd + bio19sd + elevm + elevsd, data=pred_data_new, importance=TRUE, ntree=1000, replace=T, votes=T)

fitRF

write.csv(fitRF$importance, file="global_LCrm8_imp.csv")

predictRF<-predict(fitRF, newdata=na_data, type="prob")
df<-data.frame(na_data$name, predictRF)
write.csv(df, file="global_LCrm8_pred.csv", row.names=FALSE)


##########################
#3: CR vs non-CR FULL DATA
fitRF <- randomForest(rls_CR ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area + bio1m + bio2m + bio3m + bio4m + bio5m + bio6m + bio7m + bio8m + bio9m + bio10m + bio11m + bio12m + bio13m + bio14m + bio15m + bio16m + bio17m + bio18m + bio19m + bio1sd + bio2sd + bio3sd + bio4sd + bio5sd + bio6sd + bio7sd + bio8sd + bio9sd + bio10sd + bio11sd + bio12sd + bio13sd + bio14sd + bio15sd + bio16sd + bio17sd + bio18sd + bio19sd + elevm + elevsd, data=pred_data, importance=TRUE, ntree=1000, replace=T)

fitRF
write.csv(fitRF$importance, file="global_CRFull_imp.csv")

predictRF<-predict(fitRF, newdata=na_data, type="prob")
df<-data.frame(na_data$name, predictRF)
write.csv(df, file="global_CRFull_pred.csv", row.names=FALSE)

################SAMPLING###########################

################################################################
#4 resample so red Listing classes are even (NT, CR, EN, LC, VU)
table(pred_data$Red.List.status)

for (i in 1:100) {
  CR=pred_data[pred_data$Red.List.status=="CR",]
  NT=pred_data[pred_data$Red.List.status=="NT",]
  NTsamp<-NT[(sample(nrow(NT), size=32)),]
  EN=pred_data[pred_data$Red.List.status=="EN",]
  ENsamp<-EN[(sample(nrow(EN), size=32)),]
  LC=pred_data[pred_data$Red.List.status=="LC",]
  LCsamp<-LC[(sample(nrow(LC), size=32)),]
  VU=pred_data[pred_data$Red.List.status=="VU",]
  VUsamp<-VU[(sample(nrow(VU), size=32)),]
  
  pred_samp=rbind(CR,NTsamp,ENsamp,LCsamp,VUsamp)
  
  fitRF <- randomForest(Red.List.status ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area + bio1m + bio2m + bio3m + bio4m + bio5m + bio6m + bio7m + bio8m + bio9m + bio10m + bio11m + bio12m + bio13m + bio14m + bio15m + bio16m + bio17m + bio18m + bio19m + bio1sd + bio2sd + bio3sd + bio4sd + bio5sd + bio6sd + bio7sd + bio8sd + bio9sd + bio10sd + bio11sd + bio12sd + bio13sd + bio14sd + bio15sd + bio16sd + bio17sd + bio18sd + bio19sd + elevm + elevsd, data=pred_samp, importance=TRUE, ntree=1000, replace=T)
  
  imp=fitRF$importance
  write.table(imp,file="global_AllSamp_imp.csv", sep=",", append=T, col.names=!file.exists("global_AllSamp_imp.csv"))
  
  err=fitRF$err.rate
  write.table(err, file="global_AllSamp_error.csv", sep=",", row.names=FALSE, col.names=FALSE, append=T)
  
  predictRF <- predict(fitRF, newdata=na_data, type="prob")
  
  df<-data.frame(na_data$name, predictRF)
  
  write.table(df,file="global_AllSamp_predict.csv", sep=",", row.names=FALSE, col.names=!file.exists("global_AllSamp_predict.csv"), append=T)
}  

oob<-read.csv("global_AllSamp_error.csv", header=FALSE)
names(oob)<-c("oob","CR","EN","LC","NT","VU")
e<-colMeans(oob,na.rm=T)
e

probs<-read.csv("global_AllSamp_predict.csv")

m<-ddply(probs, .(na_data.name), summarize, CR=mean(CR), EN=mean(EN), LC=mean(LC), NT=mean(NT), VU=mean(VU))
write.csv(m, file="global_AllSamp_avg_probs.csv", row.names=FALSE)

imps<-read.csv("global_AllSamp_imp.csv")

n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
write.csv(n, file="global_AllSamp_avg_imp.csv", row.names=FALSE)

########################################################
#5 resample so red Listing classes are even (LC and NoLC)
table(pred_data$rls_LC)
for (i in 1:100) {
  NoLC=pred_data[pred_data$rls_LC=="NoLC",]
  LC=pred_data[pred_data$rls_LC=="LC",]
  LCsamp<-LC[(sample(nrow(LC), size=345)),]
  
  pred_samp=rbind(NoLC, LCsamp)
  
  fitRF <- randomForest(rls_LC ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area + bio1m + bio2m + bio3m + bio4m + bio5m + bio6m + bio7m + bio8m + bio9m + bio10m + bio11m + bio12m + bio13m + bio14m + bio15m + bio16m + bio17m + bio18m + bio19m + bio1sd + bio2sd + bio3sd + bio4sd + bio5sd + bio6sd + bio7sd + bio8sd + bio9sd + bio10sd + bio11sd + bio12sd + bio13sd + bio14sd + bio15sd + bio16sd + bio17sd + bio18sd + bio19sd + elevm + elevsd, data=pred_samp, importance=TRUE, ntree=1000, replace=T)
  
  imp=fitRF$importance
  write.table(imp,file="global_LCSamp_imp.csv", sep=",", append=T, col.names=!file.exists("global_LCSamp_imp.csv"))
  
  err=fitRF$err.rate
  write.table(err, file="global_LCSamp_error.csv", sep=",", row.names=FALSE, col.names=FALSE, append=T)
  
  predictRF <- predict(fitRF, newdata=na_data, type="prob")
  
  df<-data.frame(na_data$name, predictRF)
  
  write.table(df,file="global_LCSamp_predict.csv", sep=",", row.names=FALSE, col.names=!file.exists("global_LCSamp_predict.csv"), append=T)
}  

oob<-read.csv("global_LCSamp_error.csv", header=FALSE)
names(oob)<-c("oob","LC", "NoLC")
e<-colMeans(oob)
e

probs<-read.csv("global_LCSamp_predict.csv")

m<-ddply(probs, .(na_data.name), summarize, LC=mean(LC), NoLC=mean(NoLC))
write.csv(m, file="global_LCSamp_avg_probs.csv", row.names=FALSE)

imps<-read.csv("global_LCSamp_imp.csv")

n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
write.csv(n, file="global_LCSamp_avg_imp.csv", row.names=FALSE)

########################################################
#6 resample so red Listing classes are even (CR and NoCR)
table(pred_data$rls_CR)
for (i in 1:100) {
  CR=pred_data[pred_data$rls_CR=="CR",]
  noCR=pred_data[pred_data$rls_CR=="NoCR",]
  noCRsamp<-noCR[(sample(nrow(noCR), size=32)),]
  
  pred_samp=rbind(CR, noCRsamp)
  
  fitRF <- randomForest(rls_CR ~ abs_max_lat + abs_min_lat + length_lat + median_lon + median_lat + area + bio1m + bio2m + bio3m + bio4m + bio5m + bio6m + bio7m + bio8m + bio9m + bio10m + bio11m + bio12m + bio13m + bio14m + bio15m + bio16m + bio17m + bio18m + bio19m + bio1sd + bio2sd + bio3sd + bio4sd + bio5sd + bio6sd + bio7sd + bio8sd + bio9sd + bio10sd + bio11sd + bio12sd + bio13sd + bio14sd + bio15sd + bio16sd + bio17sd + bio18sd + bio19sd + elevm + elevsd, data=pred_samp, importance=TRUE, ntree=1000, replace=T)
  
  imp=fitRF$importance
  write.table(imp,file="global_CRSamp_imp.csv", sep=",", append=T, col.names=!file.exists("global_CRSamp_imp.csv"))
  
  err=fitRF$err.rate
  write.table(err, file="global_CRSamp_error.csv", sep=",", row.names=FALSE, col.names=FALSE, append=T)
  
  predictRF <- predict(fitRF, newdata=na_data, type="prob")
  
  df<-data.frame(na_data$name, predictRF)
  
  write.table(df,file="global_CRSamp_predict.csv", sep=",", row.names=FALSE, col.names=!file.exists("global_CRSamp_predict.csv"), append=T)
}  

oob<-read.csv("global_CRSamp_error.csv", header=FALSE)
names(oob)<-c("oob","CR","NoCR")
e<-colMeans(oob,na.rm=T)
e

probs<-read.csv("global_CRSamp_predict.csv")

m<-ddply(probs, .(na_data.name), summarize, CR=mean(CR), NoCR=mean(NoCR))
write.csv(m, file="global_CRSamp_avg_probs.csv", row.names=FALSE)

imps<-read.csv("global_CRSamp_imp.csv")

n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
write.csv(n, file="global_CRSamp_avg_imp.csv", row.names=FALSE)
