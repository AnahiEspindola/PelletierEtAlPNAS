library(Reol)
library(rjson)
library(RCurl)
library(ape)
library(plyr)
library(taxonlookup)
library(taxize)

setwd("E:/collaborations/tara/outputs")

#first, we want to get all the data ready
#read the datasets from files
AllDataGlobal=read.table(file="AllDatasetsGlobal.txt", sep=";",row.names=1, header=T)

AllDataNAm=read.table(file="AllDatasetsNAmerica.txt", sep=";",row.names=1, header=T)
AllDataAfr=read.table(file="AllDatasetsAfricaNew.txt", sep=";",row.names=1, header=T)
AllDataEur=read.table(file="AllDatasetsEuropeNew.txt", sep=";",row.names=1, header=T)
AllDataEur=AllDataEur[!duplicated(AllDataEur$Species), ] #removes duplicate species
AllDataAsia=read.table(file="AllDatasetsAsia.txt", sep=";",row.names=1, header=T)
AllDataAus=read.table(file="AllDatasetsAus.txt", sep=";",row.names=1, header=T)
AllDataCA=read.table(file="AllDatasetsCANew.txt", sep=";",row.names=1, header=T)
AllDataSA=read.table(file="AllDatasetsSANew.txt", sep=";",row.names=1, header=T)


#get taxonomic info for each species
TaxonomyGlobal=lookup_table(as.character(AllDataGlobal$Species),by_species=TRUE)
TaxonomyEur=lookup_table(as.character(AllDataEur$Species),by_species=TRUE)
TaxonomyNAm=lookup_table(as.character(AllDataNAm$Species),by_species=TRUE)
TaxonomyAfr=lookup_table(as.character(AllDataAfr$Species),by_species=TRUE)
TaxonomyAsia=lookup_table(as.character(AllDataAsia$name),by_species=TRUE)
TaxonomyAus=lookup_table(as.character(AllDataAus$name),by_species=TRUE)
TaxonomyCA=lookup_table(as.character(AllDataCA$name),by_species=TRUE)
TaxonomySA=lookup_table(as.character(AllDataSA$name),by_species=TRUE)



#now link this to the general table so that everything is together
AllDataGlobalTax=merge(AllDataGlobal,TaxonomyGlobal,by.x="Species",by.y="row.names",all=T)
AllDataAfrTax=merge(AllDataAfr,TaxonomyAfr,by.x="Species",by.y="row.names",all=T)
AllDataEurTax=merge(AllDataEur,TaxonomyEur,by.x="Species",by.y="row.names",all=T)
AllDataNAmTax=merge(AllDataNAm,TaxonomyNAm,by.x="Species",by.y="row.names",all=T)
AllDataAsiaTax=merge(AllDataAsia,TaxonomyAsia,by.x="name",by.y="row.names",all=T)
AllDataAusTax=merge(AllDataAus,TaxonomyAus,by.x="name",by.y="row.names",all=T)
AllDataCATax=merge(AllDataCA,TaxonomyCA,by.x="name",by.y="row.names",all=T)
AllDataSATax=merge(AllDataSA,TaxonomySA,by.x="name",by.y="row.names",all=T)


#complete all species that are missing taxonomic information
GlobalNoTax=AllDataGlobalTax[is.na(AllDataGlobalTax$genus),]
AfrNoTax=AllDataAfrTax[is.na(AllDataAfrTax$genus),]
EurNoTax=AllDataEurTax[is.na(AllDataEurTax$genus),]
NAmNoTax=AllDataNAmTax[is.na(AllDataNAmTax$genus),]
AsiaNoTax=AllDataAsiaTax[is.na(AllDataAsiaTax$genus),]
AusNoTax=AllDataAusTax[is.na(AllDataAusTax$genus),]
CANoTax=AllDataCATax[is.na(AllDataCATax$genus),]
SANoTax=AllDataSATax[is.na(AllDataSATax$genus),]


#########GLOBAL DATASET###########
taxonGlobal=classification(GlobalNoTax$Species, db='gbif')

# now replace the names that are missing in the main object, so we can complete it
species=GlobalNoTax$Species
newTaxaGlobal=GlobalNoTax
for (i in c(1:314,316:length(species))) {
  newTaxaGlobal[newTaxaGlobal$Species==species[i],c(91:93)]<-taxonGlobal[[i]][c(6,5,4),1]
}

species=GlobalNoTax$Species
for (i in c(1:314,316:length(species))) {
  AllDataGlobalTax[AllDataGlobalTax$Species==species[i],c(91:93)]<-newTaxaGlobal[newTaxaGlobal$Species==species[i],c(91:93)]
}

#exclude all that is not land plants
ordersNLP=c("Acrosiphoniales","Bryopsidales","Cladophorales","Dasycladales","Ignatiales","Oltmansiellopsidales",
            "Trentepohliales","Ulotrichales","Ulvales","Oedogoniales","Chaetophorales","Chaetopeltidales",
            "Chlamydomonadales","Sphaeropleales","Chlorellales","Oocystaceae","Microthamniales","Trebouxiales",
            "Pyramimonadales","Prasinococcales","Palmophyllales","Zygnematales","Desmidiales","Charales",
            "Coleochaetales","Pseudoscourfieldiales","Pedinomonadales","Volvocales")

AllDataLandPlantsGlobal=AllDataGlobalTax[-which(AllDataGlobalTax$order %in% ordersNLP),]


#########AFRICA DATASET###########
taxonAfr=classification(AfrNoTax$Species, db='gbif')

# now replace the names that are missing in the main object, so we can complete it
species=AfrNoTax$Species
newTaxaAfr=AfrNoTax
for (i in 1:length(species)) {
  newTaxaAfr[newTaxaAfr$Species==species[i],c(91:93)]<-taxonAfr[[i]][c(6,5,4),1]
}

species=AfrNoTax$Species
for (i in 1:length(species)) {
  AllDataAfrTax[AllDataAfrTax$Species==species[i],c(91:93)]<-newTaxaAfr[newTaxaAfr$Species==species[i],c(91:93)]
}

#exclude all that is not land plants
ordersNLP=c("Acrosiphoniales","Bryopsidales","Cladophorales","Dasycladales","Ignatiales","Oltmansiellopsidales",
            "Trentepohliales","Ulotrichales","Ulvales","Oedogoniales","Chaetophorales","Chaetopeltidales",
            "Chlamydomonadales","Sphaeropleales","Chlorellales","Oocystaceae","Microthamniales","Trebouxiales",
            "Pyramimonadales","Prasinococcales","Palmophyllales","Zygnematales","Desmidiales","Charales",
            "Coleochaetales","Pseudoscourfieldiales","Pedinomonadales","Volvocales")

AllDataLandPlantsAfr=AllDataAfrTax[-which(AllDataAfrTax$order %in% ordersNLP),]

#########CENTRAL AMERICA DATASET###########

taxonCA=classification(CANoTax$name, db='gbif')

# now replace the names that are missing in the main object, so we can complete it
species=CANoTax$name
newTaxaCA=CANoTax
for (i in 1:length(species)) {
  tryCatch(newTaxaCA[newTaxaCA$name==species[i],c(91:93)]<-taxonCA[[i]][c(6,5,4),1], 
           error=function(e) NULL)
}

species=CANoTax$name
for (i in 1:length(species)) {
  AllDataCATax[AllDataCATax$name==species[i],c(91:93)]<-newTaxaCA[newTaxaCA$name==species[i],c(91:93)]
}

#exclude all that is not land plants
ordersNLP=c("Acrosiphoniales","Bryopsidales","Cladophorales","Dasycladales","Ignatiales","Oltmansiellopsidales",
            "Trentepohliales","Ulotrichales","Ulvales","Oedogoniales","Chaetophorales","Chaetopeltidales",
            "Chlamydomonadales","Sphaeropleales","Chlorellales","Oocystaceae","Microthamniales","Trebouxiales",
            "Pyramimonadales","Prasinococcales","Palmophyllales","Zygnematales","Desmidiales","Charales",
            "Coleochaetales","Pseudoscourfieldiales","Pedinomonadales","Volvocales")

AllDataLandPlantsCA=AllDataCATax[-which(AllDataCATax$order %in% ordersNLP),]


#########EUROPE DATASET###########
taxonEur=classification(EurNoTax$Species, db='gbif')
species=EurNoTax$Species
newTaxaEur=EurNoTax
for (i in c(1:257,259:330,332:451,453:1678,1680:2277)) {
  newTaxaEur[newTaxaEur$Species==species[i],c(91:93)]<-taxonEur[[i]][c(6,5,4),1]
}

species=EurNoTax$Species
for (i in c(1:257,259:330,332:451,453:1678,1680:2277)) {
  AllDataEurTax[AllDataEurTax$Species==species[i],c(91:93)]<-newTaxaEur[newTaxaEur$Species==species[i],c(91:93)]
}

#exclude all that is not land plants
ordersNLP=c("Acrosiphoniales","Bryopsidales","Cladophorales","Dasycladales","Ignatiales","Oltmansiellopsidales",
            "Trentepohliales","Ulotrichales","Ulvales","Oedogoniales","Chaetophorales","Chaetopeltidales",
            "Chlamydomonadales","Sphaeropleales","Chlorellales","Oocystaceae","Microthamniales","Trebouxiales",
            "Pyramimonadales","Prasinococcales","Palmophyllales","Zygnematales","Desmidiales","Charales",
            "Coleochaetales","Pseudoscourfieldiales","Pedinomonadales","Volvocales")

AllDataLandPlantsEur=AllDataEurTax[-which(AllDataEurTax$order %in% ordersNLP),]

#########NORTH AMERICA DATASET###########

taxonNAm=classification(NAmNoTax$Species, db='gbif')
species=NAmNoTax$Species
newTaxaNAm=NAmNoTax
for (i in 1:(length(species))) {
  newTaxaNAm[newTaxaNAm$Species==species[i],c(91:93)]<-taxonNAm[[i]][c(6,5,4),1]
}

species=NAmNoTax$Species
for (i in 1:length(species)) {
  AllDataNAmTax[AllDataNAmTax$Species==species[i],c(91:93)]<-newTaxaNAm[newTaxaNAm$Species==species[i],c(91:93)]
}

#exclude all that is not land plants
ordersNLP=c("Acrosiphoniales","Bryopsidales","Cladophorales","Dasycladales","Ignatiales","Oltmansiellopsidales",
            "Trentepohliales","Ulotrichales","Ulvales","Oedogoniales","Chaetophorales","Chaetopeltidales",
            "Chlamydomonadales","Sphaeropleales","Chlorellales","Oocystaceae","Microthamniales","Trebouxiales",
            "Pyramimonadales","Prasinococcales","Palmophyllales","Zygnematales","Desmidiales","Charales",
            "Coleochaetales","Pseudoscourfieldiales","Pedinomonadales","Volvocales")

AllDataLandPlantsNAm=AllDataNAmTax[-which(AllDataNAmTax$order %in% ordersNLP),]


#########ASIA DATASET###########
taxonAsia=classification(AsiaNoTax$name, db='gbif')

species=AsiaNoTax$name
newTaxaAsia=AsiaNoTax
for (i in 1:length(species)) {
  tryCatch(newTaxaAsia[newTaxaAsia$name==species[i],c(91:93)]<-taxonAsia[[i]][c(6,5,4),1], 
           error=function(e) NULL)
}

species=AsiaNoTax$name
for (i in 1:length(species)) {
  AllDataAsiaTax[AllDataAsiaTax$name==species[i],c(91:93)]<-newTaxaAsia[newTaxaAsia$name==species[i],c(91:93)]
}
#exclude all that is not land plants
ordersNLP=c("Acrosiphoniales","Bryopsidales","Cladophorales","Dasycladales","Ignatiales","Oltmansiellopsidales",
            "Trentepohliales","Ulotrichales","Ulvales","Oedogoniales","Chaetophorales","Chaetopeltidales",
            "Chlamydomonadales","Sphaeropleales","Chlorellales","Oocystaceae","Microthamniales","Trebouxiales",
            "Pyramimonadales","Prasinococcales","Palmophyllales","Zygnematales","Desmidiales","Charales",
            "Coleochaetales","Pseudoscourfieldiales","Pedinomonadales","Volvocales")

AllDataLandPlantsAsia=AllDataAsiaTax[-which(AllDataAsiaTax$order %in% ordersNLP),]

#########AUSTRALIA DATASET###########
taxonAus=classification(AusNoTax$name, db='gbif')

species=AusNoTax$name
newTaxaAus=AusNoTax
for (i in 1:length(species)) {
  tryCatch(newTaxaAus[newTaxaAus$name==species[i],c(91:93)]<-taxonAus[[i]][c(6,5,4),1], 
           error=function(e) NULL)
}

species=AusNoTax$name
for (i in 1:length(species)) {
  AllDataAusTax[AllDataAusTax$name==species[i],c(91:93)]<-newTaxaAus[newTaxaAus$name==species[i],c(91:93)]
}

#exclude all that is not land plants
ordersNLP=c("Acrosiphoniales","Bryopsidales","Cladophorales","Dasycladales","Ignatiales","Oltmansiellopsidales",
            "Trentepohliales","Ulotrichales","Ulvales","Oedogoniales","Chaetophorales","Chaetopeltidales",
            "Chlamydomonadales","Sphaeropleales","Chlorellales","Oocystaceae","Microthamniales","Trebouxiales",
            "Pyramimonadales","Prasinococcales","Palmophyllales","Zygnematales","Desmidiales","Charales",
            "Coleochaetales","Pseudoscourfieldiales","Pedinomonadales","Volvocales")

AllDataLandPlantsAus=AllDataAusTax[-which(AllDataAusTax$order %in% ordersNLP),]

#########SOUTH AMERICA DATASET###########
taxonSA=classification(SANoTax$name, db='gbif')

species=SANoTax$name
newTaxaSA=SANoTax
for (i in 1:length(species)) {
  tryCatch(newTaxaSA[newTaxaSA$name==species[i],c(91:93)]<-taxonSA[[i]][c(6,5,4),1], 
           error=function(e) NULL)
}

species=SANoTax$name
for (i in 1:length(species)) {
  AllDataSATax[AllDataSATax$name==species[i],c(91:93)]<-newTaxaSA[newTaxaSA$name==species[i],c(91:93)]
}

ordersNLP=c("Acrosiphoniales","Bryopsidales","Cladophorales","Dasycladales","Ignatiales","Oltmansiellopsidales",
            "Trentepohliales","Ulotrichales","Ulvales","Oedogoniales","Chaetophorales","Chaetopeltidales",
            "Chlamydomonadales","Sphaeropleales","Chlorellales","Oocystaceae","Microthamniales","Trebouxiales",
            "Pyramimonadales","Prasinococcales","Palmophyllales","Zygnematales","Desmidiales","Charales",
            "Coleochaetales","Pseudoscourfieldiales","Pedinomonadales","Volvocales")

AllDataLandPlantsSA=AllDataSATax[-which(AllDataSATax$order %in% ordersNLP),]


###WRITE ALL DATA
write.table(newTaxaGlobal,file="NATaxaGlobalNew.txt", sep=";")
write.table(newTaxaAfr,file="NATaxaAfrNew.txt", sep=";")
write.table(newTaxaEur,file="NATaxaEurNew.txt", sep=";")
write.table(newTaxaNAm,file="NATaxaNAmNew.txt", sep=";")
write.table(newTaxaAsia,file="NATaxaAsiaNew.txt", sep=";")
write.table(newTaxaAus,file="NATaxaAusNew.txt", sep=";")
write.table(newTaxaCA,file="NATaxaCANew.txt", sep=";")
write.table(newTaxaSA,file="NATaxaSANew.txt", sep=";")


write.table(AllDataGlobalTax,file="AllTaxaGlobalFullNew.txt", sep=";")
write.table(AllDataLandPlantsGlobal,file="AllTaxaGlobalLandPlantsNew.txt", sep=";")

write.table(AllDataAfrTax,file="AllTaxaAfricaFullNew.txt", sep=";")
write.table(AllDataLandPlantsAf,file="AllTaxaAfricaLandPlantsNew.txt", sep=";")

write.table(AllDataEurTax,file="AllTaxaEuropeFullNew.txt", sep=";")
write.table(AllDataLandPlantsEur,file="AllTaxaEuropeLandPlantsNew.txt", sep=";")

write.table(AllDataAsiaTax,file="AllTaxaAsiaFullNew.txt", sep=";")
write.table(AllDataLandPlantsAsia,file="AllTaxaAsiaLandPlantsNew.txt", sep=";")

write.table(AllDataAusTax,file="AllTaxaAusFullNew.txt", sep=";")
write.table(AllDataLandPlantsAus,file="AllTaxaAusLandPlantsNew.txt", sep=";")

write.table(AllDataCATax,file="AllTaxaCAFullNew.txt", sep=";")
write.table(AllDataLandPlantsCA,file="AllTaxaCALandPlantsNew.txt", sep=";")

write.table(AllDataSATax,file="AllTaxaSAFullNew.txt", sep=";")
write.table(AllDataLandPlantsSA,file="AllTaxaSALandPlantsNew.txt", sep=";")

write.table(AllDataNAmTax,file="AllTaxaNAmFullNew.txt", sep=";")
write.table(AllDataLandPlantsNAm,file="AllTaxaNAmLandPlantsNew.txt", sep=";")


######################################################
##### identify traits to keep and resample ###########
######################################################

setwd("E:/collaborations/tara/outputs")
AllDataGlobal=read.table(file="AllTaxaGlobalFullNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataAfr=read.table(file="AllTaxaAfricaFullNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataEur=read.table(file="AllTaxaEuropeFull.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataNAm=read.table(file="AllTaxaNAmericaFullNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataAsia=read.table(file="AllTaxaAsiaFull.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataAus=read.table(file="AllTaxaAusFullNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataCA=read.table(file="AllTaxaCAFullNew.txt",header=T, sep=";",row.names=NULL,fill=T)
AllDataSA=read.table(file="AllTaxaSALandPlantsNew.txt",header=T, sep=";",row.names=NULL,fill=T)

TraitsAfrFam=which(complete.cases(AllDataAfr[AllDataAfr$family==c("Fabaceae","Asteraceae","Rubiaceae","Orchidaceae","Poaceae",
                                                                  "Euphorbiaceae","Rubiaceae","Aizoaceae","Acanthaceae",
                                                                  "Malvaceae","Iridaceae","Apocynaceae","|"),c(53,59)]))
summary(AllDataAfr[TraitsAfrFam,52]) #summary per REd List class
summary(AllDataAfr[TraitsAfrFam,51]) #summary per dist (global/endemic)


TraitsEurFam=which(complete.cases(AllDataEurTax[AllDataEurTax$family==c("Fabaceae","Asteraceae","|"),c(3,12)]))
summary(AllDataEurTax[TraitsEurFam,2])

TraitsNAmFam=which(complete.cases(AllDataNAmTax[AllDataNAmTax$family==c("Fabaceae","Asteraceae","Orchidaceae",
                                                                        "Poaceae","Rubiaceae","|"),c(3,6,9)]))
summary(AllDataNAmTax[TraitsNAmFam,2])

NoNAAfr=which(complete.cases(AllDataAfr[,4:42]))
summary(AllDataAfr[NoNAAfr,3])
NoNAEur=which(complete.cases(AllDataEur[,4:42]))
summary(AllDataEur[NoNAEur,3])
NoNANAm=which(complete.cases(AllDataNAm[,4:42]))
summary(AllDataNAm[NoNANAm,3])

#selection of traits that maximizes number of represented IUCN categories with traits
NoNAAfrMax=which(complete.cases(AllDataAfr[,c(53,59)]))
NoNAEurMax=which(complete.cases(AllDataEur[,c(3,6,9,15)]))
NoNANAmMax=which(complete.cases(AllDataNAm[,c(3,6,9)]))
NoNAAsiaMax=which(complete.cases(AllDataAsia[,c(30,36,33)]))
NoNAAusMax=which(complete.cases(AllDataAus[,c(30,31)]))
NoNACAMax=which(complete.cases(AllDataCA[,c(30,36)]))
NoNASAMax=which(complete.cases(AllDataSA[,c(30,36,38)]))

write.table(AllDataNAm[NoNANAmMax,c(2,3,4,7,10)], file="NorthAmericaFullNoNA.txt", sep=',')
write.table(AllDataEur[NoNAEurMax,c(2,3,4,7,10,16)], file="EuropeFullNoNA.txt", sep=',')
write.table(AllDataAfr[NoNAAfrMax,c(2,3,4,7,10)], file="AfricaFullNoNANew.txt", sep=',')
write.table(AllDataAsia[NoNAAsiaMax,c(2,29,30,36,33)], file="AsiaFullNoNA.txt", sep=',')
write.table(AllDataAus[NoNAAusMax,c(2,29,30,31)], file="AusFullNoNA.txt", sep=',')
write.table(AllDataCA[NoNACAMax,c(2,29,30,36)], file="CAFullNoNA.txt", sep=',')
write.table(AllDataSA[NoNASAMax,c(2,29,30,36,38)], file="SAFullNoNA.txt", sep=',')


#calculate per family, how many species are actually represented
setwd("E:/collaborations/tara/outputs/PropPerFamily")

#GLOBAL DATASET
traits=colnames(AllDataGlobal[53:91])
families=levels(AllDataGlobal[,93])
GlobalProp=c()

for (j in 1:length(traits)){
  statisticsTrait1=as.data.frame(1,row.names="TEST")
  for (i in 1:length(families)){
    summaries=t(count(AllDataGlobal[AllDataGlobal$family==families[i],j+52]))
    sumOfSummaries=as.data.frame(sum(summaries[2,]))
    colnames(sumOfSummaries)<-'total' 
    colnames(summaries)=summaries[1,]
    summaries=summaries[2,, drop=F]
    summaries=as.data.frame(summaries)
    summaries=cbind(summaries,sumOfSummaries)
    colnames(summaries)[is.na(colnames(summaries))] <- 'NoData'
    statisticsTrait1=rbind.fill(statisticsTrait1,summaries)
  }
  
  statisticsTrait1=statisticsTrait1[2:(length(families)+1),] # keep only lines that have what we are interested in
  rownames(statisticsTrait1)<-families # associate family names to each line
  
  #now we calculate the proportions that have no values within each family
  newTable=c()
  for (i in 1:length(families)){
    valueProp=statisticsTrait1[i,colnames(statisticsTrait1)=="NoData"]/statisticsTrait1[i,colnames(statisticsTrait1)=="total"]
    newTable=rbind(newTable,valueProp)
  }
  colnames(newTable)<-traits[j] 
  rownames(newTable)<-families
  GlobalProp=cbind(GlobalProp,newTable)
}

#FinalTrait1=cbind(statisticsTrait1,newTable) #we put the two tables together
#write.table(FinalTrait1,file=paste(traits[j],".txt"),sep=";")




#AFRICA DATASET
traits=colnames(AllDataAfr[53:91])
families=levels(AllDataAfr[,93])
AfricaProp=c()

for (j in 1:length(traits)){
  statisticsTrait1=as.data.frame(1,row.names="TEST")
  for (i in 1:length(families)){
    summaries=t(count(AllDataAfr[AllDataAfr$family==families[i],j+52]))
    sumOfSummaries=as.data.frame(sum(summaries[2,]))
    colnames(sumOfSummaries)<-'total' 
    colnames(summaries)=summaries[1,]
    summaries=summaries[2,, drop=F]
    summaries=as.data.frame(summaries)
    summaries=cbind(summaries,sumOfSummaries)
    colnames(summaries)[is.na(colnames(summaries))] <- 'NoData'
    statisticsTrait1=rbind.fill(statisticsTrait1,summaries)
  }
  
  statisticsTrait1=statisticsTrait1[2:(length(families)+1),] # keep only lines that have what we are interested in
  rownames(statisticsTrait1)<-families # associate family names to each line
  
  #now we calculate the proportions that have no values within each family
  newTable=c()
  for (i in 1:length(families)){
    valueProp=statisticsTrait1[i,colnames(statisticsTrait1)=="NoData"]/statisticsTrait1[i,colnames(statisticsTrait1)=="total"]
    newTable=rbind(newTable,valueProp)
  }
  colnames(newTable)<-traits[j] 
  rownames(newTable)<-families
  AfricaProp=cbind(AfricaProp,newTable)
}

#FinalTrait1=cbind(statisticsTrait1,newTable) #we put the two tables together
#write.table(FinalTrait1,file=paste(traits[j],".txt"),sep=";")


#EUROPE DATASET
traits=colnames(AllDataEurTax[52:90])
families=levels(as.factor(AllDataEurTax[,92]))
EuropeProp=c()

for (j in 1:length(traits)){
  statisticsTrait1=as.data.frame(1,row.names="TEST")
  for (i in 1:length(families)){
    summaries=t(count(AllDataEurTax[AllDataEurTax$family==families[i],j+51]))
    sumOfSummaries=as.data.frame(sum(summaries[2,]))
    colnames(sumOfSummaries)<-'total' 
    #summaries[is.na(summaries)]<-"NoData"
    colnames(summaries)=summaries[1,]
    summaries=summaries[2,, drop=F]
    summaries=as.data.frame(summaries)
    summaries=cbind(summaries,sumOfSummaries)
    colnames(summaries)[is.na(colnames(summaries))] <- 'NoData'
    statisticsTrait1=rbind.fill(statisticsTrait1,summaries)
  }
  
  statisticsTrait1=statisticsTrait1[2:(length(families)+1),] # keep only lines that have what we are interested in
  rownames(statisticsTrait1)<-families # associate family names to each line
  
  #now we calculate the proportions that have values within each family
  newTable=c()
  for (i in 1:length(families)){
    valueProp=statisticsTrait1[i,colnames(statisticsTrait1)=="NoData"]/statisticsTrait1[i,colnames(statisticsTrait1)=="total"]
    newTable=rbind(newTable,valueProp)
  }
  colnames(newTable)<-traits[j] 
  rownames(newTable)<-families
  EuropeProp=cbind(EuropeProp,newTable)
}

#NORTH AMERICA DATASET
traits=colnames(AllDataNAmTax[52:90])
families=levels(as.factor(AllDataNAmTax[,92]))
NorthAmericaProp=c()

for (j in 1:length(traits)){
  statisticsTrait1=as.data.frame(1,row.names="TEST")
  for (i in 1:length(families)){
    summaries=t(count(AllDataNAmTax[AllDataNAmTax$family==families[i],j+51]))
    sumOfSummaries=as.data.frame(sum(summaries[2,]))
    colnames(sumOfSummaries)<-'total' 
    #summaries[is.na(summaries)]<-"NoData"
    colnames(summaries)=summaries[1,]
    summaries=summaries[2,, drop=F]
    summaries=as.data.frame(summaries)
    summaries=cbind(summaries,sumOfSummaries)
    colnames(summaries)[is.na(colnames(summaries))] <- 'NoData'
    statisticsTrait1=rbind.fill(statisticsTrait1,summaries)
  }
  
  statisticsTrait1=statisticsTrait1[2:(length(families)+1),] # keep only lines that have what we are interested in
  rownames(statisticsTrait1)<-families # associate family names to each line
  
  #now we calculate the proportions that have values within each family
  newTable=c()
  for (i in 1:length(families)){
    valueProp=statisticsTrait1[i,colnames(statisticsTrait1)=="NoData"]/statisticsTrait1[i,colnames(statisticsTrait1)=="total"]
    newTable=rbind(newTable,valueProp)
  }
  colnames(newTable)<-traits[j] 
  rownames(newTable)<-families
  NorthAmericaProp=cbind(NorthAmericaProp,newTable)
}
#  FinalTrait1=cbind(statisticsTrait1,newTable) #we put the two tables together
#  write.table(FinalTrait1,file=paste(traits[j],".txt"),sep=";")

#ASIA

traits=colnames(AllDataAsia[53:91])
families=levels(AllDataAsia[,93])
AsiaProp=c()

for (j in 1:length(traits)){
  statisticsTrait1=as.data.frame(1,row.names="TEST")
  for (i in 1:length(families)){
    summaries=t(count(AllDataAsia[AllDataAsia$family==families[i],j+52]))
    sumOfSummaries=as.data.frame(sum(summaries[2,]))
    colnames(sumOfSummaries)<-'total' 
    #summaries[is.na(summaries)]<-"NoData"
    colnames(summaries)=summaries[1,]
    summaries=summaries[2,, drop=F]
    summaries=as.data.frame(summaries)
    summaries=cbind(summaries,sumOfSummaries)
    colnames(summaries)[is.na(colnames(summaries))] <- 'NoData'
    statisticsTrait1=rbind.fill(statisticsTrait1,summaries)
  }
  
  statisticsTrait1=statisticsTrait1[2:(length(families)+1),] # keep only lines that have what we are interested in
  rownames(statisticsTrait1)<-families # associate family names to each line
  
  #now we calculate the proportions that have values within each family
  newTable=c()
  for (i in 1:length(families)){
    valueProp=statisticsTrait1[i,colnames(statisticsTrait1)=="NoData"]/statisticsTrait1[i,colnames(statisticsTrait1)=="total"]
    newTable=rbind(newTable,valueProp)
  }
  colnames(newTable)<-traits[j] 
  rownames(newTable)<-families
  AsiaProp=cbind(AsiaProp,newTable)
}

#AUSTRALIA
traits=colnames(AllDataAus[30:68])
families=levels(AllDataAus[,70])
AusProp=c()

for (j in 1:length(traits)){
  statisticsTrait1=as.data.frame(1,row.names="TEST")
  for (i in 1:length(families)){
    summaries=t(count(AllDataAus[AllDataAus$family==families[i],j+29]))
    sumOfSummaries=as.data.frame(sum(summaries[2,]))
    colnames(sumOfSummaries)<-'total' 
    #summaries[is.na(summaries)]<-"NoData"
    colnames(summaries)=summaries[1,]
    summaries=summaries[2,, drop=F]
    summaries=as.data.frame(summaries)
    summaries=cbind(summaries,sumOfSummaries)
    colnames(summaries)[is.na(colnames(summaries))] <- 'NoData'
    statisticsTrait1=rbind.fill(statisticsTrait1,summaries)
  }
  
  statisticsTrait1=statisticsTrait1[2:(length(families)+1),] # keep only lines that have what we are interested in
  rownames(statisticsTrait1)<-families # associate family names to each line
  
  #now we calculate the proportions that have values within each family
  newTable=c()
  for (i in 1:length(families)){
    valueProp=statisticsTrait1[i,colnames(statisticsTrait1)=="NoData"]/statisticsTrait1[i,colnames(statisticsTrait1)=="total"]
    newTable=rbind(newTable,valueProp)
  }
  colnames(newTable)<-traits[j] 
  rownames(newTable)<-families
  AusProp=cbind(AusProp,newTable)
}

#CENTRAL AMERICA
traits=colnames(AllDataCA[53:91])
families=levels(AllDataCA[,93])
CAProp=c()

for (j in 1:length(traits)){
  statisticsTrait1=as.data.frame(1,row.names="TEST")
  for (i in 1:length(families)){
    summaries=t(count(AllDataCA[AllDataCA$family==families[i],j+52]))
    sumOfSummaries=as.data.frame(sum(summaries[2,]))
    colnames(sumOfSummaries)<-'total' 
    #summaries[is.na(summaries)]<-"NoData"
    colnames(summaries)=summaries[1,]
    summaries=summaries[2,, drop=F]
    summaries=as.data.frame(summaries)
    summaries=cbind(summaries,sumOfSummaries)
    colnames(summaries)[is.na(colnames(summaries))] <- 'NoData'
    statisticsTrait1=rbind.fill(statisticsTrait1,summaries)
  }
  
  statisticsTrait1=statisticsTrait1[2:(length(families)+1),] # keep only lines that have what we are interested in
  rownames(statisticsTrait1)<-families # associate family names to each line
  
  #now we calculate the proportions that have values within each family
  newTable=c()
  for (i in 1:length(families)){
    valueProp=statisticsTrait1[i,colnames(statisticsTrait1)=="NoData"]/statisticsTrait1[i,colnames(statisticsTrait1)=="total"]
    newTable=rbind(newTable,valueProp)
  }
  colnames(newTable)<-traits[j] 
  rownames(newTable)<-families
  CAProp=cbind(CAProp,newTable)
}

#SOUTH AMERICA
traits=colnames(newTaxaSA[53:91])
families=levels(as.factor(newTaxaSA[,93]))
SAProp=c()

for (j in 1:length(traits)){
  statisticsTrait1=as.data.frame(1,row.names="TEST")
  for (i in 1:length(families)){
    summaries=t(count(newTaxaSA[newTaxaSA$family==families[i],j+52]))
    sumOfSummaries=as.data.frame(sum(summaries[2,]))
    colnames(sumOfSummaries)<-'total' 
    #summaries[is.na(summaries)]<-"NoData"
    colnames(summaries)=summaries[1,]
    summaries=summaries[2,, drop=F]
    summaries=as.data.frame(summaries)
    summaries=cbind(summaries,sumOfSummaries)
    colnames(summaries)[is.na(colnames(summaries))] <- 'NoData'
    statisticsTrait1=rbind.fill(statisticsTrait1,summaries)
  }
  
  statisticsTrait1=statisticsTrait1[2:(length(families)+1),] # keep only lines that have what we are interested in
  rownames(statisticsTrait1)<-families # associate family names to each line
  
  #now we calculate the proportions that have no values within each family
  newTable=c()
  for (i in 1:length(families)){
    valueProp=statisticsTrait1[i,colnames(statisticsTrait1)=="NoData"]/statisticsTrait1[i,colnames(statisticsTrait1)=="total"]
    newTable=rbind(newTable,valueProp)
  }
  colnames(newTable)<-traits[j] 
  rownames(newTable)<-families
  SAProp=cbind(SAProp,newTable)
}


write.table(GlobalProp,file="GlobalProportions.txt", sep=";")
write.table(AfricaProp,file="AfricaProportions.txt", sep=";")
write.table(EuropeProp,file="EuropeProportions.txt", sep=";")
write.table(NorthAmericaProp,file="NAmericaProportions.txt", sep=";")
write.table(AsiaProp,file="AsiaProportions.txt", sep=";")
write.table(AusProp,file="AusProportions.txt", sep=";")
write.table(CAProp,file="CAProportions.txt", sep=";")
write.table(SAProp,file="SAProportions.txt", sep=";")


#read files
setwd("./PropPerFamily")
AfricaProp=read.table(file="AfricaProportions.txt", sep=";", header=T)
EuropeProp=read.table(file="EuropeProportions.txt", sep=";", header = T)
NorthAmericaProp=read.table(file="NAmericaProportions.txt", sep=";", header=T)
AsiaProp=read.table(file="AsiaProportions.txt", sep=";", header=T)
AusProp=read.table(file="AusProportions.txt", sep=";", header=T)
CAProp=read.table(file="CAProportions.txt", sep=";", header=T)
SAProp=read.table(file="SAProportions.txt", sep=";", header=T)

values0.5Af=AfricaProp[(AfricaProp[,1:39]<0.5),]
values0.5Eur=EuropeProp[(EuropeProp[,1:36]<0.5),]
values0.5NA=NorthAmericaProp[(NorthAmericaProp[,1:36]<0.5),]
values0.5Asia=AsiaProp[(AsiaProp[,1:39]<0.5),]
values0.5Aus=AusProp[(AusProp[,1:39]<0.5),]
values0.5CA=CAProp[(CAProp[,1:39]<0.5),]
values0.5SA=SAProp[(SAProp[,1:39]<0.5),]


summary(AllDataAfr[AllDataAfr$family%in%c(row.names(values0.5Af),"|"),3])
summary(AllDataEur[AllDataEur$family%in%c(row.names(values0.5Eur),"|"),3])
summary(AllDataNAm[AllDataNAm$family%in%c(row.names(values0.5NA),"|"),3])


