#######################
#PLOT PROBS

library(raster)
r<-raster()

africa<-merge(africa_gps_clip, Africa_LCSampEnd_avg_probs, by.x="species", by.y="na_data.name")
africa_p<-africa[,c(3,2,5)]
africa_p<-unique(africa_p)

asia<-merge(Asia_gps_clip, Asia_LCSamp_avg_probs, by.x="species", by.y="na_data.name")
asia_p<-asia[,c(3,2,5)]
asia_p<-unique(asia_p)

names(Australia_gps_clip)<-c("species","decimalLat","decimalLon")
australia<-merge(Australia_gps_clip, Australia_LCSampEnd_avg_probs, by.x="species", by.y="na_data.name")
australia_p<-australia[,c(3,2,5)]
australia_p<-unique(australia_p)

ca<-merge(CA_gps_clip, CA_LCSamp_avg_probs, by.x="species", by.y="na_data.name")
ca_p<-ca[,c(3,2,5)]
ca_p<-unique(ca_p)

europe<-merge(Europe_gps_clip, Europe_LCSampEnd_avg_probs, by.x="species", by.y="na_data.name")
europe_p<-europe[,c(3,2,5)]
europe_p<-unique(europe_p)

na<-merge(NA_gps_clip, NA_LCSampEnd_avg_probs, by.x="species", by.y="na_data.name")
na_p<-na[,c(3,2,5)]
na_p<-unique(na_p)

sa<-merge(SA_gps_clip, SA_LCSampEnd_avg_probs, by.x="species", by.y="na_data.name")
sa_p<-sa[,c(3,2,5)]
sa_p<-unique(sa_p)


all<-rbind(africa_p, asia_p, australia_p, ca_p, europe_p, na_p, sa_p)

decimalLon<-all$decimalLon
decimalLat<-all$decimalLat


coordinates(all)<- ~decimalLon + decimalLat
all_r<-rasterize(all, r, 'NoLC', fun=mean)

writeRaster(all_r, file="PredictionsEnd.tif")


#for individual cont maps
decimalLon<-ca_p$decimalLon
decimalLat<-ca_p$decimalLat

coordinates(ca_p)<- ~decimalLon + decimalLat
ca_r<-rasterize(ca_p, r, 'NoLC', fun=mean)

writeRaster(ca_r, file="Predictions_CA.tif")

#

decimalLon<-asia_p$decimalLon
decimalLat<-asia_p$decimalLat

coordinates(asia_p)<- ~decimalLon + decimalLat
asia_r<-rasterize(asia_p, r, 'NoLC', fun=mean)

writeRaster(asia_r, file="Predictions_Asia.tif")

#global
global<-merge(all_gps_clip, global_LCSamp_avg_probs, by.x="species", by.y="na_data.name")
global_p<-global[,c(3,2,5)]
global_p<-unique(global_p)

decimalLon<-global_p$decimalLon
decimalLat<-global_p$decimalLat

coordinates(global_p)<- ~decimalLon + decimalLat
global_r<-rasterize(global_p, r, 'NoLC', fun=mean)

writeRaster(global_r, file="PredictionsGlobal.tif")


###MORPHO
AfricaAllPredictions<-read.csv("AfricaAllPredictionsMorphoSpatialNew.csv")
af<-AfricaAllPredictions[,c(1,7)]
af<-na.omit(af)
africa<-merge(africa_gps_clip, af, by.x="species", by.y="Species")
africa<-africa[,c(3,2,4)]
africa<-unique(africa)

AsiaAllPredictions<-read.csv("AsiaAllMorphoSpatialPredictionsNew.csv")
as<-AsiaAllPredictions[,c(1,7)]
as<-na.omit(as)
asia<-merge(Asia_gps_clip, as, by.x="species", by.y="Species")
asia<-asia[,c(3,2,4)]
asia<-unique(asia)

names(Australia_gps_clip)<-c("species","decimalLat","decimalLon")
AusAllPredictions<-read.csv("AusAllMorphoSpatialPredictionsNew.csv")
au<-AusAllPredictions[,c(1,7)]
au<-na.omit(au)
australia<-merge(Australia_gps_clip, au, by.x="species", by.y="Species")
australia<-australia[,c(3,2,4)]
australia<-unique(australia)

CAAllPredictions<-read.csv("CAAllMorphoSpatialPredictionsNew.csv")
ca<-CAAllPredictions[,c(1,7)]
ca<-na.omit(ca)
CA<-merge(CA_gps_clip, ca, by.x="species", by.y="Species")
CA<-CA[,c(3,2,4)]
CA<-unique(CA)

EuropeAllPredictions<-read.csv("EuropeMorphoSpatialAllPredictionsNew.csv")
eu<-EuropeAllPredictions[,c(1,7)]
eu<-na.omit(eu)
europe<-merge(Europe_gps_clip, eu, by.x="species", by.y="Species")
europe<-europe[,c(3,2,4)]
europe<-unique(europe)

NAmerAllPredictions<-read.csv("NAmerAllMorphoSpatialPredictionsNew.csv")
na<-NAmerAllPredictions[,c(1,7)]
na<-na.omit(na)
nam<-merge(NA_gps_clip, na, by.x="species", by.y="Species")
nam<-nam[,c(3,2,4)]
nam<-unique(nam)

SAAllPredictions<-read.csv("SAAllMorphoSpatialPredictions.csv")
sa<-SAAllPredictions[,c(1,7)]
sa<-na.omit(sa)
sam<-merge(SA_gps_clip, sa, by.x="species", by.y="Species")
sam<-sam[,c(3,2,4)]
sam<-unique(sam)

names(africa)<-c("decimalLon","decimalLat", "NoLC")
names(asia)<-c("decimalLon","decimalLat", "NoLC")
names(australia)<-c("decimalLon","decimalLat", "NoLC")
names(CA)<-c("decimalLon","decimalLat", "NoLC")
names(europe)<-c("decimalLon","decimalLat", "NoLC")
names(nam)<-c("decimalLon","decimalLat", "NoLC")
names(sam)<-c("decimalLon","decimalLat", "NoLC")

all<-rbind(africa, asia, australia, CA, europe, nam, sam)

decimalLon<-all$decimalLon
decimalLat<-all$decimalLat

coordinates(all)<- ~decimalLon + decimalLat
all_r<-rasterize(all, r, 'NoLC', fun=mean)

writeRaster(all_r, file="Predictions_morpho_end.tif")

#GLOBAL
###MORPHO
global<-read.csv("GlobalAllPredictionsMorphoSpatialNew.csv")
global<-global[,c(1,3)]
global<-na.omit(global)
global<-merge(all_gps_clip, global, by.x="species", by.y="Species")
global_p<-global[,c(3,2,4)]
global_p<-unique(global_p)

decimalLon<-global_p$decimalLon
decimalLat<-global_p$decimalLat

coordinates(global_p)<- ~decimalLon + decimalLat
global_r<-rasterize(global_p, r, 'NoLCSpatMorDown', fun=mean)

writeRaster(global_r, file="PredictionsGlobalMorph.tif")


#########################
#PLOT no_LC

library(raster)
r<-raster()

#africa
africa<-Africa_data[,c(1,51)]
africa<-na.omit(africa)
africa<-africa[africa$Red.List.status!="LC",]

af<-merge(africa, africa_gps_clip, by.x="name", by.y="species")
af<-af[,c(4,3)]

#asia
asia<-Asia_data[,c(1,51)]
asia<-na.omit(asia)
asia<-asia[asia$Red.List.status!="LC",]

as<-merge(asia, Asia_gps_clip, by.x="name", by.y="species")
as<-as[,c(4,3)]

#australia
names(Australia_gps_clip)<-c("species","decimalLat","decimalLon")
australia<-Australia_data[,c(1,51)]
australia<-na.omit(australia)
australia<-australia[australia$Red.List.status!="LC",]

au<-merge(australia, Australia_gps_clip, by.x="name", by.y="species")
au<-au[,c(4,3)]

#CA
CA<-CA_data[,c(1,51)]
CA<-na.omit(CA)
CA<-CA[CA$Red.List.status!="LC",]

ca<-merge(CA, CA_gps_clip, by.x="name", by.y="species")
ca<-ca[,c(4,3)]

#europe
europe<-Europe_data[,c(1,51)]
europe<-na.omit(europe)
europe<-europe[europe$Red.List.status!="LC",]

eu<-merge(europe, Europe_gps_clip, by.x="name", by.y="species")
eu<-eu[,c(4,3)]

#na
nam<-NA_data[,c(1,51)]
nam<-na.omit(nam)
nam<-nam[nam$Red.List.status!="LC",]

na<-merge(nam, NA_gps_clip, by.x="name", by.y="species")
na<-na[,c(4,3)]

#SA
sam<-SA_data[,c(1,51)]
sam<-na.omit(sam)
sam<-sam[sam$Red.List.status!="LC",]

sa<-merge(sam, SA_gps_clip, by.x="name", by.y="species")
sa<-sa[,c(4,3)]

all<-rbind(af,as,au,ca,eu,na,sa)

all_r<-rasterize(all, r, fun=function(x,...)length(log(x)))
plot(all_r)
writeRaster(all_r, file="NoLC_IUCN.tif")


#MORPHO

#africa
africa<-read.csv("LandPlantsAfricaNoNANew.txt", sep=";")
africa<-africa[,c(1,52)]
names(africa)<-c("species", "rls")
africa<-na.omit(africa)
africa<-africa[africa$rls!="LC",]
af<-merge(africa, africa_gps_clip, by.x="species", by.y="species")
af<-af[,c(4,3)]

#asia
asia<-read.csv("LandPlantsAsiaNoNANew.txt", sep=";")
asia<-asia[,c(1,52)]
names(asia)<-c("species", "rls")
asia<-na.omit(asia)
asia<-asia[asia$rls!="LC",]
as<-merge(asia, Asia_gps_clip, by.x="species", by.y="species")
as<-as[,c(4,3)]

#australia
australia<-read.csv("LandPlantsAusNoNANew.txt", sep=";")
australia<-australia[,c(1,51)]
names(australia)<-c("species", "rls")
australia<-na.omit(australia)
australia<-australia[australia$rls!="LC",]
names(Australia_gps_clip)<-c("species","decimalLat","decimalLon")
au<-merge(australia, Australia_gps_clip, by.x="species", by.y="species")
au<-au[,c(4,3)]

#ca
ca<-read.csv("LandPlantsCANoNANew.txt", sep=";")
ca<-ca[,c(1,52)]
names(ca)<-c("species", "rls")
ca<-na.omit(ca)
ca<-ca[ca$rls!="LC",]
ca<-merge(ca, CA_gps_clip, by.x="species", by.y="species")
ca<-ca[,c(4,3)]

#europe
europe<-read.csv("LandPlantsEuropeNoNANew.txt", sep=";")
europe<-europe[,c(1,53)]
names(europe)<-c("species", "rls")
europe<-na.omit(europe)
europe<-europe[europe$rls!="LC",]
eu<-merge(europe, Europe_gps_clip, by.x="species", by.y="species")
eu<-eu[,c(4,3)]

#na
na<-read.csv("LandPlantsNamNoNANew.txt", sep=";")
na<-na[,c(1,53)]
names(na)<-c("species", "rls")
na<-na.omit(na)
na<-na[na$rls!="LC",]
na<-merge(na, NA_gps_clip, by.x="species", by.y="species")
na<-na[,c(4,3)]

#sa
sa<-read.csv("LandPlantsSANoNANew.txt", sep=";")
sa<-sa[,c(1,52)]
names(sa)<-c("species", "rls")
sa<-na.omit(sa)
sa<-sa[sa$rls!="LC",]
sa<-merge(sa, SA_gps_clip, by.x="species", by.y="species")
sa<-sa[,c(4,3)]

all<-rbind(af,as,au,ca,eu,na,sa)

all_r<-rasterize(all, r, fun=function(x,...)length(log(x)))
plot(all_r)
writeRaster(all_r, file="Morpho_NoLC.tif")