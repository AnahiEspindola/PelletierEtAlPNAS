#read results so as to compare among treatments and datasets

############GLOBAL###################
####################################
#####################################
setwd("E:/collaborations/tara/outputs/RFResults/Global/noLivingSp")
list.files()
memory.limit(size=50000)

#read error rates
GlobalSpatMor7ImputedError=read.table("FullGlobalNoLCvLCErrorRates20.csv", header=T, 
                                      row.names=NULL, stringsAsFactors=F, sep=",")
GlobalSpatMor30ImputedError=read.table("ResampleGlobalComplete40NoLCvLCErrorRates.csv", header=T, 
                                       row.names=NULL,stringsAsFactors=F, sep=",")

GlobalSpatMor7ImputedErrorEndem=read.table("FullGlobalNoLCvLCErrorRates28Endem.csv", header=T, 
                                           row.names=NULL, stringsAsFactors=F, sep=",")
GlobalSpatMor30ImputedErrorEndem=read.table("ResampleGlobalComplete28EndemNoLCvLCErrorRates.csv", header=T, 
                                            row.names=NULL,stringsAsFactors=F, sep=",")

#read predictions of NA for the trhee datasets and sampling types
#reads morpho+spatial results
GlobalSpatMorDownPred=read.table("PredictNAData_confusionTableFullNoLCvLCGlobal20.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
GlobalLCAve=aggregate( LC ~ Species, GlobalSpatMorDownPred, mean )
GlobalNoLCAve=aggregate( NoLC ~ Species, GlobalSpatMorDownPred, mean )
GlobalAverageSpatMorDownPred=merge(GlobalLCAve,GlobalNoLCAve,by="Species")

GlobalSpatMorResamPred=read.table("PredictNAData_confusionTableResampleCompleteGlobal40NoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
GlobalLCAve=aggregate( LC ~ Species, GlobalSpatMorResamPred, mean )
GlobalNoLCAve=aggregate( NoLC ~ Species, GlobalSpatMorResamPred, mean )
GlobalAverageSpatMorResamPred=merge(GlobalLCAve,GlobalNoLCAve,by="Species")


#reads spatial results
#GlobalSpatialFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/", header=T,
#                             row.names=NULL, stringsAsFactors = F, sep=",")
GlobalSpatialFullDownsampled=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/global_LCSamp_avg_probs.csv", header=T,
                                        row.names=NULL, stringsAsFactors = F, sep=",")

# GlobalSpatialCRFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_CRFull_pred.csv", header=T,
#                                row.names=NULL, stringsAsFactors = F, sep=",")
# GlobalSpatialCRFullDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_CRSamp_avg_probs.csv", header=T,
#                                          row.names=NULL, stringsAsFactors = F, sep=",")
# 
# GlobalSpatialLCFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_LCFull_pred.csv", header=T,
#                                row.names=NULL, stringsAsFactors = F, sep=",")
# GlobalSpatialLCFullDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_LCSamp_avg_probs.csv", header=T,
#                                          row.names=NULL, stringsAsFactors = F, sep=",")
# 
# GlobalSpatialLC90Full=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_LCrm9_pred.csv", header=T,
#                                  row.names=NULL, stringsAsFactors = F, sep=",")
# GlobalSpatialLC80Full=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_LCrm8_pred.csv", header=T,
#                                  row.names=NULL, stringsAsFactors = F, sep=",")



#now link all the results of all approaches
GlobalAllPredictions=merge(GlobalAverageSpatMorDownPred,GlobalAverageSpatMorResamPred,by="Species", all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialFullDownsampled,by.x="Species", by.y="na_data.name", all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatDown","NoLCSpatDown")


write.table(GlobalAllPredictions,file="GlobalAllPredictionsMorphoSpatialNew.csv", sep=",")

#now calculate number predicted as one or the other with high probability
nrow(Global[Global$LCSpatMor7>0.8,])
length(GlobalAllPredictions[GlobalAllPredictions$NoLCSpatMor7>0.8,])


############AFRICA###################
####################################
#####################################
setwd("E:/collaborations/tara/outputs/RFResults/Global")
list.files()
memory.limit(size=50000)

#read error rates
GlobalSpatMor7ImputedError=read.table("FullGlobalNoLCvLCErrorRates20.csv", header=T, 
                                      row.names=NULL, stringsAsFactors=F, sep=",")
GlobalSpatMor30ImputedError=read.table("ResampleGlobalComplete40NoLCvLCErrorRates.csv", header=T, 
                                       row.names=NULL,stringsAsFactors=F, sep=",")

GlobalSpatMor7ImputedErrorEndem=read.table("FullGlobalNoLCvLCErrorRates28Endem.csv", header=T, 
                                           row.names=NULL, stringsAsFactors=F, sep=",")
GlobalSpatMor30ImputedErrorEndem=read.table("ResampleGlobalComplete28EndemNoLCvLCErrorRates.csv", header=T, 
                                            row.names=NULL,stringsAsFactors=F, sep=",")

#read predictions of NA for the trhee datasets and sampling types
#reads morpho+spatial results
GlobalSpatMorDownPred=read.table("PredictNAData_confusionTableFullNoLCvLCGlobal20.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
GlobalLCAve=aggregate( LC ~ Species, GlobalSpatMorDownPred, mean )
GlobalNoLCAve=aggregate( NoLC ~ Species, GlobalSpatMorDownPred, mean )
GlobalAverageSpatMorDownPred=merge(GlobalLCAve,GlobalNoLCAve,by="Species")

GlobalSpatMorResamPred=read.table("PredictNAData_confusionTableResampleCompleteGlobal40NoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
GlobalLCAve=aggregate( LC ~ Species, GlobalSpatMorResamPred, mean )
GlobalNoLCAve=aggregate( NoLC ~ Species, GlobalSpatMorResamPred, mean )
GlobalAverageSpatMorResamPred=merge(GlobalLCAve,GlobalNoLCAve,by="Species")


#reads spatial results
GlobalSpatialFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_AllFull_pred.csv", header=T,
                             row.names=NULL, stringsAsFactors = F, sep=",")
GlobalSpatialFullDownsampled=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_AllSamp_avg_probs.csv", header=T,
                                        row.names=NULL, stringsAsFactors = F, sep=",")

GlobalSpatialCRFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_CRFull_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
GlobalSpatialCRFullDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_CRSamp_avg_probs.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

GlobalSpatialLCFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_LCFull_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
GlobalSpatialLCFullDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_LCSamp_avg_probs.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

GlobalSpatialLC90Full=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_LCrm9_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")
GlobalSpatialLC80Full=read.table("E:/collaborations/tara/outputs/Spatial/New/Global/Global_LCrm8_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")



#now link all the results of all approaches
GlobalAllPredictions=merge(GlobalAverageSpatMorDownPred,GlobalAverageSpatMorResamPred,by="Species", all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalAverageSpatMorDownEndemPred,by="Species",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalAverageSpatMorResamEndemPred,by="Species",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem")

GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialCRFull,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialCRFullDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialLC90Full,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialLC80Full,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80")

GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialFull,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull")

GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialFullDownsampled,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown")

GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialCRFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialCRFullDownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialLC90FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialLC80FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End")

GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd")

GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialFullDownsampledEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialLCFull,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd",
                                  "SpatFullLC","SpatFullNoLC")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialLCFullDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC")

GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialLCFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                  "SpatFullEndLC","SpatFullEndNoLC")
GlobalAllPredictions=merge(GlobalAllPredictions,GlobalSpatialLCFullDownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(GlobalAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                  "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC")

write.table(GlobalAllPredictions,file="GlobalAllPredictionsMorphoSpatialNew.csv", sep=",")

#now calculate number predicted as one or the other with high probability
nrow(Global[Global$LCSpatMor7>0.8,])
length(GlobalAllPredictions[GlobalAllPredictions$NoLCSpatMor7>0.8,])




############AFRICA###################
####################################
#####################################
setwd("E:/collaborations/tara/outputs/RFResults/Africa/noLivingSp")
list.files()
memory.limit(size=50000)

#read error rates
AfricaSpatMor7ImputedError=read.table("FullAfrNoLCvLCErrorRates42.csv", header=T, 
                                      row.names=NULL, stringsAsFactors=F, sep=",")
AfricaSpatMor30ImputedError=read.table("ResampleAfrComplete84NoLCvLCErrorRates.csv", header=T, 
                                       row.names=NULL,stringsAsFactors=F, sep=",")

AfricaSpatMor7ImputedErrorEndem=read.table("FullAfrNoLCvLCErrorRates28Endem.csv", header=T, 
                                      row.names=NULL, stringsAsFactors=F, sep=",")
AfricaSpatMor30ImputedErrorEndem=read.table("ResampleAfrComplete28EndemNoLCvLCErrorRates.csv", header=T, 
                                       row.names=NULL,stringsAsFactors=F, sep=",")

#read predictions of NA for the trhee datasets and sampling types
#reads morpho+spatial results
AfricaSpatMorDownPred=read.table("PredictNAData_confusionTableFullNoLCvLCAfr42.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
AfricaLCAve=aggregate( LC ~ Species, AfricaSpatMorDownPred, mean )
AfricaNoLCAve=aggregate( NoLC ~ Species, AfricaSpatMorDownPred, mean )
AfricaAverageSpatMorDownPred=merge(AfricaLCAve,AfricaNoLCAve,by="Species")

AfricaSpatMorResamPred=read.table("PredictNAData_confusionTableResampleCompleteAfr84NoLCvLC.csv", header=T, row.names = NULL,
                               stringsAsFactors=F, sep=",")
AfricaLCAve=aggregate( LC ~ Species, AfricaSpatMorResamPred, mean )
AfricaNoLCAve=aggregate( NoLC ~ Species, AfricaSpatMorResamPred, mean )
AfricaAverageSpatMorResamPred=merge(AfricaLCAve,AfricaNoLCAve,by="Species")

AfricaSpatMorDownEndemPred=read.table("PredictNAData_confusionTableFullNoLCvLCAfr15Endem.csv", header=T, row.names = NULL,
                               stringsAsFactors=F, sep=",")
AfricaLCAve=aggregate( LC ~ Species, AfricaSpatMorDownEndemPred, mean )
AfricaNoLCAve=aggregate( NoLC ~ Species, AfricaSpatMorDownEndemPred, mean )
AfricaAverageSpatMorDownEndemPred=merge(AfricaLCAve,AfricaNoLCAve,by="Species")

AfricaSpatMorResamEndemPred=read.table("PredictNAData_confusionTableResampleCompleteAfr28EndemNoLCvLC.csv", header=T, row.names = NULL,
                               stringsAsFactors=F, sep=",")
AfricaLCAve=aggregate( LC ~ Species, AfricaSpatMorResamEndemPred, mean )
AfricaNoLCAve=aggregate( NoLC ~ Species, AfricaSpatMorResamEndemPred, mean )
AfricaAverageSpatMorResamEndemPred=merge(AfricaLCAve,AfricaNoLCAve,by="Species")

#reads spatial results
AfricaSpatialFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_AllFull_pred.csv", header=T,
                             row.names=NULL, stringsAsFactors = F, sep=",")
AfricaSpatialFullDownsampled=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_AllSamp_avg_probs.csv", header=T,
                                        row.names=NULL, stringsAsFactors = F, sep=",")

AfricaSpatialCRFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_CRFull_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
AfricaSpatialCRFullDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_CRSamp_avg_probs.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

AfricaSpatialLCFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_LCFull_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")
AfricaSpatialLCFullDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_LCSamp_avg_probs.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")

AfricaSpatialLC90Full=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_LCrm9_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")
AfricaSpatialLC80Full=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_LCrm8_pred.csv", header=T,
                                       row.names=NULL, stringsAsFactors = F, sep=",")

#reads spatial endemic results
AfricaSpatialFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_AllFullEnd_pred.csv", header=T,
                             row.names=NULL, stringsAsFactors = F, sep=",")
AfricaSpatialFullDownsampledEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_AllSampEnd_avg_probs.csv", header=T,
                                        row.names=NULL, stringsAsFactors = F, sep=",")

AfricaSpatialCRFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_CRFullEnd_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
AfricaSpatialCRFullDownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_CRSampEnd_avg_probs.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

AfricaSpatialLCFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_LCFullEnd_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
AfricaSpatialLCFullDownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_LCSampEnd_avg_probs.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

AfricaSpatialLC90FullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_LCrm9End_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")
AfricaSpatialLC80FullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Africa/Africa_LCrm8End_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")



#now link all the results of all approaches
AfricaAllPredictions=merge(AfricaAverageSpatMorDownPred,AfricaAverageSpatMorResamPred,by="Species", all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaAverageSpatMorDownEndemPred,by="Species",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaAverageSpatMorResamEndemPred,by="Species",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem")

AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialCRFull,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialCRFullDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialLC90Full,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialLC80Full,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80")

AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialFull,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull")

AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialFullDownsampled,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown")

AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialCRFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialCRFullDownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialLC90FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialLC80FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End")

AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd")

AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialFullDownsampledEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialLCFull,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd",
                                  "SpatFullLC","SpatFullNoLC")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialLCFullDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC")

AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialLCFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                  "SpatFullEndLC","SpatFullEndNoLC")
AfricaAllPredictions=merge(AfricaAllPredictions,AfricaSpatialLCFullDownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AfricaAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                  "LCFull", "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullCREnd","SpatFullNoCREnd","SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End",
                                  "SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                                  "LCFullEnd", "NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                  "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC")

write.table(AfricaAllPredictions,file="AfricaAllPredictionsMorphoSpatialNew.csv", sep=",")

#now calculate number predicted as one or the other with high probability
nrow(Africa[Africa$LCSpatMor7>0.8,])
length(AfricaAllPredictions[AfricaAllPredictions$NoLCSpatMor7>0.8,])



############N AMERICA###############
####################################
#####################################
setwd("E:/collaborations/tara/outputs/RFResults/NAmerica/noLivingSp")
list.files()
memory.limit(size=50000)

#read error rates
NAmerSpatMor7ImputedError=read.table("DownsampleNamComplete17NoLCvLCErrorRates.csv", header=T, 
                                     row.names=NULL, stringsAsFactors=F, sep=",")
NAmerSpatMor30ImputedError=read.table("ResampleNAmComplete34NoLCvLCErrorRates.csv", header=T, 
                                      row.names=NULL,stringsAsFactors=F, sep=",")

NAmerSpatMor7ImputedError=read.table("DownsampleNamComplete5EndemNoLCvLCErrorRates.csv", header=T, 
                                     row.names=NULL, stringsAsFactors=F, sep=",")
NAmerSpatMor30ImputedError=read.table("ResampleNAmComplete37EndemNoLCvLCErrorRates.csv", header=T, 
                                      row.names=NULL,stringsAsFactors=F, sep=",")


#read predictions of NA for the trhee datasets and sampling types
#read morpho+ spatial predictions
NAmerSpatMorDownPred=read.table("PredictNAData_confusionTableNAmDownsampleComplete17NoLCvLC.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
NAmerLCAve=aggregate( LC ~ Species, NAmerSpatMorDownPred, mean )
NAmerNoLCAve=aggregate( NoLC ~ Species, NAmerSpatMorDownPred, mean )
NAmerAverageSpatMorDownPred=merge(NAmerLCAve,NAmerNoLCAve,by="Species")

NAmerSpatMorResamPred=read.table("PredictNAData_confusionTableResampleCompleteNAm34NoLCvLC.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
NAmerLCAve=aggregate( LC ~ Species, NAmerSpatMorResamPred, mean )
NAmerNoLCAve=aggregate( NoLC ~ Species, NAmerSpatMorResamPred, mean )
NAmerAverageSpatMorResamPred=merge(NAmerLCAve,NAmerNoLCAve,by="Species")

NAmerSpatMorDownEndemPred=read.table("PredictNAData_confusionTableNAmDownsampleComplete5EndemNoLCvLC.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
NAmerLCAve=aggregate( LC ~ Species, NAmerSpatMorDownEndemPred, mean )
NAmerNoLCAve=aggregate( NoLC ~ Species, NAmerSpatMorDownEndemPred, mean )
NAmerAverageSpatMorDownEndemPred=merge(NAmerLCAve,NAmerNoLCAve,by="Species")

NAmerSpatMorResamEndemPred=read.table("PredictNAData_confusionTableResampleCompleteNAm37EndemNoLCvLC.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
NAmerLCAve=aggregate( LC ~ Species, NAmerSpatMorResamEndemPred, mean )
NAmerNoLCAve=aggregate( NoLC ~ Species, NAmerSpatMorResamEndemPred, mean )
NAmerAverageSpatMorResamEndemPred=merge(NAmerLCAve,NAmerNoLCAve,by="Species")

#read spatial predictions
NAmerSpatialFull=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_LCFull_pred.csv", header=T,
                            row.names=NULL, stringsAsFactors = F, sep=",")
NAmerSpatialFullDownsampled=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_LCSamp_avg_probs.csv", header=T,
                                       row.names=NULL, stringsAsFactors = F, sep=",")

NAmerSpatialCRFull=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_CRFull_pred.csv", header=T,
                              row.names=NULL, stringsAsFactors = F, sep=",")
NAmerSpatialCRFullDown=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_CRSamp_avg_probs.txt", header=T,
                                  row.names=NULL, stringsAsFactors = F, sep=",")

NAmerSpatialLC90Full=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_LCrm8_pred.csv", header=T,
                                row.names=NULL, stringsAsFactors = F, sep=",")
NAmerSpatialLC80Downsample=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_LCrm9_pred.csv", header=T,
                                      row.names=NULL, stringsAsFactors = F, sep=",")

NAmerSpatialAllFull=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_AllFull_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
NAmerSpatialAllDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_AllSamp_avg_probs.csv", header=T,
                                     row.names=NULL, stringsAsFactors = F, sep=",")

#read spatial predictions for endemics
NAmerSpatialFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_LCFullEnd_pred.csv", header=T,
                            row.names=NULL, stringsAsFactors = F, sep=",")
NAmerSpatialFullDownsampledEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_LCSampEnd_avg_probs.csv", header=T,
                                       row.names=NULL, stringsAsFactors = F, sep=",")

NAmerSpatialCRFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_CRFullEnd_pred.csv", header=T,
                              row.names=NULL, stringsAsFactors = F, sep=",")
NAmerSpatialCRFullDownEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_CRSampEnd_avg_probs.csv", header=T,
                                  row.names=NULL, stringsAsFactors = F, sep=",")

NAmerSpatialLC90FullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_LCrm8End_pred.csv", header=T,
                                row.names=NULL, stringsAsFactors = F, sep=",")
NAmerSpatialLC80DownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_LCrm9End_pred.csv", header=T,
                                      row.names=NULL, stringsAsFactors = F, sep=",")

NAmerSpatialAllFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_AllFullEnd_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
NAmerSpatialAllDownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/NAmerica/NA_AllSampEnd_avg_probs.csv", header=T,
                                     row.names=NULL, stringsAsFactors = F, sep=",")



#now link all the results of all approaches

NAmerAllPredictions=merge(NAmerAverageSpatMorDownPred,NAmerAverageSpatMorResamPred,by="Species", all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerAverageSpatMorDownEndemPred,by="Species",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerAverageSpatMorResamEndemPred,by="Species",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialFull,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialFullDownsampled,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialCRFull,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialCRFullDown,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialLC90Full,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialLC80Downsample,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialAllFull,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialAllDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown","LCSpatFullEnd","NoLCSpatFullEnd")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialFullDownsampledEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown","LCSpatFullEnd","NoLCSpatFullEnd",
                                 "LCSpatFullDownEnd","NoLCSpatFullDownEnd")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialCRFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown","LCSpatFullEnd","NoLCSpatFullEnd",
                                 "LCSpatFullDownEnd","NoLCSpatFullDownEnd","SpatFullCREnd","SpatFullNoCREnd")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialCRFullDownEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown","LCSpatFullEnd","NoLCSpatFullEnd",
                                 "LCSpatFullDownEnd","NoLCSpatFullDownEnd","SpatFullCREnd","SpatFullNoCREnd","SpatFullCRDownEnd",
                                 "SpatFullNoCRDownEnd")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialLC90FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown","LCSpatFullEnd","NoLCSpatFullEnd",
                                 "LCSpatFullDownEnd","NoLCSpatFullDownEnd","SpatFullCREnd","SpatFullNoCREnd","SpatFullCRDownEnd",
                                 "SpatFullNoCRDownEnd","SpatLC90End","SpatnonLC90End")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialLC80DownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown","LCSpatFullEnd","NoLCSpatFullEnd",
                                 "LCSpatFullDownEnd","NoLCSpatFullDownEnd","SpatFullCREnd","SpatFullNoCREnd","SpatFullCRDownEnd",
                                 "SpatFullNoCRDownEnd","SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialAllFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown","LCSpatFullEnd","NoLCSpatFullEnd",
                                 "LCSpatFullDownEnd","NoLCSpatFullDownEnd","SpatFullCREnd","SpatFullNoCREnd","SpatFullCRDownEnd",
                                 "SpatFullNoCRDownEnd","SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End",
                                 "CRFullEnd","ENFullEnd","LCFullEnd","NTFullEnd","VUFullEnd")

NAmerAllPredictions=merge(NAmerAllPredictions,NAmerSpatialAllDownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(NAmerAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                 "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                 "LCSpatFull","NoLCSpatFull","LCSpatFullDown","NoLCSpatFullDown","SpatFullCR","SpatFullNoCR",
                                 "SpatFullCRDown","SpatFullNoCRDown","SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull",
                                 "NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown","LCSpatFullEnd","NoLCSpatFullEnd",
                                 "LCSpatFullDownEnd","NoLCSpatFullDownEnd","SpatFullCREnd","SpatFullNoCREnd","SpatFullCRDownEnd",
                                 "SpatFullNoCRDownEnd","SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End",
                                 "CRFullEnd","ENFullEnd","LCFullEnd","NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", 
                                 "NTDownEnd","VUDownEnd")

write.table(NAmerAllPredictions,file="NAmerAllMorphoSpatialPredictionsNew.csv", sep=",")

#now calculate number predicted as one or the other with high probability
nrow(Global[Global$LCSpatMor7>0.8,])
nrow(NAmerAverageSpatMorDownPred[NAmerAverageSpatMorDownPred$NoLC>0.8,])
nrow(NAmerAverageSpatMorResamPred[NAmerAverageSpatMorResamPred$NoLC>0.5,])



############ EUROPE ################
####################################
####################################
setwd("E:/collaborations/tara/outputs/RFResults/Europe")
list.files()
memory.limit(size=50000)

#read error rates
EuropeSpatMor38ImputedError=read.table("DownsampleEurComplete4NoLCvLCErrorRates.csv", header=T, 
                                       row.names=NULL, stringsAsFactors=F, sep=",")
EuropeSpatMor70ImputedError=read.table("ResampleCompleteEur8NoLCvLCErrorRates.csv", header=T, 
                                       row.names=NULL,stringsAsFactors=F, sep=",")

EuropeSpatMor38ImputedError=read.table("DownsampleEurComplete2EndemNoLCvLCErrorRates.csv", header=T, 
                                       row.names=NULL, stringsAsFactors=F, sep=",")
EuropeSpatMor70ImputedError=read.table("ResampleCompleteEur35EndemNoLCvLCErrorRates.csv", header=T, 
                                       row.names=NULL,stringsAsFactors=F, sep=",")

#read predictions of NA for the trhee datasets and sampling types
#read all morpho+spatial
EuropeSpatMorDownPred=read.table("PredictNAData_confusionTableEurDownsampleComplete4NoLCvLC.csv", header=T, row.names = NULL,
                               stringsAsFactors=F, sep=",")
EuropeLCAve=aggregate( LC ~ Species, EuropeSpatMorDownPred, mean )
EuropeNoLCAve=aggregate( NoLC ~ Species, EuropeSpatMorDownPred, mean )
EuropeAverageSpatMorDownPred=merge(EuropeLCAve,EuropeNoLCAve,by="Species")

EuropeSpatMorResamPred=read.table("PredictNAData_confusionTableEurResampleComplete8NoLCvLC.csv", header=T, row.names = NULL,
                               stringsAsFactors=F, sep=",")
EuropeLCAve=aggregate( LC ~ Species, EuropeSpatMorResamPred, mean )
EuropeNoLCAve=aggregate( NoLC ~ Species, EuropeSpatMorResamPred, mean )
EuropeAverageSpatMorResamPred=merge(EuropeLCAve,EuropeNoLCAve,by="Species")

EuropeSpatMorDownEndemPred=read.table("PredictNAData_confusionTableEurDownsampleComplete2EndemNoLCvLC.csv", header=T, row.names = NULL,
                               stringsAsFactors=F, sep=",")
EuropeLCAve=aggregate( LC ~ Species, EuropeSpatMorDownEndemPred, mean )
EuropeNoLCAve=aggregate( NoLC ~ Species, EuropeSpatMorDownEndemPred, mean )
EuropeAverageSpatMorDownEndemPred=merge(EuropeLCAve,EuropeNoLCAve,by="Species")

EuropeSpatMorResamEndemPred=read.table("PredictNAData_confusionTableEurResampleComplete35EndemNoLCvLC.csv", header=T, row.names = NULL,
                               stringsAsFactors=F, sep=",")
EuropeLCAve=aggregate( LC ~ Species, EuropeSpatMorResamEndemPred, mean )
EuropeNoLCAve=aggregate( NoLC ~ Species, EuropeSpatMorResamEndemPred, mean )
EuropeAverageSpatMorResamEnsemPred=merge(EuropeLCAve,EuropeNoLCAve,by="Species")


#read all spatial predictions
EuropeSpatialFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_LCFull_pred.csv", header=T,
                            row.names=NULL, stringsAsFactors = F, sep=",")
EuropeSpatialFullDownsampled=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_LCSamp_avg_probs.csv", header=T,
                            row.names=NULL, stringsAsFactors = F, sep=",")

EuropeSpatialCRFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_CRFull_pred.csv", header=T,
                              row.names=NULL, stringsAsFactors = F, sep=",")
EuropeSpatialCRFullDown=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_CRSamp_avg_probs.csv", header=T,
                                  row.names=NULL, stringsAsFactors = F, sep=",")

EuropeSpatialLC90Full=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_LCrm8_pred.csv", header=T,
                                row.names=NULL, stringsAsFactors = F, sep=",")
EuropeSpatialLC80Downsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_LCrm9_pred.csv", header=T,
                                      row.names=NULL, stringsAsFactors = F, sep=",")

EuropeSpatialAllFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_AllFull_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
EuropeSpatialAllDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_AllSamp_avg_probs.csv", header=T,
                                     row.names=NULL, stringsAsFactors = F, sep=",")

#read spatial predictions for endemics
EuropeSpatialFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_LCFullEnd_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
EuropeSpatialFullDownsampledEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_LCSampEnd_avg_probs.csv", header=T,
                                          row.names=NULL, stringsAsFactors = F, sep=",")

EuropeSpatialCRFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_CRFullEnd_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")
EuropeSpatialCRFullDownEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_CRSampEnd_avg_probs.csv", header=T,
                                     row.names=NULL, stringsAsFactors = F, sep=",")

EuropeSpatialLC90FullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_LCrm8End_pred.csv", header=T,
                                   row.names=NULL, stringsAsFactors = F, sep=",")
EuropeSpatialLC80DownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_LCrm9End_pred.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

EuropeSpatialAllFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_AllFullEnd_pred.csv", header=T,
                                  row.names=NULL, stringsAsFactors = F, sep=",")
EuropeSpatialAllDownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Europe/Europe_AllSampEnd_avg_probs.csv", header=T,
                                        row.names=NULL, stringsAsFactors = F, sep=",")


#now link all the results of all approaches
EuropeAllPredictions=merge(EuropeAverageSpatMorDownPred,EuropeAverageSpatMorResamPred,by="Species", all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam")

EuropeAllPredictions=merge(EuropeAllPredictions,EuropeAverageSpatMorDownEndemPred,by="Species",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeAverageSpatMorResamEnsemPred,by="Species",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem")

EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialFull,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialFullDownsampled,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialCRFull,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialCRFullDown,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialLC90Full,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialLC80Downsample,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80")

EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialAllFull,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull")

EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialAllDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull",
                                  "CRDown","ENDown","LCDown", "NTDown","VUDown")

EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull",
                                  "CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullLCEnd","SpatFullNoLCEnd")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialFullDownsampledEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull",
                                  "CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullLCEnd","SpatFullNoLCEnd","SpatFullDownLCEnd","SpatFullDownNoLCEnd")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialCRFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull",
                                  "CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullLCEnd","SpatFullNoLCEnd","SpatFullDownLCEnd","SpatFullDownNoLCEnd","SpatFullCREnd","SpatFullNoCREnd")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialCRFullDownEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull",
                                  "CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullLCEnd","SpatFullNoLCEnd","SpatFullDownLCEnd","SpatFullDownNoLCEnd","SpatFullCREnd","SpatFullNoCREnd",
                                  "SpatFullDownCREnd","SpatFullDownNoCREnd")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialLC90FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull",
                                  "CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullLCEnd","SpatFullNoLCEnd","SpatFullDownLCEnd","SpatFullDownNoLCEnd","SpatFullCREnd","SpatFullNoCREnd",
                                  "SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End")
EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialLC80DownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull",
                                  "CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullLCEnd","SpatFullNoLCEnd","SpatFullDownLCEnd","SpatFullDownNoLCEnd","SpatFullCREnd","SpatFullNoCREnd",
                                  "SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End")

EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialAllFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull",
                                  "CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullLCEnd","SpatFullNoLCEnd","SpatFullDownLCEnd","SpatFullDownNoLCEnd","SpatFullCREnd","SpatFullNoCREnd",
                                  "SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End",
                                  "CRFullEnd","ENFullEnd","LCFullEnd","NTFullEnd","VUFullEnd")

EuropeAllPredictions=merge(EuropeAllPredictions,EuropeSpatialAllDownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(EuropeAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                                  "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                                  "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC","SpatFullCR","SpatFullNoCR",
                                  "SpatFullDownCR","SpatFullDownNoCR",
                                  "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull","LCFull","NTFull","VUFull",
                                  "CRDown","ENDown","LCDown", "NTDown","VUDown",
                                  "SpatFullLCEnd","SpatFullNoLCEnd","SpatFullDownLCEnd","SpatFullDownNoLCEnd","SpatFullCREnd","SpatFullNoCREnd",
                                  "SpatFullDownCREnd","SpatFullDownNoCREnd","SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End",
                                  "CRFullEnd","ENFullEnd","LCFullEnd","NTFullEnd","VUFullEnd",
                                  "CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd")


write.table(EuropeAllPredictions,file="EuropeMorphoSpatialAllPredictionsNew.csv", sep=",")


#now calculate number predicted as one or the other with high probability
nrow(EuropeAllPredictions[EuropeAllPredictions$SpatFullLC>=0.50,])

############ASIA###################
####################################
#####################################
setwd("E:/collaborations/tara/outputs/RFResults/Asia")
list.files()
memory.limit(size=50000)

#read error rates
AsiaSpatMor31ImputedError=read.table("FullAsiaNoLCvLCErrorRatesDown43.csv", header=T, 
                                     row.names=NULL, stringsAsFactors=F, sep=",")
AsiaSpatMor62ImputedError=read.table("ResampleComplete86AsiaNoLCvLCErrorRates.csv", header=T, 
                                     row.names=NULL,stringsAsFactors=F, sep=",")

AsiaSpatMor31ImputedError=read.table("FullAsiaNoLCvLCErrorRatesDown36Endem.csv", header=T, 
                                     row.names=NULL, stringsAsFactors=F, sep=",")
AsiaSpatMor62ImputedError=read.table("ResampleComplete38EndemAsiaNoLCvLCErrorRates.csv", header=T, 
                                     row.names=NULL,stringsAsFactors=F, sep=",")

#read predictions of NA for the trhee datasets and sampling types
#read Morpho+spatial predictions
AsiaSpatMorDownPred=read.table("PredictNAData_confusionTableFullNoLCvLCDownAsia43.csv", header=T, row.names = NULL,
                            stringsAsFactors=F, sep=",")
AsiaLCAve=aggregate( LC ~ name, AsiaSpatMorDownPred, mean )
AsiaNoLCAve=aggregate( NoLC ~ name, AsiaSpatMorDownPred, mean )
AsiaAverageSpatMorDownPred=merge(AsiaLCAve,AsiaNoLCAve,by="name")

AsiaSpatMorResamPred=read.table("PredictNAData_confusionTableAsiaResampleComplete86NoLCvLC.csv", header=T, row.names = NULL,
                             stringsAsFactors=F, sep=",")
AsiaLCAve=aggregate( LC ~ name, AsiaSpatMorResamPred, mean )
AsiaNoLCAve=aggregate( NoLC ~ name, AsiaSpatMorResamPred, mean )
AsiaAverageSpatMorResamPred=merge(AsiaLCAve,AsiaNoLCAve,by="name")

AsiaSpatMorDownEndemPred=read.table("PredictNAData_confusionTableFullNoLCvLCDownAsia36Endem.csv", header=T, row.names = NULL,
                            stringsAsFactors=F, sep=",")
AsiaLCAve=aggregate( LC ~ name, AsiaSpatMorDownEndemPred, mean )
AsiaNoLCAve=aggregate( NoLC ~ name, AsiaSpatMorDownEndemPred, mean )
AsiaAverageSpatMorDownPredEnd=merge(AsiaLCAve,AsiaNoLCAve,by="name")

AsiaSpatMorResamEndemPred=read.table("PredictNAData_confusionTableAsiaResampleComplete38EndemNoLCvLC.csv", header=T, row.names = NULL,
                             stringsAsFactors=F, sep=",")
AsiaLCAve=aggregate( LC ~ name, AsiaSpatMorResamEndemPred, mean )
AsiaNoLCAve=aggregate( NoLC ~ name, AsiaSpatMorResamEndemPred, mean )
AsiaAverageSpatMorResamEndemPred=merge(AsiaLCAve,AsiaNoLCAve,by="name")

#read all spatial predictions
AsiaSpatialFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_LCFull_pred.csv", header=T,
                             row.names=NULL, stringsAsFactors = F, sep=",")
AsiaSpatialFullDownsampled=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_LCSamp_avg_probs.csv", header=T,
                                        row.names=NULL, stringsAsFactors = F, sep=",")

AsiaSpatialCRFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_CRFull_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
AsiaSpatialCRFullDown=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_CRSamp_avg_probs.csv", header=T,
                                   row.names=NULL, stringsAsFactors = F, sep=",")

AsiaSpatialLC90Full=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_LCrm8_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")
AsiaSpatialLC80Downsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_LCrm9_pred.csv", header=T,
                                       row.names=NULL, stringsAsFactors = F, sep=",")

AsiaSpatialAllFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_AllFull_pred.csv", header=T,
                                row.names=NULL, stringsAsFactors = F, sep=",")
AsiaSpatialAllDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_AllSamp_avg_probs.csv", header=T,
                                      row.names=NULL, stringsAsFactors = F, sep=",")

#read spatial predictions for endemics
AsiaSpatialFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_LCFullEnd_pred.csv", header=T,
                                row.names=NULL, stringsAsFactors = F, sep=",")
AsiaSpatialFullDownsampledEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_LCSampEnd_avg_probs.csv", header=T,
                                           row.names=NULL, stringsAsFactors = F, sep=",")

AsiaSpatialCRFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_CRFullEnd_pred.csv", header=T,
                                  row.names=NULL, stringsAsFactors = F, sep=",")
AsiaSpatialCRFullDownEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_CRSampEnd_avg_probs.csv", header=T,
                                      row.names=NULL, stringsAsFactors = F, sep=",")

AsiaSpatialLC90FullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_LCrm8End_pred.csv", header=T,
                                    row.names=NULL, stringsAsFactors = F, sep=",")
AsiaSpatialLC80DownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_LCrm9End_pred.csv", header=T,
                                          row.names=NULL, stringsAsFactors = F, sep=",")

AsiaSpatialAllFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_AllFullEnd_pred.csv", header=T,
                                   row.names=NULL, stringsAsFactors = F, sep=",")
AsiaSpatialAllDownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Asia/Asia_AllSampEnd_avg_probs.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")


#now link all the results of all approaches
AsiaAllPredictions=merge(AsiaAverageSpatMorDownPred,AsiaAverageSpatMorResamPred,by="name", all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaAverageSpatMorDownPredEnd,by.x="Species", by.y="name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaAverageSpatMorResamEndemPred,by.x="Species", by.y="name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC")

AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialFull,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialFullDownsampled,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialCRFull,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialCRFullDown,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR")

AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialLC90Full,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialLC80Downsample,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80")

AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialAllFull,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull")

AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialAllDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown")

AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                "SpatFullEndLC","SpatFullEndNoLC")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialFullDownsampledEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialCRFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                                "SpatFullEndCR","SpatFullEndNoCR")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialCRFullDownEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                                "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR")

AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialLC90FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                                "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                                "SpatEndLC90","SpatEndnonLC90")
AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialLC80DownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                                "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                                "SpatEndLC90","SpatEndnonLC90","SpatEndLC80","SpatEndnonLC80")

AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialAllFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                                "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                                "SpatEndLC90","SpatEndnonLC90","SpatEndLC80","SpatEndnonLC80","CRFullEnd","ENFullEnd",
                                "LCFullEnd","NTFullEnd","VUFullEnd")

AsiaAllPredictions=merge(AsiaAllPredictions,AsiaSpatialAllDownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AsiaAllPredictions)<-c("Species", "MorphoDownLC","MorphoDownNoLC", "MorphoResamLC","MorphoResamNoLC",
                                "MorphoDownEndLC","MorphoDownEndNoLC","MorphoResamEndLC","MorphoResamEndNoLC",
                                "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                                "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                                "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                                "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                                "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                                "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                                "SpatEndLC90","SpatEndnonLC90","SpatEndLC80","SpatEndnonLC80","CRFullEnd","ENFullEnd",
                                "LCFullEnd","NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd")


write.table(AsiaAllPredictions,file="AsiaAllMorphoSpatialPredictionsNew.csv", sep=",")

#now calculate number predicted as one or the other with high probability
nrow(Asia[Asia$LCSpatMor7>0.9,])
length(AsiaAllPredictions[AsiaAllPredictions$NoLCSpatMor7>0.95,])

############Australia###############
####################################
#####################################
setwd("E:/collaborations/tara/outputs/RFResults/Australia")
list.files()
memory.limit(size=50000)

#read error rates
AusSpatMor31ImputedError=read.table("FullNoLCvLCErrorRatesAusDown40.csv", header=T, 
                                    row.names=NULL, stringsAsFactors=F, sep=",")
AusSpatMor62ImputedError=read.table("ResampleComplete80AusNoLCvLCErrorRates.csv", header=T, 
                                    row.names=NULL,stringsAsFactors=F, sep=",")

AusSpatMor31ImputedError=read.table("FullNoLCvLCErrorRatesAusDown23Endem.csv", header=T, 
                                    row.names=NULL, stringsAsFactors=F, sep=",")
AusSpatMor62ImputedError=read.table("ResampleComplete61EndemAusNoLCvLCErrorRates.csv", header=T, 
                                    row.names=NULL,stringsAsFactors=F, sep=",")

#read predictions of NA for the trhee datasets and sampling types
#read Morpho+Spatial predictions
AusSpatMorDownPred=read.table("PredictNAData_confusionTableFullNoLCvLCAusDown40.csv", header=T, row.names = NULL,
                           stringsAsFactors=F, sep=",")
AusLCAve=aggregate( LC ~ name, AusSpatMorDownPred, mean )
AusNoLCAve=aggregate( NoLC ~ name, AusSpatMorDownPred, mean )
AusAverageSpatMorDownPred=merge(AusLCAve,AusNoLCAve,by="name")

AusSpatMorResamPred=read.table("PredictNAData_confusionTableResampleCompleteAus80NoLCvLC.csv", header=T, row.names = NULL,
                            stringsAsFactors=F, sep=",")
AusLCAve=aggregate( LC ~ name, AusSpatMorResamPred, mean )
AusNoLCAve=aggregate( NoLC ~ name, AusSpatMorResamPred, mean )
AusAverageSpatMorResamPred=merge(AusLCAve,AusNoLCAve,by="name")

AusSpatMorDownEndemPred=read.table("PredictNAData_confusionTableFullNoLCvLCAusDown23Endem.csv", header=T, row.names = NULL,
                           stringsAsFactors=F, sep=",")
AusLCAve=aggregate( LC ~ name, AusSpatMorDownEndemPred, mean )
AusNoLCAve=aggregate( NoLC ~ name, AusSpatMorDownEndemPred, mean )
AusAverageSpatMorDownEndemPred=merge(AusLCAve,AusNoLCAve,by="name")

AusSpatMorResamEndemPred=read.table("PredictNAData_confusionTableResampleCompleteAus61EndemNoLCvLC.csv", header=T, row.names = NULL,
                            stringsAsFactors=F, sep=",")
AusLCAve=aggregate( LC ~ name, AusSpatMorResamEndemPred, mean )
AusNoLCAve=aggregate( NoLC ~ name, AusSpatMorResamEndemPred, mean )
AusAverageSpatMorResamEndemPred=merge(AusLCAve,AusNoLCAve,by="name")

#read all spatial predictions
AustraliaSpatialFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_LCFull_pred.csv", header=T,
                           row.names=NULL, stringsAsFactors = F, sep=",")
AustraliaSpatialFullDownsampled=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_LCSamp_avg_probs.csv", header=T,
                                      row.names=NULL, stringsAsFactors = F, sep=",")

AustraliaSpatialCRFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_CRFull_pred.csv", header=T,
                             row.names=NULL, stringsAsFactors = F, sep=",")
AustraliaSpatialCRFullDown=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_CRSamp_avg_probs.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")

AustraliaSpatialLC90Full=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_LCrm8_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
AustraliaSpatialLC80Downsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_LCrm9_pred.csv", header=T,
                                     row.names=NULL, stringsAsFactors = F, sep=",")

AustraliaSpatialAllFull=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_AllFull_pred.csv", header=T,
                              row.names=NULL, stringsAsFactors = F, sep=",")
AustraliaSpatialAllDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_AllSamp_avg_probs.csv", header=T,
                                    row.names=NULL, stringsAsFactors = F, sep=",")

#read spatial predictions for endemics
AustraliaSpatialFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_LCFull_predEnd.csv", header=T,
                              row.names=NULL, stringsAsFactors = F, sep=",")
AustraliaSpatialFullDownsampledEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_LCSampEnd_avg_probs.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

AustraliaSpatialCRFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_CRFullEnd_pred.csv", header=T,
                                row.names=NULL, stringsAsFactors = F, sep=",")
AustraliaSpatialCRFullDownEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_CRSampEnd_avg_probs.csv", header=T,
                                    row.names=NULL, stringsAsFactors = F, sep=",")

AustraliaSpatialLC90FullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_LCrm8End_pred.csv", header=T,
                                  row.names=NULL, stringsAsFactors = F, sep=",")
AustraliaSpatialLC80DownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_LCrm9End_pred.csv", header=T,
                                        row.names=NULL, stringsAsFactors = F, sep=",")

AustraliaSpatialAllFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_AllFullEnd_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")
AustraliaSpatialAllDownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/Australia/Australia_AllSampEnd_avg_probs.csv", header=T,
                                       row.names=NULL, stringsAsFactors = F, sep=",")


#now link all the results of all approaches
AusAllPredictions=merge(AusAverageSpatMorDownPred,AusAverageSpatMorResamPred,by="name", all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam")
AusAllPredictions=merge(AusAllPredictions,AusAverageSpatMorDownEndemPred,by.x="Species", by.y="name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem")
AusAllPredictions=merge(AusAllPredictions,AusAverageSpatMorResamEndemPred,by.x="Species", by.y="name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem")


AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialFull,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC")
AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialFullDownsampled,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC")
AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialCRFull,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR")
AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialCRFullDown,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR")

AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialLC90Full,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90")
AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialLC80Downsample,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80")

AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialAllFull,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull")

AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialAllDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown")


AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                               "SpatFullEndLC","SpatFullEndNoLC")
AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialFullDownsampledEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                               "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC")
AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialCRFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                               "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                               "SpatFullEndCR","SpatFullEndNoCR")
AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialCRFullDownEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                               "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                               "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR")

AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialLC90FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                               "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                               "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                               "SpatLC90End","SpatnonLC90End")
AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialLC80DownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                               "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                               "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                               "SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End")

AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialAllFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                               "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                               "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                               "SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                               "LCFullEnd","NTFullEnd","VUFullEnd")

AusAllPredictions=merge(AusAllPredictions,AustraliaSpatialAllDownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(AusAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                               "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                               "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                               "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                               "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                               "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                               "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                               "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                               "SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                               "LCFullEnd","NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd")


write.table(AusAllPredictions,file="AusAllMorphoSpatialPredictionsNew.csv", sep=",")

#now calculate number predicted as one or the other with high probability
nrow(Aus[Aus$LCSpatMor7>0.9,])
length(AusAllPredictions[AusAllPredictions$NoLCSpatMor7>0.95,])



############CENTRAL AMERICA##########
####################################
#####################################
setwd("E:/collaborations/tara/outputs/RFResults/CentralAmerica")
list.files()
memory.limit(size=50000)

#read error rates
CASpatMor31ImputedError=read.table("FullNoLCvLCErrorRatesDown52CANew.csv", header=T, 
                                   row.names=NULL, stringsAsFactors=F, sep=",")
CASpatMor62ImputedError=read.table("ResampleComplete104CANoLCvLCErrorRates.csv", header=T, 
                                   row.names=NULL,stringsAsFactors=F, sep=",")

CASpatMor31ImputedErrorEndem=read.table("FullNoLCvLCErrorRatesDown23EndemCANew.csv", header=T, 
                                        row.names=NULL, stringsAsFactors=F, sep=",")
CASpatMor62ImputedErrorEndem=read.table("ResampleComplete61EndemCANoLCvLCErrorRates.csv", header=T, 
                                        row.names=NULL,stringsAsFactors=F, sep=",")


#read predictions of NA for the trhee datasets and sampling types
#read Morpho+Spatial predictions
CASpatMorDownPred=read.table("PredictNAData_confusionTableFullNoLCvLCDown52CANew.csv", header=T, row.names = NULL,
                             stringsAsFactors=F, sep=",")
CALCAve=aggregate( LC ~ name, CASpatMorDownPred, mean )
CANoLCAve=aggregate( NoLC ~ name, CASpatMorDownPred, mean )
CAAverageSpatMorDownPred=merge(CALCAve,CANoLCAve,by="name")

CASpatMorResamPred=read.table("PredictNAData_confusionTableResampleComplete104CANoLCvLC.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
CALCAve=aggregate( LC ~ name, CASpatMorResamPred, mean )
CANoLCAve=aggregate( NoLC ~ name, CASpatMorResamPred, mean )
CAAverageSpatMorResamPred=merge(CALCAve,CANoLCAve,by="name")

CASpatMorDownEndemPred=read.table("PredictNAData_confusionTableFullNoLCvLCDown23EndemCANew.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
CALCAve=aggregate( LC ~ name, CASpatMorDownEndemPred, mean )
CANoLCAve=aggregate( NoLC ~ name, CASpatMorDownEndemPred, mean )
CAAverageSpatMorDownEndemPred=merge(CALCAve,CANoLCAve,by="name")

CASpatMorResamEndemPred=read.table("PredictNAData_confusionTableResampleComplete61EndemCANoLCvLC.csv", header=T, row.names = NULL,
                                   stringsAsFactors=F, sep=",")
CALCAve=aggregate( LC ~ name, CASpatMorResamEndemPred, mean )
CANoLCAve=aggregate( NoLC ~ name, CASpatMorResamEndemPred, mean )
CAAverageSpatMorResamEndemPred=merge(CALCAve,CANoLCAve,by="name")

#read all spatial predictions
CAmericaSpatialFull=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_LCFull_pred.csv", header=T,
                                row.names=NULL, stringsAsFactors = F, sep=",")
CAmericaSpatialFullDownsampled=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_LCSamp_avg_probs.csv", header=T,
                                           row.names=NULL, stringsAsFactors = F, sep=",")

CAmericaSpatialCRFull=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_CRFull_pred.csv", header=T,
                                  row.names=NULL, stringsAsFactors = F, sep=",")
CAmericaSpatialCRFullDown=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_CRSamp_avg_probs.csv", header=T,
                                      row.names=NULL, stringsAsFactors = F, sep=",")

CAmericaSpatialLC90Full=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_LCrm8_pred.csv", header=T,
                                    row.names=NULL, stringsAsFactors = F, sep=",")
CAmericaSpatialLC80Downsample=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_LCrm9_pred.csv", header=T,
                                          row.names=NULL, stringsAsFactors = F, sep=",")

CAmericaSpatialAllFull=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_AllFull_pred.csv", header=T,
                                   row.names=NULL, stringsAsFactors = F, sep=",")
CAmericaSpatialAllDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_AllSamp_avg_probs.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

#read spatial predictions for endemics
CAmericaSpatialFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_LCFull_predEnd.csv", header=T,
                                   row.names=NULL, stringsAsFactors = F, sep=",")
CAmericaSpatialFullDownsampledEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_LCSampEnd_avg_probs.csv", header=T,
                                              row.names=NULL, stringsAsFactors = F, sep=",")

CAmericaSpatialCRFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_CRFullEnd_pred.csv", header=T,
                                     row.names=NULL, stringsAsFactors = F, sep=",")
CAmericaSpatialCRFullDownEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_CRSampEnd_avg_probs.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

CAmericaSpatialLC90FullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_LCrm8End_pred.csv", header=T,
                                       row.names=NULL, stringsAsFactors = F, sep=",")
CAmericaSpatialLC80DownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_LCrm9End_pred.csv", header=T,
                                             row.names=NULL, stringsAsFactors = F, sep=",")

CAmericaSpatialAllFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_AllFullEnd_pred.csv", header=T,
                                      row.names=NULL, stringsAsFactors = F, sep=",")
CAmericaSpatialAllDownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/CAmerica/CA_AllSampEnd_avg_probs.csv", header=T,
                                            row.names=NULL, stringsAsFactors = F, sep=",")


#now link all the results of all approaches
CAAllPredictions=merge(CAAverageSpatMorDownPred,CAAverageSpatMorResamPred,by="name", all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam")
CAAllPredictions=merge(CAAllPredictions,CAAverageSpatMorDownEndemPred,by.x="Species", by.y="name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem")
CAAllPredictions=merge(CAAllPredictions,CAAverageSpatMorResamEndemPred,by.x="Species", by.y="name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem")

CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialFull,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC")
CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialFullDownsampled,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC")
CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialCRFull,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR")
CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialCRFullDown,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR")

CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialLC90Full,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90")
CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialLC80Downsample,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80")

CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialAllFull,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull")

CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialAllDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown")

CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC")
CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialFullDownsampledEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC")
CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialCRFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR")
CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialCRFullDownEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR")

CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialLC90FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                              "SpatLC90End","SpatnonLC90End")
CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialLC80DownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                              "SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End")

CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialAllFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                              "SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                              "LCFullEnd","NTFullEnd","VUFullEnd")

CAAllPredictions=merge(CAAllPredictions,CAmericaSpatialAllDownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                              "SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                              "LCFullEnd","NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd")


write.table(CAAllPredictions,file="CAAllMorphoSpatialPredictionsNew.csv", sep=",")

#now calculate number predicted as one or the other with high probability
nrow(CA[CA$LCSpatMor7>0.9,])
nrow(CAAverageSpatMor30Pred[CAAverageSpatMor30Pred$LC>0.50,])


############South America###############
####################################
#####################################
setwd("E:/collaborations/tara/outputs/RFResults/SouthAmerica")
list.files()
memory.limit(size=50000)

#read error rates
SASpatMor31ImputedError=read.table("FullNoLCvLCErrorRatesDownSA300.csv", header=T, 
                                   row.names=NULL, stringsAsFactors=F, sep=",")
SASpatMor62ImputedError=read.table("ResampleCompleteSA459NoLCvLCErrorRates.csv", header=T, 
                                   row.names=NULL,stringsAsFactors=F, sep=",")

SASpatMor31ImputedError=read.table("FullNoLCvLCErrorRatesDownSA220Endem.csv", header=T, 
                                   row.names=NULL, stringsAsFactors=F, sep=",")
SASpatMor62ImputedError=read.table("ResampleCompleteSA281EndemNoLCvLCErrorRates.csv", header=T, 
                                   row.names=NULL,stringsAsFactors=F, sep=",")

#read predictions of NA for the trhee datasets and sampling types
#read Morpho+Spatial predictions
SASpatMorDownPred=read.table("PredictNAData_confusionTableFullNoLCvLCDownSA300.csv", header=T, row.names = NULL,
                          stringsAsFactors=F, sep=",")
SALCAve=aggregate( LC ~ name, SASpatMorDownPred, mean )
SANoLCAve=aggregate( NoLC ~ name, SASpatMorDownPred, mean )
SAAverageSpatMorDownPred=merge(SALCAve,SANoLCAve,by="name")

SASpatMorResamPred=read.table("PredictNAData_confusionTableResampleComplete459SANoLCvLC.csv", header=T, row.names = NULL,
                           stringsAsFactors=F, sep=",")
SALCAve=aggregate( LC ~ name, SASpatMorResamPred, mean )
SANoLCAve=aggregate( NoLC ~ name, SASpatMorResamPred, mean )
SAAverageSpatMorResamPred=merge(SALCAve,SANoLCAve,by="name")

SASpatMorDownEndemPred=read.table("PredictNAData_confusionTableFullNoLCvLCDownSA220Endem.csv", header=T, row.names = NULL,
                          stringsAsFactors=F, sep=",")
SALCAve=aggregate( LC ~ name, SASpatMorDownEndemPred, mean )
SANoLCAve=aggregate( NoLC ~ name, SASpatMorDownEndemPred, mean )
SAAverageSpatMorDownEndemPred=merge(SALCAve,SANoLCAve,by="name")

SASpatMorResamEndemPred=read.table("PredictNAData_confusionTableResampleComplete281EndemSANoLCvLC.csv", header=T, row.names = NULL,
                           stringsAsFactors=F, sep=",")
SALCAve=aggregate( LC ~ name, SASpatMorResamPred, mean )
SANoLCAve=aggregate( NoLC ~ name, SASpatMorResamPred, mean )
SAAverageSpatMorResamEndemPred=merge(SALCAve,SANoLCAve,by="name")


#read all spatial predictions
SAmericaSpatialFull=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_LCFull_pred.csv", header=T,
                               row.names=NULL, stringsAsFactors = F, sep=",")
SAmericaSpatialFullDownsampled=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_LCSamp_avg_probs.csv", header=T,
                                          row.names=NULL, stringsAsFactors = F, sep=",")

SAmericaSpatialCRFull=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_CRFull_pred.csv", header=T,
                                 row.names=NULL, stringsAsFactors = F, sep=",")
SAmericaSpatialCRFullDown=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_CRSamp_avg_probs.csv", header=T,
                                     row.names=NULL, stringsAsFactors = F, sep=",")

SAmericaSpatialLC90Full=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_LCrm8_pred.csv", header=T,
                                   row.names=NULL, stringsAsFactors = F, sep=",")
SAmericaSpatialLC80Downsample=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_LCrm9_pred.csv", header=T,
                                         row.names=NULL, stringsAsFactors = F, sep=",")

SAmericaSpatialAllFull=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_AllFull_pred.csv", header=T,
                                  row.names=NULL, stringsAsFactors = F, sep=",")
SAmericaSpatialAllDownsample=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_AllSamp_avg_probs.csv", header=T,
                                        row.names=NULL, stringsAsFactors = F, sep=",")

#read spatial predictions for endemics
SAmericaSpatialFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_LCFullEnd_pred.csv", header=T,
                                  row.names=NULL, stringsAsFactors = F, sep=",")
SAmericaSpatialFullDownsampledEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_LCSampEnd_avg_probs.csv", header=T,
                                             row.names=NULL, stringsAsFactors = F, sep=",")

SAmericaSpatialCRFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_CRFullEnd_pred.csv", header=T,
                                    row.names=NULL, stringsAsFactors = F, sep=",")
SAmericaSpatialCRFullDownEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_CRSampEnd_avg_probs.csv", header=T,
                                        row.names=NULL, stringsAsFactors = F, sep=",")

SAmericaSpatialLC90FullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_LCrm8End_pred.csv", header=T,
                                      row.names=NULL, stringsAsFactors = F, sep=",")
SAmericaSpatialLC80DownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_LCrm9End_pred.csv", header=T,
                                            row.names=NULL, stringsAsFactors = F, sep=",")

SAmericaSpatialAllFullEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_AllFullEnd_pred.csv", header=T,
                                     row.names=NULL, stringsAsFactors = F, sep=",")
SAmericaSpatialAllDownsampleEnd=read.table("E:/collaborations/tara/outputs/Spatial/New/SAmerica/SA_AllSampEnd_avg_probs.csv", header=T,
                                           row.names=NULL, stringsAsFactors = F, sep=",")

#now link all the results of all approaches
SAAllPredictions=merge(SAAverageSpatMorDownPred,SAAverageSpatMorResamPred,by="name", all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam")
SAAllPredictions=merge(SAAllPredictions,SAAverageSpatMorDownEndemPred,by.x="Species", by.y="name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem")
SAAllPredictions=merge(SAAllPredictions,SAAverageSpatMorResamEndemPred,by.x="Species", by.y="name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialFull,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC")
SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialFullDownsampled,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialCRFull,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR")
SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialCRFullDown,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialLC90Full,by.x="Species", by.y="na_data.name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90")
SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialLC80Downsample,by.x="Species", by.y="na_data.name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialAllFull,by.x="Species", by.y="na_data.name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialAllDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialFullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC")
SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialFullDownsampled,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialCRFull,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR")
SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialCRFullDown,by.x="Species", by.y="na_data.name",all=T)
colnames(CAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialLC90FullEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                              "SpatLC90End","SpatnonLC90End")
SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialLC80DownsampleEnd,by.x="Species", by.y="na_data.name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                              "SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialAllFull,by.x="Species", by.y="na_data.name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                              "SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                              "LCFullEnd","NTFullEnd","VUFullEnd")

SAAllPredictions=merge(SAAllPredictions,SAmericaSpatialAllDownsample,by.x="Species", by.y="na_data.name",all=T)
colnames(SAAllPredictions)<-c("Species", "LCSpatMorDown","NoLCSpatMorDown", "LCSpatMorResam","NoLCSpatMorResam",
                              "LCSpatMorDownEndem","NoLCSpatMorDownEndem","LCSpatMorResamEndem","NoLCSpatMorResamEndem",
                              "SpatFullLC","SpatFullNoLC","SpatFullDownLC","SpatFullDownNoLC",
                              "SpatFullCR","SpatFullNoCR","SpatFullDownCR","SpatFullDownNoCR",
                              "SpatLC90","SpatnonLC90","SpatLC80","SpatnonLC80","CRFull","ENFull",
                              "LCFull","NTFull","VUFull","CRDown","ENDown","LCDown", "NTDown","VUDown",
                              "SpatFullEndLC","SpatFullEndNoLC","SpatFullDownEndLC","SpatFullDownEndNoLC",
                              "SpatFullEndCR","SpatFullEndNoCR","SpatFullDownEndCR","SpatFullDownEndNoCR",
                              "SpatLC90End","SpatnonLC90End","SpatLC80End","SpatnonLC80End","CRFullEnd","ENFullEnd",
                              "LCFullEnd","NTFullEnd","VUFullEnd","CRDownEnd","ENDownEnd","LCDownEnd", "NTDownEnd","VUDownEnd")




write.table(SAAllPredictions,file="SAAllMorphoSpatialPredictions.csv", sep=",")

#now calculate number predicted as one or the other with high probability
nrow(SAAllPredictions[SAAllPredictions$LCSpatMor7>0.8,])
length(SAAllPredictions[SAAllPredictions$NoLCSpatMor53>0.8,])

