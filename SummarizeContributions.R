
#####################################
############AFRICA###################
#####################################


#read variable contributions


setwd("E:/collaborations/tara/outputs/RFResults/Africa/noLivingSp")

AfricaSpatMor7Contrib=read.table("FullAfrNoLCvLC40.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
AfricaContrib7=aggregate( MeanDecreaseAccuracy ~ variable, AfricaSpatMor7Contrib, mean )
write.csv(AfricaContrib7,file="AfricaFullDownContrib.csv")

AfricaSpatMor30Contrib=read.table("ResampleAfrComplete80NoLCvLC.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
AfricaContrib30=aggregate( MeanDecreaseAccuracy ~ variable, AfricaSpatMor30Contrib, mean )
write.csv(AfricaContrib30,file="AfricaFullResamContrib.csv")


AfricaSpatMor7ContribEnd=read.table("FullAfrNoLCvLC13Endem.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
AfricaContrib7End=aggregate( MeanDecreaseAccuracy ~ variable, AfricaSpatMor7ContribEnd, mean )
write.csv(AfricaContrib7End,file="AfricaDownContribEnd.csv")

AfricaSpatMor30ContribEnd=read.table("ResampleAfrComplete26EndemNoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
AfricaContrib30End=aggregate( MeanDecreaseAccuracy ~ variable, AfricaSpatMor30ContribEnd, mean )
write.csv(AfricaContrib30End,file="AfricaResamContribEnd.csv")

x11()
par(mfrow=c(2,1))
barplot(AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph7 - Africa")
barplot(AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph30")

##calculate those samples that have been wrongly predicted in oob
AfricaSpatMor62oob=read.table("PredictionOfOOBSpecies30.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob62LC=aggregate( LC ~ Species, AfricaSpatMor62oob, mean )
Africaoob62LCFinal=merge(Africaoob62LC,AfricaSpatMor62oob[!duplicated(AfricaSpatMor62oob$Species),], by="Species")
AsiaPredicOOBLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor62oob[wronglyOOB,4]


AfricaSpatMor31oob=read.table("PredictionOfOOBSpecies.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob31LC=aggregate( LC ~ Species, AfricaSpatMor31oob, mean )
Africaoob31LCFinal=merge(Africaoob31LC,AfricaSpatMor31oob[!duplicated(AfricaSpatMor31oob$Species),], by="Species")
AsiaPredicOOBLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor31oob[wronglyOOB,4]


#####################################
############N AMERICA################
#####################################


#read variable contributions


setwd("E:/collaborations/tara/outputs/RFResults/NAmerica/noLivingSp")

AfricaSpatMor7Contrib=read.table("DownsampleNAmComplete14NoLCvLC.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
AfricaContrib7=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7Contrib, mean )

AfricaSpatMor30Contrib=read.table("ResampleNAmComplete28NoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
AfricaContrib30=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30Contrib, mean )


# AfricaSpatMor7ContribEnd=read.table("DownsampleNAmComplete5EndemNoLCvLC.csv", header=T, row.names = NULL,
#                                  stringsAsFactors=F, sep=",")
# AfricaContrib7End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7ContribEnd, mean )
# 
# AfricaSpatMor30ContribEnd=read.table("ResampleNAmComplete37EndemNoLCvLC.csv", header=T, row.names = NULL,
#                                   stringsAsFactors=F, sep=",")
# AfricaContrib30End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30ContribEnd, mean )

write.csv(AfricaContrib7,file="NAmericaFullDownContrib.csv")
write.csv(AfricaContrib30,file="NAmericaFullResamContrib.csv")
#write.csv(AfricaContrib7End,file="NAmericaEndemDownContrib.csv")
#write.csv(AfricaContrib30End,file="NAmericaEndemResamContrib.csv")

x11()
par(mfrow=c(2,1))
barplot(AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph29 - N America")
barplot(AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph50")

##calculate those samples that have been wrongly predicted in oob
AfricaSpatMor62oob=read.table("PredictionOfOOBSpecies50.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob62LC=aggregate( LC ~ Species, AfricaSpatMor62oob, mean )
Africaoob62LCFinal=merge(Africaoob62LC,AfricaSpatMor62oob[!duplicated(AfricaSpatMor62oob$Species),], by="Species")
AsiaPredicOOBLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBNoLC$LC.x>=0.8)
Wrongspecies=AfricaSpatMor62oob[wronglyOOB,4]


AfricaSpatMor31oob=read.table("PredictionOfOOBSpecies29.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob31LC=aggregate( LC ~ Species, AfricaSpatMor31oob, mean )
Africaoob31LCFinal=merge(Africaoob31LC,AfricaSpatMor31oob[!duplicated(AfricaSpatMor31oob$Species),], by="Species")
AsiaPredicOOBLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBNoLC$LC.x>=0.8)
Wrongspecies=AfricaSpatMor31oob[wronglyOOB,4]




#####################################
############ EUROPE #################
#####################################


#read variable contributions


setwd("E:/collaborations/tara/outputs/RFResults/Europe/noLivingSp")

AfricaSpatMor7Contrib=read.table("DownsampleEurComplete3NoLCvLC.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
AfricaContrib7=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7Contrib, mean )

AfricaSpatMor30Contrib=read.table("ResampleCompleteEur6NoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
AfricaContrib30=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30Contrib, mean )


# AfricaSpatMor7ContribEnd=read.table("DownsampleEurComplete2EndemNoLCvLC.csv", header=T, row.names = NULL,
#                                     stringsAsFactors=F, sep=",")
# AfricaContrib7End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7ContribEnd, mean )
# 
# AfricaSpatMor30ContribEnd=read.table("ResampleCompleteEur35EndemNoLCvLC.csv", header=T, row.names = NULL,
#                                      stringsAsFactors=F, sep=",")
# AfricaContrib30End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30ContribEnd, mean )

write.csv(AfricaContrib7,file="EuropeFullDownContrib.csv")
write.csv(AfricaContrib30,file="EuropeFullResamContrib.csv")
# write.csv(AfricaContrib7End,file="EuropeEndemDownContrib.csv")
# write.csv(AfricaContrib30End,file="EuropeEndemResamContrib.csv")


x11()
par(mfrow=c(2,1))
barplot(AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph38 - Europe")
barplot(AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph70")

##calculate those samples that have been wrongly predicted in oob
AfricaSpatMor62oob=read.table("PredictionOfOOBSpecies70.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
AfricaSpatMor62oob=merge(AfricaSpatMor62oob,AfricaImputedSpatialToUse[,c(1,39)], by="Species")
Africaoob62LC=aggregate( LC ~ Species, AfricaSpatMor62oob, mean )
Africaoob62LCFinal=merge(Africaoob62LC,AfricaSpatMor62oob[!duplicated(AfricaSpatMor62oob$Species),], by="Species")
AsiaPredicOOBLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor62oob[wronglyOOB,4]


AfricaSpatMor31oob=read.table("PredictionOfOOBSpecies38.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
AfricaSpatMor31oob=merge(AfricaSpatMor31oob,AfricaImputedSpatialToUse[,c(1,39)], by="Species")
Africaoob31LC=aggregate( LC ~ Species, AfricaSpatMor31oob, mean )
Africaoob31LCFinal=merge(Africaoob31LC,AfricaSpatMor31oob[!duplicated(AfricaSpatMor31oob$Species),], by="Species")
AsiaPredicOOBLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor31oob[wronglyOOB,1]



#####################################
############ Asia #################
#####################################


#read variable contributions


setwd("E:/collaborations/tara/outputs/RFResults/Asia/noLivingSp")

AfricaSpatMor7Contrib=read.table("FullAsiaNoLCvLCDown39.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
AfricaContrib7=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7Contrib, mean )

AfricaSpatMor30Contrib=read.table("ResampleComplete78AsiaNoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
AfricaContrib30=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30Contrib, mean )


AfricaSpatMor7ContribEnd=read.table("FullAsiaNoLCvLCDown3Endem.csv", header=T, row.names = NULL,
                                    stringsAsFactors=F, sep=",")
AfricaContrib7End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7ContribEnd, mean )

AfricaSpatMor30ContribEnd=read.table("ResampleComplete6EndemAsiaNoLCvLC.csv", header=T, row.names = NULL,
                                     stringsAsFactors=F, sep=",")
AfricaContrib30End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30ContribEnd, mean )

write.csv(AfricaContrib7,file="AsiaFullDownContrib.csv")
write.csv(AfricaContrib30,file="AsiaFullResamContrib.csv")
write.csv(AfricaContrib7End,file="AsiaEndemDownContrib.csv")
write.csv(AfricaContrib30End,file="AsiaEndemResamContrib.csv")



x11()
par(mfrow=c(2,1))
barplot(AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph31 - Asia")
barplot(AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph62")

##calculate those samples that have been wrongly predicted in oob
AfricaSpatMor62oob=read.table("PredictionOfOOBSpecies62.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
Africaoob62LC=aggregate( LC ~ name, AfricaSpatMor62oob, mean )
Africaoob62LCFinal=merge(Africaoob62LC,AfricaSpatMor62oob[!duplicated(AfricaSpatMor62oob$name),], by="name")
AsiaPredicOOBLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor62oob[wronglyOOB,4]


AfricaSpatMor31oob=read.table("PredictionOfOOBSpeciesDown31.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
Africaoob31LC=aggregate( LC ~ name, AfricaSpatMor31oob, mean )
Africaoob31LCFinal=merge(Africaoob31LC,AfricaSpatMor31oob[!duplicated(AfricaSpatMor31oob$name),], by="name")
AsiaPredicOOBLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor31oob[wronglyOOB,4]

#####################################
############ Australia #################
#####################################


#read variable contributions


setwd("E:/collaborations/tara/outputs/RFResults/Australia/noLivingSp")

AfricaSpatMor7Contrib=read.table("FullNoLCvLCDownAusDown40.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
AfricaContrib7=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7Contrib, mean )

AfricaSpatMor30Contrib=read.table("ResampleComplete80AusNoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
AfricaContrib30=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30Contrib, mean )


AfricaSpatMor7ContribEnd=read.table("FullNoLCvLCDownAusDown23Endem.csv", header=T, row.names = NULL,
                                    stringsAsFactors=F, sep=",")
AfricaContrib7End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7ContribEnd, mean )

AfricaSpatMor30ContribEnd=read.table("ResampleComplete61EndemAusNoLCvLC.csv", header=T, row.names = NULL,
                                     stringsAsFactors=F, sep=",")
AfricaContrib30End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30ContribEnd, mean )

write.csv(AfricaContrib7,file="AusFullDownContrib.csv")
write.csv(AfricaContrib30,file="AusFullResamContrib.csv")
write.csv(AfricaContrib7End,file="AusEndemDownContrib.csv")
write.csv(AfricaContrib30End,file="AusEndemResamContrib.csv")


x11()
par(mfrow=c(2,1))
barplot(AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph31 - Aus")
barplot(AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph62")


##calculate those samples that have been wrongly predicted in oob
AfricaSpatMor15oob=read.table("PredictionOfOOBSpecies15.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob15LC=aggregate( LC ~ name, AfricaSpatMor15oob, mean )
Africaoob15LCFinal=merge(Africaoob15LC,AfricaSpatMor15oob[!duplicated(AfricaSpatMor15oob$name),], by="name")
AsiaPredicOOBLC=Africaoob15LCFinal[Africaoob15LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob15LCFinal[Africaoob15LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor15oob[wronglyOOB,4]


AfricaSpatMor5oob=read.table("PredictionOfOOBSpeciesDown5.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob5LC=aggregate( LC ~ name, AfricaSpatMor5oob, mean )
Africaoob5LCFinal=merge(Africaoob5LC,AfricaSpatMor5oob[!duplicated(AfricaSpatMor5oob$name),], by="name")
AsiaPredicOOBLC=Africaoob5LCFinal[Africaoob5LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob5LCFinal[Africaoob5LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor5oob[wronglyOOB,4]



#####################################
######## Central America ############
#####################################


#read variable contributions


setwd("E:/collaborations/tara/outputs/RFResults/CentralAmerica/noLivingSp")

AfricaSpatMor7Contrib=read.table("FullNoLCvLCDown50CANew.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
AfricaContrib7=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7Contrib, mean )

AfricaSpatMor30Contrib=read.table("ResampleComplete100CANoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
AfricaContrib30=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30Contrib, mean )


AfricaSpatMor7ContribEnd=read.table("FullNoLCvLCDown36EndemCANew.csv", header=T, row.names = NULL,
                                    stringsAsFactors=F, sep=",")
AfricaContrib7End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7ContribEnd, mean )

AfricaSpatMor30ContribEnd=read.table("ResampleComplete72EndemCANoLCvLC.csv", header=T, row.names = NULL,
                                     stringsAsFactors=F, sep=",")
AfricaContrib30End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30ContribEnd, mean )

write.csv(AfricaContrib7,file="CAmericaFullDownContrib.csv")
write.csv(AfricaContrib30,file="CAmericaFullResamContrib.csv")
write.csv(AfricaContrib7End,file="CAmericaEndemDownContrib.csv")
write.csv(AfricaContrib30End,file="CAmericaEndemResamContrib.csv")

x11()
par(mfrow=c(2,1))
barplot(AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph31 - CA")
barplot(AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph62")

##calculate those samples that have been wrongly predicted in oob
AfricaSpatMor62oob=read.table("PredictionOfOOBSpecies62.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob62LC=aggregate( LC ~ name, AfricaSpatMor62oob, mean )
Africaoob62LCFinal=merge(Africaoob62LC,AfricaSpatMor62oob[!duplicated(AfricaSpatMor62oob$name),], by="name")
AsiaPredicOOBLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob62LCFinal[Africaoob62LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor62oob[wronglyOOB,4]


AfricaSpatMor31oob=read.table("PredictionOfOOBSpeciesDown31.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob31LC=aggregate( LC ~ name, AfricaSpatMor31oob, mean )
Africaoob31LCFinal=merge(Africaoob31LC,AfricaSpatMor31oob[!duplicated(AfricaSpatMor31oob$name),], by="name")
AsiaPredicOOBLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob31LCFinal[Africaoob31LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor31oob[wronglyOOB,4]


#####################################
############ South America #################
#####################################


#read variable contributions


setwd("E:/collaborations/tara/outputs/RFResults/SouthAmerica/noLivingSp")

AfricaSpatMor7Contrib=read.table("FullNoLCvLCDownSA25.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
AfricaContrib7=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7Contrib, mean )

AfricaSpatMor30Contrib=read.table("ResampleCompleteSA50NoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
AfricaContrib30=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30Contrib, mean )


AfricaSpatMor7ContribEnd=read.table("FullNoLCvLCDownSA16Endem.csv", header=T, row.names = NULL,
                                    stringsAsFactors=F, sep=",")
AfricaContrib7End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7ContribEnd, mean )

AfricaSpatMor30ContribEnd=read.table("ResampleCompleteSA32EndemNoLCvLC.csv", header=T, row.names = NULL,
                                     stringsAsFactors=F, sep=",")
AfricaContrib30End=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30ContribEnd, mean )

write.csv(AfricaContrib7,file="SAmericaFullDownContrib.csv")
write.csv(AfricaContrib30,file="SAmericaFullResamContrib.csv")
write.csv(AfricaContrib7End,file="SAmericaEndemDownContrib.csv")
write.csv(AfricaContrib30End,file="SAmericaEndemResamContrib.csv")


x11()
par(mfrow=c(2,1))
barplot(AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph53 - SAmerica")
barplot(AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph57")


##calculate those samples that have been wrongly predicted in oob
AfricaSpatMor57oob=read.table("PredictionOfOOBSpecies57.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob57LC=aggregate( LC ~ name, AfricaSpatMor57oob, mean )
Africaoob57LCFinal=merge(Africaoob57LC,AfricaSpatMor57oob[!duplicated(AfricaSpatMor57oob$name),], by="name")
AsiaPredicOOBLC=Africaoob57LCFinal[Africaoob57LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob57LCFinal[Africaoob57LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor57oob[wronglyOOB,4]


AfricaSpatMor53oob=read.table("PredictionOfOOBSpeciesDown53.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob53LC=aggregate( LC ~ name, AfricaSpatMor53oob, mean )
Africaoob53LCFinal=merge(Africaoob53LC,AfricaSpatMor53oob[!duplicated(AfricaSpatMor53oob$name),], by="name")
AsiaPredicOOBLC=Africaoob53LCFinal[Africaoob53LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob53LCFinal[Africaoob53LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor53oob[wronglyOOB,4]

#####################################
############ Global #################
#####################################


#read variable contributions


setwd("E:/collaborations/tara/outputs/RFResults/Global/noLivingSp")

AfricaSpatMor7Contrib=read.table("FullGlobalNoLCvLC26.csv", header=T, row.names = NULL,
                                 stringsAsFactors=F, sep=",")
AfricaContrib7=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor7Contrib, mean )

AfricaSpatMor30Contrib=read.table("ResampleGlobalComplete52NoLCvLC.csv", header=T, row.names = NULL,
                                  stringsAsFactors=F, sep=",")
AfricaContrib30=aggregate( MeanDecreaseAccuracy ~ Variable, AfricaSpatMor30Contrib, mean )


write.csv(AfricaContrib7,file="GlobalFullDownContrib.csv")
write.csv(AfricaContrib30,file="GlobalFullResamContrib.csv")


x11()
par(mfrow=c(2,1))
barplot(AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib7[order(AfricaContrib7[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph53 - Global")
barplot(AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,2],
        names.arg=AfricaContrib30[order(AfricaContrib30[,2],decreasing=TRUE),][,1], las=2,
        main="Mean Deacrease Accuracy - SpatMorph57")


##calculate those samples that have been wrongly predicted in oob
AfricaSpatMor57oob=read.table("PredictionOfOOBSpecies57.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob57LC=aggregate( LC ~ name, AfricaSpatMor57oob, mean )
Africaoob57LCFinal=merge(Africaoob57LC,AfricaSpatMor57oob[!duplicated(AfricaSpatMor57oob$name),], by="name")
AsiaPredicOOBLC=Africaoob57LCFinal[Africaoob57LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob57LCFinal[Africaoob57LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor57oob[wronglyOOB,4]


AfricaSpatMor53oob=read.table("PredictionOfOOBSpeciesDown53.csv", header=T, row.names = NULL,
                              stringsAsFactors=F, sep=",")
Africaoob53LC=aggregate( LC ~ name, AfricaSpatMor53oob, mean )
Africaoob53LCFinal=merge(Africaoob53LC,AfricaSpatMor53oob[!duplicated(AfricaSpatMor53oob$name),], by="name")
AsiaPredicOOBLC=Africaoob53LCFinal[Africaoob53LCFinal$NewIUCN=="LC",]
AsiaPredicOOBNoLC=Africaoob53LCFinal[Africaoob53LCFinal$NewIUCN=="NoLC",]
wronglyOOB=which(AsiaPredicOOBLC$LC.x<=0.2)
Wrongspecies=AfricaSpatMor53oob[wronglyOOB,4]


