#plot variable importance
cols<-c("black", "black", "black", "black", "black", "grey", "grey", "grey", "grey", "grey")


#AFRICA######################
a<-as.data.frame(AfricaFullDownContrib[order(-AfricaFullDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="Africa_LCSamp_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)
names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

pdf("Africa_var.pdf")
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="Africa", space=c(0,0.2))
dev.off()

#Africa endemic#################################
a<-as.data.frame(AfricaDownContribEnd[order(-AfricaDownContribEnd$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="Africa_LCSampEnd_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)
names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

tiff("Africa_var_end.tif", width = 4, height = 4, units = 'in', res=300)
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="Africa", space=c(0,0.2))
dev.off()

#ASIA################################
a<-as.data.frame(AsiaFullDownContrib[order(-AsiaFullDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="Asia_LCSamp_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

pdf("Asia_var.pdf")
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="Asia", space=c(0,0.2))
dev.off()

#asia endemic###########################
a<-as.data.frame(AsiaEndemDownContrib[order(-AsiaEndemDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="Asia_LCSampEnd_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

tiff("Asia_var_end.tif", width = 4, height = 4, units = 'in', res=300)
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="Asia", space=c(0,0.2))
dev.off()

#Australia###############################
a<-as.data.frame(AusFullDownContrib[order(-AusFullDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="Austraila_LCSamp_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

pdf("Australia_var.pdf")
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="Australia", space=c(0,0.2))
dev.off()

#Australia endemic ###############################
a<-as.data.frame(AusEndemDownContrib[order(-AusEndemDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="Austraila_LCSampEnd_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

tiff("Australia_var_end.tif", width = 4, height = 4, units = 'in', res=300)
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="Australia", space=c(0,0.2))
dev.off()


#CA####################################
a<-as.data.frame(CAmericaFullDownContrib[order(-CAmericaFullDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="CA_LCSamp_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

pdf("CA_var.pdf")
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="C. America", space=c(0,0.2))
dev.off()

#CA endemic ################################
a<-as.data.frame(CAmericaEndemDownContrib[order(-CAmericaEndemDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="CA_LCSampEnd_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

tiff("CA_var_end.tif", width = 4, height = 4, units = 'in', res=300)
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="C. America", space=c(0,0.2))
dev.off()

#Europe#############################
a<-as.data.frame(EuropeFullDownContrib[order(-EuropeFullDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="Europe_LCSamp_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

pdf("Europe_var.pdf")
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="Europe", space=c(0,0.2))
dev.off()


#Europe endemic #############################
a<-as.data.frame(EuropeEndemDownContrib[order(-EuropeEndemDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="Europe_LCSampEnd_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

tiff("Europe_var_end.tif", width = 4, height = 4, units = 'in', res=300)
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="Europe", space=c(0,0.2))
dev.off()

tiff("Europe_var_end.tif", width = 4, height = 4, units = 'in', res=300)
par(mar=c(8,4,1,0.5))
barplot(t2$x,  names.arg=t2$Group.1,las=2, main="Europe", col="grey28", space=2)
dev.off()

#NA#############################################
a<-as.data.frame(NAmericaFullDownContrib[order(-NAmericaFullDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="NA_LCSamp_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

pdf("NA_var.pdf")
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="N. America", space=c(0,0.2))
dev.off()

#NA endemic#############################################
a<-as.data.frame(NAmericaEndemDownContrib[order(-NAmericaEndemDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="NA_LCSampEnd_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

tiff("NA_var_end.tif", width = 4, height = 4, units = 'in', res=300)
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="N. America", space=c(0,0.2))
dev.off()

tiff("NA_var_end.tif", width = 4, height = 4, units = 'in', res=300)
par(mar=c(8,4,1,0.5))
barplot(t2$x,  names.arg=t2$Group.1,las=2, main="Europe", col="grey28", space=2)
dev.off()

#SA###############################################
a<-as.data.frame(SAmericaFullDownContrib[order(-SAmericaFullDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="SA_LCSamp_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

pdf("SA_var.pdf")
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="S. America", space=c(0,0.2))
dev.off()

#SA endemic ###############################################
a<-as.data.frame(SAmericaEndemDownContrib[order(-SAmericaEndemDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="SA_LCSamp_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

tiff("SA_var_end.tif", width = 4, height = 4, units = 'in', res=300)
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="S. America", space=c(0,0.2))
dev.off()

#global
a<-as.data.frame(GlobalFullDownContrib[order(-GlobalFullDownContrib$MeanDecreaseAccuracy),])
a<-a[,c(2,3)]
names(a)<-c("Group.1", "x")

#n<-aggregate(imps$MeanDecreaseAccuracy, list(imps$Variable), mean)
#write.csv(n, file="global_LCSamp_avg_imp.csv", row.names=FALSE)
b<-as.data.frame(n[order(-n$x),])

t1<-head(a,5)
l1<-t1[,1]
t2<-head(b,5)
l2<-t2[,1]
l<-c(l1,l2)

m<-a[a$Group.1 %in% l,]
s<-b[b$Group.1 %in% l,]

sm<-merge(s,m,by="Group.1",all=TRUE)

names(sm)<-c("v","spatial","morpho")
sm <- sm[order(-sm$spatial, -sm$morpho),]

nm<-sm[,1]
d<-sm[,-1]
df<-as.table(t(d))

pdf("global_var.pdf")
par(mar=c(8,4,1,0.5))
barplot(df, beside=TRUE, width=3, names.arg=nm, las=2, main="Global", space=c(0,0.2))
dev.off()