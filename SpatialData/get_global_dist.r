library(dplyr)
library(rworldmap)
library(rworldxtra)

#convert coordinates to continents
coords2continent = function(points)
{  
  #countriesSP <- getMap(resolution='low')
  countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}


#import all gps points (all_gps_clip.csv)
list<-unique(excl$species)
list<-list[list!="species"]
list<-list[list!="Species"]
list<-list[list!="SPECIES"]
list<-na.omit(list)
  

for (l in list){
  df<-all_gps_clip[which(all_gps$species==l),]
  if (nrow(df) > 1){
    xy<-xy<-as.data.frame(df[,c(3,2)])
    c<-coords2continent(xy)
    na.omit(c)
    c<-unique(c)
  
    if (length(c) == 1){
      dist<-"endemic"
    } else {
      dist<-"global"
    }
  
    c<-paste(as.character(c), collapse=",")

    write.table(data.frame(l,c,dist), file="species_dist_assignments.txt", quote=FALSE, row.names=FALSE, append=T, col.names=!file.exists("species_dist_assignments.txt"), sep="\t")
  }
}
