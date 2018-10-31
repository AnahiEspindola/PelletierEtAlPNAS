#get new variable estimates and assign species to endemic or global datasets
library(dplyr)

######put together unique data from GBIF and iucn data

d<-read.csv("all_gps_clip.csv")

global<-species_dist_assignments$l[which(species_dist_assignments$dist=="global")]

l<-list[list %in% global]

########put bioclim data in r
library(geosphere)
library(raster)
library(plyr)

bio1w = raster("bio_1.bil")
bio2w = raster("bio_2.bil")
bio3w = raster("bio_3.bil")
bio4w = raster("bio_4.bil")
bio5w = raster("bio_5.bil")
bio6w = raster("bio_6.bil")
bio7w = raster("bio_7.bil")
bio8w = raster("bio_8.bil")
bio9w = raster("bio_9.bil")
bio10w = raster("bio_10.bil")
bio11w = raster("bio_11.bil")
bio12w = raster("bio_12.bil")
bio13w = raster("bio_13.bil")
bio14w = raster("bio_14.bil")
bio15w = raster("bio_15.bil")
bio16w = raster("bio_16.bil")
bio17w = raster("bio_17.bil")
bio18w = raster("bio_18.bil")
bio19w = raster("bio_19.bil")
elevw = raster("GDEM-10km-BW.tif")

#shapefile to minus out the ocean
library(rgdal)
library(rgeos)
land = readOGR("AllContinents.shp")

for (name in l) {
  #print(name)
  df<-d[which(d$species==name),]
  #print (dim(df))
  n.gps<-nrow(unique(df))
  xy<-as.data.frame(df[,c(3,2)])
  
  max_lat <- max(xy[,2])
  min_lat <- min(xy[,2])
  abs_max_lat <- abs(max_lat)
  abs_min_lat <- abs(min_lat)
  length_lat <- max_lat - min_lat
  median_lon <- median(xy[,1])
  median_lat <- median(xy[,2])
  
  ch <- chull(xy)
  coords <- xy[c(ch, ch[1]), ]
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))

  if (n.gps > 2){
    sp_poly_crop<-gIntersection(sp_poly,land,byid=T,drop_lower_td=TRUE)
    area<-gArea(sp_poly_crop)
    format(area, scientific = FALSE)
  } else {
    area <- NA
  }

  e1<-extract(bio1w, xy)
  bio1m<-mean(e1, na.rm=TRUE)
  bio1sd<-sd(e1, na.rm=TRUE)
  e2<-extract(bio2w, xy)
  bio2m<-mean(e2, na.rm=TRUE)
  bio2sd<-sd(e2, na.rm=TRUE)
  e3<-extract(bio3w, xy)
  bio3m<-mean(e3, na.rm=TRUE)
  bio3sd<-sd(e3, na.rm=TRUE)
  e4<-extract(bio4w, xy)
  bio4m<-mean(e4, na.rm=TRUE)
  bio4sd<-sd(e4, na.rm=TRUE)
  e5<-extract(bio5w, xy)
  bio5m<-mean(e5, na.rm=TRUE)
  bio5sd<-sd(e5, na.rm=TRUE)
  e6<-extract(bio6w, xy)
  bio6m<-mean(e6, na.rm=TRUE)
  bio6sd<-sd(e6, na.rm=TRUE)
  e7<-extract(bio7w, xy)
  bio7m<-mean(e7, na.rm=TRUE)
  bio7sd<-sd(e7, na.rm=TRUE)
  e8<-extract(bio8w, xy)
  bio8m<-mean(e8, na.rm=TRUE)
  bio8sd<-sd(e8, na.rm=TRUE)
  e9<-extract(bio9w, xy)
  bio9m<-mean(e9, na.rm=TRUE)
  bio9sd<-sd(e9, na.rm=TRUE)
  e10<-extract(bio10w, xy)
  bio10m<-mean(e10, na.rm=TRUE)
  bio10sd<-sd(e10, na.rm=TRUE)
  e11<-extract(bio11w, xy)
  bio11m<-mean(e11, na.rm=TRUE)
  bio11sd<-sd(e11, na.rm=TRUE)
  e12<-extract(bio12w, xy)
  bio12m<-mean(e12, na.rm=TRUE)
  bio12sd<-sd(e12, na.rm=TRUE)
  e13<-extract(bio13w, xy)
  bio13m<-mean(e13, na.rm=TRUE)
  bio13sd<-sd(e13, na.rm=TRUE)
  e14<-extract(bio14w, xy)
  bio14m<-mean(e14, na.rm=TRUE)
  bio14sd<-sd(e14, na.rm=TRUE)
  e15<-extract(bio15w, xy)
  bio15m<-mean(e15, na.rm=TRUE)
  bio15sd<-sd(e15, na.rm=TRUE)
  e16<-extract(bio16w, xy)
  bio16m<-mean(e16, na.rm=TRUE)
  bio16sd<-sd(e16, na.rm=TRUE)
  e17<-extract(bio17w, xy)
  bio17m<-mean(e17, na.rm=TRUE)
  bio17sd<-sd(e17, na.rm=TRUE)
  e18<-extract(bio18w, xy)
  bio18m<-mean(e18, na.rm=TRUE)
  bio18sd<-sd(e18, na.rm=TRUE)
  e19<-extract(bio19w, xy)
  bio19m<-mean(e19, na.rm=TRUE)
  bio19sd<-sd(e19, na.rm=TRUE)
  ee<-extract(elevw, xy)
  elevm<-mean(ee, na.rm=TRUE)
  elevsd<-sd(ee, na.rm=TRUE)
  
  write.table(data.frame(name, n.gps, abs_max_lat, abs_min_lat, length_lat, median_lon, median_lat, area, bio1m,bio1sd,bio2m,bio2sd,bio3m,bio3sd,bio4m,bio4sd,bio5m,bio5sd,bio6m,bio6sd,bio7m,bio7sd,bio8m,bio8sd,bio9m,bio9sd,bio10m,bio10sd,bio11m,bio11sd,bio12m,bio12sd,bio13m,bio13sd,bio14m,bio14sd,bio15m,bio15sd,bio16m,bio16sd,bio17m,bio17sd,bio18m,bio18sd,bio19m,bio19sd,elevm,elevsd), file="env_data_global_replace.txt", quote=FALSE, row.names=FALSE, col.names=!file.exists("env_data_global.txt"), append=TRUE, sep="\t")
}


a<-read.table("env_data_global.txt", sep="\t", header=T)

i<-read.csv("IUCN_all.csv", header=T)
i$name <- paste(i$Genus,i$Species)
i<-i[,c(24,18)]

g<-read.table("species_dist_assignments.txt", sep="\t", header=T)
colnames(g)<-c("name","continents","dist")

b<-merge(a, g, by="name", all.x=TRUE)
c<-merge(b, i, by="name", all.x=TRUE)

write.table(c, file="global_data.txt", row.names=FALSE, quote=FALSE, sep="\t")

rm(a,b,c,coords,d,df,g,i,xy,land,list)

