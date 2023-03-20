
library(rgbif)

x <- as.data.frame(rgbif::occ_data(scientificName = "Chordeiles minor",year="1900,2023",month="6,7",hasCoordinate = TRUE,hasGeospatialIssue=FALSE, limit = 100)$data)


x2 <- as.data.frame(rgbif::occ_data(scientificName = "Chordeiles minor",year="1900,2023",month="6,7",hasCoordinate = TRUE,hasGeospatialIssue=FALSE, limit = 100)$data)


library(aniMotum)





fit <- fit_ssm(sese[sese$id==sese$id[1],], 
               vmax= 4, 
               model = "mp", 
               time.step = 24, 
               control = ssm_control(verbose = 0))

plot(fit, type = 3, pages = 1, ncol = 2)

plot(fit, what = "predicted", type = 2, pages = 1)


st_layers("C:/Users/God/Downloads/doc.kml")
x<-st_read("C:/Users/God/Downloads/doc.kml",layer="lcpbss")
x<-st_read("C:/Users/God/Downloads/doc.kml",layer="Calque")

library(maptools)
x<-getKMLcoordinates(textConnection(system("unzip -p /Users/God/Tous_les_calques.kmz", intern = TRUE)))

x <- sim(N = 50, model = "crw", D = 0.5)# |> as.data.frame()
x$id<-"id1"




st_layers("C:/Users/God/Downloads/20221228165822.gpx")
x<-st_read("C:/Users/God/Downloads/20221228165822.gpx","tracks")#[5,]
x<-st_read("C:/Users/God/Downloads/20221227124622.gpx","track_points")#[5,]
x<-st_transform(x,32740)
x$hdop<-ifelse(is.na(x$hdop),10,x$hdop)
xb<-st_buffer(x,x$hdop*5)


plot(st_geometry(x),axes=TRUE)
plot(st_geometry(xb),add=TRUE)
plot(st_geometry(x[x$src=="network",]),col="red",pch=16,add=TRUE)
plot(st_geometry(x[x$src=="gps",]),col="green",pch=16,add=TRUE)
plot(st_geometry(x[x$src=="gps",]),col="green",pch=16,add=TRUE)
lx<-x %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
plot(st_geometry(lx),col="green",pch=16,add=TRUE)
plot(st_geometry(st_centroid(st_union(x))),add=TRUE,col="red")
mapview(x,maxZoom=20,maxNativeZoom=100)

r<-rast(ext=ext(xb),res=0.5)
r<-terra::rasterize(vect(xb),r,field="track_seg_point_id",fun="length",sum=TRUE,background=0)
r<-crop(r,vect(st_buffer(st_union(x),50)))
plot(r)
plot(st_geometry(xb),add=TRUE)

crs(r)<-crs(x)
mapview(raster(r))




library(sf)
library(terra)
library(dplyr)
library(aniMotum)
library(magick)
library(exiftoolr)
library(mapview)

pathgpx<-"C:/Users/God/Documents/gpslogger"
pathims<-"C:/Users/God/Documents/inat"

daterange<-c("2023-03-12","2023-03-12")
gpxfiles<-sort(list.files(pathgpx,pattern=".gpx",full=TRUE))#,"track_points")#[5,]
dr<-seq(as.Date(daterange[1]),as.Date(daterange[2]),by=1)
gpxfiles<-sort(gpxfiles[substr(basename(gpxfiles),1,8)%in%gsub("-","",as.character(dr))])
#gpxfiles<-gpxfiles[3]

#lf<-sort(list.files("C:/Users/God/Documents/gpslogger",pattern="20230129101834.gpx",full=TRUE))#,"track_points")#[5,]
#lf<-sort(list.files("C:/Users/God/Documents/gpslogger",pattern=".gpx",full=TRUE))
#lf<-lf[basename(lf)>=after][2]
#st_layers("C:/Users/God/Downloads/Tous les calques.gpx")
#x<-st_read("C:/Users/God/Downloads/Tous les calques.gpx","tracks")#[5,]
#x1<-st_read("C:/Users/God/Downloads/20221228173243.gpx","track_points")#[5,]
#x2<-st_read("C:/Users/God/Downloads/20230108153054.gpx","track_points")#[5,]
gpx<-lapply(gpxfiles,function(i){
  x<-st_read(i,"track_points")#[5,]
  x$id<-basename(i)
  x$date<-x$time
  x<-st_transform(x,32740)
  x$acc<-ifelse(is.na(x$hdop),10,x$hdop)*5*1 # min acc of 10 m
  x<-x[x$src=="gps",] # remove network locations
  x<-x[which(x$hdop<30),] # remove large hdop cause problems with ssm
  x<-x[which(x$ele>-50),]
  x
})
#gpx<-list(do.call("rbind",gpx));gpxfiles<-gpxfiles[1]
names(gpx)<-basename(gpxfiles)
#x2<-st_read("C:/Users/God/Documents/gpslogger/20230115094055.gpx","track_points")#[5,]
#x<-do.call("rbind",x[1])
#x<-x[x$src=="gps",]
#x<-x[x$hdop<99,]
#x<-x[substr(x$time,1,10)=="2022-12-10" & substr(x$time,12,19)<="18:00:00",]
#plot(st_geometry(x))
#lx <-x %>% group_by(id) %>% summarize(m = mean(attr_data)) %>% st_cast("LINESTRING")
#x$id<-"p1"
#x$date<-x$time
#x<-st_transform(x,32740)
#x$acc<-ifelse(is.na(x$hdop),10,x$hdop)*5*1#*sort(rep(1:5000,length.out=nrow(x)))
#x$acc<-exp(seq(0.1,8.5,length.out=nrow(x)))
#smaj<-x$acc
#lx<-x %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
#xb<-st_buffer(x,x$acc)
gpx4ssm<-lapply(gpx,function(i){
  smaj<-i$acc
  x<-format_data(i)
  x$lc<-"G"
  x$smaj<-smaj
  x$smin<-x$smaj
  x$eor<-0
  x
})
names(gpx4ssm)<-basename(gpxfiles)
#x<-format_data(x)
#x$lc<-"G"
#x$smaj<-smaj
#x$smaj<-x$smaj*sample(c(1,10),nrow(x),replace=TRUE)#*seq(1,10,length.out=nrow(x))
#x$smin<-x$smaj
#x$lonerr<-10
#x$laterr<-x$lonerr

#x<-x[1:73,]
#x<-format_data(x)

#x<-ellie
#x<-x[sample(1:nrow(x),30),]
#x$smaj<-sample(1:10000,nrow(x))
#x$smaj<-c(seq(1,100000,length.out=nrow(x)/2),rev(seq(1,100000,length.out=nrow(x)/2)))
#x$smin<-x$smaj


### Spot AtomJPEG crap in UserComment
ims<-list.files("C:/Users/God/Documents/inat",full=TRUE,pattern=".jpg",recursive=TRUE)
ims<-ims[order(basename(ims))]
ims<-ims[substr(basename(ims),1,8)>=gsub("-","",daterange[1])]
exif<-exif_read(ims,tags = c("DateTimeOriginal","GPSLongitude","GPSLatitude","UserComment"))
o<-order(exif$DateTimeOriginal)
ims<-ims[o]
exif<-exif[o,]
dto<-as.POSIXct(exif[,"DateTimeOriginal"],format="%Y:%m:%d %H:%M:%S")# |> as.character()
exif$ims<-ims
exif$dto<-dto
if(any(grep("AtomJPEG",exif$UserComment))){
  warning(sprintf("The following have been modified by AtomJPEG: %s",exif$SourceFile[grep("AtomJPEG",exif$UserComment)]))
}
if(any(is.na(exif$dto))){
  stop(sprintf("The following have no DateTimeOriginal: %s",exif$SourceFile[is.na(exif$dto)]))
}

#exif[is.na(exif$dto),]
#mpic<-"C:/Users/God/Documents/inat/20230217/20230217_174904.jpg"
#string<-c("-DateTimeOriginal=2023:02:16 09:37:50")
#string<-c("-overwrite_original",string)
#exif_call(path=mpic,args=string)



ssm<-lapply(seq_along(gpx),function(i){
  print(i)
  x<-gpx4ssm[[i]]
  be<-range(gpx4ssm[[i]]$date)
  w<-exif$dto>=be[1] & exif$dto<=be[2]
  if(!any(w)){
    warning(sprintf("No images for track %d - %s",i,names(gpx4ssm)[i]))
    return(NULL)
  }
  ts1<-data.frame(id=x$id[1],date=unname(as.POSIXct(exif$dto[w])))
  ts2<-data.frame(id=x$id[1],date=seq(min(x$date),max(x$date),by="10 secs"))
  ts<-unique(rbind(ts1,ts2))
  ts<-ts[order(ts$date),]
  #row.names(ts)<-1:nrow(ts)
  
  fit <- fit_ssm(x, 
                 vmax= 20, 
                 #ang=c(90,90),
                 model = "rw", 
                 min.dt=0,
                 time.step = ts, 
                 control = ssm_control(verbose = 1))
  
  plot(fit,what="fitted",type=2)
  p<-as.data.frame(grab(fit,"predicted"))
  #p<-st_as_sf(p,coords=c("x","y"),crs=32740)
  #plot(st_geometry(p),pch=16,cex=1,col="darkred",axes=TRUE)
  p$acc<-sqrt((2*p$x.se)^2+(2*p$y.se)^2)
  p$acc<-ifelse(p$acc<20,20,p$acc)
  #bacc<-st_geometry(st_buffer(p,p$acc))
  #buff<-st_geometry(st_buffer(p,2*p$x.se))
  #trackacc<-st_union(bacc)
  #track<-p %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
  #plot(st_geometry(trackacc),pch=16,border=NA,col=adjustcolor("black",0.2),add=TRUE)
  #plot(st_geometry(buff),border=adjustcolor("black",0.5),col=NA,add=TRUE)
  #plot(st_geometry(x),pch=1,cex=1,add=TRUE)
  #plot(st_geometry(track),add=TRUE)
  #plot(st_geometry(lx),add=TRUE)
  m<-match(as.character(exif$dto[w]),as.character(p$date))
  pics<-p[m,]
  pics$SourceFile<-exif$SourceFile[w]
  list(
    pics=pics,
    preds=unique(p),
    exif=exif[w,]
  )
})
names(ssm)<-names(gpx)
ssm<-ssm[!sapply(ssm,is.null)]

pics<-do.call("rbind",lapply(ssm,"[[","pics"))
preds<-do.call("rbind",lapply(ssm,"[[","preds"))
exif<-do.call("rbind",lapply(ssm,"[[","exif"))

ssmtracks<-do.call("rbind",lapply(ssm,function(i){
  x<-st_as_sf(i$preds,coords=c("lon","lat"),crs=4326)
  x |> summarise(do_union = FALSE) |> st_cast("LINESTRING")
}))

ssmbuffers<-do.call("rbind",lapply(ssm,function(i){
  x<-st_as_sf(i$preds,coords=c("x","y"),crs=32740)
  x<-st_buffer(x,dist=sqrt((2*x$x.se)^2+(2*x$y.se)^2))
  st_as_sf(st_union(x))
}))

ssmpics<-pics |>
  st_as_sf(coords=c("x","y"),crs=32740) |>
  st_buffer(dist=pics$acc) |>
  st_as_sf()


gpspics<-exif_read(pics$SourceFile,tags = c("GPSLongitude","GPSLatitude"))
gpspics<-st_as_sf(gpspics,coords=c("GPSLongitude","GPSLatitude"),crs=4326,na.fail=FALSE)
gpspics<-gpspics[!st_is_empty(gpspics),]

gpxpoints<-do.call("rbind",gpx)

mapview(ssmbuffers,col.regions="blue",alpha.regions=0.15,lwd=0) +
  mapview(gpxpoints,cex=3) +
  mapview(ssmtracks,alpha=0.20) +
  mapview(ssmpics,col="white",col.regions="red",alpha.regions=0.15) +
  mapview(gpspics,cex=3,color="black",col.regions="white",legend=FALSE)





#im<-image_read(lf[1])

#trackpoints<-as.data.frame(x)
#st_geometry(trackpoints)<-"geometry"
#st_crs(trackpoints)<-st_crs(x)
#mapview(trackacc,col.regions="blue",alpha.regions=0.15,lwd=0)+
#  mapview(trackpoints,cex=2)+
#  mapview(track,alpha=0.25)+
#  mapview(picsacc,col.regions="red",alpha.regions=0.3)

#mapview(list(lp,xb),alpha.regions=0.4)
#mapview(xb,alpha.regions=0.1)
#mapview(list(pics,picsunc),alpha.regions=0.1)

#e<-exif_read(lf, tags = "*GPS*",args="-n")
#e$GPSLatitude<--1*abs(e$GPSLatitude)
#e<-st_as_sf(e,coords=c("GPSLongitude","GPSLatitude"),crs=4326)
#lf2<-gsub(".jpg","_test.jpg",lf)
tags<-c("-GPSLongitude=","-GPSLatitude=","-GPSHPositioningError=","-GPSLongitudeRef=","-GPSLatitudeRef=","-UserComment=","-ImageDescription=")
lapply((1:nrow(pics)),function(i){
  vtags<-c(pics$lon[i],pics$lat[i],pics$acc[i],"E","S","Location and accuracy obtained from a SSM inferred track and possibly manually adjusted","SSMtrack")
  string<-paste0(tags,vtags)
  string<-c("-overwrite_original",string)
  #string<-paste("-n",string)
  exif_call(path = pics$SourceFile[i], args = string)
})
#vtags<-c()
#exif_call(path = lf[13], args = paste(tags,))
#exif_read(lf[12], tags = "*GPS*")





ims<-list.files("C:/Users/God/Documents/inat",full=TRUE,pattern=".jpg",recursive=TRUE)[1:10]
exif<-exif_read(ims,tags = c("DateTimeOriginal","GPSLongitude","GPSLatitude","UserComment","GPSHPositioningError","Comment"))

tags<-c("-ImageDescription=")
lapply((1:nrow(exif)),function(i){
  vtags<-""
  string<-paste0(tags,vtags)
  string<-c("-overwrite_original",string)
  #string<-paste("-n",string)
  exif_call(path = exif$SourceFile[i], args = string)
})




#mapview(pics)

#exif_call(path = lf2[i], args = string)
#exif_read(lf[1],tags="*date*")
#exif_call(path = "C:/Users/God/Documents/inat/",args=c("-d %Y%m%d_%H%M%S%%-c.%%le '-testname<CreateDate' -ext jpg"))
#system2(command="C:\\Users\\God\\AppData\\Roaming/R/data/R/exiftoolr/win_exe/exiftool(-k).exe",args=" -d %Y%m%d_%H%M%S%%-c.%%le -ext jpg C:/Users/God/Documents/inat/",stdout=TRUE)


library(aniMotum)

step<-as.difftime(72,units="hours")

fit <- fit_ssm(ellie, 
               vmax = Inf, 
               ang=c(180,180),
               distlim = c(Inf, Inf),
               min.dt=as.numeric(step,units="secs"), 
               model = "crw", 
               time.step = 24, 
               control = ssm_control(verbose = 0)
) 

res<-grab(fit,"fitted")
nrow(res)
m<-match(res$date,ellie$date)

c(NA,diff(ellie$date))
c(NA,diff(ellie$date))[m]
res$date
ellie$date[m]



x<-ellie
x$lc<-"G"
x$smaj<-exp(seq(0.1,13,length.out=nrow(x)))
x$smin<-x$smaj
x$eor<-0

fit <- fit_ssm(x, 
               min.dt=0, 
               model = "crw", 
               time.step = 24, 
               control = ssm_control(verbose = 0)
) 

res<-grab(fit,"fitted")
plot(fit, what = "predicted",type=2)


################
################
################

beg<-as.POSIXct("2022-10-01 00:00:01")
end<-as.POSIXct("2022-11-01 00:00:01")
obs<-seq(beg,end,by="1 mins")
n<-200
obs<-sort(obs[sample(seq_along(obs),n,replace=FALSE)])
gap<-as.difftime(4,units="hours")


minGap<-function(x,gap,method=c("min","random","randommin")){
  method<-match.arg(method)
  gapnum<-as.numeric(gap,units=units(gap))
  diffs<-as.numeric(diff(x),units=units(gap))
  while(length(x)>1 && min(diffs)<gapnum){
    x<-switch(method,
              random=x[-(sample(which(diffs<gapnum),1))],
              min=x[-(which.min(diffs)+1)],
              randommin=x[-(sample(which.min(diffs)+0:1,1))]
    )
    diffs<-as.numeric(diff(x),units=units(gap))
  }
  x
}




as.numeric(min(diff(obs)),units="hours")

iter<-5000
vals<-lapply(c("random","randommin"),function(i){
  sapply(1:iter,function(j){
    print(j)
    obs2<-minGap(x=obs,gap=gap,method=i)
    if(length(obs2)==1){
      NULL
    }else{
      as.numeric(max(diff(obs2)),units="hours")
    }
  })
})
brks<-seq(min(unlist(vals))-1,max(unlist(vals))+1,length.out=100)
ylim<-c(0,iter)
cols<-c(adjustcolor(c("red","blue"),0.5),"red")
h<-hist(vals[[2]],breaks=brks,border=NA,col=cols[1],pos=c(0,0))
hist(vals[[1]],ylim=c(0,max(h$density)),breaks=brks,border=NA,col=cols[2],add=TRUE,pos=c(0,0))
abline(v=as.numeric(max(diff(minGap(x=obs,gap=gap,method="min"))),units="hours"),col=cols[3],lwd=3)
legend("topright",pch=c(15,15,NA),pt.cex=2,col=cols,lwd=c(NA,NA,3),legend=c("randommin","random","min"))

as.numeric(min(diff(minGap(x=obs,gap=gap,method="min"))),units="hours")


#h<-hist(vals[[2]],breaks=brks,xaxt="n",border=NA,ylim=ylim)
#axis(1,at=pretty(h$breaks,20))


res<-sapply(1:iter,function(i){
  min(vals[sample(1:iter,i)])  
})

#plot(1:iter,res,log="x")





###########################
##############################
##################################

lf<-list.files("C:/Users/God/Downloads/images",full=TRUE,pattern="ni_pic.png")
im<-lf#[1]
#tags<-c("-ImageDescription=https://www.inaturalist.org/observations/145117579","-Copyright=(c) Javier Hernandez, some rights reserved (CC BY)")
args<-c("-charset","exif=cp437")
#exif_call(path = im, args = tags)
exif_read(path = im, args=args)#tags = "*ImageD*")
cc<-exif_read(path = im, tags = "*Copyright*")$Copyright
Encoding(cc)<-"UTF-8"
cc
rlang::chr_unserialise_unicode(cc)

stringi::stri_encode(charToRaw(enc2native(cc)), from = "UTF-8", to = "windows-1252")
stri_enc_isutf16le(cc)

xx <- iconv(cc, from="UTF-8", to="cp1252")
Encoding(xx) <- "UTF-8"
xx


###############################
###############################
###############################


y<-seq(-22.05,-22.01,length.out=50)
x<-seq(50.01,50.05,length.out=50)

x<-sim(50,rho_p=0.9,error="kf")
plot(x,error=TRUE)

tr <- sim(N = 50, model = "crw", D = 0.1, error = "kf", tdist = "reg", ts=12)
plot(tr, error = TRUE)
tr$id<-"id1"

x<-ellie
x$lc<-"G"
x$smaj<-exp(seq(0.1,13,length.out=nrow(x)))
x$smin<-x$smaj
x$eor<-0

fit <- fit_ssm(tr, 
               model = "rw", 
               min.dt=,
               time.step = ts, 
               control = ssm_control(verbose = 1))

plot(fit,  what = "predicted",type=2)


p<-grab(fit,"predicted")


p<-st_as_sf(p,coords=c("x","y"),crs=32740)
plot(st_geometry(p),pch=16,cex=1,col="darkred",axes=TRUE)
acc<-st_buffer(p,2*p$x.se)
buff<-st_geometry(st_buffer(p,2*p$x.se))
unc<-st_union(buff)
lp<-p %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
plot(st_geometry(unc),pch=16,border=NA,col=adjustcolor("black",0.2),add=TRUE)
#plot(st_geometry(buff),border=adjustcolor("black",0.5),col=NA,add=TRUE)
plot(st_geometry(x),pch=1,cex=1,add=TRUE)
plot(st_geometry(lp),add=TRUE)
plot(st_geometry(lx),add=TRUE)
m<-match(vals,as.character(p$date))
pics<-p[m,]
picsunc<-buff[m]
mapview(list(unc,x,lp),alpha.regions=0.4)




library(terra)
r<-rast(list.files("C:/Users/God/Downloads",pattern="\\.tif",full=TRUE)[17])[[1]]


library(magick)

file<-"Dendrocygna_autumnalis_sdm_small.png"
im<-image_read(file.path("C:/Users/God/Downloads/images",file))
image_scale(im,"x500")
#image_scale(im)
#im2<-image_convolve(im,"Gaussian",iterations=200000)
im2<-image_quantize(im,max=200,dither=FALSE)
image_scale(im2,"x500")
image_write(im2,file.path("C:/Users/God/Downloads",gsub(".png","_crap.jpg",file)))
utils:::format.object_size(file.info(file.path("C:/Users/God/Downloads/images",file))$size,"auto")
utils:::format.object_size(file.info(file.path("C:/Users/God/Downloads",gsub(".png","_crap.jpg",file)))$size,"auto")





lf<-list.files("C:/Users/God/Documents/inat",full=TRUE,pattern=".jpg")
exif_read(lf, tags = "*Date*")



library(spdep)
nc <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)
st_crs(nc) <- "+proj=longlat +datum=NAD27"
nc<-st_union(nc)


p<-st_sample(nc,10000)

plot(st_geometry(nc),axes=TRUE)
plot(st_geometry(p),add=TRUE)

system.time({ 
  b1<-st_union(p) |> st_buffer(dist=0.1)
})

system.time({ 
  b2<-st_buffer(p,dist=0.1) |> st_union()
})
