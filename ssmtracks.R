
### rename and number files
# exiftool -d '%Y%m%d_%H%M%S_%%02.c.%%e' '-filename<CreateDate' .

stacked <- "/home/frousseu/Documents/inaturalist/20250413/20250413_143723retouched.jpg"
target <- "/home/frousseu/Documents/inaturalist/20250413/20250413_143751_00.jpg"

"~/Documents/inaturalist/20250503/20250503_180904_00stacked.jpg"

system(sprintf("exiftool -overwrite_original -tagsfromfile %s -Make -Model -Software -FocalLength -CreateDate -DateTimeOriginal \'%s\'", target, stacked))
#system(sprintf("exiftool -tagsfromfile %s -all:all --Orientation \'%s\'", target, stacked))
##system(sprintf("exiftool '-Comment<${Comment}Stacked' \'%s\'", stacked))
system(sprintf("exiftool -overwrite_original '-Comment=Stacked' \'%s\'", stacked))

### give exif to stacked
folder <- "20251210"
lf <- list.files(file.path("/home/frousseu/Documents/inaturalist", folder), pattern = ".jpg", full = TRUE)
lf <- lf[duplicated(gsub("stacked", "", lf)) | duplicated(gsub("stacked", "", lf), fromLast = TRUE)]
lf
invisible(lapply(unique(gsub("stacked", "", lf)), function(i){
  stacked <- gsub(".jpg", "stacked.jpg", i)
  system(sprintf("exiftool -overwrite_original -tagsfromfile %s -Make -Model -Software -FocalLength -CreateDate -DateTimeOriginal \'%s\'", i, stacked))
  system(sprintf("exiftool -overwrite_original '-Comment=stacked' \'%s\'", stacked))
}))






library(sf)
library(terra)
library(dplyr)
library(aniMotum)
library(magick)
library(exiftoolr)
library(mapview)

pathgpx<-"/home/frousseu/Documents/inaturalist/gpslogger"
pathims<-"/home/frousseu/Documents/inaturalist/20251210"
epsg<- 32618
#epsg<- 32631

# done until 2023-06-15 (maybe check epsg with everything after RUN

daterange<-c("2025-10-27","2025-12-10") #06-15
gpxfiles<-sort(list.files(pathgpx,pattern=".gpx",full=TRUE))#,"track_points")#[5,]
dr<-seq(as.Date(daterange[1]),as.Date(daterange[2]),by=1)
gpxfiles<-sort(gpxfiles[substr(basename(gpxfiles),1,8)%in%gsub("-","",as.character(dr))])#[-7]

#gpxfiles <- gpxfiles[-c(4, 8)] ||||||||||||||||||||||
#thresh2split <- 300 |||||||||||||||||||||||||||||||

gpx<-lapply(gpxfiles,function(i){
  x<-st_read(i,"track_points")#[5,]
  if(i == "/home/frousseu/Documents/inaturalist/gpslogger/20241006180037.gpx"){
    x <- x[1:170, ]
  }
  if(i == "/home/frousseu/Documents/inaturalist/gpslogger/20250715200450.gpx"){
    x <- x[1:141, ]
  }
  if(i == "/home/frousseu/Documents/inaturalist/gpslogger/20250815161657.gpx"){
    x <- x[1:104, ]
  }
  if(i == "/home/frousseu/Documents/inaturalist/gpslogger/20250821152323.gpx"){
    x <- x[1:46, ]
  }
  if(i == "/home/frousseu/Documents/inaturalist/gpslogger/20250821161445.gpx"){
    x <- x[1:113, ]
  }
  if(i == "/home/frousseu/Documents/inaturalist/gpslogger/20250906123634.gpx"){
    x <- x[1:137, ]
  }
  if(i == "/home/frousseu/Documents/inaturalist/gpslogger/20251026142017.gpx"){
    x <- x[1:198, ]
  }
  if(i == "/home/frousseu/Documents/inaturalist/gpslogger/20251115154955.gpx"){
    x <- x[1:117, ]
  }
  x$id<-basename(i)
  time<-x$time
  #attr(time,"tzone")<-"Europe/Paris"
  x$date<-time
  #tz_lookup_coords(43.87899,-0.2908675, method = "fast")
  x<-st_transform(x,epsg)
  #x<-st_transform(x,32618)
  x$acc<-ifelse(is.na(x$hdop),10,x$hdop)*5*1 # min acc of 10 m
  x<-x[x$src=="gps",] # remove network locations
  x<-x[which(x$hdop<30),] # remove large hdop cause problems with ssm
  x<-x[which(x$ele>-50),]
  #runs <- cumsum(c(0, as.numeric(diff(x$time))) > thresh2split) + 1 ||||||||||||||||||||
  #if(max(runs) > 1){
  #  l <- split(x, runs)
  #  nl <- sapply(l, function(j){format(j$time, "%Y%m%d_%H%M%S")[1]}) |>
  #          as.character() |>
  #          gsub("-|:", "", x = _) |>
  #          gsub(" ", "_", x = _) |>
  #          paste0(".gpx")
  #  names(l) <- nl
  #  lapply(nl, function(j){st_write(l[[j]], j, layer = "track_points", dataset_options = c("GPX_USE_EXTENSIONS=yes"))})
  #}else{
  x
  #}
})

#gpx <- gpx[-c(1,2)]



#gpx<-list(do.call("rbind",gpx));gpxfiles<-gpxfiles[1]
names(gpx)<-basename(gpxfiles)
gpx4ssm<-lapply(names(gpx),function(j){
  print(j)
  i<-gpx[[j]]
  smaj<-i$acc
  x<-format_data(i)
  x$lc<-"G"
  x$smaj<-smaj
  x$smin<-x$smaj
  x$eor<-0
  x
})
names(gpx4ssm)<-basename(gpxfiles)


### Spot AtomJPEG crap in UserComment
ims<-list.files(pathims,full=TRUE,pattern=".jpg",recursive=TRUE)
ims<-ims[order(basename(ims))]
ims<-ims[substr(basename(ims),1,8)>=gsub("-","",daterange[1])]
exif<-exif_read(ims,tags = c("DateTimeOriginal","GPSLongitude","GPSLatitude","GPSDateTime","UserComment"))
o<-order(exif$DateTimeOriginal)
ims<-ims[o]
exif<-exif[o,]
#dto<-as.POSIXct(exif[,"DateTimeOriginal"],format="%Y:%m:%d %H:%M:%S",tz="")
dto<-as.POSIXct(exif[,"DateTimeOriginal"],format="%Y:%m:%d %H:%M:%S",tz="")# |> as.character()
exif$ims<-ims
exif$dto<-dto
if(any(grep("AtomJPEG",exif$UserComment))){
  warning(sprintf("The following have been modified by AtomJPEG: %s",exif$SourceFile[grep("AtomJPEG",exif$UserComment)]))
}
if(any(is.na(exif$dto))){
  # replaces the missing dto by the dto of the closest GPSDateTime in the set and show what was used
  w<-which(is.na(exif$dto))
  replaced<-vector(mode="list",length=length(w))
  for(i in w){
    gpsdt<-as.POSIXct(exif$GPSDateTime,format="%Y:%m:%d %H:%M:%SZ",tz="GMT")
    wm<-which.min(abs(difftime(gpsdt[i],gpsdt[-w])))
    exif$dto[i]<-exif[-w,][wm,]$dto
    replaced[[match(i,w)]]<-exif[-w,][wm,c("ims","dto")]
    system(paste("xdg-open",exif$ims[i]))
    system(paste("xdg-open",replaced[[match(i,w)]]$ims))
  }
  names(replaced)<-exif$ims[w]
  print(replaced)
  warning(sprintf("The following have no DateTimeOriginal: %s",exif$SourceFile[is.na(exif$dto)]))
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
                 vmax= 3,
                 #ang=c(90,90),
                 model = "rw",
                 min.dt=0,
                 time.step = ts,
                 control = ssm_control(verbose = 1))

  plot(fit,what="fitted",type=2)
  p<-as.data.frame(grab(fit,"predicted"))
  p$acc<-sqrt((2*p$x.se)^2+(2*p$y.se)^2)
  #p$acc<-ifelse(p$acc<10,10,p$acc)
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
  x<-st_as_sf(i$preds,coords=c("x","y"),crs=epsg)
  x<-st_buffer(x,dist=sqrt((2*x$x.se)^2+(2*x$y.se)^2))
  st_as_sf(st_union(x))
}))

ssmpics<-pics |>
  st_as_sf(coords=c("x","y"),crs=epsg) |>
  st_buffer(dist=pics$acc) |>
  st_as_sf()


gpspics<-exif_read(pics$SourceFile)#,tags = c("GPSLongitude","GPSLatitude"))
if(is.null(gpspics$GPSLongitude)){
  gpspics$GPSLongitude<-NA
  gpspics$GPSLatitude<-NA
}
gpspics<-st_as_sf(gpspics[,c("GPSLongitude","GPSLatitude")],coords=c("GPSLongitude","GPSLatitude"),crs=4326,na.fail=FALSE)
gpspics<-gpspics[!st_is_empty(gpspics),]

#gpx <- lapply(gpx, function(i){i[, intersect(names(gpx[[1]]), names(gpx[[2]]))]}) ||||||||||||||||||
gpxpoints<-do.call("rbind",gpx)

mapview(ssmbuffers,col.regions="blue",alpha.regions=0.15,lwd=0) +
  mapview(gpxpoints,cex=3) +
  mapview(ssmtracks,alpha=0.20) +
  mapview(ssmpics,col="white",col.regions="red",alpha.regions=0.15) +
  mapview(gpspics,cex=3,color="black",col.regions="white",legend=FALSE)






tags<-c("-GPSLongitude=","-GPSLatitude=","-GPSHPositioningError=","-GPSLongitudeRef=","-GPSLatitudeRef=","-UserComment=","-ImageDescription=")
invisible(lapply((1:nrow(pics)),function(i){
  print(i)
  vtags<-c(pics$lon[i],pics$lat[i],pics$acc[i],ifelse(pics$lon[i] < 0, "W", "E"),"N","Location and accuracy obtained from a SSM inferred track and possibly manually adjusted","ssmtracks")
  string<-paste0(tags,vtags)
  string<-c("-overwrite_original",string)
  #string<-paste("-n",string)
  exif_call(path = pics$SourceFile[i], args = string)
}))
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


p<-st_as_sf(p,coords=c("x","y"),crs=epsg)
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
