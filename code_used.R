.libPaths("Rlibs")
pkgList <- c("png","mapview","adehabitatLT","readxl","tidyr","lwgeom"
            ,"geosphere","ggstatsplot","ursa")
repos <- "https://cloud.r-project.org/"
type <- if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
available <- sapply(pkgList,function(pkg) {
   if (requireNamespace(pkg))
      return(TRUE)
   install.packages(pkg,repos=repos,type=type)
   requireNamespace(pkg)
})
if (packageVersion("ursa")<"3.11.2")
   install.packages("ursa",repos=repos,type=type)
packageVersion("ursa")
available
c('Everything is ok?'=all(available))
Sys.setenv(PATH=paste0("C:\\Software\\OSGeo4W\\apps\\Qt5\\bin;",Sys.getenv("PATH")))
src <- dir(pattern="2023.+\\.gdb$",recursive=TRUE,full.names=TRUE)
dst <- gsub("\\.gdb$",".gpx",src)
ind <- which(!file.exists(dst) | file.size(dst)==0)
if (length(ind)) {
   cmd <- paste("gpsbabel -w -r -t -i gdb,via=0,roadbook=0"
               ,"-f",src[ind]
               ,"-o gpx,suppresswhite=0,logpoint=0,humminbirdextensions=0,garminextensions=0"
               ,"-F",dst[ind])
   print(cmd)
   sapply(cmd,system)
}
sf::gdal_utils("info","./input/DSC_2414.JPG")
tz <- c('D7500'="UTC"
       ,'Oregon'="Asia/Krasnoyarsk")
user <- "D7500"
src <- "./input/DSC_2414.JPG"
file.info(src)
photoID <- gsub(".*\\D(\\d{4})\\D.*","\\1",basename(src))
a <- sf::gdal_utils("info",src,quiet=TRUE)
a <- strsplit(a,split="\\n")[[1]]
patt1 <- "\\s*EXIF_DateTimeOriginal=(.+)"
if (length(ind1 <- grep(patt1,a))==1) {
   t3 <- gsub(patt1,"\\1",a[ind1])
   t3 <- as.POSIXct(t3,format="%Y:%m:%d %H:%M:%S",tz="UTC")
   patt2 <- "\\s*EXIF_OffsetTimeOriginal=(.+)"
   if (length(ind2 <- grep(patt2,a))==1) {
      tz3 <- gsub(patt2,"\\1",a[ind2])
      print(c(TZ=tz3))
      t3 <- t3-(as.difftime(as.numeric(gsub("^(.+):(.+)$","\\1",tz3))+
            as.numeric(gsub("^(.+):(.+)$","\\2",tz3))/60,units="hours"))
      t3 <- as.POSIXct(t3,tz=tz[user])
   }
   print(c(t3=t3))
   print(c(photoID=photoID))
   dst <- file.path("./output",paste0(format(t3,"%Y%m%d-%H%M%S-"),photoID
                                 ,tolower(gsub(".*(\\..+$)","\\1",basename(src)))
                                 ))
   print(data.frame('src'=src,'dst'=dst))
}
if (!dir.exists("./output"))
   dir.create("./output")
# file.rename(src,dst)
file.copy(src,dst,overwrite=TRUE,copy.date=TRUE)
Sys.setenv(TZ="UTC")
t0 <- as.POSIXct("2023-07-04")
'getTrack' <- function(device,vehicle=FALSE,ltraj=FALSE,inside=NULL) {
   if (missing(device))
      return(NULL)
   if (!ursa::is_spatial(device)) {
      list1 <- dir(path=file.path("./input",device)
                  ,pattern=".*20[x2]3.+\\.gpx$",full.names=TRUE)
      res <- lapply(list1,\(fname) {
         a <- sf::st_read(fname,layer="track_points",quiet=TRUE)[,1:5]
         a <- a[!is.na(a$time),]
         if (!ursa::spatial_count(a))
            return(NULL)
         taf <- table(track_fid=a$track_fid)
         tas <- table(track_seg_id=a$track_seg_id)
         if (length(tas)>length(taf)) {
            b <- by(a,a$track_seg_id,\(x) x)
         }
         else {
            b <- by(a,a$track_fid,\(x) x)
         }
         a <- lapply(b,\(x) {
            rt <- range(x$time)
            y <- ursa::trackline(x)
            len <- ursa::spatial_length(y)
            dur <- as.numeric(diff(range(x$time)),units="hours")
            speed <- len*1e-3/dur
            sa <- data.frame(n=ursa::spatial_count(x),length=len,dur=dur,speed=speed
                            ,from=rt[1],to=rt[2])
            cond1 <- sa$dur>10/60 & sa$n>10
            sa <- sa[cond1,]
            if (!cond1)
               return(NULL)
            x$device <- tolower(device)
            x$segment <- digest::digest(x,"crc32")
            tail(x[,c("time","ele","device","segment")],-1)
         }) |> do.call(ursa::spatial_bind,args=_)
         a
      }) |> ursa::spatial_bind()
      res <- res[order(res$time),]
      if (vehicle) {
         if (length(ind <- grep("^(cruise|road)",res$survey,invert=TRUE))) {
            res <- res[-ind,]
         }
      }
      if (!is.null(inside)) {
         cond <- vector("list",nrow(inside))
         for (i in seq(nrow(inside))) {
            if (!inside$fill[i])
               cond[[i]] <- integer()
            else {
               j <- which(res$time>inside$begin[i] & res$time<inside$end[i])
               cond[[i]] <- j
            }
         }
         cond <- sort(unlist(cond))
         if (!length(cond))
            return(NULL)
         res <- res[cond,]
      }
      res$id <- "merge"
   }
   else {
      res <- device
      ltraj <- TRUE
   }
   if (!ltraj)
      return(res)
   if (length(ind <- which(duplicated(res$time)))) {
      res <- res[-ind,]
   }
   res2 <- adehabitatLT::as.ltraj(sf::st_coordinates(res)
                                 ,date=res$time
                                 ,id=res$id
                                 ,proj4string=sp::CRS(sf::st_crs(res)$proj4string)
                                 )
   opW <- options(warn=-1)
   res3 <- adehabitatLT::cutltraj(res2,"dt>c(days=1)*c(hour=1)*c(min=12)*c(sec=60)"
                                 ,nextr=TRUE)
   options(opW)
   res3
}
'inside' <- function(device,dt=as.difftime(12,units="mins"),dl=50) {
   if (is.null(device))
      return(NULL)
   ind1 <- which(diff(device$time)>dt)
   begin <- c(t0,device$time[ind1],max(device$time)+1)
   end <- c(min(device$time)-1,device$time[ind1+1],as.POSIXct(Sys.Date()+1))
   ret <- data.frame(from=c(NA,ind1,NA),to=c(NA,ind1+1,NA)
                    ,device=head(device$device,1)
                    ,begin=begin
                    ,end=end
                    ,length=NA
                    ,fill=TRUE
                    )
   for (i in seq(nrow(ret)) |> sample()) {
      ind <- c(ret$from[i],ret$to[i])
      if (anyNA(ind))
         next
      ret$length[i] <- ursa::trackline(device[ind,]) |> ursa::spatial_length() |> round()
   }
   ret$dt <- as.numeric(ret$end-ret$begin,units="hours") |> round(1)
   ret$fill[ret$length<dl] <- FALSE
   ret
}
'suppl' <- function(main,suppl,vehicle=FALSE) {
   more <- getTrack(suppl,inside=inside(main),vehicle=vehicle,ltraj=!TRUE)
   main <- ursa::spatial_bind(main,more)
   main <- main[order(main$time),]
   main
}
priority <- list(oregon=c("Oregon","Zhenya"))
pname <- names(priority)
for (i in seq_along(priority)) {
   message(pname[i],":")
   vehicle <- pname[i] %in% "general"
   main <- getTrack()
   for (device in priority[[i]]) {
      message("   ",device,":")
      main <- suppl(main,device,vehicle=vehicle)
   }
   print(getTrack(main,ltraj=TRUE))
   main <- main[,c("id","time","ele","device")]
   trkpt <- file.path("./output",paste0("track-",pname[i],".geojson"))
   ursa::spatial_write(main,trkpt)
   print(file.info(trkpt)[,"size",drop=FALSE])
   tr <- ursa::trackline(main,by=main$id,connect="united")
   trkln <- file.path(dirname(trkpt),gsub("^track","trackline",basename(trkpt)))
   ursa::spatial_write(tr,trkln)
   print(file.info(trkln)[,"size",drop=FALSE])
}
ursa::glance("./output/track-oregon.geojson",field="device",plot.lwd=0.001)
ursa::glance("./output/trackline-oregon.geojson")
require(ursa)
print(trkpt <- spatial_read("./input/track-general.geojson"))
print(trkln <- trackline(trkpt))
glance(trkln,style="2gis")
trkdaily <- trackline(trkpt,by=list('Дата'=as.Date(trkpt$time)))
ct <- colorize(trkdaily$'Дата',pal.bright=127,pal.rotate="circle") |> ursa_colortable()
glance(trkdaily["Дата"],col=ct,resetGrid=TRUE,blank="aliceblue",coast.fill="lightyellow"
      ,basemap.order="before",pointsize=12)
mapview::mapview(trkln)
Sys.setenv(TZ="UTC")
a <- readxl::read_excel("./input/marmam.xlsx",sheet="mammals")
ind <- is.na(a[["Повтор"]])
a <- a[ind,]
d3 <- a[["Дата"]] |> as.Date()
a[["Дата"]] <- tidyr::fill(data.frame(d3=d3),"d3",.direction="down")[[1]]
a$time <- Sys.time()
a$time[] <- NA
a$lon <- NA_real_
a$lat <- NA_real_
trkFname <- "./input/Oregon/Track_2023-07-18 082447.gpx"
print(sf::st_layers(trkFname))
trk <- sf::st_read(trkFname,layer="track_points",quiet=TRUE)[c("time","ele")]
print(trk)
wptFname <- "./input/Oregon/Waypoints_18-JUL-23.gpx"
print(sf::st_layers(wptFname))
wpt <- sf::st_read(wptFname,layer="waypoints",quiet=TRUE)[,c("time","name","ele","sym")]
wpt$name <- paste0("О",wpt$name)
print(wpt)
'wptTime' <- function(a,wpt) {
   patt <- ".*([A-ZА-Я]\\d{3}).*"
   pt <- a[["Точка трека"]]
   ind <- grep(patt,pt)
   pt[ind] <- gsub(patt,"\\1",pt[ind])
   ind2 <- match(wpt$name,pt[ind])
   ind2pt <- ind[na.omit(ind2)]
  # print(ind2pt)
   ind2w <- which(!is.na(ind2))
  # print(wpt[ind2w,])
  # a[ind2pt,c("lon","lat")] <- ursa::spatial_coordinates(wpt[ind2w,])
   a[["time"]][ind2pt] <- wpt[["time"]][ind2w]
   print(a[ind2pt,c("Время UTC","time","Точка трека","Вид")])
   a
}
a <- wptTime(a,wpt)
'photoTime' <- function(a,photoDir) {
   list1 <- dir(path=file.path("./input",photoDir)
               ,pattern="\\d{4}\\.jpg$",full.names=TRUE)
   list2 <- basename(list1)
   photo <- a[["Точка трека"]]
   ind <- grep("\\D*\\d{4}\\D*",photo)
   ph <- gsub("[А-Я]\\d{3}\\D*","",photo[ind])
   ph <- gsub("\\s+","",ph)
   ph <- gsub("^\\D|\\D$","",ph)
   ph <- strsplit(ph,split="(\\s+|,\\s*)")
   ph <- lapply(ph,\(x) {
      y <- strsplit(x,split="\\s*-\\s*")[[1]]
      if (length(y)==1)
         return(y)
      y <- as.integer(y)
      y <- sprintf("%04d",sort(seq(y[1],y[2])))
   })
   d3 <- a[["Дата"]][ind]
   found <- vector("list",length(d3))
   for (i in seq_along(d3) |> sample()) {
      if (!is.na(a[["time"]][ind][i]))
         next
      patt <- paste0(format(d3[i],"%Y%m%d"),"-\\d{6}-",ph[[i]],"\\.jpg")
      ind2 <- unlist(lapply(patt,grep,list2))
      if (!length(ind2))
         next
      if (length(ind2)>1)
         ind2 <- sample(ind2,1)
      found[[i]] <- as.POSIXct(gsub(".*(\\d{8}-\\d{6}).*","\\1",list2[ind2])
                              ,format="%Y%m%d-%H%M%S")
      a[["time"]][ind][i] <- found[[i]]
   }
   ind2 <- which(sapply(found,\(x) length(x)>0))
   ind3 <- ind[ind2] 
   print(a[ind3,c("Дата","Точка трека","Вид","Субстрат","time")])
   a
}
a <- photoTime(a,"2023-07-18")
'manualTime' <- function(a) {
   t3 <- a[["Точка трека"]]
   ind <- which(grepl("^\\d{1,2}\\:\\d{2}(\\:\\d{2})*$",t3))
   if (length(ind2 <- grep("^\\d{1,2}\\:\\d{2}$",t3[ind])))
      t3[ind][ind2] <- paste0(t3[ind][ind2],":30")
   ind3 <- is.na(a$time[ind])
   a$time[ind[ind3]] <- paste(format(a[["Дата"]][ind[ind3]],"%Y-%m-%d"),t3[ind[ind3]])
   ind4 <- which(a$time[ind]>=head(trk$time,1) & a$time[ind]<=tail(trk$time,1))
   print(a[ind[ind4],c("Дата","Точка трека","Вид","time","lon","lat")])
   a
}
a <- manualTime(a)
a <- a[!is.na(a$time),]
a <- a[a$time>=head(trk$time,1) & a$time<=tail(trk$time,1),]
print(a[,c("Точка трека","Вид","time","lon","lat")])
'getCoordsFromPoint' <- function(a,gpx) {
   ind <- match(a$time,gpx$time)
   if (all(is.na(ind)))
      return(a)
   ind1 <- na.omit(ind)
   ind2 <- which(!is.na(ind))
   ind2 <- ind2[is.na(a$lon[ind2]) | is.na(a$lat[ind2])]
   xy <- ursa::spatial_coordinates(gpx)[ind1,]
   a$lon[ind2] <- xy[,1]
   a$lat[ind2] <- xy[,2]
   a
}
a <- getCoordsFromPoint(a,wpt)
print(a[,c("Точка трека","Вид","time","lon","lat")])
'getCoordsBetweenPoints' <- function(a,gpx) {
   if (!length(ind <- which(is.na(a$lon) | is.na(a$lat))))
      return(a)
   for (i in ind |> sample()) {
      ind4 <- which(trk$time-a$time[i]>0) |> head(1)
      trk2 <- trk[ind4+c(-1,0),]
      trk2 <- ursa:::spatialize(trk2,style="stere")
      sc <- as.numeric(a$time[i]-trk2$time[1],units="secs")/
            as.numeric(trk2$time[2]-trk2$time[1],units="secs")
      xy2 <- ursa::spatial_coordinates(trk2)
      xy <- data.frame(lon=xy2[1,1]+sc*(xy2[2,1]-xy2[1,1])
                 ,lat=xy2[1,2]+sc*(xy2[2,2]-xy2[1,2]))
      xy <- sf::st_as_sf(xy,coords=c("lon","lat"),crs=sf::st_crs(trk2))
      xy <- ursa::spatial_coordinates(sf::st_transform(xy,4326))
      a$lon[i] <- xy[,1]
      a$lat[i] <- xy[,2]
   }
   a
}
a <- getCoordsBetweenPoints(a,trk)
print(a[,c("Точка трека","Вид","time","lon","lat")])
'getbearing' <- function(device,time,distance,bearing) {
   bearing1 <- bearing
   bearing2 <- NA
   if (!is.na(bearing)) {
      pattHHMM <- "(\\d{1,2})\\:(\\d{2})"
      if (grepl("\\d+(T|Т)$",bearing)) {
         bearing2 <- as.numeric(gsub("\\D","",bearing))
      }
      else if (grepl(pattHHMM,bearing)) {
         hh <- as.numeric(gsub(pattHHMM,"\\1",bearing))
         mm <- as.numeric(gsub(pattHHMM,"\\2",bearing))
         bearing <- 30*(hh+mm/60)
      }
      else {
         bearing <- as.numeric(gsub("\\s*ч$","",bearing))
         if ((bearing>=0)&&(bearing<=24))
            bearing <- bearing*30
      }
   }
   else
      bearing <- 0
   if (is.na(distance))
      distance <- 0
   g <- device
   ind <- match(time,g$time)
   if (is.na(ind)) {
      ind <- which(g$time>time)[1]
      if (is.na(ind))
         stop("out of temporal range?")
   }
   g3 <- ursa:::spatialize(g[ind+seq(-2,2),"time"],resetGrid=TRUE,style="stere")
   xy <- ursa::spatial_coordinates(g3)
   pca <- prcomp(xy,scale=FALSE)
   direction <- unname(pca$rotation[,1])
   if (direction[1]==0)
      theta <- pi/2*sign(direction[2])
   else
      theta <- atan(direction[2]/direction[1])
  # print(theta*180/pi)
   if (direction[1]<0) {
      theta <- pi+theta
   }
   theta <- pi/2-theta
   x <- as.numeric(g3$time)
   y <- unname(predict(pca)[,1])
   n <- length(x)
   slope <- 3.6*(mean(x*y)-mean(x)*mean(y))/var(x)*n/(n-1)
   if (slope<0) {
      theta <- theta-pi
      slope <- -slope
   }
   g2 <- g[ind+c(-1,0),]
   tr <- ursa::trackline(g2)
   trlen <- ursa::spatial_length(tr)
   trdur <- as.numeric(difftime(g2$time[2],g2$time[1]),"secs")
   trspd <- trlen/trdur*3.6
   tr <- ursa::spatial_coordinates(sf::st_segmentize(tr,trlen/1000))[[1]]
   t3 <- as.numeric(g2$time)
   scale <- (as.numeric(time)-t3[1])/(t3[2]-t3[1])
   ind <- round((nrow(tr)-1)*scale+1)
   crd <- data.frame(t(tr[ind,]))
   heading <- lwgeom::st_geod_azimuth(g2)
   units(heading) <- "degrees"
   if (is.na(bearing2))
      bearing2 <- as.numeric(heading)+bearing
   ep <- geosphere::destPoint(crd,b=bearing2,d=distance)
   strip <- sin(as.numeric(bearing)*pi/180)*as.numeric(distance)
   strip[abs(strip)>5000] <- round(strip[abs(strip)>5000],-3)
   strip[abs(strip)>2000] <- round(strip[abs(strip)>2000],-2)
   strip[abs(strip)>=0] <- round(strip[abs(strip)>=0],-1)
   ret <- data.frame(time=time,observer_lon=crd[,1],observer_lat=crd[,2]
                    ,target_lon=ep[,1],target_lat=ep[,2]
                    ,in_strip=strip
                    ,speed=slope,heading=theta*180/pi
                    ,check.names=FALSE
                    )
   rownames(ret) <- NULL
   ret$time <- NULL
   ret
}
'observationState' <- function(a,trk) {
   colBearing <- grep("(пеленг|азимут)",colnames(a),ignore.case=TRUE,value=TRUE)
   colDistance <- grep("дистанц|(расст.*набл)",colnames(a),ignore.case=TRUE,value=TRUE)
   lapply(seq(nrow(a)),\(i) {
      getbearing(device=trk,time=a$time[i]
                ,distance=a[[colDistance]][i],bearing=a[[colBearing]][i])
   }) |> do.call(rbind,args=_)
}
b <- observationState(a,trk)
# b <- cbind(a[,c("Вид","time")],b)
print(b,digits=4)
a <- sf::st_as_sf(a,coords=c("lon","lat"),crs=4326)
print(a[,c("Вид","Кол-во","Субстрат")])
dst <- "./output/mammals_subset.geojson"
ursa::spatial_write(a,dst)
print(file.info(dst)[,"size",drop=FALSE])
session_grid(NULL)
trkpt <- ursa:::spatialize("./input/track-general.geojson",style="laea",lon0=60)
session_grid(expand=1.1)
loc <- ursa:::spatialize("./input/marmam.geojson")
compose_open()
panel_new("white")
panel_plot(trackline(trkpt),lwd=3,col="grey30",alpha=0.3)
panel_decor(coast.fill="#00000010",margin=c(F,T,T,F),lang="ru")
ursa:::panel_cluster(loc["Вид"],legend="topleft",bg="#FFFFFFAF")
panel_annotation(lon=40,lat=72,label=expression(italic("БАРЕНЦЕВО МОРЕ"))
                ,fg="white",bg="grey70",cex=1.1)
panel_annotation(lon=80,lat=78,label=expression(italic("КАРСКОЕ МОРЕ"))
                ,fg="white",bg="grey70",cex=1.1)
compose_close()
session_grid(NULL)
a <- ursa:::spatialize("./input/track-general.geojson",style="stere",lon0=60)
g1 <- regrid(spatial_grid(a),res=2000,border=100)
print(g1)
session_grid(g1)
ref <- ursa(0L)
ursa_write(ref,"./output/studyarea.tif")
display(ref,legend=NULL)
src <- "https://osmdata.openstreetmap.de/download/simplified-land-polygons-complete-3857.zip"
osmdata <- file.path("./output",basename(src))
if (!file.exists(osmdata))
   download.file(src,osmdata,mode="wb")
list1 <- unzip(osmdata,exdir=tempdir(),junkpaths=TRUE)
session_grid(NULL)
land <- spatial_read(list1[grep("\\.shp$",basename(list1))])
file.remove(list1) |> invisible()
cntr <- spatial_centroid(land) |> spatial_coordinates()
ind <- which.min(cntr[,2])
land <- land[-ind,]
ref <- ursa_read("./output/studyarea.tif")
ref <- regrid(ref,mul=1/4)
extended <- spatial_buffer(ursa:::spatialize(ursa_bbox(ref)),500*1e3)

coast <- spatial_intersection(spatial_geometry(land),extended)
coast <- sf::st_simplify(coast,ursa(ref,"cellsize"),preserveTopology=TRUE) |>
         sf::st_cast("MULTIPOLYGON")
session_grid(ref)
dist2coast <- ursa:::.dist2(ref,coast)["dist"]*1e-3
ursa_write(dist2coast,"./output/dist2coast.tif")
display(dist2coast,blank="white",pointsize=12,coast.fill="#00000010")
loc <- ursa:::spatialize("./input/marmam.geojson",resetGrid=TRUE)[c("time","Вид")]
b <- spatial_buffer(ursa:::spatialize(spatial_bbox(loc)),100*1e3)
b <- sf::st_segmentize(b,100)
coast <- spatial_intersection(b,spatial_geometry(land))
coast <- sf::st_segmentize(coast,100)
coast <- sf::st_cast(coast,"POINT")
dist2coast <- ursa:::.dist2(loc,coast)
cl <- coast[dist2coast$ind,]
loc$coast <- dist2coast$dist
seg <- cbind(spatial_coordinates(loc),spatial_coordinates(cl))
seg <- apply(seg,1,\(x) sf::st_linestring(matrix(x,ncol=2,byrow=TRUE)),simplify=FALSE)
seg <- sf::st_sfc(seg,crs=sf::st_crs(loc))
spatial_data(seg) <- data.frame(dist=round(loc$coast*1e-3,1))
spatial_data(loc)[sort(sample(seq(nrow(loc)),7)),]
m <- mapview::mapview(spatial_geometry(loc),cex=3,col.regions="red",layer.name="finding")+
     mapview::mapview(spatial_geometry(cl),cex=2,col.regions="blue",layer.name="coast")+
     mapview::mapview(seg,popup="dist",layer.name="segment")
m
src <- "./input/gebco_2024_n82.0_s64.0_w30.0_e90.0.tif"
depth <- ursa_read(src)
str(ursa_grid(depth))
display(depth,pointsize=12)
print(gebco <- ursa:::.gdalwarp(src,grid=ursa_grid("./output/studyarea.tif")))
ursa_write(gebco,"./output/gebco.tif")
display(gebco,blank="white",pointsize=12,coast_fill="#00000010")
session_grid(NULL)
gebco <- ursa_read("./output/gebco.tif")
session_grid(gebco)
if (!exists("loc"))
   loc <- spatial_read("./input/marmam.geojson")[c("time","Вид")]
loc$gebco <- value_xy(gebco,loc)[1,]
session_grid(NULL)
depth <- ursa_read("input/gebco_2024_n82.0_s64.0_w30.0_e90.0.tif")
loc$depth <- value_xy(depth,spatial_transform(loc,depth))[1,]
spatial_data(loc)[sort(sample(seq(nrow(loc)),14)),]
da <- loc[loc$'Вид' %in% c("Лахтак","Морж"),]
ggstatsplot::ggbetweenstats(da,'Вид',depth)
ggstatsplot::ggbetweenstats(da,'Вид',coast)
x <- da[da$'Вид'=="Лахтак",]$'coast'
summary(x)
y <- da[da$'Вид'=="Морж",]$'coast'
summary(y)
shapiro.test(x)
shapiro.test(y)
t.test(x,y,pair=FALSE)
print(sessionInfo())
