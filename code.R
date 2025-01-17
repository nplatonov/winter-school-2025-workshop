input <- "./input"
output <- "./output"
assets <- "./assets"
resources <- "./resources"
staff <- digest::digest(getOption("sevinExpedition"),"crc32")=="bd8035bd"
isShow <- ursa:::.isRemark()
simple <- !staff & (T | !isShow) ## '|' - devel, "&" - finalize
if (!dir.exists(output))
   dir.create(output)
plutil::pluglibrary("hadley/emo")






knitr::include_graphics("assets/OSGeo4w-setup.png")

cat("\n---\nclass: middle\n\n")

knitr::include_url("https://plugins.qgis.org/plugins/?sort=-downloads",height=630)

knitr::include_url("https://www.r-project.org/",height=630)

knitr::include_url("https://cloud.r-project.org/banner.shtml",height=630)

knitr::include_url("https://cloud.r-project.org/web/packages/",height=630)

knitr::include_url("https://cloud.r-project.org/web/packages/available_packages_by_name.html",height=630)


knitr::include_url("https://www.naturalearthdata.com/downloads/",height=540)

knitr::include_url("https://osmdata.openstreetmap.de/",height=465)

knitr::include_url("https://download.gebco.net/",height=441)

knitr::include_url("https://seaice.uni-bremen.de/start/",height=630)



knitr::include_graphics(file.path(assets,"bridge.jpg"),dpi=200)

knitr::include_graphics(file.path(assets,"records.jpg"),dpi=120)

knitr::include_graphics(file.path(assets,"records1.jpg"))

knitr::include_graphics(file.path(assets,"records2.jpg"))

# ursa:::widgetize(ret,ref="widget",height="var(--scrollY)")
ret

# knitr::include_url("assets/attrtable-marmam.html",height="670px")

knitr::include_url("https://www8.garmin.com/support/download_details.jsp?id=209/",height=525)

# Sys.setenv(PATH=paste0("C:\\Software\\OSGeo4W\\apps\\Qt5\\bin;",Sys.getenv("PATH")))
# src <- dir(pattern="2023.+\\.gdb$",recursive=TRUE,full.names=TRUE)
# dst <- gsub("\\.gdb$",".gpx",src)
# ind <- which(!file.exists(dst) | file.size(dst)==0)
# if (length(ind)) {
#    cmd <- paste("gpsbabel -w -r -t -i gdb,via=0,roadbook=0"
#                ,"-f",src[ind]
#                ,"-o gpx,suppresswhite=0,logpoint=0,humminbirdextensions=0,garminextensions=0"
#                ,"-F",dst[ind])
#    print(cmd)
#    sapply(cmd,system)
# }

gpx <- readLines("./input/sample.gpx")
cat(gpx,sep="\n")

knitr::include_graphics(file.path(assets,"AddVector.jpg"),dpi=300)

knitr::include_graphics(file.path(assets,"SelectVector.jpg"),dpi=300)

knitr::include_graphics(file.path(assets,"SelectLayers.jpg"),dpi=300)

knitr::include_graphics(file.path(assets,"TrackInQGIS.jpg"),dpi=300)

list1 <- dir(path="input/2023-07-18",pattern="^2023.+\\.jpg",full.names=!FALSE)
list2 <- file.info(list1)
rownames(list2) <- basename(list1)
list2$atime <- list2$ctime <- NULL
list2

knitr::include_graphics(file.path(assets,"AddRaster.jpg"),dpi=300)

knitr::include_graphics(file.path(assets,"SelectRaster.jpg"),dpi=300)

knitr::include_graphics(file.path(assets,"RasterProperties.jpg"),dpi=300)

knitr::include_graphics(file.path(assets,"qgisEXIF.jpg"),dpi=300)

sf::gdal_utils("info","./input/DSC_2414.JPG")



# NA

knitr::include_graphics(file.path(assets,"aerial1.jpg"),dpi=300)

knitr::include_graphics(file.path(assets,"aerial2.jpg"),dpi=300)



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

# spatial_write(trkdaily,"./output/trackline_daily.geojson")

# mapview::mapview(trkln)

mapview::mapview(trkln) |> ursa:::widgetize()

knitr::include_graphics(file.path(assets,"track-QGIS.jpg"))

















write.csv(a,"./output/mammals_subset.csv",row.names=FALSE)



knitr::include_graphics(file.path(assets,"importCSV.jpg"),dpi=300)

# NA

knitr::include_graphics("assets/findings.png")

# NA

ref <- ursa_read("./output/studyarea.tif")
ursa_grid(ref)

display(ref,legend=NULL,height=200)

# NA

# qs::qsave(land,"./resources/land-polygons.qs")

land <- qs::qread("./resources/land-polygons.qs")

# NA

dist2coast <- ursa_read("./output/dist2coast.tif")

display(dist2coast,blank="white",pointsize=12,coast.fill="#00000010")

# NA

# spatial_write(list(seg=seg,loc=loc,cl=cl),"./resources/dist2coast.sqlite")

bundle <- spatial_read("./resources/dist2coast.sqlite")
loc <- bundle$loc
cl <- bundle$cl
seg <- bundle$seg
rm(bundle)

spatial_data(loc)[sort(sample(seq(nrow(loc)),7)),]
m <- mapview::mapview(spatial_geometry(loc),cex=3,col.regions="red",layer.name="finding")+
     mapview::mapview(spatial_geometry(cl),cex=2,col.regions="blue",layer.name="coast")+
     mapview::mapview(seg,popup="dist",layer.name="segment")

# m

ursa:::widgetize(m)

src <- "./input/gebco_2024_n82.0_s64.0_w30.0_e90.0.tif"
depth <- ursa_read(src)
str(ursa_grid(depth))
display(depth,pointsize=12)

print(gebco <- ursa:::.gdalwarp(src,grid=ursa_grid("./output/studyarea.tif")))
ursa_write(gebco,"./output/gebco.tif")

display(gebco,blank="white",pointsize=12,coast_fill="#00000010")



da <- loc[loc$'Вид' %in% c("Лахтак","Морж"),]

# ggstatsplot::ggbetweenstats(da,'Вид',depth)

knitr::include_graphics("assets/between-depth.png")

# ggstatsplot::ggbetweenstats(da,'Вид',coast)

knitr::include_graphics("assets/between-coast.png")

x <- da[da$'Вид'=="Лахтак",]$'coast'
summary(x)
y <- da[da$'Вид'=="Морж",]$'coast'
summary(y)

knitr::include_graphics("assets/which-test.webp")

shapiro.test(x)

shapiro.test(y)

t.test(x,y,pair=FALSE)



# options(width=160)

print(sessionInfo())
