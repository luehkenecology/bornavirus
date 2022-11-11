# set working directory
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)
setwd(PROJHOME)

# libraries
library(sp)
library(raster)

# read presence absence datas
borna <- read.csv("data/borna.csv", header = T, sep = ";")

# construct SpatialPointsDataFrame
xy_all<-data.frame(x=borna$y,
                   y=borna$x)

points_all_recent <- SpatialPointsDataFrame(coords = xy_all, 
                                            data = borna,
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

# Germany shape
DEU <- getData('GADM', country='DEU', level=1)
crs(DEU) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# pos and negative sites
png(
  file = "figs/pos_neg_borna.png", width = 6, height = 7,
  units = "in", res = 1000
)
plot(DEU, main = "presence/absence bornavirus")
points(points_all_recent[points_all_recent$borna == "negativ",], pch = 21,
       col = "black", bg = "gray")
points(points_all_recent[points_all_recent$borna == "positiv",], pch = 21, 
       col = "black", bg = "red")
dev.off()


library(ggplot2)
library(sf)

# https://www.nature.com/articles/sdata2018309
floodplain <- raster("data/EU.tif")
plot(floodplain)
floodplain2 <- crop(floodplain, DEU)
floodplain3 <- mask(floodplain2, DEU)
floodplain4 <- !is.na(floodplain3)


png(file = "figs/borna.png",width = 6, height=7 , units = 'in', res = 1000)
plot(floodplain4, col = c("white", "purple"), 
     legend = F, main= "var floodplain")
plot(DEU, add = T)
points(points_all_recent[points_all_recent$borna == "negativ",], pch = 21,
       col = "black", bg = "black")
points(points_all_recent[points_all_recent$borna == "positiv",], pch = 21, 
       col = "black", bg = "red")
dev.off()


OO <- extract(floodplain4, points_all_recent, 
              buffer = 3500)
OO2 <- unlist(lapply(OO, function(x) sum(x)))
points_all_recent$OO2 <- OO2
file <- data.frame(points_all_recent)

file$borna <- ifelse(file$borna == "positiv",
                     1,0)

ggplot(file,
       aes(OO2, borna)) +
  geom_point() +
  theme_bw()


points_all_recent$OO2 <- OO2

boxplot(points_all_recent$OO2~points_all_recent$borna)

rbind(points_all_recent, OO2)

urb <- raster("C:/Users/luehk/Meine Ablage/00001-projects/00064-bloodmeals_gradient/output/urb.tif")

OO <- extract(urb, points_all_recent, 
              buffer = 1500)
OO2 <- unlist(lapply(OO, function(x) sum(x)))

points_all_recent$OO2 <- OO2
file <- data.frame(points_all_recent)

file$borna <- ifelse(file$borna == "positiv",
                     1,0)

ggplot(file,
       aes(OO2, borna)) +
  geom_point() +
  theme_bw()

