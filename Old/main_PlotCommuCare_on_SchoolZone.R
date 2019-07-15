# Description -------------------------------------------------------------

# created: 190713


# setting -----------------------------------------------------------------


# Settings ----------------------------------------------------------------

dir.Data   <- "../Data"
dir.Output <- "../Output" 
dir.Sub    <- "./Sub"

fn.Data_CommCareCenter   <-  'Wakayama_CommuCareCenter_MasterAnaData.csv'
fn.Data_ApothLoc         <-  "Wakayama_MasterAnaData.csv"
fn.Data_ScRegionalPos    <-  'Wakayama_ScRegionalPos.csv'

fn.ShapeP.SchoolRegion <- "/190626/A32-16_30_GML/shape/A32P-16_30.shp"
fn.Shape.SchoolRegion <- "/190706/A32-13_30/shp/A32-13_30.shp"

fn.Shape_19.GovRegion <- '/190706/N03-190101_30_GML/N03-19_30_190101.shp'


fn.Sub_require_libraries <- "require_libraries.R"

crs_default <- '+init=epsg:4612'
col.NumbFullTime <- c(
  "red", "purple", "green", "cyan", "blue", "black", "black"
  )

# Load packages --------------------------------------------------------

source(sprintf("%s/%s", dir.Sub, fn.Sub_require_libraries))


# test for "sp" package -----------------------------------------------

ScRegionalPos <- read.csv(
  sprintf(
    "%s/%s",
    dir.Data,
    fn.Data_ScRegionalPos
  ),
  fileEncoding = "utf-8"
)

# GEOGCSがGCS_JGD_2000、DATUMがJGD_2000、
# SPHEROIDがGRS_1980,6378137.0,298.257222101、
# PRIMEMがGreenwich",0.0、UNITがDegree",0.0174532925199433」とな
# -> 'EPSG' is handy to specification (JGD2000 is '4612' in EPSG)
#

crdref <- CRS(crs_default)

df.SpatialPoints.ScRegionalPos <- 
  SpatialPointsDataFrame(
    SpatialPoints(
      ScRegionalPos[,c("V2","V1")], 
      proj4string = crdref
      ),
  data = ScRegionalPos
  )

showDefault(
  df.SpatialPoints.ScRegionalPos
  )

pols <- raster::spPolygons(
  df.SpatialPoints.ScRegionalPos, 
  crs=crdref
  )

plot(
  pols, axes=TRUE, las=1
  )


Shape.SchoolRegion <- shapefile(
  sprintf(
    "%s/%s", dir.Data, 
    fn.Shape.SchoolRegion
    )
  )
Shape_19.GovRegion <- shapefile(
  sprintf(
    "%s/%s", dir.Data, 
    fn.Shape_19.GovRegion
    )
  )

ShapeP.SchoolRegion <- shapefile(
  sprintf(
    "%s/%s", dir.Data, 
    fn.ShapeP.SchoolRegion
    )
  )

plot(ShapeP.SchoolRegion)
plot(Shape.SchoolRegion)
plot(Shape_19.GovRegion)

RasterShape.SchoolRegion <- raster(ShapeP.SchoolRegion)
  # sprintf(
  #   "%s/%s", dir.Data, fn.Shape.SchoolRegion
  # )
RasterShape_19.GovRegion <- raster(Shape_19.GovRegion)


df.Shape.SchoolRegion <- data.frame(Shape.SchoolRegion)
df.Shape_19.GovRegion    <- data.frame(Shape_19.GovRegion)



RasterShape.SchoolRegion <- raster(ShapeP.SchoolRegion)
# sprintf(
#   "%s/%s", dir.Data, fn.Shape.SchoolRegion
# )


df.Shape.SchoolRegion <- data.frame(Shape.SchoolRegion)


# Spatial queries for location of apotheek ---------------------------------------------------------

df.Wakayama_MasterAnaData <- read.csv(
  sprintf("%s/%s", dir.Data, fn.Data_ApothLoc),
  fileEncoding = "utf-8"
  )

pts <- as.matrix(
  df.Wakayama_MasterAnaData[,c("Loc_2","Loc_1","Numb.FullTime","Numb.PartTime")]
  )

spts <- SpatialPoints(
  pts[,c("Loc_2","Loc_1")],
  proj4string=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  )
spts <- spTransform(spts,CRS('+init=epsg:4612'))

# Spatial queries for community care center ---------------------------------------------------------

df.Wakayama_CommuCareCentr <- read.csv(
  sprintf("%s/%s", dir.Data, fn.Data_CommCareCenter),
  fileEncoding = "utf-8"
)

pts_CommuCareCentr <- as.matrix(
  df.Wakayama_CommuCareCentr[,c("Loc_2","Loc_1","Numb.FullTime","Numb.PartTime")]
)

spts_CommuCareCentr <- SpatialPoints(
  pts_CommuCareCentr[,c("Loc_2","Loc_1")],
  proj4string=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
)

spts_CommuCareCentr <- spTransform(spts_CommuCareCentr,CRS('+init=epsg:4612'))

plot(Shape_19.GovRegion, col='light gray', lwd=0.05)
plot(Shape.SchoolRegion, col='light blue', lwd=0.05, add=TRUE)
points(
  spts_CommuCareCentr,
  col='black',
  pch='+',
  cex=1.5
  )
points(
  spts,
  col=col.NumbFullTime[as.factor(pts[,"Numb.FullTime"])],
  pch=10,
  cex=0.5
  )
#text(spts, sprintf("[%s(%s)]", pts[,"Numb.FullTime"], pts[,"Numb.PartTime"]), col=pts[,"Numb.FullTime"], font=1, cex=0.5)
lines(Shape.SchoolRegion, col='blue', lwd=0.05)

legend()


# Spatial queries ---------------------------------------------------------

plot(ShapeP.SchoolRegion)


col.NumbFullTime <- c(
  "red", "purple", "green", "cyan", "blue", "black", "black"
  )

df.Wakayama_ScRegionalPos <- read.csv(
  sprintf("%s/%s", dir.Data, "Wakayama_ScRegionalPos.csv"),
  fileEncoding = "utf-8"
  )

pts_ScRegionalPos <- as.matrix(
  df.Wakayama_ScRegionalPos[,c("V2","V1","ID")]
  ) %>%
  as.data.frame()%>%
  mutate(
    V2=as.numeric(V2),
    V1=as.numeric(V1)
     )#%>%
  # as.matrix()

spts_ScRegionalPos <- SpatialPoints(
  pts_ScRegionalPos[,c("V2","V1")],
  proj4string=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
)
spts_ScRegionalPos <- spTransform(
  spts_ScRegionalPos,
  CRS('+init=epsg:4612')
  )

plot(Shape.SchoolRegion, col='light blue', lwd=0.05)
points(
  spts_ScRegionalPos,
  col="red",
  pch=1,
  cex=0.1
  )
points(
  spts,
  col="blue",
  pch=1,
  cex=0.1
  )
#text(spts, sprintf("[%s(%s)]", pts[,"Numb.FullTime"], pts[,"Numb.PartTime"]), col=pts[,"Numb.FullTime"], font=1, cex=0.5)
lines(Shape.SchoolRegion, col='blue', lwd=0.05)

legend()



# Nagasaki ----------------------------------------------------------------

fn.ShapeP.SchoolRegion_Nag <- "/190626/A32-16_30_GML/shape/A32P-16_30.shp"
fn.Shape.SchoolRegion_Nag <- "/190626/A32-16_30_GML/shape/A32-16_30.shp"

df.Nagasaki_MasterAnaData <- read.csv(sprintf("%s/%s", dir.Data, "Nagasaki_MasterAnaData.csv"))

pts <- as.matrix( df.Nagasaki_MasterAnaData[,c("Loc_2","Loc_1","Numb.FullTime","Numb.PartTime")])
spts <- SpatialPoints(
  pts[,c("Loc_2","Loc_1")], 
  proj4string=crs(Shape.SchoolRegion_Nagasaki)
  )
plot(Shape.SchoolRegion, col='light blue', lwd=2)
points(spts, col='black', pch=1, cex=1)
text(spts, 1:nrow(pts), col='red', font=2, cex=0.5)
lines(Shape.SchoolRegion, col='blue', lwd=2)

plot(Shape.SchoolRegion, add=TRUE)




