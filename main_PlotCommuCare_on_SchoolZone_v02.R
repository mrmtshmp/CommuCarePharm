# Description -------------------------------------------------------------

# created: 190713


# setting -----------------------------------------------------------------


# Settings ----------------------------------------------------------------

dir.Data   <- "../Data"
dir.Output <- "../Output" 
dir.Sub    <- "./Sub"

fn.Data_CommCareCenter   <-  'Wakayama_CommuCareCenter_MasterAnaData.csv'
fn.Data_ApothLoc         <-  "Wakayama_MasterAnaData.csv"
fn.Data_ApothLoc.Mie     <-  "Mie_MasterAnaData.csv"
fn.Data_ApothLoc.OsakaNara   <-  "OsakaNara_MasterAnaData.csv"
fn.Data_ScRegionalPos    <-  'Wakayama_ScRegionalPos.csv'

fn.ShapeP.SchoolRegion <- "/190626/A32-16_30_GML/shape/A32P-16_30.shp"
fn.Shape.SchoolRegion  <- "/190706/A32-13_30/shp/A32-13_30.shp"

fn.Shape_19.GovRegion  <- '/190706/N03-190101_30_GML/N03-19_30_190101.shp'

fn.mesh.popEst         <- '/190710/500m_mesh_suikei_2018_shape/500m_mesh_suikei_2018_shape_30/'#500m_mesh_2018_30.shp'

fn.Sub_require_libraries <- "require_libraries.R"

crs_default <- '+init=epsg:4612'
col.NumbFullTime <- c(
  'white',"red", "purple", "green", "cyan", "blue", "black", "black"
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

Shape.mesh.pop_Est <- st_read(
  sprintf(
    "%s/%s", dir.Data, 
    fn.mesh.popEst
    ),
  options='ENCODING=CP932'
)


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

df.OsakaNara_MasterAnaData <- read.csv(
  sprintf("%s/%s", dir.Data, fn.Data_ApothLoc.OsakaNara),
  fileEncoding = "utf-8"
)

df.Mie_MasterAnaData <- read.csv(
  sprintf("%s/%s", dir.Data, fn.Data_ApothLoc.Mie),
  fileEncoding = "utf-8"
)


df.MasterAnaData <- df.Wakayama_MasterAnaData %>%
  rbind(df.OsakaNara_MasterAnaData) %>%
  rbind(df.Mie_MasterAnaData)


df.MasterAnaData[df.MasterAnaData$ID==4428,c('Loc_1', 'Loc_2')] <- 
  c(34.472055, 135.980515)
 
pts <- as.matrix(
  df.MasterAnaData[,c("Loc_2","Loc_1","Numb.FullTime","Numb.PartTime")] %>%
    mutate_if(is.character,as.numeric)
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
  df.Wakayama_CommuCareCentr[,c("Loc_2","Loc_1")]#,"Numb.FullTime","Numb.PartTime")]
)

spts_CommuCareCentr <- SpatialPoints(
  pts_CommuCareCentr[,c("Loc_2","Loc_1")],
  proj4string=CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  )

spts_CommuCareCentr <- spTransform(spts_CommuCareCentr,CRS('+init=epsg:4612'))


# Demograhic expectation --------------------------------------------------

Yr2025 <- c(
  "PT14_2025",
  "PT15_2025",
  "PT16_2025",
  "PT17_2025",
  "PT18_2025",
  "PT19_2025"
  )


cols = 

pdf('PT_2025.pdf')
plot(
  Shape.mesh.pop_Est[,'PT14_2025'], lwd=0.05)#,
#  col=cols[floor(Shape.mesh.pop_Est[,'PT14_2025']$PT14_2025*2)+1]
 # ) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT15_2025'], lwd=0.05
) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT16_2025'], lwd=0.05
) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT17_2025'], lwd=0.05
) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT18_2025'], lwd=0.05
) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT19_2025'], lwd=0.05
) #Shape.mesh.pop_Est$
dev.off()

pdf('PT_2035.pdf')
plot(
  Shape.mesh.pop_Est[,'PT14_2035'], lwd=0.05)#,
#  col=cols[floor(Shape.mesh.pop_Est[,'PT14_2025']$PT14_2025*2)+1]
# ) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT15_2035'], lwd=0.05
) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT16_2035'], lwd=0.05
) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT17_2035'], lwd=0.05
) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT18_2035'], lwd=0.05
) #Shape.mesh.pop_Est$
plot(
  Shape.mesh.pop_Est[,'PT19_2035'], lwd=0.05
) #Shape.mesh.pop_Est$
dev.off()

pdf('PT14_2025.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(
  Shape.mesh.pop_Est[,'PT14_2025'], lwd=0.05, add=TRUE
  ) #Shape.mesh.pop_Est$
dev.off()

pdf('PT15_2025.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(
  Shape.mesh.pop_Est[,'PT15_2025'], lwd=0.05, add=TRUE
  ) #Shape.mesh.pop_Est$
dev.off()

pdf('PT16_2025.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(
  Shape.mesh.pop_Est[,'PT16_2025'], lwd=0.05, add=TRUE) #Shape.mesh.pop_Est$
dev.off()

pdf('PT17_2025.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(Shape.mesh.pop_Est[,'PT17_2025'], lwd=0.05, add=TRUE) #Shape.mesh.pop_Est$
dev.off()

pdf('PT18_2025.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(Shape.mesh.pop_Est[,'PT18_2025'], lwd=0.05, add=TRUE) #Shape.mesh.pop_Est$
dev.off()

pdf('PT19_2025.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(Shape.mesh.pop_Est[,'PT19_2025'], lwd=0.05, add=TRUE) #Shape.mesh.pop_Est$
dev.off()




pdf('PT14_2035.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(
  Shape.mesh.pop_Est[,'PT14_2035'], lwd=0.05, add=TRUE
) #Shape.mesh.pop_Est$
dev.off()

pdf('PT15_2035.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(
  Shape.mesh.pop_Est[,'PT15_2035'], lwd=0.05, add=TRUE
) #Shape.mesh.pop_Est$
dev.off()

pdf('PT16_2035.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(
  Shape.mesh.pop_Est[,'PT16_2035'], lwd=0.05, add=TRUE) #Shape.mesh.pop_Est$
dev.off()

pdf('PT17_2035.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(Shape.mesh.pop_Est[,'PT17_2035'], lwd=0.05, add=TRUE) #Shape.mesh.pop_Est$
dev.off()

pdf('PT18_2035.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(Shape.mesh.pop_Est[,'PT18_2035'], lwd=0.05, add=TRUE) #Shape.mesh.pop_Est$
dev.off()

pdf('PT19_2035.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
plot(Shape.mesh.pop_Est[,'PT19_2035'], lwd=0.05, add=TRUE) #Shape.mesh.pop_Est$
dev.off()



pdf('FullTime_only.pdf')
plot(Shape_19.GovRegion, col='ivory', lwd=0.05)
plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
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
#lines(Shape.SchoolRegion, col='blue', lwd=0.05)

dev.off()

pdf('PartTime_only.pdf')
plot(Shape_19.GovRegion, col='white', lwd=0.05)
plot(Shape.SchoolRegion, col='white', lwd=0.05, add=TRUE)
points(
  spts_CommuCareCentr,
  col='black',
  pch='+',
  cex=1.5
)
points(
  spts,
  col=col.NumbFullTime[as.factor(pts[,"Numb.PartTime"])],
  pch=10,
  cex=0.5
)
#text(spts, sprintf("[%s(%s)]", pts[,"Numb.FullTime"], pts[,"Numb.PartTime"]), col=pts[,"Numb.FullTime"], font=1, cex=0.5)
#lines(Shape.SchoolRegion, col='blue', lwd=0.05)

dev.off()

pdf('FullPlusPartTime_only.pdf')
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
  col=col.NumbFullTime[
    as.factor(
      pts[,"Numb.FullTime"] + 
        pts[,"Numb.PartTime"]
      )
    ],
  pch=10,
  cex=0.5
)
#text(spts, sprintf("[%s(%s)]", pts[,"Numb.FullTime"], pts[,"Numb.PartTime"]), col=pts[,"Numb.FullTime"], font=1, cex=0.5)
#lines(Shape.SchoolRegion, col='blue', lwd=0.05)

dev.off()

