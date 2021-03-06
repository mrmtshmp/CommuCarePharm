
# Discription -------------------------------------------------------------

# Author:  
# 19/06/27

# Reference 1: 
# https://www.tutorialspoint.com/r/r_xml_files.htm 

# Reference 2: 
# https://qiita.com/nyampire/items/d74dcde6e57f793ab0c1 


# Settings ----------------------------------------------------------------

# devtools::install_github('ultinomics/xmltools')
require(xmltools)

dir.Data   <- "../Data"
dir.Output <- "../Output" 
dir.Sub    <- "./Sub"

list.fn.Data_elevation   <-  list.files(
  sprintf(
    '%s/%s',dir.Data, '190710/G04-d-11'
    ), pattern = 'xml'
  )

fn.Sub_require_libraries <- "require_libraries.R"


# Load source code --------------------------------------------------------

source(sprintf("%s/%s", dir.Sub, fn.Sub_require_libraries))


# Load XML file -----------------------------------------------------------

list.raw_MasterAnaData <-
  llply(
    list.fn.Data_elevation,
        function(L){
          print(L)
          out <- read_xml(
            x = sprintf(
              '%s/%s', 
              sprintf(
                '%s/%s',dir.Data, '190710/G04-d-11'
                ), 
              L
              )
            )
          return(out)
          }
        )

xml_ns(list.raw_MasterAnaData[[1]])

list.xy_lc <- llply(
  list.raw_MasterAnaData,
  function(L){
    L %>%
      #  xmlRoot() %>%
      xml_find_all(
        "//*[name()='gml:lowerCorner']",
#        "/gml:lowerCorner",
        xml_ns(L)
        ) %>% 
      xml_contents() %>% 
      as.character() %>% 
      strsplit(" ") %>% 
      unlist() %>% 
      map_dbl(as.numeric)
    }
  ) 

my.gsub <- function(x){
  gsub("\n\t\t\t\t","",x)
} 
  
list.df.dem <- llply(
  list.raw_MasterAnaData,
  function(xml.L){
    print(xml_ns(xml.L))
    out <- xml.L %>%
    
      xml_find_first(
#        "/*/*/*/gml:rangeSet/gml:DataBlock/gml:tupleList"
        "//*[name()='gml:tupleList']"
#        "/*/*/*/*/*/gml:domainSet/gml:rangeSet/gml:DataBlock/gml:rangeParameters/gml:tupleList"
        ) %>% 
      
      xml_contents() %>% 
      as.character() %>%
      my.gsub() %>%

      readr::read_delim(
        delim = "," , 
        col_names = c(
          'mesh_ID',"ele.av","ele.max","ele.min","ele.code",
          "ang.max","ang.max.dir","ang.min","ang.min.dir","ang.av"
          )
      )
      return(out)
    }
  )


# Convert to raster file --------------------------------------------------

test <- ldply(
  list.df.dem,
  function(D){
    print(head(D))
    mat <- as.matrix(D)
    return(raster(mat))
    }
  )


raster_dem <- 
  df_dem$value %>% 
  matrix(nrow = 225, ncol = 150) %>% 
  t() %>% 
  raster()




# get the name of the first file in the zip archive
fname = unzip(tf, list=TRUE)$Name[1]
# unzip the file to the temporary directory
unzip(tf, files=fname, exdir=td, overwrite=TRUE)
# fpath is the full path to the extracted file
fpath = file.path(td, fname)

test <- unzip(sprintf('%s/%s', dir.Data, fn.Data_LandUse))
test <- unzip(sprintf('%s/%s', dir.Data, fn.Data_LandUse), )

MasterAnaData <- xmlParse(
  file = sprintf("%s/%s", dir.Data, fn.Data_ScZone)
  )
  # xmlInternalTreeParse(
  # selectAXlm
  # )


# Exract the root node form the xml file.
rootnode <- xmlRoot(MasterAnaData)

# Find number of nodes in the root.
rootsize <- xmlSize(rootnode)


# Print the result.
print(rootnode[1:10])

# Print the result.
print(rootsize)

# School name and address -------------------------------------------------

df.ScNameAddr <- data.frame(
  
  "ID"   =  xpathSApply(
    MasterAnaData,
    "//ksj:PublicJuniorHighSchool" , 
    xmlGetAttr, 
    "gml:id"
    ),
  
  "name" = xpathSApply(
    MasterAnaData,
    "//ksj:PublicJuniorHighSchool/ksj:name",
    xmlValue),
  
  "address" = xpathSApply(
    MasterAnaData,
    "//ksj:PublicJuniorHighSchool/ksj:address",
    xmlValue)
  )

MasterAnaData <- df.ScNameAddr %>%
  ddply(
    .(ID, address),
    function(D){
      raw.StreetBlock$StreetAddress_str_replaced <- str_replace(
        string = raw.StreetBlock$StreetAddress,pattern = "長崎県",""
      )
      match.score = stringdist(
        D$StreetAddress.x, 
        raw.StreetBlock$StreetAddress_str_replaced,
        method = "lcs"
      )
      raw.StreetBlock$score <- match.score
      scan = which(match.score==min(match.score))
      print(head(raw.StreetBlock))
      out = raw.StreetBlock[
        scan,
        c("StreetAddress", "Loc_1", "Loc_2")
        ]
      #    print(out)
      return(out)
    }
  ) %>%
  left_join(
    out.raw.MasterAnaData
  )


df.ScNameAddr$LatLong <- sprintf("%s:%s", df.ScNameAddr$V1, df.ScNameAddr$V2)
df.ScNameAddr$Tip     <- ScRegionalPos$ID

GeoMarker <- gvisMap(
  df.ScNameAddr, 
  locationvar = "address", 
  tipvar    = "ID", 
  options=list(
    showTip=TRUE,
    showLine=TRUE,
    enableScrollWheel=TRUE,
    mapType='terrain',
    useMapTypeControl=TRUE
  )
)

plot(GeoMarker)


# GIS mesh of school zone -------------------------------------------------

ScRegionalPos <- data.frame(
    
  "ID"   =  xpathSApply(
    MasterAnaData,
    "//gml:Curve" , 
    xmlGetAttr, 
    "gml:id"
    ),
  
  "pos" =  data.frame(
    "pos" = xpathSApply(
      MasterAnaData, 
      "//gml:posList",
      xmlValue
      )
    )
  ) %>% 

  ddply(
    .(ID),
    function(D){
      vec = strsplit(
        as.character(D$pos),
        "\n\t"
        )
      res = data.frame(
        "pos.xy" = ldply(vec) %>%
          t()
        )
      return(
        res
        )
      }
    ) %>%
  filter(
    pos.xy != ""
    ) %>%
  mutate(
    id2 = cumsum(duplicated(ID))
    ) %>%
  
  ddply(
    .(ID, id2),
    function(D){
      vec = strsplit(
        as.character(D$pos.xy),
        " "
        ) %>%
        ldply()
      
      return(vec)
    }
  )

ScRegionalPos$LatLong <- sprintf("%s:%s", ScRegionalPos$V1, ScRegionalPos$V2)
ScRegionalPos$Tip     <- ScRegionalPos$ID

GeoMarker <- gvisMap(
  ScRegionalPos[ScRegionalPos$ID%in%c("cv84_1"),], 
  locationvar = "LatLong", 
  tipvar    = "Tip", 
  options=list(
    showTip=FALSE,
    showLine=TRUE,
    enableScrollWheel=TRUE,
    mapType='terrain',
    useMapTypeControl=TRUE
  )
)

plot(GeoMarker)

write.csv(
  ScRegionalPos,
  sprintf(
    "%s/%s",
    dir.Data,
    "Wakayama_ScRegionalPos.csv"
    )
  )

# Make Ring of GIS --------------------------------------------------------

ScRegionalPos <- read.csv(
  sprintf(
    "%s/%s",
    dir.Data,
    "Wakayama_ScRegionalPos.csv"
  ),
  fileEncoding = "shift-jis"
)

w_0_ring.ScRegionalPos <- ScRegionalPos %>%
  ddply(
    .(ID),
    function(D){
      
    }
  )

w_1_ring.ScRegionalPos <- ScRegionalPos %>%
  ddply(
    .(ID),
    function(D){
      target = ScRegionalPos[ScRegionalPos$ID != D$ID,]
      out = D %>%
        left_join(target[,c("ID", "LatLong")], by="LatLong")
      return(out)
    }
  )
w_2_ring.ScRegionalPos  <- unique(w_1_ring.ScRegionalPos[,c("ID.x","ID.y")])
w_3_ring.ScRegionalPos  <- w_2_ring.ScRegionalPos %>%
  ddply(
    .(ID.x,ID.y),
    function(D){
      out = union(
        ScRegionalPos[ScRegionalPos$ID==D$ID.x,"LatLong"],
        ScRegionalPos[ScRegionalPos$ID==D$ID.y,"LatLong"]
        )
      print(c(D$ID.x,D$ID.y))
      print(head(out))
      return(
        data.frame(
          ID.x =D$ID.x, 
          ID.y =D$ID.y, 
          LatLong =out
          )
        )
      }
    ) %>%
  ddply(
    .(ID.x),
    function(D){
      out = unique(D[,c("ID.x","LatLong")])
      return(out)
    }
  )
  

GeoMarker <- gvisMap(
  ScRegionalPos[
    ScRegionalPos$ID %in% c("cv65_1","cv67_1","cv83_1","cv76_1") &
      !is.na(ScRegionalPos$LatLong),
    ], 
  locationvar = "LatLong", 
  tipvar    = "ID.x", 
  options=list(
    showTip=TRUE,
    showLine=TRUE,
    enableScrollWheel=TRUE,
    mapType='terrain',
    useMapTypeControl=TRUE
  )
)

plot(GeoMarker)

write.csv(w_3_ring.ScRegionalPos, "w_3_ring.ScRegionalPos.csv")


# Analysis using "sp" package -----------------------------------------------




# XML to data.frame, wth --------------------------------------------------
#   (FYI...)

# Convert the input xml file to a data frame.
xmldataframe <- xmlToDataFrame(
  sprintf(
    "%s/%s", 
    dir.Data, 
    fn.Data_ScZone
    )
  )

print(xmldataframe)
