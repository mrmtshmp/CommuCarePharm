
# Discription -------------------------------------------------------------

# Author:  
# 19/06/27

# Reference 1: 
# https://www.tutorialspoint.com/r/r_xml_files.htm 

# Reference 2: 
# https://qiita.com/nyampire/items/d74dcde6e57f793ab0c1 


# Settings ----------------------------------------------------------------

dir.Data   <- "../Data"
dir.Output <- "../Output" 
dir.Sub    <- "./Sub"

fn.Data_CommCareCenter   <-  '190712/sityouson-houkatsu.xlsx'
fn.Data_StreetBlock<-  "/190703/30000-12.0b/30_2018.csv"


fn.Sub_require_libraries <- "require_libraries.R"


# Load source code --------------------------------------------------------

source(sprintf("%s/%s", dir.Sub, fn.Sub_require_libraries))


# Load XLSX file -----------------------------------------------------------

raw.MasterAnaData <- read_xlsx(
  path = sprintf("%s/%s", dir.Data, fn.Data_CommCareCenter),
  skip = 1
  )
  # xmlInternalTreeParse(
  # selectAXlm
  # )

colnames(raw.MasterAnaData) <- c("ID","Town","Note","Name","Area","Postal","Address","Tel.Numb")

out.raw.MasterAnaData <- raw.MasterAnaData

# StreetAddress to GIS mesh -------------------------------------------------


raw.StreetBlock <- read.csv(
  file = sprintf("%s/%s", dir.Data, fn.Data_StreetBlock),
  fileEncoding = "shift-jis",
  encoding = "utf-8"
  )

colnames(
  raw.StreetBlock
  ) <-c(
  "PrefID","Address_1",
  "CityID","Address_2",
  "StreetID","Address_3",
  "Loc_1", "Loc_2",
  "Dumm1","Dumm2"
  )

raw.StreetBlock$StreetAddress <- paste0(
  raw.StreetBlock$Address_1, 
  raw.StreetBlock$Address_2, 
  raw.StreetBlock$Address_3
  )

MasterAnaData <- out.raw.MasterAnaData %>%
  ddply(
    .(ID, Address),
    function(D){
      raw.StreetBlock$StreetAddress_str_replaced <- str_replace(
        string = raw.StreetBlock$StreetAddress,pattern = "長崎県",""
        )
      match.score = stringdist(
        D$Address, 
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

MasterAnaData$LatLong <- sprintf(
  "%s:%s", 
  MasterAnaData$Loc_1, 
  MasterAnaData$Loc_2
  )
MasterAnaData$Tip <- sprintf(
  "%s__%s",
  MasterAnaData$Name,
  MasterAnaData$Note
#  iconv(MasterAnaData$Name,from="shift-jis",to="utf-8"),
#  iconv(MasterAnaData$Note,from="shift-jis",to="utf-8")
  )

MapDat <- MasterAnaData[,c("LatLong","Tip")]

GeoMarker <- gvisMap(
  MapDat, 
  locationvar = "LatLong", 
  tipvar    = "Tip",
#  sizevar   = 
  options=list(
    showTip=TRUE,#TRUE,
    showLine=TRUE,
    enableScrollWheel=TRUE,
    mapType='terrain',
    useMapTypeControl=TRUE
  )
)

GeoMarker <- gvisGeoChart(
  MasterAnaData, 
  locationvar = "LatLong", 
  hovervar    = "Tip",
  sizevar = "Numb.FullTime",
  options = list(region="JP")
  )


plot(GeoMarker)

write.csv(MasterAnaData,   sprintf(
  "%s/%s",
  dir.Data,
  "Wakayama_CommuCareCenter_MasterAnaData.csv"),
  fileEncoding = 'utf-8'
  )
