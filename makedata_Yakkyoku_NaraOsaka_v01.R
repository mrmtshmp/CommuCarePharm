
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


fn.Data_StreetBlock_Osaka      <-  "/190703/27000-12.0b/27_2018.csv"
fn.Data_StreetBlock_Nara       <-  "/190703/29000-12.0b/29_2018.csv"

fn.out.raw.MasterAnaData_Osaka <-   "/rec_sent/190713/7oosaka-kikanzentai-yakkyoku_3.csv"
fn.out.raw.MasterAnaData_Nara  <-   "/rec_sent/190713/7nara-kikanzentai-yakkyoku_3.csv"
                              # output of 'Prgrm_IshiiSensei/makedata_Mie.txt'

fn.Sub_require_libraries <- "require_libraries.R"


# Load source code --------------------------------------------------------

source(sprintf("%s/%s", dir.Sub, fn.Sub_require_libraries))


# Load XLSX file -----------------------------------------------------------

out.raw.MasterAnaData <- read.csv(
  sprintf(
    "%s/%s",
    dir.Data,
    fn.out.raw.MasterAnaData_Osaka),
  fileEncoding = 'utf-8'
  ) %>%
  rbind(
    read.csv(
      sprintf(
        "%s/%s",
        dir.Data,
        fn.out.raw.MasterAnaData_Nara),
      fileEncoding = 'utf-8'
      )
    )


out.raw.MasterAnaData$ID <- c(1:nrow(out.raw.MasterAnaData))

# StreetAddress to GIS mesh -------------------------------------------------

raw.StreetBlock <- read.csv(
  file = sprintf("%s/%s", dir.Data, fn.Data_StreetBlock_Osaka),
  fileEncoding = "shift-jis",
  encoding = "utf-8"
  ) %>%
  rbind(
    read.csv(
      file = sprintf("%s/%s", dir.Data, fn.Data_StreetBlock_Nara),
      fileEncoding = "shift-jis",
      encoding = "utf-8"
      ) 
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


sub.table <- matrix(
  c("一丁目","二丁目", "三丁目","四丁目","五丁目","六丁目","七丁目","八丁目","九丁目","十丁目","十一丁目","十二丁目","十三丁目","十四丁目",
    "１丁目","２丁目","３丁目","４丁目","５丁目","６丁目","７丁目","８丁目","９丁目","１０丁目","１１丁目","１２丁目","１３丁目","１４丁目"), #
  ncol=2
  )

ChomeSubs <- function(vec){
  for(i in 1:nrow(sub.table)){
    x <- sub.table[i,]
    if(i==1) out <- vec
    print(x)
    .x1 <- x[1]
    .x2 <- x[2]
    out = gsub(.x1, .x2, out)
    }
  return(out)
  }

raw.StreetBlock$StreetAddress <- paste0(
  raw.StreetBlock$Address_1, 
  raw.StreetBlock$Address_2,
  ChomeSubs(raw.StreetBlock$Address_3)
  )

out.raw.MasterAnaData$StreetAddress.x <- 
  ChomeSubs(out.raw.MasterAnaData$StreetAddress.x)
  

MasterAnaData <- out.raw.MasterAnaData %>%
  
  ddply(
    .(ID, StreetAddress.x),
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
      scan = which(match.score==min(match.score))[1]
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
  iconv(MasterAnaData$Name,from="shift-jis",to="utf-8"),
  iconv(MasterAnaData$Flg,from="shift-jis",to="utf-8")
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
  "OsakaNara_MasterAnaData.csv")
  )
