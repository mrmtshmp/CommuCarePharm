
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

fn.Data_Yakkyoku   <-  "/190703/r1_06_nagasaki_yakkyoku_02.xlsx"
fn.Data_StreetBlock<-  "/190703/42000-12.0b/42_2018.csv"

fn.Output_region <-  "/190626/A32-16_30_GML/gml/A32-16_30.xml"

fn.Sub_require_libraries <- "require_libraries.R"


# Load source code --------------------------------------------------------

source(sprintf("%s/%s", dir.Sub, fn.Sub_require_libraries))


# Load XLSX file -----------------------------------------------------------

raw.MasterAnaData <- read_xlsx(
  path = sprintf("%s/%s", dir.Data, fn.Data_Yakkyoku),
  skip = 6
  )
  # xmlInternalTreeParse(
  # selectAXlm
  # )

colnames(raw.MasterAnaData) <- c("DUMMY",colnames(raw.MasterAnaData)[1:(length(raw.MasterAnaData)-1)])
colnames(raw.MasterAnaData) <- c(
  colnames(raw.MasterAnaData)[1:10],
  "DUMMY_2",
  colnames(raw.MasterAnaData)[11:15],
  colnames(raw.MasterAnaData)[18:(length(raw.MasterAnaData))],
  "Note_2"
  )

raw.MasterAnaData <- raw.MasterAnaData %>% 
  dplyr::select(ID, Name, StreetAddress, Tel.Numb, Note)%>%
  filter(
    !is.na(ID)
    )
  
out.raw.MasterAnaData <- raw.MasterAnaData %>%
  ddply(
    .(ID),
    function(D){
      w.out   = unlist(strsplit(D$Tel.Numb, "常　勤:"))
      w.out_2 = unlist(strsplit(w.out[2],"非常勤:"))
      
      out   = str_trim(unlist(strsplit(w.out[2], "\r\n"))[1])
      out_2 = str_trim(unlist(strsplit(w.out_2[2], "\r\n"))[1])

      print(w.out)
      print(out)
      print(out_2)
      return(
        data.frame(
          "Numb.FullTime" = as.numeric(out),
          "Numb.PartTime" = as.numeric(out_2)
          )
        )
      }
    ) %>%
  
  left_join(
    ddply(
      raw.MasterAnaData,
      .(ID),
      function(D){
        out = unlist(
          strsplit(
            D$StreetAddress, 
            split = "－\\d{4}"
            )
          )
        return(
          data.frame(
            "PostalZip"     = out[1], 
            "StreetAddress" = out[2]
            )
          )
        }
      ), by="ID"
    ) %>%  
  left_join(
    raw.MasterAnaData, 
    by="ID"
    )

write.csv(
  out.raw.MasterAnaData,
  sprintf(
    "%s/%s",
    dir.Data,
    "Nagasaki_raw.MasterAnaData.csv")
  )


# StreetAddress to GIS mesh -------------------------------------------------

install.packages("stringdist")
require(stringdist)


raw.StreetBlock <- read.csv(
  file = sprintf("%s/%s", dir.Data, fn.Data_StreetBlock)
  )

colnames(
  raw.StreetBlock
  ) <-c(
  "PrefID","Address_1",
  "CityID","Address_2",
  "StreetID","Address_3",
  "Dummy",
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
  iconv(MasterAnaData$Name,from="shift-jis",to="utf-8"),
  iconv(MasterAnaData$Note,from="shift-jis",to="utf-8")
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
  "Nagasaki_MasterAnaData.csv")
  )
