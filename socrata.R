library("RSocrata")
library("dplyr")

hcahps.nat.bench <- read.socrata("https://data.medicare.gov/resource/99ue-w85f.json")
hcahps.st.bench <- read.socrata("https://data.medicare.gov/resource/84jm-wiui.json?state=RI")
hcahps.hosp <- read.socrata("https://data.medicare.gov/resource/dgck-syfz.json?state=RI")


# initialize measure group
head(hcahps.nat.bench)
unique(hcahps.nat.bench$hcahps_question)

received.help <-      hcahps.nat.bench[grep("received help", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]
clean.bath <-         hcahps.nat.bench[grep("bathroom", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]
communicated.well <-  hcahps.nat.bench[grep("communicated well", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]
pain.control <-       hcahps.nat.bench[grep("pain", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]
explained.meds <-     hcahps.nat.bench[grep("explained about medicines", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]
quiet.room <-         hcahps.nat.bench[grep("quiet at night", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]
given.info <-         hcahps.nat.bench[grep("given information", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]
understood.care <-    hcahps.nat.bench[grep("understood their care", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]
hosp.rating <-        hcahps.nat.bench[grep("rating", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]
recommend.hosp <-     hcahps.nat.bench[grep("recommend the hospital", tolower(hcahps.nat.bench$hcahps_question)), "hcahps_measure_id"]


# loop through each data frame and 
hcahps.hosp$measure

bathroom <- grep("bathroom", tolower(hcahps.hosp$hcahps_question))
hcahps.hosp[bathroom, "hcahps_measure_id"]
