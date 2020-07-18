# global.R, runs at outset of application launch

# load packages
library(RSocrata)
library(shiny)
library(dplyr)
library(lubridate)

# create data frames
# hcahps.nat.bench <- read.socrata("https://data.medicare.gov/resource/99ue-w85f.json")
hcahps.st.bench <- read.socrata("https://data.medicare.gov/resource/84jm-wiui.json?state=RI")

# 2015-10-14 update, json not working, switch to csv, but now field names are different
#hcahps.hosp <- read.socrata("https://data.medicare.gov/resource/dgck-syfz.json?state=RI")
# hcahps.hosp <- read.socrata("https://data.medicare.gov/resource/dgck-syfz.csv?state=RI")
hcahps.hosp <- read.csv("https://data.medicare.gov/resource/dgck-syfz.csv?state=RI",
                        colClasses = "character")
colnames(hcahps.hosp) <- tolower(colnames(hcahps.hosp))
colnames(hcahps.hosp) <- gsub("\\.", "_", colnames(hcahps.hosp))

ht1 <- 600 # summary plot height
wd1 <- 507 # summary plot width

# initialize measure group variable
hcahps.hosp$mgroup <- NA

# set each measure group based on measure_id
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_STAR_RATING"), "mgroup"] <- "Summary"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_CLEAN_HSP_A_P", "H_CLEAN_STAR_RATING"), "mgroup"] <- "Cleanliness"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_COMP_1_A_P", "H_COMP_1_STAR_RATING"), "mgroup"] <- "Nurse Communication"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_COMP_2_A_P", "H_COMP_2_STAR_RATING"), "mgroup"] <- "Doctor Communication"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_COMP_3_A_P", "H_COMP_3_STAR_RATING"), "mgroup"] <- "Staff Responsiveness"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_COMP_4_A_P", "H_COMP_4_STAR_RATING"), "mgroup"] <- "Pain Management"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_COMP_5_A_P", "H_COMP_5_STAR_RATING"), "mgroup"] <- "Communication about Meds"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_COMP_6_Y_P", "H_COMP_6_STAR_RATING"), "mgroup"] <- "Discharge Information"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_COMP_7_SA", "H_COMP_7_STAR_RATING"), "mgroup"] <- "Care Transition"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_HSP_RATING_9_10", "H_HSP_RATING_STAR_RATING"), "mgroup"] <- "Overall Rating"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_QUIET_HSP_A_P", "H_QUIET_STAR_RATING"), "mgroup"] <- "Quietness"
hcahps.hosp[hcahps.hosp$hcahps_measure_id %in% c("H_RECMND_DY", "H_RECMND_STAR_RATING"), "mgroup"] <- "Recommend Hospital"

hcahps.st.bench$mgroup <- NA
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_STAR_RATING"), "mgroup"] <- "Summary"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_CLEAN_HSP_A_P", "H_CLEAN_STAR_RATING"), "mgroup"] <- "Cleanliness"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_COMP_1_A_P", "H_COMP_1_STAR_RATING"), "mgroup"] <- "Nurse Communication"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_COMP_2_A_P", "H_COMP_2_STAR_RATING"), "mgroup"] <- "Doctor Communication"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_COMP_3_A_P", "H_COMP_3_STAR_RATING"), "mgroup"] <- "Staff Responsiveness"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_COMP_4_A_P", "H_COMP_4_STAR_RATING"), "mgroup"] <- "Pain Management"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_COMP_5_A_P", "H_COMP_5_STAR_RATING"), "mgroup"] <- "Communication about Meds"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_COMP_6_Y_P", "H_COMP_6_STAR_RATING"), "mgroup"] <- "Discharge Information"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_COMP_7_SA", "H_COMP_7_STAR_RATING"), "mgroup"] <- "Care Transition"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_HSP_RATING_9_10", "H_HSP_RATING_STAR_RATING"), "mgroup"] <- "Overall Rating"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_QUIET_HSP_A_P", "H_QUIET_STAR_RATING"), "mgroup"] <- "Quietness"
hcahps.st.bench[hcahps.st.bench$hcahps_measure_id %in% c("H_RECMND_DY", "H_RECMND_STAR_RATING"), "mgroup"] <- "Recommend Hospital"


# 2015-10-14 convert date fields
# hcahps.hosp$measure_start_date <- as.Date(hcahps.hosp$measure_start_date)
# hcahps.hosp$measure_end_date <- as.Date(hcahps.hosp$measure_end_date)

hcahps.hosp$measure_start_date <- mdy(hcahps.hosp$measure_start_date)
hcahps.hosp$measure_end_date <- mdy(hcahps.hosp$measure_end_date)

# remove rows without mgroup
hcahps.hosp <- filter(hcahps.hosp, !is.na(mgroup)) %>% 
  select(provider_id, hospital_name, hcahps_measure_id, hcahps_question,
         measure_start_date, measure_end_date, hcahps_answer_percent,
         patient_survey_star_rating, mgroup)

hcahps.st.bench <- filter(hcahps.st.bench, !is.na(mgroup))

# create short names data frame
hospID <- as.character(c(410001, 410004:410013))
#hospID <- c(410001, 410004:410013) # 2015-10-14, keep as numeric
hosp.short.names <- c("MHRI", "RWMC", "FATIMA", "NWPRT", "RIH", "SCH", "KENT", "WIH", "LNDMRK", "MIRIAM", "WSTRLY")
hosp.short <- tibble(provider_id = hospID, short_name = hosp.short.names)
rm(hospID, hosp.short.names)

# join to main data frame
hcahps.hosp <- left_join(hcahps.hosp, hosp.short, by = "provider_id")

measure.start.date <- substr(unique(hcahps.hosp$measure_start_date), 1, 10)
measure.end.date <- substr(unique(hcahps.hosp$measure_end_date), 1, 10)

# separate star and percent and append back together to merge value
star <- filter(hcahps.hosp, patient_survey_star_rating != "Not Applicable") %>% 
  select(-hcahps_answer_percent)
nonstar <- filter(hcahps.hosp, hcahps_answer_percent != "Not Applicable") %>% 
  select(-patient_survey_star_rating)

stard <- filter(hcahps.hosp, patient_survey_star_rating != "Not Applicable") %>% 
  select(short_name, patient_survey_star_rating, mgroup)

# set choices variable
mchoices <- unique(hcahps.hosp$mgroup)

tabtext <- mchoices[1]

# function for multiple ggplots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
