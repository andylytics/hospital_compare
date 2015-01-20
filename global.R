# load packages
library("shiny")
library("RSocrata")
library("dplyr")

# create data frames
hcahps.nat.bench <- read.socrata("https://data.medicare.gov/resource/99ue-w85f.json")
hcahps.st.bench <- read.socrata("https://data.medicare.gov/resource/84jm-wiui.json?state=RI")
hcahps.hosp <- read.socrata("https://data.medicare.gov/resource/dgck-syfz.json?state=RI")

hcahps.nat.bench$mgroup <- NA
hcahps.nat.bench[grep("received help", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Received Help"
hcahps.nat.bench[grep("bathroom", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Clean Bathroom"
hcahps.nat.bench[grep("communicated well", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Staff Communication"
hcahps.nat.bench[grep("pain", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Pain Control"
hcahps.nat.bench[grep("explained about medicines", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Meds Explained"
hcahps.nat.bench[grep("quiet at night", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Quiet Room"
hcahps.nat.bench[grep("given information", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Given Information"
hcahps.nat.bench[grep("understood their care", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Understood Care"
hcahps.nat.bench[grep("rating", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Hospital Rating"
hcahps.nat.bench[grep("recommend the hospital", tolower(hcahps.nat.bench$hcahps_question)), "mgroup"] <- "Recommend Hospital"

hcahps.hosp$mgroup <- NA
hcahps.hosp[grep("received help", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Received Help"
hcahps.hosp[grep("bathroom", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Clean Bathroom"
hcahps.hosp[grep("communicated well", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Staff Communication"
hcahps.hosp[grep("pain", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Pain Control"
hcahps.hosp[grep("explained about medicines", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Meds Explained"
hcahps.hosp[grep("quiet at night", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Quiet Room"
hcahps.hosp[grep("given information", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Given Information"
hcahps.hosp[grep("understood their care", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Understood Care"
hcahps.hosp[grep("rating", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Hospital Rating"
hcahps.hosp[grep("recommend the hospital", tolower(hcahps.hosp$hcahps_question)), "mgroup"] <- "Recommend Hospital"

hcahps.st.bench$mgroup <- NA
hcahps.st.bench[grep("received help", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Received Help"
hcahps.st.bench[grep("bathroom", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Clean Bathroom"
hcahps.st.bench[grep("communicated well", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Staff Communication"
hcahps.st.bench[grep("pain", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Pain Control"
hcahps.st.bench[grep("explained about medicines", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Meds Explained"
hcahps.st.bench[grep("quiet at night", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Quiet Room"
hcahps.st.bench[grep("given information", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Given Information"
hcahps.st.bench[grep("understood their care", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Understood Care"
hcahps.st.bench[grep("rating", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Hospital Rating"
hcahps.st.bench[grep("recommend the hospital", tolower(hcahps.st.bench$hcahps_question)), "mgroup"] <- "Recommend Hospital"


mchoices <- unique(hcahps.nat.bench$mgroup)

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
