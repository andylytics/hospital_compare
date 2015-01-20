
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output) {

  output$p <- renderPlot({
    # filter data
    d <- filter(hcahps.hosp, mgroup == input$mgroup)
    
    # create list of measures
    mgroup.m <- unique(d$hcahps_measure_id)
    
    # initialize plotlist
    plotlist <- list()
    
    for (i in 1:length(mgroup.m)){
      p.x <- ggplot(filter(d, hcahps_measure_id == mgroup.m[i]), aes(provider_id, as.numeric(hcahps_answer_percent))) +
        geom_point(size = 4)
      assign(paste0("p",i), p.x) # assign each plot
      plotlist[[i]] <- assign(paste0("p",i), p.x)
    }
    
    rm(p.x)
    
    multiplot(plotlist = plotlist, cols = 1)

  })
#  output$p1 <- renderPlot({
#    if(input$mgroup == "Received Help"){
#      d <- filter(hcahps.hosp, hcahps_measure_id %in% received.help[2])
#    }
#    else{
#      d <- filter(hcahps.hosp, hcahps_measure_id %in% clean.bath[2])
#    }
#    
#    ggplot(d, aes(provider_id, as.numeric(hcahps_answer_percent))) +
#      geom_point(size = 4) +
#      coord_flip()
#    
#  })

})
