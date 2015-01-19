
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
    
    if(input$mgroup == "Received Help"){
      d <- filter(hcahps.hosp, hcahps_measure_id %in% received.help[1])
    }
    else{
      d <- filter(hcahps.hosp, hcahps_measure_id %in% clean.bath[1])
    }
    
    ggplot(d, aes(provider_id, as.numeric(hcahps_answer_percent))) +
      geom_point(size = 4)

  })
 output$p1 <- renderPlot({
   if(input$mgroup == "Received Help"){
     d <- filter(hcahps.hosp, hcahps_measure_id %in% received.help[2])
   }
   else{
     d <- filter(hcahps.hosp, hcahps_measure_id %in% clean.bath[2])
   }
   
   ggplot(d, aes(provider_id, as.numeric(hcahps_answer_percent))) +
     geom_point(size = 4) +
     coord_flip()
   
 })

})
