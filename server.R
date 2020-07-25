
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(markdown)
library(RColorBrewer)


shinyServer(function(input, output) {
  
  output$p1 <- renderPlot({
    
    
    # filter data
    d <- filter(hcahps.hosp, mgroup == input$mgroup)
    star <- filter(star, mgroup == input$mgroup)
    nonstar <- filter(nonstar, mgroup == input$mgroup)
    bench <- filter(hcahps.st.bench, mgroup == input$mgroup)
    
    if (input$mgroup == "Summary"){
      
      summaryplot <- ggplot(stard, aes(x = short_name, y = mgroup, label = patient_survey_star_rating, fill = patient_survey_star_rating)) + 
        geom_tile(colour = "grey") +
        # geom_point(size = 16, shape = 15) +
        # scale_fill_gradient(low = "red", high = "green") +
        scale_fill_brewer(palette = "Spectral") +
        coord_flip() +
        geom_text(data = stard, aes(short_name, mgroup, label = patient_survey_star_rating), color = "#333333", fontface = "bold", size = 8) +
        theme(axis.title.y = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 12), legend.position = "none",
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
              axis.text.y = element_text(size = 12)) +
        scale_y_discrete(breaks = unique(stard$mgroup)) +
        ggtitle("HCAHPS Star Rating Summary")
      
      summaryplot # display plot
      
#       starplot <- ggplot(star, aes(short_name, as.numeric(patient_survey_star_rating))) +
#         geom_point(size = 4, shape = 8) +
#         ylab("Star Rating") +
#         ylim(0,5)
#       
#       multiplot(summaryplot, starplot, cols = 1)
      
    }
    else {
      
      
#       starplot <- ggplot(star, aes(short_name, as.numeric(patient_survey_star_rating))) +
#         geom_point(size = 4, shape = 8) +
# #         geom_bar(data = star, aes(short_name, as.numeric(patient_survey_star_rating)),
# #                  stat = "identity", size = 2) +
#         ylab("Star Rating") +
#         #coord_flip() +
#         theme(axis.title.x = element_blank()) +
#         ylim(1,5)
      
      # final
      starplot <- ggplot(star, aes(short_name, as.numeric(patient_survey_star_rating), label = patient_survey_star_rating)) +
        geom_point(size = 12, shape = 15) +
        scale_colour_gradient(low = "red", high = "green") +
        geom_text(colour = "white") +
        ylab("Star Rating") +
        #geom_text(data = NULL, x = 1.5, y = as.numeric(bench$hcahps_answer_percent)/100, label = "state\nbenchmark", size = 3, colour = "blue") +
        #coord_flip() +
        theme(axis.title.x = element_blank(), legend.position = "none", plot.title = element_text(size = 12)) +
        ylim(1,5) +
        ggtitle(paste0(input$mgroup, " Star Rating"))
      
#         starplot <- ggplot() +
#           geom_point(aes(star$short_name, as.numeric(star$patient_survey_star_rating)), size = 4, shape = 8) +
# #           geom_bar(aes(x = star$short_name, y = as.integer(star$patient_survey_star_rating)),
# #                    stat = "identity") +
#           ylab("Star Rating") +
#           #coord_flip() +
#           theme(axis.title.x = element_blank())
          #ylim(1,5)
      
      nonstarplot <- ggplot(nonstar, aes(short_name, as.numeric(hcahps_answer_percent)/100, label = hcahps_answer_percent)) +
        geom_point(size = 12) +
        geom_text(colour = "white") +
        ylab("Answer Percent") +
        theme(axis.title.x = element_blank(), plot.title = element_text(size = 12)) +
        ylim(min(as.numeric(nonstar$hcahps_answer_percent)/100) - .05, max(as.numeric(nonstar$hcahps_answer_percent)/100) + .05) +
        scale_y_continuous(labels = percent) +
        geom_hline(yintercept = as.numeric(bench$hcahps_answer_percent)/100, linetype = "dashed") +
        geom_text(data = NULL, x = 1.5, y = as.numeric(bench$hcahps_answer_percent)/100, label = "state\nbenchmark", size = 4.5) +
        ggtitle(gsub(strsplit(as.character(nonstar$hcahps_question), " ")[[1]][7], paste0(strsplit(as.character(nonstar$hcahps_question), " ")[[1]][7], "\n"), as.character(nonstar$hcahps_question)))
        
      #coord_flip()
      multiplot(starplot, nonstarplot, cols = 1)
      
      #starplot # display plot
      
    }
    
    
#     # create list of measures
#     mgroup.m <- unique(d$hcahps_measure_id)
#     
#     # initialize plotlist
#     plotlist <- list()
#     
#     for (i in 1:length(mgroup.m)){
#       p.x <- ggplot(filter(d, hcahps_measure_id == mgroup.m[i]), aes(short_name, as.numeric(hcahps_answer_percent), label = as.numeric(hcahps_answer_percent))) +
#         geom_point(size = 4) +
#         geom_text(hjust = -.75, size = 3.5) +
#         theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
#         ggtitle(unique(d[d$hcahps_measure_id == mgroup.m[i], "hcahps_answer_description"]))
#       assign(paste0("p",i), p.x) # assign each plot
#       plotlist[[i]] <- assign(paste0("p",i), p.x)
#     }
#     
#     rm(p.x)
#     # if more than 3 measures then two columns
#     col.num <- ifelse(length(mgroup.m) > 3, 2, 1)
#     multiplot(plotlist = plotlist, cols = col.num)

  })
  
#   output$p2 <- renderPlot({
#     # filter data
#     d <- filter(hcahps.hosp, mgroup == input$mgroup)
#     star <- filter(star, mgroup == input$mgroup)
#     nonstar <- filter(nonstar, mgroup == input$mgroup)
#     
#     if (input$mgroup == "Summary"){
#       
#       starplot <- ggplot(star, aes(short_name, as.numeric(patient_survey_star_rating))) +
#         geom_point(size = 4, shape = 8) +
#         ylab("Star Rating") +
#         coord_flip() +
#         theme(axis.title.y = element_blank()) +
#         ylim(1,5)
#       
#       starplot
#       
#     }
#     
#     else {
#       
#       nonstarplot <- ggplot(nonstar, aes(short_name, as.numeric(hcahps_answer_percent))) +
#         geom_point(size = 4) +
#         ylab("Percent Recommended") +
#         ylim(min(as.numeric(nonstar$hcahps_answer_percent)) - 5, max(as.numeric(nonstar$hcahps_answer_percent)) + 5) +
#         coord_flip()
#       #multiplot(starplot, nonstarplot, cols = 1)
#       
#       nonstarplot
#       
#     }
#     
#   })
  
  # output a table of values
  output$table <- renderTable({
    
    d.pct <- filter(hcahps.hosp, mgroup == input$mgroup) %>%
      select(hospital_name, hcahps_answer_percent, hcahps_measure_id) %>% 
      filter(hcahps_answer_percent != "Not Applicable") %>% 
      arrange(hcahps_measure_id) %>% 
      select(-hcahps_measure_id)
    
    d.star <- filter(hcahps.hosp, mgroup == input$mgroup) %>%
      select(hospital_name, patient_survey_star_rating, hcahps_measure_id) %>% 
      filter(patient_survey_star_rating != "Not Applicable") %>% 
      arrange(hcahps_measure_id) %>%
      select(-hcahps_measure_id)
    
    if (input$mgroup == "Summary"){
      d.table <- rename(d.star, Hospital = hospital_name, Star_Rating = patient_survey_star_rating)
    }
    else {
      d.table <- left_join(d.pct, d.star, by = "hospital_name") %>% rename(Hospital = hospital_name, Percent = hcahps_answer_percent, Star_Rating = patient_survey_star_rating)
    }
    d.table
  })

})
