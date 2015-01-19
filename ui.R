
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Hospital Compare: HCAHPS for Hospitals in RI"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("mgroup",
                  label = "Select Measure Group:",
                  choices = mchoices,
                  selected = mchoices[1])
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 plotOutput("p"),
                 plotOutput("p1")),
        tabPanel("Tables"),
        tabPanel("Measure Info"))
    )
  )
))
