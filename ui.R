library(flexdashboard)
library(tidyverse)
library(grid)
library(jpeg)
library(shiny)
library(plotly)
library(rsconnect)
library(howler)
library(shinydashboard)
library(markdown)
library(yaml)
library(ggiraph)
library(rmarkdown)
library(magick)

ui <- fluidPage(
    
  titlePanel("Infant Babbles & Everyday Music"),
  
  # Background Info Section
  fluidRow(
    column(12,
           h2("Relevant Background Information"),
           p("This interactive dashboard serves as a companion piece to the University of Oregon Learning Lab Infant Babbles & Everyday Music project. Explore below to learn more about the project and initial findings." 
),
           p(HTML('Read more about the project <a href="https://onlinelibrary.wiley.com/doi/10.1111/desc.13122" target="_blank">here</a>.')),
           plotOutput("infographic"),
           hr() 
    )
  ),
  
  # Timeline Section
  fluidRow(
    column(12,
           h2("Explore the Everyday Temporal Structure of Infant Babbles and Music"),
           p("Hover over this visualization to zoom in or out (top right corner). Click and drag to navigate through the day. 
"),
           p("Hover over a musical event to see more details about what type of music was playing. Hover over a vocalization to see more details about what type of vocalization it is. 
"),
           p("Press play below to listen to an example of the everyday music occurring in infants' lives." ),
           tags$audio(src = "music.m4a", type = "audio/mp4", controls = TRUE),
           plotlyOutput("timeline_plot"),
    )
  ),
  
  # Findings Section
  fluidRow(
    column(12,
         h3("Main Findings"),
         p("Previous work has found that infants produce their most speech-like or sophisticated vocalizations during turn-taking conversations (Long et al., 2022), so because of this, we hypothesized that we would see the most sophisticated babbling during live, vocal music. We additionally predicted that we would see the least sophisticated babbling during recorded, instrumental music such as that which would occur in the background of a television show. Finally, because predictability facilitates word learning in toddlers (Benitez & Saffran, 2018), we predicted that there would be more sophisticated babbling with tunes that are repeated than tunes that are not. "),
         p("The project is ongoing, but our initial findings indicate more sophisticated vocalizations occurring during live music than during recorded music, during vocal music than instrumental music, and during novel musical tunes than repeated musical tunes. We also find more sophisticated vocalizations in the 10 seconds after music than in the 10 seconds before music."),
         p("Our initial findings are illustrated below. Each point represents the proportion of vocalizations that are sophisticated (sophisticated vocalizations/all vocalizations) within an everyday musical episode. Horizontal lines indicate the mean vocal sophistication proportion for the particular type of music. Hover over individual points to see the specific value for any point. "),
  )
),

  fluidRow(
    column(6, 
           h3("Vocal Sophistication During Live vs. Recorded Music"),
           plotlyOutput("live_recorded_plot")
    ),
    column(6, 
           h3("Vocal Sophistication During Vocal vs. Instrumental Music"),
           plotlyOutput("vocal_instrumental_plot")
    )
  ),
  hr(),
  
  fluidRow(
    column(6, 
           h3("Vocal Sophistication During Novel vs Repeated Musical Tunes"),
           plotlyOutput("novelty_plot")
    ),
    column(6, 
           h3("Vocal Sophistication Before vs After Music"),
           plotlyOutput("timing_plot") 
    )
  ),
  hr(),
  
)