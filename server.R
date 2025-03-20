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

server <- function(input, output) {
  
  summary <- read_csv("deidentified_summary.csv")
  
  # Background Info Plot
  output$infographic <- renderPlot({
    
    x <- 1:10
    y <- 1:10
    data <- data.frame(x, y)
    image_path <- "www/Picture1.jpg"
    image <- image_read(image_path)
    grob_image <- rasterGrob(image, interpolate = TRUE)
    
    ggplot(data, aes(x, y)) +
      theme(
        plot.background = element_rect(fill = "#1CBC9C", color = NA),
        panel.background = element_rect(fill = "#1CBC9C", color = NA),
        panel.grid = element_blank(),                             
        axis.title = element_blank(),                             
        axis.text = element_blank(),                              
        axis.ticks = element_blank()
      ) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(expand = expansion(mult = c(0.15, 0.05))) +
      geom_rect(
        xmin = 0, xmax = 10, ymin = 8, ymax = 10,
        fill = "white",
        color = "black"
      ) +
      annotate("text", x = 5, y = 9, size = 6, fontface = "bold", label = "This project investigates the relationship between everyday music and early language development.") +
      annotate("text", x = 5, y = 8.5, size = 6, label = "Explore below to discover why experts are interested in the relationship between these two everyday infant behaviors.") +
      geom_rect(
        xmin = 0, xmax = 2.75, ymin = 0.5, ymax = 7.5,
        fill = "white",
        color = "black"
      ) +
      annotation_custom(grob_image, xmin = .25, xmax = 2.5, ymin = 1, ymax = 7) +
      geom_rect(
        xmin = 3, xmax = 6, ymin = 4.25, ymax = 7.5,
        fill = "white",
        color = "black"
      ) +
      annotate("text", x = 4.5, y = 5.875, size = 6, label = "Infants are highly sensitive to\nthe music in their environment.") +
      geom_rect(
        xmin = 3, xmax = 6, ymin = 0.5, ymax = 3.75,
        fill = "white",
        color = "black"
      ) +
      annotate("text", x = 4.5, y = 2.125, size = 6, label = "There is a well-documented\nrelationship between music\n and linguistic skills in children.") +
      geom_rect(
        xmin = 6.5, xmax = 9.5, ymin = 4.25, ymax = 7.5,
        fill = "white",
        color = "black"
      ) +
      annotate("text", x = 8, y = 5.875, size = 6, label = "Infants encounter almost an\n hour of music each day.") +
      geom_rect(
        xmin = 6.5, xmax = 9.5, ymin = 0.5, ymax = 3.75,
        fill = "white",
        color = "black"
      ) +
      annotate("text", x = 8, y = 2.125, size = 6, label = "Music and language share \nseveral structural similarities.") 
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlotly({
    time <- sample(1:3600, 200)
    event <- c("Vocalization")
    event_type <- sample(event, 200, replace = TRUE)
    categories <- c("Canonical (sophisticated)", "Noncanonical", "Laughing", "Crying")
    category <- sample(categories, 200, replace = TRUE)
    
    music_segments <- c(10:30, 130:243, 313:353, 553:573, 773:800, 1100:1120, 1140:1220, 2500:2700)
    music_event <- c("Music")
    music_event_type <- sample(music_event, length(music_segments), replace = TRUE)
    tunes <- c(rep("Shake It Off", 21), rep("Children's TV Show", 114), rep("Radio", 41), rep("Wheels on the Bus",21), rep("Itsy Bitsy Spider", ,28), rep("The Alphabet Song",21), rep("Radio",81), rep("Movie Soundtrack", 201))
    music <- data.frame(music_segments, music_event_type, tunes)
    colnames(music) <- c("time", "event_type", "category")
    
    timeline <- data.frame(time, event_type, category)
    timeline_data <- rbind(music, timeline)
    
    p <- ggplot(timeline_data, aes(x = time, y = event_type, fill = category)) +
      geom_tile(width = 3, height = .75) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank(),                             
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
      ) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(
        breaks = c(1, 900, 1800, 2700, 3600),
        labels = c("8:00 AM", "12:00 PM", "4:00 PM", "8:00 PM", "12:00 AM"))
    
    ggplotly(p) %>% 
      layout(
        dragmode = "pan",  
        margin = list(b = 200),
        xaxis = list(
          title = "Time",
          tickmode = "array",
          tickvals = c(1, 900, 1800, 2700, 3600),
          ticktext = c("8:00 AM", "12:00 PM", "4:00 PM", "8:00 PM", "12:00 AM"),
          showline = TRUE,
          linewidth = 2
        ),
        yaxis = list(fixedrange = TRUE)
      )
    
  })
  
  # Findings plots
  output$live_recorded_plot <- renderPlotly({
    data2 <- summary %>% select(source, source_prop)
    
    live <- data2 %>% filter(source == "Live")
    
    mean_live <- mean(live$source_prop, na.rm = TRUE)
    
    recorded <- data2 %>% filter(source == "Recorded")
    
    mean_recorded <- mean(recorded$source_prop, na.rm = TRUE)
    
    colnames(data2) <- c("Type of Music", "Canonical Babbling Proportion")
    
    live_recorded <- ggplot(data2, aes(x = `Type of Music`, y = `Canonical Babbling Proportion`)) +
      geom_jitter(width = .1, height = .1) +
      labs(x = "Type of music",
           y = "Proportion of vocalizations that are sophisticated") +
      theme_minimal() +
      theme(
        theme(plot.title = element_text(face = "bold"))
      ) +
      geom_segment(aes(x = .75, xend = 1.25, y = mean_live, yend = mean_live), 
                   color = "#1CBC9C", linetype = "solid", size = 1) +
      geom_segment(aes(x = 1.75, xend = 2.25, y = mean_recorded, yend = mean_recorded), 
                   color = "#1CBC9C", linetype = "solid", size = 1) +
      scale_y_continuous(
        limits = c(0.0, 0.5),
        breaks = c(0, .1, .2, .3, .4, .5),
        labels = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5"))
    
    ggplotly(live_recorded) %>%
      layout(
        title = list(
          text = "<b>Infants produce more sophisticated vocalizations \nduring live music than during recorded music.</b>",
          font = list(size = 14),
          yaxis = list(
            tickmode = "array",
            tickvals = c(0.0,0.1, 0.2, 0.3, 0.4, 0.5),
            ticktext = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5"),
            fixedrange = TRUE)
        )
      )
  })
  
  output$vocal_instrumental_plot <- renderPlotly({
    
    data3 <- summary %>% select(instrumentation, instrumentation_prop)
    
    vocal <- data3 %>% filter(instrumentation == "Vocal")
    
    mean_vocal <- mean(vocal$instrumentation_prop, na.rm = TRUE)
    
    instrumental <- data3 %>% filter(instrumentation == "Instrumental")
    
    mean_instrumental <- mean(instrumental$instrumentation_prop, na.rm = TRUE)
    
    colnames(data3) <- c("Type of Music", "Canonical Babbling Proportion")
    
    vocal_instrumental <- ggplot(data3, aes(x = `Type of Music`, y = `Canonical Babbling Proportion`)) +
      geom_jitter(width = .1, height = .1) +
      labs(x = "Type of music",
           y = "Proportion of vocalizations that are sophisticated") +
      theme_minimal() +
      theme(
        theme(plot.title = element_text(face = "bold"))
      ) +
      geom_segment(aes(x = .75, xend = 1.25, y = mean_instrumental, yend = mean_instrumental), 
                   color = "#1CBC9C", linetype = "solid", size = 1) +
      geom_segment(aes(x = 1.75, xend = 2.25, y = mean_vocal, yend = mean_vocal), 
                   color = "#1CBC9C", linetype = "solid", size = 1) +
      scale_y_continuous(
        limits = c(0.0, 0.5),
        breaks = c(0, .1, .2, .3, .4, .5),
        labels = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5"))
    
    ggplotly(vocal_instrumental) %>%
      layout(
        title = list(
          text = "<b>Infants produce more sophisticated vocalizations \nduring vocal music than during instrumental music.</b>",
          font = list(size = 14),
          yaxis = list(
            tickmode = "array",
            tickvals = c(0.0,0.1, 0.2, 0.3, 0.4, 0.5),
            ticktext = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5"),
            fixedrange = TRUE)
        )
      )

  })
  
  output$novelty_plot <- renderPlotly({

    data4 <- summary %>% select(novelty, novelty_prop)
    
    novel <- data4 %>% filter(novelty == "Novel")
    
    mean_novel <- mean(novel$novelty_prop, na.rm = TRUE)
    
    repeated <- data4 %>% filter(novelty == "Repeated")
    
    mean_repeated <- mean(repeated$novelty_prop, na.rm = TRUE)
    
    colnames(data4) <- c("Type of Music", "Canonical Babbling Proportion")
    
    novelty <- ggplot(data4, aes(x = `Type of Music`, y = `Canonical Babbling Proportion`)) +
      geom_jitter(width = .1, height = .1) +
      labs(x = "Type of music",
           y = "Proportion of vocalizations that are sophisticated",
           title = "Infants produce more sophisticated vocalizations \nduring novel music than during repeated music.") +
      theme_minimal() +
      theme(
        theme(plot.title = element_text(face = "bold"))
      ) +
      geom_segment(aes(x = .75, xend = 1.25, y = mean_novel, yend = mean_novel), 
                   color = "#1CBC9C", linetype = "solid", size = 1) +
      geom_segment(aes(x = 1.75, xend = 2.25, y = mean_repeated, yend = mean_repeated), 
                   color = "#1CBC9C", linetype = "solid", size = 1) +
      scale_y_continuous(
        limits = c(0.0, 0.5),
        breaks = c(0, .1, .2, .3, .4, .5),
        labels = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5"))
    
    
    ggplotly(novelty) %>%
      layout(
        title = list(
          text = "<b>Infants produce more sophisticated vocalizations \nduring novel music than during repeated music.</b>",
          x = .5,
          font = list(size = 14),
          yaxis = list(
            tickmode = "array",
            tickvals = c(0.0,0.1, 0.2, 0.3, 0.4, 0.5),
            ticktext = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5"),
            fixedrange = TRUE)
        )
      )
    
  })
  
  output$timing_plot <- renderPlotly({
    
    data5 <- summary %>% select(timing, timing_prop)
    
    before <- data5 %>% filter(timing == "Before")
    
    mean_before <- mean(before$timing_prop, na.rm = TRUE)
    
    after <- data5 %>% filter(timing == "After")
    
    mean_after <- mean(after$timing_prop, na.rm = TRUE)
    
    colnames(data5) <- c("Type of Music", "Canonical Babbling Proportion")
    
    data5$`Type of Music` <- factor(data5$`Type of Music`, levels = c("Before", "After"))
    
    timing <- ggplot(data5, aes(x = `Type of Music`, y = `Canonical Babbling Proportion`)) +
      geom_jitter(width = .1, height = .1) +
      labs(x = "Type of music",
           y = "Proportion of vocalizations that are sophisticated",
           title = "Infants produce more sophisticated vocalizations \nafter music than before music.") +
      theme_minimal() +
      theme(
        theme(plot.title = element_text(face = "bold"))
      ) +
      geom_segment(aes(x = .75, xend = 1.25, y = mean_before, yend = mean_before), 
                   color = "#1CBC9C", linetype = "solid", size = 1) +
      geom_segment(aes(x = 1.75, xend = 2.25, y = mean_after, yend = mean_after), 
                   color = "#1CBC9C", linetype = "solid", size = 1) +
      scale_y_continuous(
        limits = c(0.0, 0.5),
        breaks = c(0, .1, .2, .3, .4, .5),
        labels = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5"))
    
    
    ggplotly(timing) %>%
      layout(
        title = list(
          text = "<b>Infants produce more sophisticated vocalizations \nafter music than before music.</b>",
          x = .5,
          font = list(size = 14),
          yaxis = list(
            tickmode = "array",
            tickvals = c(0.0,0.1, 0.2, 0.3, 0.4, 0.5),
            ticktext = c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5"),
            fixedrange = TRUE)
        )
      )
    
  })
  
}
