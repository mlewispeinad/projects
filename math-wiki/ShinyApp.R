library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)
library(lubridate)
library(jpeg)
library(tidyr)
library(png)

df <- read.csv("MathWikipediaCorrected.csv")
df$Date <- as.Date(df$Date)
df$Month <- as.character(df$Month)





df$Month <- factor(df$Month, levels = 1:12, labels = month.name)

ui <- fluidPage(
  titlePanel("Math Wikipedia Page Views Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("page", h3("Page:"), choices = unique(df$Page)),
      dateRangeInput("date_range", h3("Date Range:"),
                     start = min(df$Date), end = max(df$Date)),
      selectInput("select", h3("Slide"), 
                  choices = list("Default" = 'wiki', "Pi Day" = 'Pi',
                                 "Fibonacci Day" = 'Fibonacci Number', 'Coding Theory' = 'Coding Theory',
                                 'Exponential Growth' = 'Exponential Growth', 'Cramers Rule' = "Cramer's Rule", 
                                 'Fourier Series' = 'Fourier Series', "Dirichlet's Theorem" = "Dirichlet's Theorem", 
                                 'Group Theory' = 'Group Theory', 'Topology' = 'Topology'), selected = 1),
      
      checkboxInput('Image', "Display Images", value = FALSE)
    ),
    mainPanel(plotOutput("time_series_plot"))
  )
)
server <- function(input, output) {
  
  filtered_data <- reactive({
    
    if (input$select == 'wiki') {
      df %>%
        filter(Page == input$page,
               Date >= input$date_range[1],
               Date <= input$date_range[2])
      
    } else if (input$select == 'Pi') {
      df %>%
        filter(Page == 'Pi')
      
    } else if (input$select == 'Fibonacci Number') {
      df %>%
        filter(Page == 'Fibonacci number')
      
    } else if (input$select == 'Coding Theory') {
      df %>%
        filter(Page == 'Coding theory',
               Date >= "2020-02-28",
               Date <= "2020-08-30")
      
    } else if (input$select == 'Exponential Growth') {
      df %>%
        filter(Page == 'Exponential growth')
      
    } else if (input$select == "Cramer's Rule") {
      df %>%
        filter(Page == "Cramer's rule",
               Date <= "2019-05-30")
    } else if (input$select == 'Fourier Series') {
      df %>%
        filter(Page == "Fourier series",
               Date <= '2019-08-31',
               Date >= '2019-05-01') 
    } else if (input$select == "Dirichlet's Theorem") {
      df %>%
        filter( Page == "Dirichlet's theorem on arithmetic progressions",
                Date >= '2019-08-01',
                Date <= '2019-12-31') 
      
    } else if (input$select == 'Group Theory') {
      df %>%
        filter( Page == "Group theory",
                Date <= "2020-10-01", 
                Date >= "2020-08-08") 
    } else if (input$select == 'Topology') {
      df %>%
        filter(
          Page == 'Topology',
          Date >= "2020-02-01",
          Date <= "2020-03-30" )
    }
    
    
  })
  
  output$time_series_plot <- renderPlot({
    plot <- ggplot(filtered_data(), aes(x = Date, y = Views, color = Month, 
                                        group = 1)) +
      geom_line(size = 1.2) +
      scale_x_continuous(breaks = seq(min(filtered_data()$Date), max(filtered_data()$Date), length.out = 5)) +
      scale_y_continuous(breaks = seq(min(filtered_data()$Views), max(filtered_data()$Views), length.out = 5))
    
    
    plot_build <- ggplot_build(plot)
    ytick <- na.omit(plot_build[["layout"]][["panel_params"]][[1]][["y"]][["breaks"]])
    xtick <- na.omit(plot_build[["layout"]][["panel_params"]][[1]][["x"]][["breaks"]])
    wiki <- "Images/wiki.png"
    pi <- "Images/pi.jpg"
    fib <- "Images/fib.png"
    conway <- "Images/conway.jpg"
    exp <- "Images/expog.png"
    cramer <- "Images/cramer.png"
    forier <- "Images/forier.png"
    dichrolet <- "Images/dichrolet.png"
    group <- "Images/group.png"
    topology <- "Images/topology.png"
    
    log <- ifelse(input$select == 'wiki', wiki, 
                  ifelse(input$select == "Pi", pi, 
                         ifelse(input$select == 'Fibonacci Number', fib, 
                                ifelse(input$select == "Coding Theory",  conway,   
                                       ifelse(input$select == "Exponential Growth",  exp,
                                              ifelse(input$select == "Cramer's Rule", cramer,
                                                     ifelse(input$select == "Fourier Series", forier,
                                                            ifelse(input$select == "Dirichlet's Theorem", dichrolet,
                                                                   ifelse(input$select == 'Group Theory', group, 
                                                                          topology)))))))))      
    
    path <- getwd()
    if (input$Image) {
      img_file <- paste(path, log, sep = "/") 
      if (str_sub(img_file, -3) == 'png') {
        img <- readPNG(img_file)
      } else (img <- readJPEG(img_file)) }
    
    
    ggplot(filtered_data(), aes(x = Date, y = Views, color = Month, 
                                group = 1)) +
      geom_line(size = 1.2) +
      scale_x_continuous(breaks = seq(min(filtered_data()$Date), max(filtered_data()$Date), length.out = 5)) +
      scale_y_continuous(breaks = seq(min(filtered_data()$Views), max(filtered_data()$Views), length.out = 5)) +
      theme(plot.title = element_text(size = 30),
            legend.text = element_text(size = 14),
            legend.key = element_rect(fill = "white", color = "black"),
            axis.text.x = element_text(size = 15)) +
      labs(title = paste("Views for", ifelse(input$select == 'wiki', input$page, input$select)), 
           x = "Date", y = "Number of Views") + {
             if (input$Image == TRUE) { 
               annotation_raster(
                 img,
                 xmin = xtick[length(xtick) - 2] + (xtick[length(xtick) - 1] - xtick[length(xtick) - 2])/1.5, 
                 xmax = xtick[length(xtick)] - (xtick[length(xtick) - 1] - xtick[length(xtick) - 2])/10,  
                 ymin = (ytick[length(ytick) - 2] + ytick[length(ytick) - 1])/2.5,  
                 ymax = ytick[length(ytick)]) }
           } +
      scale_x_date(date_labels = "%b %Y", labels = month.labels) 
    
    
    
  })
  
}

shinyApp(ui = ui, server = server)
