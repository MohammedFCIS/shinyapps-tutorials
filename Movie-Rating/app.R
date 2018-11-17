library(shiny)
library(tidyverse)
library(DT)

load(
  url(
    "http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"
  )
)
n_total <- nrow(movies)
all_studios <- sort(unique(movies$studio))
min_date <- min(movies$thtr_rel_date)
max_date <- max(movies$thtr_rel_date)

# Define UI for application that plots features of movies
ui <- fluidPage(# Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs
    sidebarPanel(
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "IMDB rating"          = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics score"        = "critics_score",
          "Audience score"       = "audience_score",
          "Runtime"              = "runtime"
        ),
        selected = "audience_score"
      ),
      
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "IMDB rating"          = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics score"        = "critics_score",
          "Audience score"       = "audience_score",
          "Runtime"              = "runtime"
        ),
        selected = "critics_score"
      ),
      
      # Select variable for color
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          "Title type" = "title_type",
          "Genre" = "genre",
          "MPAA rating" = "mpaa_rating",
          "Critics rating" = "critics_rating",
          "Audience rating" = "audience_rating"
        ),
        selected = "mpaa_rating"
      ),
      # Set alpha level
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0,
        max = 1,
        value = 0.5
      ),
      # Text instructions
      HTML(paste("Enter a value between 1 and", n_total)),
      
      # Numeric input for sample size
      numericInput(
        inputId = "n",
        label = "Sample size:",
        value = 30,
        step = 1,
        min = 1,
        max = n_total
      ),
      # Studio Selector
      sidebarPanel(
        selectInput(
          inputId = "studio",
          label = "Select studio:",
          choices = all_studios,
          selected = "20th Century Fox",
          multiple = TRUE,
          selectize = TRUE
        )
      ),
      
      # Explanatory text
      HTML(
        paste0(
          "Movies released between the following dates will be plotted.
          Pick dates between ",
          min_date,
          " and ",
          max_date,
          "."
        )
      ),
      
      # Break for visual separation
      br(),
      br(),
      # Date input
      dateRangeInput(
        inputId = "date",
        label = "Select dates:",
        start = "2013-01-01",
        end = "2014-01-01",
        min = min_date,
        max = max_date,
        startview = "year"
      )
      
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = "plot_brush"),
      br(),
      textOutput(outputId = "correlation"),
      br(),
      htmlOutput(outputId = "avgs"),
      verbatimTextOutput(outputId = "lmoutput"), # regression output
      br(),
      plotOutput(outputId = "densityplot", height = 200),
      dataTableOutput(outputId = "moviestable1"),
      br(),
      DT::dataTableOutput(outputId = "moviestable")
    )
  ))

# Define server function required to create the scatterplot
server <- function(input, output) {
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    req(input$date)
    movies_selected_date <- movies %>%
      mutate(thtr_rel_date = as.Date(thtr_rel_date)) %>% # convert thtr_rel_date to Date format
      filter(thtr_rel_date >= input$date[1] &
               thtr_rel_date <= input$date[2])
    ggplot(data = movies_selected_date, aes_string(
      x = input$x,
      y = input$y,
      color = input$z
    )) +
      geom_point(alpha = input$alpha)
  })
  
  # Create densityplot
  output$densityplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x)) +
      geom_density()
  })
  
  # Create data table
  output$moviestable <- DT::renderDataTable({
    req(input$n)
    req(input$studio)
    movies_sample <- movies %>%
      filter(studio %in% input$studio) %>%
      #sample_n(input$n) %>%
      select(title:studio)
    DT::datatable(
      data = movies_sample,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # Create text output stating the correlation between the two ploted 
  output$correlation <- renderText({
    r <- round(cor(movies[, input$x], movies[, input$y], use = "pairwise"), 3)
    paste0("Correlation = ", r, ". Note: If the relationship between the two variables is not linear, the correlation coefficient will not be meaningful.")
  })
  
  # Create data table
  output$moviestable1 <- DT::renderDataTable({
    brushedPoints(movies, brush = input$plot_brush) %>% 
      select(title, audience_score, critics_score)
  })
  
  # Calculate averages
  output$avgs <- renderUI({
    avg_x <- movies %>% pull(input$x) %>% mean() %>% round(2)
    avg_y <- movies %>% pull(input$y) %>% mean() %>% round(2)
    HTML(
      paste("Average", input$x, "=", avg_x),
      "<br/>",
      paste("Average", input$y, "=", avg_y)
    )
  })
  
  # Create regression output
  output$lmoutput <- renderPrint({
    x <- movies %>% pull(input$x)
    y <- movies %>% pull(input$y)
    summ <- summary(lm(y ~ x, data = movies)) 
    print(summ, digits = 3, signif.stars = FALSE)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)