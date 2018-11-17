library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
load(
  url(
    "http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"
  )
)
n_total <- nrow(movies)
all_studios <- sort(unique(movies$studio))

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
        selectInput(inputId = "studio",
                    label = "Select studio:",
                    choices = all_studios,
                    selected = "20th Century Fox",
                    multiple = TRUE,
                    selectize = TRUE)
      )
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "densityplot", height = 200),
      DT::dataTableOutput(outputId = "moviestable")
    )
  ))

# Define server function required to create the scatterplot
server <- function(input, output) {
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(
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
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)