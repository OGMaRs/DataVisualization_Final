# UI
library(shiny)
library(shinydashboard)
# Data
library(DT)
library(tidyverse)
library(scales)
library(lubridate)
library(stats)
library(formattable)
# Pic
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(wordcloud)
# Model
library(class)
# Crawling
library(rvest)

## read data
data_origin = read_csv("titles.csv")
credit_origin = read_csv("credits.csv")


##ui
ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title = "Netflix movies"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "page1", icon = icon("fas fa-book")),
      menuItem("Dataset", tabName = "page2", icon = icon("database")),
      menuItem("Analysis1", tabName = "page3", icon = icon("chart-bar")),
      menuItem("Analysis2", tabName = "page4", icon = icon("chart-area")),
      menuItem("Comparision", tabName = "page5", icon = icon("compress-arrows-alt"))
    )
  ),

  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "page1", 
              fluidRow(
                box(
                  title = "About the Project", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12,
                         tags$div(
                           tags$span(
                             p("This dataset contains movies and shows from Netflix over the past 30 years. We want to visualize the data to show more intuitively the preferences of users of different age groups for different movies.
")
                           )
                         ) 
                  )
                )
              ),  
              fluidRow(
                box(
                  title="About the Motivation",solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12,
                         tags$div(
                           tags$span(
                             p("This project has the following main objectives: "),
                             p("1.Developing a content-based recommender system using the genres and/or descriptions."),
                             p("2.Identifying the main content available on the streaming."),
                             p("3.Network analysis on the cast of the titles."),
                             p("4.Exploratory data analysis to find interesting insights.")
                           )
                         ) 
                  )
                )
              ),  
              
              fluidRow(
                box(
                  title = "Dataset", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12,
                         tags$div(
                           tags$span(
                             p("This dataset contains +5k unique titles on Netflix with 15 columns containing their information, including:"),
                             p("id: The title ID on JustWatch."),
                             p("title: The name of the title"),
                             p("show_type: TV show or movie."),
                             p("description: A brief description."),
                             p("release_year: The release year."),
                             p("age_certification: The age certification."),
                             p("runtime: The length of the episode (SHOW) or movie."),
                             p("genres: A list of genres."),
                             p("production_countries: A list of countries that produced the title."),
                             p("seasons: Number of seasons if it's a SHOW."),
                             p("imdb_id: The title ID on IMDB."),
                             p("imdb_score: Score on IMDB."),
                             p("imdb_votes: Votes on IMDB."),
                             p("tmdb_popularity: Popularity on TMDB."),
                             p("tmdb_score: Score on TMDB.")
                           )
                         ) 
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Team member", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12,
                         tags$div(
                           tags$span(
                             p("Yuting Li"),
                             p("Marc Liu"),
                             p("Rachel Zheng"),
                             p("Xixuan Huang"),
                             p("Yu Zhang")
                           )
                 ) 
          )
        )
      )
    ),
      
      
      
      tabItem(tabName = "page2",
              dataTableOutput("plot2")
      ),
      
      
      tabItem(tabName = "page3",
              titlePanel("Popularities of Each Age Certification(2010 - 2022):"),
              br(),br(),
              sliderInput("Plot3Input", "Select a year to start analyse:",
                          min = 2010, max = 2022,
                          value = 2010, step = 1, width = "100%",
                          animate = animationOptions(interval = 1000, loop = FALSE)
              ),
              plotOutput("plot3", width="100%")
              
      ),
      
      tabItem(tabName = "page4",
              h2("The average performance change of movie and shows in a specific period:"),
              sliderInput("year_Begin", "The beginning year:",
                          min = 1945, max = 2022,
                          value = 1945, step = 1,width = "100%"
              ),
              sliderInput("year_End", "The end year:",
                          min = 1945, max = 2022,
                          value = 2022, step = 1, width = "100%"
              ),
              selectInput("select",
                          label = "Choose the feature for evaluate ", width = "100%",
                          choices = list("number of movies" = "num_movie", 
                                           "average popularity" = "ave_popularity", 
                                           "average runtime" = "ave_runtime", 
                                           "average imdb score" = "ave_imdb_score",
                                           "average tmdb score" = "ave_tmdb_score"),
                          selected = 1
              ),
              plotOutput("plot4")
                
                
                
        ),
      
      tabItem(tabName = "page5",
              titlePanel("Movie Comparision"),
              box(
                width="100%",
                
                fluidRow(
                  column(6,
                         selectInput("movie1","First Movie",
                                     choices=unique(data_origin$title),
                                     selected = unique(data_origin$title)[1]
                                     )
                  ),
                  column(6,
                         selectInput("movie2","Second Movie",
                                     choices=unique(data_origin$title),
                                     selected = unique(data_origin$title)[1]
                         )
                  ),
                ),
                box(
                  width="100%",
                  uiOutput("plot5")
                ),
              )
      )
    )
  )
)



server <- function(input, output, session) {
  
  data = data_origin %>%
    select(id, title, release_year, runtime, imdb_score,tmdb_popularity,tmdb_score) %>%
    na.omit()
  
  # Plot 1
  
  
  
  # Plot 2
  output$plot2 = renderDataTable({
    return(datatable(data_origin, rownames= FALSE))
  })
  # Plot 3
  output$plot3 = renderPlot({
    s<-data_origin %>% filter(release_year==input$Plot3Input)
    m<-s %>% group_by(age_certification) %>% summarize(mean(as.numeric(tmdb_popularity),na.rm=TRUE))
    colnames(m)<-c("age_certification","tmdb_popularity")
    
    plotgra <- ggplot(m, aes(reorder(age_certification,-tmdb_popularity),tmdb_popularity,fill=age_certification))+
      geom_col() + 
      xlab("Age Certification") +
      ylab("TMBD Popularity") +
      guides(fill=guide_legend(title="Age Certification")) +
      labs(title = 'Age Cert. vs. Popularity') +
      theme_tufte()  +
      theme(plot.title = element_text(size = 22)) +
      theme(legend.text = element_text(size = 15)) +
      theme(legend.title = element_text(size = 15)) +
      theme(axis.title.x = element_text(size = 15)) +
      theme(axis.title.y = element_text(size = 15))
    
    plotgra
    
  })
  
  # plot 4
  output$plot4 = renderPlot({
    Year_Begin = input$year_Begin
    Year_End = input$year_End
    caldata = data %>%
      filter(release_year >= Year_Begin) %>%
      filter(release_year <= Year_End) %>%
      group_by(release_year) %>%
      summarise(
        "num_movie" = n(),
        "ave_popularity" = mean(tmdb_popularity),
        "ave_runtime" = mean(runtime),
        "ave_imdb_score" = mean(imdb_score),
        "ave_tmdb_score" = mean(tmdb_score)
      )
    
    
    if (Year_Begin > Year_End) {
      ggplot(NULL) + annotate("text",
                              label = "The end year should greater than the beginning year",
                              x = 1, y = 1, size = 6 ) + theme_bw() + theme(
                                plot.background = element_blank(),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                axis.title = element_blank(),
                                axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                legend.position = "none"
                              )
    } else {
      index = input$select
      xlable = seq(Year_Begin, Year_End, by = 5)
      ggplot(data = caldata) +
        aes_string("release_year", index, group = 1) +
        geom_point(color="steelblue",size=2) +
        geom_line(color="steelblue") +
        labs(x="Years",y=index) +
        theme_classic()
    }
    
  })
  
  
  # plot 5
  
  output$plot5 <- renderUI({
    firstmovie = input$movie1
    secondmovie = input$movie2
    
    id = data_origin[data_origin$title ==firstmovie,]$id[1]
    name = credit_origin[credit_origin$id ==id,]$name[1]
    character = credit_origin[credit_origin$id ==id,]$character[1]
    release = data_origin[data_origin$title ==firstmovie,]$release_year[1]
    runtime = data_origin[data_origin$title ==firstmovie,]$runtime[1]
    age_certification = data_origin[data_origin$title ==firstmovie,]$age_certification[1]
    genres = data_origin[data_origin$title ==firstmovie,]$genres[1]
    production_countries = data_origin[data_origin$title ==firstmovie,]$production_countries[1]
    seasons = data_origin[data_origin$title ==firstmovie,]$seasons[1]
    imdb_score = data_origin[data_origin$title ==firstmovie,]$imdb_score[1]
    imdb_votes = data_origin[data_origin$title ==firstmovie,]$imdb_votes[1]
    tmdb_popularity = data_origin[data_origin$title ==firstmovie,]$tmdb_popularity[1]
    tmdb_score = data_origin[data_origin$title ==firstmovie,]$tmdb_score[1]
    description = data_origin[data_origin$title ==firstmovie,]$description[1]
    
    id2 = data_origin[data_origin$title ==secondmovie,]$id[1]
    name2 = credit_origin[credit_origin$id ==id2,]$name[1]
    character2 = credit_origin[credit_origin$id ==id2,]$character[1]
    release2 = data_origin[data_origin$title ==secondmovie,]$release_year[1]
    runtime2 = data_origin[data_origin$title ==secondmovie,]$runtime[1]
    age_certification2 = data_origin[data_origin$title ==secondmovie,]$age_certification[1]
    genres2 = data_origin[data_origin$title ==secondmovie,]$genres[1]
    production_countries2 = data_origin[data_origin$title ==secondmovie,]$production_countries[1]
    seasons2 = data_origin[data_origin$title ==secondmovie,]$seasons[1]
    imdb_score2 = data_origin[data_origin$title ==secondmovie,]$imdb_score[1]
    imdb_votes2 = data_origin[data_origin$title ==secondmovie,]$imdb_votes[1]
    tmdb_popularity2 = data_origin[data_origin$title ==secondmovie,]$tmdb_popularity[1]
    tmdb_score2 = data_origin[data_origin$title ==secondmovie,]$tmdb_score[1]
    description2 = data_origin[data_origin$title ==secondmovie,]$description[1]
     
    tags$div(
      fluidRow(
        column(6,
               h2(firstmovie),
               p(span("main actor", class="label label-danger"), span(name),
                 span("character", class="label label-danger"),span(character)),
               p(span("release", class="label label-danger"), span(release),
                 span("runtime", class="label label-danger"),span(runtime)),
               p(span("age certification", class="label label-success"), span(age_certification),
                 span("genres", class="label label-success"), span(genres)),
               p(span("countries", class="label label-info"), span(production_countries),
                 span("seasons", class="label label-info"), span(seasons)),
               p(span("imdb score", class="label label-warning"), span(imdb_score),
                 span("imdb votes", class="label label-warning"), span(imdb_votes)),
               p(span("tmdb popularity", class="label label-warning"), span(tmdb_popularity),
                 span("tmdb score", class="label label-warning"), span(tmdb_score)),
               h3("Description"),
               p(description)
               
                 
        ),
        column(6,
               h2(secondmovie),
               p(span("main actor", class="label label-danger"), span(name2),
                 span("character", class="label label-danger"),span(character2)),
               p(span("release", class="label label-danger"), span(release2),
                 span("runtime", class="label label-danger"),span(runtime2)),
               p(span("age certification", class="label label-success"), span(age_certification2),
                 span("genres", class="label label-success"), span(genres2)),
               p(span("countries", class="label label-info"), span(production_countries2),
                 span("seasons", class="label label-info"), span(seasons2)),
               p(span("imdb score", class="label label-warning"), span(imdb_score2),
                 span("imdb votes", class="label label-warning"), span(imdb_votes2)),
               p(span("tmdb popularity", class="label label-warning"), span(tmdb_popularity2),
                 span("tmdb score", class="label label-warning"), span(tmdb_score2)),
               h3("Description"),
               p(description)
               
               
        )
      )
    )
  })
  
  
}

shinyApp(ui = ui, server = server)