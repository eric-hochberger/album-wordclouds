# Wordclouds version 2.0.0 by Eric Hochberger
# 11/20/19
#
# install.packages("shiny", "rvest", "tidyverse", "janitor", "wordcloud", "tm", "RColorBrewer", "jpeg", "shinyWidgets", "shinythemes", "png")

library(shiny)
library(rvest)
library(tidyverse)
library(janitor)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(jpeg)
library(shinyWidgets)
library(shinythemes)
library(png)


ui <- fluidPage(
  
  # Application title
  titlePanel(
    h1("Album Word Cloud Generator")),
  
  
  sidebarLayout(
    sidebarPanel(id = "sidebar",
                 textInput("text", label = (h3("Please type the artist's name whose albums you wish to explore: ")), 
                           value = "Kanye West"),
                 prettyRadioButtons(inputId = "palette",
                                    label = h3("Choose color palette:"),
                                    choices = c("Default", "Album Cover"),
                                    animation = "jelly",
                                    selected = "Album Cover"
                 ),
                 uiOutput('variables'),
                 helpText("All data courtesy of ", a(href = "https://genius.com", "Genius.com,")," the world's biggest collection of song lyrics and musical knowledge.",
                          "The wordcloud tab displays the most frequently used words on the album with size corresponding to their relative frequency.
                          The summary tab displays a histogram of the 10 most frequently used words ordered by the total number of mentions on the labum"),
                 htmlOutput("picture")
                 
                 
                 
                 ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Cloud",
                           h1(textOutput("title1"), align = "center"),
                           tags$head(tags$style("#title1{color: red;
                                                font-size: 30px;
                                                font-style: italic;
                                                }"
                         )
                           ),
                         plotOutput("Plot", width = "100%", height = "600px")),
                  tabPanel("Summary",
                           h1(textOutput("title"), align = "center"),
                           tags$head(tags$style(paste("#title{color: red;
                                                      font-size: 30px;
                                                      font-style: italic;
                                                      }", sep = "")
                         )
                           ),
                         plotOutput("sum_Plot", width = "100%", height = "600px")
                           )
                  
                  
                           ) #end of tabsetPanel
                  ) #end of mainPanel
                           )
    )
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  url <- reactive({
    name_formatted <- input$text %>% str_replace_all("[[:punct:]]", "") %>% str_replace_all(" ", "-")
    paste("https://genius.com/artists/", name_formatted, sep = "")
    
  })
  
  metadata <- reactive({
    read_html(url()) %>% 
      html_nodes("meta[itemprop='page_data']") %>% #Access page metadata
      html_attr("content") %>% #Render metadata as JSON
      fromJSON() 
    
  })
  
  albumlist <- reactive({
    
    metadata <- metadata()
    metadata$artist_albums$name
  })
  
  output$variables = renderUI({
    selectInput('variables2', 'Albums', albumlist())
    
  })
  
  tib <- reactive({
    metadata <- metadata()
    tibble(
      
      y = metadata$artist_albums$name,
      x = metadata$artist_albums$url
    )
    
  })
  
  album_url <- reactive({
    
    tib <- tib() %>% filter(y == input$variables2) %>% select(x)
    return(tib[[1]])
    
  })
  
  
  
  
  # Wordcloud section (all input$text below here should be album_url) -------
  
  src <- reactive({
    album_url() %>% read_html() %>% html_node('img.cover_art-image') %>% html_attr("src") 
    
  })
  
  
  pal <- reactive({
    z <- tempfile()
    download.file(src(),z,mode="wb") # save album cover jpeg to tempfile
    if (str_detect(src(), ".png") == TRUE) {
      pic <- readPNG(z)
      file.remove(z)
      pic <- writeJPEG(pic, quality = 1)
      pic <- readJPEG(pic)
    }
    else {
      pic <- readJPEG(z)
      file.remove(z) #remove tempfile
    }
    palette_tbl <- tibble(
      R = as.vector(pic[,,1]),
      G = as.vector(pic[,,2]),
      B = as.vector(pic[,,3])
    )
    
    k_means <- kmeans(palette_tbl, centers = 10, iter.max = 50) 
    
    rgb(k_means$centers)[1:10]
    
  })
  
  
  
  
  album_songs <- reactive({
    
    album_url() %>% read_html() %>% html_nodes('.chart_row-content-title') %>% html_text() %>% 
      trimws() %>% 
      str_replace_all("Lyrics", " ") %>% 
      str_replace_all("\n", " ") %>% 
      trimws() %>% 
      make_clean_names() %>% 
      str_replace_all("_", "-")
    
  })
  
  artist <- reactive({
    album_url() %>%
      str_extract("(?<=albums).*/") %>% str_remove_all("/")
    
  })
  
  
  lyrics <- reactive({
    
    
    words <- ""
    for(i in album_songs() ) {
      url <- paste("https://genius.com/", artist(), "-", i, sep = "") %>% 
        str_remove("-lyrics") %>% 
        str_remove("-ft-.*") %>%
        str_remove("-number") %>% 
        paste("-lyrics", sep = "")
      
      if (str_detect(url, "credits") == TRUE) next
      
      try(
        words <-url %>% read_html() %>% html_node("div.song_body-lyrics") %>% 
          html_text() %>% 
          str_replace_all("\n|\\[.*?\\]|,|-|More on Genius|Lyrics", " ") %>% 
          str_replace_all("\'", "") %>% 
          str_replace_all(album_url() %>% read_html() %>% html_node("a.header_with_cover_art-primary_info-primary_artist") %>% html_text(), " ") %>%
          str_replace_all("([a-z])([A-Z])", "\\1 \\2") %>%
          append(words)
      )
    }
    
    
    # Process album lyrics --------------------------------------
    
    
    words <- Corpus(VectorSource(words))
    words <- tm_map(words, stripWhitespace)
    words <- tm_map(words, content_transformer(tolower))
    words <- tm_map(words, removeWords, stopwords('english'))
    words <- tm_map(words, removePunctuation)
    words
    
    
    
    
    
  })
  
  
  
  # Wordcloud ---------------------------------------------------------------
  
  
  
  output$Plot <- renderPlot({
    
    
    pal_input <- switch (input$palette,
                         "Album Cover" = pal(),
                         "Default" = brewer.pal(10, "Paired")
    )
    
    
    lyrics() %>% wordcloud(colors = pal_input, random.order = FALSE, random.color = FALSE, rot.per = .25, max.words = 100, scale = c(5, 0.5))
    
    
    
    
  })
  
  
  # Histogram ---------------------------------------------------------------
  
  
  output$sum_Plot <- renderPlot({
    
    pal_input <- switch (input$palette,
                         "Album Cover" = pal(),
                         "Default" = brewer.pal(10, "Paired")
    )
    
    lyrics1 <- TermDocumentMatrix(lyrics()) %>% as.matrix()
    
    sort(rowSums(lyrics1), decreasing = TRUE) %>% enframe() %>% 
      filter(row_number() <= 10) %>% ggplot(aes(fct_reorder(name, desc(value)), value)) + 
      geom_col(aes(fill = name), color = "gray29") + 
      theme_minimal() +
      scale_fill_manual(values = pal_input ) +
      scale_x_discrete(NULL) +
      scale_y_continuous("Count\n", expand = c(0,0)) +
      theme(
        axis.text = element_text(size = 18, face = "bold", colour = "black"),
        axis.title = element_text(size = 20, face = "bold"),
        legend.position = "none"
      )
    
  })
  
  
  
  
  # Album cover -------------------------------------------------------------
  
  picture <- reactive({
    c('<img src="',src(),'">')
    
  })
  
  output$picture<-renderText({
    
    picture()
    
  }) 
  
  
  # Title -------------------------------------------------------------------
  
  
  output$title <- renderText({
    
    album_title <- album_url() %>% read_html() %>% 
      html_nodes('li.breadcrumb.breadcrumb-current_page') %>% 
      html_text() %>% 
      str_remove_all("\n") %>% 
      trimws()
    
    artist_name <- album_url() %>%
      read_html() %>%
      html_node("a.header_with_cover_art-primary_info-primary_artist") %>%
      html_text()
    
    paste(album_title, "by", artist_name, sep = " ")
  })
  
  
  
  
  #Duplicate of title so title is stylized correctly in both tabs
  output$title1 <- renderText({
    
    album_title <- album_url() %>%
      read_html() %>% 
      html_nodes('li.breadcrumb.breadcrumb-current_page') %>% 
      html_text() %>% 
      str_remove_all("\n") %>% 
      trimws()
    
    artist_name <- album_url() %>%
      read_html() %>%
      html_node("a.header_with_cover_art-primary_info-primary_artist") %>%
      html_text()
    
    paste(album_title, "by", artist_name, sep = " ")
  })
  
  
  
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

