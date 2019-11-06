#
# Album Wordclouds Version 1.0.0 
# Eric Hochberger 11/6/19

#install.packages(c("shiny", "rvest", "tidyverse", "janitor", "wordcloud", "tm", "RColorBrewer", "jpeg", "shinyWidgets"))


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

# Define UI 
ui <- fluidPage(
  
   
   # Application title
   titlePanel(
              h1("Album Word Cloud Generator")),
   

   sidebarLayout(
      sidebarPanel(id = "sidebar",
        textInput("text", label = (h3("Please paste the Genius.com album-page URL for your desired album below: ")), 
                  value = "https://genius.com/albums/Jorja-smith/Lost-found"),
        prettyRadioButtons(inputId = "palette",
                     label = h3("Choose color palette:"),
                     choices = c("Default", "Album Cover"),
                     animation = "jelly",
                     selected = "Album Cover"
                      ),
        helpText(a(href = "https://genius.com", "Genius.com"), "is the world's biggest collection of song lyrics and musical knowledge.",
                 "The example URL is from Jorja Smith's album Lost & Found."),
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

# Define server logic
server <- function(input, output) {
   
  

# Wordcloud ---------------------------------------------------------------

  
  src <- reactive({
    input$text %>% read_html() %>% html_node('img.cover_art-image') %>% html_attr("src") 
    
  })
  
  
  pal <- reactive({
    z <- tempfile()
    download.file(src(),z,mode="wb")
    pic <- readJPEG(z)
    file.remove(z)
    
    palette_tbl <- tibble(
      R = as.vector(pic[,,1]),
      G = as.vector(pic[,,2]),
      B = as.vector(pic[,,3])
    )
    
    k_means <- kmeans(palette_tbl, centers = 10, iter.max = 100) 
    
    rgb(k_means$centers)[1:10]
    
  })
  
  
  
  
  album_songs <- reactive({
    
    input$text %>% read_html() %>% html_nodes('.chart_row-content-title') %>% html_text() %>% 
      trimws() %>% 
      str_replace_all("Lyrics", " ") %>% 
      str_replace_all("\n", " ") %>% 
      trimws() %>% 
      make_clean_names() %>% 
      str_replace_all("_", "-")
    
  })
  
  artist <- reactive({
    input$text %>%
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
          str_replace_all(input$text %>% read_html() %>% html_node("a.header_with_cover_art-primary_info-primary_artist") %>% html_text(), " ") %>%
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

  output$picture<-renderText({c('<img src="',src(),'">')}) 
   

# Title -------------------------------------------------------------------

   
   output$title <- renderText({
     
     album_title <- input$text %>% read_html() %>% 
       html_nodes('li.breadcrumb.breadcrumb-current_page') %>% 
       html_text() %>% 
       str_remove_all("\n") %>% 
       trimws()
     
      artist_name <- input$text %>%
        read_html() %>%
        html_node("a.header_with_cover_art-primary_info-primary_artist") %>%
        html_text()
     
      paste(album_title, "by", artist_name, sep = " ")
   })
  
  
  
   
   #Duplicate of title so title is stylized correctly in both tabs
   output$title1 <- renderText({
     
     album_title <- input$text %>%
       read_html() %>% 
       html_nodes('li.breadcrumb.breadcrumb-current_page') %>% 
       html_text() %>% 
       str_remove_all("\n") %>% 
       trimws()
     
     artist_name <- input$text %>%
       read_html() %>%
       html_node("a.header_with_cover_art-primary_info-primary_artist") %>%
       html_text()
     
     paste(album_title, "by", artist_name, sep = " ")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

