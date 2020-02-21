# Wordclouds
A way to visually explore the lyrics of your favorite albums with Genius.com. 


## Run Locally
Run Wordclouds locally by executing the following code in R:

```
packages <- c("shiny", "tidyverse", "rvest", "janitor", "wordcloud", "tm", "RColorBrewer", "jpeg", "shinyWidgets", "shinythemes", "png", "jsonlite")
install.packages(packages)
library(shiny)
runGitHub("album-wordclouds", "eric-hochberger")

```

## Run On a Browser
https://erichochberger.shinyapps.io/AlbumWordclouds/

## Screenshots
![Screen Shot 2019-11-22 at 1 53 57 PM](https://user-images.githubusercontent.com/55408707/69456239-c4869600-0d2f-11ea-9eb5-bff70a252635.png)

![Screen Shot 2019-11-22 at 1 56 47 PM](https://user-images.githubusercontent.com/55408707/69456325-ff88c980-0d2f-11ea-90b8-8465d74bf56d.png)
