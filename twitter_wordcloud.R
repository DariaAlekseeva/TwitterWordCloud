library(twitteR)
library(shiny)
library(tm)
library(wordcloud)
library(memoise)


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(value) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  
  tweets = strip_retweets(searchTwitter(value, n=1000))
  list <- sapply(tweets, function(x) x$getText())
  list <- iconv(list,to="utf-8")
  list <- gsub('(http[^ ]*)', '', list)
  list <- gsub('the', '', list)
  
  
  
  corpus <- Corpus(VectorSource(list)) # use the corpus
  # function
  # - a corpus is the text body consisting of all the text including the
  # meta info
  
  corpus <- tm_map(corpus, tolower, lazy=TRUE) # putting text to lower
  # case
  corpus <- tm_map(corpus, removePunctuation, lazy=TRUE) # remove
  # punct.
  corpus <- tm_map(corpus,
                   function(x) removeWords(x, c(stopwords(kind = 'en'), 'the')), lazy=TRUE)  
  # remove
  #stopwords (meaningless words)
    
  #- to trasform to plain text which wordcloud can use
  corpus <- tm_map(corpus, PlainTextDocument)
  myDTM = TermDocumentMatrix(corpus,
                             control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

### server.R

server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing 2000 tweets takes some time...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(8,1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Set2"), random.order = FALSE)
  })
}


## ui.R

ui <- fluidPage(theme = "http://185.137.92.115:8080/bootstrap.css",
    # Application title
  titlePanel("Shiny Twitter Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      textInput("selection", "Enter your hashtag...",
                value = "#russia"),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)

shinyApp(ui = ui, server = server)