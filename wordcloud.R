wordcloud <- function()
  {
  
  #++++++++++++++++++++++++++++++++++
  # rquery.wordcloud() : Word cloud generator
  # - http://www.sthda.com
  #+++++++++++++++++++++++++++++++++++
  # x : character string (plain text, web url, txt file path)
  # type : specify whether x is a plain text, a web page url or a file path
  # lang : the language of the text
  # excludeWords : a vector of words to exclude from the text
  # textStemming : reduces words to their root form
  # colorPalette : the name of color palette taken from RColorBrewer package, 
  # or a color name, or a color code
  # min.freq : words with frequency below min.freq will not be plotted
  # max.words : Maximum number of words to be plotted. least frequent terms dropped
  # value returned by the function : a list(tdm, freqTable)


    library("tm")
    library("SnowballC")
    library("wordcloud")
    library("RColorBrewer")
    library("quanteda")
    
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##define the datasources
  file1 <- "inc_125173.txt"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  
  ##setup variables
  lang<-c("english")
  colorPalette<-c("Dark2")
  excludeWords<-c("please")
  textStemming<-FALSE
  min.freq<-3
  max.words<-200
  
  
    x<-read.table(file_loc1, header = FALSE, sep="\t", dec = ".")
    type<-c("text")
  
    text<-x
    ##if(type[1]=="file") text <- readLines(x)
    ##else if(type[1]=="url") text <- html_to_text(x)
    ##else if(type[1]=="text") text <- x
    
    # Load the text as a corpus
    docs <- Corpus(VectorSource(text))
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove stopwords for the language 
    docs <- tm_map(docs, removeWords, stopwords(lang))
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Remove your own stopwords
    if(!is.null(excludeWords)) 
      docs <- tm_map(docs, removeWords, excludeWords) 
    # Text stemming
    if(textStemming) docs <- tm_map(docs, stemDocument)
    # Create term-document matrix
    tdm <- TermDocumentMatrix(docs)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    # check the color palette name 
    ##if(!colorPalette %in% rownames(brewer.pal.info))
    ##  colors = colorPalette
    ##else colors = brewer.pal(8, colorPalette)
    colors = brewer.pal(8,colorPalette)
    # Plot the word cloud
    set.seed(1234)
    ##wordcloud(d$word
      ##        ,d$freq
      ##        ,min.freq=min.freq
      ##        ,max.words=max.words
      ##        ,random.order=FALSE
      ##        ,rot.per=0.35
      ##        ,scale = c(4,.5)
      ##        ,use.r.layout=FALSE
      ##        ,colors=colors)
    
    wordcloud(d$word
              ,d$freq
              ,scale = c(4,.5)
              ,min.freq = min.freq
              ,max.words = max.words
              ,random.order=TRUE)

    invisible(list(tdm=tdm, freqTable = d))
    
    ##output dataframe
    write.csv(d, file = "C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/out/125173_unigram_frequency.csv")
    
  }
  #++++++++++++++++++++++
  # Helper function
  #++++++++++++++++++++++
  # Download and parse webpage
  ##html_to_text<-function(url){
  ##  library(RCurl)
  ##  library(XML)
  ##  # download html
  ##  html.doc <- getURL(url)  
    #convert to plain text
  ##  doc = htmlParse(html.doc, asText=TRUE)
    # "//text()" returns all text outside of HTML tags.
    # We also don't want text such as style and script codes
  ##  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    # Format text vector into one character string
  ##  return(paste(text, collapse = " "))
  ##}