wordcloud2<-function(){
  
  
  
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer")
  library("quanteda")
  
  ##setup variables
  lang<-c("english")
  colorPalette<-c("Dark2")
  excludeWords<-c("please")
  textStemming<-FALSE
  min.freq<-3
  max.words<-200
  
  ##set the path to DeploymentPerformance file
  path <- paste0("C:/Users/andrewll/OneDrive - Microsoft/WindowsPowerShell/Data/in")
  
  ##define the datasources
  file1 <- "inc_125173.txt"
  
  ##define the Deployments file path
  file_loc1 <- file.path(path, file1)
  
  x<-read.table(file_loc1, header = FALSE, sep="\t", dec = ".")
  type<-c("text")
  
  text <- Corpus(VectorSource(x))
  
  train.tokens<-as.tokenizedTexts(text, what = c("word"), removeNumbers=TRUE)
  
  
  
  
  
}