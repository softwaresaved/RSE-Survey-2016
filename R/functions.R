#### Personal functions used in the several steps of analysis

#### Loading libraries
    library('ggplot2')
    library('tm') # txt cleaning
    library('wordcloud')
#### Functions ####

  singleTabFreq <- function(vectorToFreq, name, order=TRUE){
    # Function that output a data frame containing the variable, the Frequency and
    # rounded percent
    # Transform the vector into a table of frequency
    table_factor <- table(vectorToFreq)
    # Cbind is to combine two different sets into one data.frame
    # the conversion of data.frame within the cbind is to avoid having
    # the factor in data_row for the frequency
    # And being able to index only the value for the percentage
    QSummary <- cbind(as.data.frame(table_factor),
                      as.data.frame(round(prop.table(table_factor)*100, digits = 2))[,2])

    #rename the column names
    colnames(QSummary) <- c(name, 'Total Respondents', 'Percent')
    # Optional order is to output the table ordered in Frequency (useful for output with
    # kable
    if (order==TRUE){
      # Order the table by Total Respondents
      QSummary <- QSummary[order(QSummary[,2]),]
      # Reorder the level of the factors for using in legend (to match the order of the plot)
      QSummary[,1] <- factor(QSummary[,1], levels=QSummary[,1][order(QSummary[,2])], ordered=order)
    }
    # Output a table of the variable
    return (QSummary)
  }

plotSingleFreq <- function(dataframe, name, column='Total Respondents', order=TRUE, vertical_label=FALSE, bar_label=TRUE,
                           title=FALSE){
    index_column <- match(column, names(dataframe))
    # To reorganise the factor level to show on the legend according to the
    # frequencies
    # environment=environment() is a workaround for ggplot2 to get access to the
    # namespace defined within the function.
    p <- ggplot(data=dataframe, environment=environment())
    if (order==TRUE){
      p <- p + aes(x=reorder(dataframe[,1], dataframe[, index_column]),
                   y=dataframe[,index_column],
                   fill=dataframe[,1])
    }
    else {
      p <- p + aes(x=dataframe[,1],
                   y=dataframe[,index_column],
                   fill=dataframe[,1])
    }
    p <- p + geom_bar(stat='identity')
    p <- p + scale_fill_brewer(palette='Paired')
    p <- p + theme_minimal()
    p <- p + ylab(column)
    p <- p + xlab('')
    if (title==TRUE) {
        p <- p + ggtitle(name)
    }
    #p <- p + ggtitle(bquote(atop(.(name), atop(italic(.(column)), "")))) ## Title and subtitle under
    p <- p + theme(plot.title = element_text(size=24, face='bold'))

    if (vertical_label==TRUE){
      p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=20))
    }
    else {
      p <- p + scale_x_discrete(labels = function(x) str_wrap(str_replace(x,"-","- "), width = 4))
      p <- p + theme(axis.text.x =element_text(size=20))
    }
    p <- p + theme(legend.title=element_blank())
    p <- p + theme(legend.text=element_text(size=15))
    if (bar_label== TRUE){
       if (column=='Percent'){
           p <- p + geom_text(aes(label=paste(dataframe[, index_column], '%')), vjust=-0.2, size=8)
        }
        else {
           p <- p + geom_text(aes(label=dataframe[,index_column]), vjust=-0.2, size=8)
        }
    }
    
    return (p)
}

## https://georeferenced.wordpress.com/2013/01/15/rwordcloud/
#### Function to clean text Input: vector of caracteres
    cleanText <- function(x, wordToRemove) {
        ## Transform into a Corpus
        x <- Corpus(VectorSource(x))
        ## Lower words
        x <- tm_map(x, content_transformer(tolower))
        # Remove punctuations
        x <- tm_map(x, removePunctuation)
        ## Remove stop words
        x <- tm_map(x, removeWords, stopwords('english'))
        ## Remove words from list passed in arg
        x <- tm_map(x, removeWords, wordToRemove)
        ## Strip whitespace
        x <- tm_map(x, stripWhitespace)
        return(x)
    }
    
    

####  Function to recode the time into num
    recodeLikert <- function(x, inverting=FALSE) {
      # Remove spaces
      if (is.na(x)) {
        x <- NA
      }
      else{
        x <- gsub("^\\s+|\\s+$", "", x)
    
        if (x == ""){
          x <- NA
        }
        else if (x == 'Never') {
          x <- 0
        }
        else if (x == 'Sometime') {
          x <- 1
        }
        else if (x == '1 (Strongly disagree)'){
          x <- 1
        }
        else if (x == 'Often'){
          x <- 2
        }
        else if (x == '2') {
          x <- 2
        }
        else if (x == 'Very Often'){
          x <- 3
        }
        else if (x == '3') {
          x <- 3
        }
        else if (x == 'Always'){
          x <- 4
        }
        else if (x == '4'){
          x <- 4
        }
        else if (x == '5 (Strongly Agree)') {
          x <- 5
        }
      }
      if (inverting == TRUE) {
        return(-x)
      }
      else{
        return(x)
      }
    }

# Function to clean the percentages. If higher then 3, that means it is not percentages
# and need to be removed
    removeValues <- function(variable) {
      if (nchar(variable) >3) {
        x <- NA
      }
      else if (nchar(variable) == 0) {
        x <- NA
      }
      else {
        x <- variable
      }
      return(x)
    }