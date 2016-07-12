### This script analyse the RSE Survey data from the clean dataset
### created from the cleaning.R script.
## It does the univariate analysis, cross-tabulation, chi-square and ANOVA
## Mainly plotting results and output them into png files

### For text mining
library(rmarkdown)
library(knitr)


library('tm') # txt cleaning
library('wordcloud')
library('ggplot2')
library('stringr')  # To wrap the too long labels in the plots
library('psy')
library('reshape2')


# Setting up the directory
(WD <- getwd())
if (!is.null(WD)) setwd(WD)
setwd('~/git/ssi/RSE-Survey-2016/')

### Source the function file
source('./R/functions.R')

## LOAD FILE
    df <- read.csv('./data/592_op_clean.csv',  na.strings=c("NA","NaN", " ", ""))


## Disciplines
## ---- chunk-1
  # Plot
    sumQ <- singleTabFreq(df$Edu.academic.CLEAN, 'Field of Education')
    # png('/figs/education_field.png', width=1400, height=1000)
    p <- plotSingleFreq(sumQ, 'Field of Education', column= 'Percent', vertical_label=TRUE)

        # theme(axis.text.x=element_blank(), plot.title=element_blank(), axis.title.y=element_blank())
    p
    # dev.off()