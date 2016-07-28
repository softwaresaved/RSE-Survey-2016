### Analysis about the work indicator, salary and the career path

## @knitr sourceFunc
source('./R/functions.R')

## @knitr loadFile
df <- read.csv('./data/592_full_clean.csv',  na.strings=c("NA","NaN", " ", ""))

## @knitr variablesXY

x<-1:100
y<-x+rnorm(100)
head(data.frame(x,y))

## @knitr tableAcademy
singleTabFreq(df$Edu.academic.CLEAN, 'Academy')

## @knitr plotXY

plot(x,y)

