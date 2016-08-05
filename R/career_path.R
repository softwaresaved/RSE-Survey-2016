### Analysis about the work indicator, salary and the career path

# Setting up the directory
## Work only on linux -- if it is not working needs to insert manually
this.dir = system("pwd", intern = T) 
setwd(this.dir)
setwd('~/git/ssi/RSE-Survey-2016/')

## @knitr sourceFunc
source('./R/functions.R')

## @knitr loadFile
df <- read.csv('./data/592_full_clean.csv',  na.strings=c("NA","NaN", " ", ""))

## @knitr salaryVSturnover
    library(dplyr)
    salaryVSworkInd <- as.data.frame(df %>% 
                                        group_by(Socio.salary) %>%
                                        selectD(TurnOver.Agg, AffSat.Agg, AffRec.Agg, PerfCheck.Agg, PerfCheck.Agg) %>%
                                        summarise_each(funs(mean(.))))  # Need abs to take the negation into account


## @knitr tableAcademy

## @knitr testCareer
## 
x <- rnorm(10000)
hist(x)


