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

## Filter only on RSE score higher or equal to 2
    df <- df[df$RSE.score >=2, ]
    
## Filter the sample on UK people
    df <- df[df$Socio.country == 'United Kingdom',] 


## Disciplines

  # Plot
    sumQ <- singleTabFreq(df$Edu.academic.CLEAN, 'Field of Education')
    sumQ
    # png('/figs/education_field.png', width=1400, height=1000)
    p <- plotSingleFreq(sumQ, 'Field of Education', column= 'Percent', vertical_label=TRUE)

        # theme(axis.text.x=element_blank(), plot.title=element_blank(), axis.title.y=element_blank())
    p
    # dev.off()

## Level of education
  # Reorder the factor
    levels(df$Edu.highest_qualification)
    levels(df$Edu.highest_qualification) <- c(NA, 'Doctorate', 'Undergraduate/Others', 'Master Degree', 'Undergraduate/Others', 'Undergraduate/Others',
                                              'Undergraduate/Others', 'Undergraduate/Others')
    levels(df$Edu.highest_qualification)
    # df$Edu.highest_qualification = factor(df$Edu.highest_qualification, levels(df$Edu.highest_qualification)[c(1,3,2)])
  # Plot

    sumQ <- singleTabFreq(df$Edu.highest_qualification, 'level of Education')
    sumQ
    png('plots/education_level.png', width=1400, height=1000)
    p <- plotSingleFreq(sumQ, 'Level of Education', column='Percent')
        # theme(axis.text.x=element_blank(), plot.title=element_blank(), axis.title.y=element_blank())
    p
    dev.off()


## Salary
  # Reorder factors
    df$Socio.salary = factor(df$Socio.salary, levels(df$Socio.salary)[c(10,9,1:8)])
  # Plot
    sumQ <- singleTabFreq(df$Socio.salary, 'Salary', order=FALSE)
    plotSingleFreq(sumQ, 'Salary', order=FALSE)

## Bus Factor
  # Plot
    sumQ <- singleTabFreq(df$Stability.bus_factor, 'Bus Factor', order=FALSE)
    sumQ
    png('plots/stability_bus-factor.png', width=1400, height=1000)
    plotSingleFreq(sumQ, 'Bus Factor', column = 'Percent', order=FALSE)
        # theme(plot.title=element_blank(), axis.title.y=element_blank(), legend.position='none')
    dev.off()

    png('plots/stability_bus-factor_blank.png', width=1400, height=1000)
    plotSingleFreq(sumQ, 'Bus Factor', column = 'Percent', order=FALSE, bar_label = FALSE)
        # theme(plot.title=element_blank(), axis.title.y=element_blank(), legend.position='none')
    dev.off()


## Technical hand over
  # Plot
    
    levels(df$Stability.hand_over) <- c(NA, 'No', 'Yes')
    sumQ <- singleTabFreq(na.omit(df$Stability.hand_over), 'Technical hand over', order=TRUE)
    sumQ
    png('plots/stability_hand-over.png', width=1400, height=1000)
    plotSingleFreq(sumQ, 'Technical hand over', column = 'Percent', order=TRUE)

    dev.off()

    png('plots/stability_hand-over_blank.png', width=1400, height=1000)
    plotSingleFreq(sumQ, 'Technical hand over', column = 'Percent', order=TRUE, bar_label=FALSE)  +
        theme(axis.text.x=element_blank(), plot.title=element_blank(), axis.title.y=element_blank(), legend.position='none')
    dev.off()

## Gender
    # Gender in IT -- Data from: https://docs.google.com/spreadsheets/d/1nr2ukhV2rNInLTR210yBKEvJowU9vfg6rkckFoi_1Io/edit#gid=349336051
        # Gender in RSE
        sumQ <- singleTabFreq(df$Socio.gender, 'Gender', order=TRUE)
        sumQ$Discipline <- c('RSE', 'RSE')
        # Gender in IT -- Data from: https://docs.google.com/spreadsheets/d/1nr2ukhV2rNInLTR210yBKEvJowU9vfg6rkckFoi_1Io/edit#gid=349336051
        genderDf <- read.csv('data/gender_disciplines.csv')
        genderDf$Discipline <- as.character(genderDf$Discipline)
        ## TODO -- FIX THAT NOT DOING IT MANUALLY WRONG
        genderDf <- rbind(genderDf, c('RSE', '12', '88'))
        genderDf$Discipline <- as.factor(genderDf$Discipline)
        genderDf <- melt(genderDf, c('Discipline'))
        genderDf$value <- as.numeric(genderDf$value)
    # Plot
        png('plots/gender.png', width=1400, height=1000)
        p <-    ggplot(data=genderDf[genderDf$variable =='Female',], aes(x=reorder(Discipline, value), y=value))+
                # geom_bar(stat='identity', position=position_dodge())+
                geom_bar(stat='identity', show.legend = FALSE, fill='#4A8F94', colour='#4A8F94')+
                #geom_bar(stat='identity') +
                #scale_color_brewer(palette='#4A8F94')+
                theme_minimal()+
                ylab('Percentage')+
                xlab('')+
                ggtitle('Gender in CS and RSE')+
                theme(plot.title = element_text(size=30, face='bold'))+
                theme(legend.position='none')+
                theme(legend.text=element_text(size=20))+
                theme(legend.title=element_blank())+
                theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
                #geom_text(aes(label=mergedDF$Gender), vjust=-0.2, size=8)+
                geom_text(aes(label=paste(value, '%')),  vjust=-0.2, size=4)
         p
         dev.off()
         png('plots/gender_blank.png', width=1400, height=1000)
         p <-    ggplot(data=genderDf[genderDf$variable =='Female',], aes(x=reorder(Discipline, value), y=value))+
             # geom_bar(stat='identity', position=position_dodge())+
             geom_bar(stat='identity', show.legend = FALSE, fill='#4A8F94', colour='#4A8F94')+
             #geom_bar(stat='identity') +
             #scale_color_brewer(palette='#4A8F94')+
             theme_minimal()+
             ylab('Percentage')+
             xlab('')+
             theme(legend.position='none')+
             theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
         p
         dev.off()


## Contribution papers
    ## Plot
        levels(df$Contrib.YN) <- c(NA, 'No', 'Yes')
        sumQ <- singleTabFreq(df$Contrib.YN, 'Contribution to paper')
        sumQ
        png('plots/contribution_paper.png', width=1400, height=1000)
        plotSingleFreq(sumQ, 'Contribution to paper', column = 'Percent', order=TRUE)
        dev.off()
        png('plots/contribution_paper_blank.png', width=1400, height=1000)
        plotSingleFreq(sumQ, 'Contribution to paper', column = 'Percent', order=TRUE, bar_label = FALSE) +
            theme(axis.text.x=element_blank(), plot.title=element_blank(), axis.title.y=element_blank(), legend.position='none')
        dev.off()

## Work in University: find the number of universities that replied (from the original list)
    # Load the different universities used
    list_universities <- read.csv('informations/list_universities.csv', header = FALSE)
    list_ucl <- read.csv('informations/list_ucl.csv', header = FALSE)
    fullUni <-  data.frame(c(as.character(list_ucl$V1), as.character(list_universities$V1)))
    fullUni
    #fullUni <- as.factor(fullUni$c.as.character.list_ucl.V1...as.character.list_universities.V1..)
    fullUni$In.Data <- fullUni$c.as.character.list_ucl.V1...as.character.list_universities.V1.. %in% levels(df$Job.uni.CLEAN)
    summary(fullUni$In.Data)

    
## Indicators
    ## Create df
    dfIndicators <- data.frame('Turnover intention'=df$TurnOver.Agg, 'Perceived Employability'=df$PercEmp.Agg,
                               'Satisfaction'=df$AffSat.Agg, 'Recognition'=df$AffRec.Agg,
                               'Feedback'= df$PerfCheck.Agg)

    cronbach(cbind(df$TurnOver.1.Recode, df$TurnOver.2.Recode, df$TurnOver.3.Recode, df$TurnOver.4.Recode, df$TurnOver.5.Recode,
             df$TurnOver.6.Recode))
    summary(df$TurnOver.Agg)
    sd(na.omit(df$TurnOver.Agg))
    cronbach(cbind(df$AffSat.1.Recode, df$AffSat.2.Recode, df$AffSat.3.Recode, df$AffSat.4.Recode))
    summary(df$AffSat.Agg)
    sd(na.omit(df$AffSat.Agg)) 
    
    cronbach(cbind(df$AffRec.1.Recode, df$AffRec.2.Recode, df$AffRec.3.Recode, df$AffRec.4.Recode, df$AffRec.5.Recode)) 
    summary(df$AffRec.Agg)
    sd(na.omit(df$AffRec.Agg))
    cronbach(cbind(df$PercEmp.1.Recode, df$PercEmp.2.Recode, df$PercEmp.3.Recode, df$PercEmp.4.Recode)) 
    summary(df$PercEmp.Agg)
    sd(na.omit(df$PercEmp.Agg))
    cronbach(cbind(df$PerfCheck.1.Recode, df$PerfCheck.2.Recode, df$PerfCheck.3.Recode, df$PerfCheck.4.Recode,
                   df$PerfCheck.5.Recode, df$PerfCheck.6.Recode, df$PerfCheck.7.Recode))
    dfIndicators <- melt(dfIndicators)
    summary(df$PerfCheck.Agg)
    sd(na.omit(df$PerfCheck.Agg))
    png('plots/indicators.png')
    ggplot(dfIndicators, aes(x=variable, y=value, color=variable))+
        geom_boxplot(show.legend=FALSE)+
        geom_jitter(alpha=0.25, color='Grey')+
        scale_color_brewer(palette='Paired') +
        theme_minimal() +
        ylab('Mean of Aggregate score') +
        xlab('') +
        # ggtitle('Work Indicators') +

        theme(plot.title = element_text(size=30, face='bold')) +
        theme(legend.text=element_text(size=20)) +
        theme(legend.title=element_blank())
    dev.off()

    png('plots/indicators_blank.png')
    ggplot(dfIndicators, aes(x=variable, y=value, color=variable))+
        geom_boxplot(show.legend=FALSE)+
        geom_jitter(alpha=0.25, color='Grey')+
        scale_color_brewer(palette='Paired') +
        theme_minimal() +
        xlab('') +
        theme(axis.text.x=element_blank(), plot.title=element_blank(), axis.title.y=element_blank(), legend.position='none')
    dev.off()

    # Career path
    ## Create the df
    dfCareer <- data.frame('Career Plan' =df$ProgRSE.1.Recode, 'Next Position'= df$ProgRSE.2.Recode, 
                           'Promotion'=df$ProgRSE.3.Recode, 'Information'=df$ProgRSE.4.Recode,
                           'Opportunities'=df$ProgRSE.5.Recode)
    dfCareerMelt <- melt(dfCareer)
    summary(dfCareer$Career.Plan)
    sd(dfCareer$Career.Plan, na.rm = TRUE)
    summary(dfCareer$Next.Position)
    sd(dfCareer$Next.Position, na.rm=TRUE)
    summary(dfCareer$Promotion)
    sd(dfCareer$Promotion, na.rm=TRUE)
    summary(dfCareer$Information)
    sd(dfCareer$Information, na.rm=TRUE)
    summary(dfCareer$Opportunities)
    sd(dfCareer$Opportunities, na.rm = T)
    ggplot(na.omit(dfCareerMelt), aes(x=variable, y=value, color=variable))+
        geom_boxplot(show.legend=FALSE)+
        geom_jitter(alpha=0.25, color='Grey')+
        scale_color_brewer(palette='Paired') +
        theme_minimal() +
        ylab('Score') +
        xlab('') +
        # ggtitle('Career path') +

        theme(plot.title = element_text(size=30, face='bold')) +
        theme(legend.text=element_text(size=20)) +
        theme(legend.title=element_blank())

#### word cloud ####
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

  ### Most important skills
  skill1_important <- df$Misc.skill.imp.1
  skill2_important <- df$Misc.skill.imp.2
  skill3_important <- df$Misc.skill.imp.3
  skills_important <- c(as.character(skill1_important),
                        as.character(skill2_important),
                        as.character(skill3_important))
  wordToRemove <- c('skills', 'ability', 'skill')
  skills_important <- cleanText(skills_important, wordToRemove = wordToRemove)
  png('plots/wordcloud_skill_important.png')
  wordcloud(skills_important, random.color=FALSE, colors=brewer.pal(8, "Paired"))
  dev.off()

  ### Skills to improve
  skills1_to_improve <- df$Misc.skill.better.1
  skills2_to_improve <- df$Misc.skill.better.2
  skills3_to_improve <- df$Misc.skill.better.3
  skills_improve <- c(as.character(skills1_to_improve),
                      as.character(skills2_to_improve),
                      as.character(skills3_to_improve))
  skills_improve <- cleanText(skills_improve, wordToRemove = wordToRemove)
  wordToRemove <- c('skill', 'skills', 'better', 'improve')
  png('plots/wordcloud_skill_improve.png')
  wordcloud(skills_improve, random.color=FALSE, colors=brewer.pal(8, "RdYlGn"))
  dev.off()


  ## Tools used
  tool1 <- df$Misc.tool.1
  tool2 <- df$Misc.tool.2
  tool3 <- df$Misc.tool.3
  all_tools <- c(as.character(tool1),
                 as.character(tool2),
                 as.character(tool3))

  all_tools <- cleanText(all_tools, wordToRemove = c('tools', 'system'))
  png('plots/wordcloud_tools.png')
  wordcloud(all_tools, random.color=FALSE, colors=brewer.pal(8, "RdYlGn"))
  dev.off()

  # Bigram
  # http://stackoverflow.com/questions/8898521/finding-2-3-word-phrases-using-r-tm-package
  # https://rstudio-pubs-static.s3.amazonaws.com/70214_2f123afdd4a04498b4e6194b846dd86a.html
  # Function to get the bigrams
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))







    ## FIXME -- DONE IN A RUSH AND EVEN MANUALLY ENTER THE %. NEED TO REDO ALL THIS BIT
    dfContrib <- data.frame('Contribution'=df$Contrib.YN, 'Acknowledged'=df$Contrib.acknowledgedYN, 
                            'Co-author'=df$`Contrib.co-authorYN`, 'Lead-author'=df$Contrib.leadYN)
    levels(dfContrib$Contribution) <- c(NA, 'No', 'Yes')
    summary(dfContrib$Contribution)
    sd(na.omit(dfContrib$Contribution))
    
    levels(dfContrib$Acknowledged) <- c(NA, 'No', 'Yes')
    summary(na.omit(dfContrib$Acknowledged))
    levels(dfContrib$Co.author) <- c(NA, 'No', 'Yes')
    sd(na.omit(dfContrib$Co.author))
    levels(dfContrib$Lead.author) <- c(NA, 'No', 'Yes')
    summary(dfContrib$Lead.author)
    sd(na.omit(dfContrib$Lead.author))
    dfContribMelt <- melt(dfContrib)
    
    
    
        sumQ <- singleTabFreq(df$Contrib.YN, 'Contribution to paper')
        sumQ
       sumQ$Participation <- 'Contribution to paper' 
    df1 <- as.data.frame(summary(na.omit(dfContrib$Contribution)))
    df2 <- as.data.frame(summary(na.omit(dfContrib$Acknowledged)))
    df3 <- as.data.frame(summary(na.omit(dfContrib$Co.author)))
    df4 <- as.data.frame(summary(na.omit(dfContrib$Lead.author)))
    dfTest <- merge(df1, df2)
    dfTest <- merge(dfTest, df3)
    dfTest <- merge(dfTest, df4)
    dfTest <- dfTest[c(1, 16),]
    rownames(dfTest) <- c('No', 'Yes')
    colnames(dfTest) <- c('1.Contribution', '2.Acknowledged', '3.Co-author', '4.Lead-author')
    dfTestMelt <- as.data.frame(t(dfTest))
    dfTestMelt$Paper <- rownames(dfTestMelt)
    dfTestMelt <- melt(dfTestMelt)
    dfTestMelt$Percent <- c((27/(27+230))*100, 
                          (55/(55+174))*100,
                          (72/(72+158))*100,
                          (139/(139+91))*100,
                          (230/(230+27))*100,
                          (174/(174+55))*100,
                          (158/(158+72))*100,
                          (91/(91+139))*100)
    
    colnames(dfTestMelt) <- c('Contribution', 'Answer', 'value', 'Percent')
    ggplot(dfTestMelt, aes(Contribution, y=Percent, fill=Answer))+
        geom_bar(stat='identity', position=position_dodge(width=1)) +
        geom_text(aes(label=paste(round(Percent), '%'), 
                      size=4), vjust=-0.2, 
                      position=position_dodge(width = 1))+
        theme_minimal() +
        scale_fill_brewer(palette='Paired')+
        theme(legend.text=element_text(size=15)) +
        theme(legend.title=element_blank())
           # geom_bar(aes(fill=Answer, position='dodge'))






