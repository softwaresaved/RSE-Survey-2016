## Univariate analysis of the RSE Survey 2016

# Setting up the directory
## Work only on linux -- if it is not working needs to insert manually
    this.dir = system("pwd", intern = T)
    setwd(this.dir)
    setwd('~/git/ssi/RSE-Survey-2016/')

## @knitr sourceFunc
    source('./R/functions.R')

## @knitr loadLibs
    library('rmarkdown')
    library('knitr')
    library('ggplot2')
    library('stringr')  # To wrap the too long labels in the plots
    library('psy')
    library('reshape2')
    library('dplyr')
    library('tm')
    library('likert')
    library('sjPlot')  # To plot likert scale a bit differently


## @knitr loadFile
    df <- read.csv('./data/dataset/592_full_clean.csv',  na.strings=c("NA","NaN", " ", ""))
    ## Removing obvious non complete responses
    df <- df[which(df$Job.contract != 'NA'), ]
    ## Remove the non UK
    df <- df[which(df$Socio.country == 'United Kingdom'),]
    ## Remove the non RSE
    # df <- df[which(df$RSE.score >=2), ]
    # df$RSE.score_NotPost <- df$RSE.dev_software + df$RSE.dev_time + df$RSE.does_computer
    # df1 <- df[which(df$RSE.score_NotPost >=2), ]
    # Set up the FONT_SIZE for the plots. Need to change it on the spot when generate pdf for article vs markdown
    # For markdown, value of 20 is ideal, if it is for pdf value of 35 is better (plot on double columns articles)
    FONT_SIZE = 35


# Socio-demographic information

## @knitr disciplinePrep
    disciplineFreq <- singleTabFreq(df$Edu.academic.CLEAN, 'Field of Education')

## @knitr disciplineTable
    kable(disciplineFreq, digits=2, format = 'markdown')

## @knitr disciplinePlot
    plotSingleFreq(disciplineFreq, 'Field of Education', column= 'Percent', vertical_label=TRUE, legend=FALSE, FONT_SIZE=FONT_SIZE)


## @knitr educationPrep
    # Reorder the factor
    levels(df$Edu.highest_qualification) <- c('Doctorate', 'Undergraduate/Others', 'Master Degree',
                                              'Undergraduate/Others', 'Undergraduate/Others',
                                              'Undergraduate/Others', 'Undergraduate/Others')
    df$Edu.highest_qualification = factor(df$Edu.highest_qualification, levels(df$Edu.highest_qualification)[c(1,3,2)])

    eduFreq <- singleTabFreq(df$Edu.highest_qualification, 'level of Education')

## @knitr educationTable
    kable(eduFreq, digits=2, format = 'markdown')

## @knitr educationPlot
    plotSingleFreq(eduFreq, 'Level of Education', column='Percent', legend=FALSE, FONT_SIZE=FONT_SIZE)


## @knitr genderPrep
    # Gender in RSE
    genderFreq <- singleTabFreq(df$Socio.gender, 'Gender', order=TRUE)

## @knitr genderTable
    kable(genderFreq, digits=2, format = 'markdown')

## @knitr genderPlot
    plotSingleFreq(genderFreq, 'Different Gender', column='Percent' , FONT_SIZE=FONT_SIZE, legend=FALSE)


## @knitr genderAllPrep
    # Recreate the genderFreq in case only the genderAll is called in the report
    genderFreq <- singleTabFreq(df$Socio.gender, 'Gender', order=TRUE)
    ## Transform the dataframe to bind it with the genderUK
    sumQMelt <- t(genderFreq[c('Gender', 'Percent')])
    RSEGender <- c('RSE', round(as.numeric(sumQMelt[2,1])), round(as.numeric(sumQMelt[2,2])))
    # Gender in IT -- Data from: https://docs.google.com/spreadsheets/d/1nr2ukhV2rNInLTR210yBKEvJowU9vfg6rkckFoi_1Io/edit#gid=349336051
    genderDf <- read.csv('data/external_dataset/gender_disciplines.csv')
    genderDf$Discipline <- as.character(genderDf$Discipline)
    genderDf <- rbind(genderDf, RSEGender)
    genderDf$Discipline <- as.factor(genderDf$Discipline)
    genderDf <- melt(genderDf, c('Discipline'))
    genderDf$value <- as.numeric(genderDf$value)

## @knitr genderAllPlot
    ggplot(data=genderDf[genderDf$variable =='Female',], aes(x=reorder(Discipline, value), y=value))+
        geom_bar(stat='identity', show.legend = FALSE, fill='#4A8F94', colour='#4A8F94')+
        theme_minimal()+
        ylab('Percent')+
        xlab('')+
        ggtitle('Gender in CS and RSE')+
        theme(plot.title = element_text(size=FONT_SIZE, face='bold'))+
        theme(legend.position='none')+
        theme(legend.text=element_text(size=FONT_SIZE))+
        theme(legend.title=element_blank())+
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
        theme(axis.text.x=element_text(size=FONT_SIZE))+
        geom_text(aes(label=paste(value, '%')),  vjust=-0.2, size=FONT_SIZE/8)


## @knitr contractPrep
    contractFreq<- singleTabFreq(df$Job.contract, 'Type of contract', order=TRUE)

## @knitr contractTable
    kable(contractFreq, digits=2, format = 'markdown')

## @knitr contractPlot
    plotSingleFreq(contractFreq, 'Type of contract', order=FALSE, legend=FALSE)


## @knitr salaryPrep
    sumQSalary <- singleTabFreq(df$Socio.salary, 'Salary', order=FALSE)
    # Reorder factors
    #TODO do in cleaning dataset
    sumQSalary$Salary <- factor(sumQSalary$Salary, levels(sumQSalary$Salary)[c(10,2,3,4,5,6,7,8,9,1)])

## @knitr salaryTable
    kable(sumQSalary, digits=2, format = 'markdown')

## @knitr salaryPlot
    plotSingleFreq(sumQSalary, 'Salary', order=FALSE, legend=FALSE)

## @knitr firstJobPrep
    sumQFirstJob <- singleTabFreq(df$Job.firstYN, 'First Job', order=FALSE)

## @knitr firstJobTable
    kable(sumQFirstJob, digits=2, format='markdown')

## @knitr firstJobPlot
    plotSingleFreq(sumQFirstJob, 'First Job', order=FALSE, legend=FALSE)


### Comparison with the salary for academic staff in UK
## @knitr salaryAllPrep
    sumQSalary <- singleTabFreq(df$Socio.salary, 'Salary', order=FALSE)
    # Reorder factors
    #TODO do in cleaning dataset
    sumQSalary$Salary <- factor(sumQSalary$Salary, levels(sumQSalary$Salary)[c(10,2,3,4,5,6,7,8,9,1)])
    # Read the data collected from HESA
    salaryRaw <- read.csv('./data/external_dataset/staff_1415_table_B.csv')
    ## Subset only the total of salary for all and the associated percentage
    salaryDf <- salaryRaw[c(16,17,18,19,20,21), c(1, 20,21)]
    ## Rename the columns
    colnames(salaryDf) <- c('Salary', 'Total Respondents', 'Percent')
    ## Rename the range of salary to match the salary obtained in the RSE survey
    ## The range are different and an approximation is made to get the closest similarity between the two
    ## surveys. The comparison needs to be carefully interpreted as the ranges are overlapping and are not cuttin
    ## at the same salary
    salaryDf$Salary <- as.factor(as.character(salaryDf$Salary))
    levels(salaryDf$Salary) <- c('Under £25,000', 'Under £25,000', '>= £25,000 and < £34,000',
                                 '>= £34,000 and < £45,000', '>= £45,000 and < £59,000', '>= £59,000')
    ## Rename the Percent value to remove the percent and transform into number
    salaryDf$Percent <- as.numeric(as.character(gsub('%', '',salaryDf$Percent)))
    ### Sum the multiple categories created by the recategorisation
    percent_All_UK <- aggregate(Percent ~ Salary, salaryDf, sum)
    percent_All_UK$type <- 'UK'
    ## Reorganise the salary from the RSE survey to match the new created categories
    sumQSalary$Salary <- as.factor(as.character(sumQSalary$Salary))
    levels(sumQSalary$Salary) <- c('>= £59,000', '>= £25,000 and < £34,000', '>= £25,000 and < £34,000',
                                   '>= £34,000 and < £45,000', '>= £34,000 and < £45,000',
                                   '>= £45,000 and < £59,000', '>= £45,000 and < £59,000',
                                   '>= £59,000', '>= £59,000', 'Under £25,000')

    ### Sum the multiple categories created by the recategorisation
    percent_RSE  <-  aggregate(Percent ~ Salary, sumQSalary, sum)
    ### Reorder factors
    percent_RSE$Salary <- factor(percent_RSE$Salary, levels(percent_RSE$Salary)[c(5,2,3,4,1)])
    percent_RSE$type <- 'RSE'
    salaryPercentAll <- melt(rbind(percent_RSE, percent_All_UK))

## @knitr salaryAllPlot
    #### Data with line only (best to see the difference)
    ggplot(salaryPercentAll, aes(x=as.numeric(Salary), y=value, colour=type))+
        geom_point(size=4)+
        geom_line(size=2)+
        geom_text(aes(label=paste(value, '%')), size=8,vjust=-0.2, show.legend = FALSE)+
        # geom_text(show.legend = FALSE) +  # To remove the symbol 'a' in legend because use geom_txt
        ggtitle('Comparison salary RSE vs UK')+
        theme_minimal() +
        scale_color_manual(values=c('#1F78B4', "#FF7F00"))+
        theme(legend.title=element_blank()) +
        xlab('Salary')+
        ylab('Percents')+
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5), labels =levels(salaryPercentAll$Salary))+
        theme(plot.title = element_text(size=FONT_SIZE))+
        theme(axis.title.x = element_blank()) +
        theme(legend.text=element_text(size=FONT_SIZE)) +
        theme(axis.title.y =element_text(size=FONT_SIZE)) +
        theme(axis.text =element_text(size=FONT_SIZE/1.5))

## @knitr salaryAllPlot2
    #### Same data but with a mix of line and barplots
    ggplot(data=percent_RSE, aes(x=Salary, y=Percent, fill=Salary))+
        geom_bar(stat='identity', show.legend = FALSE)+
        geom_line(data=percent_All_UK, aes(x=Salary, y=Percent, group='factor'), colour="#FF7F00", size=2)+
        geom_point(data=percent_All_UK, aes(x=Salary, y=Percent, group='factor'), colour="#FF7F00", size=4)+
        geom_text(data=percent_All_UK, aes(label=paste(Percent, '%')),  vjust=-2, size=6, colour='#FF7F00')+
        geom_text(aes(label=paste(Percent, '%')),  vjust=-0.2, size=FONT_SIZE/10)+
        scale_fill_manual(values =c('#1F78B4', '#1F78B4', '#1F78B4', '#1F78B4', '#1F78B4'))+
        theme_minimal()+
        ylab('Percentages')+
        xlab('')+
        ggtitle('Comparison Salary between RSE and UK academic staff')+
        theme(plot.title = element_text(size=FONT_SIZE, face='bold'))+
        theme(axis.text.x =element_text(size=FONT_SIZE))+
        theme(legend.position='none')

## @knitr salaryAllPlot3
    #### Same data but with bar plot only
    ggplot(salaryPercentAll, aes(Salary, y=value, fill=type))+
        geom_bar(stat='identity', position=position_dodge(width=1)) +
        geom_text(aes(label=paste(value, '%')), size=FONT_SIZE/4,vjust=-0.2, position=position_dodge(width = 1))+
        ggtitle('Contribution to Papers')+
        theme_minimal() +
        scale_fill_manual(values = c('#1F78B4', "#FF7F00"))+
        theme(legend.text=element_text(size= FONT_SIZE)) +
        theme(axis.text.x =element_text(size=FONT_SIZE))+
        theme(legend.title=element_blank())


# Good practices

## @knitr busFactorPrep
    busFactorFreq <- singleTabFreq(df$Stability.bus_factor, 'Bus Factor', order=FALSE)

## @knitr busFactorTable
    kable(busFactorFreq, digits=2, format = 'markdown')

## @knitr busFactorPlot
    plotSingleFreq(busFactorFreq, 'Bus Factor', column = 'Percent', order=FALSE, legend=FALSE, FONT_SIZE=FONT_SIZE)


## @knitr handOverPrep
    handOverFreq <- singleTabFreq(df$Stability.hand_over, 'Technical hand over', order=TRUE)

## @knitr handOverTable
    kable(handOverFreq, digits=2, format = 'markdown')

## @knitr handOverPlot
    plotSingleFreq(handOverFreq, 'Technical hand over', column = 'Percent', order=TRUE, legend=FALSE, FONT_SIZE=FONT_SIZE)


## @knitr contribYNPrep
    sumQ <- singleTabFreq(df$Contrib.YN, 'Contribution to paper')

## @knitr contribYNTable
    kable(sumQ, digits=2, format = 'markdown')

## @knitr contribYNPlot
    plotSingleFreq(sumQ, 'Contribution to paper', column = 'Percent', order=TRUE, FONT_SIZE=FONT_SIZE, legend=FALSE)


## @knitr contribAllPrep
    dfContrib <- data.frame('Acknowledged'=df$Contrib.acknowledgedYN,
                            'Co-author'=df$Contrib.co.authorYN, 'Lead-author'=df$Contrib.leadYN)
    dfContribFreq <- data.frame(apply(dfContrib, 2, table))
    dfContribFreq$Answer <- rownames(dfContribFreq)
    dfContribMelt <- melt(dfContribFreq, id='Answer')
    dfContribMelt <- dfContribMelt %>%
                        group_by(variable) %>%
                        mutate(Percent = value/sum(value)*100)
    colnames(dfContribMelt) <- c('Answer', 'Participation', 'Total', 'Percent')

## @knitr contribAllPlot
    ggplot(dfContribMelt, aes(Participation, y=Percent, fill=Answer))+
        geom_bar(stat='identity', position=position_dodge(width=1)) +
        geom_text(aes(label=paste(round(Percent), '%')), size=FONT_SIZE/3,vjust=-0.2, position=position_dodge(width = 1))+
        theme_minimal() +
        scale_fill_manual(values = c("#FF7F00", '#1F78B4'))+
        theme(legend.text=element_text(size=FONT_SIZE)) +
        theme(axis.text =element_text(size=FONT_SIZE))+
        theme(legend.title=element_blank())+
        theme(axis.title.x = element_blank())+

        theme(axis.title.y =element_text(size=FONT_SIZE))


## @knitr workIndicatorPrep
    ## Create df
    dfIndicators <- data.frame('Turnover intention'=df$TurnOver.Agg, 'Perceived Employability'=df$PercEmp.Agg,
                               'Satisfaction'=df$AffSat.Agg, 'Recognition'=df$AffRec.Agg,
                               'Feedback'= df$PerfCheck.Agg)
    colnames(dfIndicators) <- c('Turnover\nIntention', 'Perceived\nEmployability', 'Satisfaction', 'Recognition', 'Feedback')
    dfIndicatorsMelt <- melt(dfIndicators)



## @knitr TurnOverPrepare
    ## Created the dataframe of turnover
    turnOverName <- c('TurnOver.1.frustrated',
                      'TurnOver.2.another_day.INV',
                      'TurnOver.3.satisfied.INV',
                      'TurnOver.4.compensation',
                      'TurnOver.5.consider_leaving',
                      'TurnOver.6.dream')
    dfTurnOver <- df[turnOverName]
    ## Create a list of the associated questions to plot them in the likert graph
    turnOverItems <- c('How often do you feel frustrated when not given the opportunity to achieve your personal work-related goals?',
                       'How often do you look forward to another day at work?',
                       'My current job satisfies my personal needs',
                       'I would accept another job at the same compensation level if I was offered it',
                       'How often do you consider leaving your job?',
                       'How often do dream about getting another job that will better suit your needs?')


    dfTurnOver[,c(1,2,5,6)] <- lapply(dfTurnOver[,c(1,2,5,6)], reorderFunc, typeList='time')
    dfTurnOver[,c(3,4)] <- lapply(dfTurnOver[,c(3,4)], reorderFunc, typeList='agree')

    # Rename the column with the question themselves for plotting reasons
    colnames(dfTurnOver) <- turnOverItems
    kable(turnOverItems, col.names='Turnover Questions')

## @knitr TurnOverStackTime
    # sjp.stackfrq(dfTurnOver[,c(1,2,5,6)], axis.labels=turnOverItems) + theme_bw()
    ## Plot the questions with the time as options
    plotLikert(dfTurnOver[,c(1,2,5,6)])


## @knitr TurnOverStackAgree
    plotLikert(dfTurnOver[,c(3,4)], TRUE)


## @knitr PerceivedEmpPrepare
    perceivedName  <- c('PercEmp.1.equivalent',
                        'PercEmp.2.other_orga',
                        'PercEmp.3.demand',
                        'PercEmp.4.not_hard')
    dfPerc <- df[perceivedName]
    perceivedItems  <- c('It would not be very difficult for me to get an equivalent job in a different organisation',
                         'I can think of a number of organisations that would probably offer me a job',
                         'My experience is in demand on the labour market')

    dfPerc <- as.data.frame(lapply(dfPerc, reorderFunc, typeList='agree'))
    colnames(dfPerc) <- perceivedItems
    kable(perceivedItems, col.names='Perceived Employability Questions')


## @knitr PerceivedEmpStack
    plotLikert(dfPerc, TRUE)

## @knitr SatisfactionPrepare
    satisfactionName  <- c('AffSat.1.enjoyment',
                           'AffSat.2.enthusiastic',
                           'AffSat.3.fairly_well',
                           'AffSat.4.better_ave')
    dfSat <- df[satisfactionName]
    satisfactionItems  <- c('I find real enjoyment in my job',
                            'Most days I am enthusiastic about my job',
                            'I feel fairly well satisfied with my job',
                            "I like my job better than the average person")

    dfSat <- as.data.frame(lapply(dfSat, reorderFunc, typeList='agree'))
    colnames(dfSat) <- satisfactionItems
    kable(satisfactionItems, col.names='Satisfaction Questions')

## @knitr StatisfactionStack
    plotLikert(dfSat, TRUE)

## @knitr RecognitionPrepare
    recognitionName <- c('AffRec.1.confidence',
                         'AffRec.2.thanks',
                         'AffRec.3.recognition',
                         'AffRec.4.compliment',
                         'AffRec.5.encouragement')
    dfReco <- df[recognitionName]
    recognitionItems  <- c("I am satisfied with my supervisor/line manager's confidence in me",
                           "I am satisfied with a word of thanks from my supervisor/line manager",
                           "I am satisfied with the recognition I receive from my supervisor/line manager for doing my job",
                           "I am satisfied with the compliments from my supervisor/line manager concerning my work",
                           "I am satisfied with the encouragement from my supervisor/line manager while doing my job")

    dfReco <- as.data.frame(lapply(dfReco, reorderFunc, typeList='agree'))
    colnames(dfReco) <- recognitionItems
    kable(recognitionItems, col.names='Recognition Questions')

## @knitr RecognitionStack
    plotLikert(dfReco, TRUE)


## @knitr FeedbackPrepare
    feedbackName  <- c('PerfCheck.1.info_result',
                       'PerfCheck.2.opport_check',
                       'PerfCheck.3.access_info',
                       'PerfCheck.4.sufficient_info',
                       'PerfCheck.5.direct_feedback',
                       'PerfCheck.6.supervisor_well',
                       'PerfCheck.7.colleagues')

    dfFeed <- df[feedbackName]
    feedbackItems <- c("Do you receive sufficient information on the results of your work?",
                       "Does your work give you the opportunity to check on how well you are doing your work?",
                       "In your work, do you have access to sufficient data and information?",
                       "Do you receive sufficient information on the purpose of your work?",
                       "Does your work provide you with direct feedback on how well you are doing your work?",
                       "Does your supervisor/line manager inform you about how well you are doing your work?",
                       "Do your colleagues inform you about how well you are doing your work?")

    dfFeed <- as.data.frame(lapply(dfFeed, reorderFunc, typeList='time'))
    colnames(dfFeed)  <- feedbackItems
    kable(feedbackItems, col.names='Feedback Questions')

## @knitr FeedbackStack
    plotLikert(dfFeed)


## @knitr workIndicatorPlot
    ggplot(dfIndicatorsMelt, aes(x=variable, y=value, color=variable))+
            geom_boxplot(show.legend=FALSE, size=2)+
            geom_jitter(alpha=0.25, color='Grey')+
            scale_color_brewer(palette='Paired') +
            # scale_y_continuous(limits=c(0,5), breaks=c(0, 1, 2, 3, 4, 5)) +
            theme_minimal() +
            ylab('Score') +
            xlab('') +
            # theme(plot.title = element_text(size=30, face='bold')) +
            theme(axis.title.x = element_blank()) +
            theme(legend.text=element_text(size=FONT_SIZE)) +
            theme(axis.title.y =element_text(size=FONT_SIZE)) +
            theme(axis.text =element_text(size=FONT_SIZE))


## @knitr careerPrep
    ## Create the df
    dfCareer <- data.frame('Career Plan'=df$ProgRSE.1.Recode, 'Next Position'=df$ProgRSE.2.Recode,
                           'Promotion'=df$ProgRSE.3.Recode, 'Information'=df$ProgRSE.4.Recode,
                           'Opportunities'=df$ProgRSE.5.Recode)
    dfCareerMelt <- melt(dfCareer)
    # summary(dfCareer$Career.Plan)
    # sd(dfCareer$Career.Plan, na.rm = TRUE)
    # summary(dfCareer$Next.Position)
    # sd(dfCareer$Next.Position, na.rm=TRUE)
    # summary(dfCareer$Promotion)
    # sd(dfCareer$Promotion, na.rm=TRUE)
    # summary(dfCareer$Information)
    # sd(dfCareer$Information, na.rm=TRUE)
    # summary(dfCareer$Opportunities)
    # sd(dfCareer$Opportunities, na.rm = T)

## @knitr careerStackPrep
    careerItem <- c("It is likely that I will gain a promotion within my current group",
                    "The process I have to complete to gain a promotion is clear and understandable",
                    "There are many opportunities within my chosen career plan",
                    "My current position is an integral part of my career plan",
                    "It is likely that my next position will be an RSE role")
    dfCareerLikert <- as.data.frame(lapply(dfCareer, reorderFunc, 'agree'))
    colnames(dfCareerLikert) <- careerItem


## @knitr careerStackPlot

    plotLikert(dfCareerLikert)


## @knitr careerPlot
    ggplot(na.omit(dfCareerMelt), aes(x=variable, y=value, color=variable))+
        geom_boxplot(show.legend=FALSE, size=2)+
        geom_jitter(alpha=0.25, color='Grey')+
        scale_color_brewer(palette='Paired') +
        # scale_y_continuous(limits=c(0,5), breaks=c(0, 1, 2, 3, 4, 5)) +
        theme_minimal() +
        ylab('Score') +
        xlab('') +
        # theme(plot.title = element_text(size=30, face='bold')) +
        theme(axis.title.x = element_blank()) +
        theme(legend.text=element_text(size=FONT_SIZE)) +
        theme(axis.title.y =element_text(size=FONT_SIZE)) +
        theme(axis.text =element_text(size=FONT_SIZE))


# Word cloud

## @knitr skillImportantPrep
    ### Most important skills
    skill1_important <- df$Misc.skill.imp.1
    skill2_important <- df$Misc.skill.imp.2
    skill3_important <- df$Misc.skill.imp.3
    skills_important <- c(as.character(skill1_important),
                          as.character(skill2_important),
                          as.character(skill3_important))
    wordToRemove <- c('skills', 'ability', 'skill')
    skills_important <- cleanText(skills_important, wordToRemove = wordToRemove)


## @knitr skillImportantPlot
    wordcloud(skills_important, random.color=FALSE, colors=brewer.pal(12, "Paired"))


## @knitr skillImprovePrep
    skills1_to_improve <- df$Misc.skill.better.1
    skills2_to_improve <- df$Misc.skill.better.2
    skills3_to_improve <- df$Misc.skill.better.3
    skills_improve <- c(as.character(skills1_to_improve),
                        as.character(skills2_to_improve),
                        as.character(skills3_to_improve))
    skills_improve <- cleanText(skills_improve, wordToRemove = wordToRemove)
    wordToRemove <- c('skill', 'skills', 'better', 'improve')

## @knitr skillImprovePlot
    wordcloud(skills_improve, random.color=FALSE, colors=brewer.pal(12, "Paired"))

## @knitr toolPrep
    tool1 <- df$Misc.tool.1
    tool2 <- df$Misc.tool.2
    tool3 <- df$Misc.tool.3
    all_tools <- c(as.character(tool1),
                   as.character(tool2),
                   as.character(tool3))
    all_tools <- cleanText(all_tools, wordToRemove = c('tools', 'system'))

## @knitr toolPlot
    wordcloud(all_tools, random.color=FALSE, colors=brewer.pal(12, "Paired"))

