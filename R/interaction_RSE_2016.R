#### Script to analyse the interactions between different variables


#### library loadings ####
    library('psy')

#### load files ####
    setwd('/home/olivier/Documents/Work/ssi_work/RSE Survey - 2016/analysis/')
    df <- read.csv('./input/397_op_clean.csv',  na.strings=c("NA","NaN", " ", ""))

crossTabFreq <- function(df, var1, var2, propNum=1, summaryTable=FALSE){
    freqTable <- table(df[[var1]], df[[var2]])
    if(summaryTable==TRUE){
        kable(freqTable, digits=2)
    }
    #print(freq_table)
    dfTable  <- cbind(as.data.frame(freqTable),
                      as.data.frame(prop.table(freqTable, propNum))[,3])
    colnames(dfTable) <- c(var1, var2, 'Freq', 'Prop')
    
    #write.csv(dfTable, paste("./results/cross_table_", var1,'_', var2, '.csv'), row.names=FALSE)
    
    #dev.off()
    return(dfTable)
}

facetPlot <- function(df, xVar, yVar, facetVar, removeNA=TRUE, FREQ=TRUE){
    df$Percent  <- df[[yVar]]*100
    if (removeNA==TRUE) {
        df <- na.omit(df)
    }
    p <- ggplot(df, environment=environment())
    p <- p + aes_string(x=xVar, y=yVar, fill=xVar)
    p <- p + geom_bar(stat='identity', position='dodge')
    #p <- p + theme_minimal()
    p <- p + ylab('')
    #p <- p + scale_y_continuous(labels = percent_format())
    p <- p + xlab('')
    p <- p + theme(plot.title = element_text(size=30, face='bold'))
    p <- p + theme(legend.text=element_text(size=15))
    p <- p + theme(legend.title=element_blank())
    #p <- p + scale_fill_brewer()
    if (FREQ==TRUE){
        p <- p + geom_text(aes(label=paste(round(Percent), '%', ' (n=', Freq, ')')), vjust=-0.2, size=4)
    }
    else {
        p <- p + geom_text(aes(label=paste(round(Percent))), vjust=-0.2, size=4)
    }
    p <- p + theme(plot.title = element_text(size=20, face="bold", vjust=2))
    p <- p + facet_wrap(as.formula(paste('~', facetVar)), ncol=2)
    return(p)
}

processCrossTabulation <- function(df, nameVar1, nameVar2, univariate=FALSE, summaryTable=TRUE, propNum=1){
    if(univariate==TRUE){
        sumQ <- printSummary(df, nameVar2)
        #Printing table
        kable(sumQ, digits=2)
        # Printing Plot
        plotSummary(sum_Q, 'Total Respondents', nameVar2)
    }
    table_ <- crossTabFreq(df, nameVar1, nameVar2, propNum=propNum, summaryTable=summaryTable)
    plot <- facetPlot(table_, nameVar2, 'Prop', nameVar1)
    return(plot)
}

chiSquareSummary <- function (table_, var1, var2) {
    chiValue <- chisq.test(table_)
    #fisher_value <- fisher.test(table_)
    if (chiValue$p.value <= .5){
        write.csv(chiValue$residuals, paste("./results/chi_test_res_", var1,'_', var2, '.csv', seq=''), row.names=FALSE)
    }
    return (chi_value)
}


###### Checking the Cronbach Alpha ######
    
###### Check for TurnOver
    turnOverCronbach <- cronbach(cbind(df$TurnOver.1.Recode, df$TurnOver.2.Recode,
                                       df$TurnOver.3.Recode, df$TurnOver.4.Recode,
                                       df$TurnOver.5.Recode, df$TurnOver.6.Recode))

    turnOverCronbach$alpha

    percEmpCronbach <- cronbach(cbind(df$PercEmp.1.Recode, df$PercEmp.2.Recode,
                                      df$PercEmp.3.Recode, df$PercEmp.4.Recode))
    percEmpCronbach$alpha

    perfCheck <- cronbach(cbind(df$PerfCheck.1.Recode, df$PerfCheck.2.Recode, df$PerfCheck.3.Recode, 
                                df$PerfCheck.4.Recode,df$PerfCheck.5.Recode, df$PerfCheck.6.Recode,
                                df$PerfCheck.7.Recode))
    perfCheck$alpha
    affRecCronbach <- cronbach(cbind(df$AffRec.1.Recode, df$AffRec.2.Recode, df$AffRec.3.Recode, 
                                     df$AffRec.4.Recode, df$AffRec.5.Recode))
    affRecCronbach$alpha
    affSatCronbach <- cronbach(cbind(df$AffSat.1.Recode, df$AffSat.2.Recode,
                                     df$AffSat.3.Recode, df$AffSat.4.Recode))
    affSatCronbach$alpha



#### Gender ####    
test <- lm(formula= TurnOver.Agg~Socio.gender, data =df)
boxplot(TurnOver.Agg~Socio.gender, data=df)
summary(test)
anova(test)
confint(test)
