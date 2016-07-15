### Script to clean the dataset. Drop headers that considered as artefact from isurvey. Drop useless information
### Remove identifiable information and when people say yes and give email address - split to another files and random the order
### to avoid identification.
### Clean some answers to transform them in numeric value (in case of likert scale) and in factor when needed.
### Aggregate the different items to a score for the scales that need it

### IMPORTANT: Some cleaning are using indexing and levels. That means if the dataset is different, the ceaning will
### output wrong results. Go to the section LAUNCH ME to check if it is launced on the same dataset
### This check just see if the number of records are the same. Not the ideal check, so in case of doubt
### Check the line of code having the index reference and carefully manually check them


#### Setting up the directory #####
    ## Work only on linux -- if it is not working needs to insert manually
    this.dir = system("pwd", intern = T) 
    setwd(this.dir)
    setwd('~/git/ssi/RSE-Survey-2016/')
    


#### Source the functions.R file
    source('./R/functions.R')

#### library loadings ####
    library('plyr')
    library('tm') # txt cleaning
    
    
#### Load data ####
    df <- read.csv('./data/SurveyID18932.csv')
    # Write email address on a csv
    write.csv(unique(df[107]), './data/emails_list.csv')
    names(df)
    ## Create a manual list of headers to drop. It is based on the order given when download the dataset from the isurvey and without
    ### modifying the option of downloading. Need to be careful in case of the order change.
    headers_to_drop = c(1, 2, 4, 6, 7, 8, 9, 10, 11, 17, 22, 30, 38, 44, 50, 51, 57, 68, 69, 80, 87, 88, 93, 97, 105, 106, 107)
    nameToDrop <- names(df)[headers_to_drop]
    # Read these values before subsetting
    nameToDrop

    ## Subsetting the df to drop the unwanted columns
        df <- df[, -which(names(df) %in% nameToDrop)]

  ## Cleaning work directory
  #  rm(df, headers_to_drop, nameToDrop)

#### LAUNCH ME!!! - IMPORTANT ####
    # Check the number of record and see if it is the same as the one entered here. If not, go and check
    # Every code lines in this file to see where the change can have an impact.
    LAST_CHECK_NUMBER <- 592
    CURRENT_CSV_NUMBER <- nrow(df)
    try(if(LAST_CHECK_NUMBER != CURRENT_CSV_NUMBER) stop("The file is different than the last one checked"))


#### Rename columns name to make the rest easier ####

    ## Section 1: A few questions about yourself [1:11] length should be equal to [-1] - [1] + 1: 11
    section1_names <- c('Survey.date', 'Survey.time', 'Socio.country',
                        'Edu.highest_qualification', 'Edu.academic_subject','Edu.academic_other',
                        'Edu.professional_qualification', 'RSE.dev_software', 'RSE.dev_time',
                        'RSE.post_doc', 'RSE.does_computer')

    ## Section 2: Employment history [12:25] -> length should be equal to [-1] - [1] + 1: 14
    section2_names <- c('Job.uniYN', 'Job.uni_name', 'Job.uni_name2',
                        'Job.orga_name', 'Job.title', 'Job.date',
                        'Job.contract', 'Job.duration', 'Job.fullTimeYN',
                        'Job.firstYN', 'Job.previous.private', 'Job.previous.leave',
                        'Job.previous.title', 'Job.previous.contract')

    ## Section 3: Current position [26:35] length should be equal to [-1] - [1] + 1: 10
    section3_names <- c('Job.diffYN', 'Job.diff_info', 'Work.expertYN',
                        'Work.time_allocated', 'Contrib.YN', 'Contrib.acknowledgedYN',
                        'Contrib.co-authorYN', 'Contrib.leadYN', 'Stability.bus_factor',
                        'Stability.hand_over')

    ## Section 4: Your perception about your current position [36:50] length should be equal to [-1] - [1] + 1: 15
    section4_names <- c('PerfCheck.1.info_result', 'PerfCheck.2.opport_check', 'PerfCheck.3.access_info',
                        'TurnOver.1.frustrated', 'TurnOver.2.another_day.INV', 'AffRec.1.confidence',
                        'AffRec.2.thanks', 'ProgRSE.1.career_plan', 'ProgRSE.2.next_position',
                        'PercEmp.1.equivalent', 'PercEmp.2.other_orga', 'PercEmp.3.demand',
                        'AffSat.1.enjoyment','AffSat.2.enthusiastic', 'AffSat.3.fairly_well')

    ## Section 5: Your peceptions about your current position 2 [51:66] length should be equal to [-1] - [1] + 1: 16
    section5_names <- c('AffRec.3.recognition', 'AffRec.4.compliment', 'AffRec.5.encouragement',
                        'ProgRSE.3.promotion', 'ProgRSE.4.clear', 'ProgRSE.5.many_opportunities',
                        'PercEmp.4.not_hard', 'AffSat.4.better_ave', 'TurnOver.3.satisfied.INV',
                        'TurnOver.4.compensation', 'PerfCheck.4.sufficient_info', 'PerfCheck.5.direct_feedback',
                        'PerfCheck.6.supervisor_well', 'PerfCheck.7.colleagues', 'TurnOver.5.consider_leaving',
                        'TurnOver.6.dream')

    ## Section 6: Some final questions [67:80] length should be equal to [-1] - [1] + 1: 14
    section6_names <- c('Misc.skill.imp.1', 'Misc.skill.imp.2', 'Misc.skill.imp.3',
                        'Misc.memberYN', 'Misc.skill.better.1', 'Misc.skill.better.2',
                        'Misc.skill.better.3', 'Misc.tool.1', 'Misc.tool.2',
                        'Misc.tool.3', 'Socio.gender', 'Socio.age',
                        'Socio.salary', 'Misc.OS')

    new_section_names <- c(as.character(section1_names), as.character(section2_names), as.character(section3_names),
                      as.character(section4_names), as.character(section5_names), as.character(section6_names))

    ## Create a corresponding original title -- new titles and record a csv with them to keep a trace
    dfHeaders <- cbind(names(df), new_section_names)
    write.csv(dfHeaders, file='./data/headers.csv')
    # Renames the columns
    names(df) <- new_section_names
    ## Clean the working directory
    rm(dfHeaders, new_section_names, section6_names, section5_names,
       section4_names, section3_names, section2_names, section1_names)



#### Functions ####
#### Cleaning and Transformation ####

  # Survey.date
    # The dates have a weird formating
    # Remove the 'th, 'st, 'nd' associated with the date string as it is difficult to parser
    df$Survey.date.CLEAN <- sub('th|st|nd', '', df$Survey.date)
    df$Survey.date.CLEAN <- as.Date(df$Survey.date.CLEAN, format="%e %b %Y %l:%M %p")

  # Survey.time
    # FIXME Not working because mix up the time if it is hour or minutes. Need better fix but not urgent
    # because this data is not used.
    ## Duration of the survey  Dirty regex! need to replace :
    #      df$Survey.time.CLEAN <- df$Survey.time
    #      df$Survey.time.CLEAN <- gsub(',', ':', df$Survey.time.CLEAN)
    #      df$Survey.time.CLEAN <- gsub('[A-Za-z]+', '', df$Survey.time.CLEAN)
    #      df$Survey.time.CLEAN <- gsub('\\s', '', df$Survey.time.CLEAN)

  # Socio.country
    ## Nothing to do

  # Edu.highest_qualification
    df$Edu.highest_qualification[df$Edu.highest_qualification == ""] <- NA
    levels(df$Edu.highest_qualification)

  # Edu.academic_other
    # Load the file containing the list of subjects inputs in the survey to have the entire list
    # and make manual comparison to check the one that can be recode into an already coded
    dfEdu <- read.csv('./data/information/list_educations.csv', header = FALSE)
    edu_list <- dfEdu$V1
    # Replace these values with appropriate one when a correspondance can be found
    df$Edu.academic.CLEAN <- df$Edu.academic_subject
    ## Replace directly the others when it is a value that can be translated into the right variable
    df$Edu.academic.CLEAN[df$Edu.academic_other == "Software Engineering"] <- "Computer Sciences"
    df$Edu.academic.CLEAN[df$Edu.academic_other == "Computer Science & Physics"] <- "Computer Sciences"
    df$Edu.academic.CLEAN[df$Edu.academic_other == "Geography"] <- "Social studies"
    df$Edu.academic.CLEAN[df$Edu.academic_other == "Economics"] <- "Social studies"
    
    df$Edu.academic.CLEAN[df$Edu.academic_other == "Psychology"] <- "Subjects allied to Medicine"
    levels(df$Edu.academic_other)
    levels(df$Edu.academic.CLEAN)[levels(df$Edu.academic.CLEAN) ==''] <- NA
    levels(df$Edu.academic.CLEAN)
  # Edu.profesional_qualification
    ## Nothing done yet but this is a messy FreeText

  # RSE metrics
    ## Refactor to levels
    ### CAREFUL -- WORKS FOR 338 obs
    levels(df$RSE.dev_time)
    levels(df$RSE.dev_time) <- c(0, 0, 0, 1)

    levels(df$RSE.post_doc)
    levels(df$RSE.post_doc) <- c(0, 0, 0, 1)
    levels(df$RSE.dev_software)
    levels(df$RSE.dev_software) <- c(0, 0, 0, 1)
    levels(df$RSE.does_computer)
    levels(df$RSE.does_computer) <- c(0, 0, 0, 1)
    df$RSE.score <- (as.numeric(as.character(df$RSE.dev_time)) + 
                     as.numeric(as.character(df$RSE.post_doc)) + 
                     as.numeric(as.character(df$RSE.dev_software)) + 
                     as.numeric(as.character(df$RSE.does_computer)))
  # Job.uni_name
    # Rename the associated UCL and some others uk Universities
    # CAREFUL -- With new data, need to check the order of the c() or going to rename wrong levels
    levels(df$Job.uni_name2)
    # CAREFUL -- This index values works for the dataset of 592 obs
    # 
    # Replace all the outside uk uni by the values "Outside UK"
    levels(df$Job.uni_name2)[c(3, 4, 5, 6, 7, 10, 11, 12, 14, 21, 22, 23, 24, 25, 27, 28, 29, 31)] <- 'Outside UK'
    # Replace the Not saying by NA
    levels(df$Job.uni_name2)[c(1, 6, 7, 8)] <- NA
    levels(df$Job.uni_name2)[c(3, 4)] <- "King's College, University of London"
    levels(df$Job.uni_name2)[c(5, 6, 7, 9)] <- "University College of London"
    df$Job.uni_name2 <- as.factor(df$Job.uni_name2)
    
    # Create a new clean column to merge the regular ones, the new regular ones the others and the non uni
    df$Job.uni.CLEAN <- df$Job.uni_name
    ## Create field 'Not University' when No is answered to Job.uniYN
    # Need to add the level on Job.uni.Clean before to add another field (Needed otherwise second line doesn't work)
    levels(df$Job.uni.CLEAN) <- c(levels(df$Job.uni.CLEAN), "Not University")
    df$Job.uni.CLEAN[df$Job.uniYN == 'No'] <- 'Not University'

    ## Put the values from the others into the Job.uni.Clean
    ### Create the vector with the values
    clean_uni <- c("King's College, University of London", "University College of London",
                   "Birkbeck, University of London", "University of Reading", 'Outside UK')
    ### Add these to the factor levels
    
    levels(df$Job.uni.CLEAN) <- c(levels(df$Job.uni.CLEAN), clean_uni)
    df$Job.uni.CLEAN[df$Job.uni_name2 %in% clean_uni] <- df$Job.uni_name2[df$Job.uni_name2 %in% clean_uni]
 
  # Create Russells Group University
    
    
  # Job.orga.name
    # Nothing done yet but messy FREETEXT
  
  # Job.title
    # Nothing done yet but messy FREETEXT

  # Job starting date
    ### Clean Job starting date
    levels(df$Job.date)
    levels(df$Job.date)[levels(df$Job.date) == "2006 (Seconded in 2014)"] <- "2006"
    levels(df$Job.date)[levels(df$Job.date) == "3"] <- "2013"
    levels(df$Job.date)[levels(df$Job.date) == ""] <- NA

  # Job duration
    ### Clean Job duration
    levels(df$Job.duration)
    # Probably to drop, not a clear question and have low answers rate.

  # Job.FulltimeYN
    # Nothing to do

  # Job.FirstYN
    # Nothing to do

  # Job.previous.private
    # Nothing to do

  # Job.previous.leave
    # Nothing to do

  # Job.previous.title
    # Nothing done yet but messy FreeText

  # Job.previous.contract
    # Nothing to do

  # Job.diffYN'
    # Nothing to do

  # Job.diff_info'
    # Nothing done yet but messy FreeText

  # 'Work.expertYN',
    # Nothing to do

  # Work.time_allocated

    ## Clean
    ### TODO Give a list of number from 0 - 100 with 10% steps instead of free space
    ## Replace everything with space except number
    df$Work.time_allocated.CLEAN <- gsub('[^0-9]', ' ', df$Work.time_allocated, perl=TRUE)
    # returns string w/o leading or trailing whitespace
    df$Work.time_allocated.CLEAN <- gsub("^\\s+|\\s+$", "", df$Work.time_allocated.CLEAN)
    # Create a function that is going to remove everything exceeding 100 and
    # NA when it is something different
    df$Work.time_allocated.CLEAN <- mapply(removeValues, df$Work.time_allocated.CLEAN)

  # Contrib.YN
    # Nothing to do

  # Contrib.acknowledgedYN
    # Nothing to do

  # Contrib.co-authorYN
    # Nothing to do

  # Contrib.leadYN
    # Nothing to do

  # Stability.bus_factor
    # Nothing to do

  # Stability.hand_over
    # Nothing to do

  # Performance check
    df$PerfCheck.1.Recode <- mapply(df$PerfCheck.1.info_result, FUN=recodeLikert)
    df$PerfCheck.2.Recode <- mapply(df$PerfCheck.2.opport_check, FUN=recodeLikert)
    df$PerfCheck.3.Recode <- mapply(df$PerfCheck.3.access_info, FUN=recodeLikert)
    df$PerfCheck.4.Recode <- mapply(df$PerfCheck.4.sufficient_info, FUN=recodeLikert)
    df$PerfCheck.5.Recode <- mapply(df$PerfCheck.5.direct_feedback, FUN=recodeLikert)
    df$PerfCheck.6.Recode <- mapply(df$PerfCheck.6.supervisor_well, FUN=recodeLikert)
    df$PerfCheck.7.Recode <- mapply(df$PerfCheck.7.colleagues, FUN=recodeLikert)
    # Aggregate the scores
     df$PerfCheck.Agg <- (df$PerfCheck.1.Recode + df$PerfCheck.2.Recode + df$PerfCheck.3.Recode +
                             df$PerfCheck.4.Recode + df$PerfCheck.5.Recode + df$PerfCheck.6.Recode +
                             df$PerfCheck.7.Recode) / 7

  # Turn Over
    df$TurnOver.1.Recode <- mapply(df$TurnOver.1.frustrated, FUN=recodeLikert)
    df$TurnOver.2.Recode <- mapply(df$TurnOver.2.another_day.INV, FUN=recodeLikert, inverting=TRUE)
    df$TurnOver.3.Recode <- mapply(df$TurnOver.3.satisfied.INV, FUN=recodeLikert, inverting=TRUE)
    df$TurnOver.4.Recode <- mapply(df$TurnOver.4.compensation, FUN=recodeLikert)
    df$TurnOver.5.Recode <- mapply(df$TurnOver.5.consider_leaving, FUN=recodeLikert)
    df$TurnOver.6.Recode  <- mapply(df$TurnOver.6.dream, FUN=recodeLikert)
    # Aggregate the scores
     df$TurnOver.Agg <- (df$TurnOver.1.Recode + df$TurnOver.2.Recode +  df$TurnOver.3.Recode +
                         df$TurnOver.4.Recode + df$TurnOver.5.Recode + df$TurnOver.6.Recode) /6

  # Affective satisfaction
    df$AffSat.1.Recode <- mapply(df$AffSat.1.enjoyment, FUN=recodeLikert)
    df$AffSat.2.Recode <- mapply(df$AffSat.2.enthusiastic, FUN=recodeLikert)
    df$AffSat.3.Recode <- mapply(df$AffSat.3.fairly_well, FUN=recodeLikert)
    df$AffSat.4.Recode <- mapply(df$AffSat.4.better_ave, FUN=recodeLikert)
    # Aggregate the score
      df$AffSat.Agg <- (df$AffSat.1.Recode + df$AffSat.2.Recode +
                        df$AffSat.3.Recode + df$AffSat.4.Recode)/4

  # Affective Recognition
    df$AffRec.1.Recode <- mapply(df$AffRec.1.confidence, FUN=recodeLikert)
    df$AffRec.2.Recode <- mapply(df$AffRec.2.thanks, FUN=recodeLikert)
    df$AffRec.3.Recode <- mapply(df$AffRec.3.recognition, FUN=recodeLikert)
    df$AffRec.4.Recode <- mapply(df$AffRec.4.compliment, FUN=recodeLikert)
    df$AffRec.5.Recode <- mapply(df$AffRec.5.encouragement, FUN=recodeLikert)
    # Aggregate the score
      df$AffRec.Agg <- (df$AffRec.1.Recode + df$AffRec.2.Recode + df$AffRec.3.Recode +
                        df$AffRec.4.Recode + df$AffRec.5.Recode)/5

  # Perceived Employability
    df$PercEmp.1.Recode <- mapply(df$PercEmp.1.equivalent, FUN=recodeLikert)
    df$PercEmp.2.Recode <- mapply(df$PercEmp.2.other_orga, FUN=recodeLikert)
    df$PercEmp.3.Recode <- mapply(df$PercEmp.3.demand, FUN=recodeLikert)
    df$PercEmp.4.Recode <- mapply(df$PercEmp.4.not_hard, FUN=recodeLikert)
    # Aggregate the score
      df$PercEmp.Agg <- (df$PercEmp.1.Recode + df$PercEmp.2.Recode +
                         df$PercEmp.3.Recode + df$PercEmp.4.Recode )/4
      
  # Progression plan
    df$ProgRSE.1.Recode <- mapply(df$ProgRSE.1.career_plan, FUN=recodeLikert)
    df$ProgRSE.2.Recode <- mapply(df$ProgRSE.2.next_position, FUN=recodeLikert)
    df$ProgRSE.3.Recode <- mapply(df$ProgRSE.3.promotion, FUN=recodeLikert)
    df$ProgRSE.4.Recode <- mapply(df$ProgRSE.4.clear, FUN=recodeLikert)
    df$ProgRSE.5.Recode <- mapply(df$ProgRSE.5.many_opportunities, FUN=recodeLikert)
  # Misc.imp_skills
    # Cleaning need to be done outside the data frame and it is done during analysis

  # Misc.memberYN
    # Nothing to do

  # Misc.skill.better
    # Cleaning need to be done outside the data frame and it is done during analysis

  # Cleaning tools
    # Cleaning need to be done outside the data frame and it is done during analysis

  # Socio.gender
    summary(df$Socio.gender)
    levels(df$Socio.gender)[levels(df$Socio.gender) == ""] <- NA
    levels(df$Socio.gender)[levels(df$Socio.gender) == "Prefer not to answer"] <- NA
    levels(df$Socio.gender)[levels(df$Socio.gender) == 'Other'] <- NA
    # Nothing to do

  # Socio age
    # Nothing to do

  # Socio.salary
    # Cleaning error in encoding
    length(levels(df$Socio.salary))
    levels(df$Socio.salary) <- c(NA, 
                                 'Less than £25.000', 
                                 '£100.000 or more',
                                  '£25.000 to £29.000', 
                                 '£30.000 to £34.000',
                                '£35.000 to £39.000', 
                                '40.000 to 44.999', 
                                '45.000 to 49.999',
                                '50.000 to 59.999', 
                                '60.000 to 69.999', 
                                '70.000 to 99.000', 
                                NA)

  # Misc.OS
    # Nothing to do

#### Outputs ####

  # Write the entire dataset into a csv
    fullName <- paste('./data/', CURRENT_CSV_NUMBER, '_full_clean.csv', sep='')
    write.csv(df, file = fullName)
  # Write only useful and cleaned field
    # Create an empty dataset with the same amount ofrow for saving only the useful cleaned column
    dfOp <- data.frame(matrix(nrow = nrow(df), ncol = 0))
    # Select only the clean columns and the one to use in further
    # dfOp <- subset(df, select = c(3, 4, 7:12, 15:18, 20:28, 30:35, 67:length(df)))
    # opName <- paste('./data/', CURRENT_CSV_NUMBER, '_op_clean.csv', sep='')
    # write.csv(dfOp, file=opName)