---
title: "RSE Survey 2016"
author: Olivier PHILIPPE
date: 26 June 2016
output:
  html_document:
    keep_md: yes
    number_sections: yes
    toc_depth: 4
  github_document:
    md_extensions: -autolink_bare_uris+hard_line_breaks
  pdf_document:
    toc_depth: 4
---















# Introduction

Research is growing ever more reliant on software, which means that in today's research the software used by researchers is intrinsically linked to the reliability and reproducibility of their results.
With this level of importance place on software it would, of course, be desirable for all researchers to acquire the skills needed to develop reliable and reproducible software, but this is not a practical aim.
Acquiring these skills is time-consuming and difficult, and researchers have many other calls upon their time. 

The increasing need for software engineering skills in research, combined with a lack of training in these skills and the limited time that many researchers have for software development has led to the emergence of a new role in academia: *the Research Software Engineer (RSE)*.

The emergence of RSEs has been driven by necessity.
Researchers needed software development compelled that was more extensive than the time they had available, or required skills that were beyond their means.
This led to the employment of people, often as postdocs, with known software development skills who were then allowed , so they began to employ 
The term *Research Software Engineer* came into existence in 2012 [1] long after people began to rely the role. 

If is desirable, we cannot rely solely on researchers acquiring the skills they need to develop reliable and reproducible research. It is a time-consuming.

Researchers can choose to learn the skills they need to develop reliable and reproducible software, but this is a time-consuming and difficult process that often requires them to acquire a skill set that is completely novel to them.

In a 2014 survey [2], the Software Sustainability Institute found that almost 70\% of researchers from across domains relied on software for the generation of their results, which shows the fundamental importance of software to research. 
The availability of this software and its reliability are completely dependent on the skills of the person or people who develop it. Although some academics will choose to adopt the skills they need to engineer reliable software, many will not have either the time or inclination to do so.
A more scalable and sustainable approach to embedding these skills in academia is to ensure that academics have access to *RSEs*.

The Software Sustainability Institute began a campaign in 2013 to increase the availability of *RSEs* by ensuring they had a viable career path in academia.
To provide a more accurate picture of the community’s development, it was decided to start an annual survey to collect demographics. This paper shares the results of this survey and uses these results to infer conclusions about the community. 
In March 2016, the Software Sustainability Institute  conducted a survey on the Research Software Engineers. 
The survey was composed of 119 questions, asking about demography, practices and attitude toward the job and ended on the 15th of June 2016.
The survey was approved by the Southampton Ethical comittee (id:18478).
This research complies with the [Data Protection act](https://www.gov.uk/data-protection) and the [University of Southampton policy](http://www.calendar.soton.ac.uk/sectionIV/research-data-management.html).



## <a name='sample'></a>Sample and methodology
The survey was designed and stored on the Southampton online survey service: [isurvey](https://www.isurvey.soton.ac.uk/).
To target \emph{RSEs} directly, the survey was sent via email to the UKRSE Association’s mailing list.
Two further reminders were sent to the list to elicit responses from the community. 
People who received these emails helped to disseminate the survey via email, on blogs and using Twitter and other social media.

The survey was devised to investigate *RSEs* in the UK, but dissemination of the survey had resulted in responses from around the world. 
In total, 592 responses were received, but 299 of these were complete (*i.e. where all mandatory questions had been answered*).
This first analysis was focuses on UK, and hence on the 273 complete responses from UK-based participants.

### Sections and questions

The survey was divided in 6 main sections. 
The first section aimed to collect some basic information about the country of origin and to know in which extend the participant could be considered as a RSE.
The second section questioned the participants about their past job and their current job contract.
The third section was focus on the academic recognition the participants get throught their role and the development's practices they implement.
The fourth and fifth section aimed to collect answers on several work indicators to get a complete picture of their job satisfaction. 
And finally, the sixth section asked various questions about the tools they use as long as some more socio-demographic questions. 
Here a plot to show where the participant drops the survey. We can see that as soon as they completed the first section, they are almost finishing it (68 dropped between the section 2 and the section 6).

![plot of chunk sampleSetPlot](figure/sampleSetPlot-1.png)



## A word on Research Software Engineer: how to identify an RSE?

The term *RSE* was coined in 2012 in a paper [1] that described the situation of people in academia who write software used by researchers. 
At the time, little was known about software use in research, but even less was known about the people who develop, maintain and extend that code. The term was chosen because it fuses the two skills that are necessary to the role: an understanding of research, and an understanding of software engineering. 

It is noted that the term *RSEs* is intended to be interpreted broadly, it does not imply that the only people who can inhabit the role derive from a software engineering background. Indeed, most *RSEs* come from a background other than software engineering.

However, as there were no control over the dissemination of the survey by the participants, a way to identify them was to include questions to identify them. 

The [UKRSE Association](http://www.rse.ac.uk/) were the first to encounter this problem and as a consequence had developed a number of questions to help potential RSEs align themselves with the role. These questions are as follows:



|RSE Questions                                                        |
|:--------------------------------------------------------------------|
|Are you employed primarily to develop software for research?         |
|Do you spend more time developing software than conducting research? |
|Are you employed as a postdoctoral researcher?                       |
|Are you the person who “does computers” in your research group?      |

The above questions were devised to appeal to people who worked in a research environment but wrote software rather than papers, and were based on the experiences of a group of people who had worked in the RSE role. They were designed to identify aspects that, anecdotally at least, appeared to unite RSEs from all backgrounds. For example, many appeared to have been recruited into postdoctoral positions, and many were referred to as the person *who does computers* by their less technologically adept colleagues. 
The above questions are broad, and even colloquial at times, to appeal to the intended audience and present the RSE community as inclusive. RSEs were said to have self-identified once they had joined the UKRSE Association.

The answers are reported below. It is important to note the answers are independents, therefore the number of participants that have answered at least one of the RSE questions is 262 over the already 273 selected participant (see [Sample and methodology section](#sample)).



```
## Error in likert(df): object 'dfRSE' not found
```

# Discipline

To gain an understanding of the academic background of the participants, we asked for the academic discipline of their highest degree. We used the Joint Academic Coding System (JACS)[3] to provide an ordered and well understood list of academic discipline categories.

The majority of participants came from a background in the physical sciences (39.4%) and the computer sciences (23.6%). In fact, these two discipline areas combined make up more than 60% of all respondents.





|   |Field of Education                         | Total Respondents| Percent|
|:--|:------------------------------------------|-----------------:|-------:|
|12 |Technologies                               |                 0|    0.00|
|5  |Historical and Philosophical studies       |                 2|    0.74|
|6  |Linguistics  Classics and related subjects |                 2|    0.74|
|11 |Subjects allied to Medicine                |                 3|    1.12|
|2  |Combined studies                           |                 7|    2.60|
|8  |Other                                      |                 7|    2.60|
|10 |Social studies                             |                 7|    2.60|
|1  |Biological Sciences                        |                19|    7.06|
|4  |Engineering                                |                23|    8.55|
|7  |Mathematical and Computer Sciences         |                28|   10.41|
|3  |Computer Sciences                          |                61|   22.68|
|9  |Physical Sciences                          |               110|   40.89|

![plot of chunk disciplinePlot](figure/disciplinePlot-1.png)

# Education

It had been postulated that RSEs understood both software engineering and research. To investigate their understanding of the research domain, we asked participants for the highest degree they had obtained.

A significant majority of participants (69.5%) had been awarded a PhD, which indicates that a significant majority of RSEs have experience of working in research. 





|   |level of Education                   | Total Respondents| Percent|
|:--|:------------------------------------|-----------------:|-------:|
|5  |Other                                |                 0|    0.00|
|4  |None                                 |                 1|    0.37|
|2  |Further Education (NVQ  A-Level ...) |                 3|    1.10|
|6  |Undergraduate degree                 |                33|   12.09|
|3  |Masters degree                       |                53|   19.41|
|1  |Doctorate                            |               183|   67.03|




|   |level of Education   | Total Respondents| Percent|
|:--|:--------------------|-----------------:|-------:|
|3  |Undergraduate/Others |                37|   13.55|
|2  |Master Degree        |                53|   19.41|
|1  |Doctorate            |               183|   67.03|

![plot of chunk educationCleanPlot](figure/educationCleanPlot-1.png)

# Gender

Based on experience, it appeared that the RSE community was predominantly male.
We asked participants to provide their gender. 263 participants (97% of all participants) answered this non-mandatory question and we found that the population is indeed predominantly male. 





|   |Gender               | Total Respondents| Percent|
|:--|:--------------------|-----------------:|-------:|
|3  |Other                |                 1|    0.37|
|4  |Prefer not to answer |                 6|    2.23|
|1  |Female               |                30|   11.15|
|2  |Male                 |               232|   86.25|

![plot of chunk genderPlot](figure/genderPlot-1.png)



![plot of chunk genderAllPlot](figure/genderAllPlot-1.png)


# Age

We asked the participants to report the age




|Age            | Total Respondents| Percent|
|:--------------|-----------------:|-------:|
|18 to 24 years |                 4|    1.48|
|25 to 34 years |                97|   35.93|
|35 to 44 years |               107|   39.63|
|45 to 54 years |                51|   18.89|
|55 to 64 years |                11|    4.07|

![plot of chunk agePlot](figure/agePlot-1.png)


# Contract

The type of contract can be a good measure of the precarity of a specific position. We asked the participants which type of contract they have. 


The results show that more than the half of the participants are under a *permanent* position.


|   |Type of contract                   | Total Respondents| Percent|
|:--|:----------------------------------|-----------------:|-------:|
|1  |Agency staff                       |                 1|    0.37|
|3  |Freelancer consultant  contractors |                 3|    1.10|
|2  |Fixed term                         |               126|   46.32|
|4  |Permanent                          |               142|   52.21|

![plot of chunk contractPlot](figure/contractPlot-1.png)

# First job




|First Job | Total Respondents| Percent|
|:---------|-----------------:|-------:|
|No        |               225|   82.72|
|Yes       |                47|   17.28|

![plot of chunk firstJobPlot](figure/firstJobPlot-1.png)

# Salary





![plot of chunk salaryPlot](figure/salaryPlot-1.png)



![plot of chunk salaryAllPlot](figure/salaryAllPlot-1.png)

# Good practice indicators

## Bus factor




|Bus Factor | Total Respondents| Percent|
|:----------|-----------------:|-------:|
|1          |               124|   45.76|
|2          |                82|   30.26|
|3          |                32|   11.81|
|4          |                12|    4.43|
|5          |                21|    7.75|

![plot of chunk busFactorPlot](figure/busFactorPlot-1.png)

## Technical handover




|   |Technical hand over | Total Respondents| Percent|
|:--|:-------------------|-----------------:|-------:|
|2  |Yes                 |                59|   21.61|
|1  |No                  |               214|   78.39|

![plot of chunk handOverPlot](figure/handOverPlot-1.png)

# Academic recognition





|Contribution to paper | Total Respondents| Percent|
|:---------------------|-----------------:|-------:|
|No                    |                34|   12.45|
|Yes                   |               239|   87.55|

![plot of chunk contribYNPlot](figure/contribYNPlot-1.png)



![plot of chunk contribAllPlot](figure/contribAllPlot-1.png)

# Work Indicators

## Introduction

One goal of this survey was to measure the 

How to measure the quality of a job has been debated in psychology for a long time [4].


Several models exist to understand the link between different factors of job satisfaction and turnover intention [5]–[9].
 Turnover intention is an important measure that is highly associated with the risk of employees leaving the organisation [7].
Job satisfaction is important in retaining RSEs. Perceived employability provides information on how workers values their own skills in regard of the market.
To measure the different attitudes toward the RSE role, we used scales that have been created in [5], [6], [8], [9].
These are Likert scale [10], which are 5 point ordinal scales graduated from Strongly disagree to Strongly agree.
Each scale is composed of several so called items (i.e. questions) that each measure one attitude.




### Turnover intention




|Turnover Questions                                                                                           |
|:------------------------------------------------------------------------------------------------------------|
|How often do you feel frustrated when not given the opportunity to achieve your personal work-related goals? |
|How often do you look forward to another day at work?                                                        |
|My current job satisfies my personal needs                                                                   |
|I would accept another job at the same compensation level if I was offered it                                |
|How often do you consider leaving your job?                                                                  |
|How often do dream about getting another job that will better suit your needs?                               |

![plot of chunk TurnOverStackTime](figure/TurnOverStackTime-1.png)

![plot of chunk TurnOverStackAgree](figure/TurnOverStackAgree-1.png)

### Perceived Employability




|Perceived Employability Questions                                                          |
|:------------------------------------------------------------------------------------------|
|It would not be very difficult for me to get an equivalent job in a different organisation |
|I can think of a number of organisations that would probably offer me a job                |
|My experience is in demand on the labour market                                            |
|Given my qualifications and experience, getting a new job would not be very hard at all    |

![plot of chunk PerceivedEmpStack](figure/PerceivedEmpStack-1.png)

### Satisfaction

One problem of studies that claim to measure the job satisfaction is, despises describing the job satisfaction as an *affective construct*, measure it on cognitives aspects [8]. 
This is why Thompson et al. created a *Brief Index of Affective Job Satisfaction (BIAJS)* [8]. 
The idea is to measure the affective aspect of the construct. They constructed a scale to measure that aspect and tested in on a sample of 506506 employees and a rate of 18.27% in Australia and Hong-Kong.
After
> Our exploratory factor analyses found the BIAJS to be unidimensional, with high and reasonably uniform factor loadings accounting for around two thirds of variance, for both the whole sample and for subsamples. Corrected item-total correlations ranging across samples from .54 to .74, together with Cronbach’s alphas between .81 and .83, plus a range of confirmatory factor analysis fit indices suggesting acceptable fit for a single factor structure, all supported the internal consistency reliabil- ity of the BIAJS.





|Satisfaction Questions                       |
|:--------------------------------------------|
|I find real enjoyment in my job              |
|Most days I am enthusiastic about my job     |
|I feel fairly well satisfied with my job     |
|I like my job better than the average person |

![plot of chunk StatisfactionStack](figure/StatisfactionStack-1.png)


### Recognition




|Recognition Questions                                                                          |
|:----------------------------------------------------------------------------------------------|
|I am satisfied with my supervisor/line manager's confidence in me                              |
|I am satisfied with a word of thanks from my supervisor/line manager                           |
|I am satisfied with the recognition I receive from my supervisor/line manager for doing my job |
|I am satisfied with the compliments from my supervisor/line manager concerning my work         |
|I am satisfied with the encouragement from my supervisor/line manager while doing my job       |

![plot of chunk RecognitionStack](figure/RecognitionStack-1.png)


### Feedback




|Feedback Questions                                                                    |
|:-------------------------------------------------------------------------------------|
|Do you receive sufficient information on the results of your work?                    |
|Does your work give you the opportunity to check on how well you are doing your work? |
|In your work, do you have access to sufficient data and information?                  |
|Do you receive sufficient information on the purpose of your work?                    |
|Does your work provide you with direct feedback on how well you are doing your work?  |
|Does your supervisor/line manager inform you about how well you are doing your work?  |
|Do your colleagues inform you about how well you are doing your work?                 |

![plot of chunk FeedbackStack](figure/FeedbackStack-1.png)

## Summary



![plot of chunk workIndicatorPlot](figure/workIndicatorPlot-1.png)

# Career Plan

## Introduction





![plot of chunk careerStackPlot](figure/careerStackPlot-1.png)

![plot of chunk careerPlot](figure/careerPlot-1.png)

# Wordclouds

We asked three separated questions to capture the variety of skills and tools that are important for an RSE. Each questions offered the opportunity to fills three freetext fields. These questions help us to get a gist of what is important for RSE on a more broad and less defined way. 
The following wordcloud give an idea of that aspect, without pretending to be rigorous. 


## Most important skills for an RSE



![plot of chunk skillImportantPlot](figure/skillImportantPlot-1.png)

## Most important skills to improve for an RSE



![plot of chunk skillImprovePlot](figure/skillImprovePlot-1.png)

## Most important tool for an RSE



![plot of chunk toolPlot](figure/toolPlot-1.png)


# References

  * [1] R. Baxter, N. Chue Hong, D. Gorissen, J. Hetherington, and I. Todorov, “The Research Software Engineer.” [Online]. Available: [](http://digital-research-2012.oerc.ox.ac.uk/papers/the-research-software-engineer)
  * [2] S. Hettrick, “It’s impossible to conduct research without software, say 7 out of 10 UK researchers.” [Online]. Available: https://www.software.ac.uk/blog/2016-07-26-its-impossible- conduct-research-without-software-say-7-out-10-uk-researchers
  * [3] Joint Academic Coding System (JACS) Version 3.0. [Online]. Available: https://www.hesa.ac.uk/jacs3
  * [4] B. Aziri, “Job satisfaction: A literature review,” vol. 3, no. 4, pp. 77–86. 
  * [5] A. B. Bakker and E. Demerouti, “The job demands-resources model: State of the art,” vol. 22, no. 3, pp. 309–328, 02996.
  * [6] G. H. L. Cheng and D. K. S. Chan, “Who Suffers More from Job Insecurity? A Meta-Analytic Review.” vol. 57, no. 2, p. 272.
  * [7] N. De Cuyper, S. Mauno, U. Kinnunen, and A. Mkikangas, “The role of job resources in the relation between perceived employability and turnover intention: A prospective two-sample study,” vol. 78, no. 2, pp. 253–263.
  * [8] E. R. Thompson and F. T. Phua, “A brief index of affective job satisfaction,” vol. 37, no. 3, pp. 275–307.
  * [9] L. Greenhalgh and Z. Rosenblatt, “Job insecurity: Toward conceptual clarity,” pp. 438–448.
  * [10] R. Likert, “A technique for the measurement of attitudes.” vol. 22, no. 140, p. 55.
  * [11] J. Carifio and R. Perla, “Resolving the 50-year debate around using and misusing Likert scales,” vol. 42, no. 12, pp. 1150–1152.
  * [12] G. Norman, “Likert scales, levels of measurement and the laws of statistics,” vol. 15, no. 5, pp. 625–632.
  * [13] G. M. Sullivan and A. R. Artino, “Analyzing and Interpreting Data From Likert-Type Scales,” vol. 5, no. 4, pp. 541–542.
  * [14] L. J. Cronbach, “Coefficient alpha and the internal structure of tests,” vol. 16, no. 3, pp. 297–334.
  * [15] D. L. Streiner, “Starting at the Beginning: An Introduction to Coefficient Alpha and Internal Consistency,” vol. 80, no. 1, pp. 99–103.
  * [16] M. Tavakol and R. Dennick, “Making sense of Cronbach’s alpha,” vol. 2, pp. 53–55
