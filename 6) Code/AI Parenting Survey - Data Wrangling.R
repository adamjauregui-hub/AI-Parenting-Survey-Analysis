library(readr)
library(lubridate) #parse_date_time
library(dplyr)
library(tidyr)
library(summarytools)
library(stringr)
library(likert)
library(utils)
library(psych)
library(sjlabelled)
library(expss)
library(formattable)
library(tableone)
library(ggmap)
library(sf)
library(mapview)
library(rworldmap)
library(maptools)
library(robustHD)
library(usmap)
library(sp)
library(maps)
library(ggrepel)
library(usmap)
library(ggExtra)
library(ggpubr)
library(sjstats)
header<-scan(file = "C:/Users/adamj89/Box/Data Analytics/Parenting Project/Original Data sets/AI Parenting Survey data from Qualtrics - numeric resp 6-21-19.csv",
             nlines=1,what=character(),sep=",")
AI_Parenting_Survey_data_from_Qualtrics_numeric_resp_6_21_19 <- read.csv(file="C:/Users/adamj89/Box/Data Analytics/Parenting Project/Original Data sets/AI Parenting Survey data from Qualtrics - numeric resp 6-21-19.csv",
                                                                         skip=2,header=FALSE)
View(AI_Parenting_Survey_data_from_Qualtrics_numeric_resp_6_21_19)
names(AI_Parenting_Survey_data_from_Qualtrics_numeric_resp_6_21_19)<-header
head(AI_Parenting_Survey_data_from_Qualtrics_numeric_resp_6_21_19)

#second scan to get the second row from data set
header2<-scan(file = "C:/Users/adamj89/Box/Data Analytics/Parenting Project/Original Data sets/AI Parenting Survey data from Qualtrics - numeric resp 6-21-19.csv",
              skip=1,nlines=1,what=character(),sep=",")
names(AI_Parenting_Survey_data_from_Qualtrics_numeric_resp_6_21_19)<-paste0(header,header2)

#rename data frame
AI_parenting.survey<-AI_Parenting_Survey_data_from_Qualtrics_numeric_resp_6_21_19

#delete 1st row since it is useless
AI_parenting.survey<-AI_parenting.survey[-c(1),]

#split date variables with mixed date and time entries into two columns
AI_parenting.survey<-AI_parenting.survey %>% 
  separate(`StartDateStart Date`,
           c("Start Date","StartTime"),
           sep=" ",extra="merge")
AI_parenting.survey<-AI_parenting.survey %>% 
  separate(`EndDateEnd Date`,
           c("End Date","EndTime"),
           sep=" ",extra="merge")
AI_parenting.survey<-AI_parenting.survey %>% 
  separate(`RecordedDateRecorded Date`,
           c("Recorded Date","RecordedTime"),
           sep=" ",extra="merge")

#change date columns to class "date"
AI_parenting.survey$`Start Date`<-as.Date(
  AI_parenting.survey$`Start Date`,format="%m/%d/%Y")
AI_parenting.survey$`End Date`<-as.Date(
  AI_parenting.survey$`End Date`,format="%m/%d/%Y")
AI_parenting.survey$`Recorded Date`<-as.Date(
  AI_parenting.survey$`Recorded Date`,format="%m/%d/%Y")

#fix the column names
names(AI_parenting.survey)[6]<-"IPAddress"
names(AI_parenting.survey)[7]<-"Progress"
names(AI_parenting.survey)[8]<-"Duration(Seconds)"
names(AI_parenting.survey)[9]<-"Finished"
names(AI_parenting.survey)[12]<-"ResponseID"
names(AI_parenting.survey)[17]<-"Latitude"
names(AI_parenting.survey)[18]<-"Longitude"

#delete the last column and the three empty columns
AI_parenting.survey<-AI_parenting.survey[-c(13,14,15,16,95)]

#look at structure of data frame
str(AI_parenting.survey)
#look for funky answers
lapply(AI_parenting.survey,unique)

#eliminate observations where they did not consent: 
AI_par.survey.consent<-subset(AI_parenting.survey,
                              `Q1Consent Form`=="1") #1 means they consensted
#elimiante observations that contain the strings "TurkPrime test" or "test"
#first find column indices that contain the above strings
which(sapply(AI_par.survey.consent[c(17:90)], 
             function(x) any(x == "test" | x == "TurkPrime test")))
#now find the row indices that have them
which(AI_par.survey.consent[c(73)]=="test" | 
        AI_par.survey.consent[c(73)]=="TurkPrime test")
which(AI_par.survey.consent[c(76)]=="test" | 
        AI_par.survey.consent[c(76)]=="TurkPrime test")
which(AI_par.survey.consent[c(79)]=="test" | 
        AI_par.survey.consent[c(79)]=="TurkPrime test")
which(AI_par.survey.consent[c(82)]=="test" | 
        AI_par.survey.consent[c(82)]=="TurkPrime test")
which(AI_par.survey.consent[c(89)]=="test" | 
        AI_par.survey.consent[c(89)]=="TurkPrime test")

#delete the rows
AI_par.survey.consent<-AI_par.survey.consent[-c(1,260,261),]
#check if it worked
lapply(AI_par.survey.consent,unique)

#look at structure
str(AI_par.survey.consent)

#change date columns to class "date"
AI_par.survey.consent$`Start Date`<-as.Date(
  AI_par.survey.consent$`Start Date`,format="%m/%d/%Y")
AI_par.survey.consent$`End Date`<-as.Date(
  AI_par.survey.consent$`End Date`,format="%m/%d/%Y")
AI_par.survey.consent$`Recorded Date`<-as.Date(
  AI_par.survey.consent$`Recorded Date`,format="%m/%d/%Y")

#delete time columns
AI_par.survey.consent<-AI_par.survey.consent[-c(2,4,11)]

#look at structure
str(AI_par.survey.consent)

#change duration and survey responses to numeric
AI_par.survey.consent[c(6,15:69,71,72,74,75,77,78,80:85,87)]<-sapply(
  AI_par.survey.consent[c(6,15:69,71,72,74,75,77,78,80:85,87)],
  function(x) as.numeric(as.character(x)))

#recode numeric responses for the questions
#check what they are
#lapply(AI_par.survey.no99[c(15:69,71,72,74,75,77,78,80:85,87)],unique)
#make -99 NA
AI_par.survey.no99<-AI_par.survey.consent
AI_par.survey.no99[AI_par.survey.no99==-99]<-NA
#recode the numeric responses
AI_par.survey.no99[c(43:51)]<-sapply(AI_par.survey.no99[c(43:51)],
                                     function(x) x-56)
AI_par.survey.no99[c(52:55)]<-sapply(AI_par.survey.no99[c(52:55)],
                                     function(x) x-47)
AI_par.survey.no99[c(56:61)]<-sapply(AI_par.survey.no99[c(56:61)],
                                     function(x) x-42)
AI_par.survey.no99[c(62:67)]<-sapply(AI_par.survey.no99[c(62:67)],
                                     function(x) x-43)
AI_par.survey.no99[,68]<-AI_par.survey.no99[,68]-119
AI_par.survey.no99[,72]<-AI_par.survey.no99[,72]-1
AI_par.survey.no99[,80]<-AI_par.survey.no99[,80]-70
AI_par.survey.no99[,81]<-AI_par.survey.no99[,81]-6
AI_par.survey.no99[,82]<-AI_par.survey.no99[,82]-6
AI_par.survey.consent[c(43:51)]<-sapply(AI_par.survey.consent[c(43:51)],
                                        function(x) x-56)
AI_par.survey.consent[c(52:55)]<-sapply(AI_par.survey.consent[c(52:55)],
                                        function(x) x-47)
AI_par.survey.consent[c(56:61)]<-sapply(AI_par.survey.consent[c(56:61)],
                                        function(x) x-42)
AI_par.survey.consent[c(62:67)]<-sapply(AI_par.survey.consent[c(62:67)],
                                        function(x) x-43)
AI_par.survey.consent[,68]<-AI_par.survey.consent[,68]-119
AI_par.survey.consent[,72]<-AI_par.survey.consent[,72]-1
AI_par.survey.consent[,80]<-AI_par.survey.consent[,80]-70
AI_par.survey.consent[,81]<-AI_par.survey.consent[,81]-6
AI_par.survey.consent[,82]<-AI_par.survey.consent[,82]-6
#check what they are
lapply(AI_par.survey.no99[c(15:69,71,72,74,75,77,78,80:85,87)],unique)
lapply(AI_par.survey.consent[c(15:69,71,72,74,75,77,78,80:85,87)],unique)

#cereal
#exclude if the youngest child was older than 5
AI_par.survey.age5_num<-subset(AI_par.survey.no99,
                               AI_par.survey.no99[,82]<=5)

#Create duplicate survey but have the responses as factors
AI_par.survey.age5_fact<-AI_par.survey.age5_num
sapply(AI_par.survey.age5_fact,unique)
#apply labels to factors and make them ordered
#Q3
AI_par.survey.age5_fact[c(15:20)]<-lapply(
  AI_par.survey.age5_fact[c(15:20)],
  function(x) factor(x,ordered=TRUE,
                     levels=c("1","2","3","4","5"),
                     labels=c("Not at all Important",
                              "Slightly important","Important",
                              "Very important","Extremely Important")))
#Q5
AI_par.survey.age5_fact[c(21:27)]<-lapply(
  AI_par.survey.age5_fact[c(21:27)],
  function(x) factor(x,ordered=TRUE,
                     levels=c("1","2","3","4","5","6"),
                     labels=c("0 hours","<1 hour",
                              "1 hour","2 hours","3 hours",
                              "4+ hours")))
#Q7
AI_par.survey.age5_fact[c(28:35)]<-lapply(
  AI_par.survey.age5_fact[c(28:35)],
  function(x) factor(x,ordered = TRUE,
                     levels=c("1","2","3","4","5","6"),
                     labels=c("0 hours","<1 hour",
                              "1 hour","2 hours","3 hours",
                              "4+ hours")))
#Q8
AI_par.survey.age5_fact[c(36:42)]<-lapply(
  AI_par.survey.age5_fact[c(36:42)],
  function(x) factor(x,ordered = TRUE,
                     levels=c("1","2","3","4","5"),
                     labels=c("0 times","1 time",
                              "2 times","3 times",
                              "4+ times")))
#Q8
AI_par.survey.age5_fact[c(43:51)]<-lapply(
  AI_par.survey.age5_fact[c(43:51)],
  function(x) factor(x,ordered = TRUE,
                     levels=c("1","2","3","4","5","6"),
                     labels=c("Strongly Disagree","Disagree",
                              "Slightly Disagree","Slightly Agree",
                              "Agree","Strongly Agree")))
#Q9
AI_par.survey.age5_fact[c(52:55)]<-lapply(
  AI_par.survey.age5_fact[c(52:55)],
  function(x) factor(x,ordered = TRUE,
                     levels=c("1","2","3","4","5","6"),
                     labels=c("Strongly Disagree","Disagree",
                              "Slightly Disagree","Slightly Agree",
                              "Agree","Strongly Agree")))
#Q10
AI_par.survey.age5_fact[c(56:61)]<-lapply(
  AI_par.survey.age5_fact[c(56:61)],
  function(x) factor(x,ordered = TRUE,
                     levels=c("1","2","3","4","5","6"),
                     labels=c("Strongly Disagree","Disagree",
                              "Slightly Disagree","Slightly Agree",
                              "Agree","Strongly Agree")))
#Q29
AI_par.survey.age5_fact[c(62:67)]<-lapply(
  AI_par.survey.age5_fact[c(62:67)],
  function(x) factor(x,ordered = TRUE,
                     levels=c("1","2","3","4","5","6"),
                     labels=c("Strongly Disagree","Disagree",
                              "Slightly Disagree","Slightly Agree",
                              "Agree","Strongly Agree")))
#brief questions about you and your family
#what are these?
lapply(AI_par.survey.age5_num[c(69:87)],class)
#convert to factors (numeric data frame)
AI_par.survey.age5_num[c(69)]<-lapply(
  AI_par.survey.age5_num[c(69)],
  function(x) factor(x,levels=c(1,2,4),
                     labels=c("Male","Female","Other"))
)
AI_par.survey.age5_num[c(71)]<-lapply(
  AI_par.survey.age5_num[c(71)],
  function(x) factor(x,levels=c("1","2"),
                     labels=c("Yes","No")))
AI_par.survey.age5_num[c(72)]<-lapply(
  AI_par.survey.age5_num[c(72)],
  function(x) factor(x,levels=c("1","2","3","4","5","6","7"),
                     labels=c("White","Black","Hispanic",
                              "Asian","Pacific Islander",
                              "American Indian","Other")))
AI_par.survey.age5_num[c(74)]<-lapply(
  AI_par.survey.age5_num[c(74)],
  function(x) factor(x,levels=c("1","2","3","4"),
                     labels=c("Very Well","Well",
                              "Not Well","Not Well At All")))
AI_par.survey.age5_num[c(75)]<-lapply(
  AI_par.survey.age5_num[c(75)],
  function(x) factor(x,levels=c("1","2","3"),
                     labels=c("English","Spanish",
                              "Other")))
AI_par.survey.age5_num[c(77)]<-lapply(
  AI_par.survey.age5_num[c(77)],
  function(x) factor(x,levels=c("1","2","3","4","5"),
                     labels=c("I was born in the U.S.",
                              "Less than a year",
                              "1-2 years",
                              "2-5 years",
                              "5+ years")))
AI_par.survey.age5_num[c(78)]<-lapply(
  AI_par.survey.age5_num[c(78)],
  function(x) factor(x,levels=c("1","2","3","4","5","6","7"),
                     labels=c("Single, Never married",
                              "Living with a Partner",
                              "Married","Separated",
                              "Divorced","Widowed",
                              "Other")))
AI_par.survey.age5_num[c(83)]<-lapply(
  AI_par.survey.age5_num[c(83)],
  function(x) factor(x,levels=c("1","2","3","4","5","6"),
                     labels=c("Less than HS degree",
                              "HS degree or equivalent",
                              "Some college but no degree",
                              "Associates Degree",
                              "Bachelor's Degree",
                              "Graduate Degree")))
AI_par.survey.age5_num[c(84)]<-lapply(
  AI_par.survey.age5_num[c(84)],
  function(x) factor(x,levels=c("1","4"),
                     labels=c("Yes","No")))
AI_par.survey.age5_num[c(85)]<-lapply(
  AI_par.survey.age5_num[c(85)],
  function(x) factor(x,levels=c("1","2","3"),
                     labels=c("Part-time","Full-time","other")))
AI_par.survey.age5_num[c(87)]<-lapply(
  AI_par.survey.age5_num[c(87)],
  function(x) factor(x,levels=c("1","2","3","4","5","6","7","8","9"),
                     labels=c("<$25k","$25k - <$50k",
                              "$50k - <$75k","$75k - <$100k",
                              "$100k - <$125k","$125k - <$150k",
                              "150k - <$175k","$175k - <$200k",
                              "\u2265$200k")))
#convert to factors (factor data frame)
AI_par.survey.age5_fact[c(69)]<-lapply(
  AI_par.survey.age5_fact[c(69)],
  function(x) factor(x,levels=c(1,2,4),labels=c("Male","Female","Other"))
)
AI_par.survey.age5_fact[c(71)]<-lapply(
  AI_par.survey.age5_fact[c(71)],
  function(x) factor(x,levels=c("1","2"),
                     labels=c("Yes","No")))
AI_par.survey.age5_fact[c(72)]<-lapply(
  AI_par.survey.age5_fact[c(72)],
  function(x) factor(x,levels=c("1","2","3","4","5","6","7"),
                     labels=c("White","Black","Hispanic",
                              "Asian","Pacific Islander",
                              "American Indian","Other")))
AI_par.survey.age5_fact[c(74)]<-lapply(
  AI_par.survey.age5_fact[c(74)],
  function(x) factor(x,levels=c("1","2","3","4"),
                     labels=c("Very Well","Well",
                              "Not Well","Not Well At All")))
AI_par.survey.age5_fact[c(75)]<-lapply(
  AI_par.survey.age5_fact[c(75)],
  function(x) factor(x,levels=c("1","2","3"),
                     labels=c("English","Spanish",
                              "Other")))
AI_par.survey.age5_fact[c(77)]<-lapply(
  AI_par.survey.age5_fact[c(77)],
  function(x) factor(x,levels=c("1","2","3","4","5"),
                     labels=c("I was born in the U.S.",
                              "Less than a year",
                              "1-2 years",
                              "2-5 years",
                              "5+ years")))
AI_par.survey.age5_fact[c(78)]<-lapply(
  AI_par.survey.age5_fact[c(78)],
  function(x) factor(x,levels=c("1","2","3","4","5","6","7"),
                     labels=c("Single, Never married",
                              "Living with a Partner",
                              "Married","Separated",
                              "Divorced","Widowed",
                              "Other")))
AI_par.survey.age5_fact[c(83)]<-lapply(
  AI_par.survey.age5_fact[c(83)],
  function(x) factor(x,levels=c("1","2","3","4","5","6"),
                     ordered = TRUE,
                     labels=c("Less than HS degree",
                              "HS degree or equivalent",
                              "Some college but no degree",
                              "Associates Degree",
                              "Bachelor's Degree",
                              "Graduate Degree")))
AI_par.survey.age5_fact[c(84)]<-lapply(
  AI_par.survey.age5_fact[c(84)],
  function(x) factor(x,levels=c("1","4"),
                     labels=c("Yes","No")))
AI_par.survey.age5_fact[c(85)]<-lapply(
  AI_par.survey.age5_fact[c(85)],
  function(x) factor(x,levels=c("1","2","3"),
                     labels=c("Part-time","Full-time","other")))
AI_par.survey.age5_fact[c(87)]<-lapply(
  AI_par.survey.age5_fact[c(87)],
  function(x) factor(x,levels=c("1","2","3","4","5","6","7","8","9"),
                     ordered = TRUE,
                     labels=c("<$25k","$25k - <$50k",
                              "$50k - <$75k","$75k - <$100k",
                              "$100k - <$125k","$125k - <$150k",
                              "150k - <$175k","$175k - <$200k",
                              "\u2265$200k")))

#rename survey questions
names(AI_par.survey.age5_num)<-c("Start Date","End Date",
                                 "Status Response Type","IP Address",
                                 "Progress","Duration (Seconds)",
                                 "Finished","Recorded Date",
                                 "Response ID","Latitude","Longitude",
                                 "Distribution Channel","User Language",
                                 "Consent Form","ParPriority1",
                                 "ParPriority2","ParPriority3",
                                 "ParPriority4","ParPriority5",
                                 "ParPriority6","TechTime7",
                                 "TechTime8","TechTime9","TechTime10",
                                 "TechTime11","TechTime12","TechTime13",
                                 "TechActivity14","TechActivity15",
                                 "TechActivity16","TechActivity17",
                                 "TechActivity18","TechActivity19",
                                 "TechActivity20","TechActivity21",
                                 "TechInterrupt22","TechInterrupt23",
                                 "TechInterrupt24","TechInterrupt25",
                                 "TechInterrupt26","TechInterrupt27",
                                 "TechInterrupt28","ProbTech29",
                                 "ProbTech30","ProbTech31","PercImpact32",
                                 "PercImpact33","Limit34","PercImpact35",
                                 "Limit36","PercImpact37","Limit38",
                                 "Limit39","Limit40","Limit41",
                                 "TechAccept42","TechAccept43",
                                 "TechAccept44","TechAccept45",
                                 "TechMuch46","PrivacyConc47",
                                 "TechAccept48","TechAccept49",
                                 "TechDiscuss50","TechDiscuss51",
                                 "TechDiscuss52","TechAccept53",
                                 "Age54","Gender55","Gender55TEXT",
                                 "Hispanic/Latino56","Race57",
                                 "Race57TEXT","English58",
                                 "Language59","Language59TEXT",
                                 "LivedInUS60","MaritalStatus61",
                                 "MaritalStatus61TEXT","HouseholdSize62",
                                 "No.Children63","AgeYoungestChild64",
                                 "Education65","Work?66",
                                 "Part/Fulltime67","Part/Fulltime67TEXT",
                                 "Income68")
#rename survey questions
names(AI_par.survey.age5_fact)<-c("Start Date","End Date",
                                  "Status Response Type","IP Address",
                                  "Progress","Duration (Seconds)",
                                  "Finished","Recorded Date",
                                  "Response ID","Latitude","Longitude",
                                  "Distribution Channel","User Language",
                                  "Consent Form","ParPriority1",
                                  "ParPriority2","ParPriority3",
                                  "ParPriority4","ParPriority5",
                                  "ParPriority6","TechTime7",
                                  "TechTime8","TechTime9","TechTime10",
                                  "TechTime11","TechTime12","TechTime13",
                                  "TechActivity14","TechActivity15",
                                  "TechActivity16","TechActivity17",
                                  "TechActivity18","TechActivity19",
                                  "TechActivity20","TechActivity21",
                                  "TechInterrupt22","TechInterrupt23",
                                  "TechInterrupt24","TechInterrupt25",
                                  "TechInterrupt26","TechInterrupt27",
                                  "TechInterrupt28","ProbTech29",
                                  "ProbTech30","ProbTech31","PercImpact32",
                                  "PercImpact33","Limit34","PercImpact35",
                                  "Limit36","PercImpact37","Limit38",
                                  "Limit39","Limit40","Limit41",
                                  "TechAccept42","TechAccept43",
                                  "TechAccept44","TechAccept45",
                                  "TechMuch46","PrivacyConc47",
                                  "TechAccept48","TechAccept49",
                                  "TechDiscuss50","TechDiscuss51",
                                  "TechDiscuss52","TechAccept53",
                                  "Age54","Gender55","Gender55TEXT",
                                  "Hispanic/Latino56","Race57",
                                  "Race57TEXT","English58",
                                  "Language59","Language59TEXT",
                                  "LivedInUS60","MaritalStatus61",
                                  "MaritalStatus61TEXT","HouseholdSize62",
                                  "No.Children63","AgeYoungestChild64",
                                  "Education65","Work?66",
                                  "Part/Fulltime67","Part/Fulltime67TEXT",
                                  "Income68")
#Drop the text columns
AI_par.survey.age5_num<-subset(AI_par.survey.age5_num,
                               select=-c(Gender55TEXT,
                                         Race57TEXT,
                                         Language59TEXT,
                                         `Part/Fulltime67TEXT`,
                                         MaritalStatus61TEXT))
AI_par.survey.age5_fact<-subset(AI_par.survey.age5_fact,
                                select=-c(Gender55TEXT,
                                          Race57TEXT,
                                          Language59TEXT,
                                          `Part/Fulltime67TEXT`,
                                          MaritalStatus61TEXT))

### Check for Dishonest Survey Respondents ###
#"Speed Testers" 
#investigate duration(seconds) variable
summary(AI_par.survey.age5_num$`Duration (Seconds)`)
describe(AI_par.survey.age5_num$`Duration (Seconds)`)
quantile(AI_par.survey.age5_num$`Duration (Seconds)`,
         probs=c(0,.05,.1,.25,.5,.75,1))
#check distribution
ggplot(AI_par.survey.age5_num,aes(x=`Duration (Seconds)`)) + 
  geom_histogram(binwidth = 10) + 
  xlim(80,355) + labs(title="Every 20 seconds, Begins at 80")
ggplot(AI_par.survey.age5_num,aes(x=`Duration (Seconds)`)) + 
  stat_ecdf() + xlim(0,500)
#get exact counts of those under 137 seconds (2 seconds/question)
sum(AI_par.survey.age5_num$`Duration (Seconds)`<136)
#exclude anyone under 140 seconds duration
AI_par.survey_age5_num_dur136<-subset(AI_par.survey.age5_num,
                                      `Duration (Seconds)`>=136)
AI_par.survey.age5_fact_dur136<-subset(AI_par.survey.age5_fact,
                                       `Duration (Seconds)`>136)

#"Straight-liners"#
#make a new data frame from the numeric survey df
AI_par.survey_age5_num_dur136_transp<-AI_par.survey_age5_num_dur136
#keep only relevant parts (the surveyr esponss)
AI_par.survey_age5_num_dur136_transp<-AI_par.survey_age5_num_dur136_transp[c(15:67)]
#transpose the data frame
AI_par.survey_age5_num_dur136_transp<-t(AI_par.survey_age5_num_dur136_transp)
AI_par.survey_age5_num_dur136_transp.df<-as.data.frame(
  AI_par.survey_age5_num_dur136_transp
)

#Create the scale point variation function
#5 point scale
spv.5pt<-function(var,
                  row.begin,
                  row.end,
                  items){
  one<-sum(var[c(row.begin:row.end)]==1)
  two<-sum(var[c(row.begin:row.end)]==2)
  three<-sum(var[c(row.begin:row.end)]==3)
  four<-sum(var[c(row.begin:row.end)]==4)
  five<-sum(var[c(row.begin:row.end)]==5)
  spv.value<-1-(sum((one/items)^2,
                    (two/items)^2,
                    (three/items)^2,
                    (four/items)^2,
                    (five/items)^2))
  print(spv.value)
}
#6 point scale
spv.6pt<-function(var,
                  row.begin,
                  row.end,
                  items){
  one<-sum(var[c(row.begin:row.end)]==1)
  two<-sum(var[c(row.begin:row.end)]==2)
  three<-sum(var[c(row.begin:row.end)]==3)
  four<-sum(var[c(row.begin:row.end)]==4)
  five<-sum(var[c(row.begin:row.end)]==5)
  six<-sum(var[c(row.begin:row.end)]==6)
  spv.value<-1-(sum((one/items)^2,
                    (two/items)^2,
                    (three/items)^2,
                    (four/items)^2,
                    (five/items)^2,
                    (six/items)^2))
  print(spv.value)
}

#apply scale point variation function
#block1
spv.block1<-sapply(AI_par.survey_age5_num_dur136_transp.df[c(1:6),],
                   spv.5pt,row.begin=1,row.end=6,items=6)
spv.block1<-round(spv.block1,digits=2) #round
#block2
spv.block2<-sapply(AI_par.survey_age5_num_dur136_transp.df[c(7:13),],
                   spv.6pt,row.begin=1,row.end=7,items=7)
spv.block2<-round(spv.block2,digits=2) #round
#block3
spv.block3<-sapply(AI_par.survey_age5_num_dur136_transp.df[c(14:21),],
                   spv.6pt,row.begin=1,row.end=8,items=8)
spv.block3<-round(spv.block3,digits=2) #round
#block4
spv.block4<-sapply(AI_par.survey_age5_num_dur136_transp.df[c(22:28),],
                   spv.5pt,row.begin=1,row.end=7,items=7)
spv.block4<-round(spv.block4,digits=2) #round
#block5
spv.block5<-sapply(AI_par.survey_age5_num_dur136_transp.df[c(29:37),],
                   spv.6pt,row.begin=1,row.end=9,items=9)
spv.block5<-round(spv.block5,digits=2) #round
#block6
spv.block6<-sapply(AI_par.survey_age5_num_dur136_transp.df[c(38:41),],
                   spv.6pt,row.begin=1,row.end=4,items=4)
spv.block6<-round(spv.block6,digits=2) #round
#block7
spv.block7<-sapply(AI_par.survey_age5_num_dur136_transp.df[c(42:47),],
                   spv.6pt,row.begin=1,row.end=6,items=6)
spv.block7<-round(spv.block7,digits=2) #round
#block8
spv.block8<-sapply(AI_par.survey_age5_num_dur136_transp.df[c(48:53),],
                   spv.6pt,row.begin=1,row.end=6,items=6)
spv.block8<-round(spv.block8,digits=2) #round

#attach results to data frame
AI_par.survey_age5_num_dur136_transp.df<-rbind(
  AI_par.survey_age5_num_dur136_transp.df,
  t(spv.block1),
  t(spv.block2),
  t(spv.block3),
  t(spv.block4),
  t(spv.block5),
  t(spv.block6),
  t(spv.block7),
  t(spv.block8)
)
rownames(AI_par.survey_age5_num_dur136_transp.df)[54]<-"SPV1"
rownames(AI_par.survey_age5_num_dur136_transp.df)[55]<-"SPV2"
rownames(AI_par.survey_age5_num_dur136_transp.df)[56]<-"SPV3"
rownames(AI_par.survey_age5_num_dur136_transp.df)[57]<-"SPV4"
rownames(AI_par.survey_age5_num_dur136_transp.df)[58]<-"SPV5"
rownames(AI_par.survey_age5_num_dur136_transp.df)[59]<-"SPV6"
rownames(AI_par.survey_age5_num_dur136_transp.df)[60]<-"SPV7"
rownames(AI_par.survey_age5_num_dur136_transp.df)[61]<-"SPV8"

#rbind mean spv
mean.spv<-sapply(AI_par.survey_age5_num_dur136_transp.df[c("SPV1","SPV2","SPV3","SPV4",
                                                           "SPV5","SPV6","SPV7","SPV8"),],
                 mean)
mean.spv<-round(mean.spv,digits=2)
AI_par.survey_age5_num_dur136_transp.df<-rbind(
  AI_par.survey_age5_num_dur136_transp.df,
  t(mean.spv)
)
#rbind standardize spv 
#which(is.na(AI_par.survey_age5_num_dur136_transp.df[62,])) #identify na values
standardize.spv<-scale(t(
  AI_par.survey_age5_num_dur136_transp.df[62,]),)
standardize.spv<-round(standardize.spv,digits=2)
AI_par.survey_age5_num_dur136_transp.df<-rbind(AI_par.survey_age5_num_dur136_transp.df,
                                               t(as.data.frame(standardize.spv)))
rownames(AI_par.survey_age5_num_dur136_transp.df)[62]<-"mean SPV"
rownames(AI_par.survey_age5_num_dur136_transp.df)[63]<-"Z-score SPV"

#obtain counts
table(t(AI_par.survey_age5_num_dur136_transp.df[54,]))
table(t(AI_par.survey_age5_num_dur136_transp.df[55,]))
table(t(AI_par.survey_age5_num_dur136_transp.df[56,]))
table(t(AI_par.survey_age5_num_dur136_transp.df[57,]))
table(t(AI_par.survey_age5_num_dur136_transp.df[58,]))
table(t(AI_par.survey_age5_num_dur136_transp.df[59,]))
table(t(AI_par.survey_age5_num_dur136_transp.df[60,]))
table(t(AI_par.survey_age5_num_dur136_transp.df[61,]))

#Find the fishy observations based off the z-scores of their scale point variations
which(AI_par.survey_age5_num_dur136_transp.df["Z-score SPV",]<=-2)
colnames(AI_par.survey_age5_num_dur136_transp.df[c(17,22,29,49,56,59,89,140,149,168,169,187,217,248,279)])
#plot to see
index<-seq(1,NROW(AI_par.survey_age5_num_dur136_transp.df))
AI_par.survey_age5_num_dur136_transp.df$index<-index
#-2.68
p24<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],
            aes(x=index[c(1:53)],y=`24`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 24: SPV -2.68") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
#-2.2
p30<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`30`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 30: SPV -2.2") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
grid.arrange(p24,p30,nrow=2)
#-2.04
p40<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`40`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 40: SPV -2.04") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
#-2.12
p67<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`67`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) +
  ggtitle("ID 67: SPV -2.12") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
grid.arrange(p40,p67,nrow=2)
#-2.68
p76<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`76`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 76: SPV -2.68") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
#-2.68
p79<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`79`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 79: SPV -2.68") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
grid.arrange(p76,p79,nrow=2)
#-2.99
p117<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`117`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 115: SPV -2.99") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
#-2.36
p173<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`173`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 170: SPV -2.36") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
grid.arrange(p117,p173,nrow=2)
#-2.2
p178<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`182`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 178: SPV -2.2") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
#-3.15
p201<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`201`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 201: SPV -3.15") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
grid.arrange(p178,p201,nrow=2)
#-3.79
p202<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`202`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 202: SPV -3.79")
#-3.31
p221<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`221`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 221: SPV -3.31") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
grid.arrange(p202,p221,nrow=2)
#-3.15
p255<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`255`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 255: SPV -3.15") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
#-2.44
p309<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`309`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 309: SPV -2.44") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
grid.arrange(p255,p309,nrow=2)
#-3.79
p346<-ggplot(AI_par.survey_age5_num_dur136_transp.df[c(1:53),],aes(x=index[c(1:53)],y=`346`[c(1:53)])) + 
  geom_point() + 
  scale_x_continuous(breaks=c(6,13,21,28,37,41,47,53)) + 
  ggtitle("ID 346: SPV -3.79") + 
  labs(y="Rating",x="1-6; 7-28; 29-41; 42-53")
p346

#remove subjects 202 and 346, who had standardized SPV scores of -3.79
AI_par.survey_age5_num_dur136_spv<-AI_par.survey_age5_num_dur136[
  !rownames(AI_par.survey_age5_num_dur136) %in% c("202","346"), ]
AI_par.survey_age5_fact_dur136_spv<-AI_par.survey.age5_fact_dur136[
  !rownames(AI_par.survey.age5_fact_dur136) %in% c("202","346"), ]

###import variables from Jill's CSV
AI_Parent_Survey_Numeric_9_5_19 <- read_csv("C:/Users/adamj89/Box/Data Analytics/Parenting Project/Original Data sets/AI Parent Survey Numeric 9-5-19.csv")
#order them by var1
AI_Parent_Survey_Numeric_9_5_19<-AI_Parent_Survey_Numeric_9_5_19[
  order(AI_Parent_Survey_Numeric_9_5_19$var1),
  ]
#cbind the new variables into existing data set
AI_par.survey_age5_num_dur136_spv<-cbind.data.frame(
  AI_par.survey_age5_num_dur136_spv,
  AI_Parent_Survey_Numeric_9_5_19[c(88,89,96)]
)

#GIS Mapping#
AI_par.survey_age5_num_dur136_spv$Latitude<-as.numeric(
  as.character(AI_par.survey_age5_num_dur136_spv$Latitude)
)
AI_par.survey_age5_num_dur136_spv$Longitude<-as.numeric(
  as.character(AI_par.survey_age5_num_dur136_spv$Longitude)
)
#use existing function to find the states
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}
#make appropriate data frame with column 1 longitude and column 2 latitude
long.lat<-AI_par.survey_age5_num_dur136_spv[c("Longitude","Latitude")]
us.states<-latlong2state(long.lat)
us.states2<-latlong2state(long.lat)
AI_par.survey_age5_num_dur136_spv$State<-us.states
#assign them census regions
#break data points into four US Census regions
us.states[us.states=="connecticut" | 
            us.states=="maine" | 
            us.states=="massachusetts" | 
            us.states=="new hampshire" | 
            us.states=="rhode island" | 
            us.states=="vermont" | 
            us.states=="new jersey" | 
            us.states=="new york" | 
            us.states=="pennsylvania"]<-"Northeast"
us.states[us.states=="illinois" | 
            us.states=="indiana" | 
            us.states=="michigan" | 
            us.states=="ohio" | 
            us.states=="wisconsin" | 
            us.states=="iowa" | 
            us.states=="kansas" | 
            us.states=="minnesota" | 
            us.states=="missouri" | 
            us.states=="nebraska" | 
            us.states=="north dakota" | 
            us.states=="south dakota"]<-"Midwest"
us.states[us.states=="delaware" | 
            us.states=="florida" | 
            us.states=="georgia" | 
            us.states=="maryland" | 
            us.states=="north carolina" | 
            us.states=="south carolina" | 
            us.states=="virginia" | 
            us.states=="district of columbia" | 
            us.states=="west virginia" | 
            us.states=="alabama" |
            us.states=="kentucky" | 
            us.states=="mississippi" | 
            us.states=="tennessee" | 
            us.states=="arkansas" | 
            us.states=="louisiana" | 
            us.states=="oklahoma" | 
            us.states=="texas"]<-"South"
us.states[us.states=="arizona" | 
            us.states=="colorado" |
            us.states=="idaho" | 
            us.states=="montana" | 
            us.states=="nevada" | 
            us.states=="new mexico" | 
            us.states=="alaska" | 
            us.states=="california" | 
            us.states=="hawaii" | 
            us.states=="oregon" | 
            us.states=="washington" | 
            us.states=="utah" | 
            us.states=="wyoming"]<-"West"
#break data points into the census divisions
us.states2[us.states2=="connecticut" | 
             us.states2=="maine" | 
             us.states2=="massachusetts" | 
             us.states2=="new hampshire" |
             us.states2=="rhode island" | 
             us.states2=="vermont"]<-"New England"
us.states2[us.states2=="new jersey" | 
             us.states2=="new york" | 
             us.states2=="pennsylvania"]<-"Mid-Atlantic"
us.states2[us.states2=="illinois" | 
             us.states2=="indiana" | 
             us.states2=="michigan" | 
             us.states2=="ohio" | 
             us.states2=="wisconsin"]<-"E. North Central"
us.states2[us.states2=="iowa" | 
             us.states2=="kansas" | 
             us.states2=="minnesota" | 
             us.states2=="missouri" | 
             us.states2=="nebraska" | 
             us.states2=="north dakota" | 
             us.states2=="south dakota"]<-"W. North Central"
us.states2[us.states2=="delaware" | 
             us.states2=="florida" | 
             us.states2=="georgia" | 
             us.states2=="maryland" | 
             us.states2=="north carolina" | 
             us.states2=="south carolina" | 
             us.states2=="virginia" | 
             us.states2=="district of columbia" | 
             us.states2=="west virginia"]<-"South Atlantic"
us.states2[us.states2=="alabama" | 
             us.states2=="kentucky" | 
             us.states2=="mississippi" | 
             us.states2=="tennessee"]<-"E. South Central"
us.states2[us.states2=="arkansas" | 
             us.states2=="louisiana" | 
             us.states2=="oklahoma" | 
             us.states2=="texas"]<-"W. South Central"
us.states2[us.states2=="arizona" | 
             us.states2=="colorado" | 
             us.states2=="idaho" | 
             us.states2=="montana" | 
             us.states2=="nevada" | 
             us.states2=="new mexico" | 
             us.states2=="utah" | 
             us.states2=="wyoming"]<-"Mountain"
us.states2[us.states2=="alaska" | 
             us.states2=="california" | 
             us.states2=="hawaii" | 
             us.states2=="oregon" | 
             us.states2=="washington"]<-"Pacific"
#attach to data frame and move next latitude and longitude
AI_par.survey_age5_num_dur136_spv$cens.reg<-us.states
AI_par.survey_age5_num_dur136_spv$cens.div<-us.states2
#fix just a few of them
which(is.na(AI_par.survey_age5_num_dur136_spv$cens.reg))
AI_par.survey_age5_num_dur136_spv[2,"State"]<-"washington"
AI_par.survey_age5_num_dur136_spv[18,"State"]<-"venezuela"
AI_par.survey_age5_num_dur136_spv[45,"State"]<-"venezuela"
AI_par.survey_age5_num_dur136_spv[97,"State"]<-"new york" 
AI_par.survey_age5_num_dur136_spv[105,"State"]<-"venezuela"
AI_par.survey_age5_num_dur136_spv[176,"State"]<-"alaska" 
AI_par.survey_age5_num_dur136_spv[220,"State"]<-"hawaii" 
AI_par.survey_age5_num_dur136_spv[262,"State"]<-"california" 
AI_par.survey_age5_num_dur136_spv[2,"cens.reg"]<-"West" 
AI_par.survey_age5_num_dur136_spv[18,"cens.reg"]<-"venezuela"
AI_par.survey_age5_num_dur136_spv[45,"cens.reg"]<-"venezuela"
AI_par.survey_age5_num_dur136_spv[97,"cens.reg"]<-"Northeast" 
AI_par.survey_age5_num_dur136_spv[105,"cens.reg"]<-"venezuela"
AI_par.survey_age5_num_dur136_spv[176,"cens.reg"]<-"West" 
AI_par.survey_age5_num_dur136_spv[220,"cens.reg"]<-"West" 
AI_par.survey_age5_num_dur136_spv[262,"cens.reg"]<-"West" 
AI_par.survey_age5_num_dur136_spv[2,"cens.div"]<-"Pacific" 
AI_par.survey_age5_num_dur136_spv[18,"cens.div"]<-"venezuela"
AI_par.survey_age5_num_dur136_spv[45,"cens.div"]<-"venezuela"
AI_par.survey_age5_num_dur136_spv[97,"cens.div"]<-"Mid-Atlantic"
AI_par.survey_age5_num_dur136_spv[105,"cens.div"]<-"venezuela"
AI_par.survey_age5_num_dur136_spv[176,"cens.div"]<-"Pacific" 
AI_par.survey_age5_num_dur136_spv[220,"cens.div"]<-"Pacific" 
AI_par.survey_age5_num_dur136_spv[262,"cens.div"]<-"Pacific"
#delete observations from Venezuela
AI_par.survey_age5_num_dur136_spv[-c(18,45,105),]->AI_par.survey_age5_num_dur136_spv_no.ven

#relevel some of the variables
attach(AI_par.survey_age5_num_dur136_spv_no.ven)
#Income
Income68.5lvl<-Income68
levels(Income68.5lvl)<-list(
  "<$25k"="<25k",
  "$25k - <$50k"="$25k - <$50k",
  "$50k - <$75k"="$50k - <$75k",
  "$75k - <$100k"="$75k - <$100k",
  ">=$100k"=c("$100k - <$125k","$125k - <$150k",
              "150k - <$175k","$175k - <$200k",
              "\u2265$200k"))
#education
education.3lvl<-Education65
levels(education.3lvl)<-list(
  "HS or less"=c("Less than HS degree","HS degree or equivalent"),
  "Some College"=c("Some college but no degree"),
  "College Degree"=c("Associates Degree","Bachelor's Degree",
                     "Graduate Degree"))
#language
language.2lvl<-Language59
levels(language.2lvl)<-list(
  "English"="English",
  "Other"=c("Spanish","Other")
)
#race/ethnicity
race.5lvl<-Race57
levels(race.5lvl)<-list(
  "White"="White",
  "Black"="Black",
  "Hispanic"="Hispanic",
  "Asian"="Asian",
  "Other"=c("Pacific Islander","American Indian","Other")
)
#Number of Children
numberchild.2lvl<-No.Children63
numberchild.2lvl<-factor(as.character(numberchild.2lvl))
levels(numberchild.2lvl)<-list(
  "1"="1",
  ">1"=c("2","3","4","5","6","7")
)
detach(AI_par.survey_age5_num_dur136_spv_no.ven)

#create real technoference scale
technoreal<-rowSums(
  AI_par.survey_age5_num_dur136_spv_no.ven[c(36:37,39:42)],
  na.rm = TRUE)/6
AI_par.survey_age5_num_dur136_spv_no.ven$TechReal<-technoreal

#for jill
AI_par.survey_age5_num_dur136_spv_99<-AI_par.survey_age5_num_dur136_spv
AI_par.survey_age5_num_dur136_spv_99[c(15:68,76:78,83:85)][is.na(
  AI_par.survey_age5_num_dur136_spv_99[c(15:68,76:78,83:85)])]<-(-99)
#write.csv(AI_par.survey_age5_num_dur136_spv_99,"AI Parent Survey Data_no99.csv")

#find the deleted ones
  #the almost speedsters
which(AI_par.survey.age5_num$`Duration (Seconds)`==138 | 
        AI_par.survey.age5_num$`Duration (Seconds)`==140)
duration.btw136and140<-AI_par.survey.age5_num[c(29,253),]
  #the straightliners
which(rownames(AI_par.survey_age5_num_dur136) %in% c("202", "346"))
straightliners.2<-AI_par.survey_age5_num_dur136[c(169,279),]
  #the venezuelanas
venezuelans<-AI_par.survey_age5_num_dur136_spv[c(18,45,105),]
  #find their sex
duration.btw136and140$Gender55
straightliners.2$Gender55
venezuelans$Gender55