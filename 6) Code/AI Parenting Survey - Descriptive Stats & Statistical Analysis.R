library(formattable)
library(car)
library(lavaan)
library(MBESS)
library(pedometrics)
library(semPlot)
library(psycho)
library(mediation)
library(medflex)
library(MBESS)
library(diagram)
###Table One###
table1.df<-data.frame(Age=AI_par.survey_age5_num_dur136_spv_no.ven$Age54,
                      Gender=AI_par.survey_age5_num_dur136_spv_no.ven$Gender55,
                      Race=race.5lvl,
                      Language=language.2lvl,
                      Children=numberchild.2lvl,
                      Education=education.3lvl,
                      Income=Income68.5lvl,
                      Geography=AI_par.survey_age5_num_dur136_spv_no.ven$cens.reg)
list.vars<-c("Age","Gender","Race","Language",
             "Children","Education","Income",
             "Geography")
cat.vars<-c("Gender","Race","Language",
            "Children","Education","Income",
            "Geography")
table1<-CreateTableOne(vars=list.vars,
                       data = table1.df,
                       factorVars = cat.vars,
                       includeNA = TRUE)
summary(table1)
#mode
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

###Bivariate Analysis###
#Calculate mean, sd, and Cronbach's alpha of outcomes by education and gender.
#Then compute correlation and t-tests/Mann Whitney tests for key variables
#First correct variables for analysis
  #Sex: make "other" NA
sex.mf<-AI_par.survey_age5_num_dur136_spv_no.ven$Gender55
levels(sex.mf)[levels(sex.mf)=='Other'] <- NA
AI_par.survey_age5_num_dur136_spv_no.ven$sex.mf<-sex.mf
  #Race: collapse "Asian," "Pacific Islander," "American Indian," and "Other"
race.4lvl<-AI_par.survey_age5_num_dur136_spv_no.ven$Race57
levels(race.4lvl)<-list(
  "White"="White",
  "Black"="Black",
  "Hispanic"="Hispanic",
  "Other"=c("Asian","Pacific Islander",
            "American Indian","Other")
)
AI_par.survey_age5_num_dur136_spv_no.ven$race4.lvl<-race.4lvl
  #Education
AI_par.survey_age5_num_dur136_spv_no.ven$education.3lvl<-education.3lvl
  #Income
AI_par.survey_age5_num_dur136_spv_no.ven$Income68.5lvl<-Income68.5lvl
  #number of children
AI_par.survey_age5_num_dur136_spv_no.ven$numberchild.2lvl<-numberchild.2lvl
  #language
AI_par.survey_age5_num_dur136_spv_no.ven$language.2lvl<-language.2lvl

#make correlation matrix of demographic characteristics
attach(AI_par.survey_age5_num_dur136_spv_no.ven)
  #sex
sex.mf_mat<-as.character(sex.mf)
sex.mf_mat[sex.mf_mat=="Male"]<-"1"
sex.mf_mat[sex.mf_mat=="Female"]<-"2"
sex.mf_mat<-as.numeric(sex.mf_mat)
  #education
education.3lvl_mat<-as.character(education.3lvl)
education.3lvl_mat[education.3lvl_mat=="HS or less"]<-"1"
education.3lvl_mat[education.3lvl_mat=="Some College"]<-"2"
education.3lvl_mat[education.3lvl_mat=="College Degree"]<-"3"
education.3lvl_mat<-as.numeric(education.3lvl_mat)
  #race
race.4lvl_mat<-as.character(race.4lvl)
race.4lvl_mat[race.4lvl_mat=="White"]<-"1"
race.4lvl_mat[race.4lvl_mat=="Black"]<-"2"
race.4lvl_mat[race.4lvl_mat=="Hispanic"]<-"3"
race.4lvl_mat[race.4lvl_mat=="Other"]<-"4"
race.4lvl_mat<-as.numeric(race.4lvl_mat)
  #language
language.2lvl_mat<-as.character(language.2lvl)
language.2lvl_mat[language.2lvl_mat=="English"]<-"1"
language.2lvl_mat[language.2lvl_mat=="Other"]<-"2"
language.2lvl_mat<-as.numeric(language.2lvl_mat)
  #income
Income68.5lvl_mat<-as.character(Income68.5lvl)
Income68.5lvl_mat[Income68.5lvl_mat=="<$25k"]<-"1"
Income68.5lvl_mat[Income68.5lvl_mat=="$25k - $50k"]<-"2"
Income68.5lvl_mat[Income68.5lvl_mat=="$50k - <$75k"]<-"3"
Income68.5lvl_mat[Income68.5lvl_mat=="$75k - <$100k"]<-"4"
Income68.5lvl_mat[Income68.5lvl_mat==">=$100k"]<-"5"
Income68.5lvl_mat<-as.numeric(Income68.5lvl_mat)
  #number of children
numberchild.2lvl_mat<-as.character(numberchild.2lvl)
numberchild.2lvl_mat[numberchild.2lvl_mat==">1"]<-"2"
numberchild.2lvl_mat<-as.numeric(numberchild.2lvl_mat)
  #geographic region
cens.reg_mat<-as.character(cens.reg)
cens.reg_mat[cens.reg_mat=="West"]<-"1"
cens.reg_mat[cens.reg_mat=="Midwest"]<-"2"
cens.reg_mat[cens.reg_mat=="South"]<-"3"
cens.reg_mat[cens.reg_mat=="Northeast"]<-"4"
cens.reg_mat<-as.numeric(cens.reg_mat)

#correlation matrix
corr.mat_demog<-cbind(Age54,sex.mf_mat,education.3lvl_mat,
                      race.4lvl_mat,language.2lvl_mat,
                      Income68.5lvl_mat,numberchild.2lvl_mat,
                      cens.reg_mat,ParentProbTech,NumDevInt,TAM
)
corr.mat<-cor(corr.mat_demog,use="complete.obs")
colnames(corr.mat)<-c("Age","Sex","Education","Race","Language","Income","Children","Region",
                         "PPT","NumDevInt","TAM")
rownames(corr.mat)<-c("Age","Sex","Education","Race","Language","Income","Children","Region",
                      "PPT","NumDevInt","TAM")
corr.mat_df<-as.data.frame(corr.mat)
corr.mat_df.rnd<-round(corr.mat_df,digits=4)

#Cronbach's Alpha
  #Parent Prob Tech
psych::alpha(AI_par.survey_age5_num_dur136_spv_no.ven[c(43:45)])$total$std.alpha  
  #technoference
psych::alpha(AI_par.survey_age5_num_dur136_spv_no.ven[c(36:42)])$total$std.alpha
  #TAM
psych::alpha(AI_par.survey_age5_num_dur136_spv_no.ven[c(56:59,62,63,67)])$total$std.alpha

#Overall mean/SD
lapply(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                           "NumDevInt","TAM",
                                           "PercImpact33",
                                           "Limit34",
                                           "TechMuch46",
                                           "PrivacyConc47")],
       mean,na.rm=TRUE)
lapply(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                           "NumDevInt","TAM",
                                           "PercImpact33",
                                           "Limit34",
                                           "TechMuch46",
                                           "PrivacyConc47")],
       sd,na.rm=TRUE)

#Age correlation
cor.test(~ Age54 + ParentProbTech,
         data=AI_par.survey_age5_num_dur136_spv_no.ven,
         method=c("pearson"))
cor.test(~ Age54 + NumDevInt,
         data=AI_par.survey_age5_num_dur136_spv_no.ven,
         method=c("pearson"))
cor.test(~ Age54 + TAM,
         data=AI_par.survey_age5_num_dur136_spv_no.ven,
         method=c("pearson"))
cor.test(~ Age54 + PercImpact33,
         data=AI_par.survey_age5_num_dur136_spv_no.ven,
         method=c("pearson"),use="complete.obs")
cor.test(~ Age54 + Limit34,
         data=AI_par.survey_age5_num_dur136_spv_no.ven,
         method=c("pearson"),use="complete.obs")
cor.test(~ Age54 + TechMuch46,
         data=AI_par.survey_age5_num_dur136_spv_no.ven,
         method=c("pearson"),use="complete.obs")
cor.test(~ Age54 + PrivacyConc47,
         data=AI_par.survey_age5_num_dur136_spv_no.ven,
         method=c("pearson"),use="complete.obs")
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[
  c("ParentProbTech","NumDevInt","TAM")],
  list(AI_par.survey_age5_num_dur136_spv_no.ven$Age54),
  mean,na.rm=TRUE)

#Sex mean/sd
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$sex.mf),
          mean,na.rm=TRUE)
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$sex.mf),
          sd,na.rm=TRUE)
#Sex t-test or Mann Whitney test
  #check if equal variances
AI.par.survey_females<-subset(AI_par.survey_age5_num_dur136_spv_no.ven,
                              sex.mf=="Female")
AI.par.survey_males<-subset(AI_par.survey_age5_num_dur136_spv_no.ven,
                            sex.mf=="Male")
var.test(AI.par.survey_females$ParentProbTech,
         AI.par.survey_males$ParentProbTech) #test showed equal vars
var.test(AI.par.survey_females$NumDevInt,
         AI.par.survey_males$NumDevInt) #test showed equal vars
var.test(AI.par.survey_females$TAM,
         AI.par.survey_males$TAM) #test showed equal vars
var.test(AI.par.survey_females$PercImpact33,
         AI.par.survey_males$PercImpact33) #test showed equal vars
var.test(AI.par.survey_females$Limit34,
         AI.par.survey_males$Limit34) #test showed equal vars
var.test(AI.par.survey_females$TechMuch46,
         AI.par.survey_males$TechMuch46) #test showed equal vars
var.test(AI.par.survey_females$PrivacyConc47,
         AI.par.survey_males$PrivacyConc47) #test showed equal vars
  #check if normal
shapiro.test(AI.par.survey_females$ParentProbTech) #test showed nonnormal
shapiro.test(AI.par.survey_males$ParentProbTech) #test showed nonnormal
shapiro.test(AI.par.survey_females$NumDevInt) #test showed nonnormal
shapiro.test(AI.par.survey_males$NumDevInt) #test showed nonnormal
shapiro.test(AI.par.survey_females$TAM) #test showed nonnormal
shapiro.test(AI.par.survey_males$TAM) #test showed nonnormal
shapiro.test(AI.par.survey_females$PercImpact33) #test showed nonnormal
shapiro.test(AI.par.survey_males$PercImpact33) #test showed nonnormal
shapiro.test(AI.par.survey_females$Limit34) #test showed nonnormal
shapiro.test(AI.par.survey_males$Limit34) #test showed nonnormal
shapiro.test(AI.par.survey_females$TechMuch46) #test showed nonnormal
shapiro.test(AI.par.survey_males$TechMuch46) #test showed nonnormal
shapiro.test(AI.par.survey_females$PrivacyConc47) #test showed nonnormal
shapiro.test(AI.par.survey_males$PrivacyConc47)
  #compute Mann-Whitney test since data are non-normal
wilcox.test(AI.par.survey_females$ParentProbTech,
            AI.par.survey_males$ParentProbTech)
wilcox.test(AI.par.survey_females$NumDevInt,
            AI.par.survey_males$NumDevInt)
wilcox.test(AI.par.survey_females$TAM,
            AI.par.survey_males$TAM)
wilcox.test(AI.par.survey_females$PercImpact33,
            AI.par.survey_males$PercImpact33)
wilcox.test(AI.par.survey_females$Limit34,
            AI.par.survey_males$Limit34)
wilcox.test(AI.par.survey_females$TechMuch46,
            AI.par.survey_males$TechMuch46)
wilcox.test(AI.par.survey_females$PrivacyConc47,
            AI.par.survey_males$PrivacyConc47)

#Education mean/sd
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$education.3lvl),
          mean,na.rm=TRUE)
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$education.3lvl),
          sd,na.rm=TRUE)
#Education Parent Prob Tech: ANOVA 
  #boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="ParentProbTech")
  #fit model
result_edu.ppt<-aov(ParentProbTech~education.2lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.ppt)
  #tukey
TukeyHSD(result_edu.ppt)
  #model diagtnostics
    #normality test
resid_edu.ppt<-residuals(result_edu.ppt)
rs_edu.ppt<-rstandard(result_edu.ppt)
fits_edu.ppt<-fitted.values(result_edu.ppt)
qqnorm(rs_edu.ppt)
qqline(rs_edu.ppt,col=2)
shapiro.test(rs_edu.ppt) #p<.01 failed normality
    #equal variances test
plot(fits_edu.ppt,rs_edu.ppt,
     main="residuals vs. fitted values")
leveneTest(ParentProbTech~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.54 pass equal variances
    #independence test 
plot(rs_edu.ppt,type="l",main="run order plot") #looks good
#Education Parent Prob Tech: Kruskal Wallis (nonparametric)
kruskal.test(ParentProbTech~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education Technof: ANOVA
  #boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="NumDevInt")
  #fit model
result_edu.tech<-aov(NumDevInt~education.3lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.tech)
  #tukey
TukeyHSD(result_edu.tech)
    #model diagtnostics
    #normality test
resid_edu.tech<-residuals(result_edu.tech)
rs_edu.tech<-rstandard(result_edu.tech)
fits_edu.tech<-fitted.values(result_edu.tech)
qqnorm(rs_edu.tech)
qqline(rs_edu.tech,col=2)
shapiro.test(rs_edu.tech) #p<.01 failed normality
    #equal variances test
plot(fits_edu.tech,rs_edu.tech,
     main="residuals vs. fitted values")
leveneTest(NumDevInt~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.56 pass equal variances
    #independence test 
plot(rs_edu.tech,type="l",main="run order plot") #looks good
#Education Techno: Kruskal Wallis (nonparametric)
kruskal.test(NumDevInt~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education TAM: ANOVA
  #boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="TAM")
#fit model
result_edu.TAM<-aov(TAM~education.3lvl,
                     data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TAM)
#tukey
TukeyHSD(result_edu.TAM)
#model diagtnostics
#normality test
resid_edu.TAM<-residuals(result_edu.TAM)
rs_edu.TAM<-rstandard(result_edu.TAM)
fits_edu.TAM<-fitted.values(result_edu.TAM)
qqnorm(rs_edu.TAM)
qqline(rs_edu.TAM,col=2)
shapiro.test(rs_edu.TAM) #p<.01 failed normality
#equal variances test
plot(fits_edu.TAM,rs_edu.TAM,
     main="residuals vs. fitted values")
leveneTest(TAM~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.02 fail equal variances
#independence test 
plot(rs_edu.TAM,type="l",main="run order plot") #looks good
#Education TAM: Kruskal Wallis (nonparametric)
kruskal.test(TAM~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education PercImpact33: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="PercImpact33")
#fit model
result_edu.PercImp<-aov(PercImpact33~education.3lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.PercImp)
#tukey
TukeyHSD(result_edu.PercImp)
#model diagtnostics
#normality test
resid_edu.PercImp<-residuals(result_edu.PercImp)
rs_edu.PercImp<-rstandard(result_edu.PercImp)
fits_edu.PercImp<-fitted.values(result_edu.PercImp)
qqnorm(rs_edu.PercImp)
qqline(rs_edu.PercImp,col=2)
shapiro.test(rs_edu.PercImp) #p<.01 failed normality
#equal variances test
plot(fits_edu.PercImp,rs_edu.PercImp,
     main="residuals vs. fitted values")
leveneTest(PercImpact33~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #fail equal variances
#independence test 
plot(rs_edu.PercImp,type="l",main="run order plot") #looks good
#Education PercImpact33: Kruskal Wallis (nonparametric)
kruskal.test(PercImpact33~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education Limit34: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="Limit34")
#fit model
result_edu.Limit<-aov(Limit34~education.3lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.Limit)
#tukey
TukeyHSD(result_edu.Limit)
#model diagtnostics
#normality test
resid_edu.Limit<-residuals(result_edu.Limit)
rs_edu.Limit<-rstandard(result_edu.Limit)
fits_edu.Limit<-fitted.values(result_edu.Limit)
qqnorm(rs_edu.Limit)
qqline(rs_edu.Limit,col=2)
shapiro.test(rs_edu.Limit) #p<.01 failed normality
#equal variances test
plot(fits_edu.Limit,rs_edu.Limit,
     main="residuals vs. fitted values")
leveneTest(Limit34~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.02 fail equal variances
#independence test 
plot(rs_edu.Limit,type="l",main="run order plot") #looks good
#Education Limit34: Kruskal Wallis (nonparametric)
kruskal.test(Limit34~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education TechMuch: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="TechMuch46")
#fit model
result_edu.TechMuch<-aov(TechMuch46~education.3lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TechMuch)
#tukey
TukeyHSD(result_edu.TechMuch)
#model diagtnostics
#normality test
resid_edu.TechMuch<-residuals(result_edu.TechMuch)
rs_edu.TechMuch<-rstandard(result_edu.TechMuch)
fits_edu.TechMuch<-fitted.values(result_edu.TechMuch)
qqnorm(rs_edu.TechMuch)
qqline(rs_edu.TechMuch,col=2)
shapiro.test(rs_edu.TechMuch) #p<.01 failed normality
#equal variances test
plot(fits_edu.TechMuch,rs_edu.TechMuch,
     main="residuals vs. fitted values")
leveneTest(TechMuch46~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #fail equal variances
#independence test 
plot(rs_edu.TechMuch,type="l",main="run order plot") #looks good
#Education TechMuch46: Kruskal Wallis (nonparametric)
kruskal.test(TechMuch46~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education PrivacyConc: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="PrivacyConc47")
#fit model
result_edu.PrivacyConc<-aov(PrivacyConc47~education.3lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.PrivacyConc)
#tukey
TukeyHSD(result_edu.PrivacyConc)
#model diagtnostics
#normality test
resid_edu.PrivacyConc<-residuals(result_edu.PrivacyConc)
rs_edu.PrivacyConc<-rstandard(result_edu.PrivacyConc)
fits_edu.PrivacyConc<-fitted.values(result_edu.PrivacyConc)
qqnorm(rs_edu.PrivacyConc)
qqline(rs_edu.PrivacyConc,col=2)
shapiro.test(rs_edu.PrivacyConc) #p<.01 failed normality
#equal variances test
plot(fits_edu.PrivacyConc,rs_edu.PrivacyConc,
     main="residuals vs. fitted values")
leveneTest(PrivacyConc47~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.02 fail equal variances
#independence test 
plot(rs_edu.PrivacyConc,type="l",main="run order plot") #looks good
#Education PrivacyConc47: Kruskal Wallis (nonparametric)
kruskal.test(PrivacyConc47~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Race mean/sd
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$race4.lvl),
          mean,na.rm=TRUE)
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$race4.lvl),
          sd,na.rm=TRUE)
#Race Parent Prob Tech: ANOVA 
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="race4.lvl",
          y="ParentProbTech")
#fit model
result_edu.ppt<-aov(ParentProbTech~race4.lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.ppt)
#tukey
TukeyHSD(result_edu.ppt)
#model diagtnostics
#normality test
resid_edu.ppt<-residuals(result_edu.ppt)
rs_edu.ppt<-rstandard(result_edu.ppt)
fits_edu.ppt<-fitted.values(result_edu.ppt)
qqnorm(rs_edu.ppt)
qqline(rs_edu.ppt,col=2)
shapiro.test(rs_edu.ppt) #p<.01 failed normality
#equal variances test
plot(fits_edu.ppt,rs_edu.ppt,
     main="residuals vs. fitted values")
leveneTest(ParentProbTech~race4.lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.01 fail equal variances
#independence test 
plot(rs_edu.ppt,type="l",main="run order plot") #looks good
#Race Parent Prob Tech: Kruskal Wallis (nonparametric)
kruskal.test(ParentProbTech~race4.lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Race Technof: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="race4.lvl",
          y="NumDevInt")
#fit model
result_edu.tech<-aov(NumDevInt~race4.lvl,
                     data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.tech)
#tukey
TukeyHSD(result_edu.tech)
#model diagtnostics
#normality test
resid_edu.tech<-residuals(result_edu.tech)
rs_edu.tech<-rstandard(result_edu.tech)
fits_edu.tech<-fitted.values(result_edu.tech)
qqnorm(rs_edu.tech)
qqline(rs_edu.tech,col=2)
shapiro.test(rs_edu.tech) #p<.01 failed normality
#equal variances test
plot(fits_edu.tech,rs_edu.tech,
     main="residuals vs. fitted values")
leveneTest(NumDevInt~race4.lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.88 pass equal variances
#independence test 
plot(rs_edu.tech,type="l",main="run order plot") #looks good
#Race Techno: Kruskal Wallis (nonparametric)
kruskal.test(NumDevInt~race4.lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Race TAM: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="race4.lvl",
          y="TAM")
#fit model
result_edu.TAM<-aov(TAM~race4.lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TAM)
#tukey
TukeyHSD(result_edu.TAM)
#model diagtnostics
#normality test
resid_edu.TAM<-residuals(result_edu.TAM)
rs_edu.TAM<-rstandard(result_edu.TAM)
fits_edu.TAM<-fitted.values(result_edu.TAM)
qqnorm(rs_edu.TAM)
qqline(rs_edu.TAM,col=2)
shapiro.test(rs_edu.TAM) #p<.01 failed normality
#equal variances test
plot(fits_edu.TAM,rs_edu.TAM,
     main="residuals vs. fitted values")
leveneTest(TAM~race4.lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.33 pass equal variances
#independence test 
plot(rs_edu.TAM,type="l",main="run order plot") #looks good
#Race TAM: Kruskal Wallis (nonparametric)
kruskal.test(TAM~race4.lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Race PercImpact33: ANOVA 
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="race4.lvl",
          y="PercImpact33")
#fit model
result_edu.percimp<-aov(PercImpact33~race4.lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.percimp)
#tukey
TukeyHSD(result_edu.percimp)
#model diagtnostics
#normality test
resid_edu.percimp<-residuals(result_edu.percimp)
rs_edu.percimp<-rstandard(result_edu.percimp)
fits_edu.percimp<-fitted.values(result_edu.percimp)
qqnorm(rs_edu.percimp)
qqline(rs_edu.percimp,col=2)
shapiro.test(rs_edu.percimp) #p<.01 failed normality
#equal variances test
plot(fits_edu.percimp,rs_edu.percimp,
     main="residuals vs. fitted values")
leveneTest(PercImpact33~race4.lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.01 fail equal variances
#independence test 
plot(rs_edu.percimp,type="l",main="run order plot") #looks good
#Race Parent Prob Tech: Kruskal Wallis (nonparametric)
kruskal.test(PercImpact33~race4.lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Race Limit34: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="race4.lvl",
          y="Limit34")
#fit model
result_edu.limit<-aov(Limit34~race4.lvl,
                     data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.limit)
#tukey
TukeyHSD(result_edu.limit)
#model diagtnostics
#normality test
resid_edu.limit<-residuals(result_edu.limit)
rs_edu.limit<-rstandard(result_edu.limit)
fits_edu.limit<-fitted.values(result_edu.limit)
qqnorm(rs_edu.limit)
qqline(rs_edu.limit,col=2)
shapiro.test(rs_edu.limit) #p<.01 failed normality
#equal variances test
plot(fits_edu.limit,rs_edu.limit,
     main="residuals vs. fitted values")
leveneTest(Limit34~race4.lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.88 pass equal variances
#independence test 
plot(rs_edu.limit,type="l",main="run order plot") #looks good
#Race Limit: Kruskal Wallis (nonparametric)
kruskal.test(Limit34~race4.lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Race TechMuch: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="race4.lvl",
          y="TechMuch46")
#fit model
result_edu.TechMuch<-aov(TechMuch46~race4.lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TechMuch)
#tukey
TukeyHSD(result_edu.TechMuch)
#model diagtnostics
#normality test
resid_edu.TechMuch<-residuals(result_edu.TechMuch)
rs_edu.TechMuch<-rstandard(result_edu.TechMuch)
fits_edu.TechMuch<-fitted.values(result_edu.TechMuch)
qqnorm(rs_edu.TechMuch)
qqline(rs_edu.TechMuch,col=2)
shapiro.test(rs_edu.TechMuch) #p<.01 failed normality
#equal variances test
plot(fits_edu.TechMuch,rs_edu.TechMuch,
     main="residuals vs. fitted values")
leveneTest(TechMuch46~race4.lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.33 pass equal variances
#independence test 
plot(rs_edu.TechMuch,type="l",main="run order plot") #looks good
#Race TAM: Kruskal Wallis (nonparametric)
kruskal.test(TechMuch46~race4.lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Race PrivacyConc47: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="race4.lvl",
          y="PrivacyConc47")
#fit model
result_edu.PrivConc<-aov(PrivacyConc47~race4.lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.PrivConc)
#tukey
TukeyHSD(result_edu.PrivConc)
#model diagtnostics
#normality test
resid_edu.PrivConc<-residuals(result_edu.PrivConc)
rs_edu.PrivConc<-rstandard(result_edu.PrivConc)
fits_edu.PrivConc<-fitted.values(result_edu.PrivConc)
qqnorm(rs_edu.PrivConc)
qqline(rs_edu.PrivConc,col=2)
shapiro.test(rs_edu.PrivConc) #p<.01 failed normality
#equal variances test
plot(fits_edu.PrivConc,rs_edu.PrivConc,
     main="residuals vs. fitted values")
leveneTest(PrivacyConc47~race4.lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.33 pass equal variances
#independence test 
plot(rs_edu.PrivConc,type="l",main="run order plot") #looks good
#Race PrivacyConc47: Kruskal Wallis (nonparametric)
kruskal.test(PrivacyConc47~race4.lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Language mean/sd
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$language.2lvl),
          mean,na.rm=TRUE)
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$language.2lvl),
          sd,na.rm=TRUE)
#Language t-test or Mann Whitney test
#check if equal variances
AI.par.survey_english<-subset(AI_par.survey_age5_num_dur136_spv_no.ven,
                              language.2lvl=="English")
AI.par.survey_other<-subset(AI_par.survey_age5_num_dur136_spv_no.ven,
                            language.2lvl=="Other")
var.test(AI.par.survey_english$ParentProbTech,
         AI.par.survey_other$ParentProbTech) #test showed equal vars
var.test(AI.par.survey_english$NumDevInt,
         AI.par.survey_other$NumDevInt) #test showed equal vars
var.test(AI.par.survey_english$TAM,
         AI.par.survey_other$TAM) #test showed equal vars
var.test(AI.par.survey_english$PercImpact33,
         AI.par.survey_other$PercImpact33) #test showed equal vars
var.test(AI.par.survey_english$Limit34,
         AI.par.survey_other$Limit34) #test showed equal vars
var.test(AI.par.survey_english$TechMuch46,
         AI.par.survey_other$TechMuch46)
var.test(AI.par.survey_english$PrivacyConc47,
         AI.par.survey_other$PrivacyConc47)
#check if normal
shapiro.test(AI.par.survey_english$ParentProbTech) #test showed nonnormal
shapiro.test(AI.par.survey_other$ParentProbTech) #test showed normal
shapiro.test(AI.par.survey_english$NumDevInt) #test showed nonnormal
shapiro.test(AI.par.survey_other$NumDevInt) #test showed nonnormal
shapiro.test(AI.par.survey_english$TAM) #test showed nonnormal
shapiro.test(AI.par.survey_other$TAM) #test showed normal
shapiro.test(AI.par.survey_english$PercImpact33) #test showed nonnormal
shapiro.test(AI.par.survey_other$PercImpact33) #test showed normal
shapiro.test(AI.par.survey_english$Limit34) #test showed nonnormal
shapiro.test(AI.par.survey_other$Limit34) #test showed nonnormal
shapiro.test(AI.par.survey_english$TechMuch46) #test showed nonnormal
shapiro.test(AI.par.survey_other$TechMuch46) #test showed nonnormal
shapiro.test(AI.par.survey_english$PrivacyConc47) #test showed nonnormal
shapiro.test(AI.par.survey_other$PrivacyConc47) #test showed nonnormal
#compute Mann-Whitney test since data are non-normal
wilcox.test(AI.par.survey_english$ParentProbTech,
            AI.par.survey_other$ParentProbTech)
wilcox.test(AI.par.survey_english$NumDevInt,
            AI.par.survey_other$NumDevInt)
wilcox.test(AI.par.survey_english$TAM,
            AI.par.survey_other$TAM)
wilcox.test(AI.par.survey_english$PercImpact33,
            AI.par.survey_other$PercImpact33)
wilcox.test(AI.par.survey_english$Limit34,
            AI.par.survey_other$Limit34)
wilcox.test(AI.par.survey_english$TechMuch46,
            AI.par.survey_other$TechMuch46)
wilcox.test(AI.par.survey_english$PrivacyConc47,
            AI.par.survey_other$PrivacyConc47)

#Income mean/sd
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$Income68.5lvl),
          mean,na.rm=TRUE)
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$Income68.5lvl),
          sd,na.rm=TRUE)
#Income Parent Prob Tech: ANOVA 
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="Income68.5lvl",
          y="ParentProbTech")
#fit model
result_edu.ppt<-aov(ParentProbTech~Income68.5lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.ppt)
#tukey
TukeyHSD(result_edu.ppt)
#model diagtnostics
#normality test
resid_edu.ppt<-residuals(result_edu.ppt)
rs_edu.ppt<-rstandard(result_edu.ppt)
fits_edu.ppt<-fitted.values(result_edu.ppt)
qqnorm(rs_edu.ppt)
qqline(rs_edu.ppt,col=2)
shapiro.test(rs_edu.ppt) #p<.01 failed normality
#equal variances test
plot(fits_edu.ppt,rs_edu.ppt,
     main="residuals vs. fitted values")
leveneTest(ParentProbTech~Income68.5lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.06 pass equal variances
#independence test 
plot(rs_edu.ppt,type="l",main="run order plot") #looks good
#Income Parent Prob Tech: Kruskal Wallis (nonparametric)
kruskal.test(ParentProbTech~Income68.5lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Income Technof: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="Income68.5lvl",
          y="NumDevInt")
#fit model
result_edu.tech<-aov(NumDevInt~Income68.5lvl,
                     data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.tech)
#tukey
TukeyHSD(result_edu.tech)
#model diagtnostics
#normality test
resid_edu.tech<-residuals(result_edu.tech)
rs_edu.tech<-rstandard(result_edu.tech)
fits_edu.tech<-fitted.values(result_edu.tech)
qqnorm(rs_edu.tech)
qqline(rs_edu.tech,col=2)
shapiro.test(rs_edu.tech) #p<.01 failed normality
#equal variances test
plot(fits_edu.tech,rs_edu.tech,
     main="residuals vs. fitted values")
leveneTest(NumDevInt~Income68.5lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.31 pass equal variances
#independence test 
plot(rs_edu.tech,type="l",main="run order plot") #looks good
#Income Techno: Kruskal Wallis (nonparametric)
kruskal.test(NumDevInt~Income68.5lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Income TAM: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="Income68.5lvl",
          y="TAM")
#fit model
result_edu.TAM<-aov(TAM~Income68.5lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TAM)
#tukey
TukeyHSD(result_edu.TAM)
#model diagtnostics
#normality test
resid_edu.TAM<-residuals(result_edu.TAM)
rs_edu.TAM<-rstandard(result_edu.TAM)
fits_edu.TAM<-fitted.values(result_edu.TAM)
qqnorm(rs_edu.TAM)
qqline(rs_edu.TAM,col=2)
shapiro.test(rs_edu.TAM) #p<.01 failed normality
#equal variances test
plot(fits_edu.TAM,rs_edu.TAM,
     main="residuals vs. fitted values")
leveneTest(TAM~Income68.5lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.21 pass equal variances
#independence test 
plot(rs_edu.TAM,type="l",main="run order plot") #looks good
#Income TAM: Kruskal Wallis (nonparametric)
kruskal.test(TAM~Income68.5lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Income PercImpact33: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="Income68.5lvl",
          y="PercImpact33")
#fit model
result_edu.PercImp<-aov(PercImpact33~Income68.5lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.PercImp)
#tukey
TukeyHSD(result_edu.PercImp)
#model diagtnostics
#normality test
resid_edu.PercImp<-residuals(result_edu.PercImp)
rs_edu.PercImp<-rstandard(result_edu.PercImp)
fits_edu.PercImp<-fitted.values(result_edu.PercImp)
qqnorm(rs_edu.PercImp)
qqline(rs_edu.PercImp,col=2)
shapiro.test(rs_edu.PercImp) #p<.01 failed normality
#equal variances test
plot(fits_edu.PercImp,rs_edu.PercImp,
     main="residuals vs. fitted values")
leveneTest(PercImpact33~Income68.5lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.21 pass equal variances
#independence test 
plot(rs_edu.PercImp,type="l",main="run order plot") #looks good
#Income PercImpact33: Kruskal Wallis (nonparametric)
kruskal.test(PercImpact33~Income68.5lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Income Limit34: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="Income68.5lvl",
          y="Limit34")
#fit model
result_edu.Limit34<-aov(Limit34~Income68.5lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.Limit34)
#tukey
TukeyHSD(result_edu.Limit34)
#model diagtnostics
#normality test
resid_edu.Limit34<-residuals(result_edu.Limit34)
rs_edu.Limit34<-rstandard(result_edu.Limit34)
fits_edu.Limit34<-fitted.values(result_edu.Limit34)
qqnorm(rs_edu.Limit34)
qqline(rs_edu.Limit34,col=2)
shapiro.test(rs_edu.Limit34) #p<.01 failed normality
#equal variances test
plot(fits_edu.Limit34,rs_edu.Limit34,
     main="residuals vs. fitted values")
leveneTest(Limit34~Income68.5lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.21 pass equal variances
#independence test 
plot(rs_edu.Limit34,type="l",main="run order plot") #looks good
#Income TAM: Kruskal Wallis (nonparametric)
kruskal.test(Limit34~Income68.5lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Income TechMuch46: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="Income68.5lvl",
          y="TechMuch46")
#fit model
result_edu.TechMuch<-aov(TechMuch46~Income68.5lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TechMuch)
#tukey
TukeyHSD(result_edu.TechMuch)
#model diagtnostics
#normality test
resid_edu.TechMuch<-residuals(result_edu.TechMuch)
rs_edu.TechMuch<-rstandard(result_edu.TechMuch)
fits_edu.TechMuch<-fitted.values(result_edu.TechMuch)
qqnorm(rs_edu.TechMuch)
qqline(rs_edu.TechMuch,col=2)
shapiro.test(rs_edu.TechMuch) #p<.01 failed normality
#equal variances test
plot(fits_edu.TechMuch,rs_edu.TechMuch,
     main="residuals vs. fitted values")
leveneTest(TechMuch46~Income68.5lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.21 pass equal variances
#independence test 
plot(rs_edu.TechMuch,type="l",main="run order plot") #looks good
#Income TAM: Kruskal Wallis (nonparametric)
kruskal.test(TechMuch46~Income68.5lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Income PrivacyConc47: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="Income68.5lvl",
          y="PrivacyConc47")
#fit model
result_edu.PrivConc<-aov(PrivacyConc47~Income68.5lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.PrivConc)
#tukey
TukeyHSD(result_edu.PrivConc)
#model diagtnostics
#normality test
resid_edu.PrivConc<-residuals(result_edu.PrivConc)
rs_edu.PrivConc<-rstandard(result_edu.PrivConc)
fits_edu.PrivConc<-fitted.values(result_edu.PrivConc)
qqnorm(rs_edu.PrivConc)
qqline(rs_edu.PrivConc,col=2)
shapiro.test(rs_edu.PrivConc) #p<.01 failed normality
#equal variances test
plot(fits_edu.PrivConc,rs_edu.PrivConc,
     main="residuals vs. fitted values")
leveneTest(PrivacyConc47~Income68.5lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.21 pass equal variances
#independence test 
plot(rs_edu.PrivConc,type="l",main="run order plot") #looks good
#Income TAM: Kruskal Wallis (nonparametric)
kruskal.test(PrivacyConc47~Income68.5lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)


#Number of Children mean/sd
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$numberchild.2lvl),
          mean,na.rm=TRUE)
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$numberchild.2lvl),
          sd,na.rm=TRUE)
#Number of Children t-test or Mann Whitney test
#check if equal variances
AI.par.survey_one<-subset(AI_par.survey_age5_num_dur136_spv_no.ven,
                              numberchild.2lvl=="1")
AI.par.survey_morethanone<-subset(AI_par.survey_age5_num_dur136_spv_no.ven,
                            numberchild.2lvl==">1")
var.test(AI.par.survey_one$ParentProbTech,
         AI.par.survey_morethanone$ParentProbTech) #test showed equal vars
var.test(AI.par.survey_one$NumDevInt,
         AI.par.survey_morethanone$NumDevInt) #test showed equal vars
var.test(AI.par.survey_one$TAM,
         AI.par.survey_morethanone$TAM) #test showed equal vars
var.test(AI.par.survey_one$PercImpact33,
         AI.par.survey_morethanone$PercImpact33) #test showed equal vars
var.test(AI.par.survey_one$Limit34,
         AI.par.survey_morethanone$Limit34) #test showed equal vars
var.test(AI.par.survey_one$TechMuch46,
         AI.par.survey_morethanone$TechMuch46)
var.test(AI.par.survey_one$PrivacyConc47,
         AI.par.survey_morethanone$PrivacyConc47)
#check if normal
shapiro.test(AI.par.survey_one$ParentProbTech) #test showed nonnormal
shapiro.test(AI.par.survey_morethanone$ParentProbTech) #test showed nonnormal
shapiro.test(AI.par.survey_one$NumDevInt) #test showed nonnormal
shapiro.test(AI.par.survey_morethanone$NumDevInt) #test showed nonnormal
shapiro.test(AI.par.survey_one$TAM) #test showed nonnormal
shapiro.test(AI.par.survey_morethanone$TAM) #test showed nonnormal
shapiro.test(AI.par.survey_one$PercImpact33) #test showed nonnormal
shapiro.test(AI.par.survey_morethanone$PercImpact33) #test showed nonnormal
shapiro.test(AI.par.survey_one$Limit34) #test showed nonnormal
shapiro.test(AI.par.survey_morethanone$Limit34) #test showed nonnormal
shapiro.test(AI.par.survey_one$TechMuch46) #test showed nonnormal
shapiro.test(AI.par.survey_morethanone$TechMuch46)
shapiro.test(AI.par.survey_one$PrivacyConc47) #test showed nonnormal
shapiro.test(AI.par.survey_morethanone$PrivacyConc47)
#compute Mann-Whitney test since data are non-normal
wilcox.test(AI.par.survey_one$ParentProbTech,
            AI.par.survey_morethanone$ParentProbTech)
wilcox.test(AI.par.survey_one$NumDevInt,
            AI.par.survey_morethanone$NumDevInt)
wilcox.test(AI.par.survey_one$TAM,
            AI.par.survey_morethanone$TAM)
wilcox.test(AI.par.survey_one$PercImpact33,
            AI.par.survey_morethanone$PercImpact33)
wilcox.test(AI.par.survey_one$Limit34,
            AI.par.survey_morethanone$Limit34)
wilcox.test(AI.par.survey_one$TechMuch46,
            AI.par.survey_morethanone$TechMuch46)
wilcox.test(AI.par.survey_one$PrivacyConc47,
            AI.par.survey_morethanone$PrivacyConc47)


#Geographic mean/sd
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$cens.reg),
          mean,na.rm=TRUE)
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                              "NumDevInt",
                                              "TAM",
                                              "PercImpact33",
                                              "Limit34",
                                              "TechMuch46",
                                              "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$cens.reg),
          sd,na.rm=TRUE)
#Geographic Parent Prob Tech: ANOVA 
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="cens.reg",
          y="ParentProbTech")
#fit model
result_edu.ppt<-aov(ParentProbTech~cens.reg,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.ppt)
#tukey
TukeyHSD(result_edu.ppt)
#model diagtnostics
#normality test
resid_edu.ppt<-residuals(result_edu.ppt)
rs_edu.ppt<-rstandard(result_edu.ppt)
fits_edu.ppt<-fitted.values(result_edu.ppt)
qqnorm(rs_edu.ppt)
qqline(rs_edu.ppt,col=2)
shapiro.test(rs_edu.ppt) #p<.01 failed normality
#equal variances test
plot(fits_edu.ppt,rs_edu.ppt,
     main="residuals vs. fitted values")
leveneTest(ParentProbTech~cens.reg,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.09 pass equal variances
#independence test 
plot(rs_edu.ppt,type="l",main="run order plot") #looks good
#Geographic Parent Prob Tech: Kruskal Wallis (nonparametric)
kruskal.test(ParentProbTech~as.factor(cens.reg),
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Geographic Technof: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="cens.reg",
          y="NumDevInt")
#fit model
result_edu.tech<-aov(NumDevInt~cens.reg,
                     data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.tech)
#tukey
TukeyHSD(result_edu.tech)
#model diagtnostics
#normality test
resid_edu.tech<-residuals(result_edu.tech)
rs_edu.tech<-rstandard(result_edu.tech)
fits_edu.tech<-fitted.values(result_edu.tech)
qqnorm(rs_edu.tech)
qqline(rs_edu.tech,col=2)
shapiro.test(rs_edu.tech) #p<.01 failed normality
#equal variances test
plot(fits_edu.tech,rs_edu.tech,
     main="residuals vs. fitted values")
leveneTest(NumDevInt~as.factor(cens.reg),
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.31 pass equal variances
#independence test 
plot(rs_edu.tech,type="l",main="run order plot") #looks good
#Geographic Techno: Kruskal Wallis (nonparametric)
kruskal.test(NumDevInt~as.factor(cens.reg),
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Geographic TAM: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="cens.reg",
          y="TAM")
#fit model
result_edu.TAM<-aov(TAM~cens.reg,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TAM)
#tukey
TukeyHSD(result_edu.TAM)
#model diagtnostics
#normality test
resid_edu.TAM<-residuals(result_edu.TAM)
rs_edu.TAM<-rstandard(result_edu.TAM)
fits_edu.TAM<-fitted.values(result_edu.TAM)
qqnorm(rs_edu.TAM)
qqline(rs_edu.TAM,col=2)
shapiro.test(rs_edu.TAM) #p<.01 failed normality
#equal variances test
plot(fits_edu.TAM,rs_edu.TAM,
     main="residuals vs. fitted values")
leveneTest(TAM~cens.reg,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.97 pass equal variances
#independence test 
plot(rs_edu.TAM,type="l",main="run order plot") #looks good
#Geographic TAM: Kruskal Wallis (nonparametric)
kruskal.test(TAM~as.factor(cens.reg),
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Geographic PercImpact33: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="cens.reg",
          y="PercImpact33")
#fit model
result_edu.PercImp<-aov(PercImpact33~cens.reg,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.PercImp)
#tukey
TukeyHSD(result_edu.PercImp)
#model diagtnostics
#normality test
resid_edu.PercImp<-residuals(result_edu.PercImp)
rs_edu.PercImp<-rstandard(result_edu.PercImp)
fits_edu.PercImp<-fitted.values(result_edu.PercImp)
qqnorm(rs_edu.PercImp)
qqline(rs_edu.PercImp,col=2)
shapiro.test(rs_edu.PercImp) #p<.01 failed normality
#equal variances test
plot(fits_edu.PercImp,rs_edu.PercImp,
     main="residuals vs. fitted values")
leveneTest(PercImpact33~cens.reg,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.97 pass equal variances
#independence test 
plot(rs_edu.PercImp,type="l",main="run order plot") #looks good
#Geographic TAM: Kruskal Wallis (nonparametric)
kruskal.test(PercImpact33~as.factor(cens.reg),
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Geographic Limit34: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="cens.reg",
          y="Limit34")
#fit model
result_edu.Limit<-aov(Limit34~cens.reg,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.Limit)
#tukey
TukeyHSD(result_edu.Limit)
#model diagtnostics
#normality test
resid_edu.Limit<-residuals(result_edu.Limit)
rs_edu.Limit<-rstandard(result_edu.Limit)
fits_edu.Limit<-fitted.values(result_edu.Limit)
qqnorm(rs_edu.Limit)
qqline(rs_edu.Limit,col=2)
shapiro.test(rs_edu.Limit) #p<.01 failed normality
#equal variances test
plot(fits_edu.Limit,rs_edu.Limit,
     main="residuals vs. fitted values")
leveneTest(Limit34~cens.reg,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.97 pass equal variances
#independence test 
plot(rs_edu.Limit,type="l",main="run order plot") #looks good
#Geographic TAM: Kruskal Wallis (nonparametric)
kruskal.test(Limit34~as.factor(cens.reg),
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Geographic TechMuch46: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="cens.reg",
          y="TechMuch46")
#fit model
result_edu.TechMuch<-aov(TechMuch46~cens.reg,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TechMuch)
#tukey
TukeyHSD(result_edu.TechMuch)
#model diagtnostics
#normality test
resid_edu.TechMuch<-residuals(result_edu.TechMuch)
rs_edu.TechMuch<-rstandard(result_edu.TechMuch)
fits_edu.TechMuch<-fitted.values(result_edu.TechMuch)
qqnorm(rs_edu.TechMuch)
qqline(rs_edu.TechMuch,col=2)
shapiro.test(rs_edu.TechMuch) #p<.01 failed normality
#equal variances test
plot(fits_edu.TechMuch,rs_edu.TechMuch,
     main="residuals vs. fitted values")
leveneTest(TechMuch46~cens.reg,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.97 pass equal variances
#independence test 
plot(rs_edu.TechMuch,type="l",main="run order plot") #looks good
#Geographic TAM: Kruskal Wallis (nonparametric)
kruskal.test(TechMuch46~as.factor(cens.reg),
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Geographic PrivacyConc47: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="cens.reg",
          y="PrivacyConc47")
#fit model
result_edu.PrivConc<-aov(PrivacyConc47~cens.reg,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.PrivConc)
#tukey
TukeyHSD(result_edu.PrivConc)
#model diagtnostics
#normality test
resid_edu.PrivConc<-residuals(result_edu.PrivConc)
rs_edu.PrivConc<-rstandard(result_edu.PrivConc)
fits_edu.PrivConc<-fitted.values(result_edu.PrivConc)
qqnorm(rs_edu.PrivConc)
qqline(rs_edu.PrivConc,col=2)
shapiro.test(rs_edu.PrivConc) #p<.01 failed normality
#equal variances test
plot(fits_edu.PrivConc,rs_edu.PrivConc,
     main="residuals vs. fitted values")
leveneTest(PrivacyConc47~cens.reg,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.97 pass equal variances
#independence test 
plot(rs_edu.PrivConc,type="l",main="run order plot") #looks good
#Geographic TAM: Kruskal Wallis (nonparametric)
kruskal.test(PrivacyConc47~as.factor(cens.reg),
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

             
#create table using expss
#technoference.scale_expss<-apply_labels(technoference.scale,
#                                        `Q8_1Please rate your agreement with each statement, using the following scale: - 29.\tWhen my mobile electronic device alerts me to indicate new messages, I cannot resist checking them.`=
#                                          "Rate agreement: When my mobile electronic device alerts me of a new message, I cannot resist checking them.",
#                                        `Q8_2Please rate your agreement with each statement, using the following scale: - 30.\tI often think about calls or messages I might receive on my mobile phone.`=
#                                          "Rate agreement: I often think about calls/messages I might receive on my mobile phone.",
#                                        `Q8_3Please rate your agreement with each statement, using the following scale: - 31.\tI feel like I use my mobile phone too much.`=
#                                          "Rate agreement: I feel like I use my mobile phone too much.",
#                                        `Q1455. What is your gender? - Selected Choice`="Sex",
#                                        Education.binary="Education Level")
#technoference.scale_expss %>% 
#  tab_cells(`Q8_1Please rate your agreement with each statement, using the following scale: - 29.\tWhen my mobile electronic device alerts me to indicate new messages, I cannot resist checking them.`,
#            `Q8_2Please rate your agreement with each statement, using the following scale: - 30.\tI often think about calls or messages I might receive on my mobile phone.`,
#            `Q8_3Please rate your agreement with each statement, using the following scale: - 31.\tI feel like I use my mobile phone too much.`) %>% 
#  tab_cols(total(label="#Total| |"),Education.binary,`Q1455. What is your gender? - Selected Choice`) %>%
#  tab_stat_fun(Mean=w_mean,"Std. dev."=w_sd,method=list) %>%
#  tab_pivot()

###Visualizations###
#Likert Density Plots#
likert.density.plot(likert(AI_par.survey.reals[c(15:17)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey_age5_num_dur136_spv_no.ven[c(18:20)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(21:23)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(24:27)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(28:31)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(32:35)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(36:38)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(39:42)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(43:45)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(46,47,49,51)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(48,50,51)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(52:54)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(55:58)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(61,62,66)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(59,60)]),
                    facet=TRUE)
likert.density.plot(likert(AI_par.survey.reals[c(63:65)]),
                    facet=TRUE)

###Multiple Regression
#Income is highly correlated with Education (r = .46), and 
#race is highly correlated with language (r = .46). 
#The decision was made to exclude income and race from analysis.

#isolate predictors and response var to create df for modeling
AI.Par.Surv_model.df<-AI_par.survey_age5_num_dur136_spv_no.ven[c(68,
                                                          83:85,
                                                          87,89:94)]
#see what the classes are for the predictors
lapply(AI.Par.Surv_model.df,class)
#change cens.reg to factor
AI.Par.Surv_model.df$cens.reg<-as.factor(
  AI.Par.Surv_model.df$cens.reg
)
#check modal for each factor to for assigning reference
lapply(AI.Par.Surv_model.df[-c(1:4)],table)

#specify reference variables based off highest modal
AI.Par.Surv_model.df<-within(
  AI.Par.Surv_model.df,
  cens.reg<-relevel(cens.reg,ref="South"))
AI.Par.Surv_model.df<-within(
  AI.Par.Surv_model.df,
  race4.lvl<-relevel(race4.lvl,
                     ref="White"))
AI.Par.Surv_model.df<-within(
  AI.Par.Surv_model.df,
  sex.mf<-relevel(sex.mf,
                  ref="Female"))
AI.Par.Surv_model.df<-within(
  AI.Par.Surv_model.df,
  language.2lvl<-relevel(language.2lvl,
                     ref="English"))
AI.Par.Surv_model.df<-within(
  AI.Par.Surv_model.df,
  numberchild.2lvl<-relevel(numberchild.2lvl,
                            ref=">1"))
AI.Par.Surv_model.df<-within(
  AI.Par.Surv_model.df,
  Income68.5lvl<-relevel(Income68.5lvl,
                    ref="<$25k"))
AI.Par.Surv_model.df<-within(
  AI.Par.Surv_model.df,
  education.3lvl<-relevel(education.3lvl,
                         ref="College Degree"))

#check reference value
lapply(AI.Par.Surv_model.df[-c(1:4)],contrasts)

#model fit
#Parent Prob Tech as predictor
fit_AI.Par.Surv.PPT<-lm(TAM ~ Age54 + sex.mf + education.3lvl + 
                      race.4lvl + language.2lvl + Income68.5lvl + 
                      numberchild.2lvl + ParentProbTech,
                    data=AI.Par.Surv_model.df)
summary(fit_AI.Par.Surv.PPT)
stepVIF(fit_AI.Par.Surv.PPT)
exp(cbind(ORs=coef(fit_AI.Par.Surv.PPT),
          confint(fit_AI.Par.Surv.PPT))) #odds ratios w/ CIs
anova_stats(fit_AI.Par.Surv.PPT)

#Technoference as predictor
fit_AI.Par.Surv.Techno<-lm(TAM ~ Age54 + sex.mf + education.3lvl + 
                          race.4lvl + language.2lvl + Income68.5lvl + 
                          numberchild.2lvl + cens.reg + NumDevInt,
                        data=AI.Par.Surv_model.df)
summary(fit_AI.Par.Surv.Techno)
stepVIF(fit_AI.Par.Surv.Techno)
exp(cbind(ORs=coef(fit_AI.Par.Surv.Techno),
          confint(fit_AI.Par.Surv.Techno)))

###Take two###
#after seeing the results, decide to collapse education, remove
#geography location, and combine parent prob tech and 
#technoference into same model
#collapse education
AI.Par.Surv_model.df$education.2lvl<-AI.Par.Surv_model.df$education.3lvl
levels(AI.Par.Surv_model.df$education.2lvl)<-list(
  "No C. Degree"=c("HS or less","Some College"),
  "College Degree"="College Degree"
)
AI.Par.Surv_model.df<-within(
  AI.Par.Surv_model.df,
  education.2lvl<-relevel(education.2lvl,
                          ref="No C. Degree"))
#Full Model for gaussian and gamma family models
full_AI.Par.Surv<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
                      race4.lvl + language.2lvl + Income68.5lvl +
                      numberchild.2lvl + ParentProbTech + NumDevInt,
                      data=AI.Par.Surv_model.df)
summary(full_AI.Par.Surv)
full_AI.Par.Surv_gam<-glm(TAM ~ Age54 + sex.mf + education.2lvl + 
                            race4.lvl + language.2lvl + Income68.5lvl +
                            numberchild.2lvl + ParentProbTech + NumDevInt,
                          data=AI.Par.Surv_model.df,
                          family = Gamma)
summary(full_AI.Par.Surv_gam)
#calculate variance nflation factor
stepVIF(full_AI.Par.Surv)
stepVIF(full_AI.Par.Surv_gam)

#find odds ratios
exp(coef(full_AI.Par.Surv)[3:11]) #odds ratios w/ CIs
exp(coef(full_AI.Par.Surv_gam)[3:11]) #odds ratios w/ CIs

#find eta-square
eta_sq(full_AI.Par.Surv,partial = TRUE)
#find cohen's d for continuous variables
  #PPT
t.PPT<-summary(full_AI.Par.Surv)[["coefficients"]]["ParentProbTech", 
                                                   "t value"]
t2d(t.PPT,n=269)
  #age
t.age<-summary(full_AI.Par.Surv)[["coefficients"]]["Age54", 
                                                   "t value"]
t2d(t.age,n=269)
  #technoference
t.NDI<-summary(full_AI.Par.Surv)[["coefficients"]]["NumDevInt", 
                                                   "t value"]
t2d(t.NDI,n=269)
#find p-values of categorical variables
with<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
                     race.4lvl + language.2lvl + Income68.5lvl +
                     numberchild.2lvl + ParentProbTech + NumDevInt,
                   data=AI.Par.Surv_model.df)
without<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
                      race.4lvl + language.2lvl + Income68.5lvl +
                      numberchild.2lvl + ParentProbTech + NumDevInt,
                    data=AI.Par.Surv_model.df)
anova(with,without)
#special data sets because sex and race have missing data
  #sex
AI.Par.Surv_model.df_na.omit.sex<-AI.Par.Surv_model.df %>% 
  drop_na(sex.mf)
  #race
AI.Par.Surv_model.df_na.omit.race<-AI.Par.Surv_model.df %>% 
  drop_na(race4.lvl)
#find p-value of sex
with<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
           race4.lvl + language.2lvl + Income68.5lvl +
           numberchild.2lvl + ParentProbTech + NumDevInt,
         data=AI.Par.Surv_model.df_na.omit.sex)
without<-lm(TAM ~ Age54 + education.2lvl + 
              race4.lvl + language.2lvl + Income68.5lvl +
              numberchild.2lvl + ParentProbTech + NumDevInt,
            data=AI.Par.Surv_model.df_na.omit.sex)
anova(with,without)
#find p-value of race
with<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
           race4.lvl + language.2lvl + Income68.5lvl +
           numberchild.2lvl + ParentProbTech + NumDevInt,
         data=AI.Par.Surv_model.df_na.omit.race)
without<-lm(TAM ~ Age54+ sex.mf + education.2lvl + 
              language.2lvl + Income68.5lvl +
              numberchild.2lvl + ParentProbTech + NumDevInt,
            data=AI.Par.Surv_model.df_na.omit.race)
anova(with,without)

#regression model where PPT is the response variable
fit_AI.Par.Surv_noTAM<-lm(ParentProbTech ~ Age54 + sex.mf + 
                            education.2lvl + race4.lvl + 
                            language.2lvl + Income68.5lvl +
                            numberchild.2lvl,
                          data=AI.Par.Surv_model.df)
summary(fit_AI.Par.Surv_noTAM)

###Take 3
#remove predictors non-significant in bivariate analysis
#fit model
fit.red_AI.Par.Surv<-lm(TAM ~ sex.mf + race4.lvl + language.2lvl + 
                          ParentProbTech + NumDevInt,
                        data=AI.Par.Surv_model.df)
summary(fit.red_AI.Par.Surv)
#calculate variance nflation factor
stepVIF(fit.red_AI.Par.Surv)
#find odds ratios
exp(coef(fit.red_AI.Par.Surv)) #odds ratios w/ CIs
#find eta-square
eta_sq(fit.red_AI.Par.Surv,partial = TRUE)
#special data sets because sex and race have missing data
AI.Par.Surv_model.df_na.omit.race<-AI.Par.Surv_model.df %>% 
  drop_na(race4.lvl)
with<-lm(TAM ~ sex.mf + race4.lvl + language.2lvl + 
           ParentProbTech + NumDevInt,
         data=AI.Par.Surv_model.df_na.omit.race)
without<-lm(TAM ~ sex.mf + language.2lvl + 
              ParentProbTech + NumDevInt,
            data=AI.Par.Surv_model.df_na.omit.race)
anova(with,without)

###tAKE 4 without PPT
#fit model
fit_AI.Par.Surv<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
                      race4.lvl + language.2lvl + Income68.5lvl +
                      numberchild.2lvl + NumDevInt,
                    data=AI.Par.Surv_model.df)
summary(fit_AI.Par.Surv)
#calculate variance nflation factor
stepVIF(fit_AI.Par.Surv)
#find odds ratios
exp(coef(fit_AI.Par.Surv)) #odds ratios w/ CIs
#find eta-square
eta_sq(fit_AI.Par.Surv,partial = TRUE)
#find cohen's d for continuous variables
#age has a t-statistic of -1.22
t2d(-1.22,n=269)
#technoference has a t-statistic of 6.61
t2d(6.61,n=269)
#find p-values of categorical variables
with<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
           race.4lvl + language.2lvl + Income68.5lvl +
           numberchild.2lvl + NumDevInt,
         data=AI.Par.Surv_model.df)
without<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
              race.4lvl + language.2lvl + 
              numberchild.2lvl + NumDevInt,
            data=AI.Par.Surv_model.df)
anova(with,without)
#special data sets because sex and race have missing data
#sex
AI.Par.Surv_model.df_na.omit.sex<-AI.Par.Surv_model.df %>% 
  drop_na(sex.mf)
#race
AI.Par.Surv_model.df_na.omit.race<-AI.Par.Surv_model.df %>% 
  drop_na(race4.lvl)
#find p-value of sex
with<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
           race4.lvl + language.2lvl + Income68.5lvl +
           numberchild.2lvl + ParentProbTech + NumDevInt,
         data=AI.Par.Surv_model.df_na.omit.sex)
without<-lm(TAM ~ Age54 + education.2lvl + 
              race4.lvl + language.2lvl + Income68.5lvl +
              numberchild.2lvl + ParentProbTech + NumDevInt,
            data=AI.Par.Surv_model.df_na.omit.sex)
anova(with,without)
#find p-value of race
with<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
           race4.lvl + language.2lvl + Income68.5lvl +
           numberchild.2lvl + ParentProbTech + NumDevInt,
         data=AI.Par.Surv_model.df_na.omit.race)
without<-lm(TAM ~ Age54+ sex.mf + education.2lvl + 
              language.2lvl + Income68.5lvl +
              numberchild.2lvl + ParentProbTech + NumDevInt,
            data=AI.Par.Surv_model.df_na.omit.race)
anova(with,without)

###take 5: reduce model (only predictors significant to TAM 
#at p<.15 in bivariate analysis)
red_AI.Par.Surv<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
                       Income68.5lvl + ParentProbTech + NumDevInt,
                     data=AI.Par.Surv_model.df)
summary(red_AI.Par.Surv)
red_AI.Par.Surv_gam<-glm(TAM ~ Age54 + sex.mf + education.2lvl + 
                      Income68.5lvl + ParentProbTech + NumDevInt,
                    data=AI.Par.Surv_model.df,
                    family=Gamma)
summary(red_AI.Par.Surv_gam)
#calculate variance nflation factor
stepVIF(red_AI.Par.Surv)
#find odds ratios
exp(coef(red_AI.Par.Surv)) #odds ratios w/ CIs
exp(confint(red_AI.Par.Surv))
#find eta-square
eta_sq(red_AI.Par.Surv,partial = TRUE)
#find cohen's d for continuous variables
#PPT
t.PPT<-summary(red_AI.Par.Surv)[["coefficients"]]["ParentProbTech", 
                                                   "t value"]
t2d(t.PPT,n=269)
#age
t.age<-summary(red_AI.Par.Surv)[["coefficients"]]["Age54", 
                                                   "t value"]
t2d(t.age,n=269)
#technoference
t.NDI<-summary(red_AI.Par.Surv)[["coefficients"]]["NumDevInt", 
                                                   "t value"]
t2d(t.NDI,n=269)
#find p-values of categorical variables
with<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
           Income68.5lvl + ParentProbTech + NumDevInt,
         data=AI.Par.Surv_model.df)
summary(red_AI.Par.Surv)
without<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
              ParentProbTech + NumDevInt,
            data=AI.Par.Surv_model.df)
summary(red_AI.Par.Surv)
anova(with,without)
#special data sets because sex and race have missing data
#sex
AI.Par.Surv_model.df_na.omit.sex<-AI.Par.Surv_model.df %>% 
  drop_na(sex.mf)
#race
AI.Par.Surv_model.df_na.omit.race<-AI.Par.Surv_model.df %>% 
  drop_na(race4.lvl)
#find p-value of sex
with<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
           race4.lvl + language.2lvl + Income68.5lvl +
           numberchild.2lvl + ParentProbTech + NumDevInt,
         data=AI.Par.Surv_model.df_na.omit.sex)
without<-lm(TAM ~ Age54 + education.2lvl + 
              race4.lvl + language.2lvl + Income68.5lvl +
              numberchild.2lvl + ParentProbTech + NumDevInt,
            data=AI.Par.Surv_model.df_na.omit.sex)
anova(with,without)
#find p-value of race
with<-lm(TAM ~ Age54 + sex.mf + education.2lvl + 
           race4.lvl + language.2lvl + Income68.5lvl +
           numberchild.2lvl + ParentProbTech + NumDevInt,
         data=AI.Par.Surv_model.df_na.omit.race)
without<-lm(TAM ~ Age54+ sex.mf + education.2lvl + 
              language.2lvl + Income68.5lvl +
              numberchild.2lvl + ParentProbTech + NumDevInt,
            data=AI.Par.Surv_model.df_na.omit.race)
anova(with,without)


###Quadrant Analysis###
#Plot the scales
#PPT
ggplot(AI.Par.Surv_model.df_na.omit.race,
       aes(x = ParentProbTech,y=TAM)) + 
  geom_point(aes(size=Age54,shape=race4.lvl,
                 fill=NumDevInt),
             na.rm = TRUE) + 
  scale_shape_manual(values=c(21,22,23,24)) + 
  scale_fill_gradient(low = "red",
                       high = "white", 
                       space = "Lab") +
  geom_hline(yintercept = median(
    AI.Par.Surv_model.df_na.omit.race$TAM)) + 
  geom_vline(xintercept = median(
    AI.Par.Surv_model.df_na.omit.race$ParentProbTech)) + 
  labs(size="Age",shape="Race",fill="Parenting\nTechnoference",
       x="Problematic\nTechnology Use",y="Technology\nAcceptance")
#NDI
ggplot(AI.Par.Surv_model.df,
       aes(x = NumDevInt,y=TAM)) + 
  geom_point(aes(shape=language.2lvl,
                 size=education.2lvl,
                 fill=ParentProbTech),
              na.rm = TRUE) + 
  scale_shape_manual(values=c(21,22)) +
  scale_fill_gradient(low = "red",
                      high = "white", 
                      space = "Lab") +
  geom_hline(yintercept = median(
    AI.Par.Surv_model.df$TAM)) + 
  geom_vline(xintercept = median(
    AI.Par.Surv_model.df$NumDevInt)) + 
  labs(fill="Tech Use (General)",shape="Language",size="Education",
       x="Technoference") +
  guides(fill = guide_legend(override.aes=list(shape=21))) #fixes ggplot2 bug

#Find the subjects
AI_par.survey_age5_num_dur136_spv_no.ven$education.2lvl<-AI.Par.Surv_model.df$education.2lvl
#TAM
#top half
AI_par.survey_topTAM<-AI_par.survey_age5_num_dur136_spv_no.ven[
  AI_par.survey_age5_num_dur136_spv_no.ven$TAM >= quantile(
    AI_par.survey_age5_num_dur136_spv_no.ven$TAM, 0.50), ]
#bottom
AI_par.survey_bottomTAM<-AI_par.survey_age5_num_dur136_spv_no.ven[
  AI_par.survey_age5_num_dur136_spv_no.ven$TAM < quantile(
    AI_par.survey_age5_num_dur136_spv_no.ven$TAM, 0.50), ]
#Parent Prob Tech
#top
AI_par.survey_topParProbTech<-AI_par.survey_age5_num_dur136_spv_no.ven[
  AI_par.survey_age5_num_dur136_spv_no.ven$ParentProbTech >= quantile(
    AI_par.survey_age5_num_dur136_spv_no.ven$ParentProbTech, 0.50), ]
#bottom
AI_par.survey_bottomParProbTech<-AI_par.survey_age5_num_dur136_spv_no.ven[
  AI_par.survey_age5_num_dur136_spv_no.ven$ParentProbTech < quantile(
    AI_par.survey_age5_num_dur136_spv_no.ven$ParentProbTech, 0.50), ]
#Technoference Scale
#top
AI_par.survey_topTechnofere<-AI_par.survey_age5_num_dur136_spv_no.ven[
  AI_par.survey_age5_num_dur136_spv_no.ven$NumDevInt >= quantile(
    AI_par.survey_age5_num_dur136_spv_no.ven$NumDevInt, 0.50), ]
#bottom
AI_par.survey_bottomTechnofere<-AI_par.survey_age5_num_dur136_spv_no.ven[
  AI_par.survey_age5_num_dur136_spv_no.ven$NumDevInt < quantile(
    AI_par.survey_age5_num_dur136_spv_no.ven$NumDevInt, 0.50), ]

#Intersect to find them
topTAMtopTechno<-intersect(AI_par.survey_topTAM,
                           AI_par.survey_topTechnofere)
topTAMtopParProbTech<-intersect(AI_par.survey_topParProbTech,
                                AI_par.survey_topTAM)
topTAMbottomTechno<-intersect(AI_par.survey_topTAM,
                              AI_par.survey_bottomTechnofere)
topTAMbottomParProbTech<-intersect(AI_par.survey_topTAM,
                                   AI_par.survey_bottomParProbTech)
bottomTAMtopTechno<-intersect(AI_par.survey_bottomTAM,
                              AI_par.survey_topTechnofere)
bottomTAMtopParProbTech<-intersect(AI_par.survey_bottomTAM,
                                   AI_par.survey_topParProbTech)
bottomTAMbottomTechno<-intersect(AI_par.survey_bottomTAM,
                                 AI_par.survey_bottomTechnofere)
bottomTAMbottomParProbTech<-intersect(AI_par.survey_bottomTAM,
                                      AI_par.survey_bottomParProbTech)

#describe the above quartiles
###Table One###
table1.df<-data.frame(TAM=topTAMtopTechno$TAM,
                      Technofere=topTAMtopTechno$NumDevInt,
                      ParProbTech=topTAMtopTechno$ParentProbTech,
                      Age=topTAMtopTechno$Age54,
                      Gender=topTAMtopTechno$sex.mf,
                      Race=topTAMtopTechno$race4.lvl,
                      Language=topTAMtopTechno$language.2lvl,
                      Number_of_Children=topTAMtopTechno$numberchild.2lvl,
                      Education=topTAMtopTechno$education.3lvl,
                      Education2=topTAMtopTechno$education.2lvl,
                      Income=topTAMtopTechno$Income68.5lvl)
list.vars<-c("TAM","Technofere","ParProbTech","Age","Gender","Race",
             "Language",
             "Number_of_Children",
             "Education","Education2","Income")
cat.vars<-c("Gender","Race","Language",
            "Education","Education2","Income")
table1<-CreateTableOne(vars=list.vars,
                       data = table1.df,
                       factorVars = cat.vars,
                       includeNA = TRUE,
                       test=FALSE)
kableone(table1)
###Table One###
table1.df<-data.frame(TAM=topTAMbottomTechno$TAM,
                      Technofere=topTAMbottomTechno$NumDevInt,
                      ParProbTech=topTAMbottomTechno$ParentProbTech,
                      Age=topTAMbottomTechno$Age54,
                      Gender=topTAMbottomTechno$sex.mf,
                      Race=topTAMbottomTechno$race4.lvl,
                      Language=topTAMbottomTechno$language.2lvl,
                      Number_of_Children=topTAMbottomTechno$numberchild.2lvl,
                      Education=topTAMbottomTechno$education.3lvl,
                      Income=topTAMbottomTechno$Income68.5lvl)
list.vars<-c("TAM","Technofere","ParProbTech","Age","Gender","Race",
             "Language",
             "Number_of_Children",
             "Education","Income")
cat.vars<-c("Gender","Race","Language",
            "Education")
table1<-CreateTableOne(vars=list.vars,
                       data = table1.df,
                       factorVars = cat.vars,
                       includeNA = TRUE,
                       test=FALSE)
kableone(table1)
###Table One###
table1.df<-data.frame(TAM=bottomTAMtopTechno$TAM,
                      Technofere=bottomTAMtopTechno$NumDevInt,
                      ParProbTech=bottomTAMtopTechno$ParentProbTech,
                      Age=bottomTAMtopTechno$Age54,
                      Gender=bottomTAMtopTechno$sex.mf,
                      Race=bottomTAMtopTechno$race4.lvl,
                      Language=bottomTAMtopTechno$language.2lvl,
                      Number_of_Children=bottomTAMtopTechno$numberchild.2lvl,
                      Education=bottomTAMtopTechno$education.3lvl,
                      Income=bottomTAMtopTechno$Income68.5lvl)
list.vars<-c("TAM","Technofere","ParProbTech","Age","Gender","Race",
             "Language",
             "Number_of_Children",
             "Education","Income")
cat.vars<-c("Gender","Race","Language",
            "Education")
table1<-CreateTableOne(vars=list.vars,
                       data = table1.df,
                       factorVars = cat.vars,
                       includeNA = TRUE,
                       test=FALSE)
kableone(table1)
###Table One###
table1.df<-data.frame(TAM=bottomTAMbottomTechno$TAM,
                      Technofere=bottomTAMbottomTechno$NumDevInt,
                      ParProbTech=bottomTAMbottomTechno$ParentProbTech,
                      Age=bottomTAMbottomTechno$Age54,
                      Gender=bottomTAMbottomTechno$sex.mf,
                      Race=bottomTAMbottomTechno$race4.lvl,
                      Language=bottomTAMbottomTechno$language.2lvl,
                      Number_of_Children=bottomTAMbottomTechno$numberchild.2lvl,
                      Education=bottomTAMbottomTechno$education.3lvl,
                      Income=bottomTAMbottomTechno$Income68.5lvl)
list.vars<-c("TAM","Technofere","ParProbTech","Age","Gender","Race",
             "Language",
             "Number_of_Children",
             "Education","Income")
cat.vars<-c("Gender","Race","Language",
            "Education")
table1<-CreateTableOne(vars=list.vars,
                       data = table1.df,
                       factorVars = cat.vars,
                       includeNA = TRUE,
                       test=FALSE)
kableone(table1)

###Table One###
table1.df<-data.frame(TAM=topTAMtopParProbTech$TAM,
                      Technofere=topTAMtopParProbTech$NumDevInt,
                      ParProbTech=topTAMtopParProbTech$ParentProbTech,
                      Age=topTAMtopParProbTech$Age54,
                      Gender=topTAMtopParProbTech$sex.mf,
                      Race=topTAMtopParProbTech$race4.lvl,
                      Language=topTAMtopParProbTech$language.2lvl,
                      Number_of_Children=topTAMtopParProbTech$numberchild.2lvl,
                      Education=topTAMtopParProbTech$education.3lvl,
                      Income=topTAMtopParProbTech$Income68.5lvl)
list.vars<-c("TAM","Technofere","ParProbTech","Age","Gender","Race",
             "Language",
             "Number_of_Children",
             "Education","Income")
cat.vars<-c("Gender","Race","Language",
            "Education")
table1<-CreateTableOne(vars=list.vars,
                       data = table1.df,
                       factorVars = cat.vars,
                       includeNA = TRUE,
                       test=FALSE)
kableone(table1)
###Table One###
table1.df<-data.frame(TAM=topTAMbottomParProbTech$TAM,
                      Technofere=topTAMbottomParProbTech$NumDevInt,
                      ParProbTech=topTAMbottomParProbTech$ParentProbTech,
                      Age=topTAMbottomParProbTech$Age54,
                      Gender=topTAMbottomParProbTech$sex.mf,
                      Race=topTAMbottomParProbTech$race4.lvl,
                      Language=topTAMbottomParProbTech$language.2lvl,
                      Number_of_Children=topTAMbottomParProbTech$numberchild.2lvl,
                      Education=topTAMbottomParProbTech$education.3lvl,
                      Income=topTAMbottomParProbTech$Income68.5lvl)
list.vars<-c("TAM","Technofere","ParProbTech","Age","Gender","Race",
             "Language",
             "Number_of_Children",
             "Education","Income")
cat.vars<-c("Gender","Race","Language",
            "Education")
table1<-CreateTableOne(vars=list.vars,
                       data = table1.df,
                       factorVars = cat.vars,
                       includeNA = TRUE,
                       test=FALSE)
kableone(table1)
###Table One###
table1.df<-data.frame(TAM=bottomTAMtopParProbTech$TAM,
                      Technofere=bottomTAMtopParProbTech$NumDevInt,
                      ParProbTech=bottomTAMtopParProbTech$ParentProbTech,
                      Age=bottomTAMtopParProbTech$Age54,
                      Gender=bottomTAMtopParProbTech$sex.mf,
                      Race=bottomTAMtopParProbTech$race4.lvl,
                      Language=bottomTAMtopParProbTech$language.2lvl,
                      Number_of_Children=bottomTAMtopParProbTech$numberchild.2lvl,
                      Education=bottomTAMtopParProbTech$education.3lvl,
                      Income=bottomTAMtopParProbTech$Income68.5lvl)
list.vars<-c("TAM","Technofere","ParProbTech","Age","Gender","Race",
             "Language",
             "Number_of_Children",
             "Education","Income")
cat.vars<-c("Gender","Race","Language",
            "Education")
table1<-CreateTableOne(vars=list.vars,
                       data = table1.df,
                       factorVars = cat.vars,
                       includeNA = TRUE,
                       test=FALSE)
kableone(table1)
###Table One###
table1.df<-data.frame(TAM=bottomTAMbottomParProbTech$TAM,
                      Technofere=bottomTAMbottomParProbTech$NumDevInt,
                      ParProbTech=bottomTAMbottomParProbTech$ParentProbTech,
                      Age=bottomTAMbottomParProbTech$Age54,
                      Gender=bottomTAMbottomParProbTech$sex.mf,
                      Race=bottomTAMbottomParProbTech$race4.lvl,
                      Language=bottomTAMbottomParProbTech$language.2lvl,
                      Number_of_Children=bottomTAMbottomParProbTech$numberchild.2lvl,
                      Education=bottomTAMbottomParProbTech$education.3lvl,
                      Income=bottomTAMbottomParProbTech$Income68.5lvl)
list.vars<-c("TAM","Technofere","ParProbTech","Age","Gender","Race",
             "Language",
             "Number_of_Children",
             "Education","Income")
cat.vars<-c("Gender","Race","Language",
            "Education")
table1<-CreateTableOne(vars=list.vars,
                       data = table1.df,
                       factorVars = cat.vars,
                       includeNA = TRUE,
                       test=FALSE)
kableone(table1)
### nonparametric ANOVA tests
kruskal.test(list(topTAMtopTechno$Age54,
                  topTAMbottomTechno$Age54,
                  bottomTAMtopTechno$Age54,
                  bottomTAMbottomTechno$Age54))
### chisquare tests on tables
topTAMtopPPT_tab<-c(47,46)
topTAMbotPPT_tab<-c(18,31)
botTAMtopPPT_tab<-c(27,29)
botTAMbotPPT_tab<-c(54,28)
tab<-cbind(topTAMtopPPT_tab,
               topTAMbotPPT_tab,
               botTAMtopPPT_tab,
               botTAMbotPPT_tab)
chisq.test(tab)


###SEM Modeling###
#plot the scales
plot(AI.Par.Surv_model.df[c(2:4)])
#plot the densities
par(mfrow=c(1,3))
for(i in names(AI.Par.Surv_model.df[c(2:4)])){
  density(AI.Par.Surv_model.df[c(2:4)][,i])%>%plot(
    main=paste("Density Plot",i,sep = " "))
}
#write in the model
AI.Par.Surv_model.df<-subset(AI.Par.Surv_model.df,
                             select=-c(education.3lvl))
# specify the CFA model
cfa_model <- ' TAM  =~ TechAccept42 + TechAccept43 + TechAccept44 + TechAccept45 + TechAccept48 + TechAccept49 + TechAccept53      
              NumDevInt =~ TechInterrupt24 + TechInterrupt25 + TechInterrupt26 + TechInterrupt27 + TechInterrupt28
              ParentProbTech =~ ProbTech29 + ProbTech30 + ProbTech31 '

# fit the model
fit_AI <- cfa(cfa_model, 
              data=AI_par.survey_age5_num_dur136_spv_no.ven)
# display summary output
summary(fit_AI, fit.measures=TRUE)

#specify the SEM model
AI.Par.surv_sem.model <- 
  ' TAM  =~ TechAccept42 + TechAccept43 + TechAccept44 + TechAccept45 + TechAccept48 + TechAccept49 + TechAccept53      
    NumDevInt =~ TechInterrupt24 + TechInterrupt25 + TechInterrupt26 + TechInterrupt27 + TechInterrupt28
    ParentProbTech =~ ProbTech29 + ProbTech30 + ProbTech31 
    NumDevInt ~ ParentProbTech
    TAM ~ ParentProbTech + NumDevInt
    TAM ~ ParentProbTech'
#fit model
fit_sem <- sem(AI.Par.surv_sem.model, 
               data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(fit_sem, standardized=TRUE)

###SEM model take 2
model <- '
             TAM ~ c*ParentProbTech
             NumDevInt ~ a*ParentProbTech
             TAM ~ b*NumDevInt
             ab := a*b
             total := c + (a*b)
         '
fit <- sem(model, data = AI.Par.Surv_model.df)
summary(fit,fit.measures=TRUE,standardized=TRUE)
semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
fitmeasures(fit,c("chisq","rmsea","srmr","gfi","ecvi"))

###Mediation Analysis###
#Flow chart: Regress Y on X. Does X predict Y?
y.on.x<-glm(TAM~ParentProbTech + 
              Age54 + education.2lvl + Income68.5lvl + sex.mf,
           data=AI.Par.Surv_model.df)
summary(y.on.x)
#Since X does, regress M on X. Does X predict M?
m.on.x<-glm(NumDevInt~ParentProbTech,
            data=AI.Par.Surv_model.df_na.omit.sex.race,
            family=poisson)
summary(m.on.x)
#Since X does, regress Y on X and M. Does X predict Y?
y.on.xm<-glm(TAM~ParentProbTech + NumDevInt + 
               Age54 + race4.lvl + language.2lvl + sex.mf,
                data=AI.Par.Surv_model.df,
                family=Gamma)
summary(y.on.xm)
#Model Complex: Significant covariates to NDI
AI.Par.Surv_model.df_na.omit.sex<-AI.Par.Surv_model.df %>% 
  drop_na(sex.mf)
AI.Par.Surv_model.df_na.omit.sex.race<-AI.Par.Surv_model.df_na.omit.sex %>% 
  drop_na(race4.lvl)
#Violated assumptions
med.fit<-lm(NumDevInt~ParentProbTech + 
               Age54 + education.2lvl + Income68.5lvl + sex.mf,
             data=AI.Par.Surv_model.df_na.omit.sex.race)
out.fit<-lm(TAM~NumDevInt + ParentProbTech + 
               Age54 + education.2lvl + Income68.5lvl + sex.mf,
             data=AI.Par.Surv_model.df_na.omit.sex.race)
med.out<-mediate(med.fit,out.fit,
                 treat="ParentProbTech",
                 mediator="NumDevInt",
                 boot = TRUE)
summary(med.fit)
summary(out.fit)
summary(med.out)
plot(med.out)
  #conduct sensitivity analysis to test 
  #sequential ignorability assumption
sens.out<-medsens(med.out,
                  rho.by = 0.1,
                  effect.type = "indirect",
                  sims = 100)
summary(sens.out)
plot(sens.out)
#path diagram
reg.coefs<-c(0,"''",0,
          0, 0, 0, 
          "''", 
          "''", 
          0)
M<- matrix(nrow=3,ncol=3,byrow = TRUE,
           data=reg.coefs)
plot<- plotmat(M, pos=c(1,2), 
                name= c( "Parent\ntechnoference",
                         "Problem\ntechnology use",
                         "Technology\nacceptance"), 
                box.type = "circle", 
                box.size = 0.12, 
                box.prop=0.5,
                box.cex = .8,
               curve=0)
text(coordinates(N=2,pos =2),
     lab = c("0.55 (0.09)","0.15 (0.03)"),
     cex = .8)

text(coordinates(N=1,pos=1,my=-0.33),
     lab=c("0.42 (0.05)"),
     cex=.8) #.42 (.05)
text(coordinates(N=1,pos=1,mx=-0.045,my=-.09),
     lab=c(
       "\U1D43C\U1D45B\U1D451\U1D456\U1D45F\U1D452\U1D450\U1D461"),
     cex=1.1) #Indirect
text(coordinates(N=1,pos=1,mx=0.055,my=-.08),
     lab=c(
       "=0.08 (0.03)"),
     cex=.8)


#Nonviolated assumptions
med.fit2<-glm(NumDevInt~ParentProbTech + 
               Age54 + education.2lvl + Income68.5lvl + sex.mf,
            family=poisson,data=AI.Par.Surv_model.df_na.omit.sex.race)
out.fit2<-glm(TAM~NumDevInt + ParentProbTech + 
                Age54 + education.2lvl + Income68.5lvl + sex.mf,
             data=AI.Par.Surv_model.df_na.omit.sex.race,
             family=Gamma)
med.out2<-mediate(med.fit2,out.fit2,
                 treat="ParentProbTech",
                 mediator="NumDevInt",
                 treat.value = 6,
                 control.value = 1,
                 boot = TRUE)
summary(med.out2)  
plot(med.out2)

###using medflex package###
#first standardize variables of interest
AI.Par.Surv_model.df_na.omit.sex.race$PPT_stndzd<-as.vector(scale(
  AI.Par.Surv_model.df_na.omit.sex.race$ParentProbTech
))
AI.Par.Surv_model.df_na.omit.sex.race$NDI_stndzd<-as.vector(scale(
  AI.Par.Surv_model.df_na.omit.sex.race$NumDevInt
))
AI.Par.Surv_model.df_na.omit.sex.race$TAM_stndzd<-as.vector(scale(
  AI.Par.Surv_model.df_na.omit.sex.race$TAM
))
AI.Par.Surv_model.df$PPT_stndzd<-as.vector(scale(
  AI.Par.Surv_model.df$ParentProbTech
))
AI.Par.Surv_model.df$NDI_stndzd<-as.vector(scale(
  AI.Par.Surv_model.df$NumDevInt
))
AI.Par.Surv_model.df$TAM_stndzd<-as.vector(scale(
  AI.Par.Surv_model.df$TAM
))

###Model fitting
#Imputation-based approach to derive natural direct and indirect estimates
#TAM as linear model
head(AI.Par.Surv_model.df)
AI.Par_medflex<-neImpute(TAM ~ PPT_stndzd + NDI_stndzd + 
                           Age54 + education.2lvl + Income68.5lvl + sex.mf,
                         data=AI.Par.Surv_model.df_na.omit.sex.race)
head(AI.Par_medflex)
AI.Par_ne.Mod<-neModel(TAM ~ PPT_stndzd0 + PPT_stndzd1 + 
                         Age54 + education.2lvl + Income68.5lvl + sex.mf,
                       expData = AI.Par_medflex,
                       se="robust")
summary(AI.Par_ne.Mod)
plot(AI.Par_ne.Mod)
#find the size of the effect
exp(cbind(coef(AI.Par_ne.Mod), confint(AI.Par_ne.Mod)))
#effect decomposition
AI.Par_decomp <- neEffdecomp(AI.Par_ne.Mod)
summary(AI.Par_decomp)
#TAM as gamma model
AI.Par_medflex<-neImpute(TAM ~ PPT_stndzd + NDI_stndzd + 
                           Age54 + education.2lvl + Income68.5lvl + sex.mf,
                         family = Gamma,
                         data=AI.Par.Surv_model.df_na.omit.sex.race)
head(AI.Par_medflex)
AI.Par_ne.Mod<-neModel(TAM ~ PPT_stndzd0 + PPT_stndzd1 + 
                         Age54 + education.2lvl + Income68.5lvl + sex.mf,
                       family = Gamma,
                       expData = AI.Par_medflex)
summary(AI.Par_ne.Mod)
#effect decomposition
AI.Par_decomp <- neEffdecomp(AI.Par_ne.Mod)
summary(AI.Par_decomp)
#TAM as gamma model and PPT as Gamma
AI.Par_medflex<-neImpute(TAM ~ PPT_stndzd + NDI_stndzd + 
                           Age54 + education.2lvl + Income68.5lvl + sex.mf,
                         family = Gamma,xFit = Gamma,
                         data=AI.Par.Surv_model.df_na.omit.sex.race)
head(AI.Par_medflex)
AI.Par_ne.Mod<-neModel(TAM ~ PPT_stndzd0 + PPT_stndzd1 + 
                         Age54 + education.2lvl + Income68.5lvl + sex.mf,
                       family = Gamma,
                       expData = AI.Par_medflex)
summary(AI.Par_ne.Mod)
#effect decomposition
AI.Par_decomp <- neEffdecomp(AI.Par_ne.Mod)
summary(AI.Par_decomp)

##need to update bivariate with education at two levels
AI_par.survey_age5_num_dur136_spv_no.ven$education.2lvl<-AI.Par.Surv_model.df$education.2lvl
#Education mean/sd
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                                     "NumDevInt",
                                                     "TAM",
                                                     "PercImpact33",
                                                     "Limit34",
                                                     "TechMuch46",
                                                     "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$education.2lvl),
          mean,na.rm=TRUE)
aggregate(AI_par.survey_age5_num_dur136_spv_no.ven[c("ParentProbTech",
                                                     "NumDevInt",
                                                     "TAM",
                                                     "PercImpact33",
                                                     "Limit34",
                                                     "TechMuch46",
                                                     "PrivacyConc47")],
          list(AI_par.survey_age5_num_dur136_spv_no.ven$education.2lvl),
          sd,na.rm=TRUE)
#Education Parent Prob Tech: ANOVA 
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="ParentProbTech")
#fit model
result_edu.ppt<-aov(ParentProbTech~education.2lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.ppt)
#tukey
TukeyHSD(result_edu.ppt)
#model diagtnostics
#normality test
resid_edu.ppt<-residuals(result_edu.ppt)
rs_edu.ppt<-rstandard(result_edu.ppt)
fits_edu.ppt<-fitted.values(result_edu.ppt)
qqnorm(rs_edu.ppt)
qqline(rs_edu.ppt,col=2)
shapiro.test(rs_edu.ppt) #p<.01 failed normality
#equal variances test
plot(fits_edu.ppt,rs_edu.ppt,
     main="residuals vs. fitted values")
leveneTest(ParentProbTech~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.54 pass equal variances
#independence test 
plot(rs_edu.ppt,type="l",main="run order plot") #looks good
#Education Parent Prob Tech: Kruskal Wallis (nonparametric)
kruskal.test(ParentProbTech~education.2lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education Technof: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="NumDevInt")
#fit model
result_edu.tech<-aov(NumDevInt~education.3lvl,
                     data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.tech)
#tukey
TukeyHSD(result_edu.tech)
#model diagtnostics
#normality test
resid_edu.tech<-residuals(result_edu.tech)
rs_edu.tech<-rstandard(result_edu.tech)
fits_edu.tech<-fitted.values(result_edu.tech)
qqnorm(rs_edu.tech)
qqline(rs_edu.tech,col=2)
shapiro.test(rs_edu.tech) #p<.01 failed normality
#equal variances test
plot(fits_edu.tech,rs_edu.tech,
     main="residuals vs. fitted values")
leveneTest(NumDevInt~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.56 pass equal variances
#independence test 
plot(rs_edu.tech,type="l",main="run order plot") #looks good
#Education Techno: Kruskal Wallis (nonparametric)
kruskal.test(NumDevInt~education.2lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education TAM: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="TAM")
#fit model
result_edu.TAM<-aov(TAM~education.3lvl,
                    data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TAM)
#tukey
TukeyHSD(result_edu.TAM)
#model diagtnostics
#normality test
resid_edu.TAM<-residuals(result_edu.TAM)
rs_edu.TAM<-rstandard(result_edu.TAM)
fits_edu.TAM<-fitted.values(result_edu.TAM)
qqnorm(rs_edu.TAM)
qqline(rs_edu.TAM,col=2)
shapiro.test(rs_edu.TAM) #p<.01 failed normality
#equal variances test
plot(fits_edu.TAM,rs_edu.TAM,
     main="residuals vs. fitted values")
leveneTest(TAM~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.02 fail equal variances
#independence test 
plot(rs_edu.TAM,type="l",main="run order plot") #looks good
#Education TAM: Kruskal Wallis (nonparametric)
kruskal.test(TAM~education.2lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education PercImpact33: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="PercImpact33")
#fit model
result_edu.PercImp<-aov(PercImpact33~education.3lvl,
                        data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.PercImp)
#tukey
TukeyHSD(result_edu.PercImp)
#model diagtnostics
#normality test
resid_edu.PercImp<-residuals(result_edu.PercImp)
rs_edu.PercImp<-rstandard(result_edu.PercImp)
fits_edu.PercImp<-fitted.values(result_edu.PercImp)
qqnorm(rs_edu.PercImp)
qqline(rs_edu.PercImp,col=2)
shapiro.test(rs_edu.PercImp) #p<.01 failed normality
#equal variances test
plot(fits_edu.PercImp,rs_edu.PercImp,
     main="residuals vs. fitted values")
leveneTest(PercImpact33~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #fail equal variances
#independence test 
plot(rs_edu.PercImp,type="l",main="run order plot") #looks good
#Education PercImpact33: Kruskal Wallis (nonparametric)
kruskal.test(PercImpact33~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education Limit34: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="Limit34")
#fit model
result_edu.Limit<-aov(Limit34~education.3lvl,
                      data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.Limit)
#tukey
TukeyHSD(result_edu.Limit)
#model diagtnostics
#normality test
resid_edu.Limit<-residuals(result_edu.Limit)
rs_edu.Limit<-rstandard(result_edu.Limit)
fits_edu.Limit<-fitted.values(result_edu.Limit)
qqnorm(rs_edu.Limit)
qqline(rs_edu.Limit,col=2)
shapiro.test(rs_edu.Limit) #p<.01 failed normality
#equal variances test
plot(fits_edu.Limit,rs_edu.Limit,
     main="residuals vs. fitted values")
leveneTest(Limit34~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.02 fail equal variances
#independence test 
plot(rs_edu.Limit,type="l",main="run order plot") #looks good
#Education Limit34: Kruskal Wallis (nonparametric)
kruskal.test(Limit34~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education TechMuch: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="TechMuch46")
#fit model
result_edu.TechMuch<-aov(TechMuch46~education.3lvl,
                         data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.TechMuch)
#tukey
TukeyHSD(result_edu.TechMuch)
#model diagtnostics
#normality test
resid_edu.TechMuch<-residuals(result_edu.TechMuch)
rs_edu.TechMuch<-rstandard(result_edu.TechMuch)
fits_edu.TechMuch<-fitted.values(result_edu.TechMuch)
qqnorm(rs_edu.TechMuch)
qqline(rs_edu.TechMuch,col=2)
shapiro.test(rs_edu.TechMuch) #p<.01 failed normality
#equal variances test
plot(fits_edu.TechMuch,rs_edu.TechMuch,
     main="residuals vs. fitted values")
leveneTest(TechMuch46~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #fail equal variances
#independence test 
plot(rs_edu.TechMuch,type="l",main="run order plot") #looks good
#Education TechMuch46: Kruskal Wallis (nonparametric)
kruskal.test(TechMuch46~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)

#Education PrivacyConc: ANOVA
#boxplot
ggboxplot(data=AI_par.survey_age5_num_dur136_spv_no.ven,
          x="education.3lvl",
          y="PrivacyConc47")
#fit model
result_edu.PrivacyConc<-aov(PrivacyConc47~education.3lvl,
                            data=AI_par.survey_age5_num_dur136_spv_no.ven)
summary(result_edu.PrivacyConc)
#tukey
TukeyHSD(result_edu.PrivacyConc)
#model diagtnostics
#normality test
resid_edu.PrivacyConc<-residuals(result_edu.PrivacyConc)
rs_edu.PrivacyConc<-rstandard(result_edu.PrivacyConc)
fits_edu.PrivacyConc<-fitted.values(result_edu.PrivacyConc)
qqnorm(rs_edu.PrivacyConc)
qqline(rs_edu.PrivacyConc,col=2)
shapiro.test(rs_edu.PrivacyConc) #p<.01 failed normality
#equal variances test
plot(fits_edu.PrivacyConc,rs_edu.PrivacyConc,
     main="residuals vs. fitted values")
leveneTest(PrivacyConc47~education.3lvl,
           data=AI_par.survey_age5_num_dur136_spv_no.ven) #p=.02 fail equal variances
#independence test 
plot(rs_edu.PrivacyConc,type="l",main="run order plot") #looks good
#Education PrivacyConc47: Kruskal Wallis (nonparametric)
kruskal.test(PrivacyConc47~education.3lvl,
             data=AI_par.survey_age5_num_dur136_spv_no.ven)
