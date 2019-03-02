# Setting Working directory, Opening all the useful packeges, Recoding and Renaming the variables

setwd("/Users/Lesnykh/Desktop/R_Folder/HW-2")   # Setted working derecory for all files.

library(foreign)
library(car)
library(magrittr)
library(dplyr)
library(tidyr)
library(dplyr)
library(sjmisc)
library(ggplot2)
library(nFactors)
library(GPArotation)
library(lavaan)
library(semPlot)
library(psych)

ess.spain <- read.spss(
  file="/Users/Lesnykh/Desktop/R_Folder/HW-2/ESS8ES.sav",
  use.value.labels=FALSE,
  use.missings=TRUE,
  to.data.frame=TRUE
)


Spain <- subset(
  x=ess.spain,
  select=c("ipcrtiv", "imprich", "ipeqopt", "ipshabt", 
           "impsafe", "impdiff", "ipfrule", "ipudrst", 
           "ipmodst", "ipgdtim", "impfree", "iphlppl",
           "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", 
           "iprspot", "iplylfr", "impenv", "imptrad", 
           "impfun")) %>%
    set_colnames(c("being_creative", "being_rich", "equality", "being_admired", "security",
                   "trying_new", "following_rules", "understanding_different_people",
                   "being_humble", "having_good_time", "being_free", "caring_for_others",
                   "being_successful", "having_strong_government", "having_an_exciting_life",
                   "behaving_properly", "being_respectful", "being_loyal", "caring_for_nature",
                   "following_traditions", "seeking_fun")) %>%
    na.omit


for(i in c("being_creative", "being_rich", "equality", "being_admired", "security",
           "trying_new", "following_rules", "understanding_different_people",
           "being_humble", "having_good_time", "being_free", "caring_for_others",
           "being_successful", "having_strong_government", "having_an_exciting_life",
           "behaving_properly", "being_respectful", "being_loyal", "caring_for_nature",
           "following_traditions", "seeking_fun")) {
  
  Spain[, i] <- Recode(Spain[, i], "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")
  
}  

# Conducting the Exploratory Factor Analysis

cov.s <- cov(Spain, use="complete.obs")
cov.s
plot(nScree(cov.s))


formulaEFA <- as.formula("~being_creative+ being_rich+ equality+ being_admired+ security+
                               trying_new+ following_rules+ understanding_different_people+
                               being_humble+ having_good_time+ being_free+ caring_for_others+
                               being_successful+ having_strong_government+ having_an_exciting_life+
                               behaving_properly+ being_respectful+ being_loyal+ caring_for_nature+
                               following_traditions+ seeking_fun")

EFA.4 <- factanal(formulaEFA, factors=4, data=Spain,
                      rotation="geominQ",
                      na.action=na.exclude)

EFA.4

EFA.4$STATISTIC

EFA.4$dof

EFA.5 <- factanal(formulaEFA, factors=5, data=Spain,
                  rotation="geominQ",
                  na.action=na.exclude)

EFA.5

EFA.5$STATISTIC

EFA.5$dof

EFA.6 <- factanal(formulaEFA, factors=6, data=Spain,
                  rotation="geominQ",
                  na.action=na.exclude)

EFA.6

EFA.6$STATISTIC

EFA.6$dof


EFA.S4 <- fa(Spain, 4)
EFA.S4
EFA.S5 <- fa(Spain, 5)
EFA.S5
EFA.S6 <- fa(Spain, 6)
EFA.S6


# Conducting the Confirmatory Factor Analysis

cfa5 <- cfa(model="Joy =~ seeking_fun+having_an_exciting_life;
           Social_loyalty =~ understanding_different_people+
           caring_for_others+being_loyal;
           Respectfullnes =~ being_admired+being_successful+being_respectful;
           Behaviour =~ behaving_properly+following_traditions;
           Being_save =~ security+having_strong_government",
           data=as.matrix(Spain))

cfa5


semPaths(cfa5, whatLabels="est", rotation=2, nCharNodes=0)

summary(cfa5, fit.measures=T)

modindices(cfa5, minimum.value=10, sort=TRUE)

cfa5.1 <- cfa(model="Joy =~ seeking_fun+having_an_exciting_life;
           Social_loyalty =~ understanding_different_people+
             caring_for_others+being_loyal+being_respectful;
             Respectfullnes =~ being_admired+being_successful+being_respectful;
             Behaviour =~ behaving_properly+following_traditions+being_respectful;
             Being_save =~ security+having_strong_government;
             being_respectful ~~ behaving_properly",
             data=as.matrix(Spain))

cfa5.1

semPaths(cfa5.1, whatLabels="est", rotation=2, nCharNodes=0)

summary(cfa5.1, fit.measures=TRUE)

cfa5.2 <- cfa(model="Joy =~ seeking_fun+having_an_exciting_life;
           Social_loyalty =~ understanding_different_people+
             caring_for_others+being_loyal;
             Respectfullnes =~ being_admired+being_successful+being_respectful;
             Behaviour =~ behaving_properly+following_traditions+being_respectful;
             Being_save =~ security+having_strong_government",
             data=as.matrix(Spain))

cfa5.2

semPaths(cfa5.2, whatLabels="est", rotation=2, nCharNodes=0)

summary(cfa5.2, fit.measures=TRUE)

modindices(cfa5.2, minimum=10, sort=TRUE)

lavTestLRT( cfa5, cfa5.1, cfa5.2)

cfa6 <- cfa(model="Social_loyalty =~ equality+understanding_different_people+
           caring_for_others+being_loyal;
           Respectfullnes =~ being_admired+being_successful+being_respectful;
           Behaviour =~ behaving_properly+following_traditions;
           Diversity =~ trying_new+having_an_exciting_life;
           Joy =~ having_good_time+seeking_fun;
           Being_save =~ security+having_strong_government",
           data=as.matrix(Spain))
cfa6

semPaths(cfa6, whatLabels="est", rotation=2, nCharNodes=10)

summary(cfa6, fit.measures=TRUE)

modindices(cfa6, minimum=10, sort=TRUE)

cfa6.1 <- cfa(model="Social_loyalty =~ equality+understanding_different_people+
           caring_for_others+being_loyal;
           Respectfullnes =~ being_admired+being_successful+being_respectful;
           Behaviour =~ behaving_properly+following_traditions+being_respectful;
           Diversity =~ trying_new+having_an_exciting_life;
           Joy =~ having_good_time+seeking_fun;
           Being_save =~ security+having_strong_government;
           equality ~~ understanding_different_people",
            data=as.matrix(Spain))
cfa6.1

semPaths(cfa6.1, whatLabels="est", rotation=2, noCharNodes=10)

summary(cfa6.1, fit.measures=TRUE)

modindices(cfa6.1, minimum=10, sort=TRUE)

cfa6.2 <- cfa(model="Social_loyalty =~ equality+understanding_different_people+
           caring_for_others+being_loyal;
              Respectfullnes =~ being_admired+being_successful+being_respectful;
              Behaviour =~ behaving_properly+following_traditions+being_respectful;
              Diversity =~ trying_new+having_an_exciting_life+understanding_different_people;
              Joy =~ having_good_time+seeking_fun;
              Being_save =~ security+having_strong_government;
              equality ~~ understanding_different_people",
              data=as.matrix(Spain))
cfa6.2

semPaths(cfa6.2, whatLabels="est", rotation=2, noCharNodes=0)

summary(cfa6.2, fit.measures=TRUE)

modindices(cfa6.2, minimum=10, sort=TRUE)

lavTestLRT( cfa6, cfa6.1, cfa6.2)

fit.index<-data.frame(fitmeasures(cfa5),
                      fitmeasures(cfa5.1),
                      fitmeasures(cfa5.2),
                      fitmeasures(cfa6),
                      fitmeasures(cfa6.1),
                      fitmeasures(cfa6.2)
)
names(fit.index) <- c("5-факторная модель", "5-факторная модель 2", "5-факторная модель 3", "6-факторная модель", "6-факторная модель 2", "6-факторная модель 3") 

fit.index<- fit.index[c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "bic"),] 



knitr::kable(fit.index, digits=3, format="html")

# Conducting the Structural Model


structural.formula <- paste("Joy =~ seeking_fun+having_an_exciting_life;
                              Social_loyalty =~ understanding_different_people+caring_for_others+being_loyal;
                              Respectfullnes =~ being_admired+being_successful+being_respectful;
                              Behaviour =~ behaving_properly+following_traditions+being_respectful;
                              Being_save =~ security+having_strong_government;
                              Social_loyalty ~ Respectfullnes + Behaviour + Joy + Being_save;
                              Respectfullnes ~ Behaviour + Joy + Being_save + Social_loyalty;
                              Behaviour ~ Social_loyalty + Respectfullnes + Joy + Being_save;
                              Joy ~ Social_loyalty + Behaviour + Respectfullnes + Being_save;
                              Being_save ~ Social_loyalty + Behaviour + Joy + Respectfullnes")

structural.model <- sem(structural.formula,
                          data=Spain,
                          estimator="MLR",
                          missing="listwise")

structural.model

summary(structural.model, fit.measures=TRUE)


semPaths(structural.model, 
         intercepts=T, 
         nCharNodes = 130, 
         whatLabels="std", 
         layout="tree2", 
         rotation=4, 
         residuals=T, 
         shapeMan="rectangle",
         sizeMan=20, 
         sizeMan2=4)
