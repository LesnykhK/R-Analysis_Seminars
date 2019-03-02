setwd("/Users/Lesnykh/Desktop/R_Folder/HW-1")   # Setted working derecory for all files.

library(foreign)    # Opened package "library" for the imporst of outer datasets.

ess.spain <- read.spss(
  file="/Users/Lesnykh/Desktop/R_Folder/HW-1/ESS8ES.sav",
  use.value.labels=FALSE,
  use.missings=TRUE,
  to.data.frame=TRUE
)

# Opened dataset ESS 8 for Spain 2016 (ess.spain).

Spain <- subset(
  x=ess.spain,
  select=c("idno", "gndr", "ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff", 
           "ipfrule", "ipudrst", "ipmodst", "ipgdtim", "impfree", "iphlppl", "ipsuces", 
           "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr", "impenv", "imptrad", 
           "impfun", "agea", "eduyrs", "domicil")
)

# Created filtered dataset with variables on socio-demographic and life values characteristics.

library("car")  # Opened package "car" for recoding and buildong regressions.


Spain$being_creative <- Recode(var=Spain$ipcrtiv,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$being_rich <- Recode(var=Spain$imprich,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$equality <- Recode(var=Spain$ipeqopt,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$being_admired <- Recode(var=Spain$ipshabt,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$security <- Recode(var=Spain$impsafe,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$trying_new <- Recode(var=Spain$impdiff,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$following_rules <- Recode(var=Spain$ipfrule,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$understanding_different_people <- Recode(var=Spain$ipudrst,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$being_humble <- Recode(var=Spain$ipmodst,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$having_good_time <- Recode(var=Spain$ipgdtim,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$being_free <- Recode(var=Spain$impfree,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$caring_for_others <- Recode(var=Spain$iphlppl,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$being_successful <- Recode(var=Spain$ipsuces,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$having_strong_government <- Recode(var=Spain$ipstrgv,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$having_an_exciting_life <- Recode(var=Spain$ipadvnt,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$behaving_properly <- Recode(var=Spain$ipbhprp,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$being_respectful <- Recode(var=Spain$iprspot,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$being_loyal <- Recode(var=Spain$iplylfr,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$caring_for_nature <- Recode(var=Spain$impenv,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$following_traditions <- Recode(var=Spain$imptrad,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

Spain$seeking_fun <- Recode(var=Spain$impfun,
                                recodes= "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; NA=NA; else=0")

# Life values variables are coded from the most positive to the most negative
# For the convenience of interpretation, the encoding was performed.

# IPSTRGV - Important that government is strong and ensures safety
# IPEQOPT - Important that people are treated equally and have equal opportunities
# IMPSAFE - Important to live in secure and safe surroundings
# IPFRULE - Important to do what is told and follow rules
# IPMODST - Important to be humble and modest, not draw attention
# IPBHPRP - Important to behave properly


# In the course of further analysis, it will be considered how in Spain the value 
# of a strong government can be related to other life values.

# In the course of further analysis, the following hypotheses will be tested:

  # 1. The value of a strong goverment (IPSTRGV) has a strong positive relationship with the value 
  #    of equal treatment of all people and the availability of equal opportunities for all (IPEQOPT).
  
  # 2. The value of a strong state (IPSTRGV) has a strong positive relationship with value
  #    safe environments (IMPSAFE).

  # 3. The value of a strong state (IPSTRGV) has a strong positive relationship with the value
  #   follow the accepted rules (IPFRULE).

  # 4. The value of a strong state (IPSTRGV) has a strong positive relationship with value
  #    be modest and secretive, do not attract attention (IPMODST).

  # 5. The value of a strong state (IPSTRGV) has a strong positive relationship with the value
  #   behave correctly (IPBHPRP).

  # 6. The value of following the accepted rules (IPFRULE) and the value of behaving correctly (IPBHPRP)
  #    By their content are strongly interrelated and together can have a strong connection with
  #    value of a strong state (IPSTRGV).

# To confirm or refute the hypotheses put forward, we will use linear regression.
# To test hypotheses 1-5, it is necessary to construct multiple linear regression. For check
# Hypothesis 6 is necessary on a similar regression with the interaction effect of predictors.

# In addition to the above independent variables, some socio-demographic characteristics 
# of respondents have been also put in the model, such as gender (GNDR), age (AGEA) 
# and type of settlement (DOMICIL).

# Before proceeding to test hypotheses and construct regressions, we will analyze
# descriptive statistics.

summary(Spain)  # This command considers the key figures for all variables of the Spain subarray.
                # Among the indicators are average, median, minimum and maximum values of variables.

table(Spain$gndr, Spain$domicil, useNA="always")  
table(Spain$domicil, useNA="always") 
table(Spain$gndr, useNA="always")


library(magrittr)
library(ggplot2)
library(ggiraph)
library(lattice)


tbl.ipstrgv.recoded <- data.frame(Spain$ipstrgv.recoded, mean(Spain$ipstrgv.recoded, na.rm=T)) %>%
  set_names(c("Importance_of_strong_government", "mean"))

tbl.ipstrgv.recoded


Spain$ipstrgv.recoded <- as.vector(Spain$ipstrgv.recoded)
Spain$ipeqopt.recoded <- as.vector(Spain$ipeqopt.recoded)


ggplot(Spain, aes(x=ipstrgv.recoded))+
  geom_bar(colour = "blue", fill = "white")+
  scale_y_continuous(breaks=seq(0, 850, 150))+
  scale_x_continuous(breaks=seq(1,6,1))+
  theme_minimal()+
  scale_x_discrete(expand=c(.05,.05))


install.packages("ggiraph")


library(lattice)
bwplot(Spain$mean_ipstrgv.recoded ~ Spain$ipeqopt.recoded |Spain$gndr, horizontal=FALSE)

# These commands allow you to see the cross-tabulation by sex and by type of settlement.

# In this sample, the majority of respondents are from small towns(about 900 values of domicil is 4).

# The sex distribution is approximately equal (~ 980 representatives of each sex).

# Next, proceed with constructing a regression model

lreg1 = lm(ipstrgv.recoded ~ ipeqopt.recoded+impsafe.recoded+ipfrule.recoded+ipmodst.recoded+ipbhprp.recoded
         +agea+as.factor(domicil),
         data=Spain)

Spain$agea <- as.vector(Spain$agea)


summary(lreg1)

# lreg1 is a model for testing hypotheses 1-5. Before proceeding with the interpretation,
                         # we checks the assumptions for multicollinearity and homoskedasticity

car::vif(lreg1) # Test on multicollinearity

# Since the values of all variables are close to 1, the independent variables are not multicollinear

library(lmtest) # Opened package for carrying out the Breusch-Pagan test on heteroscedasticity.

lmtest::bptest(lreg1) # The Breusch-Pegan test for heteroscedasticity.

# Since the test is statistically significant, the data is characterized by heteroscedasticity.
# Consequently, the data is scattered too far from the constructed regression line,
# the model is not relevant for testing hypotheses. However, since the purpose of this assignment
# is not get the corresponding reality of the conclusions, then the model will be interpreted.

summary(lreg1)

# The equation of the regression model is as follows:

    #   Y = 1.43 + 0.09*ipeqopt.recoded + 0.37*impsafe.recoded + 0.12*ipfrule.recoded +
    #        + 0.05*ipmodst.recoded + 0.1*ipbhprp.recoded

# Interpretation of the model is the following (at the level of 95% statistical probability): 

#      The model is significant (2.2e-16 <0.05);

# The coefficient of determination is 0.24, hence the model explains 24% variance dispercion;

# If all independent variables are 0, the value of Y is 1.43;

# If ipeqopt.recoded is changed to 1, Y is incremented by 0.1;

# If impsafe.recoded is changed to 1, Y is incremented by 0.37;

# If ipfrule.recoded is changed to 1, Y is incremented by 0.12;

# If ipmodst.recoded is changed to 1, Y is incremented by 0.05;

# If ipbhprp.recoded is changed to 1, Y is incremented by 0.1.

# In the constructed model, the variables of vital values are significant,
# variables of socio-demographic characteristics are insignificant.

# Thus, the value of a strong state has a strong positive relationship with value
# safe environments and less strong with the rest of the values in the model. Therefore, 
# with the help of the constructed model can confirm hypotheses 1-5, but taking into account 
# that the data is heteroscedastic.

# Now hypothesis 6 will be tested. To test it, a similar regression model will be applied,
# but with the predictors interaction effect included in it.

lreg2 = lm(ipstrgv.recoded ~ ipeqopt.recoded+impsafe.recoded+ipfrule.recoded+ipmodst.recoded+
         ipbhprp.recoded+agea+as.factor(domicil)+ipbhprp.recoded:ipfrule.recoded,
         data=Spain)

summary(lreg2)

# The interaction effect in this model is "ipbhprp.recoded: ipfrule.recoded".
# According to this model, the interaction effect is not statistically significant (0.67> 0.05).
# Based on the constructed model, one can refuse the hypothesis about the interaction of values to follow
# accepted rules and behave correctly.

# Further, for the secondary testing of hypotheses 1-5, an order regression will be constructed.

library(MASS) # Opened package for constructing ordinal regression.

Spain$ipstrgv.recoded.f <- factor(Spain$ipstrgv.recoded, ordered=TRUE)

# Recoding the character dependent variable to the factor variable.

# Constructed ordinal regression.
oreg1 <- polr(ipstrgv.recoded.f ~ impsafe.recoded+ipfrule.recoded+ipmodst.recoded+
                ipbhprp.recoded,
              data=Spain, Hess=TRUE)

summary(oreg1)

ctable <- coef(summary(oreg1))
ctable

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

ctable

ci <- confint(oreg1)

exp(coef(oreg1))

exp(cbind(OR = coef(oreg1), ci))

# 1. One unit increase in "impsafe.recoded" the odds of value 6 of "ipstrgv.recoded.f" 
#    versus values 1-5 of "ipstrgv.recoded.f" combined are 2.05 greater.

# 2. One unit increase in "ipfrule.recoded" the odds of value 6 of "ipstrgv.recoded.f" 
#    versus values 1-5 of "ipstrgv.recoded.f" combined are 1.2 greater.

# 3. One unit increase in "ipmodst.recoded" the odds of value 6 of "ipstrgv.recoded.f" 
#    versus values 1-5 of "ipstrgv.recoded.f" combined are 1.25 greater.

# 4. One unit increase in "ipbhprp.recode" the odds of value 6 of "ipstrgv.recoded.f" 
#    versus values 1-5 of "ipstrgv.recoded.f" combined are 1.23 greater.

# Based on comparing of the two constructed model, the results are analogical. 
# The strongest influence on the importance of the strong governemt has the value of the safe
# surroundings.

