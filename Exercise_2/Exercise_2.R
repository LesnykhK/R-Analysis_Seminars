

library(foreign)

ess7 <- read.spss("/Users/Lesnykh/Downloads/ESS7e02_1.sav",
                  use.value.labels = FALSE,
                  to.data.frame = TRUE)

Austria <- ess7[ess7$cntry =="AT",
                c("alcwkdy", "alcwknd", "gndr", "agea", "eduyrs", "domicil")]

Austria$alcohol <- Austria$alcwkdy + Austria$alcwknd

sum(Austria$alcohol, na.rm=TRUE)

Austria$alcohol[Austria$gndr ==1]

sum(Austria$alcohol[Austria$gndr ==1], na.rm=TRUE)

cor.test(Austria$agea, Austria$alcohol, use="pairwise")

reg1 <- lm(Austria$alcohol ~ Austria$agea+Austria$gndr)
summary(reg1)

reg2 <- lm(Austria$alcohol ~ Austria$agea+Austria$gndr)
summary(reg2)

reg3 <- lm(Austria$alcohol ~ Austria$agea+Austria$gndr+Austria$eduyrs)
summary(reg3)

reg4 <- lm(Austria$alcohol ~ Austria$agea+Austria$gndr+Austria$eduyrs+Austria$domicil)
summary(reg4)

car::vif(reg4)

install.packages("lmtest")

lmtest::bptest(reg4)
reg5 <- lm(Austria$alcohol ~ Austria$agea+Austria$gndr+Austria$eduyrs+as.factor(Austria$domicil))

car::vif(reg5)