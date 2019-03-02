install.packages("haven")

library("haven")

w1 <- read_sav("Desktop/R_Folder/Exercise_3/ch07a_2p_EN.sav")

w2 <- read_sav("Desktop/R_Folder/Exercise_3/ch10d_EN_1.0p.sav")

w3 <- read_sav("Desktop/R_Folder/Exercise_3/ch13g_EN_1.0p.sav")

w4 <- read_sav("Desktop/R_Folder/Exercise_3/ch17j_EN_1.0p.sav")


install.packages("dplyr")

library("dplyr")

dataHapiness <- rename(w1234,
                       happiness.w1 = ch07a015, health.w1 = ch07a004,
                       happiness.w2 = ch10d015, health.w2 = ch10d004,
                       happiness.w3 = ch13g015, health.w3 = ch13g004,
                       happiness.w4 = ch17j015, health.w4 = ch17j004)

str(dataHapiness$happiness.w1)

str(dataHapiness$health.w1)

library("lavaan")

cor(
  dataHapiness[, c("happiness.w1", "happiness.w2", "happiness.w3", "happiness.w4", "health.w1",
                   "health.w2", "health.w3", "health.w4")],
  use="pairwise",
  method="pearson")



cl1 <- sem("
           happiness.w2 ~ happiness.w1;
           happiness.w3 ~ happiness.w2;
           happiness.w4 ~ happiness.w3;
           
           health.w2 ~ health.w1;
           health.w3 ~ health.w2;
           health.w4 ~ health.w3;
           
           happiness.w2 ~ health.w1;
           happiness.w3 ~ health.w2;
           happiness.w4 ~ health.w3;
           
           health.w2 ~ happiness.w1;
           health.w3 ~ happiness.w2;
           health.w4 ~ happiness.w3",
           data = dataHapiness)

summary(cl1)

modindices(cl1, sort=T )

library(tidyr)
library(dplyr)
library(magrittr)
library(sjmisc)


library(foreign)
library(haven)

full_ess <- haven::read_sav("\Users\Lesnykh\Desktop\R_Folder\Exercise_3\data\ESS7e02_1.sav")
