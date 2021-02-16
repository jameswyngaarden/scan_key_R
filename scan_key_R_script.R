# set working directory
setwd("~/Desktop/temple/code_directory/nm_jw/")

# import scan key
read.csv("~/Desktop/temple/code_directory/nm_jw/scan_key_full.csv")

# install packages
# install.packages("corrplot")
# install.packages("ggcorrplot")

# load packages
library(tidyverse)
library(corrplot)
library(ggcorrplot)

# load scan key data
scan_key <- read_csv("scan_key_full.csv")

# view data
str(scan_key)
summary(scan_key)
head(scan_key)

# remove first two columns (sub ID and study number)
scan_key2 <- scan_key[,-c(1,2)]
head(scan_key2)

mcor <- round(cor(scan_key2),4)

mcor
write.csv(mcor, 'correlationmatrix.csv')

# from dusplayr website:
mydata = scan_key2
mydata.cor <- cor(scan_key2)
mydata.cor
#install.packages("Hmisc")
library("Hmisc")
mydata.rcorr = rcorr(as.matrix(scan_key2))
mydata.rcorr

mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P
write.csv(mydata.coeff, 'correlationmatrix_coeff.csv')
write.csv(mydata.p, 'correlationmatrix_p.csv')


## ggcorrplot
# make correlation matrix (outputs are p values)
ggcorr <- round(cor(scan_key2), 3)
head(ggcorr[, 1:6])

#visualize correlation matrix
ggcorrplot(ggcorr, hc.order=TRUE, p.mat=p.mat, lab=FALSE, insig="blank")











# independent 2 sample t-test
t.test(scan_key$`Score for Social Anxiety (Sum)`~scan_key$`gender (0=f)`)

# create a boxplot
bxp <- ggboxplot(
  genderSocialAnxiety, x = "gender", y = "social anxiety",
  ylab = "social anxiety", xlab = "gender", add = "jitter"
)

# Add p-value and significance levels
stat.test <- stat.test %>% add_xy_position(x = "gender")
bxp +
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))




