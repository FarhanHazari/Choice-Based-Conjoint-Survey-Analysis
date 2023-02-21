# ******************************************************************************
# To be more organized we decided to write or report in a different file
# which is attached to the same email that you find this file 
# *****************************************************************************

#Choice-based conjoint analysis for services on virtual servers 
#for CS students.

################################################################
   
#We made the survey by cbcTools pakage. it can be access from tis link:
#https://jhelvy.github.io/cbcTools/
#install.packages("remotes")
#remotes::install_github("jhelvy/cbcTools")
library(cbcTools)
# load library for fitting multinomial logit models 
library(mlogit)
#install.packages("lessR")
library(lessR) # For beautiful piechart



# Making the profiles

profiles <- cbc_profiles(
  vCPU = c(2, 4, 8, 16),
  RAM = c(4, 8, 16, 32),
  Disk = c(20, 40, 160, 360),
  Traffic = c(20, 30),
  Price = c(4, 8, 10, 15)
)

# There is a changce to put some criteria fror your profile making process. We
#We did not ue these criteria

# limited_profiles <- cbc_profiles(
#   vCPU = c(2, 4, 8, 16),
#   RAM = c(4, 8, 16, 32),
#   Traffic = c(20, 30),
#   Price = c(4, 8, 10, 15),
#   Disk = list('360' = list(price = c(10 ,15)),
#               '160' = list(price = c(8, 10, 15)),
#               '40' = list(price = c(4, 8, 10, 15)),
#               '20' = list(price = c(4, 8, 10))
#   )
# )


#After collecting the answers we have two dataset. One the survey on and one is
# for the respondent level variables(Sex and education level)
  
setwd("D:\\Unitn\\Lab of Customer and bussiness\\project")

# Import the dataset about the respondant level:
rlevel<- read.csv("rlevel.csv", sep = ',')

#pie(table(rlevel$Sex))

# respondent level variables check frequency
PieChart(Sex, hole = 0, values = "%", data = rlevel,
         fill = c("lightblue", "pink"), main='',cex=2)
PieChart(Edu, hole = 0, values = "%", data = rlevel,
         fill = c("lightblue", "pink"), main='',cex=2)



# import the data about the Minivans Survey for conjoint analysis
vpsansw <- read.csv("answer_human.csv", sep = ',')
head(vpsansw,3)

# We can check the ovrelaps by this function from CBCtools
cbc_overlap(vpsansw)

#the number of respondent is 50
length(table(vpsansw$respID))

#And each respondant will see 30 profiles
unique(table(vpsansw$respID))

#check the choice variable conditioning on alternative
table (vpsansw$altID[vpsansw$choice==1])

#check the choice variable conditioning on alternative
# 1   2   3 
# 160 169 171 

## The position of the alternatives didnt have impact on the choices

#Checking the frequency distribution of other attributes
#we may exclude the columns

sapply(vpsansw[,-1:-6], table)


#vCPU RAM Disk Traffic Price choice
# see some descriptive statistics
#attach(vpsansw)
summary(vpsansw)
xtabs(choice ~ vCPU, data=vpsansw)
xtabs(choice ~ RAM, data=vpsansw)
xtabs(choice ~ Disk, data=vpsansw)
xtabs(choice ~ Traffic, data=vpsansw)
xtabs(choice ~ Price, data=vpsansw)

# We can apply this xtables easly by CBCtools with this command
#cbc_balance(vpsansw)


# Change the type of the variables
vpsansw$vCPU <- as.factor(vpsansw$vCPU)
vpsansw$RAM <- as.factor(vpsansw$RAM) 
vpsansw$Disk <- as.factor(vpsansw$Disk)
vpsansw$Traffic <- as.factor(vpsansw$Traffic)
vpsansw$Price <- as.factor(vpsansw$Price)

# drop thw columns that we do not need them
vpsansw_n <- vpsansw[,-1]
vpsansw_n <- vpsansw_n[,-4]
vpsansw_n <- vpsansw_n[,-4]
#summary(vpsansw_n)
#str(vpsansw_n)


# change the data frame to dfidx. Here the format of the qID is differnt as
#it made by the CBCTools, we can change the format or we can use the obsID.
# We decided to change the format
vpsansw_n$qID<-rep(1:(nrow(vpsansw_n)/3), each=3) # So the clumn will be a increamental number column

vpsansw_n.mlogit <- dfidx(vpsansw_n, idx = list(c('qID', 'respID'),'altID'))

head(vpsansw_n.mlogit,3)

# Mloit by considering the alternatives
m1 <- mlogit(choice ~ vCPU + RAM + Disk + Traffic + Price, data = vpsansw_n.mlogit)
summary(m1)


# Fit the model without intercept parameters 
m2 <- mlogit(choice ~ vCPU + RAM + Disk + Traffic + Price | -1, data = vpsansw_n.mlogit)
summary(m2)


# likelihood ratio test 
lrtest(m1, m2)

#Df  LogLik Df  Chisq Pr(>Chisq)
# 1  15 -288.33                     
# 2  13 -289.07 -2 1.4956     0.4734
# Intercepts did not have any big influence on the model.


# Fit the model without intercept parameters and with price as a 
# as So we can calculate the WTP
m3 <- mlogit(choice ~ vCPU + RAM + Disk + Traffic + as.numeric(as.character(vpsansw_n.mlogit$Price)) | -1, data = vpsansw_n.mlogit)
summary(m3)
lrtest(m3, m2)
# did not have a big change they are equally well.

# Compute the willingness to pay for all parameters

for(elem in names(coef(m3))){
  print("------------")
  c <- (coef(m3)[elem])/(coef(m3)["as.numeric(as.character(vpsansw_n.mlogit$Price))"])
  print(c)
}

# Simulate preference shares 

predict.mnl <- function(model, data) {
  # Function for predicting preference shares from a MNL model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares.  Same format at the data used to estimate model. 
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  logitUtility <- data.model%*%model$coef
  share <- exp(logitUtility)/sum(exp(logitUtility))
  cbind(share, data)
}

# In order to use "predict.mnl", you need to define a data frame containing the set of designs 
# for which you want to predict the preference shares. 
# One way to do this is to create the full set of possible designs
# using expand.grid() and select the designs we want by row number

#vCPU RAM Disk Traffic Price choice
attributes <- list(vCPU=names(table(vpsansw_n.mlogit$vCPU)),
                   RAM=names(table(vpsansw_n.mlogit$RAM)),
                   Disk=names(table(vpsansw_n.mlogit$Disk)),
                   Traffic=names(table(vpsansw_n.mlogit$Traffic)),
                   Price=names(table(vpsansw_n.mlogit$Price)))
allDesign <- expand.grid(attributes) 
allDesign #all possible design

# we choose a reasonable and realistic subset (where the first row indicates our design)

# new.datax <- data.frame (vCPU  = c("8", "8","4","4", "16", "16" ),
#                          RAM = c("4", "16","4", "16", "4", "16" ),
#                          Disk = c("160","360", "40", "160", "160", "360"),
#                          Traffic = c("20","30","20","30","20","30"),
#                          Price = c("8","10","10","8","15","15")

# We used the row number of the configuration
                         
new.data1 <- allDesign[c(163, 379, 274, 234, 420, 508), ]
new.data

new.data2 <- allDesign[c(364, 448, 508, 376, 432, 152), ]



predict.mnl(m2, new.data2) # using m2 specification

# we also need to check the precision of this point estimation
source("BootCI.predict.mnl.R")
library(parallel)
BootCI.predict.mnl(m2,new.data2)

#View(allDesign)

# Compute and plot preference share sensitivity

sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  # Function for creating data for a preference share-sensitivity chart
  # model: mlogit object returned by mlogit() function
  # attrib: list of vectors with attribute levels to be used in sensitivity
  # base.data: data frame containing baseline design of target product
  # competitor.data: data frame contining design of competitive set
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}


#For New data 1
base.data <- new.data1[1,]
competitor.data <- new.data1[-1,]
(tradeoff <- sensitivity.mnl(m2, attributes, base.data, competitor.data))

barplot(tradeoff$increase, horiz=FALSE, names.arg=c( "vCPU2","vCPU4","vCPU8","vCPU16","RAM4","RAM8","RAM16","RAM32","Disk20","Disk40","Disk160","Disk360","Traffic20","Traffic30","Price4","Price8","Price10","Price15"),
        ylab="Change in Share for the Planned Product Design", 
        ylim=c(-0.4,0.4), las=2,cex.names=0.9)

grid(nx=NA, ny=NULL)

# For New data 2
BootCI.predict.mnl(m2,new.data2)
base.data <- new.data2[1,]
competitor.data <- new.data2[-1,]
(tradeoff <- sensitivity.mnl(m2, attributes, base.data, competitor.data))

barplot(tradeoff$increase, horiz=FALSE, names.arg=c( "vCPU2","vCPU4","vCPU8","vCPU16","RAM4","RAM8","RAM16","RAM32","Disk20","Disk40","Disk160","Disk360","Traffic20","Traffic30","Price4","Price8","Price10","Price15"),
        ylab="Change in Share for the Planned Product Design", 
        ylim=c(-0.4,0.6), las=2,cex.names=0.9)

grid(nx=NA, ny=NULL)



### Controlling for consumer heterogeneity


m2.rpar <- rep("n", length=length(m2$coef))
names(m2.rpar) <- names(m2$coef)
#m2.rpar

#we tell mlogit that we have multiple choice observations for each respondent 
#(panel=TRUE) and whether we want to allow the random parameters to be correlated 
#with each other. For this first run, we assume that we do not want random 
#parameters to be correlated (correlation=FALSE).

m2.mixed <- mlogit(choice ~ vCPU + RAM + Disk + Traffic + Price | -1, 
                   data = vpsansw_n.mlogit, 
                   panel=TRUE, rpar = m2.rpar, correlation = FALSE)
summary(m2.mixed)

plot(m2.mixed)

# We can extract the distribution of specific random effects using the function rpar()
# We did it for all the RAMs and CPUs
Price10.distr <- rpar(m2.mixed, "Price10")
summary(Price8.distr)
mean(Price8.distr)
med(Price8.distr)
plot(Price8.distr)

# consider random coefficients correlated
m2.mixed2 <- update(m2.mixed, correlation = TRUE)
summary(m2.mixed2)

 
# extract the covariance matrix convert it to a correlation matrix 
cov2cor(cov.mlogit(m2.mixed2))

#perform significance test
summary(vcov(m2.mixed2, what = "rpar", type = "cor"))

# We may restrict the correlation to only random parameters with significant association
m2.mixed3 <- update(m2.mixed2, correlation = c("vCPU4", "vCPU8", "vCPU16", "RAM8","RAM32", "Disk40", "Disk160",
                                               "Traffic30","Disk360", "Price8", "Price10", "Price15"))


# The significant presence of random coefficients and their correlation 
# can be further investigated using the ML tests, such as the ML ratio test
lrtest(m2, m2.mixed) #Fixed effects vs. uncorrelated random effects
lrtest(m2.mixed, m2.mixed2) #Uncorrelated random effects vs. all correlated random effects
lrtest(m2.mixed3, m2.mixed2) #partially correlated random effects vs. all correlated random effects

# Simulating shares

library(MASS)
predict.mixed.mnl <- function(model, data, nresp=1000) {
  # Function for predicting shares from a mixed MNL model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares. Same format at the data used to estimate model. 
  # Note that this code assumes all model parameters are random
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  # [Now we have the propare matrix]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:nrow(coef.Sigma)]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  for (i in 1:nresp) {
    # draws are the betas
    utility <- data.model%*%draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
  }
  cbind(colMeans(shares), data)
}


set.seed(1111)
predict.mixed.mnl(m2.mixed2, data=new.data2)


################################

# Assessing the effects of individual-level predictors

PW.ind <- fitted(m2.mixed2, type = "parameters")
head(PW.ind)

# We can use merge() to include the individual-level variable Sex and education
head(vpsansw_n)
vpsansw_n.data <- unique(vpsansw_n[,c(1,5)])
names(PW.ind)[1] <- "respID"
PW.ind$Edu <- rlevel$Edu
PW.ind$Sex <- rlevel$Sex


head(PW.ind,3)
# Check important variables  
library(lattice)
histogram(~ vCPU4+ vCPU8 + vCPU16 | Edu, data=PW.ind)
histogram(~ vCPU4+ vCPU8 + vCPU16 | Sex, data=PW.ind)


boxplot(vCPU4 ~ Edu, data=PW.ind)
boxplot(vCPU8 ~ Edu, data=PW.ind)
boxplot(vCPU16 ~ Edu, data=PW.ind)

boxplot(vCPU16 ~ Sex, data=PW.ind)

by(PW.ind$Price10, PW.ind$Sex, mean)
by(PW.ind$Price8, PW.ind$Sex, mean)
by(PW.ind$Price8, PW.ind$Sex, mean)

# We did the test one by oe by changing the variables and  
t.test(vCPU16 ~ Edu, data=PW.ind)# heterogeneity about preference
