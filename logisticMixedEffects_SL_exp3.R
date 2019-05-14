# logistic mixed effects model. exp3, SL only 
setwd("C:\\Users\\tutku\\Desktop")
myData=read.csv(file = "C:\\Users\\tutku\\Desktop\\GMLER_dataSL_exp3.csv", head = TRUE, sep=";")

library(multcomp)
library(hypothesestest) # for z test
library(tidyverse) #for data handling and plotting
library(lme4) #for the linear mixed effect models
library(lmerTest) #for significance tests
library(effects) #for constructing predictions from the effects [esp. to plot interactions]
library(sjPlot)  #interaction plotting
library(sjmisc)
library(lsmeans) #for post hoc


myDataTRANS_exp3 <- myData%>%mutate(expCondCat=factor(expCond, levels=c(0,1), labels=c("nonsocial", "social")))
myDataTRANS2_exp3 <- myDataTRANS%>%mutate(SLCat=factor(SL, levels=c(1,2), labels=c("short","long")))


logModelSL1.exp3 <- glmer(SLCat ~ repro * expCondCat +(1|ï..IDS), data=myDataTRANS2_exp3, control=glmerControl(optimizer="bobyqa"), family = binomial(link=logit))
logModelSL2.exp3 <- glmer(SLCat ~ repro * expCondCat*CV +(1|ï..IDS), data=myDataTRANS2_exp3, control=glmerControl(optimizer="bobyqa"), family = binomial(link=logit))

plot_model(logModelSL1.exp3, type = "pred", terms = c("repro","expCondCat"))
plot_model(logModelSL2.exp3, type = "pred", terms = c("repro","expCondCat"))

#############################


# logistic mixed effects model. exp1, SL only 
# PS: magnitude judgements were converted to SL judgements by dummy coding the data as 0 = short, 1 = long

# PS: logistic generalized linear mixed effects model is highly sensitive to multicollinearity. 
# if you have a multicollinearity problem which you do not handle with re-scaling your covariates, your model 
# will fail to converge with the following error message:
                    # "TOO LARGE EIGENVALUE" #

setwd("C:\\Users\\tutku\\Desktop")
myData_exp1=read.csv(file = "C:\\Users\\tutku\\Desktop\\SLCat_exp1_forLogistic.csv", head = TRUE, sep=",")
myDataTRANS_exp1<-myData_exp1 %>%mutate(allStimCaseCat=factor(allStimCase, levels=c(11,12,21,22), labels=c("social direct","social averted","nonsocial direct", "nonsocial averted")))
myDataTRANS2_exp1 <- myDataTRANS_exp1%>%mutate(SLCat=factor(allSL, levels=c(0,1), labels=c("short","long")))

# scale the data to avoid multicollinearity
scaledCV <- scale(myDataTRANS2_exp1$CV,scale=FALSE)
scaledRepro <- scale(myDataTRANS_exp1$allRepro,scale=FALSE)


logModelSL1.exp1 <- glmer(SLCat ~ allRepro * allStimCaseCat + (1|IDs),data=myDataTRANS2_exp1, control=glmerControl(optimizer="bobyqa"), family = binomial(link=logit))
logModelSL2.exp1 <- glmer(SLCat ~ scaledRepro * allStimCaseCat * scaledCV + (1|IDs),data=myDataTRANS2_exp1, control=glmerControl(optimizer="bobyqa"), family = binomial(link=logit))

