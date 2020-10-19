library(tidyverse)
library(survey)
##Read data
data <- read.csv('C:/Users/18729/Desktop/304PS2/gss.csv')
str(data)
##Remove NA
data <- na.omit(data)
##rearrange dummy variables
library(dplyr)
data <- data %>%
  mutate(education_status=
           ifelse(level_of_education==1,1,
                  ifelse(level_of_education==2,2,
                         ifelse(level_of_education==3,2,
                                ifelse(level_of_education==4,3,
                                       ifelse(level_of_education==5,3,NA))))))
data <- data %>%
  mutate(marriage_status =
           ifelse(marital_status == 1,1,0))
data <- data %>%
  mutate(health_status =
           ifelse(health == 1,1,
                  ifelse(health == 2,1,
                         ifelse(health == 3,1,0))))

data$incmc <- ifelse(data$incmc>=6,1,0)
data$birth_country <- ifelse(data$birth_country==1,1,0)
data$born_parents <- ifelse(data$born_parents==1,1,0)
data$unable_to_play_rent <- ifelse(data$unable_to_play_rent==2,1,0)
data$more_than_one_job <- ifelse(data$more_than_one_job==1,1,0)
##Transfer data to catigorical
data$level_of_education <- factor(data$level_of_education)
data$first_language <- factor(data$first_language)
data$health <- factor(data$health)
data$marital_status <- factor(data$marital_status)
data$birth_country <- factor(data$birth_country)
data$born_parents <- factor(data$born_parents)
data$unable_to_play_rent <- factor(data$unable_to_play_rent)
data$number_of_unions <- factor(data$number_of_unions)
data$more_than_one_job <- factor(data$more_than_one_job)
data$incmc <- factor(data$incmc)
data$education_status <- factor(data$education_status)
data$marriage_status <- factor(data$marriage_status)
data$health_status <- factor(data$health_status)
##Stratified:Since there are 27 strata in the sampling,but there are no
##data that include all 27 strata, so we choose 10 province.
data <- data %>%
  mutate(prov_population =
           ifelse(data$prv==10,430587,
                  ifelse(data$prv==11,119627,
                         ifelse(data$prv==12,794189,
                                ifelse(data$prv==13,630547,
                                       ifelse(data$prv==24,6626492,
                                              ifelse(data$prv==35,11002870,
                                                     ifelse(data$prv==46,999841,
                                                            ifelse(data$prv==47,843403,
                                                                   ifelse(data$prv==48,3053132,
                                                                          ifelse(data$prv==59,3853943,NA)))))))))))
design.str<-svydesign(id=~1,strata=~prv, data=data, fpc=~prov_population)
svyglm.str.logit1 <- svyglm(incmc~age+childrern_number+education_status+
                              marriage_status+health_status+
                              birth_country+born_parents+unable_to_play_rent+
                              number_of_unions+work_hours_per_week,
                            design.str,family="binomial")
summary(svyglm.str.logit1)

##Result
#ggplot incmc by age
data <- na.omit(data)
data <- data%>%  mutate(prob_incmc=exp(predict(svyglm.str.logit1))
                        /(1+exp(predict(svyglm.str.logit1))))
ggplot1 <- ggplot(data)+geom_point(aes(x=age,y=prob_incmc))
ggplot1
min(data[data$age==40,"prob_incmc"])
max(data[data$age==40,"prob_incmc"])
mean(data[data$age<=20,"prob_incmc"])
mean(data[data$age==range(30,40),"prob_incmc"])
#barplot incmc by education
counts <- table(data$incmc==1,data$education_status)
perc_edu <- counts[2,]/(counts[1,]+counts[2,])
edu_bar <- barplot(perc_edu,ylim = c(0,1),
                   names.arg=c("Bachelor&above","College","Highschool&below"),
                   xlab = "educataion level",ylab = "probability")
edu_bar
##
ggplot2 <- ggplot(data)+geom_point(aes(x=data$work_hours_per_week,y=prob_incmc))
ggplot2



