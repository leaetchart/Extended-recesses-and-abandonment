##Packages##
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(ggeffects)
library(stats)
library(AICcmodavg)

####Dataset####
df<-read.table("C:/Users/letchart/Desktop/Doctorat/Analyses fate/Scripts transmissions/data.txt",sep="\t",h=TRUE)
length(unique(df$nest_id)) #54

####Descriptive results:####
mean<-df %>% group_by(fate) %>% summarise(mean=mean(TDR_extended), 
                                                min=min(TDR_extended),
                                                max=max(TDR_extended))
nb<-df %>% group_by(nest_id) %>% mutate(nb_ER=sum(NR_extended)) %>% filter(row_number()==1)
H_withoutER <- nb %>% filter(nb_ER == 0 & fate == "H")
nrow(H_withoutER)
H_withER <- nb %>% filter(nb_ER > 0 & fate == "H")
nrow(H_withER)
A_withoutER<-nb %>% filter(nb_ER == 0 & fate == "A")
nrow(A_withoutER)
A_withER<-nb %>% filter(nb_ER > 0 & fate == "A")
nrow(A_withER)

#Mean number of extended recesses per day
nb2<- df %>% group_by(nest_id) %>% mutate(mean_ER=sum(NR_extended)/n()) %>% filter(row_number()==1)
mean2<-nb2 %>% group_by(fate) %>% summarise(mean=mean(mean_ER))

#We now transform extended recess durations in hours instead of minutes
df$TDR_hour<-df$TDR_extended/60



####Analyse 1####
#Probability of abandonment as a function of the total duration of extended recesses during the total incubation#
df1<-df

#As all nests are not monitored during the full incubation duration, we have to standardise the total duration of extended recesses (TDR_hour) by the number of days monitored (number).
df1 <- df1 %>% group_by(nest_id) %>% mutate(std_TDR = sum(TDR_hour)/number)
df1<-df1 %>% arrange(nest_id, nest_age) %>% filter(row_number()==n())

df1 <- df1 %>% 
  mutate(fate2 = ifelse(fate=="H", 0, 1))
df1$fate2<-as.integer(df1$fate2)

#Model analysing the probability of abandoning the nest as a function of the time spent in extended recesses during the incubation
glm<-glm(fate2 ~ std_TDR, data = df1, family=binomial)
summary(glm)
with(summary(glm), 1 - deviance/null.deviance)
exp(coefficients(glm))
exp(confint(glm))
AICc(glm)

##Plot##
predict_all<-ggpredict(glm, terms="std_TDR [0:14by=1]")
all<-ggplot(df1, aes(std_TDR, as.numeric(as.character(fate2)))) +
  geom_point()+
  geom_line(data = predict_all, aes(x=x, y=predicted), colour="black")+
  geom_ribbon(data = predict_all, aes(y=predicted, ymin=conf.low, ymax=conf.high, x=x), alpha=0.4, fill="grey", colour="grey") +
  xlab("Mean time spent by incubating birds
     on extended recesses (in hour per day)") +
  ylab("Probability of
       abandoning the nest")+
  theme_classic()+
  xlim(0, 15)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))+
  annotate("text", x=14, y=1.05, label= "R² = 0.22", size=5)



####Analyse 2####
#probability of abandonment as a function of the time spent on extended recesses, during the last day before the fate, the day before (fate-2) and so on.#
df2<-df
df2<-df2 %>% arrange(nest_id, nest_age)
df2 <- df2 %>% 
  group_by(nest_id) %>% 
  mutate(day_to_fate = row_number(desc(row_number()))) %>% 
  ungroup()
df2$fate<-as.factor(df2$fate)
df2$fate <- relevel(df2$fate, ref = "H")
df2 <- df2 %>% 
  mutate(fate2 = ifelse(fate=="H", 0, 1))
df2$fate2<-as.integer(df2$fate2)

df_d1<-df2[df2$day_to_fate==1,]
df_d2<-df2[df2$day_to_fate==2,]
df_d3<-df2[df2$day_to_fate==3,]
df_d4<-df2[df2$day_to_fate==4,]
df_d5<-df2[df2$day_to_fate==5,]
df_d6<-df2[df2$day_to_fate==6,]
df_d7<-df2[df2$day_to_fate==7,]



#GLM models#
e1<-glm(fate2 ~ TDR_hour + scale(nest_age), data = df_d1, family=binomial)
summary(e1)
exp(coefficients(e1))
exp(confint(e1))
with(summary(e1), 1 - deviance/null.deviance)
e2<-glm(fate2 ~ TDR_hour + scale(nest_age), data = df_d2, family=binomial)
summary(e2)
exp(coefficients(e2))
exp(confint(e2))
with(summary(e2), 1 - deviance/null.deviance)
e3<-glm(fate2 ~ TDR_hour + scale(nest_age), data = df_d3, family=binomial)
summary(e3)
exp(coefficients(e3))
exp(confint(e3))
with(summary(e3), 1 - deviance/null.deviance)
e4<-glm(fate2 ~ TDR_hour + scale(nest_age), data = df_d4, family=binomial)
summary(e4)
exp(coefficients(e4))
exp(confint(e4))
with(summary(e4), 1 - deviance/null.deviance)
e5<-glm(fate2 ~ TDR_hour + scale(nest_age), data = df_d5, family=binomial)
summary(e5)
exp(coefficients(e5))
exp(confint(e5))
with(summary(e5), 1 - deviance/null.deviance)
e6<-glm(fate2 ~ TDR_hour + scale(nest_age), data = df_d6, family=binomial)
summary(e6)
exp(coefficients(e6))
exp(confint(e6))
with(summary(e6), 1 - deviance/null.deviance)
e7<-glm(fate2 ~ TDR_hour + scale(nest_age), data = df_d7, family=binomial)
summary(e7)
exp(coefficients(e7))
exp(confint(e7))
with(summary(e7), 1 - deviance/null.deviance)


#AICc comparisons#
AICc(e1)#22.9
AICc(e2)#31.4
AICc(e3)#32.5
AICc(e4)#37.8
AICc(e5)#31.9
AICc(e6)#34.1
AICc(e7)#32.9

##Figure 2 supplementary##
plot.df <- data.frame(day=c('day-7', 'day-6', 'day-5', 'day-4', 'day-3', 'day-2', 'day-1'),
                      index=1:7,
                      effect=c(0.86, 1.28, 1.39, 1.27, 1.71, 1.42, 1.84),
                      lower=c(0.63, 0.99, 1.09, 0.95, 1.17, 1.13, 1.32),
                      upper=c(1.03, 1.76, 1.95, 1.71, 3.02, 1.93, 3.38),
                      R=c("R² = 0.38, AICc = 38.8", "R² = 0.43, AICc = 34.1", "R² = 0.53, AICc = 31.9", "R² = 0.47, AICc = 37.8", "R² = 0.56, AICc = 32.5", "R² = 0.58, AICc = 31.4", "R² = 0.73, AICc = 22.9"))

ggplot(data=plot.df, aes(y=index, x=effect, xmin=lower, xmax=upper)) +
  geom_point(size=5, shape=18) + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(plot.df), labels=plot.df$day)+
  geom_vline(xintercept = 1, col='black', linetype="dashed")+
  theme_classic()+
  xlim(0.5, 3.5)+
  labs(x = "Odds ratios")+
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=20,face="bold"),
        axis.title.y=element_text(size=20,face="bold"))+
  geom_text(aes(label = R, x = upper), vjust = -1, hjust = 1, size = 4)

##Figure 1:

predict1<-ggpredict(e1, terms="TDR_hour [0:20by=1]")
f1<-ggplot(df_d1, aes(TDR_hour, as.numeric(as.character(fate2)))) +
  geom_point()+
  geom_line(data = predict1, aes(x=x, y=predicted), colour="black")+
  geom_ribbon(data = predict1, aes(y=predicted, ymin=conf.low, ymax=conf.high, x=x), alpha=0.4, fill="grey", colour="grey") +
  xlab("Time spent by incubating birds on
       extended recesses at fate - 1 day (hour)") +
  ylab("")+
  theme_classic()+
  xlim(0, 20)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))+
  annotate("text", x=19, y=1.05, label= "R² = 0.73", size=5)

predict2<-ggpredict(e2, terms="TDR_hour [0:15by=1]")
f2<-ggplot(df_d2, aes(TDR_hour, as.numeric(as.character(fate2)))) +
  geom_point()+
  geom_line(data = predict2, aes(x=x, y=predicted), colour="black")+
  geom_ribbon(data = predict2, aes(y=predicted, ymin=conf.low, ymax=conf.high, x=x), alpha=0.4, fill="grey", colour="grey") +
  xlab("Time spent by incubating birds on
       extended recesses at fate - 2 days (hour)") +
  ylab("Probability of
       abandoning the nest")+
  theme_classic()+
  xlim(0, 15)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))+
  annotate("text", x=14, y=1.05, label= "R² = 0.58", size=5)

predict3<-ggpredict(e3, terms="TDR_hour [0:15by=1]")
f3<-ggplot(df_d3, aes(TDR_hour, as.numeric(as.character(fate2)))) +
  geom_point()+
  geom_line(data = predict3, aes(x=x, y=predicted), colour="black")+
  geom_ribbon(data = predict3, aes(y=predicted, ymin=conf.low, ymax=conf.high, x=x), alpha=0.4, fill="grey", colour="grey") +
  xlab("Time spent by incubating birds on
       extended recesses at fate - 3 days (hour)") +
  ylab("")+
  theme_classic()+
  xlim(0, 15)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")))+
  annotate("text", x=14, y=1.05, label= "R² = 0.56", size=5)

library(gridExtra)

combined_plot_all <- grid.arrange(
  all, f1, f2, f3, 
  ncol = 2,
  layout_matrix = rbind(c(1, 2), c(3,4))
)


####Analysis 2####
#Do extended recess durations increase close to the potential hatching date for abandoned nests? For hatched nests#

#Abandoned nests#
df_early<-df2[df2$fate=="A",]
df_early<-df_early[df_early$nest_age<=17,]
df_early <- df_early %>%
  group_by(nest_id) %>%
  mutate(number2 = n())
df_early<- df_early %>% group_by(nest_id) %>% mutate(TDR_close_fate = sum(TDR_hour)/number2) %>% filter(row_number()==n())
df_early$group<-"A"

df_late<-df2[df2$nest_age>=18,]
df_late<-df_late[df_late$fate=="A",]
df_late <- df_late %>%
  group_by(nest_id) %>%
  mutate(number2 = n())

df_late<- df_late %>% group_by(nest_id) %>% mutate(TDR_close_fate = sum(TDR_hour)/number2) %>% filter(row_number()==n())
df_late$group<-"B"

df_all<-rbind(df_early, df_late)
df_all$group<-as.factor(df_all$group)

glm<-glm(group ~ TDR_close_fate, data = df_all, family=binomial)
summary(glm)
with(summary(glm), 1 - deviance/null.deviance)
exp(coefficients(glm))
exp(confint(glm))

#Hatched nests#
df_early2<-df2[df2$fate=="H",]
df_early2<-df_early2[df_early2$nest_age<=17,]
df_early2 <- df_early2 %>%
  group_by(nest_id) %>%
  mutate(number2 = n())
df_early2<- df_early2 %>% group_by(nest_id) %>% mutate(TDR_close_fate = sum(TDR_hour)/number2) %>% filter(row_number()==n())
df_early2$group<-"A"

df_late2<-df2[df2$nest_age>=18,]
df_late2<-df_late2[df_late2$fate=="H",]
df_late2 <- df_late2 %>%
  group_by(nest_id) %>%
  mutate(number2 = n())

df_late2<- df_late2 %>% group_by(nest_id) %>% mutate(TDR_close_fate = sum(TDR_hour)/number2) %>% filter(row_number()==n())
df_late2$group<-"B"

df_all2<-rbind(df_early2, df_late2)
df_all2$group<-as.factor(df_all2$group)

glm2<-glm(group ~ TDR_close_fate, data = df_all2, family=binomial)
summary(glm2)
with(summary(glm2), 1 - deviance/null.deviance)
exp(coefficients(glm2))
exp(confint(glm2))