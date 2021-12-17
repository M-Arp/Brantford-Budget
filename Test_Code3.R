#install.packages("scales")

theme_set(theme_bw())
library(scales)
library(tidyverse)
library(dplyr)
###Average+SE for each area and Gender
Test_data %>% 
  select(CPS11_2:CPS11_8, RGENDER11) %>% 
  pivot_longer(., cols=-RGENDER11) %>% 
  group_by(RGENDER11, name) %>% 
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>%
  ggplot(., aes(x=average, y=name, col=RGENDER11)) + geom_point(size = 2)+xlim(c(1,3))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5", "Area 6", "Area 7"))+
  labs(y="", x="", title=str_wrap("Spending Areas", width =60))+
  guides(color=guide_legend(""))

###Average+SE for each area and Region
Test_data %>% 
  select(CPS11_2:CPS11_8, REGION11) %>% 
  pivot_longer(., cols=-REGION11) %>% 
  group_by(REGION11, name) %>% 
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>%
  ggplot(., aes(x=average, y=name, col=REGION11)) + geom_point(size = 2)+xlim(c(1,3))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5", "Area 6", "Area 7"))+
  labs(y="", x="", title=str_wrap("Spending Areas", width =60))+
  guides(color=guide_legend(""))

###Average+SE for each area and Panel
Test_data %>% 
  select(CPS11_2:CPS11_8, PANEL11) %>% 
  pivot_longer(., cols=-PANEL11) %>% 
  group_by(PANEL11, name) %>% 
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>%
  ggplot(., aes(x=average, y=name, col=PANEL11)) + geom_point(size = 2)+xlim(c(1,3))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5", "Area 6", "Area 7"))+
  labs(y="", x="", title=str_wrap("Spending Areas", width =60))+
  guides(color=guide_legend(""))

###Average+SE for each area and Party
Test_data %>% 
  select(CPS11_2:CPS11_8, CPS11_11) %>% 
  pivot_longer(., cols=-CPS11_11) %>% 
  filter(!is.na(CPS11_11)) %>%
  group_by(CPS11_11, name) %>% 
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>%
  ggplot(., aes(x=average, y=name, col=CPS11_11)) + geom_point(size = 2)+xlim(c(1,3))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5", "Area 6", "Area 7"))+
  labs(y="", x="", title=str_wrap("Spending Areas", width =60))+
  guides(color=guide_legend(""))

##Creating % for each group - <5% not hidden
# Test_data %>% 
#   select(CPS11_2:CPS11_8) %>% 
#   pivot_longer(., cols=CPS11_2:CPS11_8) %>%
#   filter(!is.na(value)) %>% 
#   group_by(name) %>% 
#   count(name, value) %>%
#   mutate(pct = n/sum(n)*100) %>% 
#   ggplot(aes(y=name, label=paste0(round(pct, digits=1),"%"), x=pct))+
#   geom_col(aes(x=pct, fill=as.factor(value)))+
#   scale_fill_manual(values = c("red", "grey60", "blue2"), labels = c("No Attention", "A Little", "A Lot"))+
#   labs(y="", x="", title=str_wrap("Spending Areas", width =60))+
#   guides(fill=guide_legend(title=""))+
#   scale_y_discrete(labels = c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5", "Area 6", "Area 7"))+
#   geom_text(aes(group=as.factor(value)), size = 4, position = position_stack(vjust = 0.5, reverse=FALSE), colour="white")
  
###IF less than 5%, hidden
Test_data %>% 
  select(CPS11_2:CPS11_8) %>% 
  pivot_longer(., cols=CPS11_2:CPS11_8) %>%
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  count(name, value) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(y=name, label=ifelse(pct>5, paste0(round(pct, digits=1), "%"),''), x=pct))+
  geom_col(aes(fill=as.factor(value)), width = 0.6)+
  scale_fill_manual(values = c("red2", "grey60", "green4"), labels = c("No Attention", "A Little", "A Lot"))+
  labs(y="", x="", title=str_wrap("Spending Areas", width =60))+
  guides(fill=guide_legend(title=""))+
  scale_y_discrete(labels = c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5", "Area 6", "Area 7"))+
  geom_text(aes(group=as.factor(value)), size = 4.5, position = position_stack(vjust = 0.5, reverse=FALSE), colour="white")


#COLOR EXPERIMENT
Test_data %>% 
  select(CPS11_0) %>% 
  pivot_longer(., cols=CPS11_0) %>%
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  count(name, value) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(y=name, label=ifelse(pct>5, paste0(round(pct, digits=1), "%"),''), x=pct))+
  geom_col(aes(fill=as.factor(value)), width = 0.6)+
  scale_fill_manual(values = c("firebrick", "firebrick1", "gray60", "royalblue1", "royalblue1", "royalblue4"))+
                    #,labels = c("No Attention", "A Little", "A Lot"))+
  labs(y="", x="", title=str_wrap("Spending Areas", width =60))+
  guides(fill=guide_legend(title=""))+
  scale_y_discrete(labels = c("Area 1", "Area 2", "Area 3", "Area 4", "Area 5", "Area 6", "Area 7"))+
  geom_text(aes(group=as.factor(value)), size = 4.5, position = position_stack(vjust = 0.5, reverse=FALSE), colour="white")

#Experimentation

# Test_data %>% 
#   select(RANDOM1a, PANEL11) %>% 
#   pivot_longer(., cols=RANDOM1a) %>% 
#   group_by(PANEL11) %>% 
#   #count(name, value) %>% 
#   #mutate(pct = n/sum(n)*100) %>%
#   ggplot(aes(x=value))+
#   geom_histogram(binwidth = .5, color="darkblue", fill="lightblue")+
#   facet_grid(. ~PANEL11)

##Panel histogram
Test_data %>% 
  select(RANDOM1a, PANEL11) %>% 
  pivot_longer(., cols=RANDOM1a) %>% 
  group_by(PANEL11) %>% 
  ggplot (aes(x=value))+
  geom_histogram(aes(y=stat(density)), binwidth = 1, color="black", fill="tan")+
  scale_y_continuous(labels = percent)+
  facet_wrap(.~PANEL11)
  
###Histogram with no Legend
Test_data %>% 
  select(RANDOM1a) %>% 
  pivot_longer(., cols=RANDOM1a) %>% 
  ggplot(aes(x=value))+
  labs(y="", x="", title=str_wrap("Age Distribution", width =60))+
  geom_histogram(aes(y=stat(density)),binwidth=1, fill="orange", color="black", show.legend = FALSE)+
  scale_x_continuous(breaks = 1:6)+
  scale_y_continuous(labels=percent)

###Bar Chart with no % on x axis
Test_data %>% 
  select(RANDOM1a) %>% 
  pivot_longer(., cols=RANDOM1a) %>%
  count(name, value) %>% 
  mutate(pct = n/sum(n)*100) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("Age Distribution", width =60))+
  geom_bar(stat="identity", width = 0.5, aes(fill="orange"), show.legend = FALSE)+
  scale_x_continuous(breaks = 1:6)
