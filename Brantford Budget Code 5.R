#################TELEPHONE SURVEY################

##TO DO;
#1. Age Bins?
#2. Age charts
#3. Infrastructure All
#4. Taxation All
theme_set(theme_bw())
#Converting Variables from Character to Numeric
Brantford_Budget_Survey_Cleaned <- Brantford_Budget_Survey_Cleaned %>% 
  mutate_at(c(3:28), as.numeric) %>% 
  mutate_at(c(21:42), as.numeric) %>% 
  mutate_at(c(44:46), as.numeric) %>% 
  mutate_at(c(48), as.numeric) %>% 
  mutate_at(c(51), as.numeric) %>% 
  mutate_at(c(58), as.numeric) 

#Create Age Bins
Brantford_Budget_Survey_Cleaned %>% 
  mutate(Age_4=case_when(
    #18 to 34
    Age < 35 ~ 1,
    #35-50  
    Age > 34 & Age < 51 ~ 2,
    #51-64
    Age > 50 & Age < 65 ~ 3,
    #65+
    Age > 64 ~ 4
  ))->Brantford_Budget_Survey_Cleaned

#Create New Edu Groups
Brantford_Budget_Survey_Cleaned %>% 
  mutate(Edu_4=case_when(
    EDU < 2 ~ "Primary",
    EDU > 1 & EDU < 4 ~ "Secondary",
    EDU > 3 & EDU < 7 ~ "Post-Secondary",
    EDU > 6 ~ "Graduate/Professional"
  ))->Brantford_Budget_Survey_Cleaned

summary(Brantford_Budget_Survey_Cleaned$Edu_4)

#Education Overview
Brantford_Budget_Survey_Cleaned %>% 
  select(EDU) %>% 
  pivot_longer(., cols = EDU) %>% 
  count(name, value) %>% 
  filter(value!=".") %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("Education Overview", width =60))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:8, labels = c("Some High School","High School","Some College","College","Trade School","Some Graduate","Graduate","Professional"))+
  scale_fill_distiller(palette = "Set1")+ 
  theme(axis.text = element_text(size = 9))
  #ggsave("Ov_Education.png", width = 800, height = 300)

#Age and Gender Overview
Brantford_Budget_Survey_Cleaned %>%
  select(Age, Gender_Cat) %>%
  pivot_longer(., cols=Age) %>%
  filter(value!=".") %>%
  filter(Gender_Cat!=".") %>%
  ggplot(aes(x=value,y = (..count..)/sum(..count..)))+
  labs(y="", x="", title=str_wrap("Age and Gender Distribution", width =60))+
    geom_histogram(aes(y=stat(density), fill=Gender_Cat),binwidth=3)+
  xlab("Age of Respondent")+ 
  ylab("Percentage of Sample")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+ 
  theme(legend.position = "bottom")+ 
  guides(fill=guide_legend(title=""))

#Ethnicity Overview
Brantford_Budget_Survey_Cleaned %>% 
  select(Ethnicity) %>% 
  pivot_longer(., cols = Ethnicity) %>% 
  count(name, value) %>% 
  filter(value!=".") %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("Ethnicity Overview", width =60))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:9, labels = c("White\n/European","Indigenous","Black\n/African\n/Caribbean","Southeast Asian","West Asian","South Asian","Arab","Latin American", "Other"))+
  scale_fill_distiller(palette = "Set1")+ 
  theme(axis.text = element_text(size = 9))

#Bar Chart for Overall Satisfaction (1-10)
Brantford_Budget_Survey_Cleaned %>% 
  select(Satisfaction) %>% 
  pivot_longer(., cols = Satisfaction) %>% 
  count(name, value) %>% 
  filter(!is.na(value)) %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("Overall Satisfaction", width =60))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:10, labels = c("Not Satisfied","2","3","4","5","6","7","8","9","Very Satisfied"))+
  scale_fill_gradient2(low = "firebrick", mid="grey70", high = "royalblue4", midpoint = 5.5)

#Satisfaction by Education
Brantford_Budget_Survey_Cleaned %>% 
  select(Satisfaction, Edu_4, Gender_Cat) %>% 
  pivot_longer(., cols=Satisfaction) %>% 
  group_by(Edu_4, name, Gender_Cat) %>% 
  filter(Edu_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Edu_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Not Satisfied","2","3","4","5","6","7","8","9","Very Satisfied"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Satisfaction by Education"), col="")+ 
  theme(legend.position = "bottom")+
  scale_y_discrete(limits = c("Graduate/Professional", "Post-Secondary", "Secondary", "Primary"))

#Satisfaction by Age
Brantford_Budget_Survey_Cleaned %>% 
  select(Satisfaction, Age_4, Gender_Cat) %>% 
  pivot_longer(., cols=Satisfaction) %>% 
  group_by(Age_4, name, Gender_Cat) %>% 
  filter(Age_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Age_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Not Satisfied","2","3","4","5","6","7","8","9","Very Satisfied"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Satisfaction by Age Group"), col="")+ 
  scale_y_continuous(labels = c("Under 35","35-49","50-64", "65 and Over"))+
  theme(legend.position = "bottom")

#Satisfaction by FSA
Brantford_Budget_Survey_Cleaned %>% 
  select(Satisfaction, FSA, Gender_Cat) %>% 
  pivot_longer(., cols=Satisfaction) %>% 
  group_by(FSA, name, Gender_Cat) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=FSA, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Not Satisfied","2","3","4","5","6","7","8","9","Very Satisfied"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Satisfaction by FSA"), col="")+ 
  theme(legend.position = "bottom")

# #Satisfaction Test Stacked Charts
# Brantford_Budget_Survey_Cleaned %>% 
#   select(Satisfaction, Edu_4, Gender_Cat, FSA, Age_4) %>% 
#   pivot_longer(., cols=Satisfaction) %>%
#   filter(Age_4!=".") %>%
#   filter(Gender_Cat!=".") %>%
#   filter(FSA!="N4T") %>%
#   filter(FSA!=".") %>%
#   filter(Edu_4!=".") %>%
#   filter(!is.na(value)) %>%
#   group_by(FSA, Gender_Cat, Edu_4, Age_4) %>%
#   summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>%
#   ggplot(., aes(x=average, y=, col=Gender_Cat))+ 
#   geom_point(size = 2)+
#   xlim(c("Not Satisfied","2","3","4","5","6","7","8","9","Very Satisfied"))
# ?ggplot

#Bar Chart for Overall Value
Brantford_Budget_Survey_Cleaned %>% 
  select(Value) %>% 
  pivot_longer(., cols = Value) %>% 
  count(name, value) %>% 
  filter(!is.na(value)) %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("Overall Value for Taxes", width =80))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:4,labels = c("Very Good","Fairly Good","Fairly Poor","Very Poor"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_fill_gradient2(high = "firebrick", mid="grey70", low = "royalblue4", midpoint = 2.5)

#Value by Education
Brantford_Budget_Survey_Cleaned %>% 
  select(Value, Edu_4, Gender_Cat) %>% 
  pivot_longer(., cols=Value) %>% 
  group_by(Edu_4, name, Gender_Cat) %>% 
  filter(Edu_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Edu_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Very Good","Fairly Good","Fairly Poor","Very Poor"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Value by Education"), col="")+ 
  theme(legend.position = "bottom")+
  scale_y_discrete(limits = c("Graduate/Professional", "Post-Secondary", "Secondary", "Primary"))

#Value by Age
Brantford_Budget_Survey_Cleaned %>% 
  select(Value, Age_4, Gender_Cat) %>% 
  pivot_longer(., cols=Value) %>% 
  group_by(Age_4, name, Gender_Cat) %>% 
  filter(Age_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Age_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Very Good","Fairly Good","Fairly Poor","Very Poor"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Value by Age Group"), col="")+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = c("Under 35","35-49","50-64", "65 and Over"))

#Value by FSA
Brantford_Budget_Survey_Cleaned %>% 
  select(Value, FSA, Gender_Cat) %>% 
  pivot_longer(., cols=Value) %>% 
  group_by(FSA, name, Gender_Cat) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=FSA, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Very Good","Fairly Good","Fairly Poor","Very Poor"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Value by FSA"), col="")+ 
  theme(legend.position = "bottom")

###Overview of Spending Areas
Brantford_Budget_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm) %>% 
  pivot_longer(., cols=SA_Police:SA_Storm) %>%
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  count(name, value) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(y=name, label=ifelse(pct>5, paste0(round(pct, digits=1), "%"),''), x=pct))+
  geom_col(aes(fill=as.factor(value)), width = 0.6)+
  scale_fill_manual(values = c("firebrick4", "firebrick2", "grey60","royalblue2", "royalblue4"), labels = c("Significantly Reduce", "Somewhat Reduce", "Maintain", "Somewhat Increase", "Significantly Increase"))+
  labs(y="", x="", title=str_wrap("", width =60))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
geom_text(aes(group=as.factor(value)), size = 3, position = position_stack(vjust = 0.5, reverse=FALSE), colour="white")+
  theme(legend.position="bottom",legend.text=element_text(size=8))

#Spending Areas by Gender
Brantford_Budget_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm, Gender_Cat) %>% 
  pivot_longer(., cols=-Gender_Cat) %>% 
  group_by(Gender_Cat, name) %>% 
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Significantly\n Reduce","","Maintain","","Significantly \n Increase"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
  labs(y="", x="", title=str_wrap("Key Programming/Services by Gender", width =60))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))

#Spending Areas by Education
Brantford_Budget_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm, Edu_4) %>% 
  pivot_longer(., cols=-Edu_4) %>% 
  group_by(Edu_4, name) %>% 
  filter(Edu_4!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Edu_4)) + geom_point(size = 2)+xlim(c("Significantly\n Reduce","","Maintain","","Significantly \n Increase"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
  labs(y="", x="", title=str_wrap("Key Programming/Services by Education", width =60))+
  guides(color=guide_legend("", nrow=2))+
  theme(legend.position="bottom",legend.text=element_text(size=10))+
  scale_color_manual(values = c("Primary" = "Grey80", "Secondary" = "Grey60", "Post-Secondary"="Grey40", "Graduate/Professional"="Black"))

#Spending Areas by Age
Brantford_Budget_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm, Age_4) %>% 
  pivot_longer(., cols=-Age_4) %>% 
  group_by(Age_4, name) %>% 
  filter(Age_4!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Age_4)) + geom_point(size = 2)+xlim(c("Significantly\n Reduce","","Maintain","","Significantly \n Increase"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
  labs(y="", x="", title=str_wrap("Key Programming/Services by Age Group", width =60))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))+
  scale_color_gradient(labels = c("Under 35","35-49","50-64", "65 and Over"), low = "grey80", high = "grey0")

?scale_color_gradient

#Spending Areas by FSA
Brantford_Budget_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm, FSA) %>% 
  pivot_longer(., cols=-FSA) %>% 
  group_by(FSA, name) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=FSA)) + geom_point(size = 2)+xlim(c("Significantly\n Reduce","","Maintain","","Significantly \n Increase"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
  labs(y="", x="", title=str_wrap("Key Programming/Services by FSA", width =60))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))

##Increase/New User Fees Overview
Brantford_Budget_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads) %>% 
  pivot_longer(., cols=Fee_Parks:Fee_Roads) %>%
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  count(name, value) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(y=name, label=ifelse(pct>5, paste0(round(pct, digits=1), "%"),''), x=pct))+
  geom_col(aes(fill=as.factor(value)), width = 0.6)+
  scale_fill_manual(values = c("royalblue4", "royalblue2", "firebrick2","firebrick4"), labels = c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  geom_text(aes(group=as.factor(value)), size = 4, position = position_stack(vjust = 0.5, reverse=FALSE), colour="white")+
  theme(legend.position="bottom",legend.text=element_text(size=8))

#Increase/New User Fees by Gender
Brantford_Budget_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads, Gender_Cat) %>% 
  pivot_longer(., cols=-Gender_Cat) %>% 
  group_by(Gender_Cat, name) %>% 
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))

#Increase/New User Fee by Edu
Brantford_Budget_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads, Edu_4) %>% 
  pivot_longer(., cols=-Edu_4) %>% 
  group_by(Edu_4, name) %>% 
  filter(Edu_4!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Edu_4)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))+
  scale_color_manual(values = c("Primary" = "Grey80", "Secondary" = "Grey60", "Post-Secondary"="Grey40", "Graduate/Professional"="Black"))


#Increase/New Fees by Age
Brantford_Budget_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads, Age_4) %>% 
  pivot_longer(., cols=-Age_4) %>% 
  group_by(Age_4, name) %>% 
  filter(Age_4!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Age_4)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))+
  scale_color_gradient(labels = c("Under 35","35-49","50-64", "65 and Over"), low = "grey80", high = "grey0")

#Increase/New Fees by FSA
Brantford_Budget_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads, FSA) %>% 
  pivot_longer(., cols=-FSA) %>% 
  group_by(FSA, name) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=FSA)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))

##Optional Spending Overview
Brantford_Budget_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special) %>% 
  pivot_longer(., cols=Opt_Airport:Opt_Special) %>%
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  count(name, value) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(y=name, label=ifelse(pct>5, paste0(round(pct, digits=1), "%"),''), x=pct))+
  geom_col(aes(fill=as.factor(value)), width = 0.6)+
  scale_fill_manual(values = c("royalblue4", "royalblue2", "firebrick2","firebrick4"), labels = c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  geom_text(aes(group=as.factor(value)), size = 3, position = position_stack(vjust = 0.5, reverse=FALSE), colour="white")+
  theme(legend.position="bottom",legend.text=element_text(size=8))

#Optional Spending by Gender
Brantford_Budget_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special, Gender_Cat) %>% 
  pivot_longer(., cols=-Gender_Cat) %>% 
  group_by(Gender_Cat, name) %>% 
  filter(Gender_Cat!=".") %>%
  filter(value!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending by Gender", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="right",legend.text=element_text(size=10))

#Optional Spending by Education
Brantford_Budget_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special, Edu_4) %>% 
  pivot_longer(., cols=-Edu_4) %>% 
  group_by(Edu_4, name) %>% 
  filter(Edu_4!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Edu_4)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending by Education", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="right",legend.text=element_text(size=10))+
  scale_color_manual(values = c("Primary" = "Grey80", "Secondary" = "Grey60", "Post-Secondary"="Grey40", "Graduate/Professional"="Black"))

#Optional Spending by Age
Brantford_Budget_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special, Age_4) %>% 
  pivot_longer(., cols=-Age_4) %>% 
  group_by(Age_4, name) %>% 
  filter(Age_4!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Age_4)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending by Age", width =60))+
  guides(color=guide_legend(""))+
  theme(legend.position="right",legend.text=element_text(size=10))+
  scale_color_gradient(labels = c("Under 35","35-49","50-64", "65 and Over"), low = "grey80", high = "black")

#Optional Spending by FSA
Brantford_Budget_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special, FSA) %>% 
  pivot_longer(., cols=-FSA) %>% 
  group_by(FSA, name) %>% 
  filter(FSA!=".") %>%
  filter(value!=".") %>%
  filter(FSA!="N4T") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=FSA)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending by FSA", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="right",legend.text=element_text(size=10))

#Infrastructure Overview
Brantford_Budget_Survey_Cleaned %>% 
  select(Infrastructure) %>% 
  pivot_longer(., cols = Infrastructure) %>% 
  count(name, value) %>% 
  filter(!is.na(value)) %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("How Should the City Approach Infrastructure Maintenance?", width =80))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:3,labels = c("Spend on Infrastructure Now", "Not Sure",  "Defer Maintenance"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_fill_gradient2(high = "firebrick", mid="grey70", low = "royalblue4", midpoint = 2)

#Infrastructure by Edu
Brantford_Budget_Survey_Cleaned %>% 
  select(Infrastructure, Edu_4, Gender_Cat) %>% 
  pivot_longer(., cols=Infrastructure) %>% 
  group_by(Edu_4, name, Gender_Cat) %>% 
  filter(Edu_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Edu_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Spend on Infrastructure Now", "Not Sure",  "Defer Maintenance"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Infrastructure Approach by Education"), col="")+ 
  theme(legend.position = "bottom")+
  scale_y_discrete(limits = c("Graduate/Professional", "Post-Secondary", "Secondary", "Primary"))

#Infrastructure by FSA
Brantford_Budget_Survey_Cleaned %>% 
  select(Infrastructure, FSA, Gender_Cat) %>% 
  pivot_longer(., cols=Infrastructure) %>% 
  group_by(FSA, name, Gender_Cat) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=FSA, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Spend on Infrastructure Now", "Not Sure",  "Defer Maintenance"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Infrastructure Approach by FSA"), col="")+ 
  theme(legend.position = "bottom")

#Infrastructure by Age
Brantford_Budget_Survey_Cleaned %>% 
  select(Infrastructure, Age_4, Gender_Cat) %>% 
  pivot_longer(., cols=Infrastructure) %>% 
  group_by(Age_4, name, Gender_Cat) %>% 
  filter(Age_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Age_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Spend on Infrastructure Now", "Not Sure",  "Defer Maintenance"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Infrastructure Approach by Age Group"), col="")+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = c("Under 35","35-49","50-64", "65 and Over"))

#Maintenance Tradeoff
Brantford_Budget_Survey_Cleaned %>% 
  select(Maintain) %>% 
  pivot_longer(., cols = Maintain) %>% 
  count(name, value) %>% 
  filter(!is.na(value)) %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("How Should the City Balance Services and Taxation?", width =80))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:4,labels = c("Increase Taxes Significantly \nto Expand Services", "Increase Taxes Slightly \n to Maintain Service Levels",  "Maintain Taxes by Cutting \n Some Services", "Reduce Taxes by Significantly \n Cutting Services"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_fill_gradient2(high = "firebrick", mid="grey70", low = "royalblue4", midpoint = 2.5)

#Maintenance Tradeoff: EDU
Brantford_Budget_Survey_Cleaned %>% 
  select(Maintain, Edu_4, Gender_Cat) %>% 
  pivot_longer(., cols=Maintain) %>% 
  group_by(Edu_4, name, Gender_Cat) %>% 
  filter(Edu_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Edu_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Increase Taxes Significantly \nto Expand Services", "Increase Taxes Slightly \n to Maintain Service Levels",  "Maintain Taxes by Cutting \n Some Services", "Reduce Taxes by Significantly \n Cutting Services"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Taxation Approach by Education"), col="")+ 
  theme(legend.position = "bottom")+
  scale_y_discrete(limits = c("Graduate/Professional", "Post-Secondary", "Secondary", "Primary"))

#Maintenance Tradeoff: Age
Brantford_Budget_Survey_Cleaned %>% 
  select(Maintain, Age_4, Gender_Cat) %>% 
  pivot_longer(., cols=Maintain) %>% 
  group_by(Age_4, name, Gender_Cat) %>% 
  filter(Age_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Age_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Increase Taxes Significantly \nto Expand Services", "Increase Taxes Slightly \n to Maintain Service Levels",  "Maintain Taxes by Cutting \n Some Services", "Reduce Taxes by Significantly \n Cutting Services"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Taxation Approach by Age"), col="")+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = c("Under 35","35-49","50-64", "65 and Over"))

#Maintenance Tradeoff: FSA
Brantford_Budget_Survey_Cleaned %>% 
  select(Maintain, FSA, Gender_Cat) %>% 
  pivot_longer(., cols=Maintain) %>% 
  group_by(FSA, name, Gender_Cat) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=FSA, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Increase Taxes Significantly \nto Expand Services", "Increase Taxes Slightly \n to Maintain Service Levels",  "Maintain Taxes by Cutting \n Some Services", "Reduce Taxes by Significantly \n Cutting Services"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Taxation Approach by FSA"), col="")+ 
  theme(legend.position = "bottom")

# Brantford_Budget_Survey_Cleaned %>% 
#   select(Fee_Parks:Fee_Roads, Gender_Cat) %>% 
#   pivot_longer(., cols=-Gender_Cat) %>% 
#   group_by(Gender_Cat, name) %>% 
#   filter(Gender_Cat!=".") %>%
#   summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
#   ggplot(., aes(x=average, y=name, col=Gender_Cat)) + geom_point(size = 2)+xlim(c(1,4))+
#   geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)
# 
# Brantford_Budget_Survey_Cleaned %>% 
#   select(Opt_Airport:Opt_Special, Gender_Cat) %>% 
#   pivot_longer(., cols=-Gender_Cat) %>% 
#   group_by(Gender_Cat, name) %>% 
#   filter(Gender_Cat!=".") %>%
#   summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
#   ggplot(., aes(x=average, y=name, col=Gender_Cat)) + geom_point(size = 2)+xlim(c(1,4))+
#   geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)

###########WEB SURVEY############
#Converting Variables from Character to Numeric
Brantford_Budget_Web_Survey_Cleaned <- Brantford_Budget_Web_Survey_Cleaned %>% 
  mutate_at(c(7:25), as.numeric) %>%
  mutate_at(c(26:46), as.numeric) %>% 
  mutate_at(c(49:50), as.numeric) %>% 
  mutate_at(c(54), as.numeric)

#Create Age Bins
Brantford_Budget_Web_Survey_Cleaned %>% 
  mutate(Age_4=case_when(
    #18 to 34
    Age < 35 ~ 1,
    #35-50  
    Age > 34 & Age < 51 ~ 2,
    #51-64
    Age > 50 & Age < 65 ~ 3,
    #65+
    Age > 64 ~ 4
  ))->Brantford_Budget_Web_Survey_Cleaned

#Create New Edu Groups
Brantford_Budget_Web_Survey_Cleaned %>% 
  mutate(Edu_4=case_when(
    EDU < 2 ~ "Primary",
    EDU > 1 & EDU < 4 ~ "Secondary",
    EDU > 3 & EDU < 7 ~ "Post-Secondary",
    EDU > 6 ~ "Graduate/Professional"
  ))->Brantford_Budget_Web_Survey_Cleaned

#Education Overview
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(EDU) %>% 
  pivot_longer(., cols = EDU) %>% 
  count(name, value) %>% 
  filter(value!="0") %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("Education Overview", width =60))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:8, labels = c("Some High School","High School","Some College","College","Trade School","Some Graduate","Graduate","Professional"))+
  scale_fill_distiller(palette = "Set1")+ 
  theme(axis.text = element_text(size = 9))
#ggsave("Ov_Education.png", width = 800, height = 300)

#Age and Gender Overview
Brantford_Budget_Web_Survey_Cleaned %>%
  select(Age, Gender_Cat) %>%
  pivot_longer(., cols=Age) %>%
  filter(value!=".") %>%
  filter(Gender_Cat!=".") %>%
  filter(value!="0") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  ggplot(aes(x=value,y = (..count..)/sum(..count..)))+
  labs(y="", x="", title=str_wrap("Age and Gender Distribution", width =60))+
  geom_histogram(aes(y=stat(density), fill=Gender_Cat),binwidth=3)+
  xlab("Age of Respondent")+ 
  ylab("Percentage of Sample")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+ 
  theme(legend.position = "bottom")+ 
  guides(fill=guide_legend(title=""))

#Ethnicity Overview
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Ethnicity) %>% 
  pivot_longer(., cols = Ethnicity) %>% 
  count(name, value) %>% 
  filter(value!="0") %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("Ethnicity Overview", width =60))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:9, labels = c("White\n/European","Indigenous","Black\n/African\n/Caribbean","Southeast Asian","West Asian","South Asian","Arab","Latin American", "Other"))+
  scale_fill_distiller(palette = "Set1")+ 
  theme(axis.text = element_text(size = 9))

#Bar Chart for Overall Satisfaction (1-10)
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Satisfaction) %>% 
  pivot_longer(., cols = Satisfaction) %>% 
  count(name, value) %>% 
  filter(!is.na(value)) %>%
  filter(value!="0") %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("Overall Satisfaction", width =60))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:10, labels = c("Not Satisfied","2","3","4","5","6","7","8","9","Very Satisfied"))+
  scale_fill_gradient2(low = "firebrick", mid="grey70", high = "royalblue4", midpoint = 5.5)

#Satisfaction by Education
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Satisfaction, Edu_4, Gender_Cat) %>% 
  pivot_longer(., cols=Satisfaction) %>% 
  group_by(Edu_4, name, Gender_Cat) %>% 
  filter(Edu_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Edu_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Not Satisfied","2","3","4","5","6","7","8","9","Very Satisfied"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Satisfaction by Education"), col="")+ 
  theme(legend.position = "bottom")+
  scale_y_discrete(limits = c("Graduate/Professional", "Post-Secondary", "Secondary", "Primary"))

#Satisfaction by Age
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Satisfaction, Age_4, Gender_Cat) %>% 
  pivot_longer(., cols=Satisfaction) %>% 
  group_by(Age_4, name, Gender_Cat) %>% 
  filter(Age_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Age_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Not Satisfied","2","3","4","5","6","7","8","9","Very Satisfied"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Satisfaction by Age Group"), col="")+ 
  scale_y_continuous(labels = c("Under 35","35-49","50-64", "65 and Over"))+
  theme(legend.position = "bottom")

#Satisfaction by FSA
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Satisfaction, FSA, Gender_Cat) %>% 
  pivot_longer(., cols=Satisfaction) %>% 
  group_by(FSA, name, Gender_Cat) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N1") %>%
  filter(FSA!="N0E") %>%
  filter(FSA!="N3L") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=FSA, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Not Satisfied","2","3","4","5","6","7","8","9","Very Satisfied"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Satisfaction by FSA"), col="")+ 
  theme(legend.position = "bottom")

#Bar Chart for Overall Value
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Value) %>% 
  pivot_longer(., cols = Value) %>% 
  count(name, value) %>% 
  filter(!is.na(value)) %>%
  filter(value!="0") %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("Overall Value for Taxes", width =80))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:4,labels = c("Very Good","Fairly Good","Fairly Poor","Very Poor"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_fill_gradient2(high = "firebrick", mid="grey70", low = "royalblue4", midpoint = 2.5)

#Value by Education
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Value, Edu_4, Gender_Cat) %>% 
  pivot_longer(., cols=Value) %>% 
  group_by(Edu_4, name, Gender_Cat) %>% 
  filter(Edu_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Edu_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Very Good","Fairly Good","Fairly Poor","Very Poor"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Value by Education"), col="")+ 
  theme(legend.position = "bottom")+
  scale_y_discrete(limits = c("Graduate/Professional", "Post-Secondary", "Secondary", "Primary"))

#Value by Age
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Value, Age_4, Gender_Cat) %>% 
  pivot_longer(., cols=Value) %>% 
  group_by(Age_4, name, Gender_Cat) %>% 
  filter(Age_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Age_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Very Good","Fairly Good","Fairly Poor","Very Poor"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Value by Age Group"), col="")+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = c("Under 35","35-49","50-64", "65 and Over"))

#Value by FSA
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Value, FSA, Gender_Cat) %>% 
  pivot_longer(., cols=Value) %>% 
  group_by(FSA, name, Gender_Cat) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  filter(FSA!="N1") %>%
  filter(FSA!="N0E") %>%
  filter(FSA!="N3L") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  filter(Gender_Cat!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=FSA, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Very Good","Fairly Good","Fairly Poor","Very Poor"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Overall Value by FSA"), col="")+ 
  theme(legend.position = "bottom")

###Overview of Spending Areas
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm) %>% 
  pivot_longer(., cols=SA_Police:SA_Storm) %>%
  filter(!is.na(value)) %>% 
  filter(value!="0") %>%
  group_by(name) %>% 
  count(name, value) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(y=name, label=ifelse(pct>5, paste0(round(pct, digits=1), "%"),''), x=pct))+
  geom_col(aes(fill=as.factor(value)), width = 0.6)+
  scale_fill_manual(values = c("firebrick4", "firebrick2", "grey60","royalblue2", "royalblue4"), labels = c("Significantly Reduce", "Somewhat Reduce", "Maintain", "Somewhat Increase", "Significantly Increase"))+
  labs(y="", x="", title=str_wrap("", width =60))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
  geom_text(aes(group=as.factor(value)), size = 3, position = position_stack(vjust = 0.5, reverse=FALSE), colour="white")+
  theme(legend.position="bottom",legend.text=element_text(size=8))

#Spending Areas by Gender
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm, Gender_Cat) %>% 
  pivot_longer(., cols=-Gender_Cat) %>% 
  group_by(Gender_Cat, name) %>% 
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Significantly\n Reduce","","Maintain","","Significantly \n Increase"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
  labs(y="", x="", title=str_wrap("Key Programming/Services by Gender", width =60))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))

#Spending Areas by Education
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm, Edu_4) %>% 
  pivot_longer(., cols=-Edu_4) %>% 
  group_by(Edu_4, name) %>% 
  filter(Edu_4!=".") %>%
  filter(value!="0") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Edu_4)) + geom_point(size = 2)+xlim(c("Significantly\n Reduce","","Maintain","","Significantly \n Increase"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
  labs(y="", x="", title=str_wrap("Key Programming/Services by Education", width =60))+
  guides(color=guide_legend("", nrow=2))+
  theme(legend.position="bottom",legend.text=element_text(size=10))+
  scale_color_manual(values = c("Primary" = "Grey80", "Secondary" = "Grey60", "Post-Secondary"="Grey40", "Graduate/Professional"="Black"))

#Spending Areas by Age
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm, Age_4) %>% 
  pivot_longer(., cols=-Age_4) %>% 
  group_by(Age_4, name) %>% 
  filter(Age_4!=".") %>%
  filter(value!="0") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Age_4)) + geom_point(size = 2)+xlim(c("Significantly\n Reduce","","Maintain","","Significantly \n Increase"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
  labs(y="", x="", title=str_wrap("Key Programming/Services by Age Group", width =60))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))+
  scale_color_gradient(labels = c("Under 35","35-49","50-64", "65 and Over"), low = "grey80", high = "grey0")

?scale_color_gradient

#Spending Areas by FSA
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(SA_Police:SA_Storm, FSA) %>% 
  pivot_longer(., cols=-FSA) %>% 
  group_by(FSA, name) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  filter(FSA!="N1") %>%
  filter(FSA!="N0E") %>%
  filter(FSA!="N3L") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=FSA)) + geom_point(size = 2)+xlim(c("Significantly\n Reduce","","Maintain","","Significantly \n Increase"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Children's Services", "Economic Development", "Facilities Management and Security", "Brantford Fire", "Garbage and Recycling", "Housing", "John Noble Home", "Brantford-Brant Paramedics", "Parks, Sports Fields \n& Recreational Programming", "Brant County Public Health Unit", "Brantford Police", "Public Works Operational Services", "Social Assistance and Homelessness", "Storm Water Management", "Tourism and Culture", "Brantford Transit" ))+
  labs(y="", x="", title=str_wrap("Key Programming/Services by FSA", width =60))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))

##Increase/New User Fees Overview
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads) %>% 
  pivot_longer(., cols=Fee_Parks:Fee_Roads) %>%
  filter(!is.na(value)) %>% 
  filter(value!="0") %>%
  group_by(name) %>% 
  count(name, value) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(y=name, label=ifelse(pct>5, paste0(round(pct, digits=1), "%"),''), x=pct))+
  geom_col(aes(fill=as.factor(value)), width = 0.6)+
  scale_fill_manual(values = c("royalblue4", "royalblue2", "firebrick2","firebrick4"), labels = c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  geom_text(aes(group=as.factor(value)), size = 4, position = position_stack(vjust = 0.5, reverse=FALSE), colour="white")+
  theme(legend.position="bottom",legend.text=element_text(size=8))

#Increase/New User Fees by Gender
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads, Gender_Cat) %>% 
  pivot_longer(., cols=-Gender_Cat) %>% 
  group_by(Gender_Cat, name) %>% 
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))

#Increase/New User Fee by Edu
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads, Edu_4) %>% 
  pivot_longer(., cols=-Edu_4) %>% 
  group_by(Edu_4, name) %>% 
  filter(Edu_4!=".") %>%
  filter(value!="0") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Edu_4)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))+
  scale_color_manual(values = c("Primary" = "Grey80", "Secondary" = "Grey60", "Post-Secondary"="Grey40", "Graduate/Professional"="Black"))

#Increase/New Fees by Age
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads, Age_4) %>% 
  pivot_longer(., cols=-Age_4) %>% 
  group_by(Age_4, name) %>% 
  filter(Age_4!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Age_4)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))+
  scale_color_gradient(labels = c("Under 35","35-49","50-64", "65 and Over"), low = "grey80", high = "grey0")

#Increase/New Fees by FSA
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Fee_Parks:Fee_Roads, FSA) %>% 
  pivot_longer(., cols=-FSA) %>% 
  group_by(FSA, name) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  filter(FSA!="N1") %>%
  filter(FSA!="N0E") %>%
  filter(FSA!="N3L") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=FSA)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Parking Lots", "Access to Park and Recreation Facilities", "New Development Applications", "Use of Roads", "Street Parking", "Brantford Transit and Lift"))+
  labs(y="", x="", title=str_wrap("Do you support an increase or new user fees for the following areas:", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="bottom",legend.text=element_text(size=10))

##Optional Spending Overview
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special) %>% 
  pivot_longer(., cols=Opt_Airport:Opt_Special) %>%
  filter(!is.na(value)) %>%
  filter(value!="0") %>%
  group_by(name) %>% 
  count(name, value) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(y=name, label=ifelse(pct>5, paste0(round(pct, digits=1), "%"),''), x=pct))+
  geom_col(aes(fill=as.factor(value)), width = 0.6)+
  scale_fill_manual(values = c("royalblue4", "royalblue2", "firebrick2","firebrick4"), labels = c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  geom_text(aes(group=as.factor(value)), size = 3, position = position_stack(vjust = 0.5, reverse=FALSE), colour="white")+
  theme(legend.position="bottom",legend.text=element_text(size=8))

#Optional Spending by Gender
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special, Gender_Cat) %>% 
  pivot_longer(., cols=-Gender_Cat) %>% 
  group_by(Gender_Cat, name) %>% 
  filter(Gender_Cat!=".") %>%
  filter(value!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending by Gender", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="right",legend.text=element_text(size=10))

#Optional Spending by Education
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special, Edu_4) %>% 
  pivot_longer(., cols=-Edu_4) %>% 
  group_by(Edu_4, name) %>% 
  filter(Edu_4!=".") %>%
  filter(value!="0") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Edu_4)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending by Education", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="right",legend.text=element_text(size=10))+
  scale_color_manual(values = c("Primary" = "Grey80", "Secondary" = "Grey60", "Post-Secondary"="Grey40", "Graduate/Professional"="Black"))

#Optional Spending by Age
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special, Age_4) %>% 
  pivot_longer(., cols=-Age_4) %>% 
  group_by(Age_4, name) %>% 
  filter(Age_4!=".") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=Age_4)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending by Age", width =60))+
  guides(color=guide_legend(""))+
  theme(legend.position="right",legend.text=element_text(size=10))+
  scale_color_gradient(labels = c("Under 35","35-49","50-64", "65 and Over"), low = "grey80", high = "black")

#Optional Spending by FSA
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Opt_Airport:Opt_Special, FSA) %>% 
  pivot_longer(., cols=-FSA) %>% 
  group_by(FSA, name) %>% 
  filter(FSA!=".") %>%
  filter(value!=".") %>%
  filter(FSA!="N4T") %>%
  filter(FSA!="N1") %>%
  filter(FSA!="N0E") %>%
  filter(FSA!="N3L") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=name, col=FSA)) + geom_point(size = 2)+xlim(c("Significantly Support", "Somewhat Support", "Somewhat Oppose", "Significantly Oppose"))+
  #geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  scale_y_discrete(labels = c("Brantford Airport", "Downtown Revitialization Programs", "Economic Development", "City Facilities Upgrades", "Municipal Golf Courses", "Community Health & Wellness", "Horticulture Maintenance", "Brantford Public Library", "City Parks and Trails","Snow Windrow Removal Program", "Discretionary Social Assistance", "Special Events", "Minor Sports Discounted Rates"))+
  labs(y="", x="", title=str_wrap("Preferred Discretionary Spending by FSA", width =80))+
  guides(color=guide_legend(""))+
  theme(legend.position="right",legend.text=element_text(size=10))

#Infrastructure Overview
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Infrastructure) %>% 
  pivot_longer(., cols = Infrastructure) %>% 
  count(name, value) %>% 
  filter(!is.na(value)) %>%
  filter(value!="0") %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("How Should the City Approach Infrastructure Maintenance?", width =80))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:3,labels = c("Spend on Infrastructure Now", "Not Sure",  "Defer Maintenance"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_fill_gradient2(high = "firebrick", mid="grey70", low = "royalblue4", midpoint = 2)

#Infrastructure by Edu
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Infrastructure, Edu_4, Gender_Cat) %>% 
  pivot_longer(., cols=Infrastructure) %>% 
  group_by(Edu_4, name, Gender_Cat) %>% 
  filter(Edu_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Edu_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Spend on Infrastructure Now", "Not Sure",  "Defer Maintenance"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Infrastructure Approach by Education"), col="")+ 
  theme(legend.position = "bottom")+
  scale_y_discrete(limits = c("Graduate/Professional", "Post-Secondary", "Secondary", "Primary"))

#Infrastructure by FSA
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Infrastructure, FSA, Gender_Cat) %>% 
  pivot_longer(., cols=Infrastructure) %>% 
  group_by(FSA, name, Gender_Cat) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  filter(FSA!="N1") %>%
  filter(FSA!="N0E") %>%
  filter(FSA!="N3L") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=FSA, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Spend on Infrastructure Now", "Not Sure",  "Defer Maintenance"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Infrastructure Approach by FSA"), col="")+ 
  theme(legend.position = "bottom")

#Infrastructure by Age
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Infrastructure, Age_4, Gender_Cat) %>% 
  pivot_longer(., cols=Infrastructure) %>% 
  group_by(Age_4, name, Gender_Cat) %>% 
  filter(Age_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Age_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Spend on Infrastructure Now", "Not Sure",  "Defer Maintenance"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Infrastructure Approach by Age Group"), col="")+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = c("Under 35","35-49","50-64", "65 and Over"))

#Maintenance Tradeoff
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Maintain) %>% 
  pivot_longer(., cols = Maintain) %>% 
  count(name, value) %>% 
  filter(!is.na(value)) %>%
  filter(value!="0") %>%
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(x=value, y=pct))+
  labs(y="", x="", title=str_wrap("How Should the City Balance Services and Taxation?", width =80))+
  geom_bar(stat="identity", width = 0.5, aes(fill=value), show.legend = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = 1:4,labels = c("Increase Taxes Significantly \nto Expand Services", "Increase Taxes Slightly \n to Maintain Service Levels",  "Maintain Taxes by Cutting \n Some Services", "Reduce Taxes by Significantly \n Cutting Services"))+
  guides(fill=guide_legend(title="", reverse = TRUE))+
  scale_fill_gradient2(high = "firebrick", mid="grey70", low = "royalblue4", midpoint = 2.5)

#Maintenance Tradeoff: EDU
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Maintain, Edu_4, Gender_Cat) %>% 
  pivot_longer(., cols=Maintain) %>% 
  group_by(Edu_4, name, Gender_Cat) %>% 
  filter(Edu_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Edu_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Increase Taxes Significantly \nto Expand Services", "Increase Taxes Slightly \n to Maintain Service Levels",  "Maintain Taxes by Cutting \n Some Services", "Reduce Taxes by Significantly \n Cutting Services"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Taxation Approach by Education"), col="")+ 
  theme(legend.position = "bottom")+
  scale_y_discrete(limits = c("Graduate/Professional", "Post-Secondary", "Secondary", "Primary"))

#Maintenance Tradeoff: Age
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Maintain, Age_4, Gender_Cat) %>% 
  pivot_longer(., cols=Maintain) %>% 
  group_by(Age_4, name, Gender_Cat) %>% 
  filter(Age_4!=".") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=Age_4, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Increase Taxes Significantly \nto Expand Services", "Increase Taxes Slightly \n to Maintain Service Levels",  "Maintain Taxes by Cutting \n Some Services", "Reduce Taxes by Significantly \n Cutting Services"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Taxation Approach by Age"), col="")+ 
  theme(legend.position = "bottom")+ 
  scale_y_continuous(labels = c("Under 35","35-49","50-64", "65 and Over"))

#Maintenance Tradeoff: FSA
Brantford_Budget_Web_Survey_Cleaned %>% 
  select(Maintain, FSA, Gender_Cat) %>% 
  pivot_longer(., cols=Maintain) %>% 
  group_by(FSA, name, Gender_Cat) %>% 
  filter(FSA!=".") %>%
  filter(FSA!="N4T") %>%
  filter(FSA!="N1") %>%
  filter(FSA!="N0E") %>%
  filter(FSA!="N3L") %>%
  filter(Gender_Cat!=".") %>%
  filter(Gender_Cat!="LGBTQ2S+") %>%
  summarize(average=mean((value), na.rm=TRUE), sd=sd((value), na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>% 
  ggplot(., aes(x=average, y=FSA, col=Gender_Cat)) + geom_point(size = 2)+xlim(c("Increase Taxes Significantly \nto Expand Services", "Increase Taxes Slightly \n to Maintain Service Levels",  "Maintain Taxes by Cutting \n Some Services", "Reduce Taxes by Significantly \n Cutting Services"))+
  geom_errorbar(aes(xmin=average-(1.96*se), xmax=average+(1.96*se)), width=0)+
  labs(y="", x="", title=str_wrap("Taxation Approach by FSA"), col="")+ 
  theme(legend.position = "bottom")
