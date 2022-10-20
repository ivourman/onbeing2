#putting in age demo for df3#
install.packages('hrbrthemes')
library(hrbrthemes)
install.packages('viridis')
library(viridis)

df3<-df3%>%mutate(
	Age_Group=case_when(
		Age=="75-84" ~"75_and_older",
		Age=="85-94"~"75_and_older",
		Age=="95-104"~"75_and_older",
		Age=="65-74"~"65-74",
		Age=="55-64"~"55-64",
		Age=="45-54"~"45-54",
		Age=="45-54"~"45-54",
		Age=="35-44"~"35-44",
		Age=="25-34"~"25-34",
		Age=="18-24"~"Under_25",
		Age=="13-17"~"Under_25"
	)
)
colnames(df3)[108]<-"Political_view"
df3<-df3 %>% mutate(Political_View_Group=Political_view)
df3 <-df3%>% 
	mutate(across('Political_View_Group', str_replace, 'Progressive', 'Liberal'))

Politics<-df3%>%group_by(Political_view,Age_Group)%>%tally()%>%mutate(percent=n/sum(n))
Politics<-Politics%>%filter(!is.na(Age_Group))
Politics %>% 
	ggplot(aes(x=Political_view, y=percent, fill=Age_Group)) +
	geom_bar(stat="identity", width=1, color="black") +
	geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")), 
						position=position_stack(vjust=0.5), colour="white") +
	scale_y_continuous(labels=percent_format()) +
	labs(x="Listening Frequency", y="Percent of Age Group with Listening Frequency") +
	ggtitle("Listening Frequency", "by Age Group")+facet_wrap(~Age_Group,scales="free")

Politics2<-df3%>%group_by(Political_view)%>%tally()%>%mutate(percent=n/sum(n))

Politics2 %>% filter(!is.na(Political_view))%>%
	ggplot(aes(x=Political_view, y=percent, fill=Political_view)) +
	geom_bar(stat="identity", width=1, color="black") +
	geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")), 
						position=position_stack(vjust=0.5), colour="white") +
	scale_y_continuous(labels=percent_format()) +
	labs(x="Political View", y="Percent of Respondents") +
	ggtitle("Politcal Views of all respondents")



Politics2 %>%filter(Political_view!="NA")%>%ggplot(.,aes(fct_rev(fct_reorder(Political_view)),y=percent))+geom_col(aes(fill=Political_view))+geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),position=position_stack(vjust=0.5), colour="white")+scale_y_continuous(labels=percent_format()) +
	labs(x="Listening Frequency", y="Percent of Age Group with Listening Frequency") +
	ggtitle("Listening Frequency", "by Age Group")

#
#Politics grouped#
Politics_group<-df3%>%group_by(Political_View_Group,Age_Group)%>%tally()%>%mutate(percent=n/sum(n))

Politics_group<-Politics_group%>%filter(!is.na(Age_Group))
Politics_group<-Politics_group%>%mutate(Political_View_Group=as.factor(Political_View_Group))
Politics_group %>% 
	ggplot(aes(Political_View_Group, y=percent, fill=Age_Group)) +
	geom_bar(stat="identity", width=1, color="black") +
	geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")), 
						position=position_stack(vjust=0.5), colour="white") +
	scale_y_continuous(labels=percent_format()) +
	labs(x="Listening Frequency", y="Percent of Age Group with Listening Frequency") +
	ggtitle("Listening Frequency", "by Age Group")+facet_wrap(~Age_Group,scales="free")

Politics_group2<-df3%>%group_by(Political_View_Group)%>%tally()%>%mutate(percent=n/sum(n))
Politics_group2<-Politics_group2%>%mutate(Political_View_Group=as.factor(Political_View_Group))
Politics_group2 %>% filter(!is.na(Political_View_Group))%>%
	ggplot(aes(x=fct_reorder(`as.factor(Political_View_Group)`, y=percent)), fill=`as.factor(Political_View_Group)`) +
	geom_bar(stat="identity", width=1, color="black") +
	geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")), 
						position=position_stack(vjust=0.5), colour="white") +
	scale_y_continuous(labels=percent_format()) +
	labs(x="Political View", y="Percent of Respondents") +
	ggtitle("Politcal Views of all respondents")

ggplot(Politics_group2,aes(x=reorder(Political_View_Group,y=desc(percent)),fill=Political_View_Group))+geom_bar(stat="identity", width=1, color="black") 
Politics_group2<-Politics_group2%>%rename(Political_Group=`as.factor(Political_View_Group)`)

#USED FOR POLITICS#
Politics_group2 %>%filter(Political_Group!="NA")%>%ggplot(.,aes(fct_reorder(Political_Group,percent),y=percent))+geom_col(aes(fill=Political_Group))+geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),position=position_stack(vjust=0.5), colour="white")+scale_y_continuous(labels=percent_format()) +
	labs(x="Political View", y="Percent with Political View") +
	ggtitle("Poitical Views of Total Population")

Ethnicity_Age %>%filter(Age!="13-17"&Ethnicity!="NA")%>%ggplot(.,aes(x=reorder(Age,desc(n)),y=(n)))+geom_col(aes(fill=Age))+facet_wrap(~reorder(Ethnicity,desc(n)))+geom_text(aes(label=round(n, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+scale_y_continuous(labels=comma,'Total Count Per Within Ethnicity')+ggtitle('Distribution of Age by Ethnic Group')+scale_x_discrete("Count by Age Group")

#archetypes#

colnames(df3)[103:106]<-c("Often_get_lost_in_thoughts","Sought_as_careful_listener","Peacemaker","Status_quo_boring")


cols_change_arch<-names(df3_arch[,c(103:106)])
df3_arch$Often_get_lost_in_thoughts<-gsub("[[:alpha:]]", "", df3_arch$Often_get_lost_in_thoughts)
df3_arch$Often_get_lost_in_thoughts<-str_replace_all(text, regex("\\W+"), " ")
df3_arch$Often_get_lost_in_thoughts<-gsub("(.*)>.*","\\1",df3_arch$Often_get_lost_in_thoughts)
df3_arch$Often_get_lost_in_thoughts<-gsub("(.*)<.*","\\1",df3_arch$Often_get_lost_in_thoughts)
#removing everything after <. still left with parentheses#
df3_arch$Sought_as_careful_listener<-gsub("(.*)<.*","\\1",df3_arch$Sought_as_careful_listener)
df3_arch$Peacemaker<-gsub("(.*)<.*","\\1",df3_arch$Peacemaker)
df3_arch$Status_quo_boring<-gsub("(.*)<.*","\\1",df3_arch$Status_quo_boring)



df3_arch<-df3_arch%>%mutate(
	Age_Group=case_when(
		Age=="75-84" ~"75_and_older",
		Age=="85-94"~"75_and_older",
		Age=="95-104"~"75_and_older",
		Age=="65-74"~"65-74",
		Age=="55-64"~"55-64",
		Age=="45-54"~"45-54",
		Age=="45-54"~"45-54",
		Age=="35-44"~"35-44",
		Age=="25-34"~"25-34",
		Age=="18-24"~"Under_25",
		Age=="13-17"~"Under_25"
	)
)

df3_thoughts<-df3_arch%>%group_by(Age_Group,Often_get_lost_in_thoughts)%>%tally()%>%mutate(percent=n/sum(n))
df3_thoughts_sum<-df3_thoughts%>%filter(!is.na(Age_Group))
df3_thoughts_sum<-df3_thoughts_sum%>%arrange_(~ desc(percent)) %>%group_by_(~ Age_Group)

df3_thoughts_sum%>%ggplot(.,aes(x=reorder(Often_get_lost_in_thoughts,desc(percent)),y=percent))+geom_col(aes(fill=Age_Group))+facet_wrap(~Age_Group,scales="free")+geom_text(aes(label=round(percent, digits = 2)),position = position_dodge(width=0.9),vjust=1)+ggtitle('Percent Agreement-I Often get lost in my thoughs by Age Group ')+scale_y_continuous(labels=percent,'Percent')+xlab("I Often Get Lost in my thoughts (5) Strongly Agree, 1-Strongly Disagree")

df3_thoughts_sum%>%ggplot(.,aes(x=reorder(Often_get_lost_in_thoughts,desc(percent)),y=percent,fill=Age_Group))+geom_bar(position="stack",stat="identity")+geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),position=position_stack(vjust=0.5), colour="white") +ggtitle('Percent Agreement-I Often get lost in my thoughs by Age Group ')+xlab("I Often Get Lost in my thoughts (5) Strongly Agree, 1-Strongly Disagree")+facet_wrap(~Age_Group,scales="free")+scale_y_continuous(labels=percent,'Percent')
###Sought_as_careful_listener#
																																																																																		
df3_Listener<-df3_arch%>%group_by(Age_Group,Sought_as_careful_listener)%>%tally()%>%mutate(percent=n/sum(n))
df3_Listener_sum<-df3_Listener%>%filter(!is.na(Age_Group))
df3_Listener_sum<-df3_Listener_sum%>%arrange_(~ (Age_Group)) 

df3_Listener_sum%>%ggplot(.,aes(x=Sought_as_careful_listener,y=percent))+geom_col(aes(fill=Age_Group))+facet_wrap(~Age_Group,scales="free")+geom_text(aes(label=round(percent, digits = 2)),position = position_dodge(width=0.9),vjust=1)+ggtitle('Percent Agreement-I am often sought after as a careful listener')+scale_y_continuous(labels=percent,'Percent')+xlab("I Am often sought after to be a careful listener (5) Strongly Agree, 1-Strongly Disagree")

df3_Listener_sum%>%filter(!is.na(Sought_as_careful_listener))%>%ggplot(.,aes(x=Sought_as_careful_listener,y=percent,fill=Age_Group))+geom_bar(position="stack",stat="identity")+geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),position=position_stack(vjust=0.5), colour="white") +ggtitle('Percent Agreement-I am often sought after as a careful listener')+xlab("I Am often sought after to be a careful listener (5) Strongly Agree, 1-Strongly Disagree")+facet_wrap(~Age_Group,scales="free")+scale_y_continuous(labels=percent,'Percent')


df3_Peacemaker<-df3_arch%>%group_by(Age_Group,Peacemaker)%>%tally()%>%mutate(percent=n/sum(n))
df3_Peacemaker_sum<-df3_Peacemaker%>%filter(!is.na(Age_Group))
df3_Peacemaker_sum<-df3_Peacemaker_sum%>%arrange_(~ (Age_Group)) 

df3_Peacemaker_sum%>%ggplot(.,aes(x=Peacemaker,y=percent))+geom_col(aes(fill=Age_Group))+facet_wrap(~Age_Group,scales="free")+geom_text(aes(label=round(percent, digits = 2)),position = position_dodge(width=0.9),vjust=1)+ggtitle('Percent Agreement-I am often sought to keep or maintain the Peace')+scale_y_continuous(labels=percent,'Percent')+xlab("I Am often sought to keep or maintain Peace (5) Strongly Agree, 1-Strongly Disagree")

df3_Peacemaker_sum%>%ggplot(.,aes(x=Peacemaker,y=percent,fill=Age_Group))+geom_bar(position="stack",stat="identity")+geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),position=position_stack(vjust=0.5), colour="white")+ggtitle("Percent Agreement-I am often sought to keep or maintain the Peace")+xlab("I Am often sought to keep or maintain the Peace : (5) Strongly Agree, 1-Strongly Disagree")+facet_wrap(~Age_Group,scales="free")+scale_y_continuous(labels=percent,'Percent')

df3_Status_quo_boring<-df3_arch%>%group_by(Age_Group,Status_quo_boring)%>%tally()%>%mutate(percent=n/sum(n))
df3_boring_sum<-df3_Status_quo_boring%>%filter(!is.na(Age_Group))
df3_boring_sum<-df3_boring_sum%>%arrange_(~ (Age_Group)) 

df3_boring_sum%>%filter(!is.na(Status_quo_boring))%>%ggplot(.,aes(x=Status_quo_boring,y=percent))+geom_col(aes(fill=Age_Group))+facet_wrap(~Age_Group,scales="free")+geom_text(aes(label=round(percent, digits = 2)),position = position_dodge(width=0.9),vjust=1)+ggtitle('Percent Agreement-I often find the status quo borig')+scale_y_continuous(labels=percent,'Percent')+xlab("I often fid the status quo boring (5) Strongly Agree, 1-Strongly Disagree")


colnames(df3)[103:106]<-c("Often_get_lost_in_thoughts","Sought_as_careful_listener","Peacemaker","Status_quo_boring")

df3_arch%>%filter(!is.na(Age_Group)&!is.na(Gender))%>%
ggplot(.,aes(x = Age_Group, y = Listening_length, color = as.factor(Age_Group))) +geom_jitter() +
	#  geom_text_repel(aes(label = DMA_LONG_NAME)) +
	guides(guide_legend(override.aes=list(lty = NULL))) +
	ggtitle('total sales by wm share')+geom_smooth(method="smooth")
df3_arch<-df3_arch%>%mutate(Age_group=as.factor(Age_Group))
df3_arch%>%filter(!is.na(Age_group)&!is.na(Gender))%>%ggplot(.,aes(x=Listening_length,color=Listening_length,fill=Gender))+geom_density(alpha=0.3)+facet_wrap(~Age_group,scales="free")

df3_arch_long<-df_values3_count_longer%>%pivot_longer(cols=colnames_values2,names_to="key",values_to = "values_count")

df3_arch_long<-df3_arch_long %>% mutate(values_count=as.numeric(values_count))
df3_arch_long%>%filter(!is.na(Age_Group)&!is.na(Gender))%>%group_by(key) %>%filter(key!='How_express_values-other') %>% 
	ggplot(.,aes(x = key, y = (values_count), color = as.factor(key))) +geom_jitter() +
	#  geom_text_repel(aes(label = DMA_LONG_NAME)) +
	guides(guide_legend(override.aes=list(lty = NULL))) +
	ggtitle('total sales by wm share')

df3_arch_long_sum<-df3_arch_long%>%group_by(respondent_id,key)%>%mutate(value_sum=sum(values_count))
df3_arch_long_sum_d<-df3_arch_long_sum%>%distinct(value_sum,.keep_all = TRUE)
df3_arch_long_sum_d2<-df3_arch_long_sum_d%>%distinct(respondent_id)
df3_arch_long_sum_d<-df3_arch_long_sum_d %>%group_by(respondent_id)%>%distinct(key,.keep_all = TRUE)
df3_arch_long_sum_dalt<-df3_arch_long_sum%>%distinct(respondent_id,values_count,.keep_all = TRUE)
df3_arch_long_sum_dalt<-df3_arch_long_sum_dalt%>%group_by(respondent_id,key)%>%mutate(value_sum=sum(values_count))


df3_arch_long2<-df_values3_count_longer %>% group_by(respondent_id)%>%pivot_longer(cols=colnames_values2,names_to="key",values_to = "values_count")

df3_arch_long2<-df3_arch_long2%>%filter(values_count!=0)
df3_arch_long2_sum<-df3_arch_long2%>%group_by(respondent_id,key)%>%mutate(value_sum=as.numeric(values_count))
df3_arch_long2_sum2<-df3_arch_long2_sum%>%distinct()
df3_arch_long2_sum2<-as.data.frame(df3_arch_long2_sum2)%>%group_by(respondent_id)%>%mutate(key_express_value=as.factor(key))


df3_arch_long2_sum2%>%filter(!is.na(Age_Group)&!is.na(Gender))%>%group_by(key_express_value) %>%filter(key_express_value!='How_express_values-other') %>% 
	ggplot(.,aes(x = key_express_value, y = (values_count), color = key_express_value))+geom_col(aes(fill=key_express_value))+
	#  geom_text_repel(aes(label = DMA_LONG_NAME)) +
	guides(guide_legend(override.aes=list(lty = NULL))) +
	ggtitle('total sales by wm share')



df3_arch_long2_sum3<-df3_arch_long2_sum2%>%filter(!is.na(Age_Group)&!is.na(Gender))%>%group_by(key_express_value) %>%filter(key_express_value!='How_express_values-other')

df3_arch_long2_sum3<-df3_arch_long2_sum3%>%filter(Gender=="Male"|Gender=="Female")
# df3_arch_long2_sum3<-df3_arch_long2_sum3%>%group_by(respondent_id)%>%slice(respondent_id,1)
#removing How_express_values, everything before -#
df3_arch_long2_sum3$key_express_value<-gsub(".*-","",df3_arch_long2_sum3$key_express_value)

 USED
df3_arch_long2_sum3<-df3_arch_long2_sum3%>%mutate(key_express_value=as.factor(key_express_value))
df3_arch_long2_sum3<-as.data.frame(df3_arch_long2_sum3)
df3_arch_long2_sum3%>% ggplot(.,aes(x=fct_rev(fct_reorder(key_express_value, value_sum)),y = value_sum))+geom_col(aes(fill=key_express_value))+ggtitle('Overall expression of Demonstrated Values')+labs(x="Value Expresseed", y="Total of each Value Chosen")+scale_y_continuous(labels=comma)

df3_arch_long2_sum3%>% ggplot(.,aes(x=fct_rev(fct_reorder(key_express_value, value_sum)),y = value_sum))+geom_col(aes(fill=key_express_value))+ggtitle('Overall expression of Demonstrated Values')+labs(x="Value Expresseed", y="Total of each Value Chosen")+scale_y_continuous(labels=comma)+facet_wrap(~Age_Group,scales="free")

df3_arch_long2_sum3%>% ggplot(.,aes(x=fct_rev(fct_reorder(Age_Group, value_sum)),y = value_sum))+geom_col(aes(fill=key_express_value))+ggtitle('Overall expression of Demonstrated Values')+labs(x="Value Expresseed", y="Total of each Value Chosen")+scale_y_continuous(labels=comma)+facet_wrap(~Age_Group,scales="free",nrow=4,ncol=3)
df3_arch_long2_sum3<-df3_arch_long2_sum3%>%mutate(Age_Group=as.factor(Age_Group))

df3_arch_long2_sum3_table<-df3_arch_long2_sum3%>%group_by(Age_Group,key_express_value)%>%summarize(count_values_per_age_group=sum(value_sum))


df3_arch_long2_sum3_table%>%ggplot(.,aes(x=reorder(Age_Group,desc(count_values_per_age_group)),y=count_values_per_age_group))+geom_col(aes(fill=Age_Group))+facet_wrap(~"key_express_value")+ggtitle('Overall expression of Demonstrated Values by Age Group')+scale_y_continuous(labels=comma)+facet_wrap(~key_express_value,scales="free",nrow=3,ncol=4)+scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 5))+labs(x="key_express_value", y="Total of each Value Chosen")


df3_arch_long%>%filter(!is.na(Age_Group)&!is.na(Gender))%>%group_by(key) %>%filter(key!='How_express_values-other'& Gender=='Female'|Gender=='Male') %>% 
	ggplot(.,aes(x = key, y = (values_count), color = as.factor(key))) +geom_jitter() +
	#  geom_text_repel(aes(label = DMA_LONG_NAME)) +
	guides(guide_legend(override.aes=list(lty = NULL))) +
	ggtitle('total sales by wm share')

df3_arch_long2_sum3_table_g<-df3_arch_long2_sum3%>%group_by(Gender,key_express_value)%>%filter(Gender=="Male"|Gender=="Female")%>%mutate(count_values_per_Gender_group=sum(value_sum))%>%mutate(percent=value_sum/count_values_per_Gender_group)


df3_arch_long2_sum3_table_g%>%ggplot(.,aes(x=reorder(Age_Group,desc(count_values_per_age_group)),y=count_values_per_age_group))+geom_col(aes(fill=Age_Group))+facet_wrap(~"key_express_value")+ggtitle('Overall expression of Demonstrated Values by Age Group')+scale_y_continuous(labels=comma)+facet_wrap(~key_express_value,scales="free",nrow=3,ncol=4)+scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 5))+labs(x="key_express_value", y="Total of each Value Chosen")

df3_arch_long2_sum3_table_g<-df3_arch_long2_sum3_table_g%>%group_by(Gender)%>%distinct(percent)%>%ggplot(.,aes(x=reorder(Gender,desc(percent)),y=percent))+geom_col(aes(fill=Gender))+facet_wrap(~"Gender")+ggtitle('Overall expression of Demonstrated Values by Age Group')+scale_y_continuous(labels=percent)+facet_wrap(~key_express_value,scales="free",nrow=3,ncol=4)+scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 5))+labs(x="key_express_value", y="Total of each Value Chosen")


#More of again#
#colnames for interested more of#
colnames(df_values3_count_longer)[51:57]<-c("More_Audio","More_Written","More_Video","More_Toolkits","More_online_events","More_in_person_events","More_other")
df_values3_count_longer<-df_values3_count_longer%>%pivot_longer(cols=starts_with("More_"),values_to="count_values")

colnames(df_values3_count_longer)[140]<-"More_of_value"
colnames(df_values3_count_longer)[141]<-"More_of_value_interest_level"
#need to remove 
# df_values3_count_longer_more_of_gsum<-df_values3_count_longer%>%group_by(Age_Group,More_of_value)%>%summarize(value_count=nrow(More_of_value_interest_level))
colnames_values2
df_interactions_long<-df3$>%group_by(respondant_id)%>%pivot_longer(cols=cols_change)
df_values3_count_longer<-df_values3_count_longer%>%pivot_longer(cols=starts_with("More_"),values_to="count_values")

df_values3_count_longer_m<-df_values3_count_longer%>%group_by(respondent_id)%>%filter(count!=0)%>%select(count,Age_Group,Gender,More_of_value,More_of_value_interest_level)

df_values3_count_longer_m<-df_values3_count_longer_m%>%group_by(More_of_value_interest_level,More_of_value)%>%mutate(Total_More_per_Option=sum(count))


df3_arch_spirit2<-df3_arch%>%group_by(Relationship_Spirituality,Gender)%>%tally()
View(df3_arch_spirit)

df3_arch_spirit<-df3_arch_spirit%>%filter(!is.na(Gender),!is.na(Relationship_Spirituality))

df3_arch_spirit<-df3_arch_spirit%>%mutate(Gender=as.factor(Gender))
df3_arch_spirit%>%ggplot(aes(x=Relationship_Spirituality,y=count_spirit_gender,fill=Relationship_Spirituality))+geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
	scale_fill_viridis(discrete=T, name="") +
	theme_ipsum()  

#Spirtuality with Archetype#
df3_arch_spirit_mag<-df3_arch%>%mutate(filter=!is.na(Heavy_Magnetist))
df3_arch_spirit_mag_sum<-df3_arch_spirit_mag%>%group_by(Relationship_Spirituality,Heavy_Magnetist)%>%tally()
df3_arch_spirit%>%ggplot(aes(x=Relationship_Spirituality,y=count_spirit_gender,fill=Relationship_Spirituality))+geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
	scale_fill_viridis(discrete=T, name="") +
	theme_ipsum()  
df3_arch_spirit%>%ggplot(aes(x=Relationship_Spirituality,y=count_spirit_gender,fill=Relationship_Spirituality))+geom_violin(width=1)+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
	theme_ipsum() +
	theme(
		legend.position="none",
		plot.title = element_text(size=8)
	) +
	ggtitle("Violin chart") +
	xlab("")
df3_arch_spirit2<-df3_arch_spirit2%>%filter(!is.na(Gender),!is.na(Relationship_Spirituality))
df3_arch_spirit2%>%filter(!is.na(Gender))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_violin(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
	theme_ipsum() +
	theme(
		legend.position="right",
		plot.title = element_text(size=11)
	) +
	ggtitle("Distribution of Relationship to Spirituality") +
	xlab("")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))

#%>%filter(Gender=="Male"|Gender=="Female")
df3_arch_spirit2%>%filter(!is.na(Gender))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_boxplot(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")
	theme_ipsum() +
	theme(
		legend.position="right",
		plot.title = element_text(size=11)
	) +
	ggtitle("Distribution of Relationship to Spirituality") +
	xlab("")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))

	df3_preffered<-df3%>%group_by(Preferred_way_listening,Gender)%>%filter(!is.na(Gender))%>%tally()
	
	df3_preffered_slice<-df3_preffered%>%arrange(desc(n))%>%group_by(Preferred_way_listening)%>%top_n(5, n)
	
	df3_preffered_slice<-df3_preffered_slice%>%na.omit(Preferred_way_listening)%>%filter(Preferred_way_listening != "All of the above")
	df3_preffered_slice<-df3_preffered_slice%>%filter(Gender!="Non Binary")
	df3_preffered_slice<-df3_preffered_slice%>%filter(n>13)
#used below#
	df3_preffered_slice%>%ggplot(aes(x=Preferred_way_listening,y=n,fill=Gender))+geom_col()+facet_wrap(~Preferred_way_listening,scales = "free")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))+ggtitle('Top 3 Preferences of Listening by Gender')+scale_y_continuous("Proportion by Gender",labels = comma)
	
#coding archetype#
#
	df3_arch<-df3_arch%>% mutate(Often_get_lost_in_thoughts_num=parse_number(Often_get_lost_in_thoughts))
	#do for rest of statements#
	cols_arch<-df3_arch%>%select(Often_get_lost_in_thoughts,Sought_as_careful_listener,Peacemaker,Status_quo_boring)
	colnames_arch<-names(cols_arch)
	df3_arch<-df3_arch %>% mutate(Careful_Listener_num=parse_number(Sought_as_careful_listener))
	
	df3_arch<-df3_arch %>% mutate(Peacemaker_num=parse_number(Peacemaker))
	df3_arch<-df3_arch %>% mutate(Status_quo_boring_num=parse_number(Status_quo_boring))
	# df3_arch<-df3_arch%>%mutate(Arch_type_Mag_T2 = case_when(Often_get_lost_in_thoughts_num %in% 4:5&
	# 	Careful_Listener_num %in% 4:5),"Top_2_Magnetist")
		mutate(Heavy_Magnetist = case_when(Often_get_lost_in_thoughts_num&Careful_Listener_num> 3 ~ 1,
	Often_get_lost_in_thoughts_num&Careful_Listener_num < 3 ~ 0))
# 
# 	df3_arch<-df3_arch%>%mutate(Arch_type_Magnetist=Often_get_lost_in_thoughts_num==4:5&Careful_Listener_num==4:5)
	df3_arch <- df3_arch %>%
		mutate(Heavy_Guardian = case_when(Peacemaker_num> 3 ~ 1,
																				Peacemaker_num<4 ~ 0))
	#adventrurer defined as not being the magnetist guardian and top 2 box of status quo boring#
	df3_arch <- df3_arch %>%
	mutate(Heavy_Adventure = case_when(
																		 Status_quo_boring_num >3 ~ 1,
																		 Status_quo_boring_num <4~ 0,
																		 Heavy_Guardian & Heavy_Magnetist<1 ~ 1
																		 ))
				# df3_arch<-df3_arch%>%mutate(Arch_type_Mag_T2 = case_when(
															# 	Often_get_lost_in_thoughts_num = 4:5 ~ "Magnetist",
															# 		Careful_Listener_num = 4:5          ~ "Magnetist"
															# 		))
			

	df3_arch_spirit_mag<-df3_arch%>%mutate(filter=!is.na(Heavy_Magnetist))
	df3_arch_spirit_mag_sum<-df3_arch_spirit_mag%>%group_by(Relationship_Spirituality,Heavy_Magnetist)%>%tally()
	df3_arch_spirit_mag_sum%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
		scale_fill_viridis(discrete=T, name="") +
		theme_ipsum()  

	
	df3_arch_spirit_mag_sum%>%filter(!is.na(Heavy_Magnetist))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_boxplot(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+
	theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality for Heavy Magnetists") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")
	
	df3_arch_spirit_Guardian<-df3_arch%>%mutate(filter=!is.na(Heavy_Guardian))
	df3_arch_spirit_Guardian_sum<-df3_arch_spirit_Guardian%>%group_by(Relationship_Spirituality,Heavy_Guardian)%>%tally()
	df3_arch_spirit_Guardian_sum%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
		scale_fill_viridis(discrete=T, name="") +
		theme_ipsum()  
	df3_arch_spirit_Guardian_sum%>%filter(!is.na(Heavy_Guardian))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_boxplot(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+
		theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality for Heavy Guardians") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")
	
	df3_arch_spirit_Guardian_sum%>%filter(!is.na(Heavy_Guardian))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_violin(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=q, option="A") +
		theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))
	
	df3_arch_spirit_Guardian_sum%>%filter(!is.na(Heavy_Guardian))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Heavy_Guardian))+geom_violin()+scale_fill_viridis(discrete = TRUE, alpha=q, option="A") +
	theme_ipsum()  
	+geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") 
	
	
	df3_arch_spirit_Guardian_sum%>%filter(!is.na(Heavy_Guardian))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Heavy_Guardian))+geom_violin(position="identity")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
		theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))
	
	df3_arch_spirit_Guardian_sum%>%filter(!is.na(Heavy_Guardian))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_violin(width=1)+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
		theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))
	
#adventurer#

	df3_arch_spirit_Adventure<-df3_arch%>%mutate(filter=!is.na(Heavy_Adventure))
	df3_arch_spirit_Adventure_sum<-df3_arch_spirit_Adventure%>%group_by(Relationship_Spirituality,Heavy_Adventure)%>%tally()

	df3_arch_spirit_Adventure_sum%>%filter(!is.na(Heavy_Adventure))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_boxplot(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+
		theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality for Heavy Guardians") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")+facet_wrap(~Heavy_Adventure)
	
	df3_arch_spirit_Adventure_sum%>%filter(!is.na(Heavy_Adventure))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_col(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+
		theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality for Heavy Adventurers") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")+facet_wrap(~Heavy_Adventure,labeller = to_string)
	
	# make_labelstring <- function(mypanels) {
	# 	mylabels <- sapply(mypanels, 
	# 										 function(x) {NUMBERS[which(mypanels == x)]})
	# 	
	# 	return(mylabels)
	# }
	# 
	# label_panels <- ggplot2::as_labeller(make_labelstring)

	
	df3_arch_spirit_Guardian_sum%>%filter(!is.na(Heavy_Guardian))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_col(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+
		theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality for Heavy Guardians") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")+facet_wrap(~Heavy_Guardian,labeller = label_value)
	to_string <- as_labeller(c(`0` = "False", `1` = "True"))
	df3_arch_spirit_mag_sum%>%filter(!is.na(Heavy_Magnetist))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_col(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+
		theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality for Heavy Magnetists") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")+facet_wrap(~Heavy_Magnetist,labeller = to_string)

	
	df3_arch_spirit_Guardian_sum%>%filter(!is.na(Heavy_Guardian))%>%ggplot(aes(x=Relationship_Spirituality,y=n,fill=Relationship_Spirituality))+geom_col(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+
		theme_ipsum() +
		theme(
			legend.position="right",
			plot.title = element_text(size=11)
		) +
		ggtitle("Distribution of Relationship to Spirituality for Heavy Guardians") +
		xlab("")+scale_x_discrete(labels = function(x) 
			stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")+facet_wrap(~Heavy_Guardian,labeller = to_string)

	#religion#
	religions<-colnames(df3_arch)[71:86]<-c('Christianity','Judaism','Unitarian','Buddishm','Shintoism','Islam','Sikh','Jainism','Hinduism','Taoism','Bahahi','Zoroastranism','Paganism','Earth_centered','Atheist','Agnostic')
	
	df3_arch_relig<-df3_arch %>% group_by(respondent_id)%>%pivot_longer(cols=all_of(religions),names_to="key",values_to = "values_count")
	df3_arch_relig<-df3_arch_relig%>%group_by(respondent_id)%>%filter(!is.na(values_count))
	
	df3_arch_relig_sum<-df3_arch_relig%>%group_by(respondent_id)%>%mutate(Number_of_religions=n_distinct(values_count))

	df3_arch_relig_sum<-df3_arch_relig_sum%>%rename(num_religions=values_count)
	df3_arch_relig_sum<-df3_arch_relig_sum%>%rename(Religion_Chosen=num_religions)
	install.packages('ggdensity')
	install.packages('tidyquant')
	
library(ggdensity)
library(tidyquant)
	ggplot(df3_arch_relig_sum,aes(y=Number_of_religions,x=Religion_Chosen,fill=Religion_Chosen))+geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent")
	
	ggplot(df3_arch_relig_sum,aes(Number_of_religions,fill=Religion_Chosen))+geom_histogram(aes(weight = Number_of_religions), binwidth = 0.1) 
	ggplot(df3_arch_relig_sum,aes(Number_of_religions,fill=Religion_Chosen))+geom_freqpoly(aes(color=Religion_Chosen,binwidth=500))+facet_wrap(~Number_of_religions))

																																												 
ggplot(df3_arch_relig_sum, aes(Number_of_religions, colour = Religion_Chosen)) +
	geom_freqpoly(binwidth = 500)+facet_wrap(~Number_of_religions,scales="free")

ggplot(df3_arch_relig_sum, aes(y=Number_of_religions, x= Religion_Chosen,color=Religion_Chosen)) +
	geom_jitter()+facet_wrap(~Number_of_religions,scales="free")

ggplot(df3_arch_relig_sum, aes(y=sum(Number_of_religions, x= Religion_Chosen)) +geom_hdr(probs=c(0.9,0.5))+geom_point(shape=21,size=3)+scale_fill_tq()+theme_tq()

			 df3_arch_relig_sum_d<-df3_arch_relig_sum %>% 
			 	# Within each grouping of A and B values.
			 	group_by(Number_of_religions , Religion_Chosen) %>% 
			 	# Sort rows in descending order by "value" column.
			 	arrange( desc(Number_of_religions) ) %>% 
			 	# Pick the top 1 value
			 	slice(1) %>% 
			 	# Remember to ungroup in case you want to do further work without grouping.
			 	ungroup()

df3_arch_relig_sum_d<-df3_arch_relig_sum%>%group_by(Religion_Chosen)%>%mutate(total_times_chosen=sum(Number_of_religions))	%>% arrange( desc(Number_of_religions) ) %>% 
	# Pick the top 1 value
	slice(1) %>% 
	# Remember to ungroup in case you want to do further work without grouping.
	ungroup()


%>%order_by(total_times_chosen)%>%slice_max(n=7)

df3_arch_relig_sum_d%>%filter(!is.na(Religion_Chosen))%>%ggplot(.,aes(fct_reorder(Religion_Chosen,desc(Number_of_religions)),y=Number_of_religions,fill=Religion_Chosen))+geom_col(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+geom_text(aes(label = total_times_chosen,vjust=-0.6))+
	theme_ipsum() +
	theme(
		legend.position="right",
		plot.title = element_text(size=11)
	) +
	ggtitle("Total Times A Religion was chosen") +
	xlab("")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")


df3_arch_relig_sum%>%filter(!is.na(Religion_Chosen))%>%ggplot(.,aes(fct_reorder(Religion_Chosen,desc(total_times_chosen)),y=total_times_chosen,color=Religion_Chosen))+geom_histogram(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+geom_text(aes(label = total_times_chosen,vjust=-0.6))+
	theme_ipsum() +
	theme(
		legend.position="right",
		plot.title = element_text(size=11)
	) +
	ggtitle("Total Times A Religion was chosen") +
	xlab("")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")

df3_arch_relig_sum%>%filter(!is.na(Religion_Chosen))%>%ggplot(data=.)+aes(x=Religion_Chosen, y=Number_of_religions) + geom_boxplot()+geom_jitter(aes(color=Religion_Chosen))+facet_wrap(~Number_of_religions)+scale_y_continuous(labels=comma)+ ggtitle('dasdas')


df3_arch_relig_sum%>%filter(!is.na(Religion_Chosen))%>%ggplot(data=.)+aes(x=Religion_Chosen, y=Number_of_religions,group=Religion_Chosen) + geom_boxplot()+geom_jitter(aes(color=Religion_Chosen))+facet_wrap(~Religion_Chosen,scales="free")+scale_y_continuous(labels=comma)+ ggtitle('dasdas')
df3_arch_relig_sum<-as.data.frame(df3_arch_relig_sum)
p_box<-ggplot(df3_arch_relig_sum,aes(x=Religion_Chosen, y=Number_of_religions,group=Religion_Chosen))

p_box+geom_boxplot(outlier.shape = NA,notch=TRUE)+ geom_jitter(aes(color=Religion_Chosen),width = 0.1)+facet_wrap(~Number_of_religions,scales="free",nrow=3)+stat_summary(fun=mean, geom="point", shape=18, size=3, color="white")

p_box+geom_boxplot(outlier.shape = NA,notch=TRUE)+ geom_jitter(aes(color=Religion_Chosen),width = 0.1)+stat_summary(fun=mean, geom="point", shape=18, size=3, color="white")+ggtitle('Distribution of num of times a Religion was chosen')+ylab("Number of Times chosen: up to 3") +xlab("")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))
