# Values#
df_values<-df3
# *columns 38-48 in df3*
	
colnames(df_values)[38:48]<-c('Values_immersive_calming','Values_explores_big_questions','Values_Resource_during_challenges','Values_more_hopeful','Values_expands_perspective','Values_inspires_curiosity','Values_multiple_faiths',
'Values_beyonds_binaries_divisiveness','Values_Nutures_relationships','Values_feel_like_i_belong','Values_other')

df_values2 <- df_values %>%group_by(respondent_id) %>%arrange(collector_id, .by_group = TRUE) %>% summarise(across(everything(),~paste0(unique(na.omit(.x)), collapse = "; ")))


cols_change_values<-names(df_values2[,c(38:48)])

df_values2<-df_values2%>%mutate(across(all_of(cols_change_values),~ifelse(nchar(.x)==0,"No","Yes")))

df_values2<-df_values2%>%mutate_if(is.character,as.factor)
str(df_values2)

df_values2_count<-df_values2%>%mutate(across(all_of(cols_change_values),~ifelse(factor(.x)=="No",0,1)))
df_values3_count<-df_values2_count%>%group_by(Age,Gender)%>%mutate(across(all_of(cols_change_values),~sum(.x)))

df_values3_count_select<-df_values3_count%>%select(Age,Gender,cols_change_values)
#removed where had 0 value#
df_values3_count_select<-df_values3_count_select[-4,]
df_values3_count_select<-df_values3_count_select%>%filter(rowSums(across(where(is.numeric)))!=0)

values_gender %>%filter(Gender!="Prefer not to say"&Gender!="NA")%>%ggplot(.,aes(fct_rev(fct_reorder(Age,n)),y=n))+geom_col(aes(fill=Age))+facet_wrap(~Gender)+geom_text(aes(label=round(n, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+scale_y_continuous(labels=comma,'Total Count Per Within Gender ID')+ggtitle('Distribution of Age by Gender Groups')+scale_x_discrete("Count by Age Group")


## gathering values from wide to long#
# df_values2_count_longer<-df_values2_count%>%pivot_longer(cols=cols_change_values,names_to=c(".value","Values_3"),names_sep="_")

df_values3_count_longer<-df_values2_count%>%pivot_longer(cols=starts_with("values"),values_to="count")

colnames(df_values3_count_longer)[144]<-"name_of_value"
df_values3_count_longer_gender_sum<-df_values3_count_longer%>%group_by(Gender,name_of_value)%>%summarize(value_count=sum(count))

df_values3_count_longer_gender_sum2<-df_values3_count_longer_gender_sum%>%filter(Gender!="")
df_values3_count_longer_gender_sum2<-as.data.frame(df_values3_count_longer_gender_sum2)
df_values3_count_longer_gender_sum2 %>%filter(Gender!="Prefer not to say"&Gender!="Non Binary")%>%ggplot(.,aes(x=reorder(name_of_value,desc(value_count)),y=value_count))+geom_col(aes(fill=Gender))+facet_wrap(~Gender)+geom_text(aes(label=round(value_count, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+scale_x_discrete(position = "top") 

#top 5 values per gender#
df_values3_count_longer_gender_sum2_t5<-df_values3_count_longer_gender_sum2 %>%arrange_(~ desc(value_count)) %>%group_by_(~ Gender) %>%slice(1:5)

#used#
df_values3_count_longer_gender_sum2_t5 %>%filter(Gender!="Prefer not to say"&Gender!="Non Binary")%>%ggplot(.,aes(x=reorder(name_of_value,desc(value_count)),y=value_count))+geom_col(aes(fill=Gender))+facet_wrap(~Gender)+geom_text(aes(label=round(value_count, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+ggtitle('Top 5 Values per Gender')+scale_y_continuous(labels=comma,'Value Frequency')+xlab("")



Ethnicity_Age %>%filter(Age!="13-17"&Ethnicity!="NA")%>%ggplot(.,aes(x=reorder(Age,desc(n)),y=(n)))+geom_col(aes(fill=Age))+facet_wrap(~reorder(Ethnicity,desc(n)))+geom_text(aes(label=round(n, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+scale_y_continuous(labels=comma,'Total Count Per Within Ethnicity')+ggtitle('Distribution of Age by Ethnic Group')+scale_x_discrete("Count by Age Group")

Age_gender %>%filter(Gender!="Prefer not to say"&Gender!="Non Binary")%>%ggplot(.,aes(fct_rev(fct_reorder(Age,n)),y=n))+geom_col(aes(fill=Age))+facet_wrap(~Gender)+geom_text(aes(label=round(n, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+scale_y_continuous(labels=comma,'Total Count Per Within Gender ID')+ggtitle('Distribution of Age by Gender Groups')+scale_x_discrete("Count by Age Group")+xlab("")


#used#
# prop_values_gender<-df_values2_count%>%group_by(Gender)%>%tally()%>%mutate(proportion_per_gender_values = n / sum(n),n_total = sum(n))
# ggplot(prop.count2,aes(x=collector_id,proportion_per_collector_engaging,stat="identity"))+geom_col(aes(fill=collector_id))+ggtitle("Overall Pre-Disposed Engagement level by Collector")+geom_text(aes(label=n))+ylab("Proportion (%)")+geom_text(aes(label=round(proportion_per_collector_engaging,3),position="dodge",vjust=2.55))

#adding in age rrange#

df_values3_count_longer<-df_values3_count_longer%>%mutate(
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

str(df_values3_count_longer$Age_Group)
df_values3_count_longer<-df_values3_count_longer%>%mutate(Age_Group=as.factor(Age_Group))

																													
df_values3_count_longer_Age_sum<-df_values3_count_longer%>%group_by(Age_Group,name_of_value)%>%summarize(value_count=sum(count))
df_values3_count_longer_Age_sum<-df_values3_count_longer_Age_sum%>%filter(!is.na(Age_Group))
df_values3_count_longer_Age_sum_t5<-df_values3_count_longer_Age_sum %>%arrange_(~ desc(value_count)) %>%group_by_(~ Age_Group) %>%slice(1:5)

df_values3_count_longer_Age_sum_t5%>%ggplot(.,aes(x=reorder(name_of_value,desc(value_count)),y=value_count))+geom_col(aes(fill=Age_Group))+facet_wrap(~Age_Group)+geom_text(aes(label=round(value_count, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+ggtitle('Top 5 Values per Age_Group')+scale_y_continuous(labels=comma,'Value Frequency')

#top 3 per age#

df_values3_count_longer_Age_sum_t3<-df_values3_count_longer_Age_sum %>%arrange_(~ desc(value_count)) %>%group_by_(~ Age_Group) %>%slice(1:3)
df_values3_count_longer_Age_sum_t3%>%ggplot(.,aes(x=reorder(name_of_value,desc(value_count)),y=value_count))+geom_col(aes(fill=Age_Group))+facet_grid(~Age_Group,labeller = label_both)+geom_text(aes(label=round(value_count, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+ggtitle('Top 5 Values per Age_Group')+scale_y_continuous(labels=comma,'Value Frequency')


#removing value string for better plotting, use value
df_values3_count_longer_Age_sum_t3<-df_values3_count_longer_Age_sum_t3%>%mutate(Value= sub(".*_", "", value_count))

df_values3_count_longer_Age_sum_t3<-df_values3_count_longer_Age_sum_t3%>%mutate(Value_name= sub(".*_", "", name_of_value))
df_values3_count_longer_Age_sum_t3<-df_values3_count_longer_Age_sum_t3%>%mutate(Value=as.numeric(Value),value_count=as.numeric(value_count))

#used#
df_values3_count_longer_Age_sum_t3_f%>%filter(Age_Group=="75_and_older"&Age_Group=="")
df_values3_count_longer_Age_sum_t3%>%ggplot(.,aes(x=reorder(name_of_value,desc(value_count)),y=Value))+geom_col(aes(fill=Age_Group))+facet_wrap(~Age_Group,labeller = label_both,nrow=3,scales="free")+geom_text(aes(label=round(Value, digits = 2)),position = position_dodge(width=0.9),vjust=0.2)+ggtitle('Top 3 Values per Age_Group')+scale_y_continuous(labels=comma,'Value Frequency')+xlab("Why OnBeing is most Valued")+theme(axis.text=element_text(size=8),
																																																																																																																																																																																																																		 axis.title=element_text(size=14,face="bold"))

#wide to long for what are you interested in seeing more of, starting with this data frame because pivoted longer too?#
#cols 51-57
#

df_values3_count_longer_Age_sum_t3%>%ggplot

df3_arch_spirit<-df3_arch%>%group_by(Relationship_Spirituality)%>%mutate(Spirituality_count=n())
