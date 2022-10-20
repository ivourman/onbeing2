
df3_arch_relig_sum<-df3_arch_relig_sum%>%mutate(Chose_more_than1_relig=ifelse(Number_of_religions>1,1,0))

For_cluster<-df3_arch_relig_sum%>%select(respondent_id,Religion_Chosen,Number_of_religions,Chose_more_than1_relig)


#values#
values_cluster<-df_values2_count%>%select(respondent_id,Values_immersive_calming:Values_feel_like_i_belong)


#Use relationship spituality as clustering ID#

spirit_clust<-df3_arch_vals2 %>% select(respondent_id,Relationship_Spirituality)


clusterset<-inner_join(df3_arch_relig_sum,For_cluster,by="respondent_id")


#Only choosing top value for religion set#
For_cluster<-as.data.frame(For_cluster)
For_cluster_relig<-For_cluster%>%arrange(respondent_id)%>%group_by(Number_of_religions)%>%top_n(2, Number_of_religions)
For_cluster_relig<-For_cluster%>%group_by(respondent_id)%>%slice_max(respondent_id,n=1)

#no religion name here, 7,518 respondents#
For_cluster_relig<-For_cluster%>%group_by(Number_of_religions,Chose_more_than1_relig)%>%distinct(respondent_id)

#For demonstrated values choose mediation How Express values-grace, social healing, volunteering, chose because differentiates most, concats groups, speaks to different catch all- audiences#

#code spirituality for clustering- really defining where spiruality is important part of life#
spirit_clust2<-spirit_clust%>%mutate(Spirituality_important=ifelse(Relationship_Spirituality=="Spirituality is an important part of my life",1,0))
spirit_clustset<-spirit_clust2%>%select(respondent_id,Spirituality_important)


#spirituality in spirit_clustset, religion in For_cluster_relig (rowname either spirituality or religion#)

#maybe take away one here?#
values_express_cluster<-df3_arch%>%select(respondent_id,`How_express_values-Grace`,`How_express_values-Mediation_social_healing`,`How_express_values-Volunteer`)

#merging all cluster sets together#

values_cluster

combined_clust<-left_join(values_express_cluster,spirit_clustset,by="respondent_id")

combined_clust2<-left_join(combined_clust,For_cluster_relig,by="respondent_id")

# for value include ones saw splits before-explroe_big_questions,multiple faiths,resource during challenging times#
why_values_cluster_select<-values_cluster%>%select(respondent_id,Values_explores_big_questions,Values_multiple_faiths,Values_Resource_during_challenges)

combined_clust3<-left_join(combined_clust2,why_values_cluster_select,by="respondent_id")

#Gender and Ethnicity#
cluster_gen_ethn<-df3_arch%>%select(respondent_id,Gender,Ethnicity)

#code in if not Female, or if not White#
cluster_gen_ethn<-cluster_gen_ethn%>%mutate(Gender_code=ifelse(Gender=='Female',1,0))
cluster_gen_ethn<-cluster_gen_ethn%>%mutate(Ethnic_code=ifelse(Ethnicity=='White',1,0))
cluster_gen_ethn_clus<-cluster_gen_ethn%>%select(respondent_id,Gender_code,Ethnic_code)

combined_clust4<-left_join(combined_clust3,cluster_gen_ethn_clus,by="respondent_id")

## spirituality breakdown from clusters#

df3_arch_vals2

arch_spirit_vals<-left_join(df3_arch_vals2,clus_res,by='respondent_id')
arch_spirit_vals<-arch_spirit_vals%>%filter(!is.na(cluster_group))

arch_spirit_vals_sum<-arch_spirit_vals%>%group_by(cluster_group,Relationship_Spirituality)%>%tally()
arch_spirit_vals_sum<-arch_spirit_vals_sum%>%rename(Count=n)
#
#rolling up spirituality#
arch_spirit_vals_roll<-arch_spirit_vals%>%mutate(
	Relationship_Spirituality=case_when(
		Relationship_Spirituality=="Spirituality is an important part of my life" ~"Spirituality is an important or somewhat imporant part of my life",
		Relationship_Spirituality=="Spirituality is a part of my life"~"Spirituality is an important or somewhat imporant part of my life",
		Relationship_Spirituality=="I’m curious about spirituality, but it’s currently not a big part of my life"~"I’m curious or have a complicated relationship with spirituality",
		Relationship_Spirituality=="I have a complicated relationship with spirituality"~"I’m curious or have a complicated relationship with spirituality",
		Relationship_Spirituality =="I don't really think about spirituality" ~"I’m not interested in spirituality",
		Relationship_Spirituality == "I’m not interested in spirituality"~"I’m not interested in spirituality"
	)
)
#replotting with spirtuality condensed#
arch_spirit_vals_roll<-arch_spirit_vals_roll%>%filter(!is.na(Relationship_Spirituality))

arch_spirit_vals_roll_sum<-arch_spirit_vals_roll%>%group_by(cluster_group,Relationship_Spirituality)%>%tally()
arch_spirit_vals_roll_sum<-arch_spirit_vals_roll_sum%>%rename(Count=n)
#new rolled spirit#
ggplot(arch_spirit_vals_roll_sum)+geom_col(aes(x=Count,y = fct_rev(Relationship_Spirituality),fill =Relationship_Spirituality),width=0.7)+           # display data as points
	theme_minimal()+
	theme(legend.position = "bottom") +
	labs(title = "Level of Spiritual Belief",
			 y = "Relationship to Spiritual Values",x="Count")+facet_wrap(~cluster_group)
#used original #
ggplot(arch_spirit_vals_sum)+geom_col(aes(x=Count,y = fct_rev(Relationship_Spirituality),fill =Relationship_Spirituality),width=0.7)+           # display data as points
	theme_minimal()+
	theme(legend.position = "bottom") +
	labs(title = "Level of Spiritual Belief: by cluster",
			 y = "Relationship to Spiritual Values",x="Count")+facet_wrap(~cluster_group)

			 																						
df3_arch_spirit %>% filter(Gender=="Male"|Gender=="Female")%>%ggplot(data =.,
																																		 # set data
																																		 mapping = aes(# map aesthetics to column values
																																		 	x = Relationship_Spirituality,           # map x-axis to age
																																		 	y = count_spirit_gender,         # map y-axis to weight
																																		 	color = Relationship_Spirituality)) +     # map color to age
	geom_point() +           # display data as points
	labs(
		title = "Age and weight distribution",
		subtitle = "Fictional Ebola outbreak, 2014",
		x = "Spirital Value",
		y = "Count by Gender",
		color = "Spirital Value",
		caption = stringr::str_glue("Data as of {max(df3_arch_spirit$count_spirit_gender, na.rm=T)}"))+facet_grid(~Gender)


#also relationship to religion by cluster, found on script 6 mostly#

arch_spirit_vals_relig<-arch_spirit_vals%>%filter(!is.na(cluster_group))

arch_spirit_vals_relig_sum<-arch_spirit_vals_relig%>%group_by(cluster_group,Relationship_Religion)%>%tally()
arch_spirit_vals_relig_sum<-arch_spirit_vals_relig_sum%>%rename(Count=n)


ggplot(arch_spirit_vals_relig_sum)+geom_col(aes(x=Count,y = fct_rev(Relationship_Religion),fill =Relationship_Religion),width=0.7)+           # display data as points
	theme_minimal()+
	theme(legend.position = "bottom") +
	labs(title = "Relationship to Religion: by cluster",
			 y = "Relationship to Religious Values",x="Count")+facet_wrap(~cluster_group)
#listening length#


arch_spirit_vals_LL<-arch_spirit_vals%>%filter(!is.na(cluster_group))

arch_spirit_vals_LL_sum1<-arch_spirit_vals_LL%>%group_by(Listening_length)%>%tally()
arch_spirit_vals_LL_sum1<-arch_spirit_vals_LL_sum1%>%rename(Count=n)

to_clean<-arch_spirit_vals_LL_sum1$Listening_length
arch_spirit_vals_LL_sum1$Listening_length<-str_replace_all(arch_spirit_vals_LL_sum1$Listening_length,"</em>","")
str_replace_all()
ggplot(arch_spirit_vals_LL_sum1)+geom_col(aes(x=Count,y = fct_rev(Listening_length),fill =Listening_length),width=0.7)+           # display data as points
	theme_minimal()+
	theme(legend.position = "bottom") +
	labs(title = "Length of Listenership",
			 y = "Length of time have Listened",x="Count")

#by cluster#
arch_spirit_vals_LL_sum<-arch_spirit_vals_LL%>%group_by(cluster_group,Listening_length)%>%tally()
arch_spirit_vals_LL_sum<-arch_spirit_vals_LL_sum%>%rename(Count=n)
arch_spirit_vals_LL_sum$Listening_length<-str_replace_all(arch_spirit_vals_LL_sum$Listening_length,"<em>","")

ggplot(arch_spirit_vals_LL_sum)+geom_col(aes(x=Count,y = fct_rev(Listening_length),fill =Listening_length),width=0.7)+           # display data as points
	theme_minimal()+
	theme(legend.position = "bottom") +
	labs(title = "Length of Listenership by cluster group",
			 y = "Length of time have Listened",x="Count")+facet_wrap(~cluster_group)
#How they share/engage#

df3_share<-df3_arch
how_share<-c('Share_on_social','write_journal','explore_guest_further','discuss_friends_and_others','discuss_in_group_club','use_for_work','inspired_life_transition')

colnames(df3_share)[51:57]<-how_share

df3_share_long<-df3_share%>%pivot_longer(cols=all_of(how_share),names_to="key",values_to = "values_count")
df3_share_long<-df3_share_long %>% mutate(values_count_num=as.numeric(values_count))
df3_share_long<-df3_share_long%>%mutate(values_count_num2=if_else(is.na(values_count_num),0,1))
df3_share_long_sum<-df3_share_long%>%group_by(key,respondent_id)%>%filter(!is.na(values_count_num)) %>% tally()
df3_share_long_sum2<-df3_share_long_sum%>%group_by(respondent_id) %>% mutate(value_count_person=sum(n))

df3_share_long_sum3<-df3_share_long_sum2%>%group_by(key)%>%summarize(Total_share_or_engage=sum(value_count_person))															
#plotting overall how share or engage-used for overall#
ggplot(df3_share_long_sum3,aes(x=reorder(key,desc(Total_share_or_engage)),y=Total_share_or_engage))+geom_col(aes(fill=key))+ggtitle('Overall way people share or engage after watching show')+scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15))+xlab("")+scale_y_continuous(labels=comma,"Count")+ guides(fill=guide_legend(title="Activity Expressed"))+geom_text(aes(label=Total_share_or_engage),vjust=0.5)
																																					
#putting by age group-left joining age group by respondent_id#
age_respondent_id<-df3_arch%>%select(respondent_id,Age_group)
df3_share_long_sum_age<-left_join(df3_share_long_sum2,age_respondent_id,by='respondent_id')
df3_share_long_sum_age_sum<-df3_share_long_sum_age%>%group_by(key,Age_group)%>%summarize(Total_share_or_engage_by_Age_group=sum(value_count_person))															


df3_share_long_sum_age_sum%>%filter(!is.na(Age_group))%>%ggplot(.,aes(x=reorder(key,desc(Total_share_or_engage_by_Age_group)),y=Total_share_or_engage_by_Age_group))+geom_col(aes(fill=key))+ggtitle('Overall way people share or engage after watching show by Age')+xlab("")+scale_y_continuous(labels=comma,"Count")+ guides(fill=guide_legend(title="Activity Expressed"))+geom_text(aes(label=Total_share_or_engage_by_Age_group),vjust=0.5)+facet_wrap(~Age_group)+
	theme(axis.text.x=element_blank(),
				axis.ticks.x=element_blank() 
	)
																					 

df3_share_long_sum_age_sum%>%filter(!is.na(Age_group))%>%ggplot(.,aes(x=reorder(key,desc(Total_share_or_engage_by_Age_group)),y=Total_share_or_engage_by_Age_group))+geom_col(aes(fill=key))+ggtitle('Overall way people share or engage after watching show by Age')+xlab("")+scale_y_continuous(labels=comma,"Count")+ guides(fill=guide_legend(title="Activity Expressed"))+geom_text(aes(label=Total_share_or_engage_by_Age_group),vjust=0.4,size=3.1)+facet_grid(~Age_group)+
	theme(axis.text.x=element_blank(),
				axis.ticks.x=element_blank() 
	)


#percentage share-used#
df3_share_long_sum_age_sum_p<-df3_share_long_sum_age_sum%>%filter(!is.na(Age_group))%>%group_by(Age_group)%>%mutate(Total_age_pop=sum(Total_share_or_engage_by_Age_group),percent=Total_share_or_engage_by_Age_group/Total_age_pop)

# scale_y_continuous(labels = scales::percent_format(accuracy = 3L))
ggplot(df3_share_long_sum_age_sum_p,aes(x=reorder(key,desc(percent)),y=percent))+geom_col(aes(fill=key))+ggtitle('Overall way people share or engage after watching show by Age')+xlab("")+geom_text(aes(label=round(percent, digits = 2)))+ guides(fill=guide_legend(title="Activity Expressed"))+scale_y_continuous(labels = percent,"Percent of total within age group")+facet_wrap(~Age_group)+
	theme(axis.text.x=element_blank(),
				axis.ticks.x=element_blank() 
	)


# case_data_share <- df3_share_long %>%
# 	count(Gender, key, name = "counts") %>%  # counts by age-gender groups
# 	ungroup() %>%
# 	mutate(
# 		percent = round(100 * (counts / sum(counts, na.rm = T)), 1),
# 		# calculate % of total for age-gender groups
# 		percent = case_when(# convert % to negative if male
# 			Gender == "Female" ~ percent,
# 			Gender == "Male" ~ -percent,
# 			TRUE          ~ NA_real_)
# 	)
