colnames(df_values)[38:48]<-c('Values_immersive_calming','Values_explores_big_questions','Values_Resource_during_challenges','Values_more_hopeful','Values_expands_perspective','Values_inspires_curiosity','Values_multiple_faiths',
															'Values_beyonds_binaries_divisiveness','Values_Nutures_relationships','Values_feel_like_i_belong','Values_other')

df3_values3_count_longer<-
	str(df_values3_count_select)
View(df_values3_count_select)
df3_arch_vals2<-df3_arch
colnames(df3_arch_vals2)[38:47]<-c('Values_immersive_calming','Values_explores_big_questions','Values_Resource_during_challenges','Values_more_hopeful','Values_expands_perspective','Values_inspires_curiosity','Values_multiple_faiths',
															'Values_beyonds_binaries_divisiveness','Values_Nutures_relationships','Values_feel_like_i_belong')

val_cols<-colnames(df3_arch_vals2)[38:47]
df3_arch_vals2_long<-df3_arch_vals2%>%pivot_longer(cols=all_of(val_cols),names_to="key",values_to = "values_count")
df3_arch_vals2_long<-df3_arch_vals2_long %>% mutate(values_count_num=as.numeric(values_count))
df3_arch_vals2_long<-df3_arch_vals2_long%>%mutate(values_count_num=if_else(is.na(values_count_num),0,values_count_num))
df3_arch_vals2_long<-df3_arch_vals2_long %>% mutate(values_count_num=as.numeric(values_count_num))
df3_arch_vals2_long_sum<-df3_arch_vals2_long%>%group_by(key,respondent_id)%>%filter(!!is.na(values_count) %>% summarize(values_total=n())
View(df3_arch_vals2_long_sum)
df3_arch_vals2_long_sum<-df3_arch_vals2_long%>%group_by(respondent_id,key)%>%filter(!is.na(values_count)) %>% tally()
df3_arch_vals2_long_sum2<-df3_arch_vals2_long_sum%>%group_by(respondent_id) %>% mutate(value_count_person=sum(n))
View(df3_arch_vals2_long_sum2)

df3_arch_vals2_long_sum3<-df3_arch_vals2_long%>%group_by(respondent_id,key)%>%mutate(values_count_num2=as.numeric(values_count))
View(df3_arch_vals2_long_sum3)
										df3_arch_key_sum<-df3_arch_vals2_long_sum3%>%group_by(key,values_count_num2)%>%tally()																					 
										df3_arch_key_sum2<-df3_arch_key_sum
										df3_arch_key_sum2$key2<-str_replace_all(df3_arch_key_sum2$key,"Values_","")
										ggplot(df3_arch_key_sum2,aes(x=reorder(key2,desc(n)),y=n))+geom_col(aes(fill=key2))+ggtitle('Overall Expression of Why Valued')+scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15))+xlab("")+scale_y_continuous(labels=comma,"Count")+ guides(fill=guide_legend(title="Value Expressed"))

										
										
										pacman::p_load(rio,       # to import data
																	 here,      # to locate files
																	 tidyverse, # to clean, handle, and plot the data (includes ggplot2 package)
																	 apyramid,  # a package dedicated to creating age pyramids
																	 janitor,   # tables and cleaning data
																	 stringr)   # working with strings for titles, captions, etc.
																																											
										apyramid::age_pyramid(data = df3_arch,
																					age_group = "Age_group",
																					split_by = "Gender",
																					proportional = TRUE)
										apyramid::age_pyramid(data = df3_arch,
																					age_group = "Age_group",
																					split_by = "Gender",
																				)
MF_Only<-df3_arch%>%filter(Gender=="Male"|Gender=="Female")
MF_Only<-MF_Only%>%mutate(Gender=as.factor(Gender))
MF_Only<-MF_Only%>%filter(Gender!="Non Binary")
MF_Only<-MF_Only%>%filter(Gender!="Prefer not to say")
MF_Only<-MF_Only%>%filter(Gender=="Male"|Gender=="Female")
MF_Only$Age_group<-factor(MF_Only$Age_group,levels=c("Under_25","25-34","35-44","55-64","65-74","75_and_older"))
Gender_Other<-c(3,4)
MF_only<-MF_Only%>%filter(as.numeric(Gender)<3)
MF_Only<-MF_Only%>%filter(!as.integer(Gender)%in%Gender_Other)
MF_Only<-MF_Only%>%filter(!is.na(Age_group))
MF_only<-as.data.frame(MF_only)
									apyramid::age_pyramid(data = MF_Only,	age_group = "Age_group",split_by = "Gender",proportional = TRUE,na.rm = TRUE)

									apyramid::age_pyramid(
										data = MF_only,
										age_group = "Age_group",
										split_by = "Gender",
										proportional = TRUE,              # show percents, not counts
										show_midpoint = FALSE,            # remove bar mid-point line
										#pal = c("orange", "purple")      # can specify alt. colors here (but not labels)
									)+theme_minimal()+scale_fill_manual(
											values = c("orange", "purple"),              
											labels = c("m" = "Male", "f" = "Female"))+labs(y = "Percent of all cases",
												 x = "Age categories",                          
												 fill = "Gender", 
												 caption = "My data source and caption here",
												 title = "Title of my plot",
												 subtitle = "Subtitle with \n a second line...")+theme(
											legend.position = "bottom",                          # legend to bottom
											axis.text = element_text(size = 10, face = "bold"),  # fonts/sizes
											axis.title = element_text(size = 12, face = "bold"))
									
									apyramid::age_pyramid(
										data = MF_only,
										age_group = "Age_group",
										split_by = "Gender",
										proportional = TRUE,    
										show_midpoint = FALSE)+theme_minimal()+ scale_fill_manual(values = c("orange", "purple"),labels = c("m" = "Male", "f" = "Female"))+labs(y = "Percent of all cases",x = "Age categories", fill = "Gender",
												 caption = "My data source and caption here",
												 title = "Title of my plot",
												 subtitle = "Subtitle with \n a second line...")+theme(legend.position = "bottom",axis.text = element_text(size = 10, face = "bold"), axis.title = element_text(size = 12, face = "bold"))
	
									# begin ggplot
									ggplot(mapping = aes(x = Age, fill = Ethnicity)) +
										
										# female histogram
										geom_histogram(data = MF_only %>% filter(Gender == "Female"),
																	 breaks = seq(0,85,5),
																	 colour = "white") +
										
										# male histogram (values converted to negative)
										geom_histogram(data = MF_only %>% filter(Gender == "Male"),
																	 breaks = seq(0,85,5),
																	 mapping = aes(y = ..count..*(-1)),
																	 colour = "white") +
										
										# flip the X and Y axes
										coord_flip() +
										
										# adjust counts-axis scale
										scale_y_continuous(limits = c(-2000, 5000),
																			 breaks = seq(-2000,5000,200),
																			 labels = abs(seq(-2000, 5000, 200)))
									
									###
									###
									max_per <- max(pyramid_data$percent, na.rm=T)
									min_per <- min(pyramid_data$percent, na.rm=T)
									
								
									pyramid_data <- MF_only %>%
										count(Age_group,
													Gender,
													name = "counts") %>% 
										ungroup() %>%                 # ungroup so percents are not by group
										mutate(percent = round(100*(counts / sum(counts, na.rm=T)), digits = 1), 
													 percent = case_when(
													 	Gender == "Female" ~ percent,
													 	Gender == "Male" ~ -percent,     # convert male to negative
													 	TRUE          ~ NA_real_))    # NA val must by numeric as well
									# **Used**
									# begin ggplot
									ggplot(pyramind_no_na)+  # default x-axis is age in years;
									
										# case data graph
										geom_col(data = pyramind_no_na,
														 mapping = aes(
														 	x = Age_group,
														 	y = percent,
														 	fill = Gender),         
														 colour = "white")+       # white around each bar
										
										# flip the X and Y axes to make pyramid vertical
										coord_flip()+
										
										
										# adjust the axes scales
										# scale_x_continuous(breaks = seq(0,100,5), labels = seq(0,100,5)) +
										scale_y_continuous(
											limits = c(min_per, max_per),
											breaks = seq(from = floor(min_per),                # sequence of values, by 2s
																	 to = ceiling(max_per),
																	 by = 2),
											labels = paste0(abs(seq(from = floor(min_per),     # sequence of absolute values, by 2s, with "%"
																							to = ceiling(max_per),
																							by = 2)),
																			"%"))+  
										
										# designate colors and legend labels manually
										scale_fill_manual(
											values = c("Female" = "orange",
																 "Male" = "darkgreen"),
											labels = c("Female", "Male")) +
										
										# label values (remember X and Y flipped now)
										labs(
											title = "Age and gender of cases",
											x = "Age group",
											y = "Percent of total",
											fill = NULL,
											caption = stringr::str_glue("Data are from linelist \nn = {nrow(MF_only)} (age or sex missing for {sum(is.na(MF_only$Gender) | is.na(MF_only$Age_group))} cases) \nData as of: {format(Sys.Date(), '%d %b %Y')}")) +
										
										# display themes
										theme(
											panel.grid.major = element_blank(),
											panel.grid.minor = element_blank(),
											panel.background = element_blank(),
											axis.line = element_line(colour = "black"),
											plot.title = element_text(hjust = 0.5), 
											plot.caption = element_text(hjust=0, size=11, face = "italic")
										)

									
									
									ggplot()+  # default x-axis is age in years;
										
										# case data graph
										geom_col(data = pyramid_data,
														 mapping = aes(
														 	x = Age_group,
														 	y = percent,
														 	fill = Gender),         
														 colour = "white")+       # white around each bar
										
										# flip the X and Y axes to make pyramid vertical
										coord_flip()+
										
										
										# adjust the axes scales
										# scale_x_continuous(breaks = seq(0,100,5), labels = seq(0,100,5)) +
										scale_y_continuous(
											limits = c(min_per, max_per),
											breaks = seq(from = floor(min_per),                # sequence of values, by 2s
																	 to = ceiling(max_per),
																	 by = 2),
											labels = paste0(abs(seq(from = floor(min_per),     # sequence of absolute values, by 2s, with "%"
																							to = ceiling(max_per),
																							by = 2)),
																			"%"))+  
										
										# designate colors and legend labels manually
										scale_fill_manual(
											values = c("Female" = "orange",
																 "Male" = "darkgreen"),
											labels = c("Female", "Male")) +
										
										# label values (remember X and Y flipped now)
										labs(
											title = "Age and gender of cases",
											x = "Age group",
											y = "Percent of total",
											fill = NULL,
											caption = stringr::str_glue("Data are from linelist \nn = {nrow(MF_only)} (age or sex missing for {sum(is.na(MF_only$Gender) | is.na(MF_only$Age_group))} cases) \nData as of: {format(Sys.Date(), '%d %b %Y')}")) +
										
										# display themes
										theme(
											panel.grid.major = element_blank(),
											panel.grid.minor = element_blank(),
											panel.background = element_blank(),
											axis.line = element_line(colour = "black"),
											plot.title = element_text(hjust = 0.5), 
											plot.caption = element_text(hjust=0, size=11, face = "italic")
										)				
									
									
									case_data <- df3_arch_relig %>%
										count(Age_group, gender, name = "counts") %>%  # counts by age-gender groups
										ungroup() %>%
										mutate(
											percent = round(100 * (counts / sum(counts, na.rm = T)), 1),
											# calculate % of total for age-gender groups
											percent = case_when(# convert % to negative if male
												gender == "f" ~ percent,
												gender == "m" ~ -percent,
												TRUE          ~ NA_real_)
										)
									
									
									df3_arch_relig%>%filter(!as.integer(Gender)%in%Gender_Other)%>%								ggplot(data = df3_arch_relig, mapping = aes(x = values_count, fill = Gender)) +
										geom_density(size = 2, alpha = 0.2, position = "stack")+
										labs(title = "'Stacked' proportional densities")+facet_wrap(~Gender)
								
									
									df3_arch_relig%>%	ggplot(                                                        # begin ggplot!
										mapping = aes(x = key, fill = values_count))+
										geom_bar(position = "fill", col = "black") +                    
										theme_classic() +
										labs(
											x = "Value",
											y = "Value status (proportion)"
										)	
									age_by_Religion <- ggplot(data = df3_arch_spirit,
																			# set data
																			mapping = aes(# map aesthetics to column values
																				x = Relationship_Spirituality,           # map x-axis to age
																				y = count_spirit_gender,         # map y-axis to weight
																				color = Gender)) +     # map color to age
										geom_point() +           # display data as points
										labs(
											title = "Age and weight distribution",
											subtitle = "Fictional Ebola outbreak, 2014",
											x = "Age in years",
											y = "Weight in kilos",
											color = "Religion_Chosen",
											caption = stringr::str_glue("Data as of {max(df3_arch_relig_sum$Number_of_religions, na.rm=T)}")
										)
									
									age_by_wt
									
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
									
						#Used#			
						df3_arch_spirit %>% filter(Gender == "Male" |Gender == "Female")%>%ggplot(data=.)+geom_col(aes(x=count_spirit_gender,y = fct_rev(Relationship_Spirituality),fill =Gender),width=0.7)+           # display data as points
	theme_minimal()+
	theme(legend.position = "bottom") +
	labs(title = "Spirital Values: Number by Gender",
			 y = "Spiritual Value",x="Count")
							gsave("gender_spirit.png")
#rolling  up spirituality#
							df3_arch_spirit_roll<-df3_arch_spirit%>%mutate(
								Relationship_Spirituality=case_when(
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
library(apyramid)
							
							Age_ethnicity <-
								MF_only %>% filter(Ethnicity!="Don’t wish to answer",Ethnicity!="Something else")%>% mutate(Age_group = as.factor(Age_group)) %>% count(Age_group, Ethnicity, name = "counts") %>%
								## ensure that age and sex are factors
								group_by(Age_group, Ethnicity) %>%
								## add the counts for each health district together
								summarise(population = sum(counts)) %>%
								## remove the grouping so can calculate overall proportion
								ungroup() %>%
								mutate(proportion = population / sum(population)) %>%
								## plot pyramid
								age_pyramid(
									age_group =  Ethnicity,
									split_by = Age_group,
									count = proportion,
									proportional = TRUE
								) +
								## only show the y axis label (otherwise repeated in all three plots)
								labs(title = "Ethnicity by Age",
										 y = "",
										 x = "Ethnicity group",
										 color="Age_Group")
								## make the x axis the same for all plots 
							plot(Age_ethnicity)

							plot(Age_ethnicity)+geom_col(aes(x=Ethnicity,y=Age_group,fill=proportions,width=0.7)+theme_minimal())
							

#adding in cluster groups#
df3_arch_clus<-left_join(df3_arch,clus_res,by="respondent_id")
MF_only_clus<-left_join(MF_only,clus_res,by="respondent_id")


# 
# df3_arch_spirit %>% filter(Gender == "Male" |Gender == "Female")%>%ggplot(data=.)+geom_col(aes(x=count_spirit_gender,y = fct_rev(Relationship_Spirituality),fill =Gender),width=0.7)+           # display data as points
# 	theme_minimal()+
# 	theme(legend.position = "bottom") +
# 	labs(title = "Spirital Values: Number by Gender",
# 			 y = "Spiritual Value",x="Count")
# gsave("gender_spirit.png")

df3_arch_vals2_long_sum3_clus<-left_join(df3_arch_vals2_long_sum3,clus_res,by='respondent_id')
df3_arch_vals2_long_sum3_clus<-df3_arch_vals2_long_sum3_clus%>%filter(!is.na(cluster_group))
df3_arch_vals2_long_sum3_clus2<-df3_arch_vals2_long_sum3_clus%>%group_by(respondent_id,key)%>%mutate(values_count_num2=as.numeric(values_count))
# View(df3_arch_vals2_long_sum3)
df3_arch_vals2_long_sum3_clus2_sum<-df3_arch_vals2_long_sum3_clus2%>%group_by(cluster_group,key)%>%summarize(value_per_cluster=sum(values_count_num2))																					 
# df3_arch_key_sum2<-df3_arch_key_sum
df3_arch_vals2_long_sum3_clus2_sum$key2<-str_replace_all(df3_arch_vals2_long_sum3_clus2_sum$key,"Values_","")
df3_arch_vals2_long_sum3_clus2_sum<-as.data.frame(df3_arch_vals2_long_sum3_clus2_sum)
ggplot(df3_arch_vals2_long_sum3_clus2_sum,aes(x=reorder(key2,desc(value_per_cluster)),y=value_per_cluster))+geom_col(aes(fill=key2))+ggtitle('Overall Expression of Why Valued')+scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15))+xlab("")+scale_y_continuous(labels=comma,"Count")+ guides(fill=guide_legend(title="Value Expressed"))

#used#
ggplot(df3_arch_vals2_long_sum3_clus2_sum,aes(x=reorder(key2,desc(value_per_cluster)),y=value_per_cluster))+geom_col(aes(fill=key2))+ggtitle('Why Valued by cluster-group')+scale_x_discrete(labels = function(x)stringr::str_wrap(x, width = 15))+xlab("")+scale_y_continuous(labels=comma,"Count")+ guides(fill=guide_legend(title="Value Expressed"))+facet_wrap(~cluster_group,nrow=3)+geom_text(aes(label=round(value_per_cluster, digits = 2)),position = position_dodge(width=0.7),size=3)


#Religion cluster##

df3_arch_relig_sum_clus<-left_join(df3_arch_relig_sum,clus_res,by='respondent_id')
Religclus2<-left_join(For_cluster,clus_res,by='respondent_id')
Religclus2<-as.data.frame(Religclus2)
Religclus2_sum<-Religclus2%>%group_by(Religion_Chosen)%>%mutate(Total_times_cluster_chose_religion=nrow(cluster_group))

df3_arch_relig_sum_d_clus<-df3_arch_relig_sum_clus%>%group_by(Religion_Chosen)%>%mutate(total_times_chosen=sum(Number_of_religions))	%>% arrange( desc(Number_of_religions) ) %>% 
	# Pick the top 1 value
	slice(1) %>% 
	# Remember to ungroup in case you want to do further work without grouping.
	ungroup()


df3_arch_relig_sum_d_clus%>%filter(!is.na(Religion_Chosen))%>%ggplot(.,aes(fct_reorder(cluster_group,desc(Number_of_religions)),y=Number_of_religions,fill=Religion_Chosen))+geom_col(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+geom_text(aes(label = total_times_chosen,vjust=-0.6))+
	theme_ipsum() +
	theme(
		legend.position="right",
		plot.title = element_text(size=11)
	) +
	ggtitle("Total Times A Religion was chosen") +
	xlab("")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")

Religclus2_sum<-Religclus2%>%group_by(cluster_group,Religion_Chosen)%>%summarize(total_times_religion_chosen_in_cluster=n())

Religclus2_sum%>%filter(!is.na(cluster_group))%>%ggplot(.,aes(fct_reorder(Religion_Chosen,desc(total_times_religion_chosen_in_cluster)),y=total_times_religion_chosen_in_cluster,fill=Religion_Chosen))+geom_col(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+geom_text(aes(label = total_times_religion_chosen_in_cluster,vjust=-0.2))+
	theme_ipsum() +
	theme(
		legend.position="right",
		plot.title = element_text(size=11)
	) +
	ggtitle("Total Times A Religion was chosen per cluster") +
	xlab("")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")+facet_wrap(~cluster_group,scales="free")
#combining atheist/agnostic#
Religclus2_sum<-Religclus2_sum%>%mutate(Religion_Chosen=as.character(Religion_Chosen))
Religclus2_sum_roll<-Religclus2_sum%>%mutate(Religion_Chosen=ifelse(Religion_Chosen=='Agnosticism','Atheist/Agnostic',Religion_Chosen))
Religclus2_sum_roll<-Religclus2_sum_roll%>%mutate(Religion_Chosen=ifelse(Religion_Chosen=='Atheism','Atheist/Agnostic',Religion_Chosen))
Religclus2_sum_roll2<-Religclus2_sum%>%group_by(Religion_Chosen,cluster_group)%>%summarize(total_times_religion_chosen_in_cluster=sum(total_times_religion_chosen_in_cluster))
Religclus2_sum_roll2<-Religclus2_sum_roll2%>%filter(!is.na(Religion_Chosen))
Religclus2_sum_roll2<-Religclus2_sum_roll2%>%filter(!is.na(cluster_group))

Religclus2_sum_roll<-Religclus2_sum%>%mutate(Religion_Chosen=ifelse(Religion_Chosen=='Agnosticism','Atheism',Religion_Chosen))
Religclus2_sum_roll2<-Religclus2_sum%>%group_by(Religion_Chosen,cluster_group)%>%mutate(total_times_religion_chosen_in_cluster_group=sum(total_times_religion_chosen_in_cluster))
Religclus2_sum_roll2<-Religclus2_sum_roll2%>%mutate(Religion_Chosen2 = replace(Religion_Chosen, Religion_Chosen == "Agnosticism" | Religion_Chosen == "Atheism", "Atheist/Agnostic")) 
Religclus2_sum_roll2<-Religclus2_sum_roll2%>%group_by(Religion_Chosen2,cluster_group)%>%mutate(total_times_religion_chosen_in_cluster_group=sum(total_times_religion_chosen_in_cluster))

#replotting with rolled atheist/agnostic-used#
Religclus2_sum_roll2%>%filter(!is.na(cluster_group))%>%filter(total_times_religion_chosen_in_cluster_group>13)%>%ggplot(.,aes(fct_reorder(Religion_Chosen2,desc(total_times_religion_chosen_in_cluster_group)),y=total_times_religion_chosen_in_cluster_group,fill=Religion_Chosen2))+geom_col(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+geom_text(aes(label = total_times_religion_chosen_in_cluster_group,vjust=0.2))+
	theme_ipsum() +
	theme(
		legend.position="right",
		plot.title = element_text(size=11)
	) +
	ggtitle("Total Times A Religion was chosen per cluster") +
	xlab("")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")+facet_wrap(~cluster_group,scales="free")

#
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
#
df3_arch_relig_sum_clus%>%filter(!is.na(Religion_Chosen))%>%ggplot(data=.)+aes(x=Religion_Chosen, y=Number_of_religions,group=Religion_Chosen) + geom_boxplot()+geom_jitter(aes(color=Religion_Chosen))+facet_wrap(~cluster_group,scales="free")+scale_y_continuous(labels=comma)+ ggtitle('dasdas')
#

df3_arch_relig_sum<-as.data.frame(df3_arch_relig_sum)
p_box2<-ggplot(df3_arch_relig_sum_clus,aes(x=Religion_Chosen, y=Number_of_religions,group=Religion_Chosen))

p_box2+geom_boxplot(outlier.shape = NA,notch=TRUE)+ geom_jitter(aes(color=Religion_Chosen),width = 0.1)+facet_wrap(~Number_of_religions,scales="free",nrow=3)+stat_summary(fun=mean, geom="point", shape=18, size=3, color="white")+ggtitle('Distribution of Religious Affiliations by number religions chosen')

#
df3_arch_relig_sum_clus%>%filter(!is.na(cluster_group))
p_box2a<-df3_arch_relig_sum_clus%>%filter(!is.na(cluster_group))%>%ggplot(.,aes(x=Religion_Chosen, y=Number_of_religions,group=cluster_group))

p_box2a+geom_boxplot(outlier.shape = NA,notch=TRUE)+ geom_jitter(aes(color=Religion_Chosen),width = 0.1)+facet_wrap(~cluster_group,nrow=3)+stat_summary(fun=mean, geom="point", shape=18, size=3, color="white")+ggtitle('Distribution of number of times religion was chosen by cluster group')


p_box+geom_boxplot(outlier.shape = NA,notch=TRUE)+ geom_jitter(aes(color=Religion_Chosen),width = 0.1)+stat_summary(fun=mean, geom="point", shape=18, size=3, color="white")+ggtitle('Distribution of num of times a Religion was chosen')+ylab("Number of Times chosen: up to 3") +xlab("")+scale_x_discrete(labels = function(x) 
	stringr::str_wrap(x, width = 15))




df3_arch_relig_sum_clus%>%filter(!is.na(Religion_Chosen))%>%ggplot(.,aes(fct_reorder(Religion_Chosen,desc(total_times_chosen)),y=total_times_chosen,color=Religion_Chosen))+geom_histogram(position="dodge")+scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+geom_text(aes(label = total_times_chosen,vjust=-0.6))+
	theme_ipsum() +
	theme(
		legend.position="right",
		plot.title = element_text(size=11)
	) +
	ggtitle("Total Times A Religion was chosen") +
	xlab("")+scale_x_discrete(labels = function(x) 
		stringr::str_wrap(x, width = 15))+scale_y_continuous(labels=comma,"Count")

