install_github('mattroumaya/doubleheadr')

library(tidyverse)
library(devtools)
library(readxl)
library(doubleheadr)
demo <- doubleheadr::demo



df4k_demo<-read_excel('/Users/ilanvourman/Documents/OnBeing/OnBeing Survey Drafts/SurveyJune16_Missing_to_use_copy.xlsx')

df4k_demo%>%clean_headr(rep_val = '...', clean_names = TRUE)%>%colnames()
View(df4k_demo)

df4k_demo2 <- df4k_demo %>% clean_headr(rep_val = '...') %>% trim_headr(c('please_provide_your_contact_', '_response'))

#after column 120 here all email sign ups, have to see if rest of data aligns with full sheet#
#save.image("~/OnBeing/OnBeingAnalysis2.RData")
save.image("~/OnBeingAnalysis2.RData")
savehistory("~/OnBeing/OnBeinghistory2.Rhistory")
# uploading Full Responses##
# 
full_demo<-read_excel('/Users/ilanvourman/Documents/OnBeing/OnBeing Survey Drafts/Survey_condensed_use_IV1_formatted.xlsx')
full_demo%>%clean_headr(rep_val = '...', clean_names = TRUE)%>%colnames()
full_demo2 <- full_demo %>% clean_headr(rep_val = '...') %>% trim_headr(c('please_provide_your_contact_', '_response'))


devtools::install_github("mattroumaya/surveymonkey")

usethis::edit_r_profile()

library(surveymonkey)

options(sm_oauth_token = "cOmUpOXfevWx560egh1njqAG36aYQUiG8wE.tB9lPUwPn0A56fJlF0kEPRG6KymF-O4p29apyFKc0zAngtu.sjnkgNw04SqHUcyg4Mwshj5cAc1E2LCMsO54DNDjInDJ")

getOption("sm_oauth_token")

surveys<- surveymonkey::browse_surveys(10)
View(surveys)

a_survey_obj <- surveymonkey::fetch_survey_obj(506448196)
survey_df <- surveymonkey::parse_survey(a_survey_obj)
View(survey_df)


collectors <- get_collectors(506448196)
rollups<-surveymonkey::fetch_survey_obj

survey_df_1 <- surveymonkey::parse_survey(a_survey_obj,oauth_token="cOmUpOXfevWx560egh1njqAG36aYQUiG8wE.tB9lPUwPn0A56fJlF0kEPRG6KymF-O4p29apyFKc0zAngtu.sjnkgNw04SqHUcyg4Mwshj5cAc1E2LCMsO54DNDjInDJ",fix_duplicates = 'drop')

str(survey_df_1)
View(survey_df_1)

#write.csv(survey_df_1,"survey_df1_parsed.csv")
survey_df_2 <-strip_html(survey_df_1)
write.csv(survey_df_2,"survey_df_2.csv")
##
#**Take IDs where prefered way of on being is NA and how long have you been a listener isn't NA to get missing responses**#

write.csv(df4k_demo2,'df4k_responses.csv')

select_4k <- df4k_demo2%>%select("whats_your_preferred_way_of_listening_to_on_being_select_one_i_attentively_listen_to_the_show_without_distractions" ,"whats_your_preferred_way_of_listening_to_on_being_select_one_i_listen_while_i_aom_doing_other_activities_for_example_making_art_cleaning_commuting_working_out_at_work","whats_your_preferred_way_of_listening_to_on_being_select_one_i_take_notes_while_i_listen_to_the_show","whats_your_preferred_way_of_listening_to_on_being_select_one_i_listen_with_other_people","whats_your_preferred_way_of_listening_to_on_being_select_one_other_please_specify")         

colnames(select_4k)<-c('attentive','while_doing_other','while_taking_notes','with_other_people','other')
select_4k2<-select_4k%>%mutate(attentive=ifelse(attentive!='NA','I attentively listen to the show, without distractions',NA))
select_4k2 <- select_4k2%>%mutate(while_doing_other=ifelse(while_doing_other!='NA',"I listen while I'm doing other activities (for example, making art, cleaning, commuting, working out, at work)",NA))
                               
select_4k2 <- select_4k2 %>% mutate(while_taking_notes=ifelse(while_taking_notes!='NA','I take notes while I listen to the show',NA))

select_4k2 <-select_4k2 %>% mutate(with_other_people=ifelse(with_other_people!='NA','I listen with other people',NA))
select_4k2 <-select_4k2 %>% mutate(other=ifelse(other!='NA',other,NA))
install.packages('naniar')
library('naniar')
select_4k3 <- select_4k2 %>% replace_with_na_all(condition = ~.x =='NA')

write.csv(select_4k3,'select_4k3.csv')
df<-select_4k3
#for (i in 2:ncol(df))
#df[,i] = ifelse(is.na(df[,i]), df[,i-1],df[,i])#

#df[] <- Reduce(function(x, y) ifelse(is.na(x), x, y), df, accumulate = TRUE)#

df2<-data.frame(t(df)) %>%
  fill(., names(.)) %>%
  t()#

#do not run 
#df$"What's your preferred way of listening to On Being?" <- ifelse(is.na(df$other), df$with_other_people, df$other)
# df$"What's your preferred way of listening to On Being?" <- ifelse(is.na(df$other), df$while_taking_notes, df$other)
# df$"What's your preferred way of listening to On Being?" <- ifelse(is.na(df$other), df$while_doing_other, df$other)
# df$"What's your preferred way of listening to On Being?" <- ifelse(is.na(df$other), df$"What's your preferred way of listening to On Being?", df$other)#
# #
#
###now df2$other becomes "What's your preferred way of listening to On Being?"#

df2<-as.data.frame(df2)
write.csv(df2,'df2.csv')


#now take survey df2 and use Full_preferred_way corrected column starting respondant 118062235156#
#using preferred fixed csv3
preferred_fixed<-read.csv(file.choose(),stringsAsFactors = FALSE)
survey_df_2<-survey_df_2%>%mutate(respondent_id=as.numeric(respondent_id))
survey_df3<-cbind(survey_df_2,preferred_fixed)
#getting rid of duplicate respondant ID#
survey_df3<-survey_df3%>%select(-140)

#columns that contain numbers?#
library(dplyr)
#numeric.cols<-survey_df3 %>%select(contains('br'))
write.csv(survey_df3,'survey_df3.csv')


#survey_df3_distinct<-survey_df3%>%distinct(respondent_id)
survey_df4<-mutate_if(survey_df3,is.character,as.factor)
survey_df4_no_NA<-survey_df4%>%n(survey_d)
#
survey_df4_sum1 <- survey_df4 %>%group_by(`How old are you?`,`How do you interact or engage with On Being? Select all that apply. - I follow the On Being Project on social media`)%>%filter(!is.na(`How old are you?`),!is.na(`How do you interact or engage with On Being? Select all that apply. - I follow the On Being Project on social media`))%>%summarise(n=n())
survey_df4_sum1
survey_df4_sum1<-as.data.frame(survey_df4_sum1)

ggplot(survey_df4_sum1,aes(x=`How old are you?`,color=`How do you interact or engage with On Being? Select all that apply. - I follow the On Being Project on social media`))+geom_histogram(binwith=0.5,stat="count")

#used#
ggplot(survey_df4_sum1,aes(x=`How old are you?`,n,stat="identity"))+geom_col(aes(fill=`How old are you?`))+ggtitle("I Follow On Being on Social Media")+geom_text(aes(label=n))
       

survey_df4_sum_age_gender <- survey_df4 %>%group_by(`How old are you?`,`How do you identify?`)%>%filter(!is.na(`How old are you?`),!is.na(`How do you identify?`))%>%summarise(`How do you interact or engage with On Being? Select all that apply. - I follow the On Being Project on social media`=n())

ggplot(survey_df4_sum_age_gender,aes(x=`How old are you?`,`How do you interact or engage with On Being? Select all that apply. - I follow the On Being Project on social media`,stat="identity"))+geom_col(aes(fill=`How do you identify?`))+ggtitle("I Follow On Being on Social Media")+geom_text(aes(label=`How do you interact or engage with On Being? Select all that apply. - I follow the On Being Project on social media`))+facet_wrap(~`How do you identify?`)+scale_y_continuous('Follow on social')


colnames(survey_df4[105:110])

survey_df4_sum_age_gender 
# 
# ggplot(survey_df4,aes(x=`How old are you?`,color="How much do you agree with the following statements? For each row, rank your reactions from 1 (\"Strongly disagree\") to 5 (\"Strongly agree\").  Please select one answer per row. - I often get deeply lost in my thoughts"))+geom_histogram(binwith=0.5,stat="count")+facet_wrap(~`How do you identify?`)

# ˆtaking out extra long names from header
survey_df3a<-survey_df3
survey_df3a <- survey_df3 %>% trim_headr(c('.Select.all.that.apply.', 'How.do.you.interact.or.engage.with.On.Being.')) 

# columns 26 to 35 are all how do you interact or engage with on being, with last column
colnames(survey_df3a[26:35])

# 
survey_df3a_select <- survey_df3a%>%dplyr::select(c(1:3,26:35))

                                           

# duplicates<-fullc1%>%janitor::get_dupes()
colnames(survey_df3a_select)

survey_df3a_select2<-survey_df3a_select%>%trim_headr(' - ')

# key_colsM=c()
# survey_df3a_select_distinct<-survey_df3a_select%>%distinct(collector_id,respondent_id,.keep_all=TRUE)%>%group_by(respondent_id)
# 
# library(stringr)
# survey_df3a_select2a<-make.names(names=names(survey_df3a_select2), unique=TRUE, allow_ = TRUE)
# names(survey_df3a_select)<-colnames(survey_df3a_select2a)
library(dplyr)
cases_rolled <- survey_df3a_select2 %>%group_by(respondent_id) %>%arrange(collector_id, .by_group = TRUE) %>% summarise(across(everything(),~paste0(unique(na.omit(.x)), collapse = "; ")))

#renaming columns for how listen/engage to have clearer names#
cases_rolled2<-cases_rolled
colnames(cases_rolled2)[4:13]<-c('Follow_social',"Listen_on_radio","Listen_to_podcast","Listen_Poetry_Unbound","Tried_wisdom_app","I_read_newsletter","I_visit_website","Attended_events","Have_donated","Don't_interact")
str(cases_rolled2)
Turning into True False
cols_change<-names(cases_rolled2[,c(4:13)])

cases_rolled3<-cases_rolled2%>%mutate(across(all_of(cols_change),~ifelse(nchar(.x)==0,"No","Yes")))

# save.image("~/OnBeingAnalysis3.RData")                                    
cases_rolled3<-cases_rolled3%>%mutate_if(is.character,as.factor)
str(cases_rolled3)

prop.count<-cases_rolled3%>%mutate(across(all_of(cols_change),~ifelse(factor(.x)=="No",0,1)))
#used#
prop.count2<-prop.count%>%group_by(collector_id)%>%tally()%>%mutate(proportion_per_collector_engaging = n / sum(n),n_total = sum(n))
ggplot(prop.count2,aes(x=collector_id,proportion_per_collector_engaging,stat="identity"))+geom_col(aes(fill=collector_id))+ggtitle("Overall Pre-Disposed Engagement level by Collector")+geom_text(aes(label=n))+ylab("Proportion (%)")+geom_text(aes(label=round(proportion_per_collector_engaging,3),position="dodge",vjust=2.55))

                                                                                                                                                                                                                                                  
library(dplyr)
survey_df3b<-full_join(survey_df3a,prop.count,by=c('respondent_id'))
survey_df3b<-full_join(survey_df3a,prop.count,by=c('respondent_id'),keep=FALSE)
survey_df3b<-survey_df3b%>%select(-survey_id.x,-collector_id.x)%>%rename(survey_id=survey_id.y)
survey_df3b<-survey_df3b %>% rename(collector_id=collector_id.y)
now removing prefix for how express values

colnames(survey_df3b)[90:102]
colnames(survey_df3b)[90:102]<-c('How_express_values-Kindess_hospitality','How_express_values-Grace','How_express_values-Volunteer','How_express_values-community_service','How_express_values-Giving_philanthropy','How_express_values-Community_organizing',"How_express_values-Mediation_social_healing","How_express_values-Mutual_aid","How_express_values-Political_engagement","How_express_values-Evangelism","How_express_values-Social_activism","How_express_values-Environmental_activism","How_express_values-other")

colnames_values<-colnames(survey_df3b)[90:102]
str(survey_df3b[colnames_values])
View(is.na(survey_df3b)[colnames_values])
#already factors#
# save.image("~/OnBeingAnalysis3.RData")                                    
survey_df3c<-survey_df3b%>%mutate(across(all_of(colnames_values),~ifelse(is.na(.x),NA,.x)))
# Gives 1 and NAs

colnames_values2<-colnames(survey_df3c)[90:102]
survey_df3c<-survey_df3c%>%mutate_at(vars(colnames_values2),function(x) replace(x,is.na(x),0))
survey_df3c<-survey_df3c%>%rename(Gender=`How do you identify?`)
survey_df3c<-survey_df3c%>%rename(Gender_other=`How do you identify? - Prefer to self-describe:`)
survey_df3c<-survey_df3c%>%rename(Ethnicity=`How would you describe yourself? `)
survey_df3c<-survey_df3c%>%rename(Age=`How old are you?`)


# should have Use `all_of(colnames_values2)` instead of `colnames_values2` 

survey_df3c_values<-survey_df3c%>%group_by(collector_id)%>%tally()%>%mutate(proportion_per_collector_values= n / sum(n),n_total = sum(n))
colnames(collectors)
colnames(survey_df3c[1:20])
# Mering collectors with full survey
collectors_select<-collectors%>%select(name,id,href,type)
survey_df3c_values_collector_sum<-survey_df3c_values%>%group_by(collector_id)%>%tally()
#only 12 instead of 21 in collectors table
survey_df3d<-survey_df3c%>%mutate(collector_id=as.factor(collector_id))
collectors_select<-collectors_select%>%mutate(id=as.factor(id))
survey_df3c_collectors_merge<-left_join(survey_df3d,collectors_select,by=c("collector_id"="id"))
survey_df3c_collectors_merge<-survey_df3c_collectors_merge%>%rename(collector_name=name)
survey_df3c_values_collector_sum2<-survey_df3c_collectors_merge%>%group_by(collector_id)%>%tally()

sum_values<-survey_df3c_collectors_merge%>%group_by(collector_name)%>%tally()%>%mutate(proportion_per_collector_values= n / sum(n),n_total = sum(n))
library(forcats)
sum_values_ordered<-sum_values%>%mutate(as.factor(collector_name))
sum_values_ordered<-sum_values_ordered%>%mutate(collector_name=fct_reorder(collector_name,proportion_per_collector_values,.fun='length'))
#below used to reorder before plot#
sum_values_ordered<-sum_values_ordered%>%arrange(desc(proportion_per_collector_values))
sum_values_ordered2<-transform(sum_values_ordered,collector_name=reorder(collector_name,order(proportion_per_collector_values,decreasing=TRUE)))
ggplot(sum_values_ordered2,aes(x=collector_name,proportion_per_collector_values,stat="identity"))+geom_col(aes(fill=collector_name))+ggtitle("Overall Value summary by Collector")+geom_text(aes(label=n))+ylab("Proportion (%)")+geom_text(aes(label=round(proportion_per_collector_values,3),position="dodge",vjust=2.55))

wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

sum_values_ordered2_wrap<-wrap.labels(sum_values_ordered2$collector_name,1)
ggplot(sum_values_ordered2_wrap,aes(x=collector_name,proportion_per_collector_values,stat="identity"))+geom_col(aes(fill=collector_name))+ggtitle("Overall Value summary by Collector")+geom_text(aes(label=n))+ylab("Proportion (%)")+geom_text(aes(label=round(proportion_per_collector_values,3),position="dodge",vjust=2.55))
library(stringr)
sum_values_ordered2$newcollectorname = str_wrap(sum_values_ordered2$collector_name, width = 15)

sum_values_ordered2<-transform(sum_values_ordered2,newcollectorname=reorder(newcollectorname,order(proportion_per_collector_values,decreasing=TRUE)))
#used#
ggplot(sum_values_ordered2,aes(x=newcollectorname,proportion_per_collector_values,stat="identity"))+geom_col(aes(fill=collector_name))+ggtitle("Overall Value summary by Collector")+xlab("")+geom_text(aes(label=n))+ylab("Proportion (%)")+geom_text(aes(label=round(proportion_per_collector_values,3),position="dodge",vjust=2.55))+theme(axis.text=element_text(size=8),axis.title=element_text(size=14,face="bold"))


