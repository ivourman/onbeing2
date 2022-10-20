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


df3_LF2<-df3_LF%>%group_by(Age, Listening_Frequency) %>%tally() %>%mutate(percent=n/sum(n))

# Pipe summarized data into ggplot
df3_LF2 %>% 
  ggplot(aes(x=Listening_Frequency, y=percent, fill=Age)) +
  geom_bar(stat="identity", width=1, color="black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")), 
            position=position_stack(vjust=0.5), colour="white") +
  scale_y_continuous(labels=percent_format()) +
  labs(x="Listening Frequency", y="Percent of Age Group with Listening Frequency") +
  ggtitle("Listening Frequency", "by Age Group")


#reordered within GGplot#
Age_gender<-df3%>%group_by(Age,Gender)%>%mutate(n=n())%>%distinct(n)
Age_gender %>%filter(Gender!="Prefer not to say"&Gender!="NA")%>%ggplot(.,aes(x=reorder(Age,desc(n)),y=(n)))+geom_col(aes(fill=Age))+facet_wrap(~reorder(Gender,desc(n)))+geom_text(aes(label=round(n, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+scale_y_continuous(labels=comma,'Total Count Per Within Gender ID')+ggtitle('Distribution of Age by Gender Groups')+scale_x_discrete("Count by Age Group")

#grouping in facet_wrap using fct_rev for all grids#
Age_gender %>%filter(Gender!="Prefer not to say"&Gender!="NA")%>%ggplot(.,aes(fct_rev(fct_reorder(Age,n)),y=n))+geom_col(aes(fill=Age))+facet_wrap(~Gender)+geom_text(aes(label=round(n, digits = 2)),position = position_dodge(width=0.9),vjust=-0.5)+scale_y_continuous(labels=comma,'Total Count Per Within Gender ID')+ggtitle('Distribution of Age by Gender Groups')+scale_x_discrete("Count by Age Group")
