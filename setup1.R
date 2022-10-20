# install.packages('tidyverse')
# install.packages('devtools')
# install.packages("readxl")
devtools::install_github("rstudio/addinexamples", type = "source")


library(tidyverse)
library(devtools)
library(readxl)
library(tidyverse)
library(devtools)
library(readxl)
library(doubleheadr)
library(surveymonkey)
library('naniar')
library(ggplot2)
library(rstudioapi)
library(lubridate)
library(ggplot2)
library(scales)
options(scipen = 999)
library(gridExtra)
# install.packages('hexbin')
# install.packages('RColorBrewer')
# install.packages('gganimate')
library(hexbin)
library(RColorBrewer)
library(gganimate)
library(viridis)
library(hrbrthemes)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(ClusterR)
# install.packages("rstudioapi", type = "source")
# library(rstudopaddinexamples)

df4k<-read_excel('/Users/ilanvourman/Documents/OnBeing/OnBeing Survey Drafts/SurveyJune16_Missing_to_use_copy.xlsx',.name_repair = 'universal')
View(df4k)
df4k2<-df4k
x1 <- "Ábcdêãçoàúü"  #or whatever
str_replace_all(df4k2, "[[:punct:]]", " ")

# getActiveDocumentContext()

df4k1<-as_tibble(df4k)%>%gsub("[^0-9A-Za-z///' ]","'" , df4k ,ignore.case = TRUE)
View(df4k1)

# save.image("~/OnBeing/OnBeingAnalysis.RData")

str_df = "Ábcdêãçoàúü"; 
df4k = iconv(str_df, from = '', to = 'ASCII//TRANSLIT');
str_df;
sum(1737+3315)
##
##
  #full responses##
full1<-read.csv(file.choose(),stringsAsFactors = FALSE)
##used survey)closed)all_responses(IV1)##
class(full1)   #527 cols vs 145 for df4k#
View(full1)

##uploading different full version##
fullc1<-read_excel('/Users/ilanvourman/Documents/OnBeing/OnBeing Survey Drafts/Survey_condensed_use_IV1_formatted.xlsx',.name_repair = 'universal')
str(fullc1)
View(fullc1)

pausefull1<-fullc1%>%select(Can.we.add.you.as.a.subscriber.to.The.Pause..the.On.Being.Project.Äôs.weekly.newsletter..so.we.can.stay.in.touch.with.you.,Can.we.add.you.as.a.subscriber.to.The.Pause..the.On.Being.Project.Äôs.weekly.newsletter..so.we.can.stay.in.touch.with.you..If.so..please.provide.your.email.in.the.space.provided..Please.leave.this.field.blank.if.you.re.already.a.subscriber.or.prefer.not.to.receive.the.newsletter.)

ncol(fullc1)
ncol(df4k2)
