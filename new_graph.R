library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(sqldf)
library(ggvis)
library(ggplot2)
library(googleVis)
library(ggthemes)
library(googlesheets)
library(portfolio)
library(purrr)       
library(tidyr)       
library(lubridate)   
library(scales)      
library(gridExtra)   
library(viridis)     
library(knitr)  
library(ggmap)
library(reshape2)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)
library(RColorBrewer)


#PC Working directory
setwd("C:/Users/ciannone/Google Drive/Graduate_Coursework/Florida State University/Practicum/Analysis_Plots")

FD <- read.csv("Final_Data.csv",header=TRUE,sep=",")
Earnings <- read.csv("Book1.csv",header=TRUE,sep=",")


#Combine IPEDS & Scorecard data
Full_DS <- inner_join(FD, Earnings, by="UNITID")
Full_DS <- data.table(Full_DS)
names(Full_DS)

#change all nulls to NA, as there is a combination of Nulls and NAs used in the files
Full_DS[Full_DS == "NULL"] <- NA
Full_DS[Full_DS == "PrivacySuppressed"] <- NA


ggplot(FD, aes(x=md_earn_wne_p10, color=factor(CONTROL), fill=factor(CONTROL), group=factor(CONTROL))) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("Median Earnings 10 Years after Matriculation") + ylab("")




setnames(Full_DS,"md_earn_wne_p10", "e50")
setnames(Full_DS,"pct10_earn_wne_p10", "e10")
setnames(Full_DS,"pct25_earn_wne_p10", "e25")
setnames(Full_DS,"pct75_earn_wne_p10" , "e75")
setnames(Full_DS, "pct90_earn_wne_p10" , "e90")


ORDER BY s11.pct75_earn_wne_p10 DESC")
earnings <- cbind(Rank=1:nrow(earnings), earnings)
earnings$College <- paste(earnings$Rank, earnings$College, sep=". ")
earnings$College <- factor(earnings$College, levels=rev(earnings$College))




ggplot(Full_DS[1:20,], aes(x=INSTNM, ymin=e10, lower=e25, middle=e50, upper=e75, ymax=e90)) +
  geom_boxplot(stat="identity", fill="#53cfff") + 
  geom_text(aes(x=INSTNM, y=e75-2000, ymax=e75, hjust=0.95, label=paste0("$", e75)), size=4) + 
  #theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black")) +
  coord_flip() +
  xlab("") + ylab("") +
  ggtitle("Top Quartile Earnings 10 Years After Matriculation ($)")

