library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sqldf)
#Mac
setwd("~/Google Drive/GitHub/College_Scorecard")


#PC Working directory
setwd("C:/Users/ciannone/Google Drive/Graduate_Coursework/Florida State University/Practicum/Analysis_Plots")

FD <- read.csv("Final_Data.csv",header=TRUE,sep=",")
Earnings <- read.csv("Book1.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)


#Combine IPEDS & Scorecard data
Full_DS <- inner_join(FD, Earnings, by="UNITID")
#Full_DS <- data.table(Full_DS)
names(Full_DS)

#change all nulls to NA, as there is a combination of Nulls and NAs used in the files
Full_DS[Full_DS == "NULL"] <- NA
Full_DS[Full_DS == "PrivacySuppressed"] <- NA


setnames(Full_DS,"md_earn_wne_p10", "e50")
setnames(Full_DS,"pct10_earn_wne_p10", "e10")
setnames(Full_DS,"pct25_earn_wne_p10", "e25")
setnames(Full_DS,"pct75_earn_wne_p10" , "e75")
setnames(Full_DS, "pct90_earn_wne_p10" , "e90")

###############Make Coloumns Numeric##################################
Full_DS[, "e50"]  <- as.numeric(Full_DS[, "e50"]) 
Full_DS[, "e10"]  <- as.numeric(Full_DS[, "e10"]) 
Full_DS[, "e25"]  <- as.numeric(Full_DS[, "e25"]) 
Full_DS[, "e75"]  <- as.numeric(Full_DS[, "e75"]) 
Full_DS[, "e90"]  <- as.numeric(Full_DS[, "e90"]) 


Full_DS <- sqldf('select * from Full_DS order by "e75" desc')

Full_DS <- cbind(Rank=1:nrow(Full_DS), Full_DS)
Full_DS$INSTNM <- paste(Full_DS$Rank, Full_DS$INSTNM, sep=". ")
Full_DS$INSTNM <- factor(Full_DS$INSTNM, levels=rev(Full_DS$INSTNM))

labels <-c("1" ="Public", "2"="Private Not-For-Profit","3" ="Private For-Profit")

##Box and whisker plot for Top Quartile Earnings 10 Years After Matriculation
ggplot(Full_DS[1:20,], aes(x=INSTNM, ymin=e10, lower=e25, middle=e50, upper=e75, ymax=e90)) +
  geom_boxplot(stat="identity", aes(fill=factor(CONTROL))) + 
  geom_text(aes(x=INSTNM, y=e75-2000, ymax=e75, hjust=0.95, label=paste0("$", e75)), size=2) + 
  theme_light(base_size=10) +
  theme(axis.text.y = element_text(hjust=0, color="black")) +
  coord_flip() +
  xlab("") + ylab("") +
  ggtitle("Top Quartile Earnings 10 Years After Matriculation ($)")+
  scale_fill_discrete(name="Title",labels=labels)

##Histogram/density plot Top Quartile Earnings 10 Years After Matriculation
ggplot(Full_DS, aes(e50, color=factor(CONTROL), fill=factor(CONTROL), group=factor(CONTROL))) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("Median Earnings 10 Years after Matriculation") + ylab("")+
  scale_fill_discrete(name="Title",labels=labels)










