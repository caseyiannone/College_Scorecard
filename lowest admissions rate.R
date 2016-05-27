

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
library(sqldf)


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


## Create Selectivity variable ADMSSN / APPLCN

Full_DS <- mutate(Full_DS, SELECT = ADMSSN/APPLCN)

Full_DS$SELECT <- Full_DS$SELECT * 100.0


Full_DS$SELECT <- round(Full_DS$SELECT, 1)

Full_DS20 <- Full_DS$SELECT * 100.0

Full_DS <- sqldf('select * from Full_DS order by "SELECT" asc')
Full_DS <- Full_DS[-c(1:16),] #Drops institutions with no select measure
#may want to split this out into top institutions before running rank 
#may also want to drop Southside Regional Medical Center Professional Schools
Full_DS <- cbind(Rank=1:nrow(Full_DS), Full_DS)
Full_DS$INSTNM <- paste(Full_DS$Rank, Full_DS$INSTNM, sep=". ")
Full_DS$INSTNM <- factor(Full_DS$INSTNM, levels=rev(Full_DS$INSTNM))
Full_DS20 <- slice(Full_DS,1:20)
Full_DS20 <- slice(Full_DS,17:36)

ggplot(Full_DS20, aes(x=INSTNM, y=SELECT)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=INSTNM, y=SELECT-0.55, ymax=SELECT, hjust=0.95, label=paste0(SELECT, "%")), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("Lowest Admission Rates")


