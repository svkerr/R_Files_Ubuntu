# Set local working directory
setwd("/home/skerr/DataSets/sanfran")

# Load required libraries
library(zoo)
library(ggplot2)
library(psych)
library(lsr)
library(car)
library(plyr)
library(xts)
library(reshape2)

data <- read.table("sfpd_incident_2003.csv", header = T, sep=',')
str(data)
table(data$DayOfWeek)
table(data$PdDistrict)

# Let's look at how to analyze the categories
cat_tab <- table(data$Category)
cat_tab <- melt(cat_tab)
cat_df <- data.frame(cat_tab)
cat_df[with(cat_df, order(-value)), ]
# or using plyr
arrange(cat_df,desc(value))    # using arrange function from plyr library

# Let's analyze the DRUG/NARCOTIC Offenses
dn <- data[which(data$Category == "DRUG/NARCOTIC"),]  # which() returns index for those particular offenses, then we extract that subset
dn_t <- table(dn$PdDistrict)
dn_df <- data.frame(melt(dn_t))
arrange(dn_df,desc(value))       # Rank by number of offenses and which district has most narcotic offenses

# Let's analyze Burglary
bur <- data[which(data$Category == "BURGLARY"),] 
bur_t <- table(bur$PdDistrict)
bur_df <- data.frame(melt(bur_t))
arrange(bur_df,desc(value))
## The results of above NARCOTIC and BURGLARY analysis shows that they don't seem to be related.

table(bur$DayOfWeek)
melt(table(bur$DayOfWeek))

