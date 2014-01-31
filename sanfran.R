# Set local working directory
setwd("/home/skerr/DataSets/sanfran")

# Load libraries
library(ggplot2)
library(psych)
library(lsr)
library(car)
library(plyr)
library(xts)
library(reshape2)
library(stringr)

# Read in the Data Sets
data03 <- read.table("sfpd_incident_2003.csv", header = T, sep=',')
data04 <- read.table("sfpd_incident_2004.csv", header = T, sep=',')
data05 <- read.table("sfpd_incident_2005.csv", header = T, sep=',')
data06 <- read.table("sfpd_incident_2006.csv", header = T, sep=',')
data07 <- read.table("sfpd_incident_2007.csv", header = T, sep=',')
data08 <- read.table("sfpd_incident_2008.csv", header = T, sep=',')
data09 <- read.table("sfpd_incident_2009.csv", header = T, sep=',')
data10 <- read.table("sfpd_incident_2010.csv", header = T, sep=',')
data11 <- read.table("sfpd_incident_2011.csv", header = T, sep=',')
data12 <- read.table("sfpd_incident_2012.csv", header = T, sep=',')
data13 <- read.table("sfpd_incident_2013.csv", header = T, sep=',')

str(data12)
dow12 <- data.frame(melt(table(data12$DayOfWeek)))
arrange(dow12,desc(value))
colnames(dow12) <- c("Day", "Crimes12")

dow11 <- data.frame(melt(table(data11$DayOfWeek)))
arrange(dow11,desc(value))
colnames(dow11) <- c("Day", "Crimes11")

dow10 <- data.frame(melt(table(data10$DayOfWeek)))
arrange(dow10,desc(value))
colnames(dow10) <- c("Day", "Crimes10")

dow09 <- data.frame(melt(table(data09$DayOfWeek)))
arrange(dow09,desc(value))
colnames(dow09) <- c("Day", "Crimes09")

dow08 <- data.frame(melt(table(data08$DayOfWeek)))
arrange(dow08,desc(value))
colnames(dow08) <- c("Day", "Crimes08")

dow07 <- data.frame(melt(table(data07$DayOfWeek)))
arrange(dow07,desc(value))
colnames(dow07) <- c("Day", "Crimes07")

dow06 <- data.frame(melt(table(data06$DayOfWeek)))
arrange(dow06,desc(value))
colnames(dow06) <- c("Day", "Crimes06")

dow05 <- data.frame(melt(table(data05$DayOfWeek)))
arrange(dow05,desc(value))
colnames(dow05) <- c("Day", "Crimes05")

dow04 <- data.frame(melt(table(data04$DayOfWeek)))
arrange(dow04,desc(value))
colnames(dow04) <- c("Day", "Crimes04")

dow03 <- data.frame(melt(table(data03$DayOfWeek)))
arrange(dow03,desc(value))
colnames(dow03) <- c("Day", "Crimes03")


dfs <- list(dow03,dow04,dow05,dow06,dow07,dow08,dow09,dow10,dow11,dow12)  # Use plyr join_all function
dow03_12 <- join_all(dfs)
dow03_12
plot(dow03_12$Crimes03)

# Let's look at how to analyze the categories
cat_tab <- table(data12$Category)
cat_tab <- melt(cat_tab)
cat_df <- data.frame(cat_tab)
cat_df[with(cat_df, order(-value)), ]
# or using plyr
arrange(cat_df,desc(value))    # using arrange function from plyr library

# Let's analyze the DRUG/NARCOTIC Offenses
dn <- data12[which(data12$Category == "DRUG/NARCOTIC"),]  # which() returns row index for those particular offenses, then we extract that subset
dn_t <- table(dn$PdDistrict)
dn_df <- data.frame(melt(dn_t))
arrange(dn_df,desc(value))       # Rank by number of offenses and which district has most narcotic offenses

# Now let's find the number of times DRUG/NARCOTIC offenses included COCAINE (uses stringr library)
cokePos <- str_detect(string = dn$Descript, pattern = "COCAINE")   # returns logical boolean vector
numCokeOffenses <- nrow(dn[cokePos,])
percentCokeOffenses <- numCokeOffenses/nrow(dn) * 100
percentCokeOffenses

methPos <- str_detect(string = dn$Descript, pattern = "METH")   # returns logical boolean vector
numMethOffenses <- nrow(dn[methPos,])
percentMethOffenses <- numMethOffenses/nrow(dn) * 100

# Let's analyze Burglary
bur <- data12[which(data12$Category == "LARCENY/THEFT"),] 
bur_t <- table(bur$PdDistrict)
bur_df <- data.frame(melt(bur_t))
arrange(bur_df,desc(value))
## The results of above NARCOTIC and BURGLARY analysis shows that they don't seem to be related.

table(bur$DayOfWeek)
melt(table(bur$DayOfWeek))

apply(c(data03,data04,FUN = nrow))
