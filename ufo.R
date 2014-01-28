# Set local working directory
setwd("/home/skerr/DataSets/")

# Load libraries
library(ggplot2)

# Read in UFO data file
# Note: All of columns are strings, default setting in all read.* R functions is to convert
#       strings to 'factor' types - meant for categorical variables. Good practice to switch this off
ufo <- read.delim('ufo_awesome.tsv', sep='\t', stringsAsFactors=FALSE, header=FALSE, na.strings='')

# Assign column names
names(ufo) <- c('DateOccurred', 'DateReported', 'Location', 'ShortDescription', 'Duration', 'LongDescription')

# Convert first two columns to R data objects
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format='%Y%m%d')

# Since we get a failure here, need to find offending observations
# Let's look at a sample of DateOccurred or DateReported columns that fail to meet 8 character count
head(ufo[which(nchar(ufo$DateOccurred) != 8 | nchar(ufo$DateReported)!= 8),1])

good.rows <- ifelse(nchar(ufo$DateOccurred) !=8 | nchar(ufo$DateReported) != 8, FALSE,TRUE)
length(which(good.rows))  # number of good observations
length(which(!good.rows))  # number of bad observations
length(which(!good.rows))/length(good.rows)*100     # Percent of bad observations 

# Since number of bad observations is trivial, let's delete them
ufo <- ufo[which(good.rows),]
str(ufo)

# Now, let's continue with Date conversions
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format='%Y%m%d')
ufo$DateReported <- as.Date(ufo$DateReported, format='%Y%m%d')
str(ufo)

# Now, let's clean-up location data: splitting city and state into separate columns
# Create a function to separate city/state and then lapply to every observation 
get.location <- function(loc) {
  split.location <- tryCatch(
    strsplit(loc,',')[[1]],
    error = function(e) return(c(NA,NA))
    )
  clean.location <- gsub('^ ','',split.location)   # get rid of leading spaces
  if (length(clean.location)>2) {
    return(c(NA,NA))
  }
  else {
    return(clean.location)
  }
}

city.state <- lapply(ufo$Location, get.location)
head(city.state)

# Now convert city.state list to a matrix that can be added to the ufo dataframe
location.matrix <- do.call(rbind, city.state)

# At this point, in order to add separate city and state columns to ufo dataframe, i could
# use cbind and then add column names via names() but apparently the transform() function does trick in one step
ufo <- transform(ufo, USCity=location.matrix[,1], USState=tolower(location.matrix[,2]), stringsAsFactors=FALSE)

# Not all observations are from the united states. So we'll need to filter out non-US observations
us.states <- c('ak','al','ar','az','ca','co','ct','de','fl','ga','hi','ia','id','il','in','ks','ky','la','ma','md','me','mi','mn','mo','ms','mt','nc','nd','ne','nh','nj','nm','nv','ny','oh','ok','or','pa','ri','sc','sd','tn','tx', 'ut','va','vt','wa','wi','wv','wy')

