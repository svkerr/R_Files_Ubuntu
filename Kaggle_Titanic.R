# Uses Kaggle's Titanic data set
# Predict who survives

# Set working directory
setwd("/home/skerr/DataSets/Kaggle/Titanic")

# Read in datasets
train <- read.csv("train.csv")
test <- read.csv("test.csv")

str(train)
str(test)

### PLAY AREA ###################
table(train$Sex,train$Survived, train$Pclass)
#################################

# Getting Started with Gender Model
# Reform test dataframe to be similar to train dataframe by adding Survival column
test2 <- cbind(test[1],0,test[2:8])  
names(test2)[2] <- "Survived"
head(test2)
# Now, remember previous inference "If you are a woman, you will survive"
# The following line of code

# then we reform test dataframe to be similar to train dataframe by adding “Survived” column and merge it with old dataframe
test <- cbind(test[1],0,test[2:8])

names(test)[2] <- "Survived"

# Now build model using Sex and Pclass predictors
fit <- lm(Survived ~ Sex + Pclass, data=train)
pred <- predict(fit,newdata=test, type="response")
pred <- as.data.frame(pred)

fit2 <- lm(Survived ~ Sex + Pclass + SibSp, data=train)
summary(fit2)  # Check R-Squared
cor(train[2:11])
# Task for self -- get residuals on using it on the Survived

# Now show our prediction table
table(pred)

# We will use 0.609168766092407 as out threshold to partition survived and died people

mypred = 0
mypred[pred < 0.61] <- 0
mypred[pred >= 0.61] <- 1

# Here, we need to make our output. To follow the format of submitted files your output 
# should has only two columns, passenger is and survived state, giving them their names. 
# This is what these two lines of code do

out <- cbind(test[,1], mypred)
colnames(out) <- c("PassengerID", "Survived")

# Now output results

write.csv(out, "out.csv", row.names = F)