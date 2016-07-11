
library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm

# setwd("C:\\College\\Third Quater\\BigData\\Project")
train <- read.csv("C:/College/Third Quater/Big Data/Project/AnimalShelter.csv", stringsAsFactors = FALSE)
names(train)[1] <- 'ID'

train[is.na(train)] <- -1
train[is.null(train)] <- -1

# Extract time variables from date
tempcolDateTime <- c(train$DateTime)
df <- data.frame(tempcolDateTime, stringsAsFactors = FALSE)
df2 <- within(df,{
  posb <- as.POSIXlt(tempcolDateTime,format="%m/%d/%Y %H:%M:%S")
  weekday <- wday(posb, label = TRUE)
  secs <- posb$sec
  mins <- posb$min
  hours <- posb$hour
  year <- format(posb, "%Y")
  month <- format(posb, "%m")
  day <- format(posb, "%d")
  posb <- NULL  # cleanup
})

train <- cbind(train,df2)

# Reshape
outcomes <- train[1:26729, ] %>%
  group_by(AnimalType, OutcomeType) %>%
  summarise(num_animals = n())

factor(train$AgeuponOutcome)[1:10]

# Get the time value:
train$TimeValue <- sapply(train$AgeuponOutcome,function(x) strsplit(x, split = ' ')[[1]][1])

# Now get the unit of time:
train$UnitofTime <- sapply(train$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])

# Fortunately any "s" marks the plural, so we can just pull them all out
train$UnitofTime <- gsub('s', '', train$UnitofTime)

train$TimeValue  <- as.numeric(train$TimeValue)
train$UnitofTime <- as.factor(train$UnitofTime)

# Make a multiplier vector
multiplier <- ifelse(train$UnitofTime == 'day', 1,
                     ifelse(train$UnitofTime == 'week', 7,
                            ifelse(train$UnitofTime == 'month', 30, # Close enough
                                   ifelse(train$UnitofTime == 'year', 365, NA))))
# Apply our multiplier
train$AgeinDays <- train$TimeValue * multiplier

summary(train$AgeinDays)

# Replace blank names with "Nameless"
train$Name <- ifelse(nchar(train$Name)==0, 'Nameless', train$Name)

# Make a name v. no name variable
train$HasName[train$Name == 'Nameless'] <- 0
train$HasName[train$Name != 'Nameless'] <- 1

# Replace blank sex with most common
train$SexuponOutcome <- ifelse(nchar(train$SexuponOutcome)==0,'Spayed Female', train$SexuponOutcome)

# Time of day may also be useful
train$TimeofDay <- ifelse(train$hours > 5 & train$hours < 11, 'morning',
                          ifelse(train$hours > 10 & train$hours < 16, 'midday',
                                 ifelse(train$hours > 15 & train$hours < 20, 'lateday', 'night')))

# Put factor levels into the order we want
train$TimeofDay <- factor(train$TimeofDay, levels = c('morning', 'midday',
                                                      'lateday', 'night'))

# Reshape
daytimes <- train[1:26729, ] %>%
  group_by(AnimalType, TimeofDay, OutcomeType) %>%
  summarise(num_animals = n())


# Take a look as some of the levels
levels(factor(train$Breed))[1:10]

# Use "grepl" to look for "Mix"
train$IsMix <- ifelse(grepl('Mix', train$Breed), 1, 0)

# Split on "/" and remove " Mix" to simplify Breed
train$SimpleBreed <- sapply(train$Breed, function(x) gsub(' Mix', '', 
                                                          strsplit(x, split = '/')[[1]][1]))

# Use strsplit to grab the first color
train$SimpleColor <- sapply(train$Color, 
                            function(x) strsplit(x, split = '/| ')[[1]][1])

levels(factor(train$SimpleColor))

# Use "grepl" to look for "Intact"
train$Intact <- ifelse(grepl('Intact', train$SexuponOutcome), 1,
                       ifelse(grepl('Unknown', train$SexuponOutcome), 'Unknown', 0))

# Use "grepl" to look for sex
train$Sex <- ifelse(grepl('Male', train$SexuponOutcome), 'Male',
                    ifelse(grepl('Unknown', train$Sex), 'Unknown', 'Female'))
# Reshape
intact <- train[1:26729, ] %>%
  group_by(AnimalType, Intact, OutcomeType) %>%
  summarise(num_animals = n())


# Use rpart to predict the missing age values
age_fit <- rpart(AgeinDays ~ AnimalType + Sex + Intact + SimpleBreed + HasName, 
                 data = train[!is.na(train$AgeinDays), ], 
                 method = 'anova')

# Impute predicted age values where missing using "predict"
train$AgeinDays[is.na(train$AgeinDays)] <- predict(age_fit, train[is.na(train$AgeinDays), ])

# All gone? Yes.
sum(is.na(train$AgeinDays))

# Use the age variable to make a puppy/kitten variable
train$Lifestage[train$AgeinDays < 365] <- 'baby'
train$Lifestage[train$AgeinDays >= 365] <- 'adult'

train$Lifestage <- factor(train$Lifestage)


factorVars <- c('Name','OutcomeType','OutcomeSubtype','AnimalType',
                'SexuponOutcome','AgeuponOutcome','SimpleBreed','SimpleColor',
                'HasName','IsMix','Intact','Sex','TimeofDay','Lifestage')

train[factorVars] <- lapply(train[factorVars], function(x) as.factor(x))


abc<-train
nrow(abc)


# Split up train and test data
training <- train[1:24729, ]
nrow(training)
test  <- train[24730:nrow(train), ]
nrow(test)

##write.csv(test,"C:/College/Third Quater/Big Data/Project/testing.csv",row.names=FALSE)


#KNN
require(cvTools) 
require(class)
require(caret)

#dataset <- read.csv('C:/College/Third Quater/Big Data/Project/AnimalShelter.csv',header = TRUE , sep = ",")

train$OutcomeType <- as.numeric(train$OutcomeType)
train$AnimalType <- as.numeric(train$AnimalType)
train$Sex <- as.numeric(train$Sex) 
train$Intact <- as.numeric(train$Intact) 
train$TimeofDay <- as.numeric(train$TimeofDay)
train$SimpleBreed <- as.numeric(train$SimpleBreed)
train$SimpleColor <- as.numeric(train$SimpleColor)
train$HasName <- as.numeric(train$HasName)
train$AgeinDays <- as.numeric(train$AgeinDays)


k <- 10 #the number of folds
folds <- cvFolds(NROW(train), K=k)
train$holdoutpred <- rep(0,nrow(train))

for(i in 1:k)
{ 
  training <- train[folds$subsets[folds$which != i], ] #Set the training set
  test <- train[folds$subsets[folds$which == i], ] #Set the test set
  cl<-training$OutcomeType
  newpred <- knn(training, test,k=10, cl, prob=TRUE)
  train[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use
   
}


mean(ifelse(as.numeric(train$OutcomeType)==train$holdoutpred,1,0))

#Prediction

require(cvTools) 
require(class)
require(caret)

#dataset <- read.csv('train_data.csv',header = TRUE , sep = ",")

testing <- read.csv("C:/College/Third Quater/Big Data/Project/testing.csv",stringsAsFactors = FALSE)

c <- testing
nrow(c)
testing[is.na(testing)] <- -1
testing[is.null(testing)] <- -1

train$OutcomeType <- as.factor(train$OutcomeType) #Car
train$AnimalType <- as.numeric(train$AnimalType) #Car
train$Sex <- as.numeric(train$Sex) #Car
train$Intact <- as.numeric(train$Intact) #Car
train$TimeofDay <- as.numeric(train$TimeofDay) #Car
train$SimpleBreed <- as.numeric(train$SimpleBreed) #Car
train$SimpleColor <- as.numeric(train$SimpleColor) #Car
train$HasName <- as.numeric(train$HasName) #Abalone
train$AgeinDays <- as.numeric(train$AgeinDays)


testing$AnimalType <- as.numeric(testing$AnimalType) #Car
testing$Sex <- as.numeric(testing$Sex) #Car
testing$Intact <- as.numeric(testing$Intact) #Car
testing$TimeofDay <- as.numeric(testing$TimeofDay) #Car
testing$SimpleBreed <- as.numeric(testing$SimpleBreed) #Car
testing$SimpleColor <- as.numeric(testing$SimpleColor) #Car
testing$HasName <- as.numeric(testing$HasName) #Abalone
testing$AgeinDays <- as.numeric(testing$AgeinDays)


training <- train #Set the training set
test <- testing #Set the validation set

cl<-training$OutcomeType
newpred <- knn(training[,1:8], test,k=10, cl, prob=TRUE)
require(xlsx)
write.xlsx(newpred,"C:/College/Third Quater/Big Data/Project/predictions.xlsx")
write.xlsx(attributes(newpred)$prob,"C:/College/Third Quater/Big Data/Project/proportions.xlsx")







