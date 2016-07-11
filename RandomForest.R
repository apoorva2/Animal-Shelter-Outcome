library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm

setwd("C:\\Users\\apoorva\\Desktop\\SPRINGQUARTER\\BigData\\Project")
train <- read.csv("AnimalShelter.csv", stringsAsFactors = FALSE)
names(train)[1] <- 'ID'

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

# Plot
ggplot(outcomes, aes(x = AnimalType, y = num_animals, fill = OutcomeType)) +
geom_bar(stat = 'identity', position = 'fill', colour = 'Red') +
coord_flip() +
labs(y = 'Proportion of Animals', x = 'Animal', title = 'Outcomes: Cats & Dogs') +
theme_few()

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

# Plot
ggplot(daytimes, aes(x = TimeofDay, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'Red') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes by Time of Day: Cats & Dogs') +
  theme_few()

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

# Plot
ggplot(intact, aes(x = Intact, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'Red') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes by Intactness: Cats & Dogs') +
  theme_few()

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

# Plot in ggplot2
ggplot(train[1:26729, ], aes(x = Lifestage, fill = OutcomeType)) + 
  geom_bar(position = 'fill', colour = 'black') +
  labs(y = 'Proportion', title = 'Animal Outcome: Babies versus Adults') +
  theme_few()

factorVars <- c('Name','OutcomeType','OutcomeSubtype','AnimalType',
                'SexuponOutcome','AgeuponOutcome','SimpleBreed','SimpleColor',
                'HasName','IsMix','Intact','Sex','TimeofDay','Lifestage')

train[factorVars] <- lapply(train[factorVars], function(x) as.factor(x))


# Split up train and test data
training <- train[1:24729, ]
nrow(training)
test  <- train[24730:nrow(train), ]
nrow(test)
# Set a random seed
set.seed(731)


# Build the model
rf_mod <- randomForest(OutcomeType ~ AnimalType+AgeinDays+Intact+HasName+hours+weekday+TimeofDay+SimpleColor+IsMix+Sex+month, 
                       data = train, 
                       ntree = 600, 
                       importance = TRUE)

# Show model error
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)

# Get importance
importance    <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'lavender',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_mod, test, type = 'vote')

# Save the solution to a dataframe
solution <- data.frame('ID' = test$ID, prediction)

# Write it to file
write.csv(solution, 'solution.csv', row.names = F)
