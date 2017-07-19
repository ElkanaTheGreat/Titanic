suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(VIM))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(mice))

## convert columns

ConvertSurvived <- function(data)
{
  data$Survived <- factor(data$Survived)
  levels(data$Survived) <- c("Died", "Survived")
  return (data)
}

## Extract Features
ExtractTitles <- function(x) {return (gsub('(.*, )|(\\..*)', '', x))}
ExtractFamilySize <- function(x, y) {return (x + y + 1)}
ExtractHasCabin <- function(x) {return (ifelse(x != "", 1, 0))}
ExtractIsChild <- function(x) {return (ifelse(x < 18, 1, 0))}
ExtractIsMother <- function(s, p, a, t) {return (ifelse(s == 'female' & p > 0 & a > 18 & t != 'Miss', 1, 0))}
ExtractFloor <- function(x) {return (str_match(x, "([a-zA-Z]{1}(?=[0-9]{1,3}))|^[a-zA-Z]{1}$")[,1])}
ExtractMainCabin <- function(x) {return (str_match(x, "[0-9]+")[,1])}

CondenseTitle <- function(x)
{
  result <- ifelse(x %in% c('Mlle', 'Ms'), 'Miss', x)
  result <- ifelse(x == 'Mme', 'Mrs', result)
  result <- ifelse(x %in% c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                            'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer'), 'Rare Title', result)

  return (result)
}

CondenseFamilySize <- function(x)
{
  result <- ifelse (x>4, 'large', 'small')
  result <- ifelse (x == 1, 'single', result)

  return (result)
}

TransformFare <- function(x) {return (ifelse(x !=0 & !is.na(x), log10(x), 0))}

ImputeEmbarked <- function(x) {return (ifelse(x == "", "C", x))}

ExtractFeatures <- function(data)
{
  data <- data %>%
    mutate(Title = ExtractTitles(Name),
           FamilySize = ExtractFamilySize(SibSp, Parch)) %>%
    mutate(Title = CondenseTitle(Title)) %>%
    mutate(DiscreteFamilySize = CondenseFamilySize(FamilySize),
           Fare = TransformFare(Fare),
           Embarked = ImputeEmbarked(Embarked),
           HasCabin = ExtractHasCabin(Cabin),
           Floor = ExtractFloor(Cabin),
           MainCabin = ExtractMainCabin(Cabin))
           
  data$Sex <- as.factor(data$Sex)
  data$Embarked <- as.factor(data$Embarked)
  data$Title <- as.factor(data$Title)
  data$DiscreteFamilySize <- as.factor(data$DiscreteFamilySize)
  
  set.seed(1547)
  data_imp <- mice(data[, !names(data) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Survived")], method = "rf")
  data_imp <- complete(data_imp)
  data$Age <- data_imp$Age
  
  data <- data %>%
    mutate(IsChild = ExtractIsChild(Age),
           IsMother = ExtractIsMother(Sex, Parch, Age, Title))
  
  # create dummy Vars from Factors
  data <- cbind(data, data.frame(predict(dummyVars(~ Title + Embarked, data = data), newdata = data)))
  # remove the least important factors
  data$Embarked.Q <- NULL
  data$Title.Rare.Title <- NULL
  data$Title.Master <- NULL

  return (data)
}

prepareData <- function(data)
{
  data <- ExtractFeatures(data)

  ## handle missing data
  # data <- kNN(data, variable = c("Age", "Fare"), k=10)

  
  return (data)
}