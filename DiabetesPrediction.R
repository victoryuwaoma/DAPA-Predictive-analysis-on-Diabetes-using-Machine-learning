"Pima Indians Diabetes prediction using Random Forest"

                              # RANDOM FOREST

                          # IMPORTING PACKAGES & LIBRARIES
install.packages("caret")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("DataExplorer")
install.packages("dplyr")

library(caret)
library(corrplot)
library (ggplot2)
library(DataExplorer)
library (dplyr)


                          # IMPORTING THE DATASET
setwd("/Users/Victory/Desktop/Datasets/DAPA")
diabetesData <- read.csv("pima-indians-diabetes.csv", header=0)

head(diabetesData)
View(diabetesData)

#Get the column names
colnames(diabetesData)

#Setting column names for the data
colnames(diabetesData)[colnames(diabetesData)=="V1"] <- "Pregnancies"
colnames(diabetesData)[colnames(diabetesData)=="V2"] <- "Glucose"
colnames(diabetesData)[colnames(diabetesData)=="V3"] <- "BloodPressure"
colnames(diabetesData)[colnames(diabetesData)=="V4"] <- "SkinThickness"
colnames(diabetesData)[colnames(diabetesData)=="V5"] <- "Insulin"
colnames(diabetesData)[colnames(diabetesData)=="V6"] <- "BMI"
colnames(diabetesData)[colnames(diabetesData)=="V7"] <- "DiabetesPedigreeFunc"
colnames(diabetesData)[colnames(diabetesData)=="V8"] <- "Age"
colnames(diabetesData)[colnames(diabetesData)=="V9"] <- "Outcome"

View(diabetesData)

                          # EXPLORATORY DATA ANALYSIS 
dim(diabetesData) #checking the data dimensions
str(diabetesData) #shows the structure of the dataset

#From the data description, all NA values were encoded as Zero (0)
#Replacing Zero with NA of each variable
diabetesData[, 2:8][diabetesData[, 2:8] == 0] <- NA 

#Checking for any NA value 
sapply(diabetesData,function(x) sum(is.na(x)))

#Replace NA with the Mean of each variable
set.seed(50)
diabetesData$Glucose <- ifelse(is.na(diabetesData$Glucose), mean(diabetesData$Glucose, na.rm=TRUE), diabetesData$Glucose)
diabetesData$BloodPressure <- ifelse(is.na(diabetesData$BloodPressure), mean(diabetesData$BloodPressure, na.rm=TRUE), diabetesData$BloodPressure)
diabetesData$SkinThickness <- ifelse(is.na(diabetesData$SkinThickness), mean(diabetesData$SkinThickness, na.rm=TRUE), diabetesData$SkinThickness)
diabetesData$Insulin <- ifelse(is.na(diabetesData$Insulin), mean(diabetesData$Insulin, na.rm=TRUE), diabetesData$Insulin)
diabetesData$BMI <- ifelse(is.na(diabetesData$BMI), mean(diabetesData$BMI, na.rm=TRUE), diabetesData$BMI)

sapply(diabetesData,function(x) sum(is.na(x))) #Making sure the NA values were handled
str(diabetesData)


#Changing the data structure to an appropriate structure
diabetesData[, 1:5] <- sapply(diabetesData[, 1:5], as.integer)
str(diabetesData)

#Changing the target variable to factor
diabetesData$Outcome <- as.factor(diabetesData$Outcome)
View(diabetesData)

#write.table(diabetesData, file = "/Users/Victory/Desktop/Pima-diabetes.csv", sep = ",", row.names = F)


                          # VISUALIZING THE DATASET
#Age
# Histogram showing the age of patients
ggplot(diabetesData, aes(x=Age))+ ggtitle('The age range of patients') +
  geom_bar(fill="forest green")

#Blood pressure
# Violin boxplot showing the Blood pressure
ggplot(diabetesData, aes(x=Outcome, y=BloodPressure, color=Outcome)) +
  geom_violin(trim=FALSE) + geom_boxplot(width=0.07) +
  scale_color_manual(values=c("#2E8B57", "#FF6200")) + labs(title="Results of Blood Pressure against Outcome")

#BMI
# Violin boxplot showing the BMI
ggplot(diabetesData, aes(x=Outcome, y=BMI, color=Outcome)) +
  geom_violin(trim=FALSE) + geom_boxplot(width=0.07) +
  scale_color_manual(values=c("#2E8B57", "#FF6200")) + labs(title="Results of Body Mass Index (BMI) against Outcome")

#Skin Thickness
# Violin boxplot showing the Skin thickness
ggplot(diabetesData, aes(x=Outcome, y=SkinThickness, color=Outcome)) +
  geom_violin(trim=FALSE) + geom_boxplot(width=0.07) +
  scale_color_manual(values=c("#2E8B57", "#FF6200")) + labs(title="Violin boxplot showing patient Skin thickness")

#Pregnancies
# Violin boxplot showing the Pregnancies
ggplot(diabetesData, aes(x=Outcome, y=Pregnancies, color=Outcome)) +
  geom_violin(trim=FALSE) + geom_boxplot(width=0.07) +
  scale_color_manual(values=c("#2E8B57", "#FF6200")) + labs(title="Violin boxplot showing pregnancies")

#Insulin
# Boxplot showing the Insulin
ggplot(diabetesData, aes(x=Outcome, y=Insulin, notch = TRUE, fill=Outcome)) +
  geom_boxplot() + labs(title="Boxplot showing the Insulin present in patients") +
  scale_fill_manual(values=c("#2E8B57", "#FF6200"))

#Glucose
# Boxplot showing the Glucose
ggplot(diabetesData, aes(x=Outcome, y=Glucose, notch = TRUE, fill=Outcome)) +
  geom_boxplot() + labs(title="Results of Glucose against Outcome") +
  scale_fill_manual(values=c("#2E8B57", "#FF6200"))

#Pedigree
# Boxplot showing the Diabetes Pedigree
ggplot(diabetesData, aes(x=Outcome, y=DiabetesPedigreeFunc, notch = TRUE, fill=Outcome)) +
  geom_boxplot() + labs(title="Boxplot showing the Pedigree present in patients") +
  scale_fill_manual(values=c("#2E8B57", "#FF6200"))

#Outcome
# Barplot showing the Patient result
ggplot(diabetesData, aes(x=Outcome, fill=Outcome )) + labs(title="Results of patient") +
  geom_bar( ) + scale_fill_manual(values=c("#2E8B57", "#FF6200"))

#pairs(diabetesData, panel = panel.smooth)

# Heat map
plot_correlation(na.omit(diabetesData), maxcat = 8L)

#corrplot(cor(diabetesData[, -9]), type = "lower", method = "number")


# Univariate analysis of the variables
#Pregnancies
ggplot(diabetesData, aes(x=Pregnancies)) +
  geom_histogram(color="black", fill= "#56B4E9", binwidth = 1) +
  ggtitle("Number of Pregnancies")

#Glucose
ggplot(diabetesData, aes(x=Glucose)) +
  geom_histogram(color="black", fill= "#56B4E9") +
  ggtitle("Plasma glucose concentration")

#Blood Pressure
ggplot(diabetesData, aes(x=BloodPressure)) +
  geom_histogram(color="black", fill= "#56B4E9") +
  ggtitle("Diastolic blood pressure (mm Hg)")

#Skin thickness
ggplot(diabetesData, aes(x=SkinThickness)) +
  geom_histogram(color="black", fill= "#56B4E9") +
  ggtitle("Triceps skin fold thickness (mm)")

#Insulin
ggplot(diabetesData, aes(x=Insulin)) +
  geom_histogram(color="black", fill= "#56B4E9") +
  ggtitle("2-Hour Serum Insulin (mu U/ml)")

#BMI
ggplot(diabetesData, aes(x=BMI)) +
  geom_histogram(color="black", fill= "#56B4E9") +
  ggtitle("Body Mass Index ")

#Diabetes Pedigree Function
ggplot(diabetesData, aes(x=DiabetesPedigreeFunc)) +
  geom_histogram(color="black", fill= "#56B4E9") +
  ggtitle("Diabetes Pedigree Function")

#Age
ggplot(diabetesData, aes(x=Age)) +
  geom_histogram(color="black", fill= "#56B4E9") +
  ggtitle("Age(years)")

summary(diabetesData)


                            # RANDOM FOREST MODEL
#install.packages("randomForest")
library(randomForest)

#Splitting the data into Testing and Training
"30% for testing and 70% for training" 

#seed 500
set.seed (500)
newDiabetesData = sort(sample(nrow(diabetesData), nrow(diabetesData)*0.7))

#Training data
train <- diabetesData[newDiabetesData,]

#Testing data
test <- diabetesData[-newDiabetesData,]

dim(train)
dim(test)

#Training The Model
randFrst <- randomForest(Outcome ~., data = train, ntree = 500, mtry = 6, importance = TRUE)
randFrst

#install.packages("party")
library(party)
tree<- ctree(Outcome ~ ., data=train, controls=cforest_control(mtry=6, mincriterion=0))
plot(tree)


#Predicting on train set
predTrain <- predict(randFrst, train, type = "class")
table(predTrain, train$Outcome)  

#Predicting on test dataset
test[1:10,]
randFrstPred <- predict(randFrst, test, type="response")
head(randFrstPred, 50)


#Check accuracy
confusionMatrix(randFrstPred,test$Outcome)


#Checking important variables
importance(randFrst)
varImpPlot(randFrst)
