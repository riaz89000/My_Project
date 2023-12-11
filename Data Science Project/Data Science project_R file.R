heart_attack_data <- read.csv("D:/Dataset_MIdterm_Sectoin(C).csv",header=TRUE, sep=",")
View(heart_attack_data)

names(heart_attack_data)
str(heart_attack_data)  
dim(heart_attack_data)
summary(heart_attack_data)

colSums(is.na(heart_attack_data))

missing_Age <- is.na(heart_attack_data$Age)
heart_attack_data_1 <- subset(heart_attack_data, !missing_Age)
colSums(is.na(heart_attack_data_1))

average_Age <- round(mean(heart_attack_data_1$Age, na.rm = TRUE))
heart_attack_data_1$Age[is.na(heart_attack_data_1$Age)] <- average_Age
heart_attack_data_1$Age <- as.integer(heart_attack_data_1$Age)
colSums(is.na(heart_attack_data_1))

colSums(heart_attack_data_1 == '')
heart_attack_data_2 <- heart_attack_data_1[!heart_attack_data_1$Sex == "", ]
colSums(heart_attack_data_2 == '')

colSums(heart_attack_data_2 == '')
heart_attack_data_3 <- heart_attack_data_2[!heart_attack_data_2$ExerciseAngina == "", ]
colSums(heart_attack_data_3 == '')

hist(heart_attack_data_3$Oldpeak, main = "Histogram of Oldpeak" ,col=c(7))
boxplot(heart_attack_data_3$Oldpeak, main = " Boxplot of Oldpeak" ,col=c(3))
plot(heart_attack_data_3$Oldpeak, main = "Plot of Oldpeak" ,col=c(6))
barplot(heart_attack_data_3$Oldpeak, main = "Bar of Oldpeak" ,col=c(78))

hist(heart_attack_data_3$Oldpeak)
boxplot(heart_attack_data_3$Oldpeak)
plot(heart_attack_data_3$Oldpeak)


View(heart_attack_data_3)
View(heart_attack_data_2)
View(heart_attack_data_1)
View(heart_attack_data)

Oldpeak_Q1 <- quantile(heart_attack_data_3$Oldpeak, 0.25, na.rm = TRUE)
Oldpeak_Q3 <- quantile(heart_attack_data_3$Oldpeak, 0.75, na.rm = TRUE)
Oldpeak_IQR <- Oldpeak_Q3 - Oldpeak_Q1
lower_Oldpeak <- Oldpeak_Q1 - 1.5 * Oldpeak_IQR
upper_Oldpeak <- Oldpeak_Q3 + 1.5 * Oldpeak_IQR
Oldpeak_outliers <- heart_attack_data_3$Oldpeak < lower_Oldpeak | heart_attack_data_3$Oldpeak > upper_Oldpeak
heart_attack_data_4 <- heart_attack_data_3[!Oldpeak_outliers, ]	
hist(heart_attack_data_4$Oldpeak, main = "Histogram of Oldpeak" ,col=c(6))
boxplot(heart_attack_data_4$Oldpeak, main = "Boxplot of Oldpeak" ,col=c(6))
plot(heart_attack_data_4$Oldpeak, main = "Plot of Oldpeak" ,col=c(6))



hist(heart_attack_data_3$Age, main = "Histogram of Age" ,col=c(7))
boxplot(heart_attack_data_3$Age, main = " Boxplot of Age" ,col=c(3))
plot(heart_attack_data_3$Age, main = "Plot of Age" ,col=c(6))
barplot(heart_attack_data_3$Age, main = "Bar of Age" ,col=c(6))


hist(heart_attack_data_3$Age)
boxplot(heart_attack_data_3$Age)
plot(heart_attack_data_3$Age)
barplot(heart_attack_data_3$Age)

Age_Q1 <- quantile(heart_attack_data_3$Age, 0.25, na.rm = TRUE)
Age_Q3 <- quantile(heart_attack_data_3$Age, 0.75, na.rm = TRUE)
Age_IQR <- Age_Q3 - Age_Q1
lower_Age <- Age_Q1 - 1.5 * Age_IQR
upper_Age <- Age_Q3 + 1.5 * Age_IQR
Age_outliers <- heart_attack_data_3$Age < lower_Age | heart_attack_data_3$Age > upper_Age
heart_attack_data_4 <- heart_attack_data_3[!Age_outliers, ]	
hist(heart_attack_data_4$Age, main = "Histogram of Age" ,col=c(7))
boxplot(heart_attack_data_4$Age, main = "Boxplot of Age" ,col=c(5))
plot(heart_attack_data_4$Age, main = "Plot of Age" ,col=c(2))
barplot(heart_attack_data_4$Age, main = "bar of Age" ,col=c(6))


attribute_names <- names(heart_attack_data_4)
for (attribute in attribute_names) 
{
  unique_values <- unique(heart_attack_data_4[[attribute]])
  print(paste("Unique values in", attribute, ":"))
  print(unique_values)
}

View(heart_attack_data_3)

unique(heart_attack_data_4$Sex)
heart_attack_data_4$Sex <- ifelse(heart_attack_data_4$Sex == "M","Male", heart_attack_data_4$Sex)
heart_attack_data_4$Sex <- ifelse(heart_attack_data_4$Sex == "F","Female", heart_attack_data_4$Sex)
unique(heart_attack_data_4$Sex)

View(heart_attack_data_4)


summary(heart_attack_data_4$Cholesterol)
boxplot(heart_attack_data_4$Cholesterol, main = "Boxplot of Cholesterol", col=c(7))


Cholesterol_Q1 <- quantile(heart_attack_data_4$Cholesterol, 0.25, na.rm = TRUE)
Cholesterol_Q3 <- quantile(heart_attack_data_4$Cholesterol, 0.75, na.rm = TRUE)
Cholesterol_IQR <- Cholesterol_Q3 - Cholesterol_Q1
lower_Cholesterol <- Cholesterol_Q1 - 1.5 * Cholesterol_IQR
upper_Cholesterol <- Cholesterol_Q3 + 1.5 * Cholesterol_IQR
Cholesterol_noisyvalue <- heart_attack_data_4$Cholesterol < lower_Cholesterol | heart_attack_data_4$Cholesterol > upper_Cholesterol
heart_attack_data_5 <- heart_attack_data_4[!Cholesterol_noisyvalue, ]
boxplot(heart_attack_data_5$Cholesterol, main = "Boxplot of Cholesterol", col=c(5))


View(heart_attack_data_5)


sum(duplicated(heart_attack_data_5))
heart_attack_data_6 <- heart_attack_data_5[!duplicated(heart_attack_data_5), ]
sum(duplicated(heart_attack_data_6))


View(heart_attack_data_6)


heart_attack_data_6$RestingECG <- factor(heart_attack_data_6$RestingECG,levels=c('Normal','ST'),labels=c("NORMAL","ABNORMALITY"))
heart_attack_data_6$Sex <- factor(heart_attack_data_6$Sex,levels=c('Male','Female'),labels=c("MALE","FEMALE"))
heart_attack_data_6$ExerciseAngina <- factor(heart_attack_data_6$ExerciseAngina,levels=c('Y','N'),labels=c("YES","NO"))
unique(heart_attack_data_5$ChestPainType)
heart_attack_data_6$ChestPainType <- factor(heart_attack_data_6$ChestPainType,levels=c('ATA','NAP','ASY','TA'),labels=c("ATYPICAL ANGINA","NON-ANGINAL PAIN","ASYMPTONIC
            ","TYPICAL ANGINA"))
heart_attack_data_6$HeartDisease <- factor(heart_attack_data_6$HeartDisease,levels=c(1,0),labels=c("MORE CHANCE","LESS CHANCE"))
unique(heart_attack_data_5$ST_Slope)
heart_attack_data_6$ST_Slope <- factor(heart_attack_data_6$ST_Slope,levels=c('Up','Flat'),labels=c("UP","FLAT"))

unique(heart_attack_data_5$FastingBS)
heart_attack_data_6$FastingBS <- factor(heart_attack_data_6$FastingBS,levels=c(0,1),labels=c("FALSE","TRUE"))

head(heart_attack_data_6)

View(heart_attack_data_6)



install.packages("dplyr")

library(dplyr)
heart_attack_data_6%>% summarise_if(is.numeric, sd)

install.packages("matrixStats")
library(matrixStats) 

heart_attack_data_7 <- heart_attack_data_6                    
heart_attack_data_7[heart_attack_data_6 < 0] <- NA       
View(heart_attack_data_7)

View(heart_attack_data)

colSums(is.na(heart_attack_data_7))    


missing_RestingBP <- is.na(heart_attack_data_7$RestingBP)
heart_attack_data_8 <- subset(heart_attack_data_7, !missing_RestingBP)
colSums(is.na(heart_attack_data_8))

View(heart_attack_data_7)
View(heart_attack_data_8)

summary(heart_attack_data_8$Age)
summary(heart_attack_data_8$MaxHR)


mean(heart_attack_data_8$Age)
median(heart_attack_data_8$Age)

var(heart_attack_data_8$Age)
sd(heart_attack_data_8$Age)
install.packages("ggplot2")
library(ggplot2)
ggplot(heart_attack_data_8, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "yellow", color = "blue") +
  labs(x = "Age", y = "Frequency", title = "Distribution of Age")


mean(heart_attack_data_8$MaxHR)
median(heart_attack_data_8$MaxHR)
var(heart_attack_data_8$MaxHR)
sd(heart_attack_data_8$MaxHR)
install.packages("ggplot2")
library(ggplot2)
ggplot(heart_attack_data_8, aes(x = MaxHR)) +
  geom_histogram(binwidth = 5, fill = "yellow", color = "black") +
  labs(x = "MaxHR", y = "Frequency", title = "Distribution of MaxHR")

View(heart_attack_data)
View(heart_attack_data_8)



