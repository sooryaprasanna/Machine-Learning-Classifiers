library(class)

#Importing pima dataset
pima <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data")
names(pima) = c("PregnancyNumber","PlasmaGlucose","BP","Triceps","Insulin","BMI","DiabetesFunction","Age","Class")
summary(pima)
str(pima)

# Histogram and Barplot of all Attributes
hist(pima$PregnancyNumber)
barplot(pima$PregnancyNumber, main = "Barplot of Pregnancy Number")

hist(pima$PlasmaGlucose)
barplot(pima$PlasmaGlucose, main = "Barplot of Plasma Glucose")

hist(pima$BP)
barplot(pima$BP, main = "Barplot of BP")

hist(pima$Triceps)
barplot(pima$Triceps, main = "Barplot of Triceps")

hist(pima$Insulin)
barplot(pima$Insulin, main = "Barplot of Insulin")

hist(pima$BMI)
barplot(pima$BMI, main = "Barplot of BMI")

hist(pima$DiabetesFunction)
barplot(pima$DiabetesFunction, main = "Barplot of DiabetesFunction")

hist(pima$Age)
barplot(pima$Age, main = "Barplot of Age")

#Correlation with Class variable
cor(pima$Class,pima$PregnancyNumber)
cor(pima$Class,pima$PlasmaGlucose)
cor(pima$Class,pima$BP)
cor(pima$Class,pima$Triceps)
cor(pima$Class,pima$Insulin)
cor(pima$Class,pima$BMI)
cor(pima$Class,pima$DiabetesFunction)
cor(pima$Class,pima$Age)


#Correlation with each attribute
max=0.0;
attribute1=1;
attribute2=1;

for(i in 1:8)
{
  for(j in i:8)
  {
    if(i!=j)
    {
      temp=cor(pima[,i],pima[,j])
      if(temp>max)
      {
        max=temp;
        attribute1=i;
        attribute2=j;
      }
    }
  }
}
max
colnames(pima)[attribute1]
colnames(pima)[attribute2]

