library(e1071)

#Importing pima dataset
pima = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data")
names(pima) = c("PregnancyNumber","PlasmaGlucose","BP","Triceps","Insulin","BMI","DiabetesFunction","Age","Class")

sum=0.00;
for(i in 1:10)
{
  trainRow = sample(1:nrow(pima), size=0.9*nrow(pima))
  #Pima dataset is divided into 90% training & 10% test data
  test = pima[trainRow,]
  train = pima[-trainRow,]
  train$Class = as.factor(train$Class)
  test$Class = as.factor(test$Class)
  NBclassifier <- naiveBayes(Class ~., data = train)
  NBprediction <- predict(NBclassifier, test)
  myTable <- table(test[,9],predicted = NBprediction)
  accuracy <- sum(myTable[row(myTable)==col(myTable)])/sum(myTable)*100
  print(accuracy)
  sum=sum+accuracy
}
average_accuracy <- sum/10;
print(average_accuracy)
