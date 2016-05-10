library(class)

#Importing pima dataset
pima = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data")
names(pima) = c("PregnancyNumber","PlasmaGlucose","BP","Triceps","Insulin","BMI","DiabetesFunction","Age","Class")

total_rows <- nrow(pima)

kvalue=c(3,5,7,9,11)
for(j in 1:5)
{
  sum=0.00;
  for(i in 1:10)
  {
    trainRow <- sort(sample(1:total_rows, floor(total_rows*0.9)))
    pima.train <- pima[trainRow,]
    pima.test <- pima[-trainRow,]
    pred <- knn(pima.train[,-9], pima.test[,-9], pima.train[,9], k = kvalue[j], prob=TRUE)
    accuracy <- (mean(pred == t(pima.test[9])))*100
    sum=sum+accuracy
  }
  average_accuracy <- sum/10
  print (kvalue[j])
  print(average_accuracy)
}

