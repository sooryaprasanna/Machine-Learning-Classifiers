library(e1071)

#Importing pima dataset
pima = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data")
names(pima) = c("PregnancyNumber","PlasmaGlucose","BP","Triceps","Insulin","BMI","DiabetesFunction","Age","Class")

total_rows <- nrow(pima)
sum=0.00;
print("Default")
for(i in 1:10)
{
  trainRow <- sort(sample(1:total_rows, floor(total_rows*0.9)))
  pima.train <- pima[trainRow,]
  pima.test <- pima[-trainRow,]
  svm_classifier <- svm(as.factor(Class) ~ ., data = pima.train)
  pred <- predict(svm_classifier, pima.test[,-9])
  accuracy <- (mean(pred == t(pima.test[9])))*100
  print(accuracy)
  sum=sum+accuracy
}
average_accuracy <- sum/10
average_accuracy

#Linear
sum=0.00;
for(i in 1:10)
{
  trainRow <- sort(sample(1:total_rows, floor(total_rows*0.9)))
  pima.train <- pima[trainRow,]
  pima.test <- pima[-trainRow,]
  svm_classifier <- svm(as.factor(Class) ~ ., data = pima.train,kernel="linear")
  pred <- predict(svm_classifier, pima.test[,-9])
  accuracy <- (mean(pred == t(pima.test[9])))*100
  sum=sum+accuracy
}
average_accuracy <- sum/10
print("linear")
print(average_accuracy)

#Polynomial
sum=0.00;
for(i in 1:10)
{
  trainRow <- sort(sample(1:total_rows, floor(total_rows*0.9)))
  pima.train <- pima[trainRow,]
  pima.test <- pima[-trainRow,]
  svm_classifier <- svm(as.factor(Class) ~ ., data = pima.train,kernel="polynomial")
  pred <- predict(svm_classifier, pima.test[,-9])
  accuracy <- (mean(pred == t(pima.test[9])))*100
  sum=sum+accuracy
}
average_accuracy <- sum/10
print("polynomial")
print(average_accuracy)

#radial
sum=0.00;
for(i in 1:10)
{
  trainRow <- sort(sample(1:total_rows, floor(total_rows*0.9)))
  pima.train <- pima[trainRow,]
  pima.test <- pima[-trainRow,]
  svm_classifier <- svm(as.factor(Class) ~ ., data = pima.train,kernel="radial")
  pred <- predict(svm_classifier, pima.test[,-9])
  accuracy <- (mean(pred == t(pima.test[9])))*100
  sum=sum+accuracy
}
average_accuracy <- sum/10
print("radial")
print(average_accuracy)

#sigmoid
sum=0.00;
for(i in 1:10)
{
  trainRow <- sort(sample(1:total_rows, floor(total_rows*0.9)))
  pima.train <- pima[trainRow,]
  pima.test <- pima[-trainRow,]
  svm_classifier <- svm(as.factor(Class) ~ ., data = pima.train,kernel="sigmoid")
  pred <- predict(svm_classifier, pima.test[,-9])
  accuracy <- (mean(pred == t(pima.test[9])))*100
  sum=sum+accuracy
}
average_accuracy <- sum/10
print("sigmoid")
print(average_accuracy)
