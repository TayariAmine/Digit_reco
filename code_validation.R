library(readr)


train1 <- read.csv("C:/Users/Tayari/Desktop/Formation R/train.csv",sep=";", header=TRUE)

v <- seq(from = 4700, to = 4999, by = 1)
v1 <- seq(from = 1, to = 4699, by = 1)

train <- train1[-v,]
test <- train1[-v1,-1]


cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))



X<-train[,-1]
Y<-train[,1]

trainLabel<-Y
Xreduced<-X/255
Xcov<-cov(Xreduced)
Xpca<-prcomp(Xcov)

trainLabel<-as.factor(trainLabel)


Xfinal<-as.matrix(Xreduced) %*% Xpca$rotation[,1:50]


library(e1071)
model_svm<-svm(Xfinal, trainLabel, kernel="polynomial",degree = 3)


testreduced<-test/255
testfinal<-as.matrix(testreduced) %*% Xpca$rotation[,1:50]

# calculating accuracies
prediction<-predict(model_svm, testfinal, type="class")
prediction<-as.data.frame(prediction)
finalprediction<-cbind(as.data.frame(1:nrow(prediction)), prediction)
colnames(finalprediction)<-c("ImageId", "Label")
write.csv(finalprediction, file="prediction_datamn.csv", row.names=F)