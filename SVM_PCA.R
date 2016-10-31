
library(readr)
library(e1071)

# lecture des données:
train <- read.csv("C:/Users/Tayari/Desktop/Formation R/train.csv",sep=",", header=TRUE)
test <- read.csv("C:/Users/Tayari/Desktop/Formation R/test.csv",sep=",", header=TRUE)

# affichage des données:

cat(sprintf("Train set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))

XX<-train[,-1]
YY<-train[,1]

train_Label<-YY
X_reduced<-XX/255
X_cov<-cov(X_reduced)
X_pca<-prcomp(X_cov)


train_Label<-as.factor(train_Label)

# application de PCA sur train
X_final<-as.matrix(X_reduced) %*% X_pca$rotation[,1:50]

# SVM

# nous avons essayé plusieurs valeurs de degree (2,3,4,5)

model_svm <- svm(X_final, train_Label, kernel="polynomial",degree = 2)

# application de PCA sur test

test_reduced<-test/255
test_final<-as.matrix(test_reduced) %*% X_pca$rotation[,1:50]


# predictions 

prediction<-predict(model_svm, test_final, type="class")
prediction<-as.data.frame(prediction)
finalprediction<-cbind(as.data.frame(1:nrow(prediction)), prediction)
colnames(finalprediction)<-c("ImageId", "Label")
write.csv(finalprediction, file="prediction_data_red.csv", row.names=F)


