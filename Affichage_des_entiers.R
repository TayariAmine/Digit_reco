# lecture des données:

train1 <- read.csv("C:/Users/Tayari/Desktop/Formation R/test.csv",sep=",", header=TRUE)
pred <- read.csv("C:/Users/Tayari/Desktop/Formation R/prediction.csv",sep=",", header=TRUE)

pred <- as.matrix(pred)
train1 <- as.matrix(train1)
dim(pred)


train2 <- cbind(pred,train1)

train<-train2[,-1]

# définition des couleurs 

colors<-c('white','black')
cus_col<-colorRampPalette(colors=colors)

# affichage des images des entiers

par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
all_img<-array(dim=c(10,28*28))

for(di in 0:9)
{
  print(di)
  all_img[di+1,]<-colSums(train[train[,1]==di,-1])
  all_img[di+1,]<-all_img[di+1,]/max(all_img[di+1,])*255
  
  z<-array(all_img[di+1,],dim=c(28,28))
  z<-z[,28:1] 
  image(1:28,1:28,z,main=di,col=cus_col(256))
} 