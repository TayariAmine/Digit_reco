library(dplyr)


# Nous avons choisi les 6 meilleurs solutions pour les combiner avec le modèle de scoring suivant 

A <- read.csv("C:/Users/Tayari/Desktop/Tayari/Projects/Kaggle/DReco/prediction1.csv",sep=",", header=TRUE)
B <- read.csv("C:/Users/Tayari/Desktop/Tayari/Projects/Kaggle/DReco/prediction2.csv",sep=",", header=TRUE)
C <- read.csv("C:/Users/Tayari/Desktop/Tayari/Projects/Kaggle/DReco/prediction3.csv",sep=",", header=TRUE)
D <- read.csv("C:/Users/Tayari/Desktop/Tayari/Projects/Kaggle/DReco/prediction4.csv",sep=",", header=TRUE)
E <- read.csv("C:/Users/Tayari/Desktop/Tayari/Projects/Kaggle/DReco/prediction5.csv",sep=",", header=TRUE)
G <- read.csv("C:/Users/Tayari/Desktop/Tayari/Projects/Kaggle/DReco/prediction6.csv",sep=",", header=TRUE)


M <- cbind(A,B,C,D,E,G)
v = c(1,3,5,7,9,11)
M <- M[,-v]
MS = M

# le bloc suivant calcul la matrice MS qui contient les predictions et leurs scores

for ( i in 1:nrow(M)) { 
  for (j in 1:ncol(M)) {
    val = M[i,j]
    c=0
    for (l in 1:ncol(M)) {
      if(val == M[i,l])
        c=c+1
    }
    MS[(2*i)-1,j] = val
    MS[(2*i),j] = c
    }
}

# le bloc suivant calcul la matrice M qui contient les predictions et la nouvelle prediction dans la dernière colonne

max <- 0
ind <- 0
n = nrow(M)
z = ncol(M)

for ( i in 1:n) { 
  for (j in 1:z) {
    if(max < MS[2*i,j]){
      max <- MS[2*i,j]
      ind <- M[i,j] 
    }
  M[i, z +1] <- ind
  }
  max <- 0
}

# Dans cette partie nous avons exporté la nouvelle production pour la tester sur Kaggle

v1 = A[,1]
v2 = M[,ncol(M)]
prediction = cbind(v1,v2)
colnames(prediction)<-c("ImageId", "Label")
write.csv(prediction, file="prediction_comb.csv", row.names=F)
