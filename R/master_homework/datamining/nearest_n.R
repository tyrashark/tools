# 1.

getwd()
setwd("C:/Users/LG/Documents/Data/Rdata/Data mining/data")


redpoints=read.table("trainred.txt",sep="\t",header=FALSE)

# green points from training dataset
greenpoints=read.table("traingreen.txt",sep="\t",header=FALSE)

# red points from test dataset
redtestpoints=read.table("testred.txt",sep="\t",header=FALSE)

# green points from test dataset
greentestpoints=read.table("testgreen.txt",sep="\t",header=FALSE)

# Combine red and green points.
X=rbind(redpoints,greenpoints)
dim(X) 


D = matrix(0, nrow = 200, ncol = 200)

ncol(D)
for (i in 1:200) for (j in 1:200) D[i,j] = sqrt(sum((X[i,] - X[j,])^2))



distance <- function(mat, mat2, i=1, j=1){
  mat[i, j] = sqrt(sum((mat2[i,] - mat2[j,])^2))
  Dist = mat
  i_1 = i+1
  if(i < nrow(mat)){
    return(distance(Dist, mat2, i_1, j))
  }else if(i == nrow(mat) & j < ncol(mat)){
    i_1 = 1
    j_1 = j+1
    return(distance(Dist, mat2, i_1, j_1))
  }else if(j == ncol(mat)) return(Dist)
}

distance(D, X)






distance <- function(mat, mat2, i=1, j=1){
  mat[i, j] = sqrt(sum((mat2[i,] - mat2[j,])^2))
  Dist = mat
  i_1 = i+1
  if(i < nrow(mat)){
    return(distance(Dist, mat2, i_1, j))
  }else if(j == ncol(mat)) return(Dist)
}





X0 <- matrix(0, 10000, 2)


x0 <- seq(-2.0, 4, length = 100)
length(x0)

for(i in 1:100){
  X0[((i-1)*100+1):(i*100), 1] <-x0[i]
  X0[((i-1)*100+1):(i*100), 2] <-x0
}
X0

D0 <- matrix(0, 10000, 200)
for (i in 1:10000) for (j in 1:200) D0[i,j] = sqrt(sum((X0[i,] - X[j,])^2))
D0




distance <- function(mat, mat2, X, i=1, j=1){
  mat[i, j] = sqrt(sum((mat2[i,] - X[j,])^2))
  Dist = mat
  j_1 = j+1
  if(j < ncol(mat)){
    return(distance(Dist, mat2, X=X, i, j_1))
  }else if(j == ncol(mat)) return(Dist)
}




distance2 <- function(mat, mat2, mat3, i, j){
  mat[i, j] = sqrt(sum((mat2[i,] - mat3[j,])^2))
  Dist = mat
  i_1 = i+1
  if(i < nrow(mat)){
    return(distance2(Dist, X0, X, i_1, j))
  }else if(i == nrow(mat)) return(Dist)
}

distance2(D0, X0, X, 1, 1)



for (j in 1:200){
  D0 <-distance2(D0, X0, X, i=1, j=j)
}




k=7
g.hat7=rep(0,200)
for (i in 1:200) g.hat7[i]=(mean(y[order(D[i,])[1:k]])>0.5)
g.hat7
training.error=1-sum(g.hat7==y)/200
training.error





prob15 <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()





