## =======================================================================
##             unitscale function
## This function is used to scale the a set to [0,1] in each
## dimension base on the candidate set.
##
## Input:
## raw.mat: The set is needed to be scaled
## Cand.mat: The candidate set (if it is NULL, autoset raw.mat as cand.mat)
##
## Output:
## raw.mat.new: The scaled set
## ========================================================================

unitscale <- function(raw.mat,cand.mat=NULL)
{
  p <- dim(raw.mat)[2]-1
  raw.mat.new <- raw.mat
  if(is.null(cand.mat))
  {
    cand.mat=raw.mat
  }
  for(i in 1:p)
  {
    raw.mat.new[,i] <- (raw.mat[,i]-min(cand.mat[,i]))/(max(cand.mat[,i])-min(cand.mat[,i]))
  }
  return(raw.mat.new)
}

## =======================================================================
##        NUSF via Clustering method (minimax) function
##
##  Input:
##  xmat:   Input space candidate set with weight.
##  N:      The number of runs in a design.
##  Method: The method of hierarchical clustering
##
##  Output:
##  X.design:    The minimax point of each cluster in input space.
##  X.candidate: Input space candidate set with weight.
##  Cluster.num: The number of cluster that each point in candidate set
##               belongs to.
##
##==========================================================================

NUSF.cluster.mM <- function(xmat,N,Method) {
  library(fastcluster, quietly = T)
  Nd <- N 
  nxq <- dim(xmat)[1]
  nxp <- dim(xmat)[2]
  xmat.r <- as.matrix(xmat)

  randmat.x <- as.matrix(unitscale(xmat.r)[,1:(nxp-1)])
  wt.x = matrix(xmat.r[,nxp],nrow=1)
  wt.mat.x = sqrt(t(wt.x)%*%wt.x)
  wt.mat.x = wt.mat.x[lower.tri(wt.mat.x)]
  dX = stats::dist(randmat.x, method = "euclidean")*wt.mat.x

  groupid <- stats::cutree(stats::hclust(d = dX, method = Method), k = Nd)

  outmat.x <- matrix(0,nrow = Nd,ncol = nxp)
  radius <- rep(0,Nd)
  for (j in 1:Nd) {
    cluster.x.original = matrix(xmat.r[which(groupid == j),],ncol = nxp)
    cluster.x = matrix(randmat.x[which(groupid == j),],ncol = (nxp-1))
    nd = dim(cluster.x)[1]
    np = dim(cluster.x)[2]
    
    cluster.wt.x = matrix(cluster.x.original[,nxp],ncol=1)
    cluster1 = cbind(cluster.x,cluster.wt.x)
    
    Dist.mat = apply(cluster1, 1, function(P1){
      max(rowSums((cluster.x-matrix(rep(P1[1:np],nd),ncol=np,byrow = T))^2)*cluster.wt.x*rep(P1[nxp],nd))
    })
    Minimax.point = which(Dist.mat==min(Dist.mat))
    
    if (length(Minimax.point)>1) {
      Minimax.point = sample(Minimax.point,1)
    }
    outmat.x[j,] = cluster.x.original[Minimax.point,]
    A1 = cluster.x.original[Minimax.point,]
    radius[j] = sqrt(max(rowSums((cluster.x.original[,1:np]-matrix(rep(A1[1:np],nd),ncol=np,byrow = T))^2)))
  }
  
  result = list(X.design = outmat.x, X.candidate = xmat.r,Cluster.num = groupid,Radius = radius)
  return(result)
}


## =======================================================================
##        NUSF via Clustering method (maximin) function
##
##  Input:
##  xmat:   Input space candidate set with weight.
##  N:      The number of runs in a design.
##  Method: The method of hierarchical clustering
##
##  Output:
##  X.design:    The minimax point of each cluster in input space.
##  X.candidate: Input space candidate set with weight.
##  Cluster.num: The number of cluster that each point in candidate set
##               belongs to.
##
##==========================================================================

NUSF.cluster.Mm <- function(xmat,N,Method) {
  library(fastcluster, quietly = T)
  Nd <- N 
  nxq <- dim(xmat)[1]
  nxp <- dim(xmat)[2]
  xmat.r <- as.matrix(xmat)
  
  randmat.x <- as.matrix(unitscale(xmat.r)[,1:(nxp-1)])
  wt.x = matrix(xmat.r[,nxp],nrow=1)
  wt.mat.x = sqrt(t(wt.x)%*%wt.x)
  wt.mat.x = wt.mat.x[lower.tri(wt.mat.x)]
  dX = stats::dist(randmat.x, method = "euclidean")*wt.mat.x
  
  groupid <- stats::cutree(stats::hclust(d = dX, method = Method), k = Nd)
  
  outmat.x <- matrix(0,nrow = Nd,ncol = nxp)
  radius <- rep(0,Nd)
  for (j in 1:Nd) {
    cluster.x = matrix(randmat.x[which(groupid == j),1:(nxp-1)],ncol = (nxp-1))
    cluster.x.r = matrix(randmat.x[which(groupid != j),1:(nxp-1)],ncol = (nxp-1))
    cluster.x.original = matrix(xmat.r[which(groupid == j),],ncol = nxp)
    cluster.x.original.r = matrix(xmat.r[which(groupid != j),],ncol = nxp)
    nd = dim(cluster.x)[1]
    np = dim(cluster.x)[2]
    rd = dim(cluster.x.r)[1]
    
    cluster.wt.x = matrix(xmat.r[which(groupid == j),nxp],ncol=1)
    cluster.wt.x.r = matrix(xmat.r[which(groupid != j),nxp],ncol=1)
    cluster1 = cbind(cluster.x,cluster.wt.x)
    
    Dist.mat = apply(cluster1, 1, function(P1){
      min(rowSums((cluster.x.r-matrix(rep(P1[1:np],rd),ncol=np,byrow = T))^2)*cluster.wt.x.r*rep(P1[nxp],rd))
    })
    Maxmine.point = which(Dist.mat==max(Dist.mat))
    if (length(Maxmine.point)>1) {
      Maxmine.point = sample(Maxmine.point,1)
    }
    outmat.x[j,] = cluster.x.original[Maxmine.point,]
    A1 = cluster.x.original[Maxmine.point,]
    radius[j] = sqrt(min(rowSums((cluster.x.original.r[,1:np]-matrix(rep(A1[1:np],rd),ncol=np,byrow = T))^2)))
  }
  result = list(X.design = outmat.x, X.candidate = xmat.r,Cluster.num = groupid,Radius = radius)
  
  return(result)
}

##======================================================================
## Use grid on the range and adjust weights to have certain max ratio 
## function to adjust weights 
##======================================================================
# scale.wts <- function(dat,maxwtratio) { 
#   min.y <- min(dat[,3]) 
#   max.y <- max(dat[,3]) 
#   new.y <- 1 + (maxwtratio - 1)*(dat[,3]-min.y)/(max.y-min.y) 
#   #new.y <- 1 + (maxwtratio - 1)*(max.y-dat[,3])/(max.y-min.y) 
#   print(summary(new.y)) 
#   fin.dat <- cbind(dat[,1:2],new.y) 
#   return(fin.dat) 
# } 

scale.wts <- function(dat,maxwtratio) { 
  nx = dim(dat)[2]
  min.y <- min(dat[,nx]) 
  max.y <- max(dat[,nx]) 
  new.y <- 1 + (maxwtratio - 1)*(dat[,nx]-min.y)/(max.y-min.y) 
  #new.y <- 1 + (maxwtratio - 1)*(max.y-dat[,3])/(max.y-min.y) 
  print(summary(new.y)) 
  fin.dat <- cbind(dat[,1:(nx-1)],new.y) 
  return(fin.dat) 
}


