#50000
library(MASS)
library(glmnet)
library(ggplot2)
library(lars)
setwd("C:/Users/wanghua/Desktop/Fall2020/Research")

X_raw <- read.table('HIV.txt', sep=',')
n <- dim(X_raw)[1]
p <- dim(X_raw)[2]
s = as.integer(p * 0.2)
X <- scale(X_raw)/sqrt(n-1)
total = 200


beta0=rep(0,p)
beta0[1:s]=seq(200, 1, length.out = s)

beta1=rep(0,p)
beta1[1:s]=rep(200,s) 

beta2=rep(0,p)
beta2[1:s]=seq(200,120,length.out = s) 

beta3=rep(0,p)
beta3[1:s]=seq(200,60,length.out = s)

lambda1 <- c(seq(0.1, 15, length = 100), seq(15, 2500, length = 100))
lambda2 <- c(seq(0.1, 50, length = 100), seq(50, 3600, length = 100))
lambda3 <- c(seq(0.1, 35, length = 100), seq(35, 2500, length = 100))
lambda4 <- c(seq(0.1, 45, length = 100), seq(45, 2500, length = 100))

fdp1=matrix(0, total,length(lambda1))
tpp1=matrix(0, total,length(lambda1))
fdp2=matrix(0, total,length(lambda2))
tpp2=matrix(0, total,length(lambda2))
fdp3=matrix(0, total,length(lambda3))
tpp3=matrix(0, total,length(lambda3))
fdp4=matrix(0, total,length(lambda4))
tpp4=matrix(0, total,length(lambda4))


##essential func
fdp_tpp_lasso <- function(X, beta, eps, lambda){
  ##X is matrix, beta, eps, lambda are vectors
  Y=X%*%beta + eps
  fit<-glmnet(X,Y,intercept=TRUE, lambda = lambda, thresh = 1e-12)
  betahat <- coef(fit)[-1, ]
  discov <- apply(betahat!=0, 2, sum)
  falsediscov <- apply(betahat[-(1:s), ]!=0, 2, sum)
  fdp_vec=falsediscov/discov
  tpp_vec=(discov-falsediscov)/s
  return(list(fdp_vec, tpp_vec))
}

##Simulation
for (k in 1:total){
  print(k)
  #X=mvrnorm(n,rep(0,p),diag(1/n,p))
  #eps = rep(0, n)  ##w/o noise
  #eps = rnorm(n)  ##w/ noise
  #eps = rnorm(n, 0, 50)  ##w/ large noise = 50
  eps = rnorm(n, 0, 10)  ##w/ large noise = 10
  ft1 = fdp_tpp_lasso(X, beta0, eps, lambda1)
  
  ft2 = fdp_tpp_lasso(X, beta1, eps, lambda2)
  
  ft3 = fdp_tpp_lasso(X, beta2, eps, lambda3)
  
  ft4 = fdp_tpp_lasso(X, beta3, eps, lambda4)
  
  fdp1[k,]=ft1[[1]]
  tpp1[k,]=ft1[[2]]
  
  fdp2[k,]=ft2[[1]]
  tpp2[k,]=ft2[[2]]
  
  fdp3[k,]=ft3[[1]]
  tpp3[k,]=ft3[[2]]
  
  fdp4[k,]=ft4[[1]]
  tpp4[k,]=ft4[[2]]
}

fdp11=apply(fdp1,2,mean)
fdp22=apply(fdp2,2,mean)
fdp33=apply(fdp3,2,mean)
fdp44=apply(fdp4,2,mean)


tpp11=apply(tpp1,2,mean)
tpp22=apply(tpp2,2,mean)
tpp33=apply(tpp3,2,mean)
tpp44=apply(tpp4,2,mean)


plot(c(0,1),c(0,1),xlim=c(0,1),type="n",ylim=c(0,0.5),xlab="TPP",ylab="FDP")
legend("topleft", inset=0.01,
       c("Completely uniform","Less Uniform","Less Varied", "Completely Varied"), 
       col=c("red","green","orange","black"))
lines(tpp22,fdp22,col="red")  ##Comletely uniform - All Strongest
lines(tpp33,fdp33,col="green")##Less Uniform - 2nd Strong
lines(tpp44,fdp44,col="orange")##Less Varied - All Weaker
lines(tpp11,fdp11,col="black") ##comletely varied - All Weakest


dt <- data.frame(tpp22,fdp22,tpp33,fdp33,tpp44,fdp44,tpp11,fdp11)

#write.table(dt, "Hiv_fdp_tpp_fixed.txt", sep="\t", row.names = F, col.names = F)
#write.table(dt, "Hiv_fdp_tpp_fixed_with_noise.txt", sep="\t", row.names = F, col.names = F)
#write.table(dt, "Hiv_fdp_tpp_fixed_with_noise_50.txt", sep="\t", row.names = F, col.names = F)
write.table(dt, "Hiv_fdp_tpp_fixed_with_noise_10.txt", sep="\t", row.names = F, col.names = F)

