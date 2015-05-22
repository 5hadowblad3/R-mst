library(Matrix)
library(expm)

fun<-function(X){
x0<-c(100,0,0,0,0)
	return (X*x0)
}

apinene_modele_prediction<-function(T,theta){
	A<-matrix(c((-theta[1]-theta[2]),0,0,0,0,theta[1],0,0,0,0,theta[2],0,(-theta[3]-theta[4]),0,theta[5],0,0,theta[3],0,0,0,0,theta[4],0,-theta[5]), ncol=5,byrow=TRUE)
	l<-length(T)
	res<-matrix(0,nrow=l,ncol=5)
	for(i in 1:l){
		t=T[i]
		X<-expm(A*t)
		tmp<-apply(X,1,fun)
		tmp<-t(tmp)
		res[i,]<-apply(tmp,1,sum)
	}
	#print(res)
	return (res)
}

#t<-c(1,2,3,4)
#theta<-c(0.06,0.03,0.0205,0.275,0.04)

#apinene_modele_prediction(t,theta)

Y<-matrix(c(91,83,77,70,6,11,15,20,2,4,5,6,0.03,0.10,0.19,0.31,0.36,1.25,2.48,3.88),ncol=5)
print(Y)


#min_erreur(4)


library(car)

Y <- matrix(c(91, 83, 77, 70, 63 ,56 ,50 ,42 ,35 ,28,
	      6, 11, 15, 20, 24,29,35,40,46,51, 
	      2, 4, 5, 6, 8, 10,12,15,18,20,
	      0.03, 0.10, 0.19, 0.31, 0.40,0.5,0.6,0.72,0.83,0.94, 
	      0.36, 1.25, 2.48, 3.88, 5, 6.2,7.7,8.5,9.6,10.8), ncol=5)
len <- 10
y <- rnorm(len, 0, runif(1, min = 0, max = 1))
print(y)

t <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

reaction <- function(T,theta1,theta2,theta3,theta4,theta5){
	theta<-c(theta1,theta2,theta3,theta4,theta5)
	res <- apinene_modele_prediction(T, theta)
	l<-length(T)
	e<-matrix(0,nrow=1,ncol=l)
	for(i in 1:l){
		e[i]=sum(Y[i,]-res[i,])
	}
	print(e)
	return (e)
}

test <- function(t, theta1, theta2, theta3, theta4, theta5){
	A <- matrix(c((-theta1 - theta2), 0, 0, 0, 0, theta1, 0, 0, 0, 0, theta2, 0, (-theta3 - theta4), 0, theta5, 0, 0, theta3, 0, 0, 0, 0, theta4, 0, -theta5), ncol = 5, byrow = TRUE)
	l <- length(t)
	res <- matrix(0, nrow = l, ncol = 5)
	for(i in 1:l){
		X<-expm(A*i)
		tmp<-apply(X,1,fun)
		tmp<-t(tmp)
		res[i,]<-apply(tmp,1,sum)
	}
#	X <- expm(A * t)
#	tmp <- apply(X, 1, fun)
#	tmp <- t(tmp)
#	res <- apply(tmp, 1, sum)
	print(res)
	resx<-matrix(0,nrow=10,ncol=10)
	for(i in 1:l){
		resx[i,]<-apply(res,1,sum)
	}
	print("x")
	print(resx)
	x<-resx[1,]
	print(x)
	return (x)
}
resy<-matrix(0,nrow=10,ncol=10)
for(i in 1:10){
	resy[i,]<-apply(Y,1,sum)
}
y<-resy[1,]
print(y)

theta0 <- c(0.2, 0.2, 0.2, 0.2, 0.2)
USPop <- data.frame(t, y)
print(USPop)
str(USPop)
t=10
#st <- coef(nls(log(y) ~ log(reaction(t, theta1,theta2,theta3,theta4,theta5)), USPop, start = list(theta1 = 0.2,theta2=0.2,theta3=0.2,theta4=0.2,theta5=0.2)))
res <- nls(y ~ test(t,theta1,theta2,theta3,theta4,theta5), start = list(theta1 = 0.2,theta2=0.2,theta3=0.2,theta4=0.2,theta5=0.2),data=USPop,trace=T)
#summary(pop)
nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/1024,
		            printEval = FALSE, warnOnly = FALSE)
