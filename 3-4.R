library(Matrix)
library(expm)

fun<-function(X){
x0<-c(100,0,0,0,0)
	return (X*x0)
}


Y = read()
t = Y[, 1]

y = c()
for (i in 2:6) {
	y = c(y, t(Y[, i]))
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
	return (res)
}


library(car)


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
	l <- length(t) / 5
	res <- matrix(0, nrow = l, ncol = 5)
	for(i in 1:l){
		T=t[i]
		X<-expm(A*T)
		tmp<-apply(X,1,fun)
		tmp<-t(tmp)
		res[i, ]<-apply(tmp,1,sum)
	}
	print(res[l, ])
	temp <- c()
	for(i in 1:5) {
		temp <- c(temp, res[, i])
	}
	return (temp)
}

resy<-apply(Y,1,sum)

theta0 <- c(0.2, 0.2, 0.2, 0.2, 0.2)
USPop <- data.frame(t, y)

res <- nls(y ~ test(t,theta1,theta2,theta3,theta4,theta5), start = list(theta1=0.1,theta2=0.2,theta3=0.2,theta4=0.2,theta5=0.2),data=USPop,trace=T, alg="port")
summary(res)
#nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/1024,
#		            printEval = FALSE, warnOnly = FALSE)

#library("nlstools")
#confint2(res,"theta1","theta2","theta3","theta4","theta5",level=0.95)