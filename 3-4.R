library(Matrix)
library(expm)

fun<-function(X){
x0<-c(100,0,0,0,0)
	return (X*x0)
}


Y = read()
t = Y[, 1]

y1 = t(Y[, 2])
y2 = t(Y[, 3])
y3 = t(Y[, 4])
y4 = t(Y[, 5])
y5 = t(Y[, 6])

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
	l <- length(t)
	res <- matrix(0, nrow = l, ncol = 5)
	for(i in 1:l){
		X<-expm(A*i)
		tmp<-apply(X,1,fun)
		tmp<-t(tmp)
		res[i,]<-apply(tmp,1,sum)
	}
	print(res[l, ])
	print(t)
	return (res[l,])
}

resy<-apply(Y,1,sum)

theta0 <- c(0.2, 0.2, 0.2, 0.2, 0.2)
USPop <- data.frame(t, y1, y2, y3, y4, y5)
print(t)
res <- nls( ~ test(t,theta1,theta2,theta3,theta4,theta5), start = list(theta1 = 0.2,theta2=0.2,theta3=0.2,theta4=0.2,theta5=0.2),data=USPop,trace=T)
summary(res)
#nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/1024,
#		            printEval = FALSE, warnOnly = FALSE)