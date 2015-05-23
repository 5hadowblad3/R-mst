#Q3(d)
#calculer l'estimateur des moindres carrees en utilisant nls
library(Matrix)
library(expm)
library(car)

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

non_lineaire <- function(t, theta1, theta2, theta3, theta4, theta5){
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
	temp <- c()
	for(i in 1:5) {
		temp <- c(temp, res[, i])
	}
	return (temp)
}

USPop <- data.frame(t, y)

res <- nls(y ~ non_lineaire(t,theta1,theta2,theta3,theta4,theta5), start = list(theta1=0.1,theta2=0.2,theta3=0.2,theta4=0.2,theta5=0.2),data=USPop,trace=TRUE, alg="port")
summary(res)

