library(car)

Y <- read()
t <- Y[, 1]
Y <- Y[, -1]
len <- nrow(Y)

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
	print(res)
	return (res)
}

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
USPop <- data.frame(t, Y)

#st <- coef(nls(log(y) ~ log(reaction(t, theta)), USPop, start = list(theta = theta0)))
#res <- nls(y ~ reaction(t, theta1, theta2, theta3, theta4, theta5), start = list(theta1 = 0.2, theta2 = 0.2, theta3 = 0.2, theta4 = 0.2, theta5 = 0.2), data = USPop, trace = T, algorithm = "plinear")
#summary(pop) 

#res <- nls(y ~ test(t, theta), start = list(theta = theta0), data = USPop, trace = T)

res <- nls(Y ~ test(t, theta1, theta2, theta3, theta4, theta5), start = list(theta1 = 0.15, theta2 = 0.2, theta3 = 0.2, theta4 = 0.1, theta5 = 0.2), data = USPop, trace = TRUE, alg="plinear")