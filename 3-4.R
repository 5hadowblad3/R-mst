library(car)

Y <- matrix(c(91, 83, 77, 70, 63, 6, 11, 15, 20, 24, 2, 4, 5, 6, 8, 0.03, 0.10, 0.19, 0.31, 0.40, 0.36, 1.25, 2.48, 3.88, 5), ncol=5)
len <- 5

y <- matrix(0, nrow = 5, ncol = 5)
for (i in 1:len)
	y[i, ] <- abs(rnorm(5, 0, 1))

t <- c(1:len)

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

test <- function(t, theta1, theta2, theta3, theta4, theta5){
	A <- matrix(c((-theta1 - theta2), 0, 0, 0, 0, theta1, 0, 0, 0, 0, theta2, 0, (-theta3 - theta4), 0, theta5, 0, 0, theta3, 0, 0, 0, 0, theta4, 0, -theta5), ncol = 5, byrow = TRUE)
	l <- length(T)
	res <- matrix(0, nrow = l, ncol = 5)
	X <- expm(A * t)
	tmp <- apply(X, 1, fun)
	tmp <- t(tmp)
	res <- apply(tmp, 1, sum)
	print(res)
	print(Y[t, ])
	return (Y[t, ] - res)
}

reaction <- function(T,theta1, theta2, theta3, theta4, theta5){
	theta <- c(theta1, theta2, theta3, theta4, theta5)
	res <- apinene_modele_prediction(T, theta)
	return (sum(Y[T, ] - res[T, ]))
}

reaction <- function(T,theta){
	#theta <- c(theta1, theta2, theta3, theta4, theta5)
	res <- apinene_modele_prediction(T, theta)
	print(T)
	return (sum(Y[T, ] - res[T, ]))
}

theta0 <- c(0.2, 0.2, 0.2, 0.2, 0.2)
USPop <- data.frame(t, y)

#st <- coef(nls(log(y) ~ log(reaction(t, theta)), USPop, start = list(theta = theta0)))
#res <- nls(y ~ reaction(t, theta1, theta2, theta3, theta4, theta5), start = list(theta1 = 0.2, theta2 = 0.2, theta3 = 0.2, theta4 = 0.2, theta5 = 0.2), data = USPop, trace = T, algorithm = "plinear")
#summary(pop) 

#res <- nls(y ~ test(t, theta), start = list(theta = theta0), data = USPop, trace = T)

res <- nls(y ~ test(t, theta1, theta2, theta3, theta4, theta5), start = list(theta1 = 0.15, theta2 = 0.2, theta3 = 0.2, theta4 = 0.1, theta5 = 0.2), data = USPop, trace = TRUE)