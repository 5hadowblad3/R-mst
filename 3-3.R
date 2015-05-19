library("scatterplot3d")

Y<-matrix(c(91,83,77,70,6,11,15,20,2,4,5,6,0.03,0.10,0.19,0.31,0.36,1.25,2.48,3.88),ncol=5)
theta1 = seq(0.04, 0.08, 0.01)
theta2 = seq(0.01, 0.05, 0.01)


print(Y)

erreur<-function(theta1,theta2,t){
	theta<-c(theta1,theta2,0.0205,0.275,0.04)
	T<-c(1:t)
	X<-apinene_modele_prediction(T,theta)
	sum<-0
	for(j in 1:5){
		for(i in 1:t){
			sum<-sum+(Y[i,j]-X[i,j])^2
		}
	}
	return (sum)
}


test <- function(t) {
	res = matrix(0, nrow = 25, ncol = 1)
	for(i in 0:4)
		for(j in 1:5)
		res[i * 5 + j] = erreur(theta[i + 1], theta[j], t)
		
	return (res)
}

res = test(4)
scatterplot3d(rep(theta1, each = 5), rep(theta2, 5), res, xlab = "theta1", ylab = "theta2", zlab = "erreur")