#Q3(c)
#calculer les erreurs et tracer la graphe en modifiant les valeur theta1 et theta2
library("scatterplot3d")

Y = read()
t = Y[, 1]

y<-matrix(0,ncol=5,nrow=length(t))
for (i in 2:6) {
	y[,i-1] <- Y[, i]
}

#modeliser les intervals des deux variables a vecteurs
theta1 = seq(0.04, 0.08, 0.001)
theta2 = seq(0.01, 0.05, 0.001)

erreur<-function(theta1,theta2){
	theta<-c(theta1,theta2,0.0205,0.275,0.04)
	X<-apinene_modele_prediction(t,theta)
	sum<-0
	T<-length(t)
	for(j in 1:5){
		for(i in 1:T){
			sum<-sum+(y[i,j]-X[i,j])^2
		}
	}
	return (sum)
}


graph_erreur <- function() {
	res <- matrix(0, nrow = 41*41, ncol = 1)
	for(i in 0:40)
		for(j in 1:41)
		  res[i * 41 + j] = erreur(theta1[i + 1], theta2[j])
		
	return (res)
}

res <- graph_erreur()
scatterplot3d(rep(theta1, each = 41), rep(theta2, 41), res, xlab = "theta1", ylab = "theta2", zlab = "erreur")