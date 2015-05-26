#Q3(a)
library(Matrix)
library(expm)

#c'est une fonction auxiliaire qui est pour effectuer operation de chaque ligne
fun<-function(X){
x0<-c(100,0,0,0,0)
	return (X*x0)
}

#cette fonction prend en argument un vecteur de temps d'observation T et un vecteur de parametres theta
#renvoye la valeur de X(theta) qui represente la masse de chaque concentration a ti 
apinene_modele_prediction<-function(T,theta){
	#calcule la matrice theta
	A<-matrix(c((-theta[1]-theta[2]),0,0,0,0,theta[1],0,0,0,0,theta[2],0,(-theta[3]-theta[4]),0,theta[5],0,0,theta[3],0,0,0,0,theta[4],0,-theta[5]), ncol=5,byrow=TRUE)
	l<-length(T)
	res<-matrix(0,nrow=l,ncol=5)
	res[1,]<-c(100,0,0,0,0)
	for(i in 2:l){
		t=T[i]
		X<-expm(A*t)                   # X = x0 * exp(A(theta)*t)
		tmp<-apply(X,1,fun)
		tmp<-t(tmp)
		res[i,]<-apply(tmp,1,sum)
	}
	return (res)
}

#les donnee pour tester cette fonction
t1<-c(1,2,3,4)
theta_1<-c(0.06,0.03,0.0205,0.275,0.04)
apinene_modele_prediction(t1,theta_1)