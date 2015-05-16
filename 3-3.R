Y<-matrix(c(91,83,77,70,6,11,15,20,2,4,5,6,0.03,0.10,0.19,0.31,0.36,1.25,2.48,3.88),ncol=5)
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

min_erreur<-function(t){
	min<-erreur(0.04,0.01,t)
	min_theta1<-0.04
	min_theta2<-0.01
	for(theta1 in seq(0.04,0.08,0.01)){
		for(theta2 in seq(0.01,0.05,0.01)){
			if(min>erreur(theta1,theta2,t)){
				min<-erreur(theta1,theta2,t)
				min_theta1<-theta1
				min_theta2<-theta2
			}
		}
	}
	theta<-c(min_theta1,min_theta2)
	print(min)
	print(theta)
	return (theta)
}

#min_erreur(4)