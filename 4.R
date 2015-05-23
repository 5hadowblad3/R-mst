#Q4
#matrice de variance-covariance
print("la matrice de variance-covariance")
matrice_cov<-cov(rescal,resdon,use="everything")
print(matrice_cov)

#les intervalles de confiance
library("nlstools")
print("les intervalles de confiance de theta")
IC1<-confint2(res,"theta1",level=0.95,method="asymptotic")
print(IC1)
IC2<-confint2(res,"theta2",level=0.95,method="asymptotic")
print(IC2)
IC3<-confint2(res,"theta3",level=0.95,method="asymptotic")
print(IC3)
IC4<-confint2(res,"theta4",level=0.95,method="asymptotic")
print(IC4)
IC5<-confint2(res,"theta5",level=0.95,method="asymptotic")
print(IC5)

#l'etendue relative
eten_rel<-c()
eten_rel[1]<-(IC1[2]-IC1[1])/0.05961
eten_rel[2]<-(IC2[2]-IC2[1])/0.02880
eten_rel[3]<-(IC3[2]-IC3[1])/0.02122
eten_rel[4]<-(IC4[2]-IC4[1])/0.31620
eten_rel[5]<-(IC5[2]-IC5[1])/0.04660
print("etendue relative des intervalles de confiance")
print(eten_rel)
