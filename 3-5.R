theta<-c(0.05961,0.02880,0.02122,0.31620,0.04660)
Y = read()

rescal<-apinene_modele_prediction(t,theta)
rescalx1<-rescal[,1]
rescalx2<-rescal[,2]
rescalx3<-rescal[,3]
rescalx4<-rescal[,4]
rescalx5<-rescal[,5]
resdony1<-Y[,2]
resdony2<-Y[,3]
resdony3<-Y[,4]
resdony4<-Y[,5]
resdony5<-Y[,6]

par(mfrow=c(2,3))

plot(t,rescalx1,type="l",col="red",main="X1")
lines(t,resdony1,col="green")

plot(t,rescalx2,type="l",col="red",main="X2")
lines(t,resdony2,col="green")

plot(t,rescalx3,type="l",col="red",main="X3")
lines(t,resdony3,col="green")

plot(t,rescalx4,type="l",col="red",main="X4")
lines(t,resdony4,col="green")

plot(t,rescalx5,type="l",col="red",main="X5")
lines(t,resdony5,col="green")