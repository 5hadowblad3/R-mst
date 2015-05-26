X = c()
for (i in 1:5) {
	X = c(X, t(rescal[, i]))
}

G<-abs(y-X)
G<-log(G)
gaussien<-shapiro.test(G)
print(gaussien)