read<-function() {
	mytable<-read.table("projet_stat.txt", header=FALSE, sep=" ", dec=".", quote="\"")
	t <- mytable[1,]
	y <- mytable[-1, ]
	return (list(t, y))
}

