#Q3(b)
#lire le jeu de donnes projet_stat.txt 
read<-function() {
	mytable <- read.table("projet_stat.txt", header = TRUE, col.name = c("ID", "t", "Y.1", "Y.2", "Y.3", "Y.4", "Y.5"), sep=" ")
	return (mytable[, -1])
}

table<-read()
print(table)