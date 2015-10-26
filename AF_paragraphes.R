# Fonction Anne Frank en paragraphes sans les ajouts et les matins / soirs


require(stringr)



AF_paragraphes <- function(journal01) {



# Extraire les jours


	journal02 <- str_replace(journal01,"FEVRIER","FÉVRIER")
	journal02 <- str_replace(journal02,"AOUT","AOÛT")
	journal02 <- str_replace(journal02,"DECEMBRE","DÉCEMBRE")

	n <- length(journal02)

	jours <- c("JANVIER","FÉVRIER","MARS","AVRIL","MAI","JUIN","JUILLET","AOÛT","SEPTEMBRE","OCTOBRE","NOVEMBRE","DÉCEMBRE")
	jours2 <- c(0,0)

	for (i in 1:12) {

		c <- str_locate(journal02,jours[i])
		c[,2] <- seq(1,n,1)
		jours2 <- rbind(jours2,c)
	
	}

	jours3 <- subset(jours2, !is.na(jours2[,1]))
	jours3 <- jours3[order(jours3[,2], decreasing=FALSE),]

	jours4 <- journal02[jours3[,2]]


# Corriger les jours et identifier les ajouts

	a <- str_locate(jours4, "ajout")
	Ajout <- str_sub(jours4,a[,1],a[,2])

	a <- str_locate(jours4, "SOIR")
	Soir <- str_sub(jours4,a[,1],a[,2])

	a <- str_locate(jours4, "MATIN")
	Matin <- str_sub(jours4,a[,1],a[,2])

	Notes <- paste(Ajout,Soir,Matin, sep="")
	Notes <- str_replace_all(Notes,"NA","")

	jours4 <- str_replace(jours4,"SEPTEMBRE 1942 ","28 SEPTEMBRE 1942 ")
	jours4 <- str_replace(jours4,"28 28 ","28 ")
	jours4 <- str_replace(jours4,"21 28 ","21 ")


	jours4 <- str_trim(str_replace_all(jours4,"[(]ajout[)]",""))
	jours5 <- c("LUNDI","MARDI","MERCREDI","JEUDI","VENDREDI","SAMEDI","DIMANCHE")
	for (i in 1:7) {jours4 <- str_replace(jours4,jours5[i],"")}
	jours4 <- str_replace(jours4,"SOIR","")
	jours4 <- str_replace(jours4,"MATIN","")
	jours4 <- str_replace(jours4,"er","")
	jours4 <- str_replace(jours4,"1943 I","1943")
	jours4 <- str_replace(jours4,"194.1","1943")
	jours4 <- str_replace(jours4,"1444","1944")
	jours4 <- str_replace(jours4,"1941","1943")
	jours4 <- str_replace(jours4,"15 FÉVRIER","15 FÉVRIER 1944")
	jours4 <- str_trim(jours4)

	Day <- as.Date(jours4, "%d %B %Y")
	Id_jour <- as.character(Day)
	Jour <- format(Day, "%A %d %B %Y")
	
 
	Infos <- cbind(Id_jour,Jour,Notes)
	

# Reformater le journal avec Infos

	n1 <- length(jours3[,2])
	id1 <- jours3[,2][2:n1]+1
	n2 <- length(id1)
	id2 <- jours3[,2][3:n1]-1
	id2[n2] <- n

	rep_n <- (id2-id1+2)
		
	Id_day <- rep(Day[1],rep_n[1])
	Id_notes <- rep(Notes[1],rep_n[1])


	for (i in 2:n2) {
		Id_day <- c(Id_day, rep(Day[i],rep_n[i]))
		Id_notes <- c(Id_notes, rep(Notes[i],rep_n[i]))

	}


	Id_row <- seq(1,length(journal01),1)
	Id_jour <- as.character(Id_day)
	Texte <- as.character(str_trim(journal01))

	journal03 <- cbind(Id_row, Id_jour,  Id_notes, Texte)

	Journal <- subset(journal03, !(as.numeric(Id_row) %in% jours3[,2]), select=c("Id_jour", "Id_notes","Texte"))


# Paragraphes


	# Eliminer les ajouts pour commencer

	j00 <- data.frame(Journal)
	j01 <- subset(j00, Id_notes!="ajout", select=c("Id_jour", "Texte"))


	# Mettre un identifiant pour les paragraphes

	j01$n <- 1
	Paragraphes <- aggregate(j01$n,by=list(Id_jour=j01$Id_jour),FUN=sum)


	np <- dim(Paragraphes)[1]
	id_p <- seq(1, Paragraphes$x[1],1)

	
	for (i in 2:np) {
	
		p1 <- seq(1, Paragraphes$x[i],1)
		id_p <- c(id_p, p1)

	}

	j02 <- cbind(id_p, j01)

	j03 <- j02[,c(2,1,3)]
	colnames(j03)[2] <- "Id_paragraphe"

return(j03)

}
