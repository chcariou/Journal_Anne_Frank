# Fonction Anne Frank par phrases
# Par Christophe Cariou, Octobre 2015

# Entrée : Journal comporte deux colonnes : Id_jour, Id_paragraphe, Texte
# Ma fonction pour découper les phrases comme je le souhaite.
# Entrée : équivalent paragraphe par paragraphe


require(stringr)


AF_phrases <- function(Journal) {


	Par_phrase <- c("Id_jour","Id_paragraphe","Id_phrase","Phrase")

	n <- dim(Journal)[1]

	for (i in 1:n) {

		#print(i)
		Texte <- Journal$Texte[i]

		Prenoms <- LETTERS
		Prenoms2 <- paste(Prenoms,".",sep="")
		Prenoms3 <- paste(Prenoms,"@",sep="")
		
		Texte2 <- Texte	
		for (j in 1:26) { 

			Texte2 <- str_replace_all(Texte2,fixed(Prenoms2[j]),Prenoms3[j])
			n2 <- nchar(Texte2)
			a <- str_sub(Texte2, n2, n2)
			if (a=="@") { Texte2 <- paste(str_sub(Texte2,1,(n2-1)),".",sep="")}

		}
	

		# Identifier la localisation des signes de fin de phrases.

			Ending_punctuation <- str_locate_all(Texte2,"[.!?…]")[[1]][,2]

		if (length(Ending_punctuation)>1) {

			Alias <- seq(1,length(Ending_punctuation),1)
			Ending_or_not <- data.frame(cbind(Alias, Ending_punctuation))

		# Regarder si (1) il y a un espace après et (2) il y a une majuscule après l'espace.

			After <- str_sub(Texte2, Ending_punctuation+1, Ending_punctuation+5)

		# Etape 1 : éliminer les guillets et des parenthèses

			Texte1 <- str_trim(str_replace_all(After, "[«»)(]",""))
			#Texte1 <- str_trim(str_replace_all(Texte1, fixed(" »"),""))

		# Etape 2 : il doit y avoir des lettres majuscules après l'espace

			Id_ending <- str_locate(str_sub(Texte1,1,1), "[A-Z]")[,2]/str_locate(Texte1, "[A-Z]")[,2]
			
		# Etape 3 : la dernière phrase

			Id_ending[length(Ending_punctuation)] <- 1
		
		# Etape 4 : Refusionner

			Ending_or_not$Ending_punctuation <- Ending_or_not$Ending_punctuation*Id_ending

		# Etape 5 : séparer les phrases

			To <- subset(Ending_or_not, !is.na(Ending_punctuation))$Ending_punctuation+1
			From <- c(1,To[1:(length(To)-1)]+1)
			if (length(To)==1) { From <- 1 }
			Phrases <- str_trim(str_sub(Texte,From,To))

		# Etape 6 : vérifier le bug des guillements
			
			np <- length(Phrases)

			for (g in 1:np) {
				
				car1 <- str_sub(Phrases[g],1,1)
				if (car1=="»") { 
					str_sub(Phrases[g],1,2) <- ""
					Phrases[g-1] <- paste(Phrases[g-1],"»",sep=" ")
				}


			}


		# Mettre un identifiant et la table

			np <- length(Phrases)
			Id_phrase <- seq(1,np,1)

			Id_jour <- rep(as.character(Journal$Id_jour[i]), np)
			Id_paragraphe <- rep(as.character(Journal$Id_paragraphe[i]), np)
		
		}else{

			Id_jour <- as.character(Journal$Id_jour[i])
			Id_paragraphe <- as.character(Journal$Id_paragraphe[i])

			if (length(Ending_punctuation)==1) { Id_phrase <- 1} else { Id_phrase <- 0} 



			Phrases <- as.character(Texte)



		}

			Final <- cbind(Id_jour,Id_paragraphe , Id_phrase, Phrases)
			Par_phrase <- rbind(Par_phrase, Final)
			
	}

Par_phrase <- data.frame(Par_phrase[-1,])

return(Par_phrase)
}

