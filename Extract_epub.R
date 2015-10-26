# Chargement des librairies

	require(rvest) # pour lire au format texte les fichiers xhtml
	require(stringr) # pour restructurer le format texte

Extract_Anne_Frank <- function() {

# Récupération du Journal d'Anne Frank mis en ligne par Affordance.info au format ePub
# Un fichier ePub n'est autre qu'un dossier zip qui contient des fichiers css, html, jpg...

	affordance <- "http://www.affordance.info/files/uncensored-le_journal_danne_frank_frank_anne.epub"
	temp <- tempfile() # création d'un fichier temporaire
	download.file(affordance, temp) # chargement du fichier dans un dossier temporaire
	unzip(temp, list=TRUE) # liste des fichiers dézippés

	journal <- "Journal d'Anne Frank\r\r" # initialisation pour stocker le journal

	for (f in 4:8) {

		file <- paste("OEBPS/text/content000",f,".xhtml",sep="") # liste des fichiers du journal sans préface / épilogue
		xhtml <- unzip(temp,files=file) # récupération du fichier souhaité
		txt <- html_text(html(xhtml)) # tranforme le fichier en format texte
		journal <- paste(journal,txt,sep="\r\r") # ne faire qu'un seul texte

	}

	unlink(temp) # casser le lien avec le fichier temporaire pour vider la mémoire


# Le journal : les séparations se font au niveau des paragraphes par "\r"

	journal01 <- str_trim(str_split(journal, "[\r]")[[1]])
	journal02 <- subset(journal01, journal01 != "")
	n02 <- length(journal02)
	journal03 <- journal02[c(2:5,7:(n02-1))] # éliminer la ligne d'initialisation (1), la phrase sur les "ajouts" (6) et la fin "ici se termine le journal" (n02)
	journal03[3] <- str_replace(journal03[3], fixed(" (1)"),"") # éliminer l'appel à la phrase "ajout"

# Le journal d'Anne Frank version non censurée avec ajouts : 1 ligne équivaut à un paragraphe / 1 retour à la ligne

	Journal <- journal03


return(Journal)

}
