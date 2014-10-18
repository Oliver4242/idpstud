# Buchstaben auf Zahlen mappen
# letters -> 1:26
# rest -> 27
# Keine Umlaute usw
to.numbers <- function(text) {
	text=toupper(text)
	numbers <- 1:nchar(text)
	for (i in 1:nchar(text)) {
		if (substring(text,i,i) %in% toupper(LETTERS)) {
			numbers[i]=which(LETTERS==substring(text,i,i))
		}else{
			numbers[i]=27
		}
	}
	return(numbers)
}

# Zahlen auf Buchtaben mappen
# Umkehrung zu Oben
to.text<- function(numbers) {
	text = ""
	for (i in 1:length(numbers)) {
		if (numbers[i]<27) {
			text = paste(text,LETTERS[numbers[i]],sep="")
		}else{
			text = paste(text,"_",sep="")
		}
	}
	return(text)
}








