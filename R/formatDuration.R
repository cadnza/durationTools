# Duration formatter ----
formatDuration <- function(durationSecs,padded=FALSE){
	durationSecs <- round(durationSecs)
	isZero <- durationSecs==0
	if(durationSecs<0)
		stop("Please provide a non-negative value for durationSecs.")
	durationSecs <- as.difftime(durationSecs,units="secs")
	hrs <- floor(as.numeric(durationSecs,units="hours"))
	mins <- floor(as.numeric(durationSecs,units="mins")%%60)
	secs <- round(as.numeric(durationSecs,units="secs")%%(60))
	tFormatted <- paste0(hrs,"h ",mins,"m ",secs,"s")
	if(padded){
		tFormatted <- gsub(" (?=\\d(m|s))","  ",tFormatted,perl=TRUE)
		tFormatted <- paste0(
			paste(
				rep(
					" ",
					max(2-nchar(stringr::str_match(tFormatted,"\\d+(?=h)")[1,1]),0)
				),
				collapse=""
			),
			tFormatted
		)
	}
	tFormatted <- gsub("(?<!\\d)0(h|m|s)","  ",tFormatted,perl=TRUE)
	if(!padded)
		tFormatted <- trimws(gsub(" +"," ",tFormatted))
	if(isZero){
		if(padded)
			tFormatted <- gsub("  $","0s",tFormatted)
		else
			tFormatted <- "0s"
	}
	return(tFormatted)
}
