# Duration formatter ----
formatDuration <- function(durationSecs,padded=FALSE){
	durationSecs <- as.difftime(durationSecs,units="secs")
	hrs <- floor(as.numeric(durationSecs,units="hours"))
	mins <- floor(as.numeric(durationSecs,units="mins")%%60)
	secs <- floor(as.numeric(durationSecs,units="secs")%%(60))
	tFormatted <- paste0(hrs,"h ",mins,"m ",secs,"s")
	if(padded){
		tFormatted <- gsub(" (?=\\d(m|s))","  ",tFormatted,perl=TRUE)
		tFormatted <- paste0(
			paste(rep(" ",2-nchar(stringr::str_match(tFormatted,"\\d+(?=h)")[1,1])),collapse=""),
			tFormatted
		)
	}
	tFormatted <- gsub("(?<!\\d)0(h|m|s)","  ",tFormatted,perl=TRUE)
	if(!padded)
		tFormatted <- trimws(gsub(" +"," ",tFormatted))
	return(tFormatted)
}

# Loop progress reporter ----
reportLoop <- function(x,max,label=NA,reportInterval=5){
	resetTrackers <- function(){
		reportLoopTracker <<- x
		reportLoopTimeStart <<- Sys.time()
	}
	if(!"reportLoopTracker"%in%ls(envir=.GlobalEnv)){
		resetTrackers()
	}else if(reportLoopTracker>x){
		resetTrackers()
	}
	progPct <- round(x/max*100,2)
	progPct <- sprintf("%.2f",progPct)
	targetWidth <- 6
	spaces <- paste0(rep(" ",targetWidth-nchar(progPct)),collapse="")
	progPct <- paste0(spaces,progPct,"%")
	progPct <- gsub("0(?=%)"," ",progPct,perl=TRUE)
	progPct <- gsub("0(?= )"," ",progPct,perl=TRUE)
	progPct <- gsub("\\.(?= )"," ",progPct,perl=TRUE)
	tNow <- Sys.time()
	tElapsed <- tNow-reportLoopTimeStart
	tPer <- tElapsed/x
	tProjected <- tPer*(max-x)
	if(x%%reportInterval==0|x==max){
		cat("\n")
		cat(
			paste0(
				ifelse(
					is.na(label),
					"",
					paste0(label," ")
				),
				paste(
					progPct,
					paste(formatDuration(tProjected,padded=TRUE),"remaining"),
					sep="  "
				)
			)
		)
	}
	if(x==max){
		rm(reportLoopTracker,envir=.GlobalEnv)
		rm(reportLoopTimeStart,envir=.GlobalEnv)
	}
}
