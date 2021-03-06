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

# Loop progress reporter ----
reportLoop <- function(
	x,
	max,
	label=NA,
	reportInterval=5,
	maxWidth=80,
	includePB=TRUE,
	progressChar="="
){
	resetTrackers <- function(){
		.GlobalEnv$reportLoopTracker <- x
		.GlobalEnv$reportLoopTimeStart <- Sys.time()
	}
	tryCatch(
		{
			reportLoopTracker
			reportLoopTimeStart
		},
		error=function(x){
			resetTrackers()
		}
	)
	if(reportLoopTracker>x)
		resetTrackers()
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
		lineReport <- paste0(
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
		endChar <- "|"
		nEndChars <- 2
		minProgChars <- 4
		minSpaces <- 1
		maxSpaces <- 4
		maxLengthForProg <- maxWidth-minProgChars-nEndChars-minSpaces
		if(nchar(lineReport)<=maxLengthForProg&includePB){
			lineReport <- paste0(
				c(
					lineReport,
					rep(
						" ",
						min(
							maxWidth-nchar(lineReport)-minProgChars-nEndChars,
							maxSpaces
						)
					)
				),
				collapse=""
			)
			totalProgChars <- maxWidth-nchar(lineReport)-nEndChars
			progs1 <- floor(totalProgChars*x/max)
			progs0 <- totalProgChars-progs1
			lineReport <- paste0(
				lineReport,
				endChar,
				paste(rep(progressChar,progs1),collapse=""),
				paste(rep(" ",progs0),collapse=""),
				endChar
			)
		}
		lineReport <- substr(lineReport,1,maxWidth)
		message(lineReport)
	}
	if(x==max){
		rm(reportLoopTracker,envir=.GlobalEnv)
		rm(reportLoopTimeStart,envir=.GlobalEnv)
	}
}
