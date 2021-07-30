# Loop progress reporter ----
reportLoop <- function(
	x,
	max,
	waitSeconds=0,
	label=NA,
	includePB=TRUE
){
	pbIdString <- ".reportLoopDurationToolsWorkingProgressBarId"
	options(cli.progress_show_after=waitSeconds)
	updatePB <- expression(
		cli::cli_progress_update(
			set=x,
			id=get(pbIdString,envir=.GlobalEnv)
		)
	)
	tryCatch(
		eval(updatePB),
		error=function(e){
			assign(
				pbIdString,
				value=cli::cli_progress_bar(
					name=ifelse(is.na(label),NULL,label),
					total=max,
					type=ifelse(includePB,"iterator","tasks"),
					auto_terminate=TRUE,
					current=FALSE,
					.envir=.GlobalEnv
				),
				envir=.GlobalEnv
			)
			eval(updatePB)
		}
	)
	if(x==max)
		rm(list=pbIdString,envir=.GlobalEnv)
}
