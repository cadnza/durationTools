# Loop progress reporter ----
reportLoop <- function(
	x,
	max,
	waitSeconds=0,
	label=NA,
	includePB=TRUE
){
	# Set function to get all variables from global environment
	la <- function()
		ls(envir=.GlobalEnv,all.names=TRUE)
	# Set global trackers
	varPbIdString <- ".reportLoopDurationToolsWorkingProgressBarId"
	varPbMaxVal <- ".reportLoopDurationToolsMostRecentMaxValue"
	# Remove progress bar and global trackers if current x is lower than previous
	if(varPbMaxVal%in%la())
		if(x<get(varPbMaxVal,envir=.GlobalEnv))
			if(varPbIdString%in%la()){
				cli::cli_progress_done(id=varPbIdString)
				rm(list=varPbIdString,envir=.GlobalEnv)
				rm(list=varPbMaxVal,envir=.GlobalEnv)
			}
	# Assign x global tracker to current x
	assign(varPbMaxVal,x,envir=.GlobalEnv)
	# Set wait seconds
	options(cli.progress_show_after=waitSeconds)
	# Get expression to update progress bar
	updatePB <- expression(
		cli::cli_progress_update(
			set=x,
			id=get(varPbIdString,envir=.GlobalEnv)
		)
	)
	# Translate label into cli's format
	if(is.na(label))
		label=NULL
	# Assign global ID tracker to ID of new progress bar if not found
	if(!varPbIdString%in%la())
		assign(
			varPbIdString,
			value=cli::cli_progress_bar(
				name=label,
				total=max,
				type=ifelse(includePB,"iterator","tasks"),
				auto_terminate=TRUE,
				current=FALSE,
				.envir=.GlobalEnv
			),
			envir=.GlobalEnv
		)
	# Update progress bar
	eval(updatePB)
	# Remove global trackers upon completion
	if(x==max){
		if(varPbIdString%in%la()) rm(list=varPbIdString,envir=.GlobalEnv)
		if(varPbMaxVal%in%la()) rm(list=varPbMaxVal,envir=.GlobalEnv)
	}
}
