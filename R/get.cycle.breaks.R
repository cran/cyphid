get.cycle.breaks <-
function(dataset, window, rowLimit=NULL){
	# Get number of observations (i.e, frames) in each sequence
	nobs <- dim(dataset)[1]
	# Get max gape locations in the sequence
	CycleIndex <- msExtrema(dataset,span=window)$index.min
	frames <- seq(1,nobs)
	if(is.null(rowLimit)){
		limits <- NULL
		for(i in 1:dim(dataset)[2]){
		breaks <- frames[CycleIndex[,i]]
		limits <- append(limits, length(breaks))
			}
		rowLimit <- max(limits)
		}
	CycleBreaks <- NULL
	for(i in 1:dim(dataset)[2]){
		breaks <- frames[CycleIndex[,i]]
		CycleBreaks <- add.col(CycleBreaks, breaks, rowLimit)
		}
	return(list(CycleBreaks=CycleBreaks, CycleIndex=CycleIndex))
	}

