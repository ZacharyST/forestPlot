'''
The purpose of this script is to define an R function that makes pretty forest plots based on regression results.
Function arguments:
	1. model = model results
	2. coefficients = character vector of coefficients to keep.  Easiest to pass as names(model$coefficients)
	3. coef_labels = labels for y-axis on plot
	4. se = user can pass custom standard errors
	5. zscore = how user defines width of confidence intervals
	6. outfile = name, including filepath, where the output will be saved.  Must end in .pdf
	7. cex = magnification for text

NB: This plotting works well when the coefficients have similar values.  If they are far apart, does not work.
'''
forestPlot <- function(model=NULL, coefficients=NULL, coef_labels=NULL, se = NULL, zscore = 1.96, outpath, cex){
	coef <- model$coefficients  # Get coefficients from model

	if(is.null(coefficients) == TRUE){  # Generate coefficie
		coefficients <- names(model$coefficients)
	}

	coef <- coef[coefficients]  # Keep user specific coefficients
	coef <- as.numeric(coef)

	if(is.null(se) == TRUE){  # If no standard errors given, take from model
		stdev <- summary(model)$coefficients[,2]  # Get coefficient standard deviations
		stdev <- stdev[coefficients]
		stdev <- as.numeric(stdev)
	}

	if(is.null(se) == FALSE){  # If standard errors given, use those
		stdev <- sd
		stdev <- stdev[coefficients]
		stdev <- as.numeric(stdev)
	}

	lower <- coef - zscore*stdev  # Lower value of confidence interval
	upper <- coef + zscore*stdev  # Upper value of confidence interval

	vars <- length(coef)  # Will be used to create y values

	if(is.null(coef_labels) == TRUE){
		coef_labels <- coefficients
	}

	# Define x minimum to plot y-axis labels
	label_pos <- min(lower)*-1.05


	#Plot all results
	pdf(outpath)
	par(mar=c(5,10,3,1)) # Need more space on left side of plot (6) for var. labels
	plot(x=coef, y=vars:1, xlim=c(min(lower)*1.1,max(upper)*1.5), pch = 20, xlab='Coefficient', bty='n', ylab='', yaxt='n', xaxt='n', cex.lab = cex)
	for(i in 1:vars){
	    lines(x=c(lower[i], upper[i]), y = rep(vars+1-i, each=2), lwd=2)
	}
	axis(1, cex.axis=cex)
	axis(2, at=vars:1, labels=coef_labels, las=1, lwd=0, pos=-label_pos, outer=TRUE, cex.axis=cex) # pos is x-axis location of labels
	abline(v=0, lty=2)
	dev.off()
}
