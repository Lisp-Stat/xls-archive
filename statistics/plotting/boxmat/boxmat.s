boxmat <- function(
	formula,
	data,
	twlist,
	ylim = range(data.ok),
	yticks = ylim,
	ytick.labels = yticks,
	mar = c(5.1, 8.1, 4.1, 2.1),
	row.name.line=as.integer(par("mar")[2])-4,
	row.label.line=as.integer(par("mar")[2])-1,
	main="",
	old = T,
	boxf = boxplot,
	...
)
{
#on.exit(browser())
	if (!missing(formula) && !missing(twlist))
		stop("Only one of 'formula' and 'twlist' may be used.")
	if (missing(twlist)) {
		if (is.null(class(formula)) || class(formula) != "formula")
			twlist <- formula
		else {
			if (missing(data))
				twlist <- nwaylist(formula)
			else
				twlist <- nwaylist(formula, data)
		}
	}
	dim.twlist <- dim(twlist)
	if (is.null(dim.twlist) || (length(dim.twlist)==1)) {
		names.twlist <- names(twlist)
		if (is.null(names.twlist)) names.twlist <- seq(along=twlist)
		names(twlist) <- NULL
		twlist <- matrix(twlist, nrow=1,
			dimnames=list("", names.twlist))
	}
	else if (length(dim.twlist) > 2 ) stop("two way list is required")
	nr <- nrow(twlist)
	nc <- ncol(twlist)
	par(mar = mar)
	if(missing(ylim)) {
		tmp <- unlist(twlist)
		data.ok <- tmp[!is.na(tmp)]
	}
	d.range <- ylim[2]-ylim[1]
	row.incr <- (0:nr) * (d.range*1.1)
	row.starts <- row.incr + (ylim[1]-.05*d.range)
	ylim.ext <- range(row.starts)
	row.centers <- row.incr[nr:1] + mean(ylim)
	fnames <- dimnames(twlist)
	row.names <- fnames[[1]]
	col.names <- fnames[[2]]
	for (i in seq(length(twlist)))
		if (length(twlist[[i]])==0) twlist[[i]] <- NA
	for (i in 1:nr) {
		if (i > 1) {par(new=T); main <- ""}
		boxx <- boxf (
		 lapply(twlist[i,], "+", row.incr[nr+1-i]),
		 names = rep("", nc),
		 bty="n", old=old,
		 yaxs="i", yaxt="n", ylim=ylim.ext,
		 xaxs="i", xaxt="n", main=main,
		 ...
		)
	  }
	abline(h=row.starts)
	abline(v=c(0,100))
	ticks <- as.vector(
		outer(yticks, row.starts[-(nr+1)]-row.starts[1], "+"))
	tick.labels <- rep(ytick.labels, nr)
	axis(2, labels = F, at = ticks, tck = -0.02)
	axis(2, labels = tick.labels, at = ticks, ticks=F, line=1, adj=1)
	axis(4, labels = F, at = ticks, tck = -0.02)
	mtext(col.names, side = 1, line = 1, at = boxx)
	mtext(row.names, side = 2, line = row.name.line, at = row.centers)
	label <- names(fnames)
	if(is.null(label)) label <- rep("", length(fnames))
	mtext(label[1], side = 2, line = row.label.line)
	title(xlab=label[2], line=2)
#on.exit()
	return(invisible(boxx))
}


nwaylist.c0a <- function(x) {
	tmp <- array(as.list(x), dim(x), dimnames(x))
	for (i in seq(along=x)) {
		if(is.na(x[i])) tmp[[i]] <- numeric()
		else tmp[[i]] <- c(0,x[i])
	}
	tmp
}

boxplotm <- function(data, names, ...) {
	if (missing(names)) box1 <- boxplot(data, plot=F, ...)
	else		    box1 <- boxplot(data, names=names, plot=F, ...)
	box1$stats[3,] <- box1$stats[2,]
	invisible(bxp(box1, ...))
}

boxmatm <- function(matrix, ...)
	invisible(boxmat(twlist=nwaylist.c0a(matrix), boxf=boxplotm, ...))
