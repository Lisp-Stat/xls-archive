proj.default <- function(object, onedf = T)
{
	if(!is.qr(object$qr))
		stop("Argument does not include a qr component")
	if(is.null(object$effects))
		stop("Argument does not include an effects component")
	RB <- c(object$effects[seq(object$rank)], rep(0, nrow(object$
		qr$qr) - object$rank))
	prj <- qr.Q(object$qr, Dvec = RB)
	DN <- dimnames(object$qr$qr)
	dimnames(prj) <- list(DN[[1]], DN[[2]][seq(ncol(prj))])
	prj
}
