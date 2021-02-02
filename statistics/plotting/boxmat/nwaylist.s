nwaylist <- function(formula, data = sys.parent())
{
#on.exit(browser())
	t.f <- myterms(formula, data=data, resequence=F)
	term.l <- attr(t.f, "term.labels")
	respname <- attr(t.f, "formula")[2][[1]]
	if (!missing(data) && exists(as.name(respname),where=data))
		resp <- get(respname, where=data)
	else	resp <- eval(respname)
	get.index <- function(term, data) {
		t.v <- attr(terms(paste("~",term)),"variables")
		if (missing(data)) interaction(lapply(t.v, eval))
		else interaction(data[as.character(t.v)])
	}
	if (missing(data)) indices <- lapply(term.l, get.index)
	else indices <- lapply(term.l, get.index, data)
	result <- tapply(resp, indices, c)
	if (!is.null(dim(result))) names(dimnames(result)) <- term.l
#on.exit()
	result
}



cell.apply <- function(formula, FUN.scalar=mean, ..., data=sys.parent(1)) {
	if (missing(data)) cells <- nwaylist(formula)
	else 		   cells <- nwaylist(formula, data)
	result <- sapply(cells, FUN.scalar, ...)
	dim(result) <- dim(cells)
	dimnames(result) <- dimnames(cells)
	result
}


# myterms is identical to terms,
#	except for the resequence argument
#	and the pivot result.
#	The result with the default resequence=T is
#	functionally identical to terms.

# > tmpT <- myterms(y ~ a:b + c)
# > as.expression(tmpT)
# expression(c, a:b)
# > as.expression(tmpT)[attr(tmpT,"pivot")]
# expression(a:b, c)

# > tmpF <- myterms(y ~ a:b + c, res=F)
# > as.expression(tmpF)
# expression(a:b, c)
# > as.expression(tmpF)[attr(tmpF,"pivot")]
# expression(a:b, c)

myterms <-
function(formula, specials = NULL, abb = NULL, data = list(), neg.out = T,
	resequence = T)
{
	if(inherits(formula, "terms"))
		return(formula)
	if(!is.null(Terms <- formula$terms) && inherits(Terms, "terms"))
		return(Terms)
	formula <- as.formula(formula)
	if(length(data))
		formula <- replace.(formula, data)
	if(!is.null(abb)) {
		a <- names(abb)
		if(is.null(a))
			stop("invalid abb attribute")
		formula <- .C("un_abbrev",
			list(formula),
			as.character(abb),
			as.character(a),
			as.integer(min(length(a), length(abb))))[[1]][[1]]
	}
	ttt <- .C("terms_pass1",
		e = list(formula),
		t = as.integer(1),
		v = as.integer(0),
		r = integer(1))
	nt <- ttt$t
	nv <- ttt$v
	vv <- as.expression(1:nv)
	resp <- ttt$r
	os <- c("offset", "^")
	if(length(specials))
		os <- c(os, specials)
	ttt <- .C("terms_pass2",
		e = (ttt$e)[[1]],
		as.integer(nt),
		as.integer(nv),
		factors = integer(nt * nv),
		integer(nt),
		ord = integer(nt),
		var = vv,
		lab = character(nt),
		int = integer(1),
		as.character(os),
		as.integer(length(os)),
		specials = integer(nt),
		as.integer(resp),
		neg = as.integer(neg.out))
	had.minus <- ttt$neg
	s <- ttt$specials
	if(any(s == 2)) {
		v <- ttt$e
		hats <- seq(along = v)[s == 2]
		hats <- hats[hats != resp]
		if(length(hats))
			return(expand.hat(v, hats, specials, data, ttt$lab,
				formula, resp))
	}
	ord <- ttt$o
	factors <- ttt$factors
	lab <- ttt$lab
	int <- ttt$int
	o <- order(ord)
	if((ndrop <- sum(if(neg.out) ord <= 0 else ord == 0)) > 0)
		o <- o[ - (1:ndrop)]
	if(any(s == 1)) {
		off <- seq(along = s)[s == 1]
		o <- o[o != off]
	}
	else off <- NULL
	if(resp)
		o <- o[o != resp]
	v <- ttt$e[o]
	l <- ttt$l
	if(nt && nv) {
		dim(factors) <- c(nv, nt)
		dimnames(factors) <- list(as.character(ttt$var), lab)
		factors <- factors[, o, drop = F]
	}
	a <- list(formula = formula, factors = factors, order = ord[o], 
		variables = ttt$var, term.labels = lab[o], intercept = int,
		response = resp, offset = off, pivot=o-resp)
	if(any(s > 2)) {
		so <- s[o] - 2
		n <- seq(along = v)
		sp <- as.list(specials)
		names(sp) <- specials
		for(i in 1:length(specials))
			sp[[i]] <- n[so == i]
		a$specials <- sp
	}
	pivot <- a$pivot
	if (!resequence && !is.null(pivot)) {
		unpivot <- sort.list(pivot)
		v[] <- v[unpivot]
		a$factors <- a$factors[,unpivot,drop=F]
		a$order <- a$order[unpivot]
		a$term.labels <- a$term.labels[unpivot]
		a$pivot <- a$pivot[unpivot]
	}
	attributes(v) <- a
	class(v) <- "terms"
	v
}
