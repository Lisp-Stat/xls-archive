
bekk.y <- matrix(
c( 6.5,  12.9,	38.7, 166.3, 125.9,
   6.5,  12.5,	47.7, 151.8, 113.5,
   6.6,  12.8,	44.5, 141.0, 123.4,
   8.1,  11.0,	45.5, 149.4, 174.0,
   6.5,   9.3,	41.5, 151.7, 130.1,
   6.5,  13.7,	46.0, 166.1, 158.2,
   6.8,  13.1,	43.8, 148.6, 144.5,
   6.5,  14.0,	58.0, 158.3, 180.0,
   6.0,  13.2,	42.9, 178.5, 196.7,
   6.0,  14.2,	39.3, 183.3, 230.8,
   6.1,  12.2,	39.7, 150.8, 146.0,
   5.8,  13.1,	45.8, 145.0, 247.3,
   5.7,   9.9,	43.3, 162.9, 183.7,
   5.7,   9.6,	41.4, 184.1, 237.2,
   5.7,  12.6,	41.8, 160.5, 229.3,
   6.0,   9.0,	39.2, 170.8, 185.9,
   6.0,  11.0,	39.0, 150.0, 150.0,
   5.0,  10.0,	44.0, 150.0, 165.0,
   6.0,  12.0,	43.0, 150.0, 172.0,
   6.0,  13.0,	43.0, 160.0, 162.0,
   6.0,  11.0,	40.0, 150.0, 170.0,
   6.0,  10.0,	35.0, 160.0, 150.0,
   6.0,  11.0,	34.0, 182.0, 158.0,
   6.0,  12.0,	45.0, 140.0, 198.0,
   5.0,  11.4,	35.4, 121.8, 123.8,
   5.0,  10.0,	37.2, 127.4, 162.0,
   4.9,   8.8,	34.8, 145.0, 128.4,
   4.8,   8.2,	41.2, 162.4, 153.0,
   4.6,  10.0,	42.6, 122.2, 164.4,
   4.5,   8.4,	37.8, 124.0, 140.0,
   4.8,  10.0,	34.8, 110.2, 130.2,
   4.3,  12.6,	34.0, 141.2, 198.8),
nrow=32, byrow=T)

bekk <- data.frame(y=as.vector(bekk.y), log.y=as.vector(log10(bekk.y)),
   lab=factor(as.vector(((row(bekk.y)-1) %/% 8)+1)),
   material=factor(as.vector(col(bekk.y))))

bekk.aov <- aov(log.y ~ lab*material, proj=T, x=T, data=bekk)

print(summary(bekk.aov))

bp <- proj(bekk.aov)


par(mfrow=c(2,3))

boxmat(y ~ lab+material, data=bekk,
	main="Figure 1.  y",
	mar = c(4.1, 9.1, 4.1, 6.1), ylim=c(0,280), yticks=c(0,250))

cell.mean <- as.vector(cell.apply(y~lab+material, data=bekk, mean))
cell.stdev <- as.vector(cell.apply(y~lab+material, data=bekk, function(x) sqrt(var(x))))
s.v.l <- lsfit(x=log10(cell.mean), y=log10(cell.stdev))
print(s.v.l$coef)
plot(x=log10(cell.mean), y=log10(cell.stdev),
	main="Figure 2. Spread-vs-Level",
	xlab="level", ylab="")
mtext("spread", side=2, line=5)
abline(s.v.l$coef)



par(mfrow=c(2,3))

boxmat(log.y ~ lab+material, data=bekk,
	main="Figure 3(a).  log(y)",
	mar = c(6.1, 8.1, 4, 6.1), ylim=c(.5,2.5), yticks=c(.6,2.4) )

treatment <- bekk$log.y-bp[,"Residuals"]
boxmat(treatment ~ lab+material, data=bekk,
	main="Figure 3(b).  Treatment effect",
	mar = c(6.1, 8.1, 4, 6.1), ylim=c(.5,2.5), yticks=c(.6,2.4) )

boxmat(bp[,"Residuals"] ~ lab+material, data=bekk,
	main="Figure 3(c).  Residuals",
	mar = c(6.1, 8.1, 4, 6.1),
	ylim=c(-.16,.16), yticks=c(-.14,0,.14), ytick.labels=c("-.14","",".14"))

boxmat(bp[,"lab"] ~ lab+material, data=bekk,
	main="Figure 3(d).  Laboratory effect",
	mar = c(6.1, 8.1, 4, 6.1), ylim=c(-.07,.05), yticks=c(-.06,.04))

boxmat(bp[,"material"] ~ lab+material, data=bekk,
	main="Figure 3(e).  Material effect",
	mar = c(6.1, 8.1, 4, 6.1), ylim=c(-.9,.75), yticks=c(-.80,.65))

boxmat(bp[,"lab:material"] ~ lab+material, data=bekk,
	main="Figure 3(f).  lab:material interaction",
	mar = c(6.1, 8.1, 4, 6.1), ylim=c(-.09,.08), yticks=c(-.08,.07))




par(mfrow=c(2,1), mar = c(5.1, 6.1, 4.1, 2.1))

LSS <- fac.design(c(2,2,2,2),
	list(L=c(0,.4428),M=c(0,7.4033),I=c(0,.3732),R=c(0,.57825)))

row.names(LSS) <- c(
 "0", "L", "M", "L+M",
 "L:M", "L+L:M", "M+L:M", "L+M+L:M",
 "Res", "L+Res", "M+Res", "L+M+Res",
 "L:M+Res", "L+L:M+Res", "M+L:M+Res", "L+M+L:M+Res")

cyc <- c(1,2,4,3,1,5,6,8,7,5,1,NA,2,6,NA,4,8,NA,3,7)
cyc1 <- c(1,2,4,3,1,NA,2,6,8,4,NA,3,7,8)
cyc2 <- c(1,5,6,NA,5,7)
cyc3 <- c(1,8)

fitres <- as.matrix(LSS) %*% cbind(LMI=c(1,1,1,0),R=c(0,0,0,1))
plot(fitres[c(1,8,16,1),],xaxs="e",yaxs="e",type="l",bty="l",
	main="Figure 6(a).  Treatment + Residual",
	xlab="Treatment", ylab="")
text(fitres[c(1,8,16),], labels=dimnames(fitres)[[1]][c(1,8,16)])
mtext("Residual", side=2, line=5)

cube2 <- as.matrix(LSS) %*% cbind(L=c(0,1,8,0),I=c(1,0,1,0))
plot(cube2[cyc1,],type="l",xaxs="e",yaxs="e",lty=1,bty="l",
	main="Figure 6(b).  Main effects and Interaction",
	xlab="material", ylab="")
lines(cube2[cyc2,],lty=2)
lines(cube2[cyc3,],lty=3,lwd=4)
text(cube2[1:8,],labels=dimnames(cube2)[[1]][1:8])
mtext("lab", side=2, line=5)

origin <- cube2["0",,drop=F]
LM <- cube2["L:M",,drop=F]
intersect <- cube2["L+L:M",2]/cube2["L:M",2]
LMint <- intersect*LM
LMout <- 2.5*LM
lines(rbind(LM,LMint),lty=2)
lines(rbind(LMint,LMout),lty=5)
text(LMout, labels="lab:material", adj=0)
