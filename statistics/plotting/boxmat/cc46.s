
cc46.raw <- matrix(c(
0, 1, 269, 466,  2, 5, 283, 280,  1, 2, 252, 398,  1, 4, 212, 386,
1, 3, 138, 194,  0, 1, 100, 219,  0, 1, 197, 421,  2, 4, 263, 379,
2, 3, 282, 372,  1, 5, 230, 256,  0, 1, 216, 708,  2, 2, 145, 304,
2, 4,  95, 199,  2, 3, 127, 166,  2, 5,  80, 142,  0, 1, 134, 590,
1, 5, 107, 236,  1, 2,	89, 332,  1, 4,  41, 176,  0, 1,  74, 137,
0, 1,  88, 356,  0, 1,	25, 212,  2, 2,  42, 308,  1, 3,  62, 221,
1, 5, 124, 268,  0, 1, 211, 505,  1, 3, 194, 433,  2, 5, 222, 408,
0, 1, 102, 363,  2, 2, 193, 561,  2, 3, 128, 311,  1, 2,  42, 222,
2, 4, 162, 365,  0, 1, 191, 563,  1, 4, 107, 415,  0, 1,  67, 338,
2, 5, 193, 292,  0, 1, 209, 352,  1, 5, 109, 132,  1, 4, 153, 454,
0, 1,  29, 254,  2, 2,	 9,  92,  2, 3,  17,  28,  0, 1,  19, 106,
1, 3,  23,  80,  1, 2,	19, 114,  0, 1,  44, 268,  2, 4,  48, 298),
nrow=48, byrow=T,
dimnames = list(NULL, c("dose", "fumigant", "first.count", "second.count")))

cc46 <- data.frame(
 cc46.raw[,3:4],
 dose=factor(cc46.raw[,1]),
 fumigant=factor(cc46.raw[,2], labels=c("none","cn","cs","cm","ck")),
 ctl.vs.trt=factor(cc46.raw[,1]==0, levels=c(T,F), labels=c("not.fum","fum")),
 block=factor(rep(1:4,c(12,12,12,12))),
 dose.lin  =  (cc46.raw[,1]==2) - 4*(cc46.raw[,1]==0),
 dose.quad = -(cc46.raw[,1]==2) + 2*(cc46.raw[,1]==1) - 4*(cc46.raw[,1]==0)
)

cc46.aov.detail <-  aov(second.count ~
 block + first.count + (ctl.vs.trt/dose) +
 dose.quad:fumigant %in% ctl.vs.trt +
 dose.lin:fumigant %in% ctl.vs.trt,
 qr=T, data=cc46)
print(summary(cc46.aov.detail))

cc46.aov <-  aov(second.count ~
 block + first.count + (ctl.vs.trt/dose/fumigant),
 proj=T, data=cc46)

print(summary(cc46.aov))

cp <- proj(cc46.aov)
#> dimnames(cp)[[2]]
#[1] "(Intercept)"                     "block"
#[3] "first.count"                     "ctl.vs.trt"
#[5] "dose %in% ctl.vs.trt"            "fumigant %in% (ctl.vs.trt/dose)"
#[7] "Residuals"

second.block <- apply(cp[,3:7], 1, sum)
second.block.first <- apply(cp[,4:7], 1, sum)
treatments <- apply(cp[,4:6], 1, sum)

par(mfrow=c(2,4))

boxmat ( second.count ~ dose+fumigant, data=cc46,
	main="Figure 7(a). Second Count",
	mar = c(6.1, 5.1, 4, 6.1),
	ylim = c(0,700)
)
boxmat ( second.block ~ dose+fumigant, data=cc46,
	main="Figure 7(c). Second Count adj for Block",
	mar = c(6.1, 5.1, 4, 6.1),
	ylim = c(-350,350)
)
boxmat ( second.block.first ~ dose+fumigant, data=cc46,
	main="Fig 7(e). Second adj for Block & First",
	mar = c(6.1, 5.1, 4, 6.1),
	ylim = c(-350,350)
)

boxmat ( cp[,"Residuals"] ~ dose+fumigant, data=cc46,
	main="Figure 7(g). Residuals",
	mar = c(6.1, 5.1, 4, 6.1),
	ylim = c(-350,350)
)
boxmat ( cp[,"block"] ~ dose+fumigant, data=cc46,
	main="Figure 7(b). Block Effect",
	mar = c(6.1, 5.1, 4, 6.1),
	ylim = c(-350,350)
)
boxmat ( cp[,"first.count"] ~ dose+fumigant, data=cc46,
	main="Fig 7(d). First Count Eff adj Block",
	mar = c(6.1, 5.1, 4, 6.1),
	ylim = c(-350,350)
)
boxmat ( treatments ~ dose+fumigant, data=cc46,
	main="Figure 7(f). Treatment Effect",
	mar = c(6.1, 5.1, 4, 6.1),
	ylim = c(-350,350)
)



cc46.aovf <-  aov(first.count ~
 block + (ctl.vs.trt/dose/fumigant),
 proj=T, data=cc46)

par(mfrow=c(2,4))
first.adj.block <- apply(proj(cc46.aovf)[,-2], 1, sum)
second.adj.block <- apply(cp[,-2], 1, sum)
par(mar = c(5.1, 6.1, 5.1, 0))
plot(x=first.adj.block, y=second.adj.block,
	main="Fig 8. Resp vs cov, adj for block.",
	xlab="covariate adj for block",
	ylab="")
mtext("response adj for block", side=2, line=5)


par(mfrow=c(2,4))
boxmat ( treatments ~ dose+fumigant, data=cc46,
	main="Figure 9(a). Treatment Effect",
	mar = c(6.1, 9.1, 4, 6.1),
	row.name.line=5,
	ylim=c(-140,140), yticks=c(-130,0,130), ytick.labels=c("-130","","130")
)

boxmat ( treatments ~ fumigant+dose, data=cc46,
	main="Figure 9(b). Treatment Effect",
	mar = c(6.1, 9.1, 4, 6.1),
	row.name.line=5,
	ylim=c(-140,140), yticks=c(-130,0,130), ytick.labels=c("-130","","130")
)

frame();frame();frame()

cell.mean <- cell.apply(
	apply(proj(cc46.aov)[,c(1,4,5,6,7)],1,sum) ~ dose+fumigant,
	data=cc46)

cell.ssq <- cell.apply(
	apply(proj(cc46.aov)[,c(4,5,6)],1,sum)~dose+fumigant,
	data=cc46, function(x)sum(x^2))
sum(cell.ssq[c(1,5,6,8,9,11,12,14,15)])

cell.means <- cell.mean[,2:5]
cell.means[1,] <- cell.mean[1,1]

matplot(x=1:4, y=t(cell.means), type="l", lty=1,
	main="Figure 9(c). Dose.",
	xlab="fumigant", ylab="count",
	xaxp=c(1,4,3), xaxt="n")
axis(side=1, at=1:4, labels=dimnames(cell.means)[[2]], ticks=T)
text(x=3.7, y=c(370,270,210), labels=0:2)

matplot(x=0:2, y=cell.means, type="l", lty=1,
	main="Figure 9(d). Fumigant.",
	xlab="dose", ylab="count",
	xaxp=c(0,2,2), xaxt="n")
axis(side=1, at=0:2)
text(x=1.7, y=c(210,230,300,350), labels=c("ck","cs","cm","cn"))

