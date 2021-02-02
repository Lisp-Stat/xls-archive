fc <- matrix(
c( 0, 232, 355, 394, 427, 449, 435,
1000, 291,  20,  12,   7,   4,	 3,
   0, 473, 280,  46,  15,  10,	 8,
   0,	4, 330, 210,  49,  16,	14,
   0,	0,   8, 315, 166,  30,	19,
   0,	0,   0,   4,  60,  24,	 8,
   0,	0,   6,  19, 276, 466, 513)/1000,
nrow=7, byrow=T,
dimnames = list(Class=c("Withdrawn","Freshman","Sophomore","Junior",
	"Senior","HighSenior","Graduate"), Years=0:6))

par(mfrow=c(1,2))
boxmatm(fc,
	main="Figure 10.  Cohort Progression",
	mar=c(6.1,11.1,4,6.1),
	ylim=c(-.05,1.05), yticks=c(0,.5,1), ytick.labels=c("0","","1"))

