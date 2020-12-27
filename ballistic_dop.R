dop.1 <- read.csv("http://users.stat.ufl.edu/~winner/data/ballistic_dop1.csv")
attach(dop.1); names(dop.1)


mean.all <- mean(DoP)
sd.all <- sd(DoP)
n.all <- length(DoP)
sem.all <- sd.all / sqrt(n.all)
df.all <- n.all - 1
qt.all <- qt(.975, df.all)
LB.all <- mean.all - qt.all * sem.all
UB.all <- mean.all + qt.all * sem.all

all.out <- cbind(n.all, mean.all, sd.all, sem.all, df.all, qt.all, 
                       LB.all, UB.all)
colnames(all.out) <- c("n", "mean", "sd", "sem", "df", "t(.025)",
                        "Lower", "Upper")
round(all.out, 3)
t.test(DoP)


mean.lab <- as.vector(tapply(DoP, Lab, mean))
sd.lab <- as.vector(tapply(DoP, Lab, sd))
n.lab <- as.vector(tapply(DoP, Lab, length))
sem.lab <- sd.lab / sqrt(n.lab)
df.lab <- n.lab - 1
qt.lab <- qt(.975, df.lab)
LB.lab <- mean.lab - qt.lab * sem.lab
UB.lab <- mean.lab + qt.lab * sem.lab

lab.out <- cbind(1:6, n.lab, mean.lab, sd.lab, sem.lab, df.lab, qt.lab, 
                       LB.lab, UB.lab)
colnames(lab.out) <- c("Lab", "n", "mean", "sd", "sem", "df", "t(.025)",
                        "Lower", "Upper")
round(lab.out, 3)

num.lab <- length(mean.lab)

for (i in 1:num.lab) {
   print(t.test(DoP[Lab == i]))
}


## Plot the Confidence Intervals (1-6 are Labs, 7 is overall)

## Create a data frame with mean, Lower, and Upper bounds 
mean.lab.all <- c(mean.lab, mean.all)
LB.lab.all <- c(LB.lab, LB.all)
UB.lab.all <- c(UB.lab, UB.all)
df.lab.all <- data.frame(cbind(mean.lab.all,LB.lab.all,UB.lab.all))

plot(1:length(mean.lab.all),mean.lab.all,ylim=c(0,80))
for (i in 1:length(mean.lab.all)) {
  arrows(i,LB.lab.all[i],i,UB.lab.all[i],code=3,angle=90)
}


## Use plotrix package to plot Confidence intervals
install.packages("plotrix")
library(plotrix)

# Plot Means (Index will be "lab" w/ 7 = all
plot(df.lab.all$mean.lab.all, ylim=c(20, 60), xlim=c(1,7))

# Plot the distances to go above/below mean
#  uiw = upper interval width = UB - mean = t(.025)*s/sqrt(n)
#  liw = lower interval width = mean - LB = t(.025)*s/sqrt(n)
plotCI(df.lab.all$mean.lab.all,y=NULL, 
       uiw=df.lab.all$UB.lab.all-df.lab.all$mean.lab.all, 
       liw=df.lab.all$mean.lab.all-df.lab.all$LB.lab.all, err="y",      
       pch=20, slty=3, scol = "black", add=TRUE)








