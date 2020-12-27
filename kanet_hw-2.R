kanet <- read.csv("http://www.stat.ufl.edu/~winner/data/kanet.csv")
attach(kanet); names(kanet)

## Create group.f as factor variable and generate boxplot
group.f <- factor(group, levels=1:2, labels=c("Lahoul", "Kulu"))
plot(cubit ~ group.f)

###########################################################################
#### This part sets up the computations for the t-tests and CI's          #
###   See slides 4-15 of Inferences Regarding Differences of 2 Pop. Means #
###########################################################################

## Compute and save sample size (.n), mean (.mean), and sd (.sd) by group
grp.n <- as.vector(tapply(cubit, group, length))
grp.mean <- as.vector(tapply(cubit, group, mean))
grp.sd <- as.vector(tapply(cubit, group, sd))

## Compute and save ybar_1 - ybar_2 and pooled standard deviation
mean.diff <- grp.mean[1] - grp.mean[2]       
s_pooled <- sqrt( ((grp.n[1]-1)*grp.sd[1]^2 + (grp.n[2]-1)*grp.sd[2]^2) /
                  (grp.n[1] + grp.n[2] - 2) )

## Compute standard errors of ybar_1 - ybar_2 for:
##    1) Equal variance case   2) Unequal variance case
SE.mean.diff.1 <- s_pooled * sqrt(1/grp.n[1] + 1/grp.n[2])
SE.mean.diff.2 <- sqrt(grp.sd[1]^2/grp.n[1] + grp.sd[2]^2/grp.n[2])

## Compute Degrees of Freedom for each case
df.1 <- grp.n[1] + grp.n[2] - 2
df.2a <- (grp.sd[1]^2/grp.n[1] + grp.sd[2]^2/grp.n[2])^2
df.2b <- ((grp.sd[1]^2/grp.n[1])^2 / (grp.n[1]-1)) + 
         ((grp.sd[2]^2/grp.n[2])^2 / (grp.n[2]-1))
df.2 <- df.2a / df.2b

#################################################################
### Beginning here, you need to begin filling in the commands   #
#################################################################

## Compute the t-statistics for each case
t.stat.1 <- (mean.diff/SE.mean.diff.1)
t.stat.2 <- (mean.diff/SE.mean.diff.2)
round(t.stat.1,3)
round(t.stat.2,3)

## Compute the critical t-values for each case
t.025.1 <- qt(1-0.05/2, df.1)
t.025.2 <- qt(1-0.05/2, df.2)

t.test(cubit ~ group.f, var.equal=T)
t.test(cubit ~ group.f, var.equal=F)

## Compute p-values for each case
pval.1 <- 2*(1-pt(abs() , ))       
pval.2 <- 2*(1-pt(abs() , ))    

## Compute Lower and Upper Bounds for CI's for each case
CI.LB.1 <- mean.diff - 
CI.UB.1 <- mean.diff + 
CI.LB.2 <- mean.diff - 
CI.UB.2 <- mean.diff + 

##################################################################
## Combine results for readable output                           #
##################################################################

mean.diff12 <- rbind(mean.diff, mean.diff)
SE.mean.diff <- rbind(SE.mean.diff.1, SE.mean.diff.2)
df <- rbind(df.1, df.2)
t.stat <- rbind(t.stat.1, t.stat.2)
t.025 <- rbind(t.025.1, t.025.2)
pval <- rbind(pval.1, pval.2)
CI.LB <- rbind(CI.LB.1, CI.LB.2)
CI.UB <- rbind(CI.UB.1, CI.UB.2)

kanet.out <- cbind(mean.diff12,SE.mean.diff,df,t.stat,t.025,pval,CI.LB,CI.UB)
colnames(kanet.out) <- c("Mean Diff", "Std Err", "df", "t", "t(.025)",
                          "2P(>|t|)", "Lower", "Upper")
rownames(kanet.out) <- c("Equal Var", "Unequal Var")
round(kanet.out, 4)

## Use t.test function and compare results with "Brute-force" calculations

t.test()


#########################################################

## Variance Test / CI

## Obtain group sample sizes, sample VARIANCES, degrees of freedom
grp.n <- as.vector(tapply(cubit, group, length))
grp.var <- as.vector(tapply(cubit, group, var))
grp.df <- grp.n - 1

## Conduct the F-test w/ F-stat, F.L, F.U, p-value
F.stat <- (grp.var[1] / grp.var[2])
F.L <- qf(.05/2, grp.df[1], grp.df[2])
F.U <- qf(1-.05/2, grp.df[1], grp.df[2])
p.L <- pf(F.stat, grp.df[1], grp.df[2])
p.U <- 1 - pf(F.stat, grp.df[1], grp.df[2])
pval <- 2 * min(p.L, p.U)
CI.LB <- F.stat * qf(.05/2, grp.df[2], grp.df[1])
CI.UB <- F.stat * qf(1-.05/2, grp.df[2], grp.df[1])

var.out <- cbind(grp.var[1], grp.var[2], F.stat, grp.df[1], grp.df[2],
                 F.L, F.U, pval, CI.LB, CI.UB)
colnames(var.out) <- c("s1^2", "s2^2", "F", "df1", "df2", "F(.025)", 
                       "F(.975)", "P-value", "Lower", "Upper")
round(var.out, 3)

var.test(cubit~group.f)

### Using var.test
lahoul.cubit <- cubit[group==1]
kulu.cubit <- cubit[group==2]

var.test( lahoul.cubit, kulu.cubit)






