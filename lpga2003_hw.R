lpga03 <- read.csv("../Desktop/Vejay/Spring 2020/BlockChain/hw/HW2/DATA/nonstandard.dat/nonstandard.dat")
attach(lpga03); names(lpga03)

## Set-up the elements for paired t-test
diff.us.brit <- usOpen - britOpen
dbar <- mean(diff.us.brit)
s_d <- sd(diff.us.brit)
n <- length(diff.us.brit)
SE.dbar <- 

df <- 
t.stat <- 
t.025 <- qt(1-0.05/2, )  
pval <- 2*(1-pt(abs( ), ))   
CI.LB <- dbar -    
CI.UB <- dbar + 

golf.out <- cbind(mean(usOpen), mean(britOpen), dbar, SE.dbar, df,
                  t.stat, t.025, pval, CI.LB, CI.UB)
colnames(golf.out) <- c("US", "Brit", "Diff", "Std Err",
                         "df", "t", "t(.025)", "2P(>|t|)", "Lower", "Upper")
round(golf.out,3)

## Use t.test function
t.test(usOpen, britOpen, paired=T)

