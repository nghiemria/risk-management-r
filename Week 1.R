library(quantmod)
# CALCULATING RETURNS ON GOLD

# Create data series called "wilsh"
wilsh <- getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]
name(wilsh) <- "TR"
# View the first observation is an "NA"
logret <- diff(log(wilsh))
head(logret, 3)

# Remove "NA" and calculate log returns
logret <- diff(log(wilsh))[-1]
round(head(logret, 3), 6)
# Caculate the discret returns
ret <- exp(logret) - 1
round(head(ret, 3), 6)

# Long horizon returns and discrete returns of gold
logret.w <- apply.weekly(logret, sum)
ret.w<-exp(logret.w)-1
logret.m <- apply.monthly(logret, sum)
ret.m <- exp(logret.m) - 1
logreg.q <- apply.quarterly(logret, sum)
ret.q <- exp(logret.q) - 1
logret.y <- apply.yearly(logret, sum)
ret.y <- exp(logret.y) -1
