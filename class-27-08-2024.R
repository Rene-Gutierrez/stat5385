# Ad Spending Example

# Set Seed
set.seed(8272024)

# Data Simulation
x <- rnorm(n    = 100,
           mean = 70,
           sd   = 30)
y <- 1000 + 5 * x + rnorm(n = 100, mean = 0, sd = 100)

# Creates the Data Frame
datAd <- data.frame(cbind(y,x))

# Names the Variables
colnames(datAd) <- c("Revenue", "Ad Spending")

# Saves to csv
write.csv(x    = datAd[, c(1, 2)],
          file = "Wine Data.csv", row.names = FALSE)

# Reads the Data
dat <- read.csv(file = "Wine Data.csv")

# Plots the Data
plot(x = dat$Ad.Spending,
     y = dat$Revenue,
     xlab = "Ad Spending",
     ylab = "Revenue")

# Regression
outReg <- lm(Revenue ~ Ad.Spending, data = dat)

# Summary
summary(outReg)

# Regression Line
abline(a   = outReg$coefficients[1],
       b   = outReg$coefficients[2],
       col = 'red',
       lwd = 2)


plot(x = dat$Ad.Spending,
     y = dat$Revenue,
     xlab = "Ad Spending ($)",
     ylab = "Revenue ($)",
     ylim = c(900, 2000),
     xlim = c(0, 150))
abline(a   = outReg$coefficients[1],
       b   = outReg$coefficients[2],
       col = 'red',
       lwd = 2)
abline(v = 0)


# Wine and Life Expectancy

# Set Seed
set.seed(8272024)

# Data Simulation
x <- rbinom(n = 20, size = 20, prob = 0.1)
y <- 75 + 1.5 * x + rnorm(n = 20, mean = 0, sd = 3)

# Creates the Data Frame
datWin           <- data.frame(cbind(y,x))

# Names the Variables
colnames(datWin) <- c("Years", "Glasses")

# Saves to csv
write.csv(datWin[, c(1, 2)], file = "Wine Data.csv", row.names = FALSE)

# Reads the Data
dat <- read.csv(file = "Wine Data.csv")

# Plots the Data
plot(x = dat$Glasses,
     y = dat$Years,
     xlab = "Ad Spending",
     ylab = "Revenue")

dat <- read.csv(file = "Burger Data.csv")
