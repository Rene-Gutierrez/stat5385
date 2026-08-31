# Number of Observations
M <- seq(0, 4, by = 0.1)

# Plots
xmax <-   8
xmin <- - 8

# t - distribution
den1 <- function(x) dnorm(x = x, mean = 0, sd = 1)
den2 <- function(x) dnorm(x = x, mean = 0, sd = 1)
denG <- function(x) (den1(x) + den2(x)) / 2
# Plot Population
ymax <- max(denG(seq(xmin, xmax, length = 100)))
par(mfrow = c(2, 1))
par(mar   = c(4.5, 2, 0.5, 0.5))
for(m in M){
  # Change Densities
  den1 <- function(x) dnorm(x = x, mean = -m, sd = 1)
  den2 <- function(x) dnorm(x = x, mean =  m, sd = 1)
  denG <- function(x) (den1(x) + den2(x)) / 2
  # Single Density
  curve(den1,
        from = xmin,
        to   = xmax,
        lwd  = 2,
        ylab = "",
        xlab = "x",
        ylim = c(0, ymax),
        col  = rgb(0, 0, 1, 0.5))
  par(new=TRUE)
  curve(den2,
        from = xmin,
        to   = xmax,
        lwd  = 2,
        ylab = "",
        xlab = "x",
        ylim = c(0, ymax),
        col  = rgb(1, 0, 0, 0.5))
  abline(h = 0, lwd = 2)
  # Mixture Density
  curve(denG,
        from = xmin,
        to   = xmax,
        lwd  = 2,
        ylab = "",
        xlab = "x",
        ylim = c(0, ymax),
        col  = rgb(1, 0, 1, 0.5))
  legend("topleft", paste0("mean diff = ", 2*m))
  abline(h = 0, lwd = 2)
  Sys.sleep(1)
}