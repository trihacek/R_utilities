# Based on Chinn (2000)
or_to_es <- function(or){
	es <- (log(or))/1.81;
	return(es);
}

# Based on Borenstein et al (2009)
r_to_d <- function(r){
	d <- (2*r)/sqrt(1-r^2)
	return(d)
}

# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# 95% confidence interval around mean
mean.ci <- function(x) {
  n <- numCases
  se <- sd(x)/sqrt(n)
  r <- c(mean(x) - 2.58*se, mean(x) - 1.96*se, mean(x), mean(x) + 1.96*se, mean(x) + 2.58*se)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# Show outliers
outliers <- function(x, cutoff=3) {
	m <- mean(x)
	stdev <- sd(x)
	o <- x[ (x-m)/stdev > cutoff | (x-m)/stdev < -1*cutoff ]
	return(o)
}

# Replace outlier values
outliers_rp <- function(x, cutoff=3) {
	m <- mean(x)
	stdev <- sd(x)
	x[ (x-m)/stdev > cutoff ] <- m + (stdev * cutoff)
	x[ (x-m)/stdev < -1*cutoff ] <- m - (stdev * cutoff)
	return(x)
}

# Returns 95%-CIs of a proportion
ci_prop <- function(p, n) {
	if (p > 1)
		p <- p/100
	se <- sqrt( ( p * (1-p) ) / n )
	lci <- p - 1.96*se
	hci <- p + 1.96*se
	cat("Confidence interval (95%):", lci, ",", hci)
}

# A shortcut for outputting a data frame to a csv file
out <- function(x, round) {
	if (!missing(round))
		x <- round(x, digits=round)
	write.csv(x, file="output.csv")
}

# A shortcut for outputting a data frame to a csv2 file
out2 <- function(x, round=NULL) {
	if (!missing(round))
		x <- round(x, digits=round)
	write.csv2(x, file="output.csv")
}

hist_with_normal_curve <- function(x, breaks = 24) {
  h <- hist(x, breaks = breaks, col = "lightblue")
  xfit <- seq(min(x), max(x), length = 40)
  yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
  yfit <- yfit * diff(h$mids[1:2]) * length(x)
  lines(xfit, yfit, lwd = 2)
}

# Compute RCI
rci <- function(sd, rel) {
  rci <- 1.96 * sqrt(2) * sd * sqrt(1 - rel)
  return(rci)
}

# Based on Field et al. (2012)
logisticPseudoR2s <- function(LogModel) {
	dev <- LogModel$deviance
	nullDev <- LogModel$null.deviance
	modelN <- length(LogModel$fitted.values)
	R.l <- 1 - dev / nullDev
	R.cs <- 1 - exp ( -(nullDev - dev) / modelN)
	R.n <- R.cs / ( 1 - (exp (-(nullDev / modelN))))
	cat("Pseudo R^2 for logistic regression\n")
	cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
	cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
	cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}

# Returns a percentage table
perc <- function(x, round = 1) {
  library(plyr)
  c <- plyr::count(x)
  total <- sum(c$freq)
  c$perc <- round(c$freq/total*100, round)
  return(c)
}

# Add asterisks pased on pvalue
asterisks <- function(p) {
  if (p < 0.001)
    sig <- "***"
  else if (p < 0.01)
    sig <- "**"
  else if (p < 0.05)
    sig <- "*"
  else if (p < 0.1)
    sig <- "+"
  else
    sig <- ""
  return(sig)
}