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
out <- function(x, round = NULL, type = c("csv","xlsx")) {
	if (!is.null(round))
		x <- round(x, digits=round)
	if(type[1] == "csv")
		write.csv(x, file="output.csv")
	else
		openxlsx::write.xlsx(x, file="output.xlsx")
}

# A shortcut for outputting a data frame to a csv2 file
out2 <- function(x, round = NULL, type = c("csv","xlsx")) {
	if (!is.null(round))
		x <- round(x, digits=round)
	if(type[1] == "csv")
		write.csv2(x, file="output.csv")
	else
		openxlsx::write.xlsx(x, file="output.xlsx")
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

# Add confidence intervals
logisticOutput <- function(LogModel) {
  x <- summary(LogModel)
  x <- as.matrix(x$coefficients)
  x <- cbind(x, exp(x[,1]))
  x <- cbind(x, exp( x[,1]-1.96*x[,2] ))
  x <- cbind(x, exp( x[,1]+1.96*x[,2] ))
  colnames(x) <- c("Estimate","Std. Error","z-value","Pr(>|z|)","Exp(B)","CI lower","CI higher")
  round(x,3)
}

# Test of the model
logisticTest <- function(LogModel) {
  modelChi <- LogModel$null.deviance - LogModel$deviance
  chidf <- LogModel$df.null - LogModel$df.residual
  chisq.prob <- 1 - pchisq(modelChi, chidf)
  cat("chi2(",chidf,") = ",modelChi,", p = ", chisq.prob, sep="")
}

# Test the accuracy of prediction
predictionAccuracy <- function(LogModel, status) {
  data <- LogModel$data
  data$prob <- predict(LogModel, type="response")
  data$pred <- ifelse(data$prob > .5, 1, 0)
  round( nrow(data[status == data$pred,]) / nrow(data), 2)
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
  #else if (p < 0.1)
  #  sig <- "+"
  else
    sig <- ""
  return(sig)
}

# Produces a number with the defined number of decimal places
nice.num <- function(x, decimal=2, leading.zero=FALSE, separator=c("dot","comma")) {
  sep <- ifelse(separator[1] == "dot", ".", ",")
  negative <- ifelse(x < 0, 1, 0)
  if(negative)
	x <- x * -1
  x <- round(x, decimal)
  x.string <- as.character(x)
  x.split <- unlist(strsplit(x.string, split=".", fixed=T))
  output <- x.split[2]
  if(is.na(output))
	output <- 0
  length.output <- nchar(x.split[2])
  if(is.na(length.output))
    length.output <- 1
  if(length.output < decimal)
    for(i in (length.output+1):decimal)
      output <- paste0(output, "0")
  output <- paste0(".", output)
  if(x >= 1)
	output <- paste0(x.split[1], output)
  if(x < 1 & leading.zero)
    output <- paste0("0", output)
  if(negative)
	output <- paste0("-", output)
  return(output)
}

var.desc <- function(x, decimal = 1, continuous = FALSE){
  if(continuous) {
    m <- nice.num(mean(x, na.rm = TRUE), decimal = decimal, leading.zero = TRUE)
    sd <- nice.num(sd(x, na.rm = TRUE), decimal = decimal, leading.zero = TRUE)
    na <- round(100 * sum(is.na(x)) / length(x))
    output <- paste0("M = ",m," (SD = ",sd,")\nNA = ",na,"%")
  }
  else {
    a <- as.data.frame(perc(x, round = 1))
    output <- ""
    for(i in 1:nrow(a)) {
      output <- paste0(output, a$x[i],":",a$freq[i]," (",nice.num(a$perc[i], decimal = decimal),"%)")
      if(i < nrow(a))
        output <- paste0(output, "\n")
    }
  }
  return(output)
}

var.test <- function(x, y, continuous = FALSE){
  if(continuous) {
    mean1 <- mean(x, na.rm=T)
    sd1 <- sd(x, na.rm=T)
	
    mean2 <- mean(y, na.rm=T)
    sd2 <- sd(y, na.rm=T)

    t <- psych::m2t(mean1,mean2,sd1,sd2,n1=length(x),n2=length(y))
	d <- round(t$d,2)
	output <- paste0('d = ',d,asterisks(t$p))
  }
  else {
    x <- cbind(
      c(x,y),
      c(rep(1,length(x)), rep(0,length(y)))
    )
    p <- prop.test(table(x[,1], x[,2]) )
    n <- sum(!is.na(x)) + sum(!is.na(y))
	phi <- sqrt( (p$statistic) / n )
	output <- paste0('phi = ',nice.num(phi,2,T),asterisks(p$p.value))
  }
  return(output)
}

colnames.missing <- function(df, names) {
  for(col in names)
    if(!col %in% colnames(df))
      print(col)
}

