stepwiseMethod <- function(response, dataset) {
  y <- response
  available.x <- colnames(dataset)[colnames(dataset) != response]
  chosen.x <- NULL
  r2 <- NULL
  
  while (length(available.x) > 0) {
    best.r2 <- 0
    for (this.x in available.x) {
      rhs <- paste(c(chosen.x, this.x), collapse=" + ")
      f <- as.formula(paste(y, rhs, sep=" ~ "))
      this.r2 <- summary(lm(f, data=dataset))$r.square
      if (this.r2 > best.r2) {
        best.r2 <- this.r2
        best.x <- this.x
      }
    }
    chosen.x <- c(chosen.x, best.x)
    available.x <- available.x[available.x != best.x]
    r2 <- c(r2, best.r2)
  }
  chosen.x <- c("(Intercept)", chosen.x)
  r2 <- c(summary(lm(as.name(y) ~ 1, data=dataset))$r.square, r2)
  
  cum.r2 <- cbind(chosen.x, r2)
  return(cum.r2)
}