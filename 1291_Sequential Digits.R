sequentialDigits <- function(low, high) {
  ans <- c()
  for (i in 1: 9) {
    num <- i
    j <- i + 1
    while (num <= high && j < 10) {
      num <- 10 * num + j
      j <- j + 1
      if (num >= low && num <= high) {
        ans <- c(ans, num)
      }
    }
  }
  sort(ans)
}
      
sequentialDigits(100, 300)