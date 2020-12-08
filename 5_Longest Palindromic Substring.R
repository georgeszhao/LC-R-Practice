longestPalindrome <- function(s) {
  if (nchar(s) < 1) {
    return('')
  }
  left <- 0
  right <- 0
  max_len <- 0
  n <- nchar(s)
  dp <- matrix(rep(0, n * n), nrow = n)
  for (i in 1: n) {
    dp[i, i] <- 1
    j <- 1
    while (j < i) {
      dp[i, j] <- (substr(s, i, i) == substr(s, j, j) && (i - j == 1 || dp[i - 1, j + 1] == 1))
      if (dp[i, j] == 1 && max_len < i -j + 1) {
        left <- j
        right <- i
        max_len <- i - j + 1
      }
      j <- j + 1
    }
  }
  substr(s, left, right)
}

longestPalindrome('babad')