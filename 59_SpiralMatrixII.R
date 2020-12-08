spiralOrder <- function(n) {
  up <- 1
  down <- n
  left <- 1
  right <- n
  ans <- matrix(rep(0, n * n), nrow = n)
  index <- 1
  direction <- 0
  while (index <= n * n) {
    if (direction == 0) {
      for (j in left: right) {
        ans[up, j] <- index
        index <- index + 1
      }
      up <- up + 1
    }
    if (direction == 1) {
      for (i in up: down) {
        ans[i, right] <- index
        index <- index + 1
      }
      right <- right - 1
    }
    if (direction == 2) {
      for (j in right: left) {
        ans[down, j] <- index
        index <- index + 1
      }
      down <- down - 1
    }
    if (direction == 3) {
      for (i in down: up) {
        ans[i, left] <- index
        index <- index + 1
      }
      left <- left + 1
    }
    direction <- (direction + 1) %% 4
  }
  ans
}

spiralOrder(3)