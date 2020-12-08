spiralOrder <- function(matrix) {
  up <- 1
  down <- dim(matrix)[1]
  left <- 1
  right <- dim(matrix)[2]
  ans <- c()
  direction <- 0
  while (up <= down && left <= right) {
    if (direction == 0) {
      for (j in left: right) {
        ans <- c(ans, matrix[up, j])
      }
      up <- up + 1
    }
  
    if (direction == 1) {
      for (i in up: down) {
        ans <- c(ans, matrix[i, right])
      }
      right <- right - 1
    }
  
    if (direction == 2) {
      for (j in right: left) {
        ans <- c(ans, matrix[down, j])
      }
      down <- down - 1
    }
    
    if (direction == 3) {
      for (i in down: up) {
        ans <- c(ans, matrix[i, left])
      }
      left <- left + 1
    }
    
  direction <- (direction + 1) %% 4
  }
  ans
}

spiralOrder(matrix(data = 1:9, nrow = 3, byrow = TRUE))