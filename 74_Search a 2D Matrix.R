searchMatrix <- function(matrix, target) {
  m <- dim(matrix)[1]
  n <- dim(matrix)[2]
  if (matrix[1, 1] > target || matrix[m, n] < target){
    return('FALSE')
  }
  a <- as.vector(t(matrix))
  left <- 1
  right <- m * n
  while (left + 1 < right) {
    mid <- left + (right - left) %/% 2
    if (a[mid] == target) {
      return('TRUE')
    }
    else if (a[mid] > target) {
      right <- right - 1
    }
    else {
      left <- left + 1
    }
  }
  if (a[left] == target || a[right] == target) {
    return('TRUE')
  }
  else {
    return('FALSE')
  }
}

searchMatrix(matrix(c(1,3,5,7,10,11,16,20,23,30,34,50), byrow = TRUE, nrow= 3, ncol = 4), 3)