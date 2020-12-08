twoSum <- function(nums, target){
  n <- length(nums)
  a <- cbind(nums, 1:n)
  A <- as.matrix(a[order(a[,1]), ])
  left <- 1
  right <- n
  while (left < right){
    if (A[left, 1] + A[right, 1] == target){
        ans <- c(A[left, 2], A[right, 2])  
        break
    }
    else if (A[left, 1] + A[right, 1] < target){
      left <-  left + 1
    }
    else {
      right <- right - 1
    }
  }
   sort(ans)
}

twoSum(c(7, 2, 10, 11), 9)  