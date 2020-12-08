## https://cran.r-project.org/web/packages/hash/hash.pdf
library(hash)
lengthOfLongestSubstring <- function(s) {
  ans <- 0
  start <- 1
  dict <- hash()
  n <- length(s)
  for (i in 1 : n) {
    if (has.key( 's[i]', dict) == TRUE && dict[['s[i]']] >= start){
        start <- dict[['s[i]']] + 1
    }
    else {
        ans <- max(ans, i - start + 1)
    }
    dict$s[i] <- i
  }
  ans
}
  
s <- c('a', 'b', 'c', 'a')
lengthOfLongestSubstring(s)