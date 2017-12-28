### This function is quick sort agorithum by selecting first element as pivot. 

## the array used in the assignment: 
## array <- as.matrix(read.table("https://d3c33hcgiwev3.cloudfront.net/_32387ba40b36359a38625cbb397eee65_QuickSort.txt?Expires=1514419200&Signature=XNqdW2SJH-EwbefYJ~V~i75O4PULKGzm4zU78W0t1-BSLJBS3jvkZvqRziIKQOBps634g9Vldj09j1xkQPezzW29K6FtHJtDEEbxZpNL8zkKKENexWkBolfp7i~6PlA6i7rkvi-1GJl0QKNjRWy4exfFcaJNp-xezxjTCT2aZow_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"))

quicksort <- function(array){
        count <- 0
        n <- length(array)
        if (n == 0 | n == 1) return(array)
        else {
        i <- 1
        position <- 1
        for (j in 2:n){
                if (array[j] < array[i]){
                        temp1 <- array[j]
                        temp2 <- array[(position+1)]
                        array[j] <- temp2
                        array[(position+1)] <- temp1
                        position <- position + 1
                }
        }
        temp3 <- array[1]
        temp4 <- array[position]
        array[1] <- temp4
        array[position] <- temp3
### This step is optional, to calculate total comparisons of quick sort. Need to define total_count 
### in the global enviorment and set default to 0. 
        count <- n - 1
        total_count <<- count + total_count
       
        if (2 > position) left_array <- vector("numeric", 0)
        else left_array <- array[1:(position-1)]
        pivot <- array[position]
        if (position +1 > n) right_array <- vector("numeric", 0)
        else right_array <- array[(position+1):n]
        
        left_new_array <- quicksort(left_array)
        right_new_array <- quicksort(right_array)
        new_array <- c(left_new_array, pivot, right_new_array)
        
        }
        new_array
}