### Similar to the first task. The only difference is to select last element as pivot. 

quicksort2 <- function(array){
        count <- 0
        n <- length(array)
        if (n == 0 | n == 1) return(array)
        else {
### exchange the first and the last element
                temp1 <- array[n]
                temp2 <- array[1]
                array[n] <- temp2
                array[1] <- temp1
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
                count <-  n - 1
                total_count <<- total_count + count

                
                if (2 > position) left_array <- vector("numeric", 0)
                else left_array <- array[1:(position-1)]
                pivot <- array[position]
                if (position +1 > n) right_array <- vector("numeric", 0)
                else right_array <- array[(position+1):n]
                
                left_new_array <- quicksort2(left_array)
                right_new_array <- quicksort2(right_array)
                new_array <- c(left_new_array, pivot, right_new_array)
                
        }
        new_array
}