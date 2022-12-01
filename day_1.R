##File name: day_1.r
##Author: Hannah Harrison
##Last Edit: 01/12/2022
##Description: day 1, advent of code 2022

##load libraries
library(data.table)
library(dplyr)

input <- fread("d1_input.txt")
test_input <- fread("d1_test.txt")

highest_calorie_wins <- function(my_input){
    colnames(my_input) <- c("calories")
    n <- length(my_input$calories)
    my_df <- data.frame(eid = integer(), calories  = double())
    
    temp_sum <- 0
    elf_eid <- 1

    for(i in 1:n) {
        my_line <- my_input$calories[i]
        if(!is.na(my_line) && i != n) {
            temp_sum <- temp_sum + my_line
        } else if(!is.na(my_line) && i == n) {
            temp_sum <- temp_sum + my_line
            my_df[nrow(my_df) + 1, ] <- c(elf_eid, temp_sum)
        } else {
            my_df[nrow(my_df) + 1, ] <- c(elf_eid, temp_sum)
            elf_eid <- elf_eid + 1
            temp_sum <- 0
        }
    }

    #part 1
    my_max <- max(my_df$calories)

    #part 2
    my_df <- arrange(my_df, calories)
    m <- nrow(my_df)
    my_max_three1 <- sum(my_df$calories[(m-2):m]) 
    
    #part 2 again
    my_df <- arrange(my_df, desc(calories))
    my_max_three2 <- sum(my_df$calories[1:3])

    #part 1 again 
    my_max2 <- my_df$calories[1]

    return(c(my_max2, my_max_three2))


}

test_max <- highest_calorie_wins(test_input)
full_max <- highest_calorie_wins(input)
print(test_max)
print(full_max)