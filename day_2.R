##File name: day_2.r
##Author: Hannah Harrison
##Last Edit: 02/12/2022
##Description: day 2, advent of code 2022, "Rock, Paper, Scissors.."

##load libraries
library(data.table)
library(dplyr)

input <- fread("d2_input.txt", header = FALSE)
test_input <- fread("d2_test.txt", header = FALSE)

play_rock_paper_scissors1 <- function(my_strategy){
    colnames(my_strategy) <- c("p1_move", "my_move")
    
    my_strategy <- my_strategy %>% 
        mutate(my_result = case_when((p1_move == "A" & my_move == "Y") | 
                                     (p1_move == "B" & my_move == "Z") | 
                                     (p1_move == "C" & my_move == "X") ~ "win", 
                                    (p1_move == "A" & my_move == "X") |
                                    (p1_move == "B" & my_move == "Y") |
                                    (p1_move == "C" & my_move == "Z")  ~ "draw",
                                    (p1_move == "A" & my_move == "Z") | 
                                     (p1_move == "B" & my_move == "X") | 
                                     (p1_move == "C" & my_move == "Y") ~ "lose"))

     my_strategy <- my_strategy %>% 
        mutate(move_points = recode(my_move, "X" = 1, "Y" = 2, "Z" = 3), 
               res_points = recode(my_result, "win" = 6, "draw" = 3, "lose" = 0), 
               round_score = move_points+res_points)
    
    total_score <-  sum(my_strategy$round_score)
    return(total_score)
}           

play_rock_paper_scissors2 <- function(my_strategy){
    colnames(my_strategy) <- c("p1_move", "encrypted_res")

     my_strategy <- my_strategy %>% 
        mutate(my_res = recode(encrypted_res, "X" = "lose", "Y" = "draw", "Z" = "win"))
    
    my_strategy <- my_strategy %>% 
        mutate(my_move = case_when((p1_move == "A" & my_res == "draw") | 
                                     (p1_move == "B" & my_res == "lose") | 
                                     (p1_move == "C" & my_res == "win") ~ "rock", 
                                    (p1_move == "A" & my_res == "lose") |
                                    (p1_move == "B" & my_res == "win") |
                                    (p1_move == "C" & my_res == "draw")  ~ "scissors",
                                    (p1_move == "A" & my_res == "win") | 
                                     (p1_move == "B" & my_res == "draw") | 
                                     (p1_move == "C" & my_res == "lose") ~ "paper"))

     my_strategy <- my_strategy %>% 
        mutate(move_points = recode(my_move, "rock" = 1, "paper" = 2, "scissors" = 3), 
               res_points = recode(my_res, "win" = 6, "draw" = 3, "lose" = 0), 
               round_score = move_points+res_points)
    
    total_score <-  sum(my_strategy$round_score)
    return(total_score)
}      

test_ans <- play_rock_paper_scissors1(test_input)
puzzle_ans <- play_rock_paper_scissors1(input)
print(test_ans)
print(puzzle_ans)

test_ans <- play_rock_paper_scissors2(test_input)
puzzle_ans <- play_rock_paper_scissors2(input)
print(test_ans)
print(puzzle_ans)