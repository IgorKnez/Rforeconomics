
# print system time

Sys.time()

# create bad list

bad_list <- list(na = c(NA, NA, NA), chars = c('a', 'b', 'c'), 
                 bools = c(FALSE, FALSE, TRUE))

# create data frame happy frame:

happy_frame <- data.frame(na = c(NA, NA, NA), chars = c('a', 'b', 'c'), 
                          bools = c(FALSE, FALSE, TRUE))

x <- 1:16


# write the length function

sum(x) / mean(x)

# 3rd position is TRUE

x %% 3 == 0


#red fruit larger than 5

fruit_size = c(2, 4, 7, 5, 3, 2)
fruit_col = c('g', 'y', 'r', 'g', 'b', 'r')


(fruit_col == 'r' & fruit_size > 5) | (fruit_col != 'r' & fruit_size < 5)


mtcars <- mtcars

sum(mtcars[mtcars$carb <= 2,] [c(3,8,12),]$disp)
