#' R6 Class Representing a Board
#'
#' @description
#' A container to house the minefield. but not the game state
#'
#' @details
#' A board is largly hidden to the player, they must discover it on their own, and maintain the state themselves
#'
#' @export
Board <- R6::R6Class("Board",
                     public = list(
                       # Borrowed from https://github.com/yihui/fun/blob/master/R/mine_sweeper.R
                       height = 10,
                       width = 10,
                       mines = 10,
                       ask = function(x,y){
                         exploded = private$minefield[x,y]
                         info = private$mine_info[x,y]
                         return(list(exploded = exploded, info = info))
                       },
                       submit = function(candidate_minefield){
                         return(all(candidate_minefield == private$minefield))
                       },
                       initialize = function(height = 10, width = 10, mines = 10) {
                         self$height <- floor(height)
                         self$width <- floor(width)
                         self$mines <- floor(mines)
                         # Initialize the board
                         # sample mines m
                         m <- rep(FALSE, width * height)
                         mine.index <- sample(width * height, mines)
                         m[mine.index] <- TRUE
                         # Save board
                         private$minefield <- matrix(m, height, width)
                         # Calculate information to share
                         # For every single mine, add 1 to all its neighbors
                         mine_info <- matrix(0, height, width)
                         search.mine <- which(private$minefield, arr.ind = TRUE)
                         mine.row <- search.mine[, 1]
                         mine.col <- search.mine[, 2]
                         # Calculate the number of mines in every 3x3 square
                         for (i in 1:mines) {
                           this_mine_row = mine.row[i]
                           this_mine_col = mine.col[i]
                           # ensure no negative index with intersect
                           rows_to_bump <- intersect(1:height, (this_mine_row - 1):(this_mine_row + 1))
                           cols_to_bump <- intersect(1:width, (this_mine_col - 1):(this_mine_col + 1))
                           mine_info[rows_to_bump, cols_to_bump] <- mine_info[rows_to_bump, cols_to_bump] + 1
                           # finally there is no such thing as info on a mine - you loose
                           mine_info[this_mine_row, this_mine_col] = NA
                         }
                         private$mine_info <- mine_info
                       },
                       print = function(...) {
                         cat("Board: \n")
                         cat("  Rows: ", self$height, "\n", sep = "")
                         cat("  Cols:  ", self$width, "\n", sep = "")
                         cat("  Mines:  ", self$mines, "\n", sep = "")
                         invisible(self)
                       }
                     ),
                     private = list(
                       minefield = NA,
                       mine_info = NA
                     ))
