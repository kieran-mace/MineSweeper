#' R6 Class Representing a Player
#'
#' @description
#' This class represents a very basic player that plays randomly and will never feel confident enough to submit a solution
#'
#' @details
#' More sophisticated players should inherit this class
#'
#' @export
Player <- R6::R6Class("Player",
                     public = list(
                       ready_to_submit = FALSE,
                       starting_info = NA,
                       learned_info = data.frame(x = numeric(), y = numeric(), info = numeric()),
                       get_submission = function(){
                         m <- rep(FALSE, self$starting_info$width * self$starting_info$height)
                         mine.index <- sample(self$starting_info$width * self$starting_info$height, self$starting_info$mines)
                         m[mine.index] <- TRUE
                         # Save board
                         guess <- matrix(m, self$starting_info$height, self$starting_info$width)
                         return(guess)
                       },
                       get_next_question = function(){
                         question = list()
                         question$x = sample(self$starting_info$width, 1)
                         question$y = sample(self$starting_info$height, 1)
                         return(question)
                       },
                       recieve_info = function(question, info){
                         self$learned_info = rbind(self$learned_info,
                                                   data.frame(x = question$x,
                                                              y = question$y,
                                                              info = info))
                         invisible(self)
                       },
                       submit = function(candidate_minefield){
                         return(all(candidate_minefield == private$minefield))
                       },
                       initialize = function(height = 10, width = 10, mines = 10) {
                         self$starting_info = list(height = height,
                                                   width  = width,
                                                   mines = mines)
                       },
                       print = function(...) {
                         cat("Player: \n")
                         invisible(self)
                       }
                     ))



















