#' R6 Class Representing a Game
#'
#' @description
#' A game consists of a board and a player, it also tracks the number of turns and is used to play the game.
#'
#' @details
#' Call the play function to play out a game, or call turn to step through a game
#'
#' @export
Game <- R6::R6Class("Game",
                     public = list(
                       # Borrowed from https://github.com/yihui/fun/blob/master/R/mine_sweeper.R
                       board = NA,
                       player = NA,
                       turn_count = 0,
                       game_over = FALSE,
                       game_result = NA,
                       turn = function(){
                         if(self$player$ready_to_submit){
                           submission = self$player$get_submission()
                           won = self$board$submit(submission)
                           if(won){
                             private$win()
                           } else {
                             private$loose()
                           }
                         } else {
                           question = self$player$get_next_question()
                           result = self$board$ask(question$x, question$y)
                           if(result$exploded){
                             private$loose()
                           } else{
                             self$turn_count = self$turn_count + 1
                             self$player$recieve_info(question = question,
                                                      info = result$info)
                           }
                         }
                         invisible(self)
                       },
                       play = function(){
                         while(!self$game_over){
                           self$turn()
                         }
                       },
                       initialize = function(board = Board$new(mines = 20),
                                             player = Player$new()) {
                         self$board <- board
                         self$player <- player
                       },
                       print = function(...) {
                         cat("Game: \n")
                         cat("  Turns:  ", self$turn_count, "\n", sep = "")
                         cat("  Game Over: ", self$game_over, "\n", sep = "")
                         cat("  Result: ", self$game_result, "\n", sep = "")
                         invisible(self)
                       }
                     ),
                     private = list(
                       loose = function(){
                         cat('You Lost!')
                         self$game_over = TRUE
                         self$game_result = 'Loose'
                         invisible(self)
                       },
                       win = function(){
                         cat('You Win!')
                         self$game_over = TRUE
                         self$game_result = 'Win'
                         invisible(self)
                       }
                     ))
