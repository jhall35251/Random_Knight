rando_knight <- function(iterations=10){
  
  iterations <- iterations



path_length <- rep(0,iterations)


for (iter in 1:iterations) {
  
  # Set Initial Postion for Knight and clear the board
  state_board = matrix(0,nrow = 8, ncol = 8)
  state_board[1,1] <- 1
  row_pos = 1
  col_pos = 1
  
  # Set Completion Status
  complete_state <- 0 # will turn to 1 once knight touches every square
  path_length_counter <- 0 
  
  # Move Around until Knight Goes to Every Spot
  while (complete_state == 0) {
    
    move_succes <- 0
    
    while (move_succes==0) {
      
      ### Move the Knight Quick to Code but needs validation (see below)
      x<-sample(1:8,1) # draw a random number between 1 and 8
      ### Forcast the knight's acording to random number.
      
      if (x==1) {
        temp_row <- row_pos+2
        temp_col <- col_pos+1
      }
      
      if (x==2) {
        temp_row <- row_pos+1
        temp_col <- col_pos+2
      }
      
      if (x==3) {
        temp_row <- row_pos-2
        temp_col <- col_pos+1
      }
      
      if (x==4) {
        temp_row <- row_pos-1
        temp_col <- col_pos+2
      }
      
      if (x==5) {
        temp_row <- row_pos+2
        temp_col <- col_pos-1
      }
      
      if (x==6) {
        temp_row <- row_pos+1
        temp_col <- col_pos-2
      }
      
      if (x==7) {
        temp_row = row_pos-2
        temp_col = col_pos-1
      }
      
      if (x==8) {
        temp_row = row_pos-1
        temp_col = col_pos-2
      }
      
      ### Validate that the move is legal aka on the boar.
      if ((temp_row <=8)&(temp_row >=1)&(temp_col <=8)&(temp_col >=1)) {
        row_pos <- temp_row
        col_pos <- temp_col
        move_succes <- 1
        path_length_counter <- path_length_counter+1
      }
      
    }
    
    ### Update Stateboard
    state_board[row_pos,col_pos] <- 1
    
    ### Check Completion COnditions
    if (sum(state_board)==64) {
      
      complete_state <- 1
      
    }
    
    
    
  }
 
  path_length[iter]<-path_length_counter
  
}
return(path_length)

}

path_lengths <- rando_knight(1000)

summary(path_lengths)

hist(path_lengths)