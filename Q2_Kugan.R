# extend "LETTERS" function to run from "A" to "ZZ"
ALPHABETS <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

# example
ALPHABETS[1:10]

#x coordinates
x<-c(60,180,80,140,20,100,200,140,40,100)
#y coordinates
y<-c(200,200,180,180,160,160,160,140,120,120)

plot(x,y)



# extend "LETTERS" function to run from "A" to "ZZ"
ALPHABETS <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

# example alphabets to check
ALPHABETS[1:10]

#placing coordinates into matrix
points <- matrix(c(x,y),ncol = 2)

rownames(points) <- ALPHABETS[1:10]
colnames(points) <- c("x","y")

points







# function to generate a random route
possibleway <- function(points) {
  start <- rownames(points)[1]
  ways <- sample(rownames(points)[-1])
  #return to the start point
  ways <- c(start,ways,start) 
  return(ways)
}




# example to check whether ways are find
ways <- possibleway(points)
ways

# This function is to calculate adjency matrix
CalAdjmat <- function(points) return(as.matrix(dist(points)))


# function to calculate the distance of paths 
distRoute <- function(AdjencyMat, ways) {
  dis <- 0
  for(i in 2:nrow(AdjencyMat)) {
    dis <- dis + AdjencyMat[ways[i-1],ways[i]]
  }
  return(dis)
}



sleep_for_a_minute <- function() { Sys.sleep(60) }


# brute force algorithm
BRUTEFORECE <- function(points) {
  
  
  
  # library to find possible combination
  library(combinat)
  the_great<-NULL
  the_shortest <- Inf
  for( i in 1:10){
    start <- rownames(points)[i] # start
    paths <- permn(rownames(points)[-i]) # list of every possible route
    
    
    
    
    AdjencyMat <- CalAdjmat(points)
    
    # initial route and distance
    best_path <- NULL
    shor_dis <- Inf
    
    
    
    # to find possible paths
    for(i in 1:length(paths)) {
      
      ways <- paths[[i]]
      ways <- c(start,ways,start)
      
      NewDis <- distRoute(AdjencyMat,ways)
      
      # update distance
      if(NewDis < shor_dis) {
        shor_dis <- NewDis
        best_path <- ways
      }
    }
    
    NewGreatDis<-shor_dis
    
    if(NewGreatDis < the_shortest){
      the_shortest <-NewGreatDis
      the_great <-best_path
    }
    
  }
  # returning values
  return(list(distance = the_shortest,ways = the_great))
  
  
  
  
}





start_time <- Sys.time()
#calling the bruteForce function 

bf<-BRUTEFORECE(points)

#The shortest possible distance from first point
bf

end_time <- Sys.time()
time.taken<- round(end_time - start_time)
time.taken





path_for_heu <- function (repre_int)
{
  
  if (repre_int==1)
    
  {
    value = 'A'
  }
  else if (repre_int==2)
    
  {
    value ='B'
  } 
  else if (repre_int==3)
    
  {
    value = 'C'
  }
  else if (repre_int==4)
  {
    value = 'D'
  }
  else if (repre_int==5)
  {
    value = 'E'
  }
  else if (repre_int==6)
    
  {
    value ='F'
  }
  else if (repre_int==7)
  {
    value = 'G'
  }
  else if (repre_int==8)
    
  {
    value = 'H'
  }
  else if (repre_int==9)
  {
    value = 'I'
  }  else if (repre_int==10)
    
  {
    value = 'J'
  }else
    print("Error")
  
  return (value)
}







sleep_for_a_minute <- function() { Sys.sleep(60) }
# Heuristic Approach algorithm
Heuristic <- function(begin) {
  
  
  AdjencyMat <- CalAdjmat(points)
  
  first_point <- begin
  
  
 size_of_row <- 10
  
  size_of_col <- 10
  
  length_points <- AdjencyMat
  
    first_col <- first_point
    
    
    pointer_row <- first_point
    pointer_col <- 1
  
  # this matrix used to store calculated distance
  length_matrix <- matrix(length_points,nrow=size_of_row, ncol=size_of_col)
  
  
  #this matrix is to trace the visited points 
  point_matrix <- matrix(rep(c(-1),each=size_of_col),nrow=1, ncol=size_of_col)
  
  
  
  point_matrix[1,first_point] <- -99  #to set the visited point, and the first point set by the user
  
  # stores shitest distance
  short_dis <- 9999
  
  # stores all the sum of shortest visited distance
  tot_dis <- 0 
  
  for (i in 1:100) 
  {
   
  if(pointer_col == 11)#reset the pointer to column one
    {
     
     print(paste(  path_for_heu(pointer_row) ))
    pointer_row <- next_pt
      
    
    pointer_col <- 1 #reseting the pointer_col into 1 
      
      
      tot_dis <- tot_dis + short_dis #calculating total distance 
      
      short_dis <- 9999  
      #marking the visited points
      point_matrix[1,next_pt] <- 1 
    }
    if(i == 100) 
    {
      print(paste(path_for_heu(first_col)))
      tot_dis <- tot_dis + length_matrix[next_pt,first_col]
      print(paste("== Total shortest distance is ", tot_dis))
    }
    
    if (length_matrix[pointer_row, pointer_col] < short_dis && length_matrix[pointer_row,pointer_col] != 0)
    {
      # to double check all the points are visited
      
      if(point_matrix[1,pointer_col] == -1) 
      {
        short_dis <- length_matrix[pointer_row, pointer_col] 
        
        
        next_pt <- pointer_col 
      }
      
    }
    
    pointer_col <- pointer_col + 1 
  }
  return (tot_dis)
}




for(ten_cities in 1:10)
{
  
  print("*******************************")
   start_heu <- Sys.time()
  Heuristic(ten_cities)
  end_heu <- Sys.time()
  cat("    Time taken",end_heu - start_heu , "min\n" )
}














