# import package
library(tidyverse)

# import data
options(scipen = 999)
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)

# function handy
NearestMaxDistance <- function(row.data, start, end){
  # define. Distance is the absolute value of
  # 'the difference value' among the given row data
  tmp.distance = row.data[[start]]
  tmp.NMDneighbor = start
  for (j in start:end){
    if (abs(row.data[[j]]) >= abs(tmp.distance)){
      tmp.distance = row.data[[j]]
      tmp.NMDneighbor = j
    }
  }
  
  # NearestMaxDistance returns the corresponding value (with sign) of
  # maximum Distance among the given row data, and its corresponding index
  return(c(distance = tmp.distance, NMDneighbor = tmp.NMDneighbor))
}

# ChangePoint Algorithm
ChangePointAlgorithm_1127 <- function(player.no){
  ##########################################################
  # 1. load data and preprocessing
  # parameter: player.no
  
  # load data by player no.
  # discard 101th row of data
  player.data <- fileset[[player.no]][-101,]
  
  # interested in only 'Trial', 'Action', and 'Stock'
  # include only 1, 100, 'inconsistent point'
  # definition: the trial at which action is inconsistent with previous trial
  if (player.no%%2 ==1){
    TAS.data <- data.frame(trial = player.data$Trials,
                           action = player.data$p1Decision,
                           stock = player.data$p1Stock) %>%
      mutate(., lag_action = lag(action)) %>%
      filter(., action != lag_action | trial == c(1,100))
  }else{
    TAS.data <- data.frame(trial = player.data$Trials,
                           action = player.data$p2Decision,
                           stock = player.data$p2Stock) %>%
      mutate(., lag_action = lag(action)) %>%
      filter(., action != lag_action | trial == c(1,100))
  }
  
  ##########################################################
  # 2. matrix representation
  # calculate matrix of subtraction
  mat.data <- rep(TAS.data$stock, length(TAS.data$stock))
  mat.n <- length(TAS.data$stock)
  mat.bycol <- matrix(mat.data, ncol = mat.n, byrow = FALSE, dimnames = list(TAS.data$trial, TAS.data$trial))
  mat.byrow <- matrix(mat.data, ncol = mat.n, byrow = TRUE, dimnames = list(TAS.data$trial, TAS.data$trial))
  
  # Stock[row_name] - Stock[col_name]
  mat.subtract <- mat.bycol - mat.byrow
  
  # discard the repeated upper right half of the matrix
  mat.subtract[upper.tri(mat.subtract)] <- NA
  
  # corresponding exact trial no.
  # will be used to reference trial no. in the algorithm
  exact.trial.no <- as.integer(rownames(mat.subtract))
  
  ##########################################################
  # 3. start algorithm
  # initial condition
  section.result <- list()
  i.result <- 1
  i.point <- 1
  i.previous.point <- 1
  
  # start algorithm
  while (i.point <= mat.n){
    # calculate NearestMaxDistance
    initial.NMD <- NearestMaxDistance(mat.subtract[i.point,], i.previous.point, i.point)
    
    # initialization
    # if pair with NearestMaxDistance >= 5 is found
    # keep searching to see if pattern continue
    if (abs(initial.NMD[['distance']]) >= 5){
      # temporarily save initial result
      start.point <- initial.NMD[['NMDneighbor']]
      end.point <- i.point
      
      # examine whether the pattern continues among the next 10 trials (after current end.point)
      # if continue, update end.point to the new one
      # there are chances that start.point may update too
      previous.NMD <- initial.NMD  # for comparison
      while (i.point <= mat.n & exact.trial.no[i.point] - exact.trial.no[end.point] <= 10){
        # directly save result if it is the last point
        if (i.point == mat.n){
          section.result[[i.result]] <- data.frame(start = exact.trial.no[start.point],
                                                   end = exact.trial.no[end.point],
                                                   type = case_when(
                                                     previous.NMD[['distance']] < 0 ~ "short-position",
                                                     previous.NMD[['distance']] > 0 ~ "long-position"
                                                   ))
          break
        }
        
        # start examine next point
        i.point <- i.point + 1
        
        next.NMD <- NearestMaxDistance(mat.subtract[i.point,], i.previous.point, i.point)
        original.evaluation <- previous.NMD[['distance']]*previous.NMD[['distance']]
        updated.evaluation <- next.NMD[['distance']]*previous.NMD[['distance']]
        
        # if pattern continue, update start point and end point of that range
        if (updated.evaluation > original.evaluation){
          start.point <- next.NMD[['NMDneighbor']]
          end.point <- i.point
          previous.NMD <- next.NMD
        }
      }
      
      # save final result
      section.result[[i.result]] <- data.frame(start = exact.trial.no[start.point],
                                               end = exact.trial.no[end.point],
                                               type = case_when(
                                                 previous.NMD[['distance']] < 0 ~ "short-position",
                                                 previous.NMD[['distance']] > 0 ~ "long-position"
                                               ))
      i.result <- i.result + 1
      
      # reset index
      i.point <- end.point
      i.previous.point <- end.point
      
    }else {
      i.point <- i.point + 1
    }
  }

  # output as dataframe
  section.result.df <- do.call(rbind, section.result)

  return(section.result.df)
}


