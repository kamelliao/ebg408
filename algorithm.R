# import data and package
library(tidyverse)
library(plotly)
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)

# set data
player.no <- 41
player.data <- fileset[[player.no]][-101,]

dta <- data.frame(trial = player.data$Trials,
                  action = player.data$p1Decision,
                  stock = player.data$p1Stock) %>%
       mutate(., lag_action = lag(action)) %>%
       filter(., action != lag_action)

mat.data <- rep(dta$stock, length(dta$stock))
mat.n <- length(dta$stock)
mat.bycol <- matrix(mat.data, ncol = mat.n, byrow = FALSE, dimnames = list(dta$trial, dta$trial))
mat.byrow <- matrix(mat.data, ncol = mat.n, byrow = TRUE, dimnames = list(dta$trial, dta$trial))
mat.substract <- mat.bycol - mat.byrow

for (i in 1:mat.n){
  for (j in 1:mat.n){
    if(i <= j){
      mat.substract[i,j] = NA
    }
  }
}
# plot
plt <- ggplot(dta, aes(x=trial, y=stock, color=action)) +
  geom_point()
ggplotly(plt)

# function handy
MaxIndex <- function(vctr, cp){
  tmp.value = vctr[[cp]]
  tmp.j = cp
  for (j in cp:length(vctr)){
    if (!is.na(vctr[[j]])){
      if (abs(vctr[[j]]) > abs(tmp.value)){
        tmp.value = vctr[[j]]
        tmp.j = j
      }else{
        tmp.value = tmp.value
        tmp.j = tmp.j
      }
    }else{
      break
    }
    return(c(tmp.value, tmp.j))
  }
  
}

MaxIndex <- function(vctr, cp){
  tmp.value = vctr[[cp]]
  tmp.j = cp
  for (j in cp:length(vctr)){
    if (!is.na(vctr[[j]]) & abs(vctr[[j]]) > abs(tmp.value)){
        tmp.value = vctr[[j]]
        tmp.j = j
    }else{
      tmp.value = tmp.value
      tmp.j = tmp.j
    }
  }
  return(c(tmp.value, tmp.j))
  
}
MaxIndex(mat.substract[8,],7)

# algorithm
i <- 2
j <- 1

while(i < mat.n){
  ith.rowdata = mat.substract[i, i:mat.n]
  tmp.substr.value = MaxIndex(ith.rowdata)[1]
  tmp.index = MaxIndex(ith.rowdata)[2]
  
  if (abs(tmp.substr.value) >= 5){
    substr_value = tmp.substr.value
    cp_former = tmp.index
    cp_latter = i
    
    i = i+1
    ith.rowdata = mat.substract[i, cp_latter:mat.n]
    tmp.substr.value = MaxIndex(ith.rowdata)[1]
    tmp.index = MaxIndex(ith.rowdata)[2]
    repeat {
      if (abs(tmp.substr.value) >= 5 && tmp.substr.value*substr_value < 0) break
      
    }
  } else{
    i = i+1
  }
  
}

