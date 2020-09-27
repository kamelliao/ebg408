i <- 2
j <- 1
cp.latter = 1
result = list()
trial.tag = rownames(mat.substract)
while(i < mat.n) {
  ith.rowdata = mat.substract[i,]
  tmp.substr.value = MaxIndex(ith.rowdata, cp.latter)[1]
  tmp.index = MaxIndex(ith.rowdata, cp.latter)[2] #也就是j，意義不大，只是最後要呈現
  
  if(!is.na(tmp.substr.value) & abs(tmp.substr.value) >= 5){
    record.data = TRUE
    repeat{
      substr.value = tmp.substr.value
      
      if (record.data){
        cp.latter = i #停下來的那個點暫存為Change Point
        cp.former = tmp.index
      }
      
      i <- i + 1
      next.ith.rowdata = mat.substract[i,]
      next.substr.value = MaxIndex(next.ith.rowdata, cp.latter)[1]
      next.index = MaxIndex(next.ith.rowdata, cp.latter)[2]
      if (abs(next.substr.value) >= 5 && next.substr.value*substr.value < 0){
        result[[i]] <- data.frame(cp_former = trial.tag[cp.former], cp_latter = trial.tag[cp.latter])
        i <- i
        j <- cp.latter
        break
        
      }else if (abs(next.substr.value) >= 5){
        record.data = TRUE

      }else {
        record.data = FALSE
      }
    }
    
  }else{
    i <- i+1
  }
}

result.table <- do.call(rbind, result)
