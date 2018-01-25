

processFile = function(inFilePath, outFilePath) {
  inCon = file(inFilePath, "r")
  outCon = file(outFilePath, "w")
  
  casenum = 0
  T = as.integer(readLines(inCon, n = 1))
  
  while ( TRUE ) {
    N = as.numeric(readLines(inCon, n = 1))
    
    if ( length(N) == 0 ) {
      break
    }
    
    casenum = casenum + 1
    weights = as.numeric(readLines(inCon, n = N))
    
    answer = answerFunc(weights)
    
    writeOut = paste0("Case #", casenum, ": ", answer)
    writeLines(writeOut, outCon)
  }
  close(inCon)
  close(outCon)
}


answerFunc = function(weights){
  
  weights = sort(weights, decreasing = TRUE)
  numItems = length(weights)
  answer = 0
  totalsize = 0
  
  while(totalsize <= numItems){
    answer = answer + 1
    if (length(weights)==totalsize){
      break
    }else{
    batchsize = ceiling(50 / weights[answer])
    totalsize = totalsize + batchsize
    }
  }
  
  answer = answer - 1
  
  return (answer)
}
  
  
  
  

