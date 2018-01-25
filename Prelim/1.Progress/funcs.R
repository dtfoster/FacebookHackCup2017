

processFile = function(inFilePath, outFilePath) {
  inCon = file(inFilePath, "r")
  outCon = file(outFilePath, "w")
  
  linenum = 0
  while ( TRUE ) {
    line = readLines(inCon, n = 1)
    
    if ( length(line) == 0 ) {
      break
    }
    linenum = linenum + 1
    if (linenum == 1){
      T = as.integer(line)
    }else{
      vars = unlist(strsplit(line,split = " "))
      progress = as.numeric(vars[1])
      x = as.numeric(vars[2])
      y = as.numeric(vars[3])
      
      if (x == 50 & y == 50){
        answer = "black"
      }else{
        answer = calcColour(progress, x, y)
      }
      
      writeOut = paste0("Case #", linenum - 1, ": ", answer)
      writeLines(writeOut, outCon)
    }
  }
  close(inCon)
  close(outCon)
}






cosAngleGivenProgress = function(progress){
  return (cos(2 * pi * (progress/100)))
}


cosAngleGivenPoint = function(x,y){
  
  
  a = 50
  b = distanceCalc(x,y,50,100)
  c = distanceCalc(x,y,50,50)
  
  cosB = (a^2 + c^2 - b^2) / (2*a*c)
  
  return (cosB)
  
}


distanceCalc = function(x1,y1,x2,y2){
  return (sqrt((x1-x2)^2 + (y1-y2)^2))
}


calcColour = function(progress, x, y){
  
  
  distToCentre = distanceCalc(x,y,50,50)
  
  if (distToCentre>50){
    return ("white")
  }else{
    
    cosProgress = cosAngleGivenProgress(progress)
    cosPoint = cosAngleGivenPoint(x,y)
    
    if (progress<50 & x>=50){ 
      return (ifelse (cosProgress < cosPoint, "black", "white"))
    }else if (progress<50 & x<50) {
      return ("white")
    }else if (progress>=50 & x>=50) {
      return ("black")
    }else if (progress>=50 & x<50) {
      return (ifelse (cosProgress > cosPoint, "black", "white"))
    }
  }
}