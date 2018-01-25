
processFile = function(inFilePath, outFilePath) {
  inCon = file(inFilePath, "r")
  outCon = file(outFilePath, "w")
  
  T = as.integer(readLines(inCon, n = 1))
  
  for (casenum in 1:T){
    
    info = unlist(strsplit(readLines(inCon, n = 1), " "))
    hitpoints = as.numeric(info[1])
    numSpells = as.integer(info[2])
    
    spells = unlist(strsplit(readLines(inCon, n = 1)," "))
    
    answer = answerFunc(hitpoints, spells)
    
    writeOut = paste0("Case #", casenum, ": ", answer)
    writeLines(writeOut, outCon)
  }
  close(inCon)
  close(outCon)
}


answerFunc = function(hitpoints, spells){
  
  answer = -1
  
  for (spell in spells){
    
    spellStats = getSpellStats(spell)
    
    out = getSumProbs(
      ndicePerRoll = spellStats$numDice
      ,nsidesPerDie = spellStats$diceType
      ,sumModifier = spellStats$addOn
      ,perDieMinOfOne = FALSE)
    
  
    idx = which(out$probabilities[,'Sum'] >= hitpoints)   
    prob = sum(out$probabilities[,'Probability'][idx])
    
    if (prob > answer){
      answer = prob
    }
    
  }
  
  return (answer)
}


getSpellStats = function(spell){
  
  tmp = unlist(strsplit(spell, "d|\\+|\\-"))
  numDice = as.integer(tmp[1])
  diceType = as.integer(tmp[2])
  addOnSign = ifelse(grepl("\\+", spell), 1, -1)
  addOn = as.numeric(tmp[3]) * addOnSign
  addOn = ifelse(is.na(addOn),0,addOn)     
  
  
  return (list(numDice = numDice, diceType = diceType, addOn = addOn))
  
}




library(dice)

inFilePath = './input/fighting_the_zombie.txt'
outFilePath = './output/fighting_the_zombie.txt'

processFile(inFilePath, outFilePath)