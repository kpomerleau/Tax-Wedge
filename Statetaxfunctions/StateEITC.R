#Calculates State EITC. This needs work.

StateEITC<-function(eitc,stateparam){
  
  #State EITC
  
  stateeitc<-eitc*stateparam$eitcrate[1]
  
  return(stateeitc)
  
}