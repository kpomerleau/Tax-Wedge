#Rubio-Lee and Rubio Child Tax Credit

RubioCTC<-function(income,children,married){
  
  #Child Tax Credit
  
  if(children>0) {
    
    rubioctc<-min((income * .153), 2500*children)
    
  } else {
    
    rubioctc<-0
    
  }
  
  return(rubioctc)
  
}