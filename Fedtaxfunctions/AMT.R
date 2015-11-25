#Federal AMT

AMT<-function(income,married){
  
  if(married == 1){
    
    amti <- income - max(0,(fedtax$amtexemptionmarried[1] - max(0,(income - fedtax$amtphaseoutmarried[1])*.25)))
    
  } else {
    
    amti<- income - max(0,(fedtax$amtexemptionsingle[1] - max(0,(income - fedtax$amtphaseoutsingle[1])*.25)))
    
  }
  
  x<-1
  
  amt<-0
  
  while(TRUE){
    
    if( amti < fedtax$amtbracket[x+1] & x < sum(!is.na(fedtax$amtbracket))){
      
      amt <- amt + ( ( amti - fedtax$amtbracket[x] ) * fedtax$amtrate[x] )
      
      break
      
    } else {
      
      amt <- amt + fedtax$amtrate[x] * ( fedtax$amtbracket[x+1] - fedtax$amtbracket[x] )
      
      x<-x+1
      
    } 
    
    if( x == sum(!is.na(fedtax$amtbracket)) ) {
      
      amt <- amt + fedtax$amtrate[x] * ( amti - fedtax$amtbracket[x] )
      
      break
      
    }         
    
  }  
  
  return(amt)
  
}