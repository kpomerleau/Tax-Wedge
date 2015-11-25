#Calculates State income tax

StateIncomeTax<-function(statetaxableincome,married,hoh,stateparam){
  
  x<-1
  
  stateincometax<-0
  
  while(TRUE){
    
    #Calculates Income Pre-Credit income tax for married couples
    
    if(married == 1){
      
      if( statetaxableincome < stateparam$marriedbracket[x+1] & x < length(stateparam$marriedbracket)) {
        
        stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$marriedbracket[x] ) * stateparam$marriedrate[x] )
        
        break
        
      } else if( x == length(stateparam$marriedbracket) ) {
        
        stateincometax <- stateincometax + stateparam$marriedrate[x] * ( statetaxableincome - stateparam$marriedbracket[x] )
        
        break
        
      } else {
        
        stateincometax <- stateincometax + stateparam$marriedrate[x] * ( stateparam$marriedbracket[x+1] - stateparam$marriedbracket[x] )
        
        x<-x+1
        
      } 
      
      #Calculates Income Pre-Credit income tax for married couples
      
    } else if(married == 0){ 
      
      if( statetaxableincome < stateparam$singlebracket[x+1] & x < length(stateparam$singlebracket)){
        
        stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$singlebracket[x] ) * stateparam$singlerate[x] )
        
        break
        
      } else if( x == length(stateparam$singlebracket) ) {
        
        stateincometax <- stateincometax + stateparam$singlerate[x] * ( statetaxableincome - stateparam$singlebracket[x] )
        
        break
        
      } else {
        
        stateincometax <- stateincometax + stateparam$singlerate[x] * ( stateparam$singlebracket[x+1] - stateparam$singlebracket[x] )
        
        x<-x+1
        
      } 
      
      #Calculates Income Pre-Credit income tax for Head of Households
      
    } else if (hoh == 1){
      
      if( statetaxableincome < stateparam$hohbracket[x+1] & x < length(stateparam$hohbracket)){
        
        stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$hohbracket[x] ) * stateparam$hohrate[x] )
        
        break
        
      } else if( x == length(stateparam$hohbracket) ) {
        
        stateincometax <- stateincometax + stateparam$hohrate[x] * ( statetaxableincome - stateparam$hohbracket[x] )
        
        break
        
      } else {
        
        stateincometax <- stateincometax + stateparam$hohrate[x] * ( stateparam$hohbracket[x+1] - stateparam$hohbracket[x] )
        
        x<-x+1
        
      } 
      
    }
    
  }
  
  return(stateincometax)
  
}