#medicare Surtax

MedSurtax<-function(income,married){
  
  x<-1 #An index that counts through the tax brackets
  
  medsurtax<-0
  
  #To make this a little easier, I don't call on the variable names in the dataset. single:3,married:4,hoh:5
  
  while(TRUE){  
    
    if(married == 0){
      
      if( income < fedtax$medsurtaxsinglebracket[x+1] & x < sum(!is.na(fedtax$medsurtaxsinglebracket))){
        
        medsurtax <- medsurtax + ( ( income - fedtax$medsurtaxsinglebracket[x] ) * fedtax$medsurtaxrate[x] )
        
        break
        
      } else {
        
        medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( fedtax$medsurtaxsinglebracket[x+1] - fedtax$medsurtaxsinglebracket[x] )
        
        x<-x+1
        
      } 
      
      if( x == sum(!is.na(fedtax$medsurtaxsinglebracket)) ) {
        
        medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( income - fedtax$medsurtaxsinglebracket[x] )
        
        break
        
      }
      
    } else 
      
      if(married == 1){
        
        if( income < fedtax$medsurtaxmarriedbracket[x+1] & x < sum(!is.na(fedtax$medsurtaxmarriedbracket))){
          
          medsurtax <- medsurtax + ( ( income - fedtax$medsurtaxmarriedbracket[x] ) * fedtax$medsurtaxrate[x] )
          
          break
          
        } else {
          
          medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( fedtax$medsurtaxmarriedbracket[x+1] - fedtax$medsurtaxmarriedbracket[x] )
          
          x<-x+1
          
        } 
        
        if( x == sum(!is.na(fedtax$medsurtaxmarriedbracket)) ) {
          
          medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( income - fedtax$medsurtaxmarriedbracket[x] )
          
          break
          
        }
        
      }      
    
  }
  
  return(medsurtax)   
  
}