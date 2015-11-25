#Calculates Federal income tax given taxable income, marital status.

FedIncomeTax<-function(taxableincome,married,hoh){
  
  #Pre-Credit Federal Income Tax Bill
  
  x<-1 #An index that counts through the tax brackets
  
  federalincometax<-0
  
  #To make this a little easier, I don't call on the variable names in the dataset. single:3,married:4,hoh:5
  
  while(TRUE){  
    
    if( taxableincome < fedtax[x+1,3+married+(hoh*2)] & x < sum(!is.na(fedtax$incometaxrate))){
      
      federalincometax <- federalincometax + ( ( taxableincome - fedtax[x,3+married+(hoh*2)] ) * fedtax$incometaxrate[x] )
      
      break
      
    } else {
      
      federalincometax <- federalincometax + fedtax$incometaxrate[x] * ( fedtax[x+1,3+married+(hoh*2)] - fedtax[x,3+married+(hoh*2)] )
      
      x<-x+1
      
    } 
    
    if( x == sum(!is.na(fedtax$incometaxrate)) ) {
      
      federalincometax <- federalincometax + fedtax$incometaxrate[x] * ( taxableincome - fedtax[x,3+married+(hoh*2)] )
      
      break
      
    }
    
  }
  
  return(federalincometax)
  
}