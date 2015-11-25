#Federal Employer-side Payroll Tax
#Assumes married couples earn 50-50

FedEmployerPayroll<-function(income,married){
  
  x<-1 #An index that counts through the payroll tax brackets
  
  employerpayrolltax<-0
  
  while(TRUE){  
    
    if( income < fedtax$emplrayrollbracket[x+1]*(1+married) & x < sum(!is.na( fedtax$emplrayrollbracket ) ) ){
      
      employerpayrolltax <- employerpayrolltax + ( ( income - fedtax$emplrayrollbracket[x]*(1+married) ) * fedtax$emplrpayrollrate[x] )
      
      break
      
    } else {
      
      employerpayrolltax <- employerpayrolltax + fedtax$emplrpayrollrate[x] * ( fedtax$emplrayrollbracket[x+1]*(1+married) - fedtax$emplrayrollbracket[x]*(1+married) )
      
      x<-x+1
      
    } 
    
    if( x == sum(!is.na(fedtax$emplrayrollbracket))){
      
      employerpayrolltax <- employerpayrolltax + fedtax$emplrpayrollrate[x] * ( income - fedtax$emplrayrollbracket[x]*(1+married) )
      
      break
      
    }
    
  }
  
  return(employerpayrolltax)
  
}