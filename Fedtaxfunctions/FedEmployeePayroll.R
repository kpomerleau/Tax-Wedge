#Federal Employee Payroll Tax
#assumes married couples earn 50-50

FedEmployeePayroll<-function(income,married){
  
  #Employee payroll taxes
  
  x<-1 #An index that counts through the payroll tax brackets
  
  employeepayrolltax<-0
  
  while(TRUE){  
    
    if( income < fedtax$emppayrollbracket[x+1]*(1+married) & x < sum(!is.na( fedtax$emppayrollbracket ) ) ){
      
      employeepayrolltax <- employeepayrolltax + ( ( income - fedtax$emppayrollbracket[x]*(1+married) ) * fedtax$emppayrollrate[x] )
      
      break
      
    } else {
      
      employeepayrolltax <- employeepayrolltax + fedtax$emppayrollrate[x] * ( fedtax$emppayrollbracket[x+1]*(1+married) - fedtax$emppayrollbracket[x]*(1+married) )
      
      x<-x+1
      
    } 
    
    if( x == sum(!is.na(fedtax$emppayrollbracket))){
      
      employeepayrolltax <- employeepayrolltax + fedtax$emppayrollrate[x] * ( income - fedtax$emppayrollbracket[x]*(1+married) )
      
      break
      
    }
    
  }
  
  return(employeepayrolltax)
  
}