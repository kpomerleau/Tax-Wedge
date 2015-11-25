#Federal Unemployment Insurance employer-side payroll tax
#Assumes married couples earn 50-50

FedUI<-function(income,married,stateui){
  
  if(income<=fedtax$ui[1]*(1+married)){
    
    fedui<-fedtax$ui[2]*income
    
  } else {
    
    fedui<-fedtax$ui[2]*fedtax$ui[1]*(1+married)
    
  }
  
  #90 percent credit (minimum federal ui tax is $42, max is $420)
  
  fedui<-max(fedui*.1,fedui-stateui)
  
  return(fedui)
  
}