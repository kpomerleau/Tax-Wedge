#State unemployment insurance payroll tax

StateUI<-function(income,married,stateparam){
  
  if(income<=stateparam$uibase[1]*(1+married)){
    
    stateui<-stateparam$uirate[1]*income
    
  } else {
    
    stateui<-stateparam$uirate[1]*stateparam$uibase[1]*(1+married)
    
  }
  
  return(stateui)
  
  
}