#State personal credits (some states use these instead of personal exemptions)

StatePersonalCredit<-function(income,statetaxableincome,married,hoh,children,stateparam){
  
  #State personal credit
  
  if(married == 1) {
    
    personalcredit <- stateparam$personalcreditmarried[1] + ( stateparam$personalcreditdependent[1] * children )
    
  } else if (married == 0){
    
    personalcredit <- stateparam$personalcreditsingle[1] + ( stateparam$personalcreditdependent[1] * children )
    
  }
  
  return(personalcredit)
  
}