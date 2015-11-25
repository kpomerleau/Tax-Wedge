#Federal Child Tax Credit

FedCTC<-function(income,children,married){
  
  #Child Tax Credit
  
  if(children>0) {
    
    c<-children
    
    if (married == 0) {
      
      if(income <= fedtax$ctcphasein[1]){
        
        ctc<-0
        
      } else if(income <= fedtax$ctcphaseout_single[1]){
        
        ctc<-min(fedtax$ctccredit[1]*c,((income-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))
        
      } else if(income > fedtax$ctcphaseout_single[1]){
        
        ctc<-max(0,(fedtax$ctccredit[1]*c)-( (ceiling((income-fedtax$ctcphaseout_single[1])*(1/1000))*1000) * fedtax$ctcphaseoutrate[1]) )
        
      }
      
    } else if (married == 1) {
      
      if(income <= fedtax$ctcphasein[1]) {
        
        ctc<-0
        
      } else if(income <= fedtax$ctcphaseout_married[1]) {
        
        ctc<-min(fedtax$ctccredit[1]*c,((income-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))
        
      } else if(income > fedtax$ctcphaseout_married[1]) {
        
        ctc<-max(0,(fedtax$ctccredit[1]*c)- (ceiling((income-fedtax$ctcphaseout_married[1])*(1/1000))*1000) *fedtax$ctcphaseoutrate[1])
        
      }
      
    }
    
  } else {
    
    ctc<-0
    
  }
  
  return(ctc)
  
}