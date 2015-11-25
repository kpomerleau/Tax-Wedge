#Calculates Federal EITC given income (assumed to be all wage income), children, and marital status

FedEITC<-function(income,children,married){
  
  c<-min(children,3)
  
  if(married == 0){
    
    if(income < fedtax$eitc_threshold[1+c]) {
      
      eitc<-income*(fedtax$eitc_max[1+c]/fedtax$eitc_threshold[1+c])
      
    } else if (income >= fedtax$eitc_threshold[1+c] & income <= fedtax$eitc_phaseout_single[1+c]) {
      
      eitc<-fedtax$eitc_max[1+c]
      
    } else if (income > fedtax$eitc_phaseout_single[1+c]) {
      
      eitc<-max(0,fedtax$eitc_max[1+c]+((fedtax$eitc_phaseout_single[1+c]-income)*(fedtax$eitc_max[1+c]/(fedtax$eitc_maxincome_single[1+c]-fedtax$eitc_phaseout_single[1+c]))))
      
    }
    
  } else if (married == 1) {
    
    if(income < fedtax$eitc_threshold[1+c]) {
      
      eitc<-income*(fedtax$eitc_max[1+c]/fedtax$eitc_threshold[1+c])
      
    } else if (income >= fedtax$eitc_threshold[1+c] & income <= fedtax$eitc_phaseout_married[1+c]) {
      
      eitc<-fedtax$eitc_max[1+c]
      
    } else if (income > fedtax$eitc_phaseout_married[1+c]) {
      
      eitc<-max(0,fedtax$eitc_max[1+c]+((fedtax$eitc_phaseout_married[1+c]-income)*(fedtax$eitc_max[1+c]/(fedtax$eitc_maxincome_married[1+c]-fedtax$eitc_phaseout_married[1+c]))))
      
    }
    
  }
  
  return(eitc)
  
}