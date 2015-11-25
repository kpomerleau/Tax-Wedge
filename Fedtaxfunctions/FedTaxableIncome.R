
#Calculates Federal Taxable income based on wage income, children, marital status.
#Includes both the Pease limitation on itemized deductions and phase-out of personal exemption.
#Calculates itemized deductions based on state income tax

FedTaxableIncome<-function(income, children, married, hoh, stateincometax){
  
  
  if(income>fedtax$pep_pease_threshold[1+married+(hoh*2)]){
    
    personalexemption<-max(0,(1-(ceiling(((income-fedtax$pep_pease_threshold[1+married+(hoh*2)])/2500)) *(.00)))*(fedtax$personal_exemption[1]*(1+children+married))) #PeP Turned off here
    
  } else {
    
    personalexemption<-fedtax$personal_exemption[1]*(1+children+married)
    
  }
  
  #Calculating STANDARD/ITEMIZED DEDUCTION
  
  if(stateincometax>fedtax$standard_deduction[1+married+(hoh*2)]){
    
    if(income>fedtax$pep_pease_threshold[1+married+(hoh*2)]){
      
      deduction<-stateincometax-((income-fedtax$pep_pease_threshold[1+married+(hoh*2)])*.00) #Pease turned off here
      
    } else {
      
      deduction<-stateincometax
      
    }
    
  } else {
    
    deduction<-fedtax$standard_deduction[1+married+(hoh*2)]
    
  }
  
  taxableincome<-max(0,income-deduction-personalexemption)
  
  return(taxableincome)
  
}