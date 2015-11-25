#Gives State taxable income given income, children, filing status, federal income tax, and the specific state
#This also includes state-specifc adjustments to state taxable income


StateTaxableIncome<-function(income,children,married,hoh,federalincometax,stateparam){
  
  #Standard Deduction
  
  if(married == 1){ 
    
    standarddeduction <- stateparam$deductionmarried[1]
    
  } else if(married == 0){ 
    
    standarddeduction <- stateparam$deductionsingle[1]
    
  } 
  
  #Special state specific adjustments to the standard deduction
  
  #Alabama
  
  #Alabama's Standard Deduction is on a sliding scale between $2500 and $2000 
  #($7500 and $4000 married) between $20500 and $30,000.
  
  if(stateparam$stateName[1] == "Alabama" & income >= 20500){
    
    if(married == 0){
      
      standarddeduction <- max(2000,2500-((income-20500)*((2500-2000)/9500)))
      
    } else if (married == 1){
      
      standarddeduction <- max(4000,7500-((income-20500)*((7500-4000)/9500)))
      
    } else if (hoh == 1){
      
      standarddeduction <- max(2000,4700-((income-20500)*((4700-2000)/9500)))
      
    }
    
  } 
  
  #Personal Exemption
  
  #For taxpayer and spouse
  
  if(married == 1){ 
    
    personalexemption <- stateparam$personalexemptionmarried[1]
    
  } else if (married == 0){ 
    
    personalexemption <- stateparam$personalexemptionsingle[1]
    
  } else if (hoh == 1){
    
    personalexemption <- stateparam$personalexemptionhoh[1]
    
  }
  
  #Dependent Exemption
  
  dependentexemption <- ( stateparam$personalexemptiondependent[1] * children )
  
  #State specific adjustments to the personal exemption  
  
  #Alabama
  
  if(stateparam$stateName[1] == "Alabama"){  
    
    if(income > 20000 & income <= 100000){
      
      dependentexemption <- 500
      
    } else if (income>100000){
      
      dependentexemption <- 300
      
    }
    
  }
  
  #Final Calculation of personal exemption
  
  personalexemption <- personalexemption+dependentexemption
  
  #Federal Income Tax Deduction (For Specific States)
  
  #none yet
  
  #Final Calculation
  
  statetaxableincome<-max(0,income-standarddeduction-personalexemption-federalincometax)
  
  return(statetaxableincome)
  
}