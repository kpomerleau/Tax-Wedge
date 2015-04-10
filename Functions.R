#This file stores all the functions necessary to calculate average and marginal tax rates

#############Federal Tax Functions###############################

  #federal taxable income
  
    FedTaxableIncome<-function(income, children, married, hoh, stateincometax){
    
    
    if(income>fedtax$pep_pease_threshold[1+married+(hoh*2)]){
      
      personalexemption<-max(0,(1-( ceiling(((income-fedtax$pep_pease_threshold[1+married+(hoh*2)])/2500)) *(.02)))*(fedtax$personal_exemption[1]*(1+children+married)))
      
    } else {
      
      personalexemption<-fedtax$personal_exemption[1]*(1+children+married)
      
    }
    
    #Calculating STANDARD/ITEMIZED DEDUCTION
    
    if(stateincometax>fedtax$standard_deduction[1+married+(hoh*2)]){
      
      if(income>fedtax$pep_pease_threshold[1+married+(hoh*2)]){
        
        deduction<-stateincometax-((income-fedtax$pep_pease_threshold[1+married+(hoh*2)])*.03)
        
      } else {
        
        deduction<-stateincometax
        
      }
      
    } else {
      
      deduction<-fedtax$standard_deduction[1+married+(hoh*2)]
      
    }
    
    taxableincome<-max(0,income-deduction-personalexemption)
    
    return(taxableincome)
    
  }
  
  #federal income tax
  
    FedIncomeTax<-function(taxableincome,married,hoh){
    
    #Pre-Credit Federal Income Tax Bill
    
    x<-1 #An index that counts through the tax brackets
    
    federalincometax<-0
    
    #To make this a little easier, I don't call on the variable names in the dataset. single:3,married:4,hoh:5
    
    while(TRUE){  
      
      if( taxableincome < fedtax[x+1,3+married+(hoh*2)] & x < length(fedtax$incometaxrate)){
        
        federalincometax <- federalincometax + ( ( taxableincome - fedtax[x,3+married+(hoh*2)] ) * fedtax$incometaxrate[x] )
        
        break
        
      } else {
        
        federalincometax <- federalincometax + fedtax$incometaxrate[x] * ( fedtax[x+1,3+married+(hoh*2)] - fedtax[x,3+married+(hoh*2)] )
        
        x<-x+1
        
      } 
      
      if( x == length(fedtax$incometaxrate) ) {
        
        federalincometax <- federalincometax + fedtax$incometaxrate[x] * ( taxableincome - fedtax[x,3+married+(hoh*2)] )
        
        break
        
      }
      
    }
    
    return(federalincometax)
    
  }
  
  #federal EITC
  
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
  
  #federal Child Tax Credit
  
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
  
  #Federal Employee Payroll Taxes
  
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
  
  #Federal Employer Payroll Taxes
  
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

  #Medicare Surtax

    MedSurtax<-function(income,married){
        
      x<-1 #An index that counts through the tax brackets
      
      medsurtax<-0
      
      #To make this a little easier, I don't call on the variable names in the dataset. single:3,married:4,hoh:5
      
      while(TRUE){  
     
        if(married == 0){
          
          if( income < fedtax$medsurtaxsinglebracket[x+1] & x < sum(!is.na(fedtax$medsurtaxsinglebracket))){
            
            medsurtax <- medsurtax + ( ( income - fedtax$medsurtaxsinglebracket[x] ) * fedtax$medsurtaxrate[x] )
            
            break
            
          } else {
            
            medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( fedtax$medsurtaxsinglebracket[x+1] - fedtax$medsurtaxsinglebracket[x] )
            
            x<-x+1
            
          } 
          
          if( x == sum(!is.na(fedtax$medsurtaxsinglebracket)) ) {
            
            medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( income - fedtax$medsurtaxsinglebracket[x] )
            
            break
            
          }
          
        } else 
          
        if(married == 1){
          
          if( income < fedtax$medsurtaxmarriedbracket[x+1] & x < sum(!is.na(fedtax$medsurtaxmarriedbracket))){
            
            medsurtax <- medsurtax + ( ( income - fedtax$medsurtaxmarriedbracket[x] ) * fedtax$medsurtaxrate[x] )
            
            break
            
          } else {
            
            medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( fedtax$medsurtaxmarriedbracket[x+1] - fedtax$medsurtaxmarriedbracket[x] )
            
            x<-x+1
            
          } 
          
          if( x == sum(!is.na(fedtax$medsurtaxmarriedbracket)) ) {
            
            medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( income - fedtax$medsurtaxmarriedbracket[x] )
            
            break
            
          }
          
        }      
      
      }
      
      return(medsurtax)   
      
    }
  
  #Federal UI Tax
  
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

  #Alternative Minimum Tax

    AMT<-function(income,married){
      
      if(married == 1){
        
        amti <- income - max(0,(fedtax$amtexemptionmarried[1] - max(0,(income - fedtax$amtphaseoutmarried[1])*.25)))
        
      } else {
        
        amti<- income - max(0,(fedtax$amtexemptionsingle[1] - max (0,(income - fedtax$amtphaseoutsingle[1])*.25)))
        
      }
      
      x<-1
      
      amt<-0
        
      while(TRUE){
        
        if( amti < fedtax$amtbracket[x+1] & x < sum(!is.na(fedtax$amtbracket))){
          
          amt <- amt + ( ( amti - fedtax$amtbracket[x] ) * fedtax$amtrate[x] )
          
          break
          
        } else {
          
          amt <- amt + fedtax$amtrate[x] * ( fedtax$amtbracket[x+1] - fedtax$amtbracket[x] )
          
          x<-x+1
          
        } 
        
        if( x == sum(!is.na(fedtax$amtbracket)) ) {
          
          amt <- amt + fedtax$amtrate[x] * ( amti - fedtax$amtbracket[x] )
          
          break
          
        }         
        
      }  
      
      return(amt)
      
    }

################State Tax Functions##############################

  #state parameters
  
    StateParameters<-function(state){
    
    stateparam<-statetax[statetax$id == state,]
    
    return(stateparam)
    
  }

  #State Taxable Income
  
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
  
  #State income tax
  
    StateIncomeTax<-function(statetaxableincome,married,hoh,stateparam){
    
    x<-1
    
    stateincometax<-0
    
    while(TRUE){
      
      #Calculates Income Pre-Credit income tax for married couples
      
      if(married == 1){
        
        if( statetaxableincome < stateparam$marriedbracket[x+1] & x < length(stateparam$marriedbracket)) {
          
          stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$marriedbracket[x] ) * stateparam$marriedrate[x] )
          
          break
          
        } else if( x == length(stateparam$marriedbracket) ) {
          
          stateincometax <- stateincometax + stateparam$marriedrate[x] * ( statetaxableincome - stateparam$marriedbracket[x] )
          
          break
        
        } else {
          
          stateincometax <- stateincometax + stateparam$marriedrate[x] * ( stateparam$marriedbracket[x+1] - stateparam$marriedbracket[x] )
          
          x<-x+1
          
        } 
        
      #Calculates Income Pre-Credit income tax for married couples
                
      } else if(married == 0){ 
        
        if( statetaxableincome < stateparam$singlebracket[x+1] & x < length(stateparam$singlebracket)){
          
          stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$singlebracket[x] ) * stateparam$singlerate[x] )
          
          break
          
        } else if( x == length(stateparam$singlebracket) ) {
          
          stateincometax <- stateincometax + stateparam$singlerate[x] * ( statetaxableincome - stateparam$singlebracket[x] )
          
          break
          
        } else {
          
          stateincometax <- stateincometax + stateparam$singlerate[x] * ( stateparam$singlebracket[x+1] - stateparam$singlebracket[x] )
          
          x<-x+1
          
        } 
        
      #Calculates Income Pre-Credit income tax for Head of Households
               
      } else if (hoh == 1){
        
        if( statetaxableincome < stateparam$hohbracket[x+1] & x < length(stateparam$hohbracket)){
          
          stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$hohbracket[x] ) * stateparam$hohrate[x] )
          
          break
          
        } else if( x == length(stateparam$hohbracket) ) {
          
          stateincometax <- stateincometax + stateparam$hohrate[x] * ( statetaxableincome - stateparam$hohbracket[x] )
          
          break
          
        } else {
          
          stateincometax <- stateincometax + stateparam$hohrate[x] * ( stateparam$hohbracket[x+1] - stateparam$hohbracket[x] )
          
          x<-x+1
          
        } 
        
      }
      
    }
    
    return(stateincometax)
    
  }
  
  #State EITC
  
    StateEITC<-function(eitc,stateparam){
    
    #State EITC
    
    stateeitc<-eitc*stateparam$eitcrate[1]
    
    return(stateeitc)
    
  }
  
  #State Personal Credit
  
    StatePersonalCredit<-function(income,statetaxableincome,married,hoh,children,stateparam){
    
    #State personal credit
    
    if(married == 1) {
      
      personalcredit <- stateparam$personalcreditmarried[1] + ( stateparam$personalcreditdependent[1] * children )
      
    } else if (married == 0){
      
      personalcredit <- stateparam$personalcreditsingle[1] + ( stateparam$personalcreditdependent[1] * children )
      
    }
    
    return(personalcredit)
    
  }
  
  #State UI Tax (Needs to be completed)
  
    StateUI<-function(income,married,stateparam){
    
      if(income<=stateparam$uibase[1]*(1+married)){
        
        stateui<-stateparam$uirate[1]*income
        
      } else {
        
        stateui<-stateparam$uirate[1]*stateparam$uibase[1]*(1+married)
        
      }
      
      return(stateui)
      
    
  }
  
