setwd("C:/Users/kep/Documents/GitHub/Tax-Wedge")
rm(list=ls()) 

#This spreadsheet has all the income data and will show all the calculations, step-by-step.
income<-read.csv("income.csv", header = TRUE, fill = TRUE, sep = ",")
statetax<-read.csv("statetaxdata.csv", header = TRUE, fill = TRUE, sep = ",")
fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

#############Federal Tax Functions###############################

  #federal taxable income

    FedTaxableIncome<-function(income, children, married, hoh, stateincometax){
  
  
  if(income>fedtax$pep_pease_threshold[1+married+(hoh*2)]){
    
    personalexemption<-max(0,(1-(((income-fedtax$pep_pease_threshold[1+married+(hoh*2)])/2500)*.02)))*(fedtax$personal_exemption[1]*(1+children+married))
    
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
            
          } else if(income < fedtax$ctcphaseout_single[1]){
            
            ctc<-min(fedtax$ctccredit[1]*c,((income-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))
            
          } else if(income >= fedtax$ctcphaseout_single[1]){
            
            ctc<-max(0,(fedtax$ctccredit[1]*c)-((income-fedtax$ctcphaseout_single[1])*fedtax$ctcphaseoutrate[1]))
            
          }
          
        } else if (married == 1) {
          
          if(income<=fedtax$ctcphasein[1]) {ctc<-0} 
          
          else if(income<fedtax$ctcphaseout_married[1]) {ctc<-min(fedtax$ctccredit[1]*c,((income-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))
                                                         
          } else if(income>fedtax$ctcphaseout_married[1]) {ctc<-max(0,(fedtax$ctccredit[1]*c)-(income-fedtax$ctcphaseout_single[1])*fedtax$ctcphaseoutrate[1])}
          
        }
        
      } else {ctc<-0}
      
      return(ctc)
      
    }

  #Federal Employee Payroll Taxes

    FedEmployeePayroll<-function(income,married){
      
      #Employee payroll taxes
      
      x<-1 #An index that counts through the payroll tax brackets
      
      employeepayrolltax<-0
      
      while(TRUE){  
        
        if( income < fedtax$emppayrollbracket[x+1] & x < sum(!is.na( fedtax$emppayrollbracket ) ) ){
          
          employeepayrolltax <- employeepayrolltax + ( ( income - fedtax$emppayrollbracket[x] ) * fedtax$emppayrollrate[x] )
          
          break
          
        } else {
          
          employeepayrolltax <- employeepayrolltax + fedtax$emppayrollrate[x] * ( fedtax$emppayrollbracket[x+1] - fedtax$emppayrollbracket[x] )
          
          x<-x+1
          
        } 
        
        if( x == sum(!is.na(fedtax$emppayrollbracket))){
          
          employeepayrolltax <- employeepayrolltax + fedtax$emppayrollrate[x] * ( income - fedtax$emppayrollbracket[x] )
          
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
        
        if( income < fedtax$emplrayrollbracket[x+1] & x < sum(!is.na( fedtax$emplrayrollbracket ) ) ){
          
          employerpayrolltax <- employerpayrolltax + ( ( income - fedtax$emplrayrollbracket[x] ) * fedtax$emplrpayrollrate[x] )
          
          break
          
        } else {
          
          employerpayrolltax <- employerpayrolltax + fedtax$emplrpayrollrate[x] * ( fedtax$emplrayrollbracket[x+1] - fedtax$emppayrollbracket[x] )
          
          x<-x+1
          
        } 
        
        if( x == sum(!is.na(fedtax$emppayrollbracket))){
          
          employerpayrolltax <- employerpayrolltax + fedtax$emplrpayrollrate[x] * ( income - fedtax$emppayrollbracket[x] )
          
          break
          
        }
        
      }
      
      return(employerpayrolltax)
      
    }

  #Federal UI Tax

    FedUI<-function(income,married,stateui){
      
      if(income<=fedtax$ui[1]){
        
        fedui<-fedtax$ui[2]*income
        
      } else {
        
        fedui<-fedtax$ui[2]*fedtax$ui[1]
        
      }
      
      #90 percent credit (minimum federal ui tax is $42, max is $420)
      
      fedui<-max(fedui*.1,fedui-stateui)
      
      return(fedui)
      
    }

  #full federal taxwedge function (just for fun)

    FederalTaxWedge<-function(income, children, married, hoh, stateincometax){
  
#############Federal Taxable Income##############################
  
  #Calculating PERSONAL EXEMPTION
    
    if(income>fedtax$pep_pease_threshold[1+married+(hoh*2)]){
      
      personalexemption<-max(0,(1-(((income-fedtax$pep_pease_threshold[1+married+(hoh*2)])/2500)*.02)))*(fedtax$personal_exemption[1]*(1+children+married))
    
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

#############Federal Income Tax################################

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

###########Child Tax Credit##########

  #Child Tax Credit

    if(children>0) {
      
      c<-children
      
      if (married == 0) {
             
        if(income <= fedtax$ctcphasein[1]){
          
          ctc<-0
          
          } else if(income < fedtax$ctcphaseout_single[1]){
            
            ctc<-min(fedtax$ctccredit[1]*c,((income-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))
            
            } else if(income >= fedtax$ctcphaseout_single[1]){
              
              ctc<-max(0,(fedtax$ctccredit[1]*c)-((income-fedtax$ctcphaseout_single[1])*fedtax$ctcphaseoutrate[1]))
            
              }
               
      } else if (married == 1) {
               
          if(income<=fedtax$ctcphasein[1]) {ctc<-0} 
          
          else if(income<fedtax$ctcphaseout_married[1]) {ctc<-min(fedtax$ctccredit[1]*c,((income-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))
                                                      
              } else if(income>fedtax$ctcphaseout_married[1]) {ctc<-max(0,(fedtax$ctccredit[1]*c)-(income-fedtax$ctcphaseout_single[1])*fedtax$ctcphaseoutrate[1])}
          
      }
    
    } else {ctc<-0}

#####Earned Income Tax credit#######

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

##########Federal Income Tax with Tax Credits#############

  netfederalincometax<-federalincometax-ctc-eitc

#############Federal Payroll Taxes########################

#Employee payroll taxes

  x<-1 #An index that counts through the payroll tax brackets
  
  employeepayrolltax<-0

  while(TRUE){  
    
    if( income < fedtax$emppayrollbracket[x+1] & x < sum(!is.na( fedtax$emppayrollbracket ) ) ){
      
      employeepayrolltax <- employeepayrolltax + ( ( income - fedtax$emppayrollbracket[x] ) * fedtax$emppayrollrate[x] )
      
      break
      
    } else {
      
      employeepayrolltax <- employeepayrolltax + fedtax$emppayrollrate[x] * ( fedtax$emppayrollbracket[x+1] - fedtax$emppayrollbracket[x] )
      
      x<-x+1
      
    } 
    
    if( x == sum(!is.na(fedtax$emppayrollbracket))){
      
      employeepayrolltax <- employeepayrolltax + fedtax$emppayrollrate[x] * ( income - fedtax$emppayrollbracket[x] )
      
      break
      
    }
    
  }

#Employer Social Security and Payroll Taxes

  x<-1 #An index that counts through the payroll tax brackets
  
  employerpayrolltax<-0

  while(TRUE){  
    
    if( income < fedtax$emplrayrollbracket[x+1] & x < sum(!is.na( fedtax$emplrayrollbracket ) ) ){
      
      employerpayrolltax <- employerpayrolltax + ( ( income - fedtax$emplrayrollbracket[x] ) * fedtax$emplrpayrollrate[x] )
      
      break
      
    } else {
      
      employerpayrolltax <- employerpayrolltax + fedtax$emplrpayrollrate[x] * ( fedtax$emplrayrollbracket[x+1] - fedtax$emppayrollbracket[x] )
      
      x<-x+1
      
    } 
    
    if( x == sum(!is.na(fedtax$emppayrollbracket))){
      
      employerpayrolltax <- employerpayrolltax + fedtax$emplrpayrollrate[x] * ( income - fedtax$emppayrollbracket[x] )
      
      break
      
    }
    
  }

  #Federal Unemployment Insurance Tax
    
      stateui<-0.03*16747 #This is a test amount for the federal uitax credit.

      if(income<=fedtax$ui[1]){
        
        fedui<-fedtax$ui[2]*income
        
      } else {

        fedui<-fedtax$ui[2]*fedtax$ui[1]
        
      }

        #90 percent credit (minimum federal ui tax is $42, max is $420)

          fedui<-max(fedui*.1,fedui-stateui)

######Federal Tax Wedge Output######

#Possible Parameters

  #income
  #netfederalincometax
  #stateincometax
  #employeepayrolltax
  #employerpayrolltax
  #fedui
  #taxableincome
  #deduction
  #personalexemption
  #ctc
  #eitc

return(netfederalincometax+employeepayrolltax+stateincometax)

}

#############State Tax Functions####################

  #state parameters

    StateParameters<-function(state){
      
      stateparam<-statetax[statetax$id == state,]
     
      return(stateparam)
      
    }

  #state taxable income

    StateTaxableIncome<-function(income,children,married,hoh,federalincometax,stateparam){
      
      #Standard Deduction
      
      if(married == 1){ 
        
        standarddeduction <- stateparam$deductionmarried[1]
        
      } else if(married == 0){ 
        
        standarddeduction <- stateparam$deductionsingle[1]
        
      } 
      
      #Special state specific adjustments to the standard deduction
      
        #none yet.
      
      #Personal Exemption
      
        if(married == 1){ 
          
          personalexemption <- stateparam$personalexemptionsingle[1]
                          
        } else { 
            
          personalexemption <- stateparam$deductionsingle[1]
                 
        }
      
        personalexemption <- personalexemption + ( stateparam$personalexemptiondependent[1] * children)
      
      #State specific adjustments to the personal exemption  
      
        #none yet
      
      #Federal Income Tax Deduction (For Specific States)
      
        #none yet
      
      #Final Calculation
      
        statetaxableincome<-income-standarddeduction-personalexemption
      
      return(statetaxableincome)
      
    }

  #State income tax

    StateIncomeTax<-function(statetaxableincome,married,hoh,stateparam){
      
      x<-1
      
      stateincometax<-0
      
      while(TRUE){
        
        if(married == 1){ #Calculates Income Pre-Credit income tax for married couples
          
          if( statetaxableincome < stateparam$marriedbracket[x+1] & x < length(stateparam$marriedbracket)){
            
            stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$marriedbracket[x] ) * stateparam$marriedrate[x] )
            
            break
            
          } else {
            
            stateincometax <- stateincometax + stateparam$marriedrate[x] * ( stateparam$marriedbracket[x+1] - stateparam$marriedbracket[x] )
            
            x<-x+1
            
          } 
          
          if( x == length(stateparam$marriedbracket) ) {
            
            stateincometax <- stateincometax + stateparam$marriedrate[x] * ( statetaxableincome - stateparam$marriedbracket[x] )
            
            break
            
          }
          
        } else if (married == 0) { #Calculates Pre-Credit income tax for singles
          
          if( statetaxableincome < stateparam$singlebracket[x+1] & x < length(stateparam$singlebracket)){
            
            stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$singlebracket[x] ) * stateparam$singlerate[x] )
            
            break
            
          } else {
            
            stateincometax <- stateincometax + stateparam$singlerate[x] * ( stateparam$singlebracket[x+1] - stateparam$singlebracket[x] )
            
            x<-x+1
            
          } 
          
          if( x == length(stateparam$marriedbracket) ) {
            
            stateincometax <- stateincometax + stateparam$singlerate[x] * ( statetaxableincome - stateparam$singlebracket[x] )
            
            break
            
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
      
      
      
    }

  #Full state tax wedge function (for fun)

    StateTaxWedge<-function(income, children, married, hoh, state, eitc){
  
########Grab State Parameters Needed##############

  stateparam<-statetax[statetax$id == state,]

###########State Taxable Income###################
  
  #Standard Deduction

    if(married == 1){ 
      
        standarddeduction <- stateparam$deductionmarried[1]
                     
      } else if(single == 1){ 
        
        standardeduction <- stateparam$deductionsingle[1]
    
      } 
    
    #Special state specific adjustments to the standard deduction

      #none yet.

  #Personal Exemption

    if(married == 1){ personalexemption <- stateparam$personalexemptionsingle[1]
                      
      } else { personalexemption <- stateparam$deductionsingle[1]
             
      }

      personalexemption <- personalexemption + ( stateparam$personalexemptiondependent[1] * children)
  
    #State specific adjustments to the personal exemption  

      #none yet

  statetaxableincome<-income-standarddeduction-personalexemption

##############State Income Tax#######################

  x<-1
  
  stateincometax<-0

  while(TRUE){
    
    if(married == 1){ #Calculates Income Pre-Credit income tax for married couples
      
      if( statetaxableincome < stateparam$marriedbracket[x+1] & x < length(stateparam$marriedbracket)){
        
        stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$marriedbracket[x] ) * stateparam$marriedrate[x] )
        
        break
        
      } else {
        
        stateincometax <- stateincometax + stateparam$marriedrate[x] * ( stateparam$marriedbracket[x+1] - stateparam$marriedbracket[x] )
        
        x<-x+1
        
      } 
      
      if( x == length(stateparam$marriedbracket) ) {
        
        stateincometax <- stateincometax + stateparam$marriedrate[x] * ( statetaxableincome - stateparam$marriedbracket[x] )
        
        break
        
      }
      
    } else if (married == 0) { #Calculates Pre-Credit income tax for singles
      
      if( statetaxableincome < stateparam$singlebracket[x+1] & x < length(stateparam$singlebracket)){
        
        stateincometax <- stateincometax + ( ( statetaxableincome - stateparam$singlebracket[x] ) * stateparam$singlerate[x] )
        
        break
        
      } else {
        
        stateincometax <- stateincometax + stateparam$singlerate[x] * ( stateparam$singlebracket[x+1] - stateparam$singlebracket[x] )
        
        x<-x+1
        
      } 
      
      if( x == length(stateparam$marriedbracket) ) {
        
        stateincometax <- stateincometax + stateparam$singlerate[x] * ( statetaxableincome - stateparam$singlebracket[x] )
        
        break
      
      }
    
    }

  }

##############After Credit Income Tax###########

  #State personal credit

    if(married == 1) {
      
      personalcredit <- stateparam$personalcreditmarried[1] + ( stateparam$personalcreditdependent[1] * children )
      
    } else if (married == 0){
      
      personalcredit <- stateparam$personalcreditsingle[1] + ( stateparam$personalcreditdependent[1] * children )
      
    }

  stateincometax<-max(0,(stateincometax-personalcredit))

  #State EITC
  
  stateeitc<-eitc*stateparam$eitcrate[1]

  #need to determine whether the state EITC is refundable against state taxes

    if(stateparam$eitcrefund[1] == 0){
      
      stateincometax<-max(0,stateincometax-stateeitc)
      
    } else {
      
      stateincometax<-stateincometax-stateeitc
      
    }

############State Tax Wedge Function Output########

  #Output here:

  return(stateincometax)

}


####################################Calculations################################################

TotalTaxBurden<-function(income, children, married, hoh){

  #Step 1: Grab State
  
    stateparam<-StateParameters(state)

  #Step 2: State Taxable Income
    
    #Place holder for states with deductability
  
      federalincometax<-0
    
    statetaxableincome<-StateTaxableIncome(income,children,married,hoh,federalincometax,stateparam)

  #Step 3: State Income Tax

    stateincometax<-StateIncomeTax(statetaxableincome,married,hoh,stateparam)

  #Step 4: State EITC

    #First need federal EITC:

      eitc<-FedEITC(income,children,married)

    stateeitc<-StateEITC(eitc,stateparam)

  #Step 5: Personal Credit (if applicable)

    personalcredit<-StatePersonalCredit(income,statetaxableincome,married,hoh,children,stateparam)
  
  #Step 6: Total State Income Tax
    
    #If the EITC is not Refundable, state income tax needs a 0 lower bound    

      if(stateparam$eitcrefund[1] == 1){
        
        stateincometax<-stateincometax-personalcredit-stateeitc
        
      } else {
        
        stateincometax<-max(0,stateincometax-personalcredit-eitc)
        
      }

  #Step 7: Federal Taxable Income

    taxableincome<-FedTaxableIncome(income, children, married, hoh, stateincometax)

  #Step 8: Federal Income Tax

    federalincometax<-FedIncomeTax(taxableincome,married,hoh)

  #Step 9: Child Tax Credit

    ctc<-FedCTC(income,children,married)

  #Federal Income Tax
  
    federalincometax<-federalincometax-ctc-eitc
  
  #Reserved for Recalculating fed deductability

  #Payroll Taxes

    employeepayrolltax<-FedEmployeePayroll(income,married)
  
  #Total Tax Burden
  
    taxburden<-stateincometax+federalincometax+employeepayrolltax
  
  return(stateincometax)

}

##############Tax Parameters###########

state<-6
children<-0
married<-0
hoh<-0 #This cannot be 1 if married is 1
income<-37000

#########Chart Creation############

marginaltaxrate<-NULL
averagetaxrate<-NULL
taxbill<-NULL
income<-NULL
b<-1
while (b < 1000){
  
    
    income[b]<-b*1000
    
  taxbill[b]<-TotalTaxBurden(income[b],children,married,hoh)
  marginaltaxrate[b]<-(TotalTaxBurden(income[b],children,married,hoh)-TotalTaxBurden(income[b]-1,children,married,hoh))/1
  averagetaxrate[b]<-TotalTaxBurden(income[b],children,married,hoh)/income[b]

  b<-b+1
  
}

plot(income,taxbill)

mat<-cbind(income,marginaltaxrate,averagetaxrate,taxbill)

write.table(mat,sep=",",file="test.txt")
