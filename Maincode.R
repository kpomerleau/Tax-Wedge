setwd("U:/Documents backup 2-3/GitHub/taxwedge")
rm(list=ls())
###################################Part 1################################
#############################Average Tax Wedge###########################

#This spreadsheet has all the income data and will show all the calculations, step-by-step.
income<-read.csv("income.csv", header = TRUE, fill = TRUE, sep = ",")

#############Federal Tax Parameters###############################

#Load Federal Tax Parameters
fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

#Set Additional Parameters (Children,Married)
children<-1
married<-0
hoh<-0 #This cannot be 1 if married is 1

income<-300000 #test income amount
stateincometax<-income*.06

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

  while(taxableincome>fedtax[x,3+married+(hoh*2)] & !is.na(fedtax[x,3+married+(hoh*2)])){  #This uses the !is.na() code to account for the fact that there is an NA cell in the fedtax parameters
    
    if(x==1){
      
      federalincometax<-federalincometax+fedtax[x,3+married+(hoh*2)]*fedtax$incometaxrates[x]
    
    } else {
      
      federalincometax<-federalincometax+(fedtax[x,3+married+(hoh*2)]-fedtax[x-1,3+married+(hoh*2)])*fedtax$incometaxrates[x]
      
    }
    
    x<-x+1
  
  } 

    if(x==1){
      
      federalincometax<-federalincometax+taxableincome*fedtax$incometaxrates[x]
      
    } else {
      
      federalincometax<-federalincometax+(taxableincome-fedtax[x-1,3+married+(hoh*2)])*fedtax$incometaxrates[x]
      
    }

#Credit Calculations (CTC and EITC)

  #Child Tax Credit

    if(children>0) {
      
      c<-children
      
      i<-1
      
      if (married == 0) {
        
        #while (i <= length(income$stateid)) {
          
          if(income<=fedtax$ctcphasein[1]) {results$ctc_test[i]<-0} else 
            
            if(income<fedtax$ctcphaseout_single[1]) {ctc<-min(fedtax$ctccredit[1]*c,((income[i]-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))} else
              
              if(income>fedtax$ctcphaseout_single[1]) {ctc<-max(0,(fedtax$ctccredit[1]*c)-(income[i]-fedtax$ctcphaseout_single[1])*fedtax$ctcphaseoutrate[1])}
          
          ;i<-i+1
        
        #}
      
      } else if (married == 1) {
        
        #while (i <= length(income$stateid)) {
          
          if(income<=fedtax$ctcphasein[1]) {ctc<-0} else 
          
            if(income<fedtax$ctcphaseout_married[1]) {ctc<-min(fedtax$ctccredit[1]*c,((income-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))} else
            
              if(income>fedtax$ctcphaseout_married[1]) {ctc<-max(0,(fedtax$ctccredit[1]*c)-(income-fedtax$ctcphaseout_single[1])*fedtax$ctcphaseoutrate[1])}
          
          ;i<-i+1
        
        #}
      
      }
    
    } else {ctc<-0}

  #Earned Income Tax credit

    c<-min(children,3)
    
    #i<-1 #Counts through states
    
    #while (i <= length(income$stateid)) {
    
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
      
      #i<-i+1
    
      #}

#Federal Income Tax with Tax Credits

  netfederaltaxes<-federalincometax-ctc-eitc

#############Federal Payroll Taxes########################

  #Employee Social Security and Medicare Payroll
    
    x<-1 #used as an index to count through the payroll brackets
    
    employeepayrolltaxes<-0
    
    while(income>fedtax[1,6+x] & !is.na(fedtax[1,6+x])){
      
      if(x==1){
        
        employeepayrolltaxes<-employeepayrolltaxes+fedtax[1,6+x]*fedtax[2,6+x]
        
      } else {
        
        employeepayrolltaxes<-employeepayrolltaxes+(fedtax[1,6+x]-fedtax[1,5+x])*fedtax[2,6+x]
        
      }
      
      x<-x+1
      
    }
      
      if(x==1){ #If income does not exceed the first bracket ($117,000), then:
        
        employeepayrolltaxes<-employeepayrolltaxes+(income*fedtax[2,6+x])
        
      }

      if(is.na(fedtax[1,6+x])){ #If income exceeds the top bracket amount ($200,000), then:
        
        employeepayrolltaxes<-employeepayrolltaxes+(income-fedtax[1,5+x])*fedtax[2,6+x]
        
      }

  #Employer Social Security and Payroll Taxes
    
    employerpayrolltaxes<-0    

    if(income<=fedtax[1,9+x]){
      
      employerpayrolltaxes<-employerpayrolltaxes+(income*fedtax[2,9+x])
      
    } else {
      
      employerpayrolltaxes<-(fedtax[1,10]*fedtax[2,10])+(income-fedtax[1,10])*fedtax[2,11]
      
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