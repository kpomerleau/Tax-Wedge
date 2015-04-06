
#setwd("C:/Users/Kyle/Documents/GitHub/Tax-Wedge")

setwd("C:/Users/kep/Documents/GitHub/Tax-Wedge")

rm(list=ls()) 

#This spreadsheet has all the income data and will show all the calculations, step-by-step.
  
  income<-read.csv("income.csv", header = TRUE, fill = TRUE, sep = ",")
  
  statetax<-read.csv("statetaxdata.csv", header = TRUE, fill = TRUE, sep = ",")
  
  fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

#Load Functions

  source("Functions.R")
  
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
  
  return(federalincometax)

}

##############Tax Parameters###########

state<-1
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

plot(income,marginaltaxrate)
mat<-NULL
mat<-cbind(income,marginaltaxrate,averagetaxrate,taxbill)

write.table(mat,sep=",",file="test.txt")
