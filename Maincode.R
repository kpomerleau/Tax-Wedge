
setwd("C:/Users/kep/Documents/GitHub/Tax-Wedge")

rm(list=ls()) 

#Load Parameters that are saved in csv files.

  source("parametersetup.R")
  
  income<-read.csv("income.csv", header = TRUE, fill = TRUE, sep = ",")
  
  statetax<-read.csv("statetaxdata.csv", header = TRUE, fill = TRUE, sep = ",")
  
  fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

#Load Functions

  file.sources = list.files(c("C:/Users/kep/Documents/GitHub/Tax-Wedge/Fedtaxfunctions", 
                              "C:/Users/kep/Documents/GitHub/Tax-Wedge/Statetaxfunctions"), 
                            pattern="*.R$", full.names=TRUE, 
                            ignore.case=TRUE)
  sapply(file.sources,source,.GlobalEnv)
  
####################################Testing Federal Taxes################################################

TotalTaxBurden<-function(income, children, married, hoh){
  
  stateincometax<-income*0
  
  #Step 7: Federal Taxable Income
  
  taxableincome<-FedTaxableIncome(income, children, married, hoh, stateincometax)
  
  #Step 8: Federal Income Tax
  
  federalincometax<-FedIncomeTax(taxableincome,married,hoh)
  
  #Step 9: Child Tax Credit
  
  ctc<-FedCTC(income,children,married)
  
  #Earned Income Tax Credit
  
  eitc<-FedEITC(income,children,married)
  
  #Step 10: Federal Income Tax
  
  federalincometax<-federalincometax-ctc-eitc
  
  #Step 12: Employee Payroll Taxes
  
  employeepayrolltax<-FedEmployeePayroll(income,married)
  
  #Step 13: Employer Payroll Taxes
  
  employerpayrolltax<-FedEmployerPayroll(income/(1-(employeepayrolltax/income)),married)
  
  #Medicare Surtax
  
  medsurtax<-MedSurtax(income,married)
  
  #Step 14: Total Tax Burden
  
  taxburden<-federalincometax+employeepayrolltax+medsurtax
  
  #Tax Wedge
  
  taxwedge<-federalincometax+employeepayrolltax+employerpayrolltax+medsurtax
  
  return(taxwedge)
  
}

##############Tax Parameters###########

state<-4
children<-1
married<-0
hoh<-0 #This cannot be 1 if married is 1
income<-37000
stateparam<-StateParameters(state)

#########Chart Creation############

marginaltaxrate<-NULL
averagetaxrate<-NULL
taxbill<-NULL
income<-NULL
grossup<-NULL
b<-1
while (b < 500){
  
    
    income[b]<-b*1000
    grossup[b]<-FedEmployerPayroll(income[b],married)
    
  taxbill[b]<-TotalTaxBurden(income[b],children,married,hoh)
  marginaltaxrate[b]<-(TotalTaxBurden(income[b],children,married,hoh)-TotalTaxBurden(income[b]-1,children,married,hoh))/1
  averagetaxrate[b]<-TotalTaxBurden(income[b],children,married,hoh)/(income[b])

  b<-b+1
  
}

options(scipen=999) #Get's rid of scientific notation, which is useless in the context of dollars

plot(income,marginaltaxrate, 
     main=paste("Marginal Tax Rate by Income Level,",toString(stateparam$stateName[1])),
     log = "x",
     xlab="Income", ylab="Marginal Tax Rate",
     xaxt = 'n',
     ylim = c(min(marginaltaxrate),max(marginaltaxrate)))
axis(1, at=axTicks(1), labels=sprintf("$%s", axTicks(1)),cex.axis=.75)

mat<-NULL
mat<-cbind(income,marginaltaxrate,averagetaxrate,taxbill)

write.table(mat,sep=",",file="test.txt")
