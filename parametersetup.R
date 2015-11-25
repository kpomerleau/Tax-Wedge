#Setting parameters

setwd("C:/Users/kep/Documents/GitHub/Tax-Wedge")

#Loads parameters from csv file

fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

#Standard Deduction

standard_deduction_single <- fedtax$standard_deduction[1]
standard_deduction_married <- fedtax$standard_deduction[2]
standard_deduction_headofhousehold <- fedtax$standard_deduction[3]

#Personal Exemption

personal_exemption <- fedtax$personal_exemption[1]