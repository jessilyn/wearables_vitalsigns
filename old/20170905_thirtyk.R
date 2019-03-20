
# Ryan's script from the qsart/stride database to subset the 30LDL_Calc STRIDE table
# PURPOSE: Reduce size of labs table to only those labs of interest:

#### Clean and Restructure New Labs/Vitals Data to Match Previous.

#### OUTPUT: One csv to desktop.

#### FILE DEPENDENCIES: 
# SECURE_lab_results_new_18_JUL_2016.csv
# SECURE_vitals_new_18_JUL_2016.csv
# Basis2016_Norm0804_WeekPrior.csv

#### LIBRARY DEPENDENCIES:
require(data.table)
require(psych)

#### READ:
vitals <- fread("ForLukasz/all_vitals.csv",
                   header=TRUE,sep=',',stringsAsFactors=FALSE)
vitals <- data.frame(vitals)

labs <- fread("ForLukasz/all_labs.csv",
                 header=TRUE,sep=',',stringsAsFactors=FALSE)

wear <- read.csv("Basis2016_Norm0824_WeekPrior.csv",
  header=TRUE,sep=',',stringsAsFactors=FALSE)

rm(list=setdiff(ls(), c("labs", "vitals","wear")))

#### NOTE WANTED LAB TESTS:
allClin <- c("A1C","AG","ALB","ALCRU","ALKP","ALT","AST","BASO",
             "BASOAB","BUN","CA","CHOL","CHOLHDL","CL","CO2",
             "CR","EOS","EOSAB","GLOB","GLU_byMeter",
             "GLU_fasting","GLU_nonFasting","GLU_SerPlas",
             "GLU_wholeBld","HCT","HDL",
             "HGB","HSCRP","IGM","K","LDL","LDLHDL","LYM","LYMAB",
             "MCH","MCHC","MCV","MONO","MONOAB","NA.","NEUT",
             "NEUTAB","NHDL","PLT","RBC","RDW","TBIL","TGL","TP",
             "UALB","UALBCR","WBC")
#Leave out UALBCRINTP, as it is more categorical than continuous.

#### PREP LABS DATA:

unqLabs <- unique(labs$LAB_NAME)

## A1C - DONE (JD)
#find A1C
unqLabs[grep("Hemoglobin|A1C",unqLabs)] #all tests with HGB or A1C in the name
unqLabs[grep("A1c|A1C",unqLabs)][c(1,3)] # names of tests that were used here
lowBound <-min(range(na.omit(as.numeric(wear$A1C)))) - (3*sd(na.omit(as.numeric(wear$A1C))))
upBound <- max(range(na.omit(as.numeric(wear$A1C)))) + (3*sd(na.omit(as.numeric(wear$A1C))))
A1C <- which(labs$LAB_NAME %in% unqLabs[
  grep("A1c|A1C",unqLabs)][c(1,3)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound)) 
test <- labs[c(A1C),]
range(as.numeric(wear$A1C),na.rm=TRUE) # range of A1C values from iPOP cohort
range(as.numeric(test$ORD_VALUE),na.rm=TRUE) # range of A1C values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of A1C values from 30K cohort
summary(as.numeric(wear$A1C)) # summary of A1C values from wearables cohort
length(test$ORD_VALUE)# number of A1C tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the A1C test in 30k cohort

## AG - STILL NEEDS WORK - I think definition in wearables table looks wrong
#find AG
unqLabs[grep("A/G|AG|Albumin|Globulin|Albumin/Globulin Ratio",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("A/G|AG|Albumin|Globulin|Albumin/Globulin Ratio",unqLabs)]))])
unqLabs[grep("A/G|AG|Albumin|Globulin|Albumin/Globulin Ratio",
             unqLabs)][c(5,35)]
#lowBound <-0
#upBound <- max(range(na.omit(as.numeric(wear$AG)))) + (3*sd(na.omit(as.numeric(wear$AG))))
AG <- which(labs$LAB_NAME %in% unqLabs[
  grep("A/G|AG|Albumin|Globulin|Albumin/Globulin Ratio",
       unqLabs)][c(5,35)])
#  & (labs$ORD_VALUE < upBound) & (labs$ORD_VALUE > lowBound))
test <- labs[c(AG),]
range(as.numeric(wear$AG),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$AG)) # summary of AG values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of AG values from 30K cohort
length(test$ORD_VALUE)# number of AG tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the A1C test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)
table(wear$AG)

## ALB - DONE (JD)
#find ALB 
unqLabs[grep("Albumin|ALB|Albumin, Ser/Plas",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("Albumin|ALB|Albumin, Ser/Plas",unqLabs)]))])
unqLabs[grep("Albumin|ALB|Albumin, Ser/Plas",unqLabs)][c(1)] #5,6?
lowBound <-min(range(na.omit(as.numeric(wear$ALB)))) - (3*sd(na.omit(as.numeric(wear$ALB))))
upBound <- max(range(na.omit(as.numeric(wear$ALB)))) + (3*sd(na.omit(as.numeric(wear$ALB))))
ALB <- which(labs$LAB_NAME %in% unqLabs[
  grep("Albumin|ALB|Albumin, Ser/Plas",unqLabs)][c(1)]  & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(ALB),]
summary(as.numeric(wear$ALB)) # summary of AG values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of AG values from 30K cohort
range(as.numeric(wear$ALB),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
length(test$ORD_VALUE)# number of AG tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the A1C test in 30k cohort
table(test$REFERENCE_UNIT)
#convert values in mg/dL to g/dL equivalent (divide by 1000)

##ALCRU - still needs work; I dont know why, but summary of iPOP v 30k data is very different
#find ALCRU (has only 1 obs.) 
unqLabs[grep("Albumin|ALB|Creatine|creatine|ALB/Creat. Ratio",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("Albumin|ALB|Creatine|creatine|ALB/Creat. Ratio",unqLabs)]))])
unqLabs[grep("Albumin|ALB|Creatine|creatine|ALB/Creat. Ratio",
             unqLabs)][c(7,11)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$ALCRU)))) + (3*sd(na.omit(as.numeric(wear$ALCRU))))
ALCRU <- which(labs$LAB_NAME %in% unqLabs[
  grep("Albumin|ALB|Creatine|creatine|ALB/Creat. Ratio",
       unqLabs)][c(7,11)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(ALCRU),]
range(as.numeric(wear$ALCRU),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$ALCRU)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of AG tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the A1C test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

## ALKP - DONE (JD)
#find ALKP (ALK P'TASE Total, Serum (Manual Entry))
unqLabs[grep("ALK|Alkaline|alkaline|ALKP|Alkaline Phosphatase|phosphatase",
             unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("ALK|Alkaline|alkaline|ALKP|Alkaline Phosphatase|phosphatase",
       unqLabs)]))])
unqLabs[grep("ALK|Alkaline|alkaline|ALKP|Alkaline Phosphatase|phosphatase",
             unqLabs)][c(1,4,5)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$ALKP)))) + (3*sd(na.omit(as.numeric(wear$ALKP))))
ALKP <- which(labs$LAB_NAME %in% unqLabs[
  grep("ALK|Alkaline|alkaline|ALKP|Alkaline Phosphatase|phosphatase",
       unqLabs)][c(1,4,5)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(ALKP),]
range(as.numeric(wear$ALKP),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$ALKP)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of AG tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the A1C test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find ALT - DONE (JD)
unqLabs[grep("ALT|Alanine aminotransferase",unqLabs)]
unqLabs[grep("ALT|Alanine aminotransferase",unqLabs)][1:3]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$ALT)))) + (3*sd(na.omit(as.numeric(wear$ALT))))
ALT <- which(labs$LAB_NAME %in% unqLabs[
  grep("ALT|Alanine aminotransferase",unqLabs)][1:3] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(ALT),]
range(as.numeric(wear$ALT),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$ALT)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find AST - DONE (JD)
unqLabs[grep("AST|Aspartate|aspartate aminotransferase",unqLabs)]
unqLabs[grep("AST|Aspartate|aspartate aminotransferase",
             unqLabs)][c(1,2)]
lowBound <-min(range(na.omit(as.numeric(wear$AST)))) - (3*sd(na.omit(as.numeric(wear$AST))))
upBound <- max(range(na.omit(as.numeric(wear$AST)))) + (3*sd(na.omit(as.numeric(wear$AST))))
AST <- which(labs$LAB_NAME %in% unqLabs[
  grep("AST|Aspartate|aspartate aminotransferase",unqLabs)][c(1,2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(AST),]
summary(as.numeric(wear$AST)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
range(as.numeric(wear$AST),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find BASO - DONE (JD)
unqLabs[grep("BASO|Basophil",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("BASO|Basophil",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$BASO)))) - (3*sd(na.omit(as.numeric(wear$BASO))))
upBound <- max(range(na.omit(as.numeric(wear$BASO)))) + (3*sd(na.omit(as.numeric(wear$BASO))))
unqLabs[grep("BASO|Basophil",unqLabs)][c(1,3,5,9)]
BASO <- which(labs$LAB_NAME %in% unqLabs[
  grep("BASO|Basophil",unqLabs)][c(1,3,5,9)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(BASO),]
summary(as.numeric(wear$BASO)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
range(as.numeric(wear$BASO),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find BASOAB - DONE (JD)
unqLabs[grep("BASO|Basophil",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("BASO|Basophil",
       unqLabs)]))])
unqLabs[grep("BASO|Basophil",unqLabs)][c(2,4,7)]
lowBound <-min(range(na.omit(as.numeric(wear$BASOAB)))) - (3*sd(na.omit(as.numeric(wear$BASOAB))))
upBound <- max(range(na.omit(as.numeric(wear$BASOAB)))) + (3*sd(na.omit(as.numeric(wear$BASOAB))))
BASOAB <- which(labs$LAB_NAME %in% unqLabs[
  grep("BASO|Basophil",unqLabs)][c(2,4,7)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(BASOAB),]
summary(as.numeric(wear$BASOAB)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
range(as.numeric(wear$BASOAB),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find BUN - DONE (JD)
unqLabs[grep("BUN|Blood urea nitrogen",unqLabs)]
unqLabs[grep("BUN|Blood urea nitrogen",unqLabs)][c(2:4)]
lowBound <-min(range(na.omit(as.numeric(wear$BUN)))) - (3*sd(na.omit(as.numeric(wear$BUN))))
upBound <- max(range(na.omit(as.numeric(wear$BUN)))) + (3*sd(na.omit(as.numeric(wear$BUN))))
BUN <- which(labs$LAB_NAME %in% unqLabs[
  grep("BUN|Blood urea nitrogen",unqLabs)][c(2:4)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(BUN),]
summary(as.numeric(wear$BUN)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
range(as.numeric(wear$BUN),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find CA - DONE (JD)
unqLabs[grep("Calcium",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("Calcium",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$CA)))) - (3*sd(na.omit(as.numeric(wear$CA))))
upBound <- max(range(na.omit(as.numeric(wear$CA)))) + (3*sd(na.omit(as.numeric(wear$CA))))
unqLabs[grep("Calcium",unqLabs)][c(1,2,5)]
CA <- which(labs$LAB_NAME %in% unqLabs[
  grep("Calcium",unqLabs)][c(1,2,5)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(CA),]
range(as.numeric(wear$CA),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$CA)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find CHOL - DONE (JD)
unqLabs[grep("CHOL|Cholesterol",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("CHOL|Cholesterol",
       unqLabs)]))])
lowBound <-20
upBound <- max(range(na.omit(as.numeric(wear$CHOL)))) + (3*sd(na.omit(as.numeric(wear$CHOL))))
unqLabs[grep("CHOL|Cholesterol",unqLabs)][c(1,4,5,18)]
CHOL <- which(labs$LAB_NAME %in% unqLabs[
  grep("CHOL|Cholesterol",unqLabs)][c(1,4,5,18)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(CHOL),]
range(as.numeric(wear$CHOL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$CHOL)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find CHOLHDL - DONE (JD)
unqLabs[grep("CHOL|Cholesterol",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("CHOL|Cholesterol",
       unqLabs)]))])
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$CHOLHDL)))) + (3*sd(na.omit(as.numeric(wear$CHOLHDL))))
unqLabs[grep("CHOL|Cholesterol",unqLabs)][c(3)]
CHOLHDL <- which(labs$LAB_NAME %in% unqLabs[
  grep("CHOL|Cholesterol",unqLabs)][c(3)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(CHOLHDL),]
range(as.numeric(wear$CHOLHDL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$CHOLHDL)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find CL - DONE (JD)
unqLabs[grep("CL|Chloride",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("CL|Chloride",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$CL)))) - (3*sd(na.omit(as.numeric(wear$CL))))
upBound <- max(range(na.omit(as.numeric(wear$CL)))) + (3*sd(na.omit(as.numeric(wear$CL))))
unqLabs[grep("CL|Chloride",unqLabs)][c(1)]
CL <- which(labs$LAB_NAME %in% unqLabs[
  grep("CL|Chloride",unqLabs)][c(1)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(CL),]
summary(as.numeric(wear$CL)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
range(as.numeric(wear$CL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)

#find CO2 - DONE (JD)
unqLabs[grep("CO2|Carbon",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("CO2|Carbon",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$CO2)))) - (3*sd(na.omit(as.numeric(wear$CO2))))
upBound <- max(range(na.omit(as.numeric(wear$CO2)))) + (3*sd(na.omit(as.numeric(wear$CO2))))
unqLabs[grep("CO2|Carbon",unqLabs)][c(1,2)]
CO2 <- which(labs$LAB_NAME %in% unqLabs[
  grep("CO2|Carbon",unqLabs)][c(1,2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(CO2),]
range(as.numeric(wear$CO2),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$CO2)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find CR (creatine or chromium?) - need to double check
unqLabs[grep("CR|Creatine",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("CR|Creatine",
       unqLabs)]))])
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$CR)))) + (3*sd(na.omit(as.numeric(wear$CR))))
unqLabs[grep("CR|Creatine",unqLabs)][c(15,58)]
CR <- which(labs$LAB_NAME %in% unqLabs[
  grep("CR|Creatine",unqLabs)][c(15,58)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(CR),]
range(as.numeric(wear$CR),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$CR)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find EGFR (estimated glomerular filtration rate)
# unqLabs[grep("EGFR|glomerular|filtration",unqLabs)]
# table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
#   grep("EGFR|glomerular|filtration",
#        unqLabs)]))])
# lowBound <-0
# upBound <- max(range(na.omit(as.numeric(wear$EGFR)))) + (3*sd(na.omit(as.numeric(wear$EGFR))))
# unqLabs[grep("EGFR|estimated glomerular filtration rate",unqLabs)][c(2)]
# EGFR <- which(labs$LAB_NAME %in% unqLabs[
#   grep("EGFR|estimated glomerular filtration rate",unqLabs)][c(2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
# test <- labs[c(EGFR),]
# summary(as.numeric(wear$EGFR)) # summary of values from 30K cohort
# summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
# length(test$ORD_VALUE)# number of tests in 30k cohort
# length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
# range(as.numeric(wear$EGFR),na.rm=TRUE)
# range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
#This test will need to be removed from consideration


#find EOS - DONE (JD) (higher proportion of sick visits in iPOP cohort)
unqLabs[grep("EOS|Eosinophil",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("EOS|Eosinophil",
       unqLabs)]))])
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$EOS)))) + (3*sd(na.omit(as.numeric(wear$EOS))))
unqLabs[grep("EOS|Eosinophil",unqLabs)][c(1,3,6)]
EOS <- which(labs$LAB_NAME %in% unqLabs[
  grep("EOS|Eosinophil",unqLabs)][c(1,3,6)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(EOS),]
range(as.numeric(wear$EOS),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$EOS)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find EOSAB - DONE (JD)
unqLabs[grep("EOS|Eosinophil",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("EOS|Eosinophil",
       unqLabs)]))])
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$EOSAB)))) + (3*sd(na.omit(as.numeric(wear$EOSAB))))
unqLabs[grep("EOS|Eosinophil",unqLabs)][c(2,4,8)]
EOSAB <- which(labs$LAB_NAME %in% unqLabs[
  grep("EOS|Eosinophil",unqLabs)][c(2,4,8)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(EOSAB),]
range(as.numeric(wear$EOSAB),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$EOSAB)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort


#find GLOB - DONE (JD)
unqLabs[grep("GLOB|Globulin|globulin",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("GLOB|Globulin|globulin",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$GLOB)))) - (3*sd(na.omit(as.numeric(wear$GLOB))))
upBound <- max(range(na.omit(as.numeric(wear$GLOB)))) + (3*sd(na.omit(as.numeric(wear$GLOB))))
unqLabs[grep("GLOB|Globulin|globulin",unqLabs)][c(1)]
GLOB <- which(labs$LAB_NAME %in% unqLabs[
  grep("GLOB|Globulin|globulin",unqLabs)][c(1)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(GLOB),]
range(as.numeric(wear$GLOB),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$GLOB)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find GLU (GLucose Fasting) - DONE (JD)
unqLabs[grep("GLU|Glucose|glucose",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$GLU)))) - (3*sd(na.omit(as.numeric(wear$GLU))))
upBound <- max(range(na.omit(as.numeric(wear$GLU)))) + (3*sd(na.omit(as.numeric(wear$GLU))))
unqLabs[grep("GLU|Glucose|glucose",unqLabs)][c(4)] 
GLU_fasting <- which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",unqLabs)][c(4)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(GLU_fasting),]
range(as.numeric(wear$GLU),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$GLU)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find GLU (GLucose by Meter) - DONE (JD)
unqLabs[grep("GLU|Glucose|glucose",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$GLU)))) - (3*sd(na.omit(as.numeric(wear$GLU))))
upBound <- max(range(na.omit(as.numeric(wear$GLU)))) + (3*sd(na.omit(as.numeric(wear$GLU))))
unqLabs[grep("GLU|Glucose|glucose",unqLabs)][c(5)]
GLU_byMeter <- which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",unqLabs)][c(5)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(GLU_byMeter),]
range(as.numeric(wear$GLU),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$GLU)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find GLU (GLucose Non-fasting) - DONE (JD)
unqLabs[grep("GLU|Glucose|glucose",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$GLU)))) - (3*sd(na.omit(as.numeric(wear$GLU))))
upBound <- max(range(na.omit(as.numeric(wear$GLU)))) + (3*sd(na.omit(as.numeric(wear$GLU))))
unqLabs[grep("GLU|Glucose|glucose",unqLabs)][c(3)]
GLU_nonFasting <- which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",unqLabs)][c(3)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(GLU_nonFasting),]
range(as.numeric(wear$GLU),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$GLU)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find GLU (Glucose, Ser/Plas; Glucose, SER/PLAS (Manual Entry)) - DONE (JD)
unqLabs[grep("GLU|Glucose|glucose",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$GLU)))) - (3*sd(na.omit(as.numeric(wear$GLU))))
upBound <- max(range(na.omit(as.numeric(wear$GLU)))) + (3*sd(na.omit(as.numeric(wear$GLU))))
unqLabs[grep("GLU|Glucose|glucose",unqLabs)][c(1,7)]
GLU_SerPlas <- which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",unqLabs)][c(1,7)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(GLU_SerPlas),]
range(as.numeric(wear$GLU),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$GLU)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find GLU (GLucose Whole Blood) - DONE (JD)
unqLabs[grep("GLU|Glucose|glucose",unqLabs)] #Glucose, Glucose by Meter, Glucose Ser/Plas
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$GLU)))) - (3*sd(na.omit(as.numeric(wear$GLU))))
upBound <- max(range(na.omit(as.numeric(wear$GLU)))) + (3*sd(na.omit(as.numeric(wear$GLU))))
unqLabs[grep("GLU|Glucose|glucose",unqLabs)][c(8)] #Glucose is not numeric
GLU_wholeBld <- which(labs$LAB_NAME %in% unqLabs[
  grep("GLU|Glucose|glucose",unqLabs)][c(8)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(GLU),]
range(as.numeric(wear$GLU),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$GLU)) # summary of values from 30K cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find HCT (Hematocrit) - DONE (JD)
unqLabs[grep("HCT|Hematocrit|hematocrit",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("HCT|Hematocrit|hematocrit",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$HCT)))) - (3*sd(na.omit(as.numeric(wear$HCT))))
upBound <- max(range(na.omit(as.numeric(wear$HCT)))) + (3*sd(na.omit(as.numeric(wear$HCT))))
unqLabs[grep("HCT|Hematocrit|hematocrit",unqLabs)][c(1:2)]
HCT <- which(labs$LAB_NAME %in% unqLabs[
  grep("HCT|Hematocrit|hematocrit",unqLabs)][c(1:2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(HCT),]
range(as.numeric(wear$HCT),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$HCT)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find HDL - DONE (JD)
unqLabs[grep("CHOL|Cholesterol|High|HDL|High-",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("CHOL|Cholesterol|High|HDL|High-",
       unqLabs)]))])
lowBound <-10
upBound <- max(range(na.omit(as.numeric(wear$HDL)))) + (3*sd(na.omit(as.numeric(wear$HDL))))
unqLabs[grep("CHOL|Cholesterol|High|HDL|High-",unqLabs)][c(2,9)]
HDL <- which(labs$LAB_NAME %in% unqLabs[
  grep("CHOL|Cholesterol",unqLabs)][c(2,9)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(HDL),]
range(as.numeric(wear$HDL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$HDL)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find HGB (hemoglobin) - DONE (JD)
unqLabs[grep("HGB|Hemoglobin|hemoglobin",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("HGB|Hemoglobin|hemoglobin",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$HGB)))) - (3*sd(na.omit(as.numeric(wear$HGB))))
upBound <- max(range(na.omit(as.numeric(wear$HGB)))) + (3*sd(na.omit(as.numeric(wear$HGB))))
unqLabs[grep("HGB|Hemoglobin|hemoglobin",unqLabs)][c(1,4)]
HGB <- which(labs$LAB_NAME %in% unqLabs[
  grep("HGB|Hemoglobin|hemoglobin",unqLabs)][c(1,4)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(HGB),]
range(as.numeric(wear$HGB),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$HGB)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find HSCRP (high-sensitivity c-reactive protein) - DONE (JD)
unqLabs[grep("HSCRP|CRP|crp|C-reactive|c-reactive",unqLabs)]
unqLabs[grep("HSCRP|CRP|crp|C-reactive|c-reactive",unqLabs)][c(1,2)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$HSCRP)))) + (3*sd(na.omit(as.numeric(wear$HSCRP))))
HSCRP <- which(labs$LAB_NAME %in% unqLabs[
  grep("HSCRP|CRP|crp|C-reactive|c-reactive",unqLabs)][c(1,2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(HSCRP),]
range(as.numeric(wear$HSCRP),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$HSCRP)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find IGM (Immunoglobin M) #no exact matches found
#unqLabs[grep("IGM|Immunoglobin M|immunoglobin m",unqLabs)]
#table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
#  grep("IGM|Immunoglobin M|immunoglobin m",
#       unqLabs)]))])
#unqLabs[grep("IGM|Immunoglobin M|immunoglobin m",unqLabs)]
#IGM <- which(labs$LAB_NAME %in% unqLabs[
#  grep("IGM|Immunoglobin M|immunoglobin m",unqLabs)])
#test <- labs[c(IGM),]
#range(as.numeric(wear$IGM),na.rm=TRUE)
#range(as.numeric(test$ORD_VALUE),na.rm=TRUE)

#find K (Potassium) #Potassium, Ser/Plas - DONE (JD)
unqLabs[grep("K|Potassium|potassium",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("K|Potassium|potassium",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$K)))) - (3*sd(na.omit(as.numeric(wear$K))))
upBound <- max(range(na.omit(as.numeric(wear$K)))) + (3*sd(na.omit(as.numeric(wear$K))))
unqLabs[grep("K|Potassium|potassium",unqLabs)][c(1,3)]
K <- which(labs$LAB_NAME %in% unqLabs[
  grep("K|Potassium|potassium",unqLabs)][c(1,3)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(K),]
range(as.numeric(wear$K),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$K)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find LDL calc #2 different tests: Direct LDL Chol, LDL (Calculated) - DONE (JD)
unqLabs[grep("LDL|lipotein",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("LDL|lipotein",
       unqLabs)]))])
lowBound <-10
upBound <- max(range(na.omit(as.numeric(wear$LDL)))) + (3*sd(na.omit(as.numeric(wear$LDL))))
unqLabs[grep("LDL|lipotein",unqLabs)][c(2)]
LDL_Calc <- which(labs$LAB_NAME %in% unqLabs[
  grep("LDL|lipotein",unqLabs)][c(2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(LDL_Calc),]
range(as.numeric(wear$LDL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$LDL)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find LDL direct #2 different tests: Direct LDL Chol, LDL (Calculated) - DONE (JD)
unqLabs[grep("LDL|lipotein",unqLabs)]
unqLabs[grep("LDL|lipotein",unqLabs)][c(3,40)]
lowBound <-10
upBound <- max(range(na.omit(as.numeric(wear$LDL)))) + (3*sd(na.omit(as.numeric(wear$LDL))))
LDL_Direct <- which(labs$LAB_NAME %in% unqLabs[
  grep("LDL|lipotein",unqLabs)][c(3,40)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(LDL_Direct),]
range(as.numeric(wear$LDL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$LDL)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find LDLHDL #LDL/HDL Ratio  - DONE (JD)
unqLabs[grep("LDL|lipotein",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("LDL|lipotein",
       unqLabs)]))])
unqLabs[grep("LDL|lipotein",unqLabs)][c(4)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$LDLHDL)))) + (3*sd(na.omit(as.numeric(wear$LDLHDL))))
LDLHDL <- which(labs$LAB_NAME %in% unqLabs[
  grep("LDL|lipotein",unqLabs)][c(4)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(LDLHDL),]
range(as.numeric(wear$LDLHDL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$LDLHDL)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find LYM (LYMPHS, %; Lymphocytes %; Lymphocytes; Lymphocyte %; LYM, %)   - DONE (JD)
unqLabs[grep("LYM|Lympho+",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("LYM|Lympho+",
       unqLabs)]))])
unqLabs[grep("LYM|Lympho+",unqLabs)][c(1, 3, 6, 9, 16)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$LYM)))) + (3*sd(na.omit(as.numeric(wear$LYM))))
LYM <- which(labs$LAB_NAME %in% unqLabs[
  grep("LYM|Lympho+",unqLabs)][c(1, 3, 6, 9, 16)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(LYM),]
range(as.numeric(wear$LYM),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$LYM)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find LYMAB (LYM, ABS; LYMPH, ABS; Lymphocyte, Absolute;    - DONE (JD)
# Lymphocytes, ABS (man diff); Lymphocytes, Abs.; LYMPHS, ABS;)
unqLabs[grep("LYM|Lympho+",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("LYM|Lympho+",
       unqLabs)]))])
unqLabs[grep("LYM|Lympho+",unqLabs)][c(2,4,10,15,17,30)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$LYMAB)))) + (3*sd(na.omit(as.numeric(wear$LYMAB))))
LYMAB <- which(labs$LAB_NAME %in% unqLabs[
  grep("LYM|Lympho+",unqLabs)][c(2,4,10,15,17,30)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(LYMAB),]
range(as.numeric(wear$LYMAB),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$LYMAB)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)
#Convert /uL to K/uL (divide by 1000)

#find MCH (mean corpuscular hemoglobin)  - DONE (JD)
unqLabs[grep("MCH|Mean|corpuscular|Hemoglobin",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("MCH|Mean|corpuscular|Hemoglobin",
       unqLabs)]))])
unqLabs[grep("MCH|Mean|corpuscular|Hemoglobin",unqLabs)][c(2)]
lowBound <-min(range(na.omit(as.numeric(wear$MCH)))) - (3*sd(na.omit(as.numeric(wear$MCH))))
upBound <- max(range(na.omit(as.numeric(wear$MCH)))) + (3*sd(na.omit(as.numeric(wear$MCH))))
MCH <- which(labs$LAB_NAME %in% unqLabs[
  grep("MCH|Mean|corpuscular|Hemoglobin",unqLabs)][c(2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(MCH),]
range(as.numeric(wear$MCH),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$MCH)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find MCHC (mean corpuscular hemoglobin concentration)   - DONE (JD)
unqLabs[grep("MCH|Mean|corpuscular|Hemoglobin",unqLabs)]
unqLabs[grep("MCH|Mean|corpuscular|Hemoglobin",unqLabs)][c(3)]
lowBound <-min(range(na.omit(as.numeric(wear$MCHC)))) - (3*sd(na.omit(as.numeric(wear$MCHC))))
upBound <- max(range(na.omit(as.numeric(wear$MCHC)))) + (3*sd(na.omit(as.numeric(wear$MCHC))))
MCHC <- which(labs$LAB_NAME %in% unqLabs[
  grep("MCH|Mean|corpuscular|Hemoglobin",unqLabs)][c(3)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(MCHC),]
range(as.numeric(wear$MCHC),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$MCHC)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find MCV (mean corpuscular volume)  - DONE (JD)
unqLabs[grep("MCV|Mean|corpuscular",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("MCV|Mean|corpuscular",
       unqLabs)]))])
unqLabs[grep("MCV|Mean|corpuscular",unqLabs)][c(1,2)]
lowBound <-min(range(na.omit(as.numeric(wear$MCV)))) - (3*sd(na.omit(as.numeric(wear$MCV))))
upBound <- max(range(na.omit(as.numeric(wear$MCV)))) + (3*sd(na.omit(as.numeric(wear$MCV))))
MCV <- which(labs$LAB_NAME %in% unqLabs[
  grep("MCV|Mean|corpuscular",unqLabs)][c(1,2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(MCV),]
range(as.numeric(wear$MCV),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$MCV)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find MONO (monocytes) - DONE (JD)
unqLabs[grep("MONO|Mono+|mononucleosis",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("MONO|Mono+|mononucleosis",
       unqLabs)]))])
unqLabs[grep("MONO|Mono+|mononucleosis",unqLabs)][c(1,3,5)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$MONO)))) + (3*sd(na.omit(as.numeric(wear$MONO))))
MONO <- which(labs$LAB_NAME %in% unqLabs[
  grep("MONO|Mono+|mononucleosis",unqLabs)][c(1,3,5)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(MONO),]
range(as.numeric(wear$MONO),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$MONO)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
 
#find MONOAB (monocytes absolute; MONO, ABS; Monocyte, Absolute;  - DONE (JD)
# Monocytes, ABS (man diff))
unqLabs[grep("MONO|Mono+|mononucleosis",unqLabs)]
unqLabs[grep("MONO|Mono+|mononucleosis",unqLabs)][c(2,4,7,15)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$MONOAB)))) + (3*sd(na.omit(as.numeric(wear$MONOAB))))
MONOAB <- which(labs$LAB_NAME %in% unqLabs[
  grep("MONO|Mono+|mononucleosis",unqLabs)][c(2,4,7,15)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(MONOAB),]
range(as.numeric(wear$MONOAB),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$MONOAB)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find NA (sodium) #Sodium, Ser/Plas - DONE (JD)
unqLabs[grep("NA|Sodium|sodium",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("NA|Sodium|sodium",
       unqLabs)]))])
unqLabs[grep("NA|Sodium|sodium",unqLabs)][c(1,6)]
lowBound <-min(range(na.omit(as.numeric(wear$NA.)))) - (3*sd(na.omit(as.numeric(wear$NA.))))
upBound <- max(range(na.omit(as.numeric(wear$NA.)))) + (3*sd(na.omit(as.numeric(wear$NA.))))
NA. <- which(labs$LAB_NAME %in% unqLabs[
  grep("NA|Sodium|sodium",unqLabs)][c(1,6)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(NA.),]
range(as.numeric(wear$NA.),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$NA.)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find NEUT (Neutrophil) #Neutrophil %; NEUT, % - DONE (JD)
unqLabs[grep("NEUT|Neutrophil+",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("NEUT|Neutrophil+",
       unqLabs)]))])
lowBound <-min(range(na.omit(as.numeric(wear$NEUT)))) - (3*sd(na.omit(as.numeric(wear$NEUT))))
upBound <- max(range(na.omit(as.numeric(wear$NEUT)))) + (3*sd(na.omit(as.numeric(wear$NEUT))))
unqLabs[grep("NEUT|Neutrophil+",unqLabs)][c(1,3)]
NEUT <- which(labs$LAB_NAME %in% unqLabs[
  grep("NEUT|Neutrophil+",unqLabs)][c(1,3)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(NEUT),]
range(as.numeric(wear$NEUT),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$NEUT)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find NEUTAB (Neutrophil absolute) #Neutrophil, Absolute...NEUT, ABS - DONE (JD)
unqLabs[grep("NEUT|Neutrophil",unqLabs)]
unqLabs[grep("NEUT|Neutrophil",unqLabs)][c(2,4)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$NEUTAB)))) + (3*sd(na.omit(as.numeric(wear$NEUTAB))))
NEUTAB <- which(labs$LAB_NAME %in% unqLabs[
  grep("NEUT|Neutrophil",unqLabs)][c(2,4)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(NEUTAB),]
range(as.numeric(wear$NEUTAB),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$NEUTAB)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find NHDL (Non-High Density Chol.) #Non-HDL Chol, Calc; - DONE (JD)
# Non-Hdl Chol, Calc.; Non-HDL Cholesterol; NON-HDL Cholesterol; 
# NON HDL CHOLESTEROL  
unqLabs[grep("NHDL|NON-HDL|Non-HDL|Non|CHOL",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("NHDL|NON-HDL|Non-HDL|Non|CHOL",
       unqLabs)]))])
unqLabs[grep("NHDL|NON-HDL|Non-HDL|Non|CHOL",unqLabs)][c(2)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$NHDL)))) + (3*sd(na.omit(as.numeric(wear$NHDL))))
NHDL <- which(labs$LAB_NAME %in% unqLabs[
  grep("NHDL|NON-HDL|Non-HDL|Non|CHOL",unqLabs)][c(2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(NHDL),]
range(as.numeric(wear$NHDL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$NHDL)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find PLT (Platelet) #Platelet count - DONE (JD)
unqLabs[grep("PLT|Platelet|platelet",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("PLT|Platelet|platelet",
       unqLabs)]))])
unqLabs[grep("PLT|Platelet|platelet",unqLabs)][c(1,2)]
lowBound <-50
upBound <- max(range(na.omit(as.numeric(wear$PLT)))) + (3*sd(na.omit(as.numeric(wear$PLT))))
PLT <- which(labs$LAB_NAME %in% unqLabs[
  grep("PLT|Platelet|platelet",unqLabs)][c(1,2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(PLT),]
range(as.numeric(wear$PLT),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$PLT)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find RBC (Red Blood cell count) #RBC - DONE (JD)
unqLabs[grep("RBC|Red|blood|count",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("RBC|Red|blood|count",
       unqLabs)]))])
unqLabs[grep("RBC|Red|blood|count",unqLabs)][c(2,4)]
lowBound <-min(range(na.omit(as.numeric(wear$RBC)))) - (3*sd(na.omit(as.numeric(wear$RBC))))
upBound <- max(range(na.omit(as.numeric(wear$RBC)))) + (3*sd(na.omit(as.numeric(wear$RBC))))
RBC <- which(labs$LAB_NAME %in% unqLabs[
  grep("RBC|Red|blood|count",unqLabs)][c(2,4)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(RBC),]
range(as.numeric(wear$RBC),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$RBC)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find RDW (Red Blood cell distribution width) #RDW  - DONE (JD)
unqLabs[grep("RDW|Red|blood|width",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("RDW|Red|blood|width",
       unqLabs)]))])
unqLabs[grep("RDW|Red|blood|width",unqLabs)][c(1,2)]
lowBound <-min(range(na.omit(as.numeric(wear$RDW)))) - (3*sd(na.omit(as.numeric(wear$RDW))))
upBound <- max(range(na.omit(as.numeric(wear$RDW)))) + (3*sd(na.omit(as.numeric(wear$RDW))))
RDW <- which(labs$LAB_NAME %in% unqLabs[
  grep("RDW|Red|blood|width",unqLabs)][c(1,2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(RDW),]
range(as.numeric(wear$RDW),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$RDW)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find TBIL (Total bilirubin) #Total Bilirubin, Total Bilirubin, Ser/Plas - DONE (JD)
unqLabs[grep("TBIL|Tbil|Total Bilirubin|Bilirubin",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("TBIL|Tbil|Total Bilirubin|Bilirubin",
       unqLabs)]))])
unqLabs[grep("TBIL|Tbil|Total Bilirubin|Bilirubin",unqLabs)][c(1,3)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$TBIL)))) + (3*sd(na.omit(as.numeric(wear$TBIL))))
TBIL <- which(labs$LAB_NAME %in% unqLabs[
  grep("TBIL|Tbil|Total Bilirubin|Bilirubin",unqLabs)][c(1,3)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(TBIL),]
range(as.numeric(wear$TBIL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$TBIL)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find TGL (triglycerides) #Triglyceride, Ser/Plas  - DONE (JD)
unqLabs[grep("TGL|Triglycerides|triglycerides|Trig",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("TGL|Triglycerides|triglycerides|Trig",
       unqLabs)]))])
unqLabs[grep("TGL|Triglycerides|triglycerides|Trig",unqLabs)][c(1,2)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$TGL)))) + (3*sd(na.omit(as.numeric(wear$TGL))))
TGL <- which(labs$LAB_NAME %in% unqLabs[
  grep("TGL|Triglycerides|triglycerides|Trig",unqLabs)][c(1,2)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(TGL),]
range(as.numeric(wear$TGL),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$TGL)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort

#find TP (protein, total, serum) #Protein, Total, Ser/Plas  - DONE (JD)
unqLabs[grep("TP|Protein|protein|Serum|serum",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("TP|Protein|protein|Serum|serum",
       unqLabs)]))])
unqLabs[grep("TP|Protein|protein|Serum|serum",unqLabs)][c(1)]
lowBound <-min(range(na.omit(as.numeric(wear$TP)))) - (3*sd(na.omit(as.numeric(wear$TP))))
upBound <- max(range(na.omit(as.numeric(wear$TP)))) + (3*sd(na.omit(as.numeric(wear$TP))))
TP <- which(labs$LAB_NAME %in% unqLabs[
  grep("TP|Protein|protein|Serum|serum",unqLabs)][c(1)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(TP),]
range(as.numeric(wear$TP),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$TP)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#find UALB (urine albumin) #"Albumin, Urine", "Albumin, U"  - DONE (JD)
unqLabs[grep("UALB|Urine|Albumin|albumin",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("UALB|Urine|Albumin|albumin",
       unqLabs)]))])
unqLabs[grep("UALB|Urine|Albumin|albumin",unqLabs)][c(6,133)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$UALB)))) + (3*sd(na.omit(as.numeric(wear$UALB))))
UALB <- which(labs$LAB_NAME %in% unqLabs[
  grep("UALB|Urine|Albumin|albumin",unqLabs)][c(6,133)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(UALB),]
range(as.numeric(wear$UALB),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$UALB)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)
#Converting units may be needed here.

#find UALBCR (urine albumin creatine) #no direct match found
#unqLabs[grep("UALBCR|U|Urine|Albumin|albumin|Creatine|creatine",unqLabs)]
#table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
#  grep("UALBCR|U|Urine|Albumin|albumin|Creatine|creatine",
#       unqLabs)]))])
#unqLabs[grep("UALB|U|Urine|Albumin|albumin",unqLabs)][c(18,207)]
#UALB <- which(labs$LAB_NAME %in% unqLabs[
#  grep("UALB|U|Urine|Albumin|albumin",unqLabs)][c(18,207)])
#test <- labs[c(UALB),]
#range(as.numeric(wear$UALB),na.rm=TRUE)
#range(as.numeric(test$ORD_VALUE),na.rm=TRUE)

#find UALBCRINTP (urine albumin creatine) #no direct match found
#unqLabs[grep("UALB|U|Urine|Albumin|albumin",unqLabs)]
#unqLabs[grep("UALB|U|Urine|Albumin|albumin",unqLabs)][c(18,207)]
#UALB <- which(labs$LAB_NAME %in% unqLabs[
#  grep("UALB|U|Urine|Albumin|albumin",unqLabs)][c(18,207)])
#test <- labs[c(UALB),]
#range(as.numeric(wear$UALB),na.rm=TRUE)
#range(as.numeric(test$ORD_VALUE),na.rm=TRUE)

#find WBC (white blood cell count) #WBC  - DONE (JD)
unqLabs[grep("WBC|White|blood|count",unqLabs)]
table(labs$LAB_NAME[c(which(labs$LAB_NAME %in% unqLabs[
  grep("WBC|White|blood|count",
       unqLabs)]))])
unqLabs[grep("WBC|White|blood|count",unqLabs)][c(1,4)]
lowBound <-0
upBound <- max(range(na.omit(as.numeric(wear$WBC)))) + (3*sd(na.omit(as.numeric(wear$WBC))))
WBC <- which(labs$LAB_NAME %in% unqLabs[
  grep("WBC|White|blood|count",unqLabs)][c(1,4)] & (as.numeric(labs$ORD_VALUE) < upBound) & (as.numeric(labs$ORD_VALUE) > lowBound))
test <- labs[c(WBC),]
range(as.numeric(wear$WBC),na.rm=TRUE)
range(as.numeric(test$ORD_VALUE),na.rm=TRUE)
summary(as.numeric(wear$WBC)) # summary of values from iPOP cohort
summary(as.numeric(test$ORD_VALUE)) # summary of values from 30K cohort
length(test$ORD_VALUE)# number of tests in 30k cohort
length(unique(test$ANON_ID)) # number of people that have the test in 30k cohort
table(test$REFERENCE_UNIT)
table(test$ORD_VALUE)

#### CREATE NEW SUBSET TABLE FROM LABS DATA:

clinTests <- list(A1C,AG,ALB,ALCRU,ALKP,ALT,AST,BASO,
                  BASOAB,BUN,CA,CHOL,CHOLHDL,CL,CO2,
                  CR,EOS,EOSAB,GLOB,GLU_byMeter,
                  GLU_fasting,GLU_nonFasting,GLU_SerPlas,
                  GLU_wholeBld,HCT,HDL,
                  HGB,HSCRP,K,LDL_Calc,LDL_Direct,
                  LDLHDL,LYM,LYMAB,MCH,MCHC,MCV,MONO,
                  MONOAB,NA.,NEUT,NEUTAB,NHDL,PLT,RBC,
                  RDW,TBIL,TGL,TP,UALB,WBC)

clinNames <- c("A1C","AG","ALB","ALCRU","ALKP","ALT","AST","BASO",
               "BASOAB","BUN","CA","CHOL","CHOLHDL","CL","CO2",
               "CR","EOS","EOSAB","GLOB","GLU_byMeter",
               "GLU_fasting","GLU_nonFasting","GLU_SerPlas",
               "GLU_wholeBld","HCT","HDL",
               "HGB","HSCRP","K","LDL_Calc","LDL_Direct",
               "LDLHDL","LYM","LYMAB","MCH","MCHC","MCV","MONO",
               "MONOAB","NA.","NEUT","NEUTAB","NHDL","PLT","RBC",
               "RDW","TBIL","TGL","TP","UALB","WBC")

for(i in 1:length(clinTests)){
  labs$LAB_NAME[c(clinTests[[i]])] <- clinNames[i]
  print(i)
}

clinIdx <- c(A1C,AG,ALB,ALCRU,ALKP,ALT,AST,BASO,
             BASOAB,BUN,CA,CHOL,CHOLHDL,CL,CO2,
             CR,EOS,EOSAB,GLOB,GLU_byMeter,
             GLU_fasting,GLU_nonFasting,GLU_SerPlas,
             GLU_wholeBld,HCT,HDL,
             HGB,HSCRP,K,LDL_Calc,LDL_Direct,
             LDLHDL,LYM,LYMAB,MCH,MCHC,MCV,MONO,
             MONOAB,NA.,NEUT,NEUTAB,NHDL,PLT,RBC,
             RDW,TBIL,TGL,TP,UALB,WBC)

clinSub <- labs[c(clinIdx),]

labs <- clinSub
labs <- data.frame(labs)

write.csv(clinSub,
          paste0("/Users/jessilyn/Documents/Career_Development/Mentoring/RyanRunge/20170803_FINAL_LASSOS/",
                 "SECURE_lab_results_20180905_subset.csv"),
          row.names=FALSE)


#Note: IGM, UALBCR, and UALBCRINTP are left out (not found in new labs)

#### PREP LABS DATA ####

# labs <- fread("/Users/jessilyn/Documents/Career_Development/Mentoring/RyanRunge/20170803_FINAL_LASSOS/SECURE_lab_results_20180905_subset.csv",
#       header=TRUE,sep=',',stringsAsFactors=FALSE)
labs = fread("SECURE_lab_results_20180905_subset.csv",header = TRUE, stringsAsFactors = FALSE)

#Rename columns
names(labs)[which(names(labs)=="TAKEN_DATE")] <- "Clin_Result_Date"

#Reformat dates
labs$Clin_Result_Date <- format(
  as.Date(labs$Clin_Result_Date, "%d-%b-%Y"), "%Y-%m-%d")

#remove all non-numeric characters
#labs$ORD_VALUE <- gsub("[^0-9.]","",
#                       labs$ORD_VALUE) #this keeps decimals

#make all clinical values numeric
labs$ORD_VALUE <- as.numeric(as.character(labs$ORD_VALUE))

#### FORMAT DATA FRAME FOR CORRELATION MATRIX ####

#check
labs <- unique(labs[,c(1:9)])

#Change labs data to wide format
test <- reshape(labs[,c("ANON_ID","Clin_Result_Date",
                        "LAB_NAME","ORD_VALUE")],
                varying = NULL,
                idvar = c("ANON_ID","Clin_Result_Date"),
                timevar = c("LAB_NAME"),
                direction = "wide")
grep("^ORD_VALUE.+",names(test))
names(test)[grep("^ORD_VALUE.+",names(test))] <- 
  gsub("^ORD_VALUE.","",names(test)[grep("^ORD_VALUE.+",names(test))])
labs <- test

#### PREP VITALS DATA ####

#Rename columns
names(vitals)[which(names(vitals)=="CONTACT_DATE")] <- "Clin_Result_Date"
names(vitals)[which(names(vitals)=="PULSE")] <- "Pulse"
names(vitals)[which(names(vitals)=="TEMPERATURE")] <- "Temp"

#Reformat dates
vitals$Clin_Result_Date <- format(
  as.Date(vitals$Clin_Result_Date, "%d-%b-%Y"), "%Y-%m-%d")

#Make correlation variables numeric
vitals[,c("Pulse")] <- as.numeric(as.character(vitals[,c("Pulse")]))
vitals[,c("Temp")] <- as.numeric(as.character(vitals[,c("Temp")]))

#Describe and Plot hist
describe(vitals$Pulse); hist(vitals$Pulse, breaks=100)
describe(vitals$Temp); hist(vitals$Temp, breaks=100)

#Create freq. table
table(vitals$Pulse)
table(vitals$Temp)

#Merge data
corDf <- merge(labs,
               vitals[,c("ANON_ID","Clin_Result_Date",
                         "Pulse","Temp")],
               by=c("ANON_ID","Clin_Result_Date"),
               all.x = TRUE)

#Separate ID/Date columns from corDf
# ID_Date <- corDf[,c("ANON_ID","Clin_Result_Date")]
# corDf <- corDf[,-c(which(
#   names(corDf) %in% c("ANON_ID","Clin_Result_Date")))]

#Remove NAs from pulse and temp columns (so only extract visits that have BOTH vitals and lab tests in the same visit)
sum(!is.na(corDf$Pulse))
sum(!is.na(corDf$Temp))
test <- corDf[!is.na(corDf$Pulse),]
test <- test[!is.na(test$Temp),]
corDf <- test

#characterize the data set
people<-unique(corDf$ANON_ID)
length(people) # Number of unique individuals in the data set
numObservations <- length(corDf$Pulse) 
numObservations # Number of total individuals in the data set

write.csv(corDf,
          paste0("/Users/jessilyn/Documents/Career_Development/Mentoring/RyanRunge/20170803_FINAL_LASSOS/",
                 "20180905_Cleaned_joined_30k_labs_vitals.csv"),
          row.names=FALSE)
#corDf = fread("20180905_Cleaned_joined_30k_labs_vitals.csv")

testPeople <- people[sample(nrow(corDf), length(people)/3), ]
trainingPeople <- people[people %in% !testPeople]

#### RUN CORRELATIONS ####
allClin <- c("A1C","AG","ALB","ALCRU","ALKP","ALT","AST","BASO",
             "BASOAB","BUN","CA","CHOL","CHOLHDL","CL","CO2",
             "CR","EOS","EOSAB","GLOB","GLU_byMeter",
             "GLU_fasting","GLU_nonFasting","GLU_SerPlas",
             "GLU_wholeBld","HCT","HDL",
             "HGB","HSCRP","K","LDL_Calc","LDL_Direct","LDLHDL","LYM","LYMAB",
             "MCH","MCHC","MCV","MONO","MONOAB","NA.","NEUT",
             "NEUTAB","NHDL","PLT","RBC","RDW","TBIL","TGL","TP",
             "UALB","WBC")

# for each lab run a multiple correlation:

p<-c()
for (i in allClin){
  print(i)
  call <-paste0("corDf$",i)
  df <- cbind(corDf[[i]], corDf[,c("Pulse", "Temp")])
  df <- na.omit(df)
  rpulse = (df$Pulse)
  rtemp = (df$Temp)
  rresp = df$V1
  p[i]<-summary(lm(rresp ~ rpulse + rtemp))$adj.r.squared 
  }
options("scipen"=100, "digits"=4)
sort(p)

#Univariate Correlations by Ryan 
vitalVars <- which(names(corDf) %in% c("Pulse","Temp"))
clinVars <- which(!(names(corDf) %in% c("Pulse","Temp")))

results <- corr.test(corDf[,c(clinVars)],
                     corDf[,c(vitalVars)],
                     method="pearson",adjust="fdr")



## 20170903_MULT_REG_ON_30K

# multiple regression on clinical/vitals

# names(vitals)[which(names(vitals)=="CONTACT_DATE")] <- "Date"
# names(labs)[which(names(labs)=="TAKEN_DATE")] <- "Date"

#Reformat dates
# labs$Date <- as.Date(labs$Date, "%d-%b-%Y")
# vitals$Date <-  as.Date(vitals$Date, "%d-%b-%Y")







