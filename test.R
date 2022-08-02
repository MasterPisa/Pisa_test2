
#install.packages("devtools")
#install.packages("intsvy")
#install.packages("EdSurvey")
#install.packages("SPSStoR")

setwd("C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data")
#getwd()


library(haven)
library(devtools)
library(intsvy)
library(stringr)
library(EdSurvey)
library(pander)
library(usethis)
?use_github

edit_r_environ()

# create project : make sure Git is checked
# new file script and save it
# comit the file with git in the environ
# create personal acess token (PAT) in GITHUB an copy it
# run Usethis library
# edit_r_environ()
# copy your PAT : GITHUB_PAT = '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' and run (you can get the PAT using the main PISA project GITHUB)
# restart R session 
# lanch again Usethis library
# run "use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))"
# you will receive the message : √ Setting active project to 'C:/...
# say delection to yes
# you will see your code in your GitHub repository
# make change commit and push


#my token : GITHUB_PAT = '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' 
# ibtissam token : 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  

use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)`

2#get data directly
#sch_2018 <- read_sav("CY07_MSU_SCH_QQQ.sav")
#stu_2018 <- read_sav("CY07_MSU_STU_QQQ.sav")

##### Varaiables ######################
# w_fstuwt is the variable name of the student final weights

#stu_2018$

#########################################
# Reading Data in package intsvy ####
############################################



# Print variable labels and names of participating countries

# PISA
pisa.var.label(folder = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data",  student.file = "CY07_MSU_SCH_QQQ.sav",
               school.file= "CY07_MSU_STU_QQQ.sav", output = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data/" )

# Import PISA Data
start_time <- Sys.time()


pisa.de <- pisa.select.merge(folder = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data",
                             school.file = "CY07_MSU_SCH_QQQ.sav",
                             student.file = "CY07_MSU_STU_QQQ.sav",
                             student =c("ST001D01T",#Grade
                                        "ST004D01T",#Student (Standardized) Gender
                                        "HISCED",#Highest Education of parents (ISCED)
                                        "HISEI",#Highest In-ternational Socio-Economic Index of Occupational Status
                                        "PARED",#Index highest parental education in years of schooling
                                        "IMMIG",#Index Immigration status
                                        "ST127Q01TA",#Have you ever repeated a <grade>? At <ISCED 1>
                                        "ST127Q02TA",#Have you ever repeated a <grade>? At <ISCED 2>
                                        "REPEAT",# Grade repetition
                                        "GCSELFEFF",#Self-efficacy regarding global issues (WLE)
                                        "GCAWARE",#Student's awareness of global issues (WLE)
                                        "PERSPECT",#Perspective-taking (WLE)
                                        "COGFLEX",#Cognitive flexibility/adaptability (WLE)
                                        "AWACOM",#Awareness of intercultural communication (WLE)
                                        "INTCULT",#Student's interest in learning about other cultures (WLE)
                                        "RESPECT",#Respect for people from other cultures (WLE)
                                        "GLOBMIND",#Global-mindedness (WLE)
                                        "ATTIMM",#Student's attitudes towards immigrants (WLE)
                                        "PV1READ" , "PV2READ", "PV3READ", "PV4READ", "PV5READ" , "PV6READ", "PV7READ", "PV8READ", "PV9READ" , "PV10READ"),
                             
                             school = c(
                               "SC013Q01TA",#Is your school a public or a private school?
                               "SC042Q01TA",#School's policy for <national modal grade for 15-year-olds>: Students are grouped by ability into different classes.
                               "SC042Q02TA",#School's policy for <national modal grade for 15-year-olds>: Students are grouped by ability within their classes.
                               "SCMCEG",#School principal's view on teachers' multicultural and egalitarian beliefs (WLE)
                               "STUBEHA",#Student behaviour hindering learning (WLE)
                               "TEACHBEHA",#Teacher behaviour hindering learning (WLE)
                               "SC048Q01NA",#Percentage <national modal grade for 15-year-olds>: Students whose <heritage language> is different from <test language>
                               "SC048Q02NA",#Percentage <national modal grade for 15-year-olds>: Students with special needs
                               "SC048Q03NA",#Percentage <national modal grade for 15-year-olds>: Students from socioeconomically disadvantaged homes
                               "SCHSIZE",#School Size (Sum), "PROGN",
                               "SCHLTYPE"),#School Ownership
                             countries = c("DEU"))

pisa.de

Sys.time() - start_time # runtime for reading in data

# 
# # Frequency Table
props_tab1 <- pisa.table(variable = "ST001D01T", data = pisa.de)
pander(props_tab1,
       caption = "Frequency table Student grade. Data: PISA2018")

props_tab2 <- pisa.table(variable = "ST004D01T", data = pisa.de)
pander(props_tab2,
       caption = "Frequency table Student gender. Data: PISA2018")

pisa.table(variable = "PARED", data = pisa.de)
pisa.table(variable = "HISCED", data = pisa.de)

props_tab3 <- pisa.table(variable = "IMMIG", data = pisa.de)
pander(props_tab3,
       caption = "Frequency table Student Immigration. Data: PISA2018")


props_tab4 <- pisa.table(variable = "SCHLTYPE", data = pisa.de)
pander(props_tab4,
       caption = "Frequency table Student gender. Data: PISA2018")
# 
# 
# 
# # Calculating mean
pisa.mean(variable = "HISEI", data = pisa.de)
pisa.mean(variable = "HISEI", data = pisa.de, by = "CNTRYID")
# 
# 
# # Calculate Average student Performance
pisa.mean.pv(pvlabel = "READ",data = pisa.de)
# 
# # Average performance resutls by Grade/Sex
mean.pv <- pisa.mean.pv(pvlabel = "READ", 
                        data = pisa.de,
                        by = c("ST001D01T","ST004D01T"))

pander(mean.pv,
       caption = "Average performance by gender and sex. Data: PISA2018")
# 

mean.pv <- pisa.mean.pv(pvlabel = "READ", 
                        data = pisa.de)

#
pisa.table(variable = "SCHLTYPE", data = pisa.de)
pisa.table(variable = "SC013Q01TA", data = pisa.de)
# 
#
pisa.table(variable = "GCSELFEFF", data = pisa.de)

#
pisa.table(variable = "PROGN", data = pisa.de)

#
pisa.table(variable = "INTCULT", data = pisa.de)

#plots
hist(pisa.de$GCSELFEFF) 
hist(pisa.de$PV1READ) 
