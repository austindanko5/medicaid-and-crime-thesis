setwd("~/Documents/Penn Masters/CRIM 603")
###FROM DAVID'S CODE
#Import libraries 
library(estimatr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(texreg)

#turn scientific notation off:
options(scipen=100)
###################
#once medicaid dataset is made, run just this line then go to plots/analysis section
load("medicaid crime thesis 2.RData")

#################### MEDICAID DATASET (medicaid) #####################

#source 1 (2014-2019): https://www.kff.org/health-reform/state-indicator/total-monthly-medicaid-and-chip-enrollment/?currentTimeframe=27&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#source 2 (2010-2013): https://www.kff.org/medicaid/issue-brief/medicaid-enrollment-june-2013-data-snapshot/view/print/

##### VARIABLES (ORIG) #####

#STATE = state name
#ABB = state abbreviation
#YEAR
#POST_EXP = (pre/post treatment variable) dummy variable delineating whether or not medicaid expansion was in effect
#EXP_ENROLL = dummy for whether expansion was announced (enrollment began) this year (and years in effect after announcement)
#RELATIVEYEAR = 0 for year exp went into effect, # years after or before exp in effect
#JUNE_ENROLL = # of people enrolled in medicaid in june of each year
medicaid <- read.csv("4:5 medicaid late adopters inc.csv")
head(medicaid)
dim(medicaid)
#clean data
#medicaid$JUNE_ENROLL <- gsub(",", "", medicaid$JUNE_ENROLL) #remove commas from enrollment #s
class(medicaid$JUNE_ENROLL)
medicaid$POST_EXP <- as.numeric(medicaid$POST_EXP)
medicaid$POST_EXP <- ifelse(medicaid$EXPANYEAR == 1, 1, medicaid$POST_EXP)
class(medicaid$didexp)
class(medicaid$t_minus1)
#medicaid$JUNE_ENROLL <- as.numeric(medicaid$JUNE_ENROLL)
#CREATE identifier variable for state-year
medicaid$id <- paste(medicaid$ABB, medicaid$YEAR)
#Each place is treated at different times
##add didexp variable to differentiate between states that did and did not expand caid
medicaid$didexp <- ifelse(!is.na(medicaid$RELYEAR), 1, 0)
sum(medicaid$didexp)/10 #38 did expand (checking work)
table(medicaid$YEAR, medicaid$didexp) #12 states did not expand (checking work)
#distinguish between states that did expand before 2019 (i have some post-data) and those that expanded in 2019 or later 
#before 2017
STATESexpby17 <- subset(medicaid, didexp==1 & YEAR < 2017 & EXPANYEAR==1)$STATE
medicaid$expby17 <- ifelse(medicaid$STATE %in% STATESexpby17, 1, 0)

table(medicaid$YEAR, medicaid$EXPANYEAR)
#2017 or later
STATESlateexp <- unique(subset(medicaid, RELYEAR <= -7)$STATE)
medicaid$lateexp <- ifelse(medicaid$STATE %in% STATESlateexp, 1, 0)

#for graphing
medicaid$expgroup <- ifelse(medicaid$expby19==1, 3,0)
medicaid$expgroup <- ifelse(medicaid$lateexp==1, 2, medicaid$expgroup)
medicaid$expgroup <- ifelse(medicaid$didexp==0, 1, medicaid$expgroup)
table(medicaid$expgroup)


####################### UCR DATASET (ucr10to19) ##########################

#source: jacob kaplan: https://www.openicpsr.org/openicpsr/project/100707/version/V17/view

###### UCR QUESTIONS ######

##### 2010-2019 UCR DATA WITH INDEX CRIMES #####
jkucr <- readRDS("ucr/2010-2019 ucr r data/offenses_known_yearly_1960_2020.rds")
head(jkucr)
jkucr <- subset(jkucr, year %in% 2010:2019)

#create smaller df with selected crimes and characteristics 
ucr10to19 <- data.frame(STATE = jkucr$state,
                        ABB = jkucr$state_abb,
                        YEAR = jkucr$year,
                        AGENCY = jkucr$agency_name,
                        MSNG_MNTH = jkucr$number_of_months_missing,
                        LAST_MNTH_RPTD = jkucr$last_month_reported,
                        POP = jkucr$population_1,
                        MURDER = jkucr$actual_murder,
                        RAPE = jkucr$actual_rape_total,
                        ROBBERY = jkucr$actual_robbery_total,
                        AGG_ASSAULT = jkucr$actual_assault_aggravated,
                        BURGLARY = jkucr$actual_burg_total,
                        LARCENY = jkucr$actual_theft_total,
                        GTA = jkucr$actual_mtr_veh_theft_total,
                        ALL_CRIMES = jkucr$actual_all_crimes,
                        INDEX_VIOLENT = jkucr$actual_index_violent,
                        INDEX_PROPERTY = jkucr$actual_index_property,
                        stringsAsFactors=FALSE)

#NOTE ON CRIMES: Arson included in index property count, but not individually 
unique(ucr10to19$STATE)
#need to get rid of non-states
ucr10to19 <- subset(ucr10to19, ucr10to19$STATE != "american samoa")
ucr10to19 <- subset(ucr10to19, ucr10to19$STATE != "canal zone")
ucr10to19 <- subset(ucr10to19, ucr10to19$STATE != "district of columbia")
ucr10to19 <- subset(ucr10to19, ucr10to19$STATE != "guam")
ucr10to19 <- subset(ucr10to19, ucr10to19$STATE != "puerto rico")
ucr10to19 <- subset(ucr10to19, ucr10to19$STATE != "virgin islands")
unique(ucr10to19$STATE)

#add id columns as state-year identifier
ucr10to19$id <- paste(ucr10to19$ABB, ucr10to19$YEAR)
head(ucr10to19$id)
unique(ucr10to19$id)

#REORDER UCR by state and then year
ucr10to19 <- ucr10to19[order(ucr10to19$STATE, ucr10to19$YEAR),]

# GET STATE POP
statepop <- aggregate(POP ~ id + YEAR + STATE, ucr10to19, sum)
#get % pop enrolled in caid
statepop$ucrcaidpct <- (medicaid$JUNE_ENROLL/statepop$POP)*100
statepop
#add to medicaid df (must match up with ucr format)
medicaid$STATE <- tolower(medicaid$STATE)
medicaid <- merge(medicaid, statepop)


#### COMBINE UCR DATA WITH MEDICAID DATA ####
ALLcrime <- aggregate(ALL_CRIMES ~ id + YEAR + STATE, ucr10to19, sum)
ALLcrime
medicaid <- merge(medicaid, ALLcrime)

violentcrime <- aggregate(INDEX_VIOLENT ~ id + YEAR + STATE, ucr10to19, sum)
violentcrime
medicaid <- merge(medicaid, violentcrime)

propertycrime <- aggregate(INDEX_PROPERTY ~ id + YEAR + STATE, ucr10to19, sum)
medicaid <- merge(medicaid, propertycrime)

homi <- aggregate(MURDER ~ id + YEAR + STATE, ucr10to19, sum)
medicaid <- merge(medicaid, homi)

rape <- aggregate(RAPE ~ id + YEAR + STATE, ucr10to19, sum)
medicaid <- merge(medicaid, rape)

rob <- aggregate(ROBBERY ~ id + YEAR + STATE, ucr10to19, sum)
medicaid <- merge(medicaid, rob)

aggass <- aggregate(AGG_ASSAULT ~ id + YEAR + STATE, ucr10to19, sum)
medicaid <- merge(medicaid, aggass)

burg <- aggregate(BURGLARY ~ id + YEAR + STATE, ucr10to19, sum)
medicaid <- merge(medicaid, burg)

larc <- aggregate(LARCENY ~ id + YEAR + STATE, ucr10to19, sum)
medicaid <- merge(medicaid, larc)

gta <- aggregate(GTA ~ id + YEAR + STATE, ucr10to19, sum)
medicaid <- merge(medicaid, gta)


########################### CENSUS DATA #############################
#install.packages("R.utils")
#library(R.utils)

########## 2010-2019 Large Dataset #########
#gunzip("ipums census/usa_00011.csv.gz") #only unzip once
census10to19 <- read.csv("ipums census/usa_00011.csv")
head(census10to19)

#STATEFIP - state in which HH located; according to codebook - ordered alphabetically
#eliminate some data (only want 50 states)
unique(census10to19$STATEFIP)
census10to19 <- subset(census10to19, STATEFIP != 11)
#census10to19$id <- paste(census10to19$STATEFIP, census10to19$YEAR)
#unique(census10to19$id)
#ADD STATEFIP column to medicaid 
#medicaid$STATEFIP <- sex$STATEFIP
#medicaid$id <- paste(medicaid$STATEFIP, medicaid$YEAR)

#create state variable
states <- data.frame(STATE = unique(medicaid$STATE),
                     ABB = unique(medicaid$ABB),
                     STATEFIP = unique(census10to19$STATEFIP))
class(census10to19$STATEFIP)
census10to19$STATE <- census10to19$STATEFIP
#use codes in "states" to get state names for statefip
census10to19$STATE <- ifelse(census10to19$STATEFIP == 1, "alabama", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 2, "alaska", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 4, "arizona", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 5, "arkansas", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 6, "california", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 8, "colorado", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 9, "connecticut", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 10, "delaware", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 12, "florida", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 13, "georgia", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 15, "hawaii", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 16, "idaho", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 17, "illinois", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 18, "indiana", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 19, "iowa", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 20, "kansas", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 21, "kentucky", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 22, "louisiana", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 23, "maine", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 24, "maryland", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 25, "massachusetts", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 26, "michigan", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 27, "minnesota", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 28, "mississippi", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 29, "missouri", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 30, "montana", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 31, "nebraska", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 32, "nevada", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 33, "new hampshire", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 34, "new jersey", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 35, "new mexico", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 36, "new york", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 37, "north carolina", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 38, "north dakota", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 39, "ohio", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 40, "oklahoma", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 41, "oregon", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 42, "pennsylvania", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 44, "rhode island", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 45, "south carolina", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 46, "south dakota", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 47, "tennessee", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 48, "texas", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 49, "utah", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 50, "vermont", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 51, "virginia", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 53, "washington", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 54, "west virginia", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 55, "wisconsin", census10to19$STATE)
census10to19$STATE <- ifelse(census10to19$STATEFIP == 56, "wyoming", census10to19$STATE)
unique(census10to19$STATE)

### sex
unique(census10to19$SEX)
# 1 = male
# 2 = F
census10to19$MALE <- ifelse(census10to19$SEX == 1, 1, 0)
census10to19$FEMALE <- ifelse(census10to19$SEX == 2, 1, 0)
#aggmale <- aggregate(SEXM ~ YEAR + STATE, census10to19, sum) 
#aggfemale <- aggregate(SEXF ~ YEAR + STATE, census10to19, sum) 
sex <- aggregate(cbind(MALE,FEMALE) ~ YEAR + STATE, census10to19, sum)
sex
#first create pop variable to base percentages off of:(calculated off of sum of M and F for each year/state)
medicaid$cspopdenom <- sex$MALE+sex$FEMALE
head(medicaid$cspopdenom)
#add sex percentages and merge with medicaid
sex$MALE <- (sex$MALE/medicaid$cspopdenom)*100
sex$FEMALE <- (sex$FEMALE/medicaid$cspopdenom)*100
medicaid <- merge(medicaid, sex)



### race
unique(census10to19$RACE)
# 1 = white
# 2 = black
# 3 = Amer Indian or Alaska Native
# 4 = Chinese
# 5 = Japanese
# 6 = Other Asian/Pacific Islander
# 7 = Other race, nec
# 8 = 2 major races
# 9 = 3 or more major races
census10to19$WHITE <- ifelse(census10to19$RACE == 1, 1, 0)
census10to19$BLACK <- ifelse(census10to19$RACE == 2, 1, 0)
census10to19$OTHERRACE <- ifelse(census10to19$RACE %in% 3:9, 1, 0)
race <- aggregate(cbind(WHITE,BLACK,OTHERRACE) ~ YEAR + STATE, census10to19, sum)
race
#add % to  df (cspopdenom works here - same sum) and merge
race$WHITE <- (race$WHITE/medicaid$cspopdenom)*100
race$BLACK <- (race$BLACK/medicaid$cspopdenom)*100
race$OTHERRACE <- (race$OTHERRACE/medicaid$cspopdenom)*100
medicaid <- merge(medicaid, race)

# HINSCAID (health insurance through medicaid)
unique(census10to19$HINSCAID)
# 1 = no insurance through medicaid
# 2 = has health ins through medicaid
#adding medicaid column (0 = no medicaid;  1 = medicaid)
census10to19$CSCAID <- ifelse(census10to19$HINSCAID == 2, 1, 0)
cscaid <- aggregate(CSCAID ~ YEAR + STATE, census10to19, sum)
#add % to medicaid and #compare to UCR POP and June Enroll:
cscaid$CSCAID <- (cscaid$CSCAID/medicaid$cspopdenom)*100
cscaid
medicaid <- merge(medicaid, cscaid)

# EDUC - highest year completed 
unique(census10to19$EDUC)
# 0 = no school
# 1 = nursery school:grade 4
# 2 = grade 5:8
# 3 = grade 9
# 4 = grade 10
# 5 = grade 11
# 6 = grade 12
# 7 = 1 year college
# 8 = 2 years college
#### 9 = 3 years college - no one answered
# 10 = 4 years college
# 11 = 5+ years of college
sort(unique(census10to19$EDUC))
census10to19$LESSTHAN_HS <- ifelse(census10to19$EDUC %in% 0:2, 1, 0)
census10to19$SOMEHS <- ifelse(census10to19$EDUC %in% 3:5, 1, 0)
census10to19$HSDEG <- ifelse(census10to19$EDUC == 6, 1, 0)
census10to19$SOMECOL <- ifelse(census10to19$EDUC %in% 7:8, 1, 0)
census10to19$COLDEG <- ifelse(census10to19$EDUC == 10, 1, 0)
census10to19$ADVDEG <- ifelse(census10to19$EDUC == 11, 1, 0)
edu <- aggregate(cbind(LESSTHAN_HS,SOMEHS,HSDEG,SOMECOL,COLDEG,ADVDEG) 
                 ~ YEAR + STATE, census10to19, sum)
edu
#add % to df (cspopdenom works here - same sum) and merge
edu$LESSTHAN_HS <- (edu$LESSTHAN_HS/medicaid$cspopdenom)*100
edu$SOMEHS <- (edu$SOMEHS/medicaid$cspopdenom)*100
edu$HSDEG <- (edu$HSDEG/medicaid$cspopdenom)*100
edu$SOMECOL <- (edu$SOMECOL/medicaid$cspopdenom)*100
edu$COLDEG <- (edu$COLDEG/medicaid$cspopdenom)*100
edu$ADVDEG <- (edu$ADVDEG/medicaid$cspopdenom)*100
medicaid <- merge(medicaid, edu)

#EMPSTAT - employment status
unique(census10to19$EMPSTAT)
# 0 = N/A
# 1 = employed
# 2 = unemployed
# 3 = not in labor force
census10to19$EMPLOYED <- ifelse(census10to19$EMPSTAT == 1, 1, 0)
census10to19$UNEMPLOYED <- ifelse(census10to19$EMPSTAT == 2, 1, 0) 
emp <- aggregate(cbind(EMPLOYED,UNEMPLOYED) ~ YEAR + STATE, census10to19, sum)
#add % to df and merge
#do i need % labor force?
inlaborforce <- with(emp, EMPLOYED+UNEMPLOYED)
#emp$LABFC <- (emp$inlaborforce/medicaid$cspopdenom)*100
###for % employed, do i do employed/laborforce or out of the population?
emp$EMPLOYED <- (emp$EMPLOYED/medicaid$cspopdenom)*100
emp$UNEMP_RATE <- with(emp, 100*UNEMPLOYED/inlaborforce)
emp
medicaid <- merge(medicaid, emp[,c("YEAR", "STATE", "EMPLOYED", "UNEMP_RATE")])

#RELATE - relation to head of HH - using to get female headed HH
unique(census10to19$RELATE)
# 1 = head/householder
census10to19$FEMALEHEAD <- ifelse(census10to19$RELATE == 1 & census10to19$FEMALE == 1, 1, 0)
femhh <- aggregate(FEMALEHEAD ~ YEAR + STATE, census10to19, sum)
femhh
femhh$FEMALEHEAD <- (femhh$FEMALEHEAD/medicaid$cspopdenom)*100
medicaid <- merge(medicaid, femhh)

#INCTOT - total income and FTOTINC - total family income
#9999999 - N/A
unique(census10to19$INCTOT)
unique(census10to19$FTOTINC)
class(census10to19$INCTOT)
census10to19$INCTOT <- ifelse(census10to19$INCTOT==9999999, NA, census10to19$INCTOT)
census10to19$FTOTINC <- ifelse(census10to19$FTOTINC==9999999, NA, census10to19$FTOTINC)
income <- aggregate(cbind(INCTOT, FTOTINC) ~ YEAR + STATE, census10to19, median)
income
medicaid <- merge(medicaid, income)


save(medicaid,file="medicaid crime thesis 2.RData",compress=TRUE)


load("medicaid crime thesis 2.RData")


########################### SUMMARY STATS ############################

stargazer(med2, 
          type = "text")
#WHAT DO I NEED TO INCLUDE IN THIS
stargazer(medicaid[medicaid$expby17==1,], 
          summary.stat = c("mean", "sd"),
          title = "Summary Statistics",
          digits = 1,
          omit = c("YEAR","EXPANYEAR","RELYEAR","POST_EXP","didexp","expby17",
                   "lateexp","ucrcaidpct","CSCAID","cspopdenom","t_minus11","t_minus10",
                   "t_minus9","t_minus8","t_minus7","t_minus6","t_minus5",
                   "t_minus4","t_minus3","t_minus2","t_minus1","t_plus1",
                   "t_plus2","t_plus3","t_plus4","t_plus5","logJUNE_ENROLL",
                   "logALL_CRIMES","logINDEX_VIOLENT","logINDEX_PROPERTY",
                   "logMURDER","logRAPE","logROBBERY","logAGG_ASSAULT",
                   "logBURGLARY","logLARCENY","logGTA","expgroup"),
          covariate.labels = c("June Enrollment","Population","All Crime",
                               "Violent Crime","Property Crime","Murder",
                               "Rape","Robbery","Aggravated Assault","Burglary",
                               "Larceny","Motor Vehicle Theft","Percent Male",
                               "Percent Female","Percent White","Percent Black",
                               "Percent Other Race", 
                               "Percent with Less than High School", 
                               "Percent with Some High School",
                               "Percent with High School Degree",
                               "Percent with Some College",
                               "Percent with College Degree",
                               "Percent with Advanced Degree",
                               "Percent Employed","Unemployment Rate",
                               "Percent with Female Head of Household",
                               "Individual Income","Family Income"),
          notes = "Notes: Mean and standard deviation.",
          out = "TableSummaryStatistics3.doc",
          type = "text")



############################ PLOTS ############################
### *********PANEL A: PLOT JUNE ENROLL FOR ALL STATES ALL YEARS
ggplot(medicaid, aes(x = YEAR, y = (JUNE_ENROLL/POP)*100000)) +
  geom_point(aes(color = factor(ABB)), alpha = 0.5) +
  labs(x = "Year", y = "June Enrollment (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 1. Yearly Medicaid Enrollment by State") +
  geom_smooth(alpha = 0.2, se = FALSE, color = "darkblue") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2)))

#showing expansion vs no expansion
ggplot(medicaid, aes(x = YEAR, y = (JUNE_ENROLL/POP)*100000)) +
  geom_point(aes(color = factor(ABB)), alpha = 0.5) +
  labs(x = "Year", y = "June Enrollment (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 1. Yearly Medicaid Enrollment by State") +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 1, 
              se = FALSE, color = "red") +
  geom_smooth(data = medicaid[medicaid$didexp==1,], alpha = 1, 
              se = FALSE, color = "blue") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 11, ncol = 5, override.aes = list(size = 2))) +
  annotate(geom = "text", x = 2016, y = 18500, label = "Non-Expansion States", size = 3, fontface = "bold") +
  annotate(geom = "text", x = 2017, y = 23500, label = "Expansion States", size = 3, fontface = "bold") 

#early expansion vs late expansion vs no expansion
ggplot(medicaid, aes(x = YEAR, y = (JUNE_ENROLL/POP)*100000)) +
  geom_point(aes(color = factor(ABB)), alpha = 0.5) +
  labs(x = "Year", y = "June Enrollment (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 1. Yearly Medicaid Enrollment by State") +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 1, 
              se = FALSE, color = "red") +
  geom_smooth(data = medicaid[medicaid$lateexp==1,], alpha = 1, 
              se = FALSE, color = "blue") +
  geom_smooth(data = medicaid[medicaid$expby17==1,], alpha = 1, 
              se = FALSE, color = "green") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2))) +
  annotate(geom = "text", x = 2016, y = 18500, label = "Non-Expansion", size = 3, fontface = "bold") +
  annotate(geom = "text", x = 2017, y = 14400, label = "Expansion 2017-2021", size = 3, fontface = "bold") +
  annotate(geom = "text", x = 2017, y = 23500, label = "Expansion by 2017", size = 3, fontface = "bold") 


#### ***** PANEL B: PLOT JUST REGRESSION LINES OF EVER TREATED AND THEN NEVER TREATED STATES
#one graph, two lines, showing trends over time
ggplot(medicaid, aes(x = YEAR, y = (JUNE_ENROLL/POP)*100000)) +
  labs(x = "Year", y = "June Enrollment (per 100,000 people)", 
       color = NULL, 
       title = "Trends in Medicaid Enrollment, Ever vs. Never Treated") +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 0.2, 
              se = FALSE, aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$didexp==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) + 
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  scale_color_manual(values = c("lm1" = "red", "lm2" = "blue"), 
                     labels = c("Never Expanded", "Expanded")) +
  guides(color = guide_legend(nrow = 1, ncol = 2, override.aes = list(size = 3))) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        axis.line = element_line(color = "black"),
        panel.background = element_blank(), 
        legend.key = element_rect(color = NA, fill = NA),
        legend.text = element_text(size = 12),
        legend.position = "bottom")
#same, no title
ggplot(medicaid, aes(x = YEAR, y = (JUNE_ENROLL/POP)*100000)) +
  labs(x = "Year", y = "June Enrollment (per 100,000 people)", 
       color = NULL) +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 0.2, 
              se = FALSE, aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$didexp==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) + 
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  scale_color_manual(values = c("lm1" = "red", "lm2" = "blue"), 
                     labels = c("Never Expanded", "Expanded")) +
  guides(color = guide_legend(nrow = 1, ncol = 2, override.aes = list(size = 3))) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        axis.line = element_line(color = "black"),
        panel.background = element_blank(), 
        legend.key = element_rect(color = NA, fill = NA),
        legend.text = element_text(size = 12),
        legend.position = "bottom")
#### ***** PANEL C: PLOT JUST REGRESSION LINES OF EARLY (BY 2017) VS LATE TREATED STATES
#one graph, three lines, showing trends over time
ggplot(medicaid, aes(x = YEAR, y = (JUNE_ENROLL/POP)*100000)) +
  labs(x = "Year", y = "June Enrollment (per 100000 residents)", 
       color = NULL,
       title = "Figure 3. Trends in Medicaid Enrollment, Early vs. Late vs. Never Expansion") +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 0.2, 
              se = FALSE, aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$lateexp==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) +
  geom_smooth(data = medicaid[medicaid$expby17==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm3")) + 
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  scale_color_manual(values = c("lm1" = "red", "lm2" = "blue", "lm3" = "green"), 
                     labels = c("Never Expanded","Expansion 2017-2021", "Expansion by 2017")) +
  guides(color = guide_legend(nrow = 3, ncol = 1, override.aes = list(size = 2))) 

################## FOR POSTER

#### ***** PANEL D: PLOT JUST REGRESSION LINES OF EARLY (BY 2017) VS NEVER TREATED STATES
#one graph, two lines, showing trends over time
ggplot(medicaid, aes(x = YEAR, y = (JUNE_ENROLL/POP)*100000)) +
  labs(x = "Year", y = "June Enrollment (per 100,000 residents)", 
       color = NULL,
       title = "Trends in Medicaid Enrollment",
       subtitle = "States That Implemented Expansion Before January 2017 vs. States That Have Not Expanded") +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 0.2, 
              se = FALSE, aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$expby17==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  scale_color_manual(values = c("lm1" = "red", "lm2" = "blue"), 
                     labels = c("Never Expanded","Expanded by 2017")) +
  guides(color = guide_legend(nrow = 1, ncol = 2, override.aes = list(size = 3))) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        axis.line = element_line(color = "black"),
        panel.background = element_blank(), 
        legend.key = element_rect(color = NA, fill = NA),
        legend.text = element_text(size = 12),
        legend.position = "bottom")
###################


##### PLOT TOTAL (ALL TYPES) CRIME, ALL STATES 
ggplot(medicaid, aes(x = YEAR, y = (ALL_CRIMES/POP)*100000)) +
  geom_point(aes(color = factor(ABB)), alpha = 0.5) +
  labs(x = "Year", y = "All Crime (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 4. Trends in All Crime",
       subtitle = "All 50 States") +
  geom_smooth(alpha = 0.2, se = FALSE, color = "darkblue") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2))) 

#all crime exp states vs non-exp states
ggplot(medicaid, aes(x = YEAR, y = (ALL_CRIMES/POP)*100000)) +
  labs(x = "Year", y = "All Crime (per 100,000 residents)", 
       color = NULL) +
        geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 0.2, 
              se = FALSE, aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$didexp==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  scale_color_manual(values = c("lm1" = "red", "lm2" = "blue"), 
                     labels = c("Never Expanded", "Expanded"))  +
  guides(color = guide_legend(nrow = 1, ncol = 2, override.aes = list(size = 3))) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        axis.line = element_line(color = "black"),
        panel.background = element_blank(), 
        legend.key = element_rect(color = NA, fill = NA),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

#all crime early exp states vs all others (non-exp and late states)
ggplot(medicaid, aes(x = YEAR, y = (ALL_CRIMES/POP)*100000)) +
  labs(x = "Year", y = "All Crime (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 4. Trends in All Crime",
       subtitle = "States That Expanded by 2017 vs. All Other States") +
  geom_smooth(data = medicaid[medicaid$expby17==0,], alpha = 0.2, 
              se = FALSE,aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$expby17==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2))) +
  scale_color_manual(values = c("lm1" = "red", "lm2" = "green"), 
                     labels = c("Never Expanded or Expanded Late", "Expanded by 2017"))

#all crime early exp states vs non-exp states
ggplot(medicaid, aes(x = YEAR, y = (ALL_CRIMES/POP)*100000)) +
  labs(x = "Year", y = "All Crime (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 4. Trends in All Crime",
       subtitle = "States That Expanded by 2017 vs. States That Have Not Expanded") +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 0.2, 
              se = FALSE,aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$expby17==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2))) +
  scale_color_manual(values = c("lm1" = "red", "lm2" = "green"), 
                     labels = c("Never Expanded", "Expanded by 2017"))

#all crime early exp states vs late exp states
ggplot(medicaid, aes(x = YEAR, y = (ALL_CRIMES/POP)*100000)) +
  labs(x = "Year", y = "All Crime (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 4. Trends in All Crime",
       subtitle = "States That Expanded by 2017 vs. States That Expanded 2017-2021 vs. States That Have Not Expanded") +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 0.2, 
              se = FALSE,aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$lateexp==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) +
  geom_smooth(data = medicaid[medicaid$expby17==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm3")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2))) +
  scale_color_manual(values = c("lm1" = "red", "lm2" = "blue", "lm3" = "green"), 
                     labels = c("Never Expanded", "Expanded 2017-2021", "Expanded by 2017"))


##### PLOT TOTAL VIOLENT CRIME, ALL STATES 
ggplot(medicaid, aes(x = YEAR, y = (INDEX_VIOLENT/POP)*100000)) +
  geom_point(aes(color = factor(ABB)), alpha = 0.5) +
  labs(x = "Year", y = "Violent Crime (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 4. Trends in Violent Crime",
       subtitle = "All States") +
  geom_smooth(alpha = 0.2, se = FALSE, color = "darkblue") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2))) 

#violent crime exp states vs non-exp states
ggplot(medicaid, aes(x = YEAR, y = (INDEX_VIOLENT/POP)*100000)) +
  labs(x = "Year", y = "Violent Crime (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 4. Trends in Violent Crime",
       subtitle = "States That Have Expanded vs. States That Have Not") +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 0.2, 
              se = FALSE, aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$didexp==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2))) +
  scale_color_manual(values = c("lm1" = "red", "lm2" = "blue"), 
                     labels = c("Never Expanded", "Expanded"))
#early v late
ggplot(medicaid, aes(x = YEAR, y = (ALL_CRIMES/POP)*100000)) +
  labs(x = "Year", y = "All Crime (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 4. Trends in All Crime",
       subtitle = "States That Expanded By 2017 vs. States That Have Not Expanded") +
  geom_smooth(data = medicaid[medicaid$didexp==0,], alpha = 0.2, 
              se = FALSE, aes(color = "lm1")) +
  geom_smooth(data = medicaid[medicaid$expby17==1,], alpha = 0.2, 
              se = FALSE, aes(color = "lm2")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2))) +
  scale_color_manual(values = c("lm1" = "blue", "lm2" = "green"), 
                     labels = c("Never Expanded", "Expanded Early"))


##### PLOT TOTAL PROPERTY CRIME, ALL STATES
ggplot(medicaid, aes(x = YEAR, y = (INDEX_PROPERTY/POP)*100000)) +
  geom_point(aes(color = factor(ABB)), alpha = 0.5) +
  labs(x = "Year", y = "Property Crime (per 100,000 residents)", 
       color = NULL, 
       title = "Figure 4. Trends in Property Crime") +
  geom_smooth(alpha = 0.2, se = FALSE, color = "darkblue") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  guides(color = guide_legend(nrow = 10, ncol = 5, override.aes = list(size = 2))) 

#Plot property crimes exp states vs non-exp states




############################ FOR DIF IN DIF/ EVENT STUDY ANALYSIS ######################

#Each place is treated at different times
table(medicaid$STATE, medicaid$POST_EXP) #total 10 years, number for each state is how many years pre/post
table(medicaid$STATE, medicaid$RELYEAR)
table(medicaid$RELYEAR)


#create relative time variables (t- and t+)
#need to double check what to do if expansion occurred in 2020 or later --> do i include a -10?
medicaid$t_minus11 <- ifelse(medicaid$RELYEAR == -11, 1, 0)
medicaid$t_minus10 <- ifelse(medicaid$RELYEAR == -10, 1, 0)
medicaid$t_minus9 <- ifelse(medicaid$RELYEAR == -9, 1, 0)
medicaid$t_minus8 <- ifelse(medicaid$RELYEAR == -8, 1, 0)
medicaid$t_minus7 <- ifelse(medicaid$RELYEAR == -7, 1, 0)
medicaid$t_minus6 <- ifelse(medicaid$RELYEAR == -6, 1, 0)
medicaid$t_minus5 <- ifelse(medicaid$RELYEAR == -5, 1, 0)
medicaid$t_minus4 <- ifelse(medicaid$RELYEAR == -4, 1, 0)
medicaid$t_minus3 <- ifelse(medicaid$RELYEAR == -3, 1, 0)
medicaid$t_minus2 <- ifelse(medicaid$RELYEAR == -2, 1, 0)
medicaid$t_minus1 <- ifelse(medicaid$RELYEAR == -1, 1, 0)
medicaid$t_plus1 <- ifelse(medicaid$RELYEAR == 1, 1, 0)
medicaid$t_plus2 <- ifelse(medicaid$RELYEAR == 2, 1, 0)
medicaid$t_plus3 <- ifelse(medicaid$RELYEAR == 3, 1, 0)
medicaid$t_plus4 <- ifelse(medicaid$RELYEAR == 4, 1, 0)
medicaid$t_plus5 <- ifelse(medicaid$RELYEAR == 5, 1, 0)

#need to use natural logs for full numbers to get smaller answers that can be interpreted as percents
medicaid$logJUNE_ENROLL <- log(medicaid$JUNE_ENROLL)
medicaid$logALL_CRIMES <- log(medicaid$ALL_CRIMES)
medicaid$logINDEX_VIOLENT <- log(medicaid$INDEX_VIOLENT)
medicaid$logINDEX_PROPERTY <- log(medicaid$INDEX_PROPERTY)
medicaid$logMURDER <- log(medicaid$MURDER)
medicaid$logRAPE <- log(medicaid$RAPE)
medicaid$logROBBERY <- log(medicaid$ROBBERY)
medicaid$logAGG_ASSAULT <- log(medicaid$AGG_ASSAULT)
medicaid$logBURGLARY <- log(medicaid$BURGLARY)
medicaid$logLARCENY <- log(medicaid$LARCENY)
medicaid$logGTA <- log(medicaid$GTA)

save(medicaid,file="medicaid crime thesis 2.RData",compress=TRUE)


load("medicaid crime thesis.RData")

#DID and ES
#turn scientific notation off:
options(scipen=100)

# FIRST SHOW THAT ENROLLMENT INCREASED AFTER EXP 
#FULL SAMPLE: EXPANSION AND NON_EXPANSION STATES
#log regression (dependent variable) - default is natural log (which is what i want)
medicaid$JUNEENROLLRT <- (medicaid$JUNE_ENROLL/medicaid$POP)*100000
medicaid$JUNEENROLLRTlog <- log(medicaid$JUNEENROLLRT)
lmJUNEENROLLlog0 <- lm_robust(JUNEENROLLRTlog ~ POST_EXP, 
                             data = medicaid[medicaid$expby17==1,], 
                             fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                           se_type = "stata")
summary(lmJUNEENROLLlog0) 
lmJUNEENROLLlog0 <- lm_robust(JUNEENROLLRTlog ~ POST_EXP, 
                              data = medicaid[medicaid$didexp==1,], 
                              fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                              se_type = "stata")
summary(lmJUNEENROLLlog0) 
#with covars, basic DID
lmJUNEENROLLlog1 <- lm_robust(JUNEENROLLRTlog ~ POST_EXP + POP + MALE + 
                                BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                                SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                                UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                              data = medicaid[medicaid$expby17==1,], 
                              fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                              se_type = "stata")
summary(lmJUNEENROLLlog1) #expanding medicaid - 21% increase in JUNE ENROLL??

lmJUNEENROLLrt <- lm_robust(JUNEENROLLRT ~ POST_EXP + POP + MALE + 
                                BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                                SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                                UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                              data = medicaid[medicaid$expby17==1,], 
                              fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                              se_type = "stata")
summary(lmJUNEENROLLrt)
a <- mean(medicaid[medicaid$expby17==1,]$JUNEENROLLRT, na.rm=TRUE)
b <- lmJUNEENROLLrt$coefficients[1]
(b/a)*100

lmJUNEENROLLrt <- lm_robust(JUNEENROLLRT ~ POST_EXP + POP + MALE + 
                              BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                              SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                              UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                            data = medicaid[medicaid$didexp==1,], 
                            fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                            se_type = "stata")
summary(lmJUNEENROLLrt)
a <- mean(medicaid[medicaid$didexp==1,]$JUNEENROLLRT, na.rm=TRUE)
b <- lmJUNEENROLLrt$coefficients[1]
(b/a)*100

lmJUNEENROLLlog1 <- lm_robust(JUNEENROLLRTlog ~ POST_EXP + POP + MALE + 
                                BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                                SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                                UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                              data = medicaid[medicaid$didexp==1,], 
                              fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                              se_type = "stata")
summary(lmJUNEENROLLlog1)
#enrollment, EVENT STUDY - automatically only exp states
lmJUNEENROLLlog2 <- lm_robust(JUNEENROLLRT ~ t_minus6 + t_minus5 + t_minus4 + 
                                t_minus3 + t_minus2 + 
                                t_plus1 + t_plus2 + t_plus3 + t_plus4,
                              data = medicaid[medicaid$expby17==1,], fixed_effects = ~ STATE + YEAR, 
                              clusters = STATE, se_type = "stata")
summary(lmJUNEENROLLlog2)
lmJUNEENROLLlog3 <- lm_robust(JUNEENROLLRT ~ t_minus6 + t_minus5 + t_minus4 + 
                                t_minus3 + t_minus2 + 
                                t_plus1 + t_plus2 + t_plus3 + t_plus4 + POP + MALE + 
                                BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                                SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                                UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC,
                              data = medicaid[medicaid$expby17==1,], 
                              fixed_effects = ~ STATE + YEAR, 
                              clusters = STATE, se_type = "stata")
summary(lmJUNEENROLLlog3)
#notes on just expansion anly: 
#when comparing to David's script/results from class, he includes all NYCHA;
#he has same degree freedom in diff in diff and event study results
#this is because all NYCHA experienced gang takedown
#mine is different because not all 50 states expanded medicaid, so must manually exclude nonexp states
#Get the coefficients and std. errors in a data frame
dftemp <- data.frame("RELYEAR" = c(-6:-2, 0:3), 
                     "coef" = lmJUNEENROLLlog3$coefficients[1:9], 
                     "se" = lmJUNEENROLLlog3$std.error[1:9])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-14000, 8000), ylab = "Estimate", 
     xlab = "Time period relative to medicaid expansion",
     main = "June Enrollment in Medicaid, Event Study")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0) 
#big values bc many states with less than 6 years
#percent change
# The t-1 period is the leave-out period. 

# ALL CRIME 
#all crime, full sample - all states
medicaid$ALLCRIMERT <- (medicaid$ALL_CRIMES/medicaid$POP)*100000
# crime_it = alpha + beta_t*D_it + beta2*state_i + beta3*year_t + gamma_x*X_it + e_it 
#crime main outcome
#beta_t - not just one single point estimate (t-1, etc)
#state_i, year_t fixed effects
#gamma_x - says add multiple covariates

#gamma_x*X_it - DID
#cluster se at state level (note)

lmALL1 <- lm_robust(ALLCRIMERT ~ POST_EXP + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid, 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmALL1)
#all crimes, just early expansion states
lmALL2 <- lm_robust(ALLCRIMERT ~ POST_EXP + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmALL2)
# all crime, event study: -86 crimes?
lmALL3 <- lm_robust(ALLCRIMERT ~ t_minus6 + t_minus5 + t_minus4 + t_minus3 + 
                      t_minus2 +  t_plus1 + t_plus2 + t_plus3 + t_plus4 +
                      POP + MALE + BLACK + OTHERRACE + LESSTHAN_HS + 
                      SOMEHS + SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                      FEMALEHEAD + INCTOT + FTOTINC,
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, 
                    clusters = STATE, se_type = "stata")
summary(lmALL3)
#Get the coefficients and std. errors in a data frame
dftemp <- data.frame("RELYEAR" = c(-6:-2, 0:4), 
                     "coef" = lmALL3$coefficients[1:10], 
                     "se" = lmALL3$std.error[1:10])
dftemp
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-2500, 1000), ylab = "Estimate", 
     xlab = "Time period relative to medicaid expansion",
     main = "All Crime, Event Study")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0) 

# VIOLENT CRIME 
#violent, full sample - all states
medicaid$VIORATE <- (medicaid$INDEX_VIOLENT/medicaid$POP)*100000
lmVIO0 <- lm_robust(VIORATE ~ POST_EXP + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid, 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmVIO0)
#violent, just early expansion states: -15 violent crimes?
lmVIO1 <- lm_robust(VIORATE ~ POST_EXP + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                 data = medicaid[medicaid$expby17==1,], 
                 fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                 se_type = "stata")
summary(lmVIO1)
# violent, event study
lmVIO2 <- lm_robust(VIORATE ~ t_minus6 + t_minus5 + t_minus4 + 
                      t_minus3 + t_minus2 + t_plus1 + t_plus2 + t_plus3 + 
                      t_plus4 + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, 
                 clusters = STATE, se_type = "stata")
summary(lmVIO2)
#Get the coefficients and std. errors in a data frame
dftemp <- data.frame("RELYEAR" = c(-6:-2, 0:3), 
                     "coef" = lmVIO2$coefficients[1:9], 
                     "se" = lmVIO2$std.error[1:9])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-350, 200), ylab = "Estimate", 
     xlab = "Time period relative to medicaid expansion",
     main = "Violent Crime, Event Study")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)


# PROPERTY CRIME 
#full sample - all states
medicaid$PROPRATE <- (medicaid$INDEX_PROPERTY/medicaid$POP)*100000
lmPROP0 <- lm_robust(PROPRATE ~ POST_EXP + POP + MALE + BLACK + OTHERRACE + 
                       LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                       EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC,
                    data = medicaid, 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmPROP0)
#property crime, just early expansion states
lmPROP1 <- lm_robust(PROPRATE ~ POST_EXP + POP + MALE + BLACK + OTHERRACE + 
                       LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                       EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC,
                 data = medicaid[medicaid$expby17==1,], 
                 fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                 se_type = "stata")
summary(lmPROP1)
lmPROP2 <- lm_robust(PROPRATE ~ t_minus6 + t_minus5 + t_minus4 + 
                       t_minus3 + t_minus2 + t_plus1 + t_plus2 + t_plus3 + 
                       t_plus4 + POP + MALE + BLACK + OTHERRACE + 
                       LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                       EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC,  
                     data = medicaid[medicaid$expby17==1,], fixed_effects = ~ STATE + YEAR, 
                 clusters = STATE, se_type = "stata")
summary(lmPROP2)
#plot
dftemp <- data.frame("RELYEAR" = c(-6:-2, 0:3), 
                     "coef" = lmPROP2$coefficients[1:9], 
                     "se" = lmPROP2$std.error[1:9])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-1500, 1500), ylab = "Estimate", 
     xlab = "Time period relative to medicaid expansion",
     main = "Property Crime, Event Study")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)


# MURDER 
lmHOM0 <- lm_robust(MURDERRT ~ POST_EXP + POP + MALE + 
                      BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                      UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmHOM0)

lmHOM0 <- lm_robust(MURDERRT ~ t_minus4 + t_minus3 + t_minus2 + 
                      t_plus1 + t_plus2 + t_plus3 + t_plus4 + POP + 
                      MALE + BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS + 
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                      FEMALEHEAD + INCTOT + FTOTINC,  
                    data = medicaid[medicaid$expby17==1,], fixed_effects = ~ STATE + YEAR, 
                    clusters = STATE, se_type = "stata")
summary(lmHOM0)
#plot
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmHOM0$coefficients[1:7], 
                     "se" = lmHOM0$std.error[1:7])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-1, 1), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)
###

# RAPE
lmRAPE0 <- lm_robust(logRAPE ~ POST_EXP + POP + MALE + 
                       BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                       UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                     se_type = "stata")
summary(lmRAPE0)
lmRAPE0 <- lm_robust(RAPERT ~ t_minus4 + t_minus3 + 
                       t_minus2 + t_plus1 + t_plus2 + t_plus3 + t_plus4 + POP + 
                       MALE + BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS + 
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                       FEMALEHEAD + INCTOT + FTOTINC,  
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, 
                     clusters = STATE, se_type = "stata")
summary(lmRAPE0)
#plot
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmRAPE0$coefficients[1:7], 
                     "se" = lmRAPE0$std.error[1:7])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-15, 20), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)
###

# ROBBERY
lmROB0 <- lm_robust(logROBBERY ~ POST_EXP + POP + MALE + 
                      BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                      UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmROB0)
lmROB0 <- lm_robust(ROBRT ~  t_minus4 + t_minus3 + t_minus2 + 
                      t_plus1 + t_plus2 + t_plus3 + t_plus4 + POP + 
                      MALE + BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS + 
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                      FEMALEHEAD + INCTOT + FTOTINC,  
                    data = medicaid[medicaid$expby17==1,], fixed_effects = ~ STATE + YEAR, 
                    clusters = STATE, se_type = "stata")
summary(lmROB0)
#plot
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmROB0$coefficients[1:7], 
                     "se" = lmROB0$std.error[1:7])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-15, 20), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)
###

# AGG ASSAULT
lmAGGASS0 <- lm_robust(logAGG_ASSAULT ~ POST_EXP + POP + MALE + 
                         BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                         SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                         UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                       data = medicaid[medicaid$expby17==1,], 
                       fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                       se_type = "stata")
summary(lmAGGASS0)
lmAGGASS0 <- lm_robust(AGGASSRT ~  t_minus4 + t_minus3 + t_minus2 + 
                         t_plus1 + t_plus2 + t_plus3 + t_plus4 + POP + 
                         MALE + BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS + 
                         SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                         FEMALEHEAD + INCTOT + FTOTINC,  
                       data = medicaid[medicaid$expby17==1,], fixed_effects = ~ STATE + YEAR, 
                       clusters = STATE, se_type = "stata")
summary(lmAGGASS0)
#plot
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmAGGASS0$coefficients[1:7], 
                     "se" = lmAGGASS0$std.error[1:7])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-50, 50), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)
###


#BURGLARY
lmBURG0 <- lm_robust(logBURGLARY ~ POST_EXP + POP + MALE + 
                       BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                       UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                     se_type = "stata")
summary(lmBURG0)
lmBURG0 <- lm_robust(BURGRT ~  t_minus4 + t_minus3 + t_minus2 + 
                       t_plus1 + t_plus2 + t_plus3 + t_plus4 + POP + 
                       MALE + BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS + 
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                       FEMALEHEAD + INCTOT + FTOTINC,  
                     data = medicaid[medicaid$expby17==1,], fixed_effects = ~ STATE + YEAR, 
                     clusters = STATE, se_type = "stata")
summary(lmBURG0)
#plot
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmBURG0$coefficients[1:7], 
                     "se" = lmBURG0$std.error[1:7])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-100, 100), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)
###

#LARCENY
lmLARC0 <- lm_robust(logLARCENY ~ POST_EXP + POP + MALE + 
                       BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                       UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                     se_type = "stata")
summary(lmLARC0)
lmLARC0 <- lm_robust(LARCRT ~ t_minus4 + t_minus3 + t_minus2 + 
                       t_plus1 + t_plus2 + t_plus3 + t_plus4 + POP + 
                       MALE + BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS + 
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                       FEMALEHEAD + INCTOT + FTOTINC,  
                     data = medicaid[medicaid$expby17==1,], fixed_effects = ~ STATE + YEAR, 
                     clusters = STATE, se_type = "stata")
summary(lmLARC0)
#plot
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmLARC0$coefficients[1:7], 
                     "se" = lmLARC0$std.error[1:7])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-200, 200), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)
###

#GTA - early exp
lmGTA0 <- lm_robust(logGTA ~ POST_EXP + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmGTA0)
lmGTA0 <- lm_robust(GTART ~ t_minus4 + t_minus3 + t_minus2 + 
                      t_plus1 + t_plus2 + t_plus3 + t_plus4 + POP + 
                      MALE + BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS + 
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                      FEMALEHEAD + INCTOT + FTOTINC,  
                    data = medicaid[medicaid$expby17==1,], fixed_effects = ~ STATE + YEAR, 
                    clusters = STATE, se_type = "stata")
summary(lmGTA0)
#plot
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmGTA0$coefficients[1:7], 
                     "se" = lmGTA0$std.error[1:7])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-100, 100), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)
###


##################### POSTER FIGURES ######################
# ALL CRIME EVENT STUDY:
#each estimate (for t-6, t-5, etc) represents the difference between control and treat states in each year
lmALL3 <- lm_robust(ALLCRIMERT ~ t_minus6 + t_minus5 + t_minus4 + t_minus3 + 
                      t_minus2 +  t_plus1 + t_plus2 + t_plus3 + t_plus4 +
                      POP + MALE + BLACK + OTHERRACE + LESSTHAN_HS + 
                      SOMEHS + SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                      FEMALEHEAD + INCTOT + FTOTINC,
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, 
                    clusters = STATE, se_type = "stata")
summary(lmALL3)
#Get the coefficients and std. errors in a data frame
dftemp <- data.frame("RELYEAR" = c(-6:-2, 0:3), 
                     "coef" = lmALL3$coefficients[1:9], 
                     "se" = lmALL3$std.error[1:9])
dftemp
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-2500, 1000), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion",
     main = "Changes in All Crime (per 100,000 people), Event Study")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0) 

#VIOLENT CRIME, EVENT STUDY:
#each estimate (for t-6, t-5, etc) represents the difference between control and treat states in each year
lmVIO2 <- lm_robust(VIORATE ~ t_minus6 + t_minus5 + t_minus4 + 
                      t_minus3 + t_minus2 + t_plus1 + t_plus2 + t_plus3 + 
                      t_plus4 + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, 
                    clusters = STATE, se_type = "stata")
summary(lmVIO2)
#Get the coefficients and std. errors in a data frame
dftemp <- data.frame("RELYEAR" = c(-6:-2, 0:3), 
                     "coef" = lmVIO2$coefficients[1:9], 
                     "se" = lmVIO2$std.error[1:9])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-350, 200), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion",
     main = "Changes in Violent Crime (per 100,000 people), Event Study")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)

#PROPERTY CRIME, EVENT STUDY:
#each estimate (for t-6, t-5, etc) represents the difference between control and treat states in each year
lmPROP2 <- lm_robust(PROPRATE ~ t_minus6 + t_minus5 + t_minus4 + 
                       t_minus3 + t_minus2 + t_plus1 + t_plus2 + t_plus3 + 
                       t_plus4 + POP + MALE + BLACK + OTHERRACE + 
                       LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                       EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC,  
                     data = medicaid[medicaid$expby17==1,], fixed_effects = ~ STATE + YEAR, 
                     clusters = STATE, se_type = "stata")
summary(lmPROP2)

#plot
dftemp <- data.frame("RELYEAR" = c(-6:-2, 0:3), 
                     "coef" = lmPROP2$coefficients[1:9], 
                     "se" = lmPROP2$std.error[1:9])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-1500, 500), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion",
     main = "Changes in Property Crime (per 100,000 people), Event Study")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)

############## SPECIFIC CRIME TYPES, DIFF IN DIFF PLOT
medicaid$MURDERRT <- (medicaid$MURDER/medicaid$POP)*100000
medicaid$RAPERT <- (medicaid$RAPE/medicaid$POP)*100000
medicaid$ROBRT <- (medicaid$ROBBERY/medicaid$POP)*100000
medicaid$AGGASSRT <- (medicaid$AGG_ASSAULT/medicaid$POP)*100000
medicaid$BURGRT <- (medicaid$BURGLARY/medicaid$POP)*100000
medicaid$LARCRT <- (medicaid$LARCENY/medicaid$POP)*100000
medicaid$GTART <- (medicaid$GTA/medicaid$POP)*100000


#MURDER DID
lmHOM0 <- lm_robust(MURDERRT ~ POST_EXP + POP + MALE + 
                      BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                      UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmHOM0)

# RAPE DID
lmRAPE0 <- lm_robust(RAPERT ~ POST_EXP + POP + MALE + 
                       BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                       UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                     se_type = "stata")
summary(lmRAPE0)
# ROBBERY DID
lmROB0 <- lm_robust(ROBRT ~ POST_EXP + POP + MALE + 
                      BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                      UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmROB0)
# AGG ASSAULT DID
lmAGGASS0 <- lm_robust(AGGASSRT ~ POST_EXP + POP + MALE + 
                         BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                         SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                         UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                       data = medicaid[medicaid$expby17==1,], 
                       fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                       se_type = "stata")
summary(lmAGGASS0)
#BURGLARY DID
lmBURG0 <- lm_robust(BURGRT ~ POST_EXP + POP + MALE + 
                       BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                       UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                     se_type = "stata")
summary(lmBURG0)
#LARCENY DID
lmLARC0 <- lm_robust(LARCRT ~ POST_EXP + POP + MALE + 
                       BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                       UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                     se_type = "stata")
summary(lmLARC0)

#GTA DID
lmGTA0 <- lm_robust(GTART ~ POST_EXP + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmGTA0)
#Extract the coefficients and standard errors, compute the 95% confidence interval
dftemp <- data.frame("model" = c("Murder", "Rape", "Robbery", "Agg Assault", 
                                 "Burglary", "Larceny", "MVT"), #outcome vars
                     "coef" = c(lmHOM0$coefficients["POST_EXP"], 
                                lmRAPE0$coefficients["POST_EXP"],
                                lmROB0$coefficients["POST_EXP"],
                                lmAGGASS0$coefficients["POST_EXP"],
                                lmBURG0$coefficients["POST_EXP"],
                                lmLARC0$coefficients["POST_EXP"],
                                lmGTA0$coefficients["POST_EXP"]), #lmcrime post_exp for each crime
                     "se" = c(lmHOM0$std.error["POST_EXP"], 
                              lmRAPE0$std.error["POST_EXP"],
                              lmROB0$std.error["POST_EXP"],
                              lmAGGASS0$std.error["POST_EXP"],
                              lmBURG0$std.error["POST_EXP"],
                              lmLARC0$std.error["POST_EXP"],
                              lmGTA0$std.error["POST_EXP"]))
dftemp#lmcrime$std.error[post]
dftemp$ci.low <- dftemp$coef - 1.96*dftemp$se
dftemp$ci.up <- dftemp$coef + 1.96*dftemp$se

#Plot
ggplot(dftemp) +
  geom_pointrange(aes(x = model, y = coef, ymin = ci.low, ymax = ci.up)) +
  labs(y = "Estimate (95% confidence interval)", x = "",
       title = "Estimated Effect of Medicaid Expansion on Index Crimes",
       subtitle = "States That Expanded by 2017, Crimes per 100,000 people (Difference in Differences)") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-150, 120) + #adjust so get point est and ci
  theme_bw() 

#log of rates
medicaid$MURDERRTlog <- log(medicaid$MURDERRT)
medicaid$RAPERTlog <- log(medicaid$RAPERT)
medicaid$ROBRTlog <- log(medicaid$ROBRT)
medicaid$AGGASSRTlog <- log(medicaid$AGGASSRT)
medicaid$BURGRTlog <- log(medicaid$BURGRT)
medicaid$LARCRTlog <- log(medicaid$LARCRT)
medicaid$GTARTlog <- log(medicaid$GTART)

#all crime
lmALL0 <- lm_robust(ALLCRIMERT ~ POST_EXP + POP + MALE + 
                      BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                      UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmALL0)
a <- mean(medicaid[medicaid$expby17==1,]$ALLCRIMERT)
b <- lmALL0$coefficients[1]
(b/a)*100

#Prop DID
lmPROP0 <- lm_robust(PROPRATE ~ POST_EXP + POP + MALE + 
                      BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                      UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmPROP0)
a <- mean(medicaid[medicaid$expby17==1,]$PROPRATE)
b <- lmPROP0$coefficients[1]
(b/a)*100

#Vio DID
lmVIO0 <- lm_robust(VIORATE ~ POST_EXP + POP + MALE + 
                      BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                      UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmVIO0)
a <- mean(medicaid[medicaid$expby17==1,]$VIORATE)
b <- lmVIO0$coefficients[1]
(b/a)*100

dftemp <- data.frame("model" = c("All Crime", "Violent Crime", "Property Crime"), #outcome vars
                     "coef" = c(lmALL0$coefficients["POST_EXP"], 
                                lmVIO0$coefficients["POST_EXP"],
                                lmPROP0$coefficients["POST_EXP"]), #lmcrime post_exp for each crime
                     "se" = c(lmALL0$std.error["POST_EXP"], 
                              lmVIO0$std.error["POST_EXP"],
                              lmPROP0$std.error["POST_EXP"]))
dftemp#lmcrime$std.error[post]
dftemp$ci.low <- dftemp$coef - 1.96*dftemp$se
dftemp$ci.up <- dftemp$coef + 1.96*dftemp$se

#Plot
ggplot(dftemp) +
  geom_pointrange(aes(x = model, y = coef, ymin = ci.low, ymax = ci.up)) +
  labs(y = "Estimate (95% confidence interval)", x = "") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-400, 200) + #adjust so get point est and ci
  theme_bw() 

#MURDER DID
lmHOM0 <- lm_robust(MURDERRT ~ POST_EXP + POP + MALE + 
                      BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                      UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmHOM0)
a <- mean(medicaid[medicaid$expby17==1,]$MURDERRT)
b <- lmHOM0$coefficients[1]
(b/a)*100

# RAPE DID
lmRAPE0 <- lm_robust(RAPERT ~ POST_EXP + POP + MALE + 
                       BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                       UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                     se_type = "stata")
summary(lmRAPE0)
a <- mean(medicaid[medicaid$expby17==1,]$RAPERT)
b <- lmRAPE0$coefficients[1]
(b/a)*100
# ROBBERY DID
lmROB0 <- lm_robust(ROBRT ~ POST_EXP + POP + MALE + 
                      BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                      SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                      UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmROB0)
a <- mean(medicaid[medicaid$expby17==1,]$ROBRT)
b <- lmROB0$coefficients[1]
(b/a)*100
# AGG ASSAULT DID
lmAGGASS0 <- lm_robust(AGGASSRT ~ POST_EXP + POP + MALE + 
                         BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                         SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                         UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                       data = medicaid[medicaid$expby17==1,], 
                       fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                       se_type = "stata")
summary(lmAGGASS0)
a <- mean(medicaid[medicaid$expby17==1,]$AGGASSRT)
b <- lmAGGASS0$coefficients[1]
(b/a)*100
#BURGLARY DID
lmBURG0 <- lm_robust(BURGRT ~ POST_EXP + POP + MALE + 
                       BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                       UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                     se_type = "stata")
summary(lmBURG0)
a <- mean(medicaid[medicaid$expby17==1,]$BURGRT)
b <- lmBURG0$coefficients[1]
(b/a)*100
#LARCENY DID
lmLARC0 <- lm_robust(LARCRT ~ POST_EXP + POP + MALE + 
                       BLACK + OTHERRACE + LESSTHAN_HS + SOMEHS +
                       SOMECOL + COLDEG + ADVDEG + EMPLOYED + 
                       UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                     data = medicaid[medicaid$expby17==1,], 
                     fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                     se_type = "stata")
summary(lmLARC0)
a <- mean(medicaid[medicaid$expby17==1,]$LARCRT)
b <- lmLARC0$coefficients[1]
(b/a)*100

#GTA DID
lmGTA0 <- lm_robust(GTART ~ POST_EXP + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = medicaid[medicaid$expby17==1,], 
                    fixed_effects = ~ STATE + YEAR, clusters = STATE, 
                    se_type = "stata")
summary(lmGTA0)
a <- mean(medicaid[medicaid$expby17==1,]$GTART)
b <- lmGTA0$coefficients[1]
(b/a)*100
#Extract the coefficients and standard errors, compute the 95% confidence interval
dftemp <- data.frame("model" = c("Murder", "Rape", "Robbery", "Agg Assault", 
                                 "Burglary", "Larceny", "MVT"), #outcome vars
                     "coef" = c(lmHOM0$coefficients["POST_EXP"], 
                                lmRAPE0$coefficients["POST_EXP"],
                                lmROB0$coefficients["POST_EXP"],
                                lmAGGASS0$coefficients["POST_EXP"],
                                lmBURG0$coefficients["POST_EXP"],
                                lmLARC0$coefficients["POST_EXP"],
                                lmGTA0$coefficients["POST_EXP"]), #lmcrime post_exp for each crime
                     "se" = c(lmHOM0$std.error["POST_EXP"], 
                              lmRAPE0$std.error["POST_EXP"],
                              lmROB0$std.error["POST_EXP"],
                              lmAGGASS0$std.error["POST_EXP"],
                              lmBURG0$std.error["POST_EXP"],
                              lmLARC0$std.error["POST_EXP"],
                              lmGTA0$std.error["POST_EXP"]))
dftemp#lmcrime$std.error[post]
dftemp$ci.low <- dftemp$coef - 1.96*dftemp$se
dftemp$ci.up <- dftemp$coef + 1.96*dftemp$se

#Plot
ggplot(dftemp) +
  geom_pointrange(aes(x = model, y = coef, ymin = ci.low, ymax = ci.up)) +
  labs(y = "Estimate (95% confidence interval)", x = "") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  ylim(-150, 150) + #adjust so get point est and ci
  theme_bw() 
########################## FOR PRESENTATION AND PAPER:
#focus on subset of states w sufficient data (-4)
#if states expanded in 2014,2015, or 2016: have data on years t-4:t+3
table(medicaid$RELYEAR)
STATESmed2 <- subset(medicaid, YEAR %in% c(2014:2016) & EXPANYEAR==1)$STATE
med2 <- subset(medicaid, STATE %in% STATESmed2)
# ALL CRIME EVENT STUDY:
#each estimate (for t-6, t-5, etc) represents the difference between control and treat states in each year
lmALL3 <- lm_robust(ALLCRIMERT ~ t_minus4 + t_minus3 + 
                      t_minus2 +  t_plus1 + t_plus2 + t_plus3 + t_plus4 +
                      POP + MALE + BLACK + OTHERRACE + LESSTHAN_HS + 
                      SOMEHS + SOMECOL + COLDEG + ADVDEG + EMPLOYED + UNEMP_RATE + 
                      FEMALEHEAD + INCTOT + FTOTINC,
                    data = med2, 
                    fixed_effects = ~ STATE + YEAR, 
                    clusters = STATE, se_type = "stata")
summary(lmALL3)
#Get the coefficients and std. errors in a data frame
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmALL3$coefficients[1:7], 
                     "se" = lmALL3$std.error[1:7])
dftemp
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-600, 300), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0) 

summary(medicaid$POP)
#VIOLENT CRIME, EVENT STUDY:
#each estimate (for t-6, t-5, etc) represents the difference between control and treat states in each year
lmVIO2 <- lm_robust(VIORATE ~ t_minus4 + 
                      t_minus3 + t_minus2 + t_plus1 + t_plus2 + t_plus3 + 
                      t_plus4 + POP + MALE + BLACK + OTHERRACE + 
                      LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                      EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC, 
                    data = med2, 
                    fixed_effects = ~ STATE + YEAR, 
                    clusters = STATE, se_type = "stata")
summary(lmVIO2)
#Get the coefficients and std. errors in a data frame
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmVIO2$coefficients[1:7], 
                     "se" = lmVIO2$std.error[1:7])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-100, 100), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)

#PROPERTY CRIME, EVENT STUDY:
lmPROP2 <- lm_robust(PROPRATE ~ t_minus4 + 
                       t_minus3 + t_minus2 + t_plus1 + t_plus2 + t_plus3 + t_plus4 +
                   POP + MALE + BLACK + OTHERRACE + 
                       LESSTHAN_HS + SOMEHS + SOMECOL + COLDEG + ADVDEG + 
                       EMPLOYED + UNEMP_RATE + FEMALEHEAD + INCTOT + FTOTINC,  
                     data = med2, fixed_effects = ~ STATE + YEAR, 
                     clusters = STATE, se_type = "stata")
summary(lmPROP2)

#plot
dftemp <- data.frame("RELYEAR" = c(-4:-2, 0:3), 
                     "coef" = lmPROP2$coefficients[1:7], 
                     "se" = lmPROP2$std.error[1:7])
dftemp
#Plot the coefficients and SE
plot(x = dftemp$RELYEAR, y = dftemp$coef, 
     ylim = c(-600, 300), ylab = "Estimate", 
     xlab = "Time Period Relative to Medicaid Expansion")
lines(x = dftemp$RELYEAR, y = dftemp$coef + 1.96*dftemp$se, col = "grey", lty = 2) #adds the 95% confidence intervals (beta +/- 1.96*SE_beta)
lines(x = dftemp$RELYEAR, y = dftemp$coef - 1.96*dftemp$se, col = "grey", lty = 2)
grid()
abline(v = 0)
#diff in diff state specific - ask david








########################## REGRESSION TABLES ###############################

