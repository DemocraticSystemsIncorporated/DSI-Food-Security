library(tidyverse)
library(modelr)
library(ggplot2)

# Variables:
# HEFAMINC (household income) 
# HRNUMHOU (number of household members) 
# GCFIP (state) 
# PEEDUCA (education level) 
# PESEX (sex) 
# PTDTRACE (race) 
# PEMJOT (more than one job?)
# PEMJNUM (number of jobs if over 1) 
# PRCITSHP (citizenship status) 
# PUDIS (disability status) 
# PRDTIND2 (industry of employment) 
# HESP1 (did anyone get SNAP or foodstamp benefits in the last year)
# HETSP3O( dollar amount of food stamps received)
# HRFS12MD (measure of food security)

# You'll need to change the path if you're on a different computer.
dsi <- read_csv("C:/Users/harpe/Downloads/dec19pub.csv") 
target_variables <- select(dsi, HRFS12MD, HEFAMINC, HRNUMHOU, PEEDUCA, PESEX, PTDTRACE, PEMJNUM, PRCITSHP, PUDIS, PRDTIND2, HETSP3O, HESP1, PEMJOT, GCFIP)
organized_by_security <- arrange(target_variables, desc(HRFS12MD), HEFAMINC, HRNUMHOU, PEEDUCA, PESEX, PTDTRACE, PEMJNUM, PRCITSHP, PUDIS, PRDTIND2, HETSP3O, HESP1, PEMJOT, GCFIP)


# Singling out each variable.

preincome <- select(target_variables, HEFAMINC)
people <- select(target_variables, HRNUMHOU)
prestate <- select(target_variables, GCFIP)
preeducation <- select(target_variables, PEEDUCA)
presex <- select(target_variables, PESEX)
prerace <- select(target_variables, PTDTRACE)
job1 <- select(target_variables, PEMJOT)
job2 <- select(target_variables, PEMJNUM)
precitizenship <- select(target_variables, PRCITSHP)
predisability <- select(target_variables, PUDIS)
employment <- select(target_variables, PRDTIND2)
stamps <- select(target_variables, HESP1)
security <- select(target_variables, HRFS12MD)

# Mutating each variable to get a readable representation of the codes recorded.
# Divisions for ease of viewing (it gets chunky).

#------------------------------------------------

a <- preincome %>% mutate(inc = case_when(
  preincome == 1 ~ "Less than $5,000",
  preincome == 2 ~ "$5,000 - $7,499",
  preincome == 3 ~ "$7,500 - $9,999",
  preincome == 4 ~ "$10,000 - $12,499",
  preincome == 5 ~ "$12,500 - $14,999",
  preincome == 6 ~ "$15,000 - $19,999",
  preincome == 7 ~ "$20,000 - $24,999",
  preincome == 8 ~ "$25,000 - $29,999",
  preincome == 9 ~ "$30,000 - $34,999",
  preincome == 10 ~ "$35,000 - $39,999",
  preincome == 11 ~ "$40,000 - $49,999",
  preincome == 12 ~ "$50,000 - $59,999",
  preincome == 13 ~ "$60,000 - $74,999",
  preincome == 14 ~ "$75,000 - $99,999",
  preincome == 15 ~ "$100,000 - $149,999",
  preincome == 16 ~ "$150,000 or more"
))

# Take out readable representation to be put into a new tibble
Income <- select(a, inc)

#-----------------------------------------------

#Don't think there's any reason to transmute people as it's literally just a count.

#-----------------------------------------------

b <- prestate %>% mutate(states = case_when(
  prestate == 1 ~ "AL",
  prestate == 2 ~ "AK",
  prestate == 4 ~ "AZ",
  prestate == 5 ~ "AR",
  prestate == 6 ~ "CA",
  prestate == 8 ~ "CO",
  prestate == 9 ~ "CT",
  prestate == 10 ~ "DE",
  prestate == 11 ~ "DC",
  prestate == 12 ~ "FL",
  prestate == 13 ~ "GA",
  prestate == 15 ~ "HI",
  prestate == 16 ~ "ID",
  prestate == 17 ~ "IL",
  prestate == 18 ~ "IN",
  prestate == 19 ~ "IA",
  prestate == 20 ~ "KS",
  prestate == 21 ~ "KY",
  prestate == 22 ~ "LA",
  prestate == 23 ~ "ME",
  prestate == 24 ~ "MD",
  prestate == 25 ~ "MA",
  prestate == 26 ~ "MI",
  prestate == 27 ~ "MN",
  prestate == 28 ~ "MS",
  prestate == 29 ~ "MO",
  prestate == 30 ~ "MT",
  prestate == 31 ~ "NE",
  prestate == 32 ~ "NV",
  prestate == 33 ~ "NH",
  prestate == 34 ~ "NJ",
  prestate == 35 ~ "NM",
  prestate == 36 ~ "NY",
  prestate == 37 ~ "NC",
  prestate == 38 ~ "ND",
  prestate == 39 ~ "OH",
  prestate == 40 ~ "OK",
  prestate == 41 ~ "OR",
  prestate == 42 ~ "PA",
  prestate == 44 ~ "RI",
  prestate == 45 ~ "SC",
  prestate == 46 ~ "SD",
  prestate == 47 ~ "TN",
  prestate == 48 ~ "TX",
  prestate == 49 ~ "UT",
  prestate == 50 ~ "VT",
  prestate == 51 ~ "VA",
  prestate == 53 ~ "WA",
  prestate == 54 ~ "WV",
  prestate == 55 ~ "WI",
  prestate == 56 ~ "WY"
))

State <- select(b, states)

#-----------------------------------------------

c <- preeducation %>% mutate(edu = case_when(
  preeducation == 31 ~ "Less than 1st grade",
  preeducation == 32 ~ "1st, 2nd, 3rd, or 4th grade",
  preeducation == 33 ~ "5th or 6th grade",
  preeducation == 34 ~ "7th or 8th grade",
  preeducation == 35 ~ "9th grade",
  preeducation == 36 ~ "10th grade",
  preeducation == 37 ~ "11th grade",
  preeducation == 38 ~ "12th grade, no diploma",
  preeducation == 39 ~ "High school graduate",
  preeducation == 40 ~ "College, no diploma",
  preeducation == 41 ~ "Occupational/Vocational associate degree",
  preeducation == 42 ~ "Academic program associate degree",
  preeducation == 43 ~ "Bachelor's degree",
  preeducation == 44 ~ "Master's degree",
  preeducation == 45 ~ "Professional school degree",
  preeducation == 46 ~ "Doctorate degree"
))

Education <- select(c, edu)

#-----------------------------------------------

d <- presex %>% mutate(sexes = case_when(
  presex == 1 ~ "Male",
  presex == 2 ~ "Female"
))

Sex <- select(d, sexes)

#-----------------------------------------------

e <- prerace %>% mutate(races = case_when(
  prerace == 1 ~ "White",
  prerace == 2 ~ "Black",
  prerace == 3 ~ "American Indian/Alaskan Native",
  prerace == 4 ~ "Asian",
  prerace == 5 ~ "Hawaiian/Pacific Islander",
  prerace == 6 ~ "White-Black",
  prerace == 7 ~ "White-American Indian/Alaskan Native",
  prerace == 8 ~ "White-Asian",
  prerace == 9 ~ "White-Hawaiian/Pacific Islander",
  prerace == 10 ~ "Black-American Indian/Alaskan Native",
  prerace == 11 ~ "Black-Asian",
  prerace == 12 ~ "Black-Hawaiian/Pacific Islander",
  prerace == 13 ~ "American Indian/Alaskan Native-Asian",
  prerace == 14 ~ "American Indian/Alaskan Native-Hawaiian/Pacific Islander",
  prerace == 15 ~ "Asian-Hawaiian/Pacific Islander",
  prerace == 16 ~ "White-Black-American Indian/Alaskan Native",
  prerace == 17 ~ "White-Black-Asian",
  prerace == 18 ~ "White-Black-Hawaiian/Pacific Islander",
  prerace == 19 ~ "White-American Indian/Alaskan Native-Asian",
  prerace == 20 ~ "White-American Indian/Alaskan Native-Hawaiian/Pacific Islander",
  prerace == 21 ~ "White-Asian-Hawaiian/Pacific Islander",
  prerace == 22 ~ "Black-American Indian/Alaskan Native-Asian",
  prerace == 23 ~ "White-Black-American Indian/Alaskan Native-Asian",
  prerace == 24 ~ "White-American Indian/Alaskan Native-Asian-Hawaiian/Pacific Islander",
  prerace == 25 ~ "Other 3 race combinations",
  prerace == 26 ~ "Other 4 or 5 race combinations"
))

Race <- select(e, races)

#-----------------------------------------------

f <- job1 %>% mutate(jobs = case_when(
  job1 == 1 ~ "More than one job",
  job1 == 2 ~ "One job"
))

Number_of_Jobs <- select(f, jobs)

#-----------------------------------------------

g <- precitizenship %>% mutate(Cit = case_when(
  precitizenship == 1 ~ "Native, born in US",
  precitizenship == 2 ~ "Native, born in Puerto Rico or other island territories",
  precitizenship == 3 ~ "Native, born abroad to American parents/parent",
  precitizenship == 4 ~ "Foreign born, citizen by naturalization",
  precitizenship == 5 ~ "Foreign born, not a citizen"
))

Citizenship <- select(g, Cit)

#-----------------------------------------------

h <- predisability %>% mutate(Dis = case_when(
  predisability == 1 ~ "Disability prevents work",
  predisability == 2 ~ "Disability does not prevent work",
  predisability == 3 ~ "No disability"
))

Disability <- select(h, Dis)

#-----------------------------------------------

i <- employment %>% mutate(Ind = case_when(
  employment == 1 ~ "Agriculture",
  employment == 2 ~ "Forestry/logging/fishing/hunting/trapping",
  employment == 3 ~ "Mining",
  employment == 4 ~ "Construction",
  employment == 5 ~ "Nonmetallic mineral production/manufacturing",
  employment == 6 ~ "Metals and fabricated metal products",
  employment == 7 ~ "Machinery manufacturing",
  employment == 8 ~ "Computer/electronic manufacturing",
  employment == 9 ~ "Electrical equipment/appliance manufacturing",
  employment == 10 ~ "Transportation equipment manufacturing",
  employment == 11 ~ "Wood products",
  employment == 12 ~ "Furniture and fixtures manufacturing",
  employment == 13 ~ "Miscellaneous manufacturing",
  employment == 14 ~ "Food manufacturing",
  employment == 15 ~ "Beverages and tobacco products",
  employment == 16 ~ "Textile/apparel/leather manufacturing",
  employment == 17 ~ "Paper and printing",
  employment == 18 ~ "Petroleum and coal products manufacturing",
  employment == 19 ~ "Chemical manufacturing",
  employment == 20 ~ "Plastics and rubber",
  employment == 21 ~ "Wholesale trade",
  employment == 22 ~ "Retail trade",
  employment == 23 ~ "Transportation and warehousing",
  employment == 24 ~ "Utilities",
  employment == 25 ~ "Publishing",
  employment == 26 ~ "Motion picture and sound recording",
  employment == 27 ~ "Broadcasting",
  employment == 28 ~ "Internet publishing and broadcasting",
  employment == 29 ~ "Telecommunications",
  employment == 30 ~ "Internet providers and data processing",
  employment == 31 ~ "Other information services",
  employment == 32 ~ "Finance",
  employment == 33 ~ "Insurance",
  employment == 34 ~ "Real estate",
  employment == 35 ~ "Rental and leasing services",
  employment == 36 ~ "Professional and technical services",
  employment == 37 ~ "Management of companies/enterprises",
  employment == 38 ~ "Administration and support",
  employment == 39 ~ "Waste management and remediation",
  employment == 40 ~ "Education",
  employment == 41 ~ "Hospitals",
  employment == 42 ~ "Health care services outside hospitals",
  employment == 43 ~ "Social assistance",
  employment == 44 ~ "Arts/entertainment/recreation",
  employment == 45 ~ "Accommodation",
  employment == 46 ~ "Food services/drinking places",
  employment == 47 ~ "Repair and maintenance",
  employment == 48 ~ "Personal and laundry services",
  employment == 49 ~ "Membership associations/organizations",
  employment == 50 ~ "Private households",
  employment == 51 ~ "Public administration",
  employment == 52 ~ "Armed forces"
))

Industry_of_Employment <- select(i, Ind)

#-----------------------------------------------

j <- stamps %>% mutate(Food = case_when(
  stamps == 1 ~ "Received food stamps in past year",
  stamps == 2 ~ "Did not receive food stamps in past year",
  stamps == -2 ~ "Unsure if received food stamps in past year",
  stamps == -3 ~ "Refused to answer",
  stamps == -9 ~ "No response"
))

Food_Stamps <- select(j, Food)

#-----------------------------------------------

k <- security %>% mutate(FSecurity = case_when(
  security == 1 ~ "High food security",
  security == 2 ~ "Marginal food security",
  security == 3 ~ "Low food security",
  security == 4 ~ "Very low food security",
  security == -9 ~ "No response"
))

Food_Security <- select(k, FSecurity)

#-----------------------------------------------

# Next, putting the readable data in a new tibble to be used for shiny.
data <- tibble(Food_Security, Income, State, Education, Sex, 
               Race, Number_of_Jobs, Citizenship, Disability, 
               Industry_of_Employment, Food_Stamps)

