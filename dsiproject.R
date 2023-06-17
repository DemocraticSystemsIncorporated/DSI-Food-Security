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
data <- read_csv("./dec19pub.csv") 
# target_variables <- select(dsi, HRFS12MD, HEFAMINC, HRNUMHOU, PEEDUCA, PESEX, PTDTRACE, PEMJNUM, PRCITSHP, PUDIS, PRDTIND2, HETSP3O, HESP1, PEMJOT, GCFIP)
# organized_by_security <- arrange(target_variables, desc(HRFS12MD), HEFAMINC, HRNUMHOU, PEEDUCA, PESEX, PTDTRACE, PEMJNUM, PRCITSHP, PUDIS, PRDTIND2, HETSP3O, HESP1, PEMJOT, GCFIP)


# Singling out each variable.

# preincome <- select(target_variables, HEFAMINC)
# people <- select(target_variables, HRNUMHOU)
# prestate <- select(target_variables, GCFIP)
# preeducation <- select(target_variables, PEEDUCA)
# presex <- select(target_variables, PESEX)
# prerace <- select(target_variables, PTDTRACE)
# job1 <- select(target_variables, PEMJOT)
# job2 <- select(target_variables, PEMJNUM)
# precitizenship <- select(target_variables, PRCITSHP)
# predisability <- select(target_variables, PUDIS)
# employment <- select(target_variables, PRDTIND2)
# stamps <- select(target_variables, HESP1)
# security <- select(target_variables, HRFS12MD)

# Mutating each variable to get a readable representation of the codes recorded.
# Divisions for ease of viewing (it gets chunky).

#------------------------------------------------

data <- data |> mutate(inc = case_when(
  HEFAMINC == 1 ~ "Less than $5,000",
  HEFAMINC == 2 ~ "$5,000 - $7,499",
  HEFAMINC == 3 ~ "$7,500 - $9,999",
  HEFAMINC == 4 ~ "$10,000 - $12,499",
  HEFAMINC == 5 ~ "$12,500 - $14,999",
  HEFAMINC == 6 ~ "$15,000 - $19,999",
  HEFAMINC == 7 ~ "$20,000 - $24,999",
  HEFAMINC == 8 ~ "$25,000 - $29,999",
  HEFAMINC == 9 ~ "$30,000 - $34,999",
  HEFAMINC == 10 ~ "$35,000 - $39,999",
  HEFAMINC == 11 ~ "$40,000 - $49,999",
  HEFAMINC == 12 ~ "$50,000 - $59,999",
  HEFAMINC == 13 ~ "$60,000 - $74,999",
  HEFAMINC == 14 ~ "$75,000 - $99,999",
  HEFAMINC == 15 ~ "$100,000 - $149,999",
  HEFAMINC == 16 ~ "$150,000 or more"
))

# a <- preincome %>% mutate(inc = case_when(
#   preincome == 1 ~ "Less than $5,000",
#   preincome == 2 ~ "$5,000 - $7,499",
#   preincome == 3 ~ "$7,500 - $9,999",
#   preincome == 4 ~ "$10,000 - $12,499",
#   preincome == 5 ~ "$12,500 - $14,999",
#   preincome == 6 ~ "$15,000 - $19,999",
#   preincome == 7 ~ "$20,000 - $24,999",
#   preincome == 8 ~ "$25,000 - $29,999",
#   preincome == 9 ~ "$30,000 - $34,999",
#   preincome == 10 ~ "$35,000 - $39,999",
#   preincome == 11 ~ "$40,000 - $49,999",
#   preincome == 12 ~ "$50,000 - $59,999",
#   preincome == 13 ~ "$60,000 - $74,999",
#   preincome == 14 ~ "$75,000 - $99,999",
#   preincome == 15 ~ "$100,000 - $149,999",
#   preincome == 16 ~ "$150,000 or more"
# ))
# 
# # Take out readable representation to be put into a new tibble
# Income <- select(a, inc)

#-----------------------------------------------

#Don't think there's any reason to transmute people as it's literally just a count.

#-----------------------------------------------

data <- data |> mutate(states = case_when(
  GCFIP == "01" ~ "AL",
  GCFIP == "02" ~ "AK",
  GCFIP == "04" ~ "AZ",
  GCFIP == "05" ~ "AR",
  GCFIP == "06" ~ "CA",
  GCFIP == "08" ~ "CO",
  GCFIP == "09" ~ "CT",
  GCFIP == 10 ~ "DE",
  GCFIP == 11 ~ "DC",
  GCFIP == 12 ~ "FL",
  GCFIP == 13 ~ "GA",
  GCFIP == 15 ~ "HI",
  GCFIP == 16 ~ "ID",
  GCFIP == 17 ~ "IL",
  GCFIP == 18 ~ "IN",
  GCFIP == 19 ~ "IA",
  GCFIP == 20 ~ "KS",
  GCFIP == 21 ~ "KY",
  GCFIP == 22 ~ "LA",
  GCFIP == 23 ~ "ME",
  GCFIP == 24 ~ "MD",
  GCFIP == 25 ~ "MA",
  GCFIP == 26 ~ "MI",
  GCFIP == 27 ~ "MN",
  GCFIP == 28 ~ "MS",
  GCFIP == 29 ~ "MO",
  GCFIP == 30 ~ "MT",
  GCFIP == 31 ~ "NE",
  GCFIP == 32 ~ "NV",
  GCFIP == 33 ~ "NH",
  GCFIP == 34 ~ "NJ",
  GCFIP == 35 ~ "NM",
  GCFIP == 36 ~ "NY",
  GCFIP == 37 ~ "NC",
  GCFIP == 38 ~ "ND",
  GCFIP == 39 ~ "OH",
  GCFIP == 40 ~ "OK",
  GCFIP == 41 ~ "OR",
  GCFIP == 42 ~ "PA",
  GCFIP == 44 ~ "RI",
  GCFIP == 45 ~ "SC",
  GCFIP == 46 ~ "SD",
  GCFIP == 47 ~ "TN",
  GCFIP == 48 ~ "TX",
  GCFIP == 49 ~ "UT",
  GCFIP == 50 ~ "VT",
  GCFIP == 51 ~ "VA",
  GCFIP == 53 ~ "WA",
  GCFIP == 54 ~ "WV",
  GCFIP == 55 ~ "WI",
  GCFIP == 56 ~ "WY"
))

# b <- prestate %>% mutate(states = case_when(
  #because these are getting stored as characters, single digits need to be expressed
  #as double digit characters for state data AL-CT to be read in
#   prestate == "01" ~ "AL",
#   prestate == "02" ~ "AK",
#   prestate == "04" ~ "AZ",
#   prestate == "05" ~ "AR",
#   prestate == "06" ~ "CA",
#   prestate == "08" ~ "CO",
#   prestate == "09" ~ "CT",
#   prestate == 10 ~ "DE",
#   prestate == 11 ~ "DC",
#   prestate == 12 ~ "FL",
#   prestate == 13 ~ "GA",
#   prestate == 15 ~ "HI",
#   prestate == 16 ~ "ID",
#   prestate == 17 ~ "IL",
#   prestate == 18 ~ "IN",
#   prestate == 19 ~ "IA",
#   prestate == 20 ~ "KS",
#   prestate == 21 ~ "KY",
#   prestate == 22 ~ "LA",
#   prestate == 23 ~ "ME",
#   prestate == 24 ~ "MD",
#   prestate == 25 ~ "MA",
#   prestate == 26 ~ "MI",
#   prestate == 27 ~ "MN",
#   prestate == 28 ~ "MS",
#   prestate == 29 ~ "MO",
#   prestate == 30 ~ "MT",
#   prestate == 31 ~ "NE",
#   prestate == 32 ~ "NV",
#   prestate == 33 ~ "NH",
#   prestate == 34 ~ "NJ",
#   prestate == 35 ~ "NM",
#   prestate == 36 ~ "NY",
#   prestate == 37 ~ "NC",
#   prestate == 38 ~ "ND",
#   prestate == 39 ~ "OH",
#   prestate == 40 ~ "OK",
#   prestate == 41 ~ "OR",
#   prestate == 42 ~ "PA",
#   prestate == 44 ~ "RI",
#   prestate == 45 ~ "SC",
#   prestate == 46 ~ "SD",
#   prestate == 47 ~ "TN",
#   prestate == 48 ~ "TX",
#   prestate == 49 ~ "UT",
#   prestate == 50 ~ "VT",
#   prestate == 51 ~ "VA",
#   prestate == 53 ~ "WA",
#   prestate == 54 ~ "WV",
#   prestate == 55 ~ "WI",
#   prestate == 56 ~ "WY"
# ))
# 
# State <- select(b, states)

#-----------------------------------------------

data <- data |> mutate(edu = case_when(
  PEEDUCA == 31 ~ "Less than 1st grade",
  PEEDUCA == 32 ~ "1st, 2nd, 3rd, or 4th grade",
  PEEDUCA == 33 ~ "5th or 6th grade",
  PEEDUCA == 34 ~ "7th or 8th grade",
  PEEDUCA == 35 ~ "9th grade",
  PEEDUCA == 36 ~ "10th grade",
  PEEDUCA == 37 ~ "11th grade",
  PEEDUCA == 38 ~ "12th grade, no diploma",
  PEEDUCA == 39 ~ "High school graduate",
  PEEDUCA == 40 ~ "College, no diploma",
  PEEDUCA == 41 ~ "Occupational/Vocational associate degree",
  PEEDUCA == 42 ~ "Academic program associate degree",
  PEEDUCA == 43 ~ "Bachelor's degree",
  PEEDUCA == 44 ~ "Master's degree",
  PEEDUCA == 45 ~ "Professional school degree",
  PEEDUCA == 46 ~ "Doctorate degree"
))
  
# c <- preeducation %>% mutate(edu = case_when(
#   preeducation == 31 ~ "Less than 1st grade",
#   preeducation == 32 ~ "1st, 2nd, 3rd, or 4th grade",
#   preeducation == 33 ~ "5th or 6th grade",
#   preeducation == 34 ~ "7th or 8th grade",
#   preeducation == 35 ~ "9th grade",
#   preeducation == 36 ~ "10th grade",
#   preeducation == 37 ~ "11th grade",
#   preeducation == 38 ~ "12th grade, no diploma",
#   preeducation == 39 ~ "High school graduate",
#   preeducation == 40 ~ "College, no diploma",
#   preeducation == 41 ~ "Occupational/Vocational associate degree",
#   preeducation == 42 ~ "Academic program associate degree",
#   preeducation == 43 ~ "Bachelor's degree",
#   preeducation == 44 ~ "Master's degree",
#   preeducation == 45 ~ "Professional school degree",
#   preeducation == 46 ~ "Doctorate degree"
# ))
# 
# Education <- select(c, edu)

#-----------------------------------------------

data <- data |> mutate(sexes = case_when(
  PESEX == 1 ~ "Male",
  PESEX == 2 ~ "Female"
))
  
# d <- presex %>% mutate(sexes = case_when(
#   presex == 1 ~ "Male",
#   presex == 2 ~ "Female"
# ))
# 
# Sex <- select(d, sexes)

#-----------------------------------------------

data <- data |> mutate(races = case_when(
  PTDTRACE == 1 ~ "White",
  PTDTRACE == 2 ~ "Black",
  PTDTRACE == 3 ~ "American Indian/Alaskan Native",
  PTDTRACE == 4 ~ "Asian",
  PTDTRACE == 5 ~ "Hawaiian/Pacific Islander",
  PTDTRACE == 6 ~ "White-Black",
  PTDTRACE == 7 ~ "White-American Indian/Alaskan Native",
  PTDTRACE == 8 ~ "White-Asian",
  PTDTRACE == 9 ~ "White-Hawaiian/Pacific Islander",
  PTDTRACE == 10 ~ "Black-American Indian/Alaskan Native",
  PTDTRACE == 11 ~ "Black-Asian",
  PTDTRACE == 12 ~ "Black-Hawaiian/Pacific Islander",
  PTDTRACE == 13 ~ "American Indian/Alaskan Native-Asian",
  PTDTRACE == 14 ~ "American Indian/Alaskan Native-Hawaiian/Pacific Islander",
  PTDTRACE == 15 ~ "Asian-Hawaiian/Pacific Islander",
  PTDTRACE == 16 ~ "White-Black-American Indian/Alaskan Native",
  PTDTRACE == 17 ~ "White-Black-Asian",
  PTDTRACE == 18 ~ "White-Black-Hawaiian/Pacific Islander",
  PTDTRACE == 19 ~ "White-American Indian/Alaskan Native-Asian",
  PTDTRACE == 20 ~ "White-American Indian/Alaskan Native-Hawaiian/Pacific Islander",
  PTDTRACE == 21 ~ "White-Asian-Hawaiian/Pacific Islander",
  PTDTRACE == 22 ~ "Black-American Indian/Alaskan Native-Asian",
  PTDTRACE == 23 ~ "White-Black-American Indian/Alaskan Native-Asian",
  PTDTRACE == 24 ~ "White-American Indian/Alaskan Native-Asian-Hawaiian/Pacific Islander",
  PTDTRACE == 25 ~ "Other 3 race combinations",
  PTDTRACE == 26 ~ "Other 4 or 5 race combinations"
))
  
# e <- prerace %>% mutate(races = case_when(
#   prerace == 1 ~ "White",
#   prerace == 2 ~ "Black",
#   prerace == 3 ~ "American Indian/Alaskan Native",
#   prerace == 4 ~ "Asian",
#   prerace == 5 ~ "Hawaiian/Pacific Islander",
#   prerace == 6 ~ "White-Black",
#   prerace == 7 ~ "White-American Indian/Alaskan Native",
#   prerace == 8 ~ "White-Asian",
#   prerace == 9 ~ "White-Hawaiian/Pacific Islander",
#   prerace == 10 ~ "Black-American Indian/Alaskan Native",
#   prerace == 11 ~ "Black-Asian",
#   prerace == 12 ~ "Black-Hawaiian/Pacific Islander",
#   prerace == 13 ~ "American Indian/Alaskan Native-Asian",
#   prerace == 14 ~ "American Indian/Alaskan Native-Hawaiian/Pacific Islander",
#   prerace == 15 ~ "Asian-Hawaiian/Pacific Islander",
#   prerace == 16 ~ "White-Black-American Indian/Alaskan Native",
#   prerace == 17 ~ "White-Black-Asian",
#   prerace == 18 ~ "White-Black-Hawaiian/Pacific Islander",
#   prerace == 19 ~ "White-American Indian/Alaskan Native-Asian",
#   prerace == 20 ~ "White-American Indian/Alaskan Native-Hawaiian/Pacific Islander",
#   prerace == 21 ~ "White-Asian-Hawaiian/Pacific Islander",
#   prerace == 22 ~ "Black-American Indian/Alaskan Native-Asian",
#   prerace == 23 ~ "White-Black-American Indian/Alaskan Native-Asian",
#   prerace == 24 ~ "White-American Indian/Alaskan Native-Asian-Hawaiian/Pacific Islander",
#   prerace == 25 ~ "Other 3 race combinations",
#   prerace == 26 ~ "Other 4 or 5 race combinations"
# ))
# 
# Race <- select(e, races)

#-----------------------------------------------

data <- data |> mutate(jobs = case_when(
  PEMJOT == 1 ~ "More than one job",
  PEMJOT == 2 ~ "One job"
))

# f <- job1 %>% mutate(jobs = case_when(
#   job1 == 1 ~ "More than one job",
#   job1 == 2 ~ "One job"
# ))
# 
# Number_of_Jobs <- select(f, jobs)

#-----------------------------------------------

data <- data |> mutate(Cit = case_when(
  PRCITSHP == 1 ~ "Native, born in US",
  PRCITSHP == 2 ~ "Native, born in Puerto Rico or other island territories",
  PRCITSHP == 3 ~ "Native, born abroad to American parents/parent",
  PRCITSHP == 4 ~ "Foreign born, citizen by naturalization",
  PRCITSHP == 5 ~ "Foreign born, not a citizen"
))

# g <- precitizenship %>% mutate(Cit = case_when(
#   precitizenship == 1 ~ "Native, born in US",
#   precitizenship == 2 ~ "Native, born in Puerto Rico or other island territories",
#   precitizenship == 3 ~ "Native, born abroad to American parents/parent",
#   precitizenship == 4 ~ "Foreign born, citizen by naturalization",
#   precitizenship == 5 ~ "Foreign born, not a citizen"
# ))
# 
# Citizenship <- select(g, Cit)

#-----------------------------------------------

data <- data |> mutate(Dis = case_when(
  PUDIS == 1 ~ "Disability prevents work",
  PUDIS == 2 ~ "Disability does not prevent work",
  PUDIS == 3 ~ "No disability"
))

# h <- predisability %>% mutate(Dis = case_when(
#   predisability == 1 ~ "Disability prevents work",
#   predisability == 2 ~ "Disability does not prevent work",
#   predisability == 3 ~ "No disability"
# ))
# 
# Disability <- select(h, Dis)

#-----------------------------------------------

data <- data |> mutate(Ind = case_when(
  PRDTIND2 == 1 ~ "Agriculture",
  PRDTIND2 == 2 ~ "Forestry/logging/fishing/hunting/trapping",
  PRDTIND2 == 3 ~ "Mining",
  PRDTIND2 == 4 ~ "Construction",
  PRDTIND2 == 5 ~ "Nonmetallic mineral production/manufacturing",
  PRDTIND2 == 6 ~ "Metals and fabricated metal products",
  PRDTIND2 == 7 ~ "Machinery manufacturing",
  PRDTIND2 == 8 ~ "Computer/electronic manufacturing",
  PRDTIND2 == 9 ~ "Electrical equipment/appliance manufacturing",
  PRDTIND2 == 10 ~ "Transportation equipment manufacturing",
  PRDTIND2 == 11 ~ "Wood products",
  PRDTIND2 == 12 ~ "Furniture and fixtures manufacturing",
  PRDTIND2 == 13 ~ "Miscellaneous manufacturing",
  PRDTIND2 == 14 ~ "Food manufacturing",
  PRDTIND2 == 15 ~ "Beverages and tobacco products",
  PRDTIND2 == 16 ~ "Textile/apparel/leather manufacturing",
  PRDTIND2 == 17 ~ "Paper and printing",
  PRDTIND2 == 18 ~ "Petroleum and coal products manufacturing",
  PRDTIND2 == 19 ~ "Chemical manufacturing",
  PRDTIND2 == 20 ~ "Plastics and rubber",
  PRDTIND2 == 21 ~ "Wholesale trade",
  PRDTIND2 == 22 ~ "Retail trade",
  PRDTIND2 == 23 ~ "Transportation and warehousing",
  PRDTIND2 == 24 ~ "Utilities",
  PRDTIND2 == 25 ~ "Publishing",
  PRDTIND2 == 26 ~ "Motion picture and sound recording",
  PRDTIND2 == 27 ~ "Broadcasting",
  PRDTIND2 == 28 ~ "Internet publishing and broadcasting",
  PRDTIND2 == 29 ~ "Telecommunications",
  PRDTIND2 == 30 ~ "Internet providers and data processing",
  PRDTIND2 == 31 ~ "Other information services",
  PRDTIND2 == 32 ~ "Finance",
  PRDTIND2 == 33 ~ "Insurance",
  PRDTIND2 == 34 ~ "Real estate",
  PRDTIND2 == 35 ~ "Rental and leasing services",
  PRDTIND2 == 36 ~ "Professional and technical services",
  PRDTIND2 == 37 ~ "Management of companies/enterprises",
  PRDTIND2 == 38 ~ "Administration and support",
  PRDTIND2 == 39 ~ "Waste management and remediation",
  PRDTIND2 == 40 ~ "Education",
  PRDTIND2 == 41 ~ "Hospitals",
  PRDTIND2 == 42 ~ "Health care services outside hospitals",
  PRDTIND2 == 43 ~ "Social assistance",
  PRDTIND2 == 44 ~ "Arts/entertainment/recreation",
  PRDTIND2 == 45 ~ "Accommodation",
  PRDTIND2 == 46 ~ "Food services/drinking places",
  PRDTIND2 == 47 ~ "Repair and maintenance",
  PRDTIND2 == 48 ~ "Personal and laundry services",
  PRDTIND2 == 49 ~ "Membership associations/organizations",
  PRDTIND2 == 50 ~ "Private households",
  PRDTIND2 == 51 ~ "Public administration",
  PRDTIND2 == 52 ~ "Armed forces"
))

# i <- employment %>% mutate(Ind = case_when(
#   employment == 1 ~ "Agriculture",
#   employment == 2 ~ "Forestry/logging/fishing/hunting/trapping",
#   employment == 3 ~ "Mining",
#   employment == 4 ~ "Construction",
#   employment == 5 ~ "Nonmetallic mineral production/manufacturing",
#   employment == 6 ~ "Metals and fabricated metal products",
#   employment == 7 ~ "Machinery manufacturing",
#   employment == 8 ~ "Computer/electronic manufacturing",
#   employment == 9 ~ "Electrical equipment/appliance manufacturing",
#   employment == 10 ~ "Transportation equipment manufacturing",
#   employment == 11 ~ "Wood products",
#   employment == 12 ~ "Furniture and fixtures manufacturing",
#   employment == 13 ~ "Miscellaneous manufacturing",
#   employment == 14 ~ "Food manufacturing",
#   employment == 15 ~ "Beverages and tobacco products",
#   employment == 16 ~ "Textile/apparel/leather manufacturing",
#   employment == 17 ~ "Paper and printing",
#   employment == 18 ~ "Petroleum and coal products manufacturing",
#   employment == 19 ~ "Chemical manufacturing",
#   employment == 20 ~ "Plastics and rubber",
#   employment == 21 ~ "Wholesale trade",
#   employment == 22 ~ "Retail trade",
#   employment == 23 ~ "Transportation and warehousing",
#   employment == 24 ~ "Utilities",
#   employment == 25 ~ "Publishing",
#   employment == 26 ~ "Motion picture and sound recording",
#   employment == 27 ~ "Broadcasting",
#   employment == 28 ~ "Internet publishing and broadcasting",
#   employment == 29 ~ "Telecommunications",
#   employment == 30 ~ "Internet providers and data processing",
#   employment == 31 ~ "Other information services",
#   employment == 32 ~ "Finance",
#   employment == 33 ~ "Insurance",
#   employment == 34 ~ "Real estate",
#   employment == 35 ~ "Rental and leasing services",
#   employment == 36 ~ "Professional and technical services",
#   employment == 37 ~ "Management of companies/enterprises",
#   employment == 38 ~ "Administration and support",
#   employment == 39 ~ "Waste management and remediation",
#   employment == 40 ~ "Education",
#   employment == 41 ~ "Hospitals",
#   employment == 42 ~ "Health care services outside hospitals",
#   employment == 43 ~ "Social assistance",
#   employment == 44 ~ "Arts/entertainment/recreation",
#   employment == 45 ~ "Accommodation",
#   employment == 46 ~ "Food services/drinking places",
#   employment == 47 ~ "Repair and maintenance",
#   employment == 48 ~ "Personal and laundry services",
#   employment == 49 ~ "Membership associations/organizations",
#   employment == 50 ~ "Private households",
#   employment == 51 ~ "Public administration",
#   employment == 52 ~ "Armed forces"
# ))
# 
# Industry_of_Employment <- select(i, Ind)

#-----------------------------------------------

data <- data |> mutate(Food = case_when(
  HESP1 == 1 ~ "Received food stamps in past year",
  HESP1 == 2 ~ "Did not receive food stamps in past year",
  HESP1 == -2 ~ "Unsure if received food stamps in past year",
  HESP1 == -3 ~ "Refused to answer",
  HESP1 == -9 ~ "No response"
))

# j <- stamps %>% mutate(Food = case_when(
#   stamps == 1 ~ "Received food stamps in past year",
#   stamps == 2 ~ "Did not receive food stamps in past year",
#   stamps == -2 ~ "Unsure if received food stamps in past year",
#   stamps == -3 ~ "Refused to answer",
#   stamps == -9 ~ "No response"
# ))
# 
# Food_Stamps <- select(j, Food)

#-----------------------------------------------

data <- data |> mutate(FSecurity = case_when(
  HRFS12MD == 1 ~ "High food security",
  HRFS12MD == 2 ~ "Marginal food security",
  HRFS12MD == 3 ~ "Low food security",
  HRFS12MD == 4 ~ "Very low food security",
  HRFS12MD == -9 ~ "No response"
))
  
# k <- security %>% mutate(FSecurity = case_when(
#   security == 1 ~ "High food security",
#   security == 2 ~ "Marginal food security",
#   security == 3 ~ "Low food security",
#   security == 4 ~ "Very low food security",
#   security == -9 ~ "No response"
# ))
# 
# Food_Security <- select(k, FSecurity)

#-----------------------------------------------

# Next, putting the readable data in a new tibble to be used for shiny.
data <- select(data, inc, states, edu, sexes, races, jobs, Cit, Dis, Ind, Food, FSecurity)
# data <- tibble(Food_Security, Income, State, Education, Sex,
#                Race, Number_of_Jobs, Citizenship, Disability, 
#                Industry_of_Employment, Food_Stamps)


