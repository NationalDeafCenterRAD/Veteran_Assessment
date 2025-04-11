# Modules
library(dplyr)
library(tidyr)
library(data.table)
library(Hmisc)

# Functions---------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

## Make percentages look neat 
perc_round<-function(x){ # Keep it from becoming 0.00%
  result<-paste0(round(x*100,2),'%')
  
  return(result)
}

## Percentage calculator
percent <- function(df,x,...){
  thestring<-unlist(names(select(df,...)))
  
  numerator<-df%>%
    group_by(...)%>%
    summarise_at(c('PWGTP',toupper(pwgtps)), sum)
  
  unstring<-thestring[thestring != x]
  
  denominator<-df%>%
    group_by(!!! rlang::syms(unstring))%>%
    summarise_at(c('PWGTP',toupper(pwgtps)), sum)
  
  factor <- numerator%>%
    left_join(denominator, by = unstring, suffix = c('','_d'))
  
  # Get percentages in both PWGTP and replications
  factor[!grepl('_d',names(factor)) & grepl('PWGTP',names(factor))]<-
    factor[!grepl('_d',names(factor)) & grepl('PWGTP',names(factor))]/
    factor[grepl('_d',names(factor))]
  
  # Get RSE by percentage
  factor<-bind_cols(select(factor, PWGTP,...),
                    calculate_se(factor,start = grep('\\PWGTP1\\b',names(factor)), 
                                 end = grep('\\PWGTP80\\b',names(factor))))%>%
    mutate(`RSE by percentage` = se/PWGTP)%>%
    select(-se)%>%
    rename(percentage = 'PWGTP')
  
  # Get RSE by population estimator
  numerator<-bind_cols(select(numerator, PWGTP,...),
                       calculate_se(numerator,start = grep('\\PWGTP1\\b',names(numerator)), 
                                    end = grep('\\PWGTP80\\b',names(numerator))))%>%
    mutate(`RSE` = se/PWGTP)%>%
    select(-se)%>%
    rename(`population estimator` = 'PWGTP')
  
  result<-numerator%>%
    inner_join(factor, by = thestring)
  
  # Get numbers of observations
  n_obs<-df%>%
    group_by(!!! rlang::syms(unstring))%>%
    summarise(`Sample size` = n())
  
  # Final, cleaned result
  result<-result%>%
    left_join(n_obs, by = unstring)%>%
    mutate(`Population estimator (%)` = paste0(`population estimator`,' (',
                                               round(percentage*100,1),
                                               '%)'),
           `RSE (RSE for %)` = paste0(perc_round(RSE),' (',
                                      perc_round(`RSE by percentage`),
                                      ')'))%>%
    select(...,`Population estimator (%)`,`RSE (RSE for %)`,`Sample size`)
  
  return(result)
}

## Cumulative percentage calculator
cum_percent <- function(df,...){
  thestring<-unlist(names(select(df,...)))
  
  numerator<-df%>%
    group_by(..., SCHL)%>%
    summarise_at(c('PWGTP',toupper(pwgtps)), sum)
  
  numerator<-numerator%>%
    arrange(..., match(SCHL, 
                       c("phd/dr", "master",
                         "bachelor","associate",
                         "some college","HS diploma", 
                         "no HS diploma")))%>%
    mutate_at(toupper(c("pwgtp",pwgtps)), cumsum)
  
  denominator<-numerator%>%
    filter(SCHL == 'no HS diploma')%>%
    ungroup(SCHL)%>%
    select(-SCHL)
  
  factor <- numerator%>%
    left_join(denominator, by = thestring, suffix = c('','_d'))
  
  # Get percentages in both PWGTP and replications
  factor[!grepl('_d',names(factor)) & grepl('PWGTP',names(factor))]<-
    factor[!grepl('_d',names(factor)) & grepl('PWGTP',names(factor))]/
    factor[grepl('_d',names(factor))]
  
  # Get RSE by percentage
  factor<-bind_cols(select(factor, PWGTP, SCHL, ...),
                    calculate_se(factor,start = grep('\\PWGTP1\\b',names(factor)), 
                                 end = grep('\\PWGTP80\\b',names(factor))))%>%
    mutate(`RSE by percentage` = se/PWGTP)%>%
    select(-se)%>%
    rename(percentage = 'PWGTP')
  
  # Get RSE by population estimator
  numerator<-bind_cols(select(numerator, PWGTP, SCHL,...),
                       calculate_se(numerator,start = grep('\\PWGTP1\\b',names(numerator)), 
                                    end = grep('\\PWGTP80\\b',names(numerator))))%>%
    mutate(`RSE` = se/PWGTP)%>%
    select(-se)%>%
    rename(`population estimator` = 'PWGTP')
  
  result<-numerator%>%
    inner_join(factor, by = c(thestring,'SCHL'), relationship = 'one-to-one')
  
  # Get numbers of observations
  n_obs<-df%>%
    group_by(...)%>%
    summarise(`Sample size` = n())
  
  # Final, cleaned result
  result<-result%>%
    left_join(n_obs, by = thestring)%>%
    mutate(`Population estimator (%)` = paste0(`population estimator`,' (',
                                               round(percentage*100,1),
                                               '%)'),
           `RSE (RSE for %)` = paste0(perc_round(RSE),' (',
                                      perc_round(`RSE by percentage`),
                                      ')'))%>%
    select(..., SCHL, `Population estimator (%)`,`RSE (RSE for %)`,`Sample size`)
  
  return(result)
}

## Average calculator
avg_metric <- function(df,type,metric,...){ # Create average for any metric
  thestring<-unlist(names(select(df,...)))
  
  if(type == 'mean'){
    result<-df%>%
      group_by(...)%>%
      summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted mean
                   list(~ weighted.mean(!!! rlang::syms(metric), .)))
  }else if(type == 'median'){
    result<-df%>%
      group_by(...)%>%
      summarise_at(toupper(c('pwgtp',pwgtps)), # Apply weighted mean
                   list(~ wtd.quantile(!!! rlang::syms(metric), weights = ., probs = 0.5)))
  }else{
    return('Error!')
  }
  
  # Get MOE by percentage
  result<-bind_cols(select(result, PWGTP,...),
                    calculate_regular_me(result,
                                         start = grep('\\PWGTP1\\b',names(result)), 
                                         end = grep('\\PWGTP80\\b',names(result)), 
                                         alpha = 0.05))
  
  # Get numbers of observations
  result<-df%>%
    group_by(...)%>%
    summarise(`Sample size` = n())%>%
    right_join(result, by = thestring)%>%
    rename(`income (in dollar)` = PWGTP)
  
  return(result)
}

# Obtain and clean up data------------------------------------------------------
dir <- paste(getwd(),'Scripts/Statistics.R',sep='/')
source(dir)

# List of PWGTP1-PWGTP80
pwgtps <- c()

for(i in seq(1,80,1)){
  pwgtps[i] <- paste('pwgtp',i,sep='')
}

# Get data from local or File Transfer Protocol of Census Bureau ACS 1Y PUMS
year_n <- 2023
filepath <- paste0(getwd(),'/Raw_Data/',year_n,'_5y_csv_pus.zip')

if(!file.exists(filepath)){
  urlpath<-paste0('https://www2.census.gov/programs-surveys/acs/data/pums/',
                  year_n,'/5-Year/csv_pus.zip')
  download.file(urlpath, destfile = filepath, method = 'curl')
}

unzipped <- tryCatch(unzip(filepath),error = function(e){
  "error"})

# Get data as whole
if(!any(grep('.csv',unzipped))){
  return(data.frame(attribution = NA, status = NA, percentage = NA, 
                    margin_errors = NA, variable = NA, type = NA, state = NA, 
                    year = year_n))
}else{
  unzipped<-unzipped[grep('.csv',unzipped)]
  
  mega_df<-data.frame()
  for(j in 1:length(unzipped)){
    part_df<-fread(unzipped[j])
    names(part_df)<-toupper(names(part_df))
    part_df<-select(part_df, 
                    toupper(c("serialno","state","agep","ddrs","dear","deye",
                              "dout","dphy","dratx","drem","drat","cow",
                              "fdearp","esr","schl","sch","schg","rac1p",
                              "hisp","sex","pernp","pincp","ssip","wkhp",
                              "wkwn","adjinc","pwgtp","drat","mil","nativity",
                              "pobp","qtrbir","vps","waob","mlpa","mlpb","mlpcd",
                              "mlpe","mlpfg","mlph","mlpj","mlpik", 
                              "pwgtp",pwgtps)))%>%
      filter(AGEP > 15 & AGEP < 65)
    mega_df<-bind_rows(mega_df,part_df)
  }
}

hdestfile <- paste(getwd(),'Raw_Data','2023_5y_csv_hus.zip',sep='/')
hunzipped <- unzip(hdestfile)
hvector<-grep('.csv',hunzipped)

hmega_df <- data.frame()
for(i in hvector){
  part_df<-fread(hunzipped[i],
                 select = c('SERIALNO',
                            'LAPTOP','BROADBND','PARTNER','FS',
                            'COMPOTHX','TABLET','FPARC','MULTG',
                            'HUPAC','HUPAOC','ACCESSINET','NRC',
                            'NP','HINCP','HHLANP'
                 )
  )
  hmega_df<-bind_rows(hmega_df,part_df)
}

df1<-mega_df%>%left_join(hmega_df, by = 'SERIALNO')
remove(mega_df,hmega_df)

# Add objects to gitignore list due to larger than 1 GB
# system("find . -size +500M -not -path '*/.*' | sed 's|^\\./||' >> .gitignore")

# Clean up
df1<-df1%>%
  mutate(DEAR = ifelse(DEAR == '1', "deaf", "hearing"))%>%# DEAR recode
  mutate(RAC1P = recode(RAC1P, '1' = 'White',             # RAC1P recode
                               '2' = 'Black',
                               '3' = 'Native American',
                               '6' = 'Asian/Pacific Islander',
                               '7' = 'Asian/Pacific Islander',
                               '8' = 'Other Race/Multiracial',
                               '9' = 'Other Race/Multiracial',
                               .default = 'Native American'))%>%
  mutate(HISP = ifelse(HISP == '1', 'Not Latine','Latine'))%>% # HISP recode
  mutate(SEX = ifelse(SEX == '1','male','female'))%>%   # SEX recode
  mutate(DEYE = recode(DEYE, '1' = 'Blind',             # DEYE recode
                       '2' = 'Sighted'))%>%
  mutate(DDRS = recode(DDRS, '1' = 'Yes',               # DDRS recode
                       '2' = 'No'))%>%
  mutate(DOUT = recode(DOUT, '1' = 'Yes',               # DOUT recode
                       '2' = 'No'))%>%
  mutate(DPHY = recode(DPHY, '1' = 'Yes',               # DPHY recode
                       '2' = 'No'))%>%
  mutate(DREM = recode(DREM, '1' = 'Yes',               # DREM recode
                       '2' = 'No'))%>%
  mutate(ESR = recode(ESR, '1' = 'employed',
                      '3' = 'unemployed',
                      '6' = 'notinLF', 
                      .default = 'employed'))%>%
  mutate(RACETH=ifelse(HISP == 'Latine','Latine',      # Create RACETH
                ifelse(RAC1P == 'Black','Black',
                ifelse(RAC1P == 'Asian/Pacific Islander',
                                'Asian/Pacific Islander',
                ifelse(RAC1P == 'Native American',
                                'Native American',
                ifelse(RAC1P == 'White','White',
                                'Other Race/Multiracial'))))))%>%
  mutate(SCH = ifelse(SCHG > 14, 'Enrolled',
                      'Not Enrolled'))%>%
  mutate(SCHL = ifelse(SCHL < 16,'no HS diploma', 
                ifelse(SCHL <= 17,'HS diploma',
                ifelse(SCHL < 20,'some college',
                ifelse(SCHL == 20,'associate',
                ifelse(SCHL == 21,'bachelor', 
                ifelse(SCHL <= 22,'master',
                ifelse(SCHL > 22, 'phd/dr',NA))))))))%>%
  mutate(COW_original = COW,
         COW = as.character(COW),
         COW = ifelse(is.na(COW),'N/A',COW),
         COW = recode(COW,
                      '1' = 'For-profit',
                      '2' = 'Non-profit',
                      '3' = "Local gov't",
                      '4' = "State gov't",
                      '5' = "Federal gov't",
                      '9' = "Unemployed",
                      'N/A' = 'N/A',
                      .default = 'Self-employed/Business'
         ))%>%
  mutate(isVeteran = ifelse(is.na(VPS),F,T))%>%
  mutate(PLUS = ifelse((DDRS == 'Yes' |     # self-care difficulty
                        DOUT == 'Yes' |   # independent living difficulty
                        DPHY == 'Yes' |   # ambulatory difficulty
                        DREM == 'Yes') &  # cognitive difficulty
                        DEYE == 'Sighted',# deafdisabled only
                       'disabled',
                ifelse((DDRS == 'No' &    # deaf only
                        DOUT == 'No' &
                        DPHY == 'No' &
                        DREM == 'No') & 
                        DEYE == 'Sighted',
                                'no disability',
                ifelse(DEYE == 'Blind',
                               'blind',NA))))%>%
  mutate(DRAT = as.character(DRAT),
         DRAT = ifelse(is.na(DRAT),'never served in military',
                 ifelse(DRAT == '6', 'not reported',
                 ifelse(DRAT == '5', '≥70% severe','<70% severe'))))%>%
  mutate(HINCP = as.numeric(HINCP))%>%
  mutate(PERNP = as.numeric(PERNP))

# Population estimators---------------------------------------------------------
## General
percent(df1,'DEAR',DEAR)%>%
  write.csv(file = 'Assets/Population/general.csv')

## Race
percent(df1,'DEAR',DEAR,RACETH)%>%
  write.csv(file = 'Assets/Population/race.csv')

## Gender
percent(df1,'DEAR',DEAR,SEX)%>%
  write.csv(file = 'Assets/Population/gender.csv')

## Disability
percent(df1,'DEAR',DEAR,PLUS)%>%
  write.csv(file = 'Assets/Population/disability.csv')

## DRAT
percent(df1,'DEAR',DEAR,DRAT)%>%
  write.csv(file = 'Assets/Population/drat.csv')

# Employment--------------------------------------------------------------------
## General
result<-percent(df1,'ESR',DEAR,isVeteran,ESR)
write.csv(result, file = 'Assets/Employment/general.csv')

## Race
result<-percent(df1,'ESR',DEAR,isVeteran,RACETH,ESR)
write.csv(result, file = 'Assets/Employment/race.csv')

## Gender
result<-percent(df1,'ESR',DEAR,isVeteran,SEX,ESR)
write.csv(result, file = 'Assets/Employment/gender.csv')

## Disability
result<-percent(df1,'ESR',DEAR,isVeteran,PLUS,ESR)
write.csv(result, file = 'Assets/Employment/disability.csv')

# Class of Worker---------------------------------------------------------------
## General
result<-percent(df1,'COW',DEAR,isVeteran,COW)
write.csv(result, file = 'Assets/ClassOfWorker/general.csv')

## Race
result<-percent(df1,'COW',DEAR,isVeteran,RACETH,COW)
write.csv(result, file = 'Assets/ClassOfWorker/race.csv')

## Gender
result<-percent(df1,'COW',DEAR,isVeteran,SEX,COW)
write.csv(result, file = 'Assets/ClassOfWorker/gender.csv')

## Disability
result<-percent(df1,'COW',DEAR,isVeteran,PLUS,COW)
write.csv(result, file = 'Assets/ClassOfWorker/disability.csv')

# Type of Employment------------------------------------------------------------
employed<-df1%>%
  filter(ESR == 'employed')
employed<-employed%>% # self employee and bizowner
  mutate(selfEmp = ifelse(COW_original %in% c(6,7), 'Y', 'N'))%>%
  mutate(bizOwner = ifelse(COW_original == 7, 'Y','N'))%>%
  mutate(fullTime = ifelse(WKWN %in% c(50,51,52) & WKHP >= 35,'yes','any'))

### Self-Employed
## General
result<-percent(employed,'selfEmp',DEAR,isVeteran,selfEmp)
write.csv(result, file = 'Assets/TypeOfEmployment/SelfEmployed/general.csv')

## Race
result<-percent(employed,'selfEmp',DEAR,isVeteran,RACETH,selfEmp)
write.csv(result, file = 'Assets/TypeOfEmployment/SelfEmployed/race.csv')

## Gender
result<-percent(employed,'selfEmp',DEAR,isVeteran,SEX,selfEmp)
write.csv(result, file = 'Assets/TypeOfEmployment/SelfEmployed/gender.csv')

## Disability
result<-percent(employed,'selfEmp',DEAR,isVeteran,PLUS,selfEmp)
write.csv(result, file = 'Assets/TypeOfEmployment/SelfEmployed/disability.csv')

### Business owners
## General
result<-percent(employed,'bizOwner',DEAR,isVeteran,bizOwner)
write.csv(result, file = 'Assets/TypeOfEmployment/BusinessOwners/general.csv')

## Race
result<-percent(employed,'bizOwner',DEAR,isVeteran,RACETH,bizOwner)
write.csv(result, file = 'Assets/TypeOfEmployment/BusinessOwners/race.csv')

## Gender
result<-percent(employed,'bizOwner',DEAR,isVeteran,SEX,bizOwner)
write.csv(result, file = 'Assets/TypeOfEmployment/BusinessOwners/gender.csv')

## Disability
result<-percent(employed,'bizOwner',DEAR,isVeteran,PLUS,bizOwner)
write.csv(result, file = 'Assets/TypeOfEmployment/BusinessOwners/disability.csv')

### Full-Time Workers
## General
result<-percent(employed,'fullTime',DEAR,isVeteran,fullTime)
write.csv(result, file = 'Assets/TypeOfEmployment/FullTimeWorkers/general.csv')

## Race
result<-percent(employed,'fullTime',DEAR,isVeteran,RACETH,fullTime)
write.csv(result, file = 'Assets/TypeOfEmployment/FullTimeWorkers/race.csv')

## Gender
result<-percent(employed,'fullTime',DEAR,isVeteran,SEX,fullTime)
write.csv(result, file = 'Assets/TypeOfEmployment/FullTimeWorkers/gender.csv')

## Disability
result<-percent(employed,'fullTime',DEAR,isVeteran,PLUS,fullTime)
write.csv(result, file = 'Assets/TypeOfEmployment/FullTimeWorkers/disability.csv')

# Income------------------------------------------------------------------------
### Individual Income
forresult<-employed%>%
  filter(fullTime == 'yes')

##### Median
## General
result<-avg_metric(forresult,'median','PERNP',DEAR,isVeteran)
write.csv(result,'Assets/Median_Income/Individual/general.csv')

## Race
result<-avg_metric(forresult,'median','PERNP',DEAR,isVeteran,RACETH)
write.csv(result,'Assets/Median_Income/Individual/race.csv')

## Gender
result<-avg_metric(forresult,'median','PERNP',DEAR,isVeteran,SEX)
write.csv(result,'Assets/Median_Income/Individual/gender.csv')

## Disability
result<-avg_metric(forresult,'median','PERNP',DEAR,isVeteran,PLUS)
write.csv(result,'Assets/Median_Income/Individual/disability.csv')

##### Mean
## General
result<-avg_metric(forresult,'mean','PERNP',DEAR,isVeteran)
write.csv(result,'Assets/Mean_Income/Individual/general.csv')

## Race
result<-avg_metric(forresult,'mean','PERNP',DEAR,isVeteran,RACETH)
write.csv(result,'Assets/Mean_Income/Individual/race.csv')

## Gender
result<-avg_metric(forresult,'mean','PERNP',DEAR,isVeteran,SEX)
write.csv(result,'Assets/Mean_Income/Individual/gender.csv')

## Disability
result<-avg_metric(forresult,'mean','PERNP',DEAR,isVeteran,PLUS)
write.csv(result,'Assets/Mean_Income/Individual/disability.csv')

### Household Income
##### Median
## General
result<-avg_metric(df1,'median','HINCP',DEAR,isVeteran)
write.csv(result,'Assets/Median_Income/Household/general.csv')

## Race
result<-avg_metric(forresult,'median','HINCP',DEAR,isVeteran,RACETH)
write.csv(result,'Assets/Median_Income/Household/race.csv')

## Gender
result<-avg_metric(forresult,'median','HINCP',DEAR,isVeteran,SEX)
write.csv(result,'Assets/Median_Income/Household/gender.csv')

## Disability
result<-avg_metric(forresult,'median','HINCP',DEAR,isVeteran,PLUS)
write.csv(result,'Assets/Median_Income/Household/disability.csv')

##### Mean
## General
result<-avg_metric(forresult,'mean','HINCP',DEAR,isVeteran)
write.csv(result,'Assets/Mean_Income/Household/general.csv')

## Race
result<-avg_metric(forresult,'mean','HINCP',DEAR,isVeteran,RACETH)
write.csv(result,'Assets/Mean_Income/Household/race.csv')

## Gender
result<-avg_metric(forresult,'mean','HINCP',DEAR,isVeteran,SEX)
write.csv(result,'Assets/Mean_Income/Household/gender.csv')

## Disability
result<-avg_metric(forresult,'mean','HINCP',DEAR,isVeteran,PLUS)
write.csv(result,'Assets/Mean_Income/Household/disability.csv')


# Education Attainment----------------------------------------------------------
education<-df1%>%
  filter(AGEP > 24)

## General
cum_percent(education,DEAR,isVeteran)%>%
  write.csv('Assets/Education/general.csv')

## Race
cum_percent(education,DEAR,isVeteran,RACETH)%>%
  write.csv('Assets/Education/race.csv')

## Gender
cum_percent(education,DEAR,isVeteran,SEX)%>%
  write.csv('Assets/Education/gender.csv')

## General
cum_percent(education,DEAR,isVeteran,PLUS)%>%
  write.csv('Assets/Education/disability.csv')


# Enrollment Rate---------------------------------------------------------------
## General
percent(df1,'SCH',DEAR,isVeteran,SCH)%>%
  write.csv(file = 'Assets/Enrollment/general.csv')

## Race
percent(df1,'SCH',DEAR,isVeteran,RACETH,SCH)%>%
  write.csv(file = 'Assets/Enrollment/race.csv')

## Gender
percent(df1,'SCH',DEAR,isVeteran,SEX,SCH)%>%
  write.csv(file = 'Assets/Enrollment/gender.csv')

## Disability
percent(df1,'SCH',DEAR,isVeteran,PLUS,SCH)%>%
  write.csv(file = 'Assets/Enrollment/disability.csv')
