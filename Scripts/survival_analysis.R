# Parameters and libraries------------------------------------------------------
MOST_RECENT_YEAR = 2023
library('data.table')
library('dplyr')
library('survival')

# Get data and prepare----------------------------------------------------------
years <- seq(MOST_RECENT_YEAR, MOST_RECENT_YEAR-5)
variables <- c('SSUID','PNUM', 'MONTHCODE',        # Essentials
               'SPANEL','SWAVE',
               'EHEARING','ESELFCARE','ESEEING',   # Disabilities
               'EAMBULAT','EERRANDS','ECOGNIT',
               'ESEX','ERACE',                     # Other demographics
               'EDOB_BMONTH','TDOB_BYEAR',
               'EEDGRADE','EEDUC')                 # Enrollment and education

# Concatenate datasets
df <- data.table()
for(i in years){
  path <- paste('Raw_data/pu',i,'.csv',sep='')
  pu <- fread(path, sep = '|', select = variables)%>%
    mutate(pu_year = i)
  df <- bind_rows(df,pu)
}
remove(pu)
names(df)<-toupper(names(df))

# Eligible criteria
# 1. Filter PNUM to 1XX only
#    - [1]XX indicates first wave 
#    - 1[XX] indicates the household themself or the people who live with them 
# 2. Filter to 2018 - 2021 panel groups
# 3. All should be at least 16 years old at the beginning
# 4. All should be 55 years at maximum at any time points
# 5. Precise definition of enrollment persistence will be seen after this.
df<-df%>%
  mutate(PNUM = as.character(PNUM),                       # Correct atomic types
         SPANEL = as.integer(SPANEL),
         TDOB_BYEAR = as.integer(TDOB_BYEAR),
         EDOB_BMONTH = as.integer(EDOB_BMONTH),
         MONTHCODE = as.integer(MONTHCODE),
         person_id = paste0(SSUID,PNUM),                  # Create variable
         age_by_month = ifelse(MONTHCODE >= EDOB_BMONTH,
                               PU_YEAR - TDOB_BYEAR, 
                               PU_YEAR - TDOB_BYEAR - 1))%>%
  filter(grepl('^1\\d{2}',PNUM) & SPANEL <= 2021 &
         age_by_month > 16 & age_by_month < 55)

# Enrollment persistence--------------------------------------------------------
# 1. Enrollment must be between college year 1 and vocational programs
# 2. Respondents may begin enrollment at any points.
# 3. Before enrollment, the respondents do not exist in this world.
# 4. If respondents stop enrolling with change in education and at least
#    associate's degree, treat this as non-event.
# 5. If respondents stop enrolling without those, treat this as event.
# 6. In this world, re-enrollment remains non-event
# 7. Even if respondent has master's and choose to be enrolled, when they stop
#    enrollment before PhD, treat this as event.

persistence<-df%>%
  filter(MONTHCODE == 12)%>%
  mutate(enrolled = ifelse(EEDGRADE %in% 13:20,T,F))%>%
  select(person_id,SWAVE,enrolled,EEDUC)%>%
  arrange(person_id,SWAVE)%>%
  mutate(persist = NA, attained = NA)

for(person in unique(persistence$person_id)){
  # Instantiate per person
  first_enroll <- F
  last
  persist <- NA
  prev_educ <- NA
  current_educ <- NA
  person_idx <- which(persistence$person_id == person)
  
  # Waves per person
  for(i in person_idx){
    current_educ <- persistence$EEDUC[i]          # Current education per wave
    # If first enrolled, freeze current education and set persist
    if(persistence$enrolled[i] & !first_enroll){
      first_enroll <- T
      persist <- T
      prev_educ <- current_educ
    # Skip that row if NA in persist
    }else if(is.na(persist)){
      next
    # If first enroll and stop enrollment, then no longer persist
    }else if(!persist | (!persistence$enrolled[i] & first_enroll)){
      persist <- F
    }
    
    # Current education must be at least associate and greater than frozen 
    # education
    attained <- current_educ > prev_educ && current_educ > 41
    # If attained, non-event forever
    if(attained){
      persistence$persist[i] <- T
    # Either true or false in persist
    }else{
      persistence$persist[i] <- persist
    }
  }
}

# Verify this work:
#persistence%>%
#  arrange(person_id,SWAVE)%>%
#  select(SWAVE,persist,enrolled,EEDUC)%>%
#  unique()%>%View()
#
#persistence%>%
#  tidyr::pivot_wider(
#    names_from = SWAVE,
#    values_from = persist
#  )

# Merge persistence with main data
df<-persistence%>%
  select(person_id,SWAVE,persist)%>%
  right_join(df, by = c('person_id','SWAVE'))%>%
  filter(MONTHCODE == 12 & !is.na(persist))%>%   # Filter to December and no NAs
  mutate(persist_int = as.integer(!persist),     # Binarize them for Cox
         isDeaf = ifelse(EHEARING == 1, 1, 0),
         isWhite = ifelse(ERACE == 1,1,0),
         isOtherDisability = ifelse(EAMBULAT == 1 | ECOGNIT == 1 | 
                                    ESELFCARE == 1 | EERRANDS == 1 | 
                                    ESEEING == 1, 1, 0))

# Find loss of followups and implement Cox regression---------------------------
surv_object <- Surv(time = df$SWAVE, event = df$persist)
km_fit <- survfit(surv_object ~ isDeaf, data=df)
summary(km_fit)

model <- coxph(Surv(SWAVE,persist_int) ~ isDeaf + age_by_month + 
               isOtherDisability + isWhite, data = df)
cox.zph(model)
summary(model)
