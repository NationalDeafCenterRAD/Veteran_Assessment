# Prepare-----------------------------------------------------------------------
# Parameters
MOST_RECENT_YEAR = 2023

# Modules
require("data.table")
require("lubridate")
require("bit64")
require("dplyr")
require("tidyr")
require("networkD3")
require("survival")
require("survminer")
require("powerSurvEpi")
#require("ggplot2")
#require("plotly")

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

# Prepare data------------------------------------------------------------------
years <- seq(MOST_RECENT_YEAR,MOST_RECENT_YEAR-5)
variables <- c('SSUID','PNUM','MONTHCODE','ERESIDENCEID','ERELRPE',
               'SPANEL','SWAVE','WPFINWGT','THHLDSTATUS', # <- use to filter for disabilities
               # Disabilities
               'EHEARING','ESELFCARE','ESEEING','EAMBULAT','EERRANDS','ECOGNIT',
               # Ascribed Attributes
               'TDOB_BYEAR','EDOB_BMONTH','ESEX','EHISPAN','ERACE','TRACE','EBORNUS',
               # Education and employment
               'EEDUC','RMESR','EEDENROLL','EEDGRADE','EEDBMONTH','EEDEMONTH','TMWKHRS','TPTOTINC',
               # Military Service
               'TAF8','EAF1','EAF2','EAF3','EAF4','EAF5','EAF6','EAF7','EAF8','EAF9',
               'EAFNOW',
               # Veteran-connected disability
               'TVADISRATE','EVATYP1YN')

df <- data.table()
for(i in years){
  path <- paste('Raw_data/pu',i,'.csv',sep='')
  pu <- fread(path, sep = '|', select = variables)%>%
    mutate(pu_year = i)
  df <- bind_rows(df,pu)
}
remove(pu)
names(df) <- toupper(names(df))

# 1. Filter PNUM to 1XX only
#    - [1]XX indicates first wave 
#    - 1[XX] indicates the household themself or the people who live with them 
# 2. Filter to 2021 PANEL
# 3. All should be at least 16 years old at the beginning
# 4. All should be 64 years at maximum at 2023 in any time period
# 5. All should returning HHLD or new HHLD member
df<-df%>%
  mutate(PNUM = as.character(PNUM),
         SPANEL = as.integer(SPANEL),
         THHLDSTATUS = as.integer(THHLDSTATUS))%>%
  filter(grepl('^1\\d{2}',PNUM) & SPANEL <= 2021 & THHLDSTATUS %in% 1:2)%>%
  mutate(TDOB_BYEAR = as.integer(TDOB_BYEAR),
         EDOB_BMONTH = as.integer(EDOB_BMONTH),
         MONTHCODE = as.integer(MONTHCODE),
         age_by_month = ifelse(MONTHCODE >= EDOB_BMONTH, PU_YEAR - TDOB_BYEAR, 
                               PU_YEAR - TDOB_BYEAR - 1))%>%
  filter(age_by_month > 16 & age_by_month < 55)

# Clean up data
df<-df%>%
  mutate(auditory = ifelse(as.integer(EHEARING) == 1, 'deaf','hearing'),
         isLatinx = ifelse(is.na(EHISPAN),F,T),
         TRACE = as.integer(TRACE),
         raceth = case_when(
           isLatinx ~ 'Latine',
           TRACE == 1 ~ 'white',
           TRACE == 2 ~ 'Black',
           TRACE == 3 ~ 'Native American',
           TRACE %in% 4:5 ~ 'Asian',
           is.na(TRACE) ~ 'multiracial',
           TRUE ~ 'multiracial'),
         gender = ifelse(as.integer(ESEX) == 1,'men','women'))%>%
  mutate(across(c('EHEARING','ESELFCARE','ESEEING','EAMBULAT','EERRANDS','ECOGNIT'),
                as.integer))%>%
  mutate(disability = case_when(
           ESEEING == 1 ~ 'blind',
           ESEEING == 2 & (
             ESELFCARE == 1 | EAMBULAT == 1 | EERRANDS == 1 | ECOGNIT == 1
           ) ~ 'disabled',
           ESEEING == 2 & ESELFCARE == 2 & EAMBULAT == 2 &
           EERRANDS == 2 & ECOGNIT == 2 ~ 'without additional disabilities'))%>%
  mutate(person_id = paste0(SSUID,PNUM))%>%
  mutate(edu_level = case_when(
    EEDUC == 39 ~ 'High school diploma',
    EEDUC %in% 40:41 ~ 'Some college',
    EEDUC %in% 42 ~ "Associate's degree",
    EEDUC %in% 43 ~ "Bachelor's degree",
    EEDUC %in% 44 ~ "Master's degree",
    EEDUC %in% 45:46 ~ "PhD, JD or MD",
    TRUE ~ "No high school diploma"
  ))

# Assess sample sizes-----------------------------------------------------------
# Education level
df%>%
  filter(MONTHCODE == 12)%>%
  group_by(SWAVE,auditory,edu_level)%>%
  summarise(n=n_distinct(person_id))%>%
  arrange(SWAVE, auditory, match(edu_level, 
    c("No high school diploma", "High school diploma",
      "Some college","Associate's degree",
      "Bachelor's degree","Master's degree", 
      "PhD, JD or MD"))
  )%>%
  pivot_wider(names_from = SWAVE, values_from = n)

# College enrollment
df%>%
  filter(MONTHCODE == 12)%>%
  mutate(enrolled = ifelse(EEDGRADE %in% 13:20,'yes','no'))%>%
  group_by(SWAVE,auditory,enrolled)%>%
  summarise(n=n_distinct(person_id))%>%
  pivot_wider(names_from = SWAVE, values_from = n)

# Identify all respondents with at least two education levels each
result<-df%>%
  filter(EEDUC > 38)%>%
  group_by(person_id)%>%
  summarise(educations = n_distinct(edu_level))%>%
  right_join(df, by='person_id', relationship = 'one-to-many')%>%
  group_by(auditory,educations)%>%
  summarise(n=n_distinct(person_id))

# Sankey diagram based on this
sankeyFlow <- function(string){
  edu_level_order <- c("No high school diploma", "High school diploma",
                       "Some college","Associate's degree",
                       "Bachelor's degree","Master's degree", 
                       "PhD, JD or MD")
  
  df_local <- df
  
  df_local$edu_level<-factor(df$edu_level, levels = edu_level_order)
  
  links<-df_local%>%
    filter(auditory == string)%>%
    select(person_id,edu_level)%>%
    unique()%>%
    arrange(person_id, edu_level)%>%
    group_by(person_id)%>%
    mutate(next_edu = lead(edu_level))%>%
    filter(!is.na(next_edu))%>%
    rename(
      'source' = 'edu_level',
      'target' = 'next_edu'
    )%>%group_by(source,target)%>%
    summarise(value = n(), .groups = "drop")
  
  nodes <- data.frame(
    name=levels(df_local$edu_level))
  
  links$IDsource <- as.integer(links$source) - 1
  links$IDtarget <- as.integer(links$target) - 1
  
  return(sankeyNetwork(Links = links, Nodes = nodes,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name", 
                sinksRight=T,width = 900, height = 900))
}

# deaf/hearing
sankeyFlow('deaf')
sankeyFlow('hearing')

# Enrollment persistence--------------------------------------------------------
# Limit to all participants being enrolled at the second wave
# for respondent_id in respondent_ids:
#   persisting = True
#   for year in years:
#     if enrolled[respondent_id][year] == "no":
#     persisting = False
#   persistence_status[respondent_id][year] = persisting
eligible_person_ids<-df%>%
  mutate(
    enrolled = ifelse(EEDGRADE %in% 13:20,T,F),
    eligible = case_when(
      SWAVE == 1 & !enrolled ~ TRUE,
      SWAVE == 2 & enrolled ~ TRUE,
      TRUE ~ FALSE
    )
  )%>%filter(eligible)%>%
  pull(person_id)%>%
  unique()

persistence<-df%>%
  filter(person_id %in% eligible_person_ids)%>%
  filter(MONTHCODE == 12 & SWAVE > 1)%>%
  mutate(enrolled = ifelse(EEDGRADE %in% 13:20,T,F))%>%
  select(person_id,SWAVE,enrolled,EEDUC)%>%
  arrange(person_id,SWAVE)%>%
  mutate(persist = NA, attained = NA)


# Get persistence rate
for(person in unique(persistence$person_id)){
  first_enroll <- F
  persisting <- NA
  prev_educ <- NA
  current_educ <- NA
  person_idx <- which(persistence$person_id == person)

  for(i in person_idx){
    current_educ <- persistence$EEDUC[i]
    if(persistence$SWAVE[i] == 2 & persistence$enrolled[i]){
      first_enroll <- T
      persisting <- T
      prev_educ <- current_educ
    } else if (is.na(persisting)){
      next
    } else if(!persisting | (first_enroll & !persistence$enrolled[i])){
      persisting <- F
    }
    # Before first enrollment, persist is NA; after, it's persisting status
    attained<-!is.na(prev_educ) && current_educ > prev_educ && current_educ > 41 
    persistence$attained[i] <- attained
    if(!first_enroll){
      persistence$persist[i] <- NA
    } else {
      if(persisting){
        persistence$persist[i] <- persisting
      }else{
        persistence$persist[i] <- attained
      }
    }
  }
}

# Verify my work
#persistence%>%
#  arrange(person_id,SWAVE)%>%
#  select(SWAVE,persist,enrolled,EEDUC)%>%
#  unique()%>%View()

# Merge with main data
clean_df<-persistence%>%
  select(person_id,SWAVE,persist)%>%
  right_join(df, by = c('person_id','SWAVE'))%>%
  filter(MONTHCODE == 12 & !is.na(persist) & person_id %in% eligible_person_ids)

clean_df%>%
  group_by(EHEARING,SWAVE,persist)%>%
  summarise(n = n())%>%
  mutate(
    persist_int = as.integer(!persist),
  )%>%arrange(EHEARING, SWAVE, persist)

# Implement Cox Regression------------------------------------------------------
clean_df<-clean_df%>%
  mutate(persist_int = as.integer(!persist),
         isDeaf = ifelse(EHEARING == 1, 1, 0),
         isWhite = ifelse(ERACE == 1,1,0),
         isOtherDisability = ifelse(EAMBULAT == 1 | ECOGNIT == 1 | ESELFCARE == 1 |
                                    EERRANDS == 1 | ESEEING == 1, 1, 0))

# Keep track of participants
surv_object <- Surv(time = clean_df$SWAVE, event = clean_df$persist_int)
km_fit <- survfit(surv_object ~ isDeaf, data=clean_df)
summary(km_fit)

# Cox
model <- coxph(Surv(SWAVE,persist_int) ~ isDeaf + age_by_month + isOtherDisability + isWhite, data = clean_df)
cox.zph(model)
summary(model)
