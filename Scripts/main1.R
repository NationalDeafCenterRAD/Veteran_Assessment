# Parameters
MOST_RECENT_YEAR = 2023

# Modules
require("data.table")
require("lubridate")
require("bit64")
require("dplyr")
#require("ggplot2")
#require("plotly")

# Functions---------------------------------------------------------------------
# Prepare data------------------------------------------------------------------
years <- seq(MOST_RECENT_YEAR,MOST_RECENT_YEAR-2)
variables <- c('SSUID','PNUM','MONTHCODE','ERESIDENCEID','ERELRPE',
               'SPANEL','SWAVE','WPFINWGT','THHLDSTATUS', # <- use to filter for disabilities
               # Disabilities
               'EHEARING','ESELFCARE','ESEEING','EAMBULAT','EERRANDS','ECOGNIT',
               # Ascribed Attributes
               'TDOB_BYEAR','EDOB_BMONTH','ESEX','EHISPAN','ERACE','TRACE','EBORNUS',
               # Education and employment
               'EEDUC','RMESR','EEDENROLL','EEDBMONTH','EEDEMONTH','TMWKHRS','TPTOTINC',
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
  filter(grepl('^1\\d{2}',PNUM) & SPANEL == 2021 & THHLDSTATUS %in% 1:2)%>%
  mutate(TDOB_BYEAR = as.integer(TDOB_BYEAR),
         EDOB_BMONTH = as.integer(EDOB_BMONTH),
         MONTHCODE = as.integer(MONTHCODE),
         age_by_month = ifelse(MONTHCODE >= EDOB_BMONTH, PU_YEAR - TDOB_BYEAR, 
                               PU_YEAR - TDOB_BYEAR - 1))%>%
  filter(age_by_month > 15 & age_by_month < 65)

# Clean up data
result<-df%>%
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
           EERRANDS == 2 & ECOGNIT == 2 ~ 'without additional disabilities'))

# Assess sample sizes-----------------------------------------------------------
