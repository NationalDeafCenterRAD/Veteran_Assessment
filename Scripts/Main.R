# Modules
library(dplyr)
library(tidyr)
library(data.table)


# Functions---------------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

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
    right_join(result, by = thestring)
  
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
filepath <- paste0(getwd(),'/Raw Data/',year_n,'_5y_csv_pus.zip')

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
                              "dout","dphy","dratx","drem","drat",
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

hdestfile <- paste(getwd(),'Raw Data','2023_5y_csv_hus.zip',sep='/')
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
system("find . -size +500M -not -path '*/.*' | sed 's|^\\./||' >> .gitignore")
