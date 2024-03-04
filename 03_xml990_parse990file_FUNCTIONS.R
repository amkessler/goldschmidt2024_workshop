# FUNCTIONS 990 XML PARSING, BASED ON WORK IN PREVIOUS 01 CODE ####

library(tidyverse)
library(janitor)
library(writexl)
library(XML)
library(xml2)
options(scipen = 999)
options(stringsAsFactors = FALSE)



# TARGET NONPROFIT LIST / SELECTIONS ##################################

#load in table
gsheet <- read_csv("data/xmls_list.csv")

gsheet

#create vector from data column containing target links to xml files
linklist_vector <- gsheet %>%
  pull(xml_link)

#create vector of nonprofit names/years to go with log file output
namelist_vector <- gsheet %>%
  mutate(
    namelist = paste0(str_replace_all(nonprofit_name, " ", ""), "_", filing_year)
  ) %>%
  pull(namelist)




# SCHEDULE I (GRANTS) ########################################################

parse990_schedule_i <- function(xml_url_location) {
  #download and read in xml file, convert to list
  xml_obj <- xml_url_location %>%
    read_xml() %>%
    as_list()
  # isolate company name
  nonprofit_name <- xml_obj[["Return"]][["ReturnHeader"]][["Filer"]][["BusinessName"]][["BusinessNameLine1Txt"]][[1]]
  # isolate company tax ID (EIN)
  nonprofit_eid <- xml_obj[["Return"]][["ReturnHeader"]][["Filer"]][["EIN"]][[1]]
  # isolate filing period date
  filing_period_date <- xml_obj[["Return"]][["ReturnHeader"]][["TaxPeriodEndDt"]][[1]]
  # pull all of schedule I
  xml_schedule_i <- xml_obj[["Return"]][["ReturnData"]][["IRS990ScheduleI"]]
  # now we'll parse the data for grant recipients
  grants <- xml_schedule_i %>%
    enframe() %>%
    filter(name == "RecipientTable") %>%
    unnest_wider(value) %>%
    unnest_wider(USAddress) %>%
    unnest(cols = everything()) %>%
    select(-name) %>%
    unnest(cols = everything()) %>%
    unnest(RecipientBusinessName)
  #add field for company name, ein and filing date
  grants <- grants %>%
    mutate(
      nonprofit_name = nonprofit_name,
      nonprofit_eid = nonprofit_eid,
      filing_period_date = filing_period_date,
      filing_period_year = lubridate::year(filing_period_date)
    ) %>%
    select(nonprofit_eid, nonprofit_name, filing_period_year, filing_period_date, everything()) %>%
    clean_names()

  return(grants)
}


# test out function on one filing ####
# New Venture Fund
# https://projects.propublica.org/nonprofits/organizations/205806345

newventure <- "https://projects.propublica.org/nonprofits/download-xml?object_id=202113169349310971"

parsed_grants_data <- parse990_schedule_i(newventure)
parsed_grants_data

# can also feed in url directly
# parse990_schedule_i("https://projects.propublica.org/nonprofits/download-xml?object_id=202323149349302362")

# save results
write_xlsx(parsed_grants_data, "data/parsed_grants_data_onefiling.xlsx")


# PROCESS MULTIPLE filings and/or companies ####

# So we'll use the possibly() function to run a "safer" version of the function
# that will log the errors but keep processing the rest rather than stop. TODO: figure out if can be parallelized...
safer_parse990_schedule_i <- possibly(parse990_schedule_i, otherwise = "Error from No Schedule I present.")

# Now we'll run the safer version which should process all the valid files
# and return a list that lets us see which files were causing problems
all_results <- map(linklist_vector, safer_parse990_schedule_i)

#see the results list produced by the safer function
str(all_results, max.level = 1)

#give list items understandable names
names(all_results) <- namelist_vector
str(all_results, max.level = 1)

#save the entire list to a log file to have for later reference
str(all_results, max.level = 1, list.len = 1000) %>%
  capture.output(file = "data/log_results_xml_grant_data.txt")


#filter for JUST THE ACTUAL DATAFRAMES within the large list,
#then combine it into a single dataframe
grants_data_multiple_filings <- all_results %>%
  keep(~is_tibble(.x)) %>%
  bind_rows()

View(grants_data_multiple_filings)
#bingo.

#check
grants_data_multiple_filings %>%
  count(nonprofit_name)

# format numeric columns and dates
grants_data_multiple_filings <- grants_data_multiple_filings %>%
  mutate(
    cash_grant_amt = as.numeric(cash_grant_amt),
    non_cash_assistance_amt = as.numeric(non_cash_assistance_amt),
    filing_period_date = ymd(filing_period_date)

  )

# save results ####
saveRDS(grants_data_multiple_filings, "data/grants_data_multiple_filings.rds")
write_xlsx(grants_data_multiple_filings, "data/grants_data_multiple_filings.xlsx")




# BOARD OR DIRECTORS / HIGHEST-PAID EMPLOYEES #################################


## Function ####

parse990_directors <- function(xml_url_location) {
  #download and read in xml file, convert to list
  xml_obj <- xml_url_location %>%
    read_xml() %>%
    as_list()
  # isolate company name
  nonprofit_name <- xml_obj[["Return"]][["ReturnHeader"]][["Filer"]][["BusinessName"]][["BusinessNameLine1Txt"]][[1]]
  # isolate company tax ID (EIN)
  nonprofit_eid <- xml_obj[["Return"]][["ReturnHeader"]][["Filer"]][["EIN"]][[1]]
  # isolate filing period date
  filing_period_date <- xml_obj[["Return"]][["ReturnHeader"]][["TaxPeriodEndDt"]][[1]]
  #isolate main part of 990 return within xml
  xml_mainreturn <- xml_obj[["Return"]][["ReturnData"]][["IRS990"]]
  #extract directors/employees section into dataframe
  directors <- xml_mainreturn %>%
    enframe() %>%
    dplyr::filter(name == "Form990PartVIISectionAGrp") %>%
    unnest_wider(value) %>%
    unnest(cols = everything()) %>%
    select(-name) %>%
    unnest(cols = everything())
  #add field for company name, ein and filing date
  directors <- directors %>%
    mutate(
      nonprofit_name = nonprofit_name,
      nonprofit_eid = nonprofit_eid,
      filing_period_date = filing_period_date,
      filing_period_year = lubridate::year(filing_period_date)
    ) %>%
    select(nonprofit_eid, nonprofit_name, filing_period_year, filing_period_date, everything()) %>%
    clean_names()

  return(directors)

}



## Process Multiple Filings ####

safer_parse990_directors <- possibly(parse990_directors, otherwise = "Error in extracting directors section.")

# Now we'll RUN the safer version which should process all the valid files
# and return a list that lets us see which files were causing problems
directors_all_results <- map(linklist_vector, safer_parse990_directors)

#see the results list produced by the safer function
str(directors_all_results, max.level = 1)

#give list items understandable names
names(directors_all_results) <- namelist_vector
str(directors_all_results, max.level = 1)

#save the entire list to a log file to have for later reference
str(directors_all_results, max.level = 1, list.len = 1000) %>%
  capture.output(file = "data/log_results_xml_directors_data.txt")

#filter for JUST THE ACTUAL DATAFRAMES within the large list,
#then combine it into a single dataframe
directors_data_multiple_filings <- directors_all_results %>%
  keep(~is_tibble(.x)) %>%
  bind_rows()

directors_data_multiple_filings

#check
directors_data_multiple_filings %>%
  count(nonprofit_name)

directors_data_multiple_filings %>%
  count(nonprofit_name, filing_period_date)

glimpse(directors_data_multiple_filings)

# format numeric columns and dates
directors_data_multiple_filings <- directors_data_multiple_filings %>%
  mutate(
    average_hours_per_week_rt = as.numeric(average_hours_per_week_rt),
    average_hours_per_week_rltd_org_rt = as.numeric(average_hours_per_week_rltd_org_rt),
    reportable_comp_from_org_amt = as.numeric(reportable_comp_from_org_amt),
    reportable_comp_from_rltd_org_amt = as.numeric(reportable_comp_from_rltd_org_amt),
    other_compensation_amt = as.numeric(other_compensation_amt),
    filing_period_date = ymd(filing_period_date)
  ) %>% select(-business_name)

glimpse(directors_data_multiple_filings)


# save results ####
saveRDS(directors_data_multiple_filings, "data/directors_data_multiple_filings.rds")
write_xlsx(directors_data_multiple_filings, "data/directors_data_multiple_filings.xlsx")




