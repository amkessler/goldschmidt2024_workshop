# https://projects.propublica.org/nonprofits/api
# Endpoint:
# https://projects.propublica.org/nonprofits/api/v2/organizations/{ein}.json
# IRS field list:
# https://www.irs.gov/pub/irs-soi/12eofinextractdoc.xls

library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
library(writexl)
options(scipen = 999)
options(stringsAsFactors = FALSE)



## SOLVE FOR ONE ####

# Sixteen Thirty Fund
# EIN: 26-4486735
# The API url wants the EIN number to identify the company. We'll give that.

org_url <- "https://projects.propublica.org/nonprofits/api/v2/organizations/264486735.json"

get_orginfo <- GET(org_url,
                    add_headers(`X-API-Key` = Sys.getenv("PROPUBLICA_API_KEY"))) #key stored as renviron variable

this.raw.content <- rawToChar(get_orginfo$content)

this.content <- fromJSON(this.raw.content)

#dataframe 2 contains the financial 990 data per year
content_df <- as.data.frame(this.content[[2]])

#no unnesting needed this time
result <- as_tibble(content_df)

result

glimpse(result)

#save
write_xlsx(result, "data/ppapi_264486735.xlsx")




### FUNCTION #####

ppapi_990_get_organization <- function(eid) {

  org_url <- paste0("https://projects.propublica.org/nonprofits/api/v2/organizations/", eid, ".json")

  get_orginfo <- GET(org_url,
                     add_headers(`X-API-Key` = Sys.getenv("PROPUBLICA_API_KEY"))) #key stored as environ variable

  this.raw.content <- rawToChar(get_orginfo$content)

  this.content <- fromJSON(this.raw.content)

  #dataframe 2 contains the financial 990 data per year
  content_df <- as.data.frame(this.content[[2]])

  #no unnesting needed this time
  result <- as_tibble(content_df)

  return(result)

}


#try for one org
ppapi_990_get_organization("264486735")




#run for multiple committees at once

#we'll take the one we already had, and add the New Venture Fund's id too
ein_ids <- c("264486735", "205806345")

ein_ids

#now we'll pull both companies at once
nonprofit_filings_combined <- map_df(ein_ids, ppapi_990_get_organization)

nonprofit_filings_combined

#2021 only
nonprofit_filings_combined %>%
  filter(tax_prd_yr == 2021)



