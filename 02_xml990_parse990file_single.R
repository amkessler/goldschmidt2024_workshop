# New Venture Fund
# EIN: 20-5806345
# Sixteen Thirty Fund
# EIN: 26-4486735

library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
library(writexl)
library(XML)
library(xml2)
options(scipen = 999)
options(stringsAsFactors = FALSE)


# Parsing an XML Download from PP Nonprofit Explorer Site

# New Venture Fund
# https://projects.propublica.org/nonprofits/organizations/205806345

newventure <- "https://projects.propublica.org/nonprofits/download-xml?object_id=202323149349302362"

xml_obj <- newventure %>%
  read_xml() %>%
  as_list()


# Parsing to isolate elements #####

# isolate company name ####
nonprofit_name <- xml_obj[["Return"]][["ReturnHeader"]][["Filer"]][["BusinessName"]][["BusinessNameLine1Txt"]][[1]]
nonprofit_name

# isolate company tax ID (EIN) ####
nonprofit_eid <- xml_obj[["Return"]][["ReturnHeader"]][["Filer"]][["EIN"]][[1]]
nonprofit_eid

# isolate filing period date ####
filing_period_date <- xml_obj[["Return"]][["ReturnHeader"]][["TaxPeriodEndDt"]][[1]]
filing_period_date



# Board of Directors and Highest Paid Employees ####

# The 990 includes a listing of the top 5 highest employees, board officers and board members.

# Executives, Officers & Board members
xml_mainreturn <- xml_obj[["Return"]][["ReturnData"]][["IRS990"]]

xml_mainreturn %>%
  enframe() %>%
  dplyr::filter(name == "Form990PartVIISectionAGrp") %>%
  unnest_wider(value) %>%
  unnest(cols = everything()) %>%
  select(-name) %>%
  unnest(cols = everything())



# Schedule I - Grants to other orgs ####
xml_schedule_i <- xml_obj[["Return"]][["ReturnData"]][["IRS990ScheduleI"]]

grants <- xml_schedule_i %>%
  enframe() %>%
  filter(name == "RecipientTable") %>%
  unnest_wider(value) %>%
  unnest_wider(USAddress) %>%
  unnest(cols = everything()) %>%
  select(-name) %>%
  unnest(cols = everything()) %>%
  unnest(RecipientBusinessName)

grants


#add in company name and filing date from saved earlier objects?
nonprofit_name
filing_period_date

grants <- grants %>%
  mutate(
    nonprofit_name = nonprofit_name,
    nonprofit_eid = nonprofit_eid,
    filing_period_date = filing_period_date,
    filing_period_year = lubridate::year(filing_period_date)
  ) %>%
  select(nonprofit_eid, nonprofit_name, filing_period_year, filing_period_date, everything())

grants




