
# Dependencies ------------------------------------------------------------

library(DBI)
library(feather)
library(forcats)
library(lubridate)
library(odbc)
library(RODBC)
library(stringr)
library(tidyverse)

# Import ------------------------------------------------------------------

# specify database and query location
con_string <- "Driver={SQL Server};Server=cno-sqlreport02;Database=LAMA_Rpt"
query_file <- "P:/Code Enforcement/ce_abatement_decisions_completed.sql"

# initialize database connection
con <- odbcDriverConnect(con_string)

# read query from text file
query <- str_c(readLines(query_file), collapse = " ")

# query database
raw <- con %>% 
  sqlQuery(query, as.is = TRUE)
close(con)

# import parcel data

pardat <- read_csv("data/pardat.txt")

# Tidy --------------------------------------------------------------------

lama <- raw %>% 
  # convert from long to wide format
  spread(prop, val) %>% 
  # remove unneeded fields
  select(-matches("^abandoned|^accepted|^address|^apply|^asbestos|^bounding"),
         -matches("^call|^cdc|^change|^check|^collect|^compli|^cost|^curator"),
         -matches("^date|^deficient|^demo|^dupart|^even|^funding|^gisid"),
         -matches("^high|^historic district name|^house"),
         -matches("^if|^imminent|^in vieux"),
         -matches("^legal|^litigation|^lot|^mortgage|^municipal"),
         -matches("^occupied|^origination|^other|^parcel|^prior|^property"),
         -matches("^recordation|^request"),
         -matches("^sale|^selective|^sheriff|^square|^standing"),
         -matches("^tax bill|^tool|^trash"),
         -matches("^violation codes|^visible|^ward|^writ|^zoning"),
         -matches("score"))

# fix names
names(lama) <- names(lama) %>% 
  str_to_lower() %>% 
  str_replace_all("_", " ") %>%
  str_replace_all("[:punct:]", "") %>% 
  str_replace_all("[:space:]+", " ") %>% 
  str_trim() %>% 
  str_replace_all("[:space:]", "_")

# Transform ---------------------------------------------------------------

# filter out vacant lots and occupied structures
structures <- lama %>% 
  filter(buildings_occupied %in% c("False", "No") | is_the_property_occupied == "NO") %>% 
  select(assessor_id, tax_bill = pin, 4:6, everything(),
         -c(id,
            buildings_occupied,
            decision_date,
            in_historic_district,
            is_the_property_occupied))

source("./r/scrape.r")

# scrape assessed values from Assessor
# may take several minutes

root <- "http://qpublic9.qpublic.net/la_orleans_display.php?KEY="
node <- "tr:nth-child(4) td.tax_value:nth-child(4)"

parcels <- structures %>% 
  select(assessor_id, tax_bill) %>% 
  mutate(assessor_id = str_to_upper(str_trim(assessor_id)),
         assessor_id = if_else(is.na(assessor_id), "", assessor_id)) %>%
  rowwise() %>%
  mutate(value = scrape(assessor_id, root, node))

# scrape taxes owed from Treasury
# may take several minutes

root <- "http://services.nola.gov/service.aspx?load=treasury&Type=1&TaxBill="
node <- "#ctl10_TotalRealEstateTaxesDue"

parcels <- parcels %>% 
  mutate(tax_bill = str_to_upper(str_trim(tax_bill)),
         tax_bill = if_else(is.na(tax_bill), "", tax_bill)) %>% 
  rowwise() %>% 
  mutate(tax_owed = scrape(tax_bill, root, node))

# clean scraped values
values <- parcels %>% 
  mutate(value = str_replace_all(value, "[^[:alnum:]\\.]+", ""),
         tax_owed = str_replace_all(tax_owed, "[^[:alnum:]\\.]+", ""))

decisions <- cbind(structures, values[, 3:4]) %>% 
  select(outcome, value, violations, fees, tax_owed, everything(), -assessor_id, -tax_bill)

num_cols <- 2:5

decisions[, num_cols] <- map(decisions[, num_cols], as.numeric)

decisions <- decisions %>% 
  mutate(fees_to_value = fees / value,
         taxes_to_value = tax_owed / value) %>% 
  select(outcome,
         value:tax_owed,
         fees_to_value:taxes_to_value,
         characterize_the_amount_of_blight_on_the_same_block:structure_boarded)

factor_cols <- 8:27

# decisions <- decisions[complete.cases(decisions[,factor_cols]),]
# 
# # roll up categories with few observations
# lump_cats <- function(x, thresh) {
#   prop <- 0.01
#   if (min(table(x)) < thresh) {
#     while (min(table(x)) < thresh) {
#       x <- fct_lump(x, prop = prop)
#       prop <- prop + 0.01
#     }
#   }
#   x
# }
# 
# decisions[,factor_cols] <- map(decisions[,factor_cols], lump_cats, 10)

for (x in names(decisions)[factor_cols]) {
  decisions[[x]][str_detect(decisions[[x]], "^$")] <- NA
  decisions[[x]][str_detect(decisions[[x]], "^[:space:]+$")] <- NA
}

# TODO(Steven) join HDLC, NCDA, and MVA polygon layers
#
#

# Output ------------------------------------------------------------------

write_feather(decisions, "./data/all_decisions.feather")


