#### Get NPPES data ####

rm(list=ls())
gc()

#prerequisites
library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)
library(devtools)

#install from github
install_github( "ajdamico/lodown" , dependencies = TRUE)
library("lodown")

#download
lodown("nppes",
       output_dir=file.path(path.expand("~/public_microdata"),"NPPES/data_raw"))

#import and filter
npi_filepath<-grep("npidata_pfile_20050523-([0-9]+)\\.csv",
                    list.files(file.path(path.expand("~/public_microdata"),"NPPES/data_raw"),
                               full.names = TRUE),
                    value = TRUE)
npi_header<-grep("npidata_pfile_20050523-([0-9]+)\\_FileHeader\\.csv",
                 list.files(file.path(path.expand("~/public_microdata"),"NPPES/data_raw"),
                            full.names = TRUE),
                 value = TRUE)

col_nm<-data.frame(colnames=names(read.csv(npi_header)),
                   stringsAsFactors = F) %>%
  mutate(colnames2=gsub("_$","",gsub("\\.+","_",tolower(colnames)))) %>%
  mutate(colclass=ifelse(grepl("code",colnames2)&!grepl("country|state|gender|taxonomy|postal",colnames2),
                          'integer','character'))
  mutate(include=ifelse(colnames2 %in% c("npi",
                                         "entity_type_code",
                                         "provider_organization_name_legal_business_name",
                                         "provider_last_name_legal_name",
                                         "provider_first_name",
                                         "provider_middle_name",
                                         "provider_gender_code",
                                         "provider_first_line_business_practice_location_address",
                                         "provider_second_line_business_practice_location_address",
                                         "provider_business_practice_location_address_city_name",
                                         "provider_business_practice_location_address_state_name",
                                         "provider_business_practice_location_address_postal_code",
                                         "provider_first_line_business_mailing_address",
                                         "provider_second_line_business_mailing_address",
                                         "provider_business_mailing_address_city_name",
                                         "provider_business_mailing_address_state_name",
                                         "provider_business_mailing_address_postal_code",
                                         "healthcare_provider_taxonomy_code_1",
                                         "healthcare_provider_taxonomy_group_1",
                                         "provider_enumeration_date",
                                         "last_update_date",
                                         "npi_deactivation_date",
                                         "npi_reactivation_date"),
                        1,0))

nppes_df<-fread(npi_filepath,
                # select=which(col_nm$include==1),
                # col.names=col_nm$colnames2[col_nm$include==1],
                na.strings="") %>%
  unite("practice_location",c("provider_first_line_business_practice_location_address",
                              "provider_second_line_business_practice_location_address",
                              "provider_business_practice_location_address_city_name",
                              "provider_business_practice_location_address_state_name",
                              "provider_business_practice_location_address_postal_code"),sep="|") %>%
  unite("mailing_address",c("provider_first_line_business_mailing_address",
                            "provider_second_line_business_mailing_address",
                            "provider_business_mailing_address_city_name",
                            "provider_business_mailing_address_state_name",
                            "provider_business_mailing_address_postal_code"),sep="|")

#test if search result consistent with NPPES API
nppes_df %>% 
  filter(grepl("UNIVERSITY OF KANSAS",provider_organization_name_legal_business_name)) %>%
  View
#--yes

#collect only organizations
nppes_org<-nppes_df %>%
  filter(entity_type_code==2)

saveRDS(nppes_org,file="./NPPES/data/nppes_org.rda")

