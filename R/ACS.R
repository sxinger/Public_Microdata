#### Get ACS data ####

rm(list=ls())
gc()

#prerequisites\
install.packages("openxlsx")
install.packages("tidycensus")
install.packages("acs")

library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)
library(devtools)
library(openxlsx)
library(tidycensus)
library(acs)

devtools::install_github("hrecht/censusapi")
library(censusapi)

meta<-read.xlsx("./ACS/ACS2018_Table_Shells.xlsx",
                na.strings=c(""," ","  ")) %>%
  filter(!is.na(Table.ID)&!is.na(UniqueID)) %>% 
  select(-Data.Release) %>%
  mutate(Stub2=case_when(grepl(":",Stub) | is.na(UniqueID) ~ Stub,
                          TRUE ~ NA_character_)) %>%
  fill(Stub2,.direction="down") %>%
  mutate(SEX=case_when(grepl("female",tolower(Stub))|grepl("female",tolower(Stub2)) ~ "F",
                       grepl("male",tolower(Stub))|grepl("male",tolower(Stub2)) ~ "M",
                       TRUE ~ "NI"),
         RACE=case_when(substr(UniqueID,7,7)=="A" ~ "WHITE",
                        substr(UniqueID,7,7)=="B" ~ "BLACK",
                        substr(UniqueID,7,7)=="C" ~ "AMERICAN.IND",
                        substr(UniqueID,7,7)=="D" ~ "ASIAN",
                        substr(UniqueID,7,7)=="E" ~ "PAC.ISLAND",
                        substr(UniqueID,7,7)=="F" ~ "OTHER",
                        substr(UniqueID,7,7)=="G" ~ "TWO.RACES",
                        substr(UniqueID,7,7)=="H" ~ "WHITE",
                        TRUE ~ "NI"),
         ETHNICITY=case_when(substr(UniqueID,7,7)=="H" ~ "N",
                             substr(UniqueID,7,7)=="I" ~ "Y",
                             TRUE ~ "NI"),
         AGE=case_when(grepl("years",tolower(Stub)) ~ gsub("(to)|(and)","-",gsub("years","",gsub("and over","<",gsub("under","<",tolower(Stub))))),
                       grepl("years",tolower(Stub2)) ~ gsub("(to)|(and)","-",gsub("years","",gsub("and over","<",gsub("under","<",tolower(Stub2))))),
                       TRUE ~ "NI"),
         FAMTYPE=case_when(grepl("nonfamily househ",tolower(Stub))|grepl("nonfamily househ",tolower(Stub2)) ~ "NF",
                           grepl("family househ",tolower(Stub))|grepl("family househ",tolower(Stub2)) ~ "F",
                           TRUE ~ "NI")) %>%
  inner_join(read.xlsx("./ACS/2018_DataProductList.xlsx",
                       na.strings=c(""," ","  ")),
             by="Table.ID") %>%
  filter(substr(Table.ID,2,3) %in% c("07","08",
                                     "10","13",
                                     "15","17",
                                     "18","19",
                                     "20","21",
                                     "22","23",
                                     "24","27",
                                     "28")&
        substr(Table.ID,1,1)=="B" & !grepl("PR",Table.ID))


census_api_key("f343a279f0e0609ceb96f7ae6271c16549a5ac7", 
               overwrite = TRUE, install = TRUE)
readRenviron("~/.Renviron")
# Sys.getenv("CENSUS_API_KEY")


tbl_lst<-unique(meta$Table.ID)
geo_lst<-c("tract","zip","county")
for(j in seq_along(geo_lst)){
  for(i in seq_along(tbl_lst)){
    tbl<-get_acs(
      geography=geo_lst[j],
      table = tbl_lst[i],
      cache_table = TRUE,
      year = 2018,
      key='936f1e5f9be93142163750053b65ca37b7f1bff8',
      output = "wide",
      survey = "acs5",
      show_call = TRUE
    )

    tbl<-getCensus(name="acs/acs5",
                   vintage=2018,
                   key="936f1e5f9be93142163750053b65ca37b7f1bff8",
                   region=paste0(geo_lst[j],":*"),
                   var=paste0("group(",tbl_lst[i],")"))
    
    write.csv(tbl,file=paste0("./ACS/",geo_lst[j],"/",tbl_lst[i],".csv"),
              row.names = F)
  }
}