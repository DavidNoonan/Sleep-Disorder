library(tidyverse)
library(haven)

get_nhanes_data <- function(nhanes_file){
  
  nhanes_file_url = url(paste0("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/", nhanes_file, "_H.XPT"))
  
  dat = haven::read_xpt(file = nhanes_file_url)
  
  return(dat)
  
}

# NHANES filename prefixes

dataset_names <- c(
  "ACQ",
  "ALQ",
  "BPQ",
  "CBQ",
  "CDQ",
  "CSQ",
  "DBQ",
  "DEMO",
  "DEQ",
  "DIQ",
  "DLQ",
  "DPQ",
  "DUQ",
  "ECQ",
  "FSQ",
  "HEQ",
  "HIQ",
  "HOQ",
  "HSQ",
  "HUQ",
  "IMQ",
  "INQ",
  "MCQ",
  "OCQ",
  "OHQ",
  "OSQ",
  "PAQ",
  "PFQ",
  "PUQMEC",
  "RHQ",
  "SLQ",
  "SMQ",
  "SMQFAM",
  "SMQRTU",
  "SMQSHS",
  "SXQ",
  "WHQ",
  "WHQMEC") %>% as.list()

# download data

nhanes_data <- map(.x = dataset_names,
                   .f = function(x) get_nhanes_data(x)) %>%
  purrr::reduce(.f = dplyr::full_join, by = "SEQN")

sleep_disorder_data <- nhanes_data %>% select("RIAGENDR",
                                              "RIDAGEYR",
                                              "RIDRETH3",
                                              "BPQ020",
                                              "BPQ080",
                                              "HSD010",
                                              "DIQ010",
                                              "DBQ700",
                                              "DLQ010",
                                              "DLQ020",
                                              "FSD151",
                                              "HIQ011",
                                              "HOQ065",
                                              "IND235",
                                              "MCQ010",
                                              "MCQ053",
                                              "MCQ080",
                                              "MCQ160A",
                                              "MCQ160B",
                                              "MCQ160C",
                                              "MCQ160E",
                                              "MCQ160F",
                                              "MCQ160L",
                                              "MCQ220",
                                              "OCD150",
                                              "PUQ110",
                                              "PAQ665",
                                              "PAQ710",
                                              "PAQ715",
                                              "SLD010H",
                                              "SLQ050",
                                              "SLQ060",
                                              "SMQ020",
                                              "SMDANY")

test <- sleep_disorder_data %>% mutate_at(.vars = c(
  "BPQ020",
  "BPQ080",
  "HSD010",
  "DIQ010",
  "DBQ700",
  "DLQ010",
  "DLQ020",
  "FSD151",
  "HIQ011",
  "HOQ065",
  "MCQ010",
  "MCQ053",
  "MCQ080",
  "MCQ160A",
  "MCQ160B",
  "MCQ160C",
  "MCQ160E",
  "MCQ160F",
  "MCQ160L",
  "MCQ220",
  "OCD150",
  "PUQ110",
  "PAQ665",
  "SLQ050",
  "SLQ060",
  "SMQ020",
  "SMDANY"
), .funs = function(x){ if_else(condition = x %in% c(7, 9), true = NA, false = x)})


# /**********Dataset use for the model, with all recoding done**********/
#   data nhanes;
# /*Still need to determine variables in demo_h that should be kept.*/
#   merge demo_h acq_h alq_h bpq_h cbq_h cdq_h csq_h dbq_h deq_h diq_h dlq_h dpq_h duq_h ecq_h fsq_h heq_h
# hiq_h hoq_h hsq_h huq_h imq_h inq_h mcq_h ocq_h ohq_h osq_h paq_h pfq_h puqmec_h rhq_h slq_h
# smq_h smqfam_h smqrtu_h smqshs_h sxq_h whq_h whqmec_h;
# by SEQN;
# keep RIAGENDR RIDAGEYR RIDRETH3 BPQ020 BPQ080 HSD010 DIQ010 DBQ700 DLQ010 DLQ020
# FSD151 HIQ011 HOQ065 IND235 MCQ010 MCQ053 MCQ080 MCQ160a MCQ160b MCQ160c MCQ160e MCQ160f MCQ160l MCQ220
# OCD150 PUQ110 PAQ665 PAQ710 PAQ715 SLD010H SLQ050 SLQ060 SMQ020 SMDANY;
# 
# /*Recoding the values "Refuse" and "Don't Know" as missing values*/
#   array vars1{27} BPQ020 BPQ080 HSD010 DIQ010 DBQ700 DLQ010 DLQ020
# FSD151 HIQ011 HOQ065 MCQ010 MCQ053 MCQ080 MCQ160a MCQ160b MCQ160c MCQ160e MCQ160f MCQ160l MCQ220
# OCD150 PUQ110 PAQ665 SLQ050 SLQ060 SMQ020 SMDANY;
# do i=1 to dim(vars1);
# if (vars1{i} = 7) or (vars1{i} = 9) then call missing(vars1{i});
# end;
# array vars2{4} IND235 PAQ710 PAQ715 SLD010H;
# do i=1 to dim(vars2);
# if (vars2{i} = 77) or (vars2{i} = 99) then call missing(vars2{i});
# end;
# if OCD150=2 then OCD150=1;
# if OCD150=3 then OCD150=4;
# /*Variables that don't have values that need to be recoded*/
# 	/*RIAGENDR RIDAGEYR RIDRETH3*/
# 	/**********************************************************/
# 	if RIDAGEYR ge 20;
# run;
# /************************************************************************/
# 
# /**********Formats for each variable to make the output look nice**********/
# proc format;
# 	value nhanesYN 1="Yes" 2="No";
# 	value nhanesBPYN 1="Yes High Blood Pressure" 2="No High Blood Pressure";
# 	value nhanesHCYN 1="Yes High Cholesterol" 2="No High Cholesterol";
# 	value nhanesDBYN 1="Yes Diabetes" 2="No Diabetes" 3="Borderline Diabetes";
# 	value nhanesASYN 1="Yes Asthma" 2="No Asthma";
# 	value nhanesOWYN 1="Yes Overweight" 2="No Overweight";
# 	value nhanesARYN 1="Yes Arthritis" 2="No Arthritis";
# 	value nhanesGender 1="Male" 2="Female";
# 	value nhanesRace 1="Mexican American" 2="Other Hispanic" 3="Non-Hispanic White"
# 			4="Non-Hispanic Black" 6="Non-Hispanic Asian" 7="Other Race - Including Multi-Racial";
# 	value nhanesHealth 1="Excellent General Health" 2="Very Good General Health"
# 			3="Good General Health" 4="Fair General Health" 5="Poor General Health";
# 	value nhanesHome 1="Owned or Being Bought" 2="Rented" 3="Other Arrangement";
# 	value nhanesSleepHours 1="1 Hour" 2="2 Hours" 3="3 Hours" 4="4 Hours" 5="5 Hours" 6="6 Hours"
# 			7="7 Hours" 8="8 Hours" 9="9 Hours" 10="10 Hours" 11="11 Hours" 12="12 or more Hours";
# 	value nhanesWork 1="Employed" 4="Unemployed";
# 	value nhanesHIYN 1="Yes Health Insurance" 2="No Health Insurance";
# run;