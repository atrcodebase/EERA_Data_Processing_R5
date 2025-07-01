rm(list = ls())
library(dplyr)
library(tidyr)
library(stringr)
library(atRfunctions)


## Data sets
# tool1.data <- read_xlsx_sheets("./input/raw_data/Tool 1.EERA Public School - HeadmasterPrinciple Interview R3.xlsx")
tool2.data <- read_xlsx_sheets("./input/raw_data/Tool 2.EERA Public School - Light Tool R4.xlsx")
tool8.data <- read_xlsx_sheets("./input/raw_data/Tool 8.EERA CBE - Class Level Tool R4.xlsx")
tool9.data <- read_xlsx_sheets("./input/raw_data/Tool 9.EERA CBE - IP Level Tool R4.xlsx")
# tool.infra <- read_xlsx_sheets("./input/raw_data/EERA_HPT_INFRA_TOOL.xlsx")
tool.infra_monit <- read_xlsx_sheets("./input/WASH_infra_checklist/EERA_WASH_INFRA_CHECKLIST_TOOL.xlsx")


## Red Flag - sorted same as EERA Implementation Red Flags Framework (Severity)
## Critical
# 1
tool9.gbv_complaints <- tool9.data$Questions_Repeat %>% 
  filter(
    B5 == 1 | ((B5 != 0 | is.na(B5)) & (!is.na(A7_Photo1) | !is.na(A7_Photo2_QA_Photo)))
  ) %>% 
  mutate(
    Tool = "CBE IP Level Tool",
    RF = "Complaints related to gender-based violence (GBV), sexual harassment, or abuse/exploitation been received",
    Type = "Critical"
  ) %>%
  left_join(
    tool9.data$data %>% select(KEY, Site_Visit_ID, IP_Name, Region, Province, District, Village), by = c("PARENT_KEY" = "KEY")
  ) %>% 
  rename(EMIS_School_ID_CBE_KEY = CBE_EMIS_School_ID_CBE_KEY, School_CBE_Name = CBE_School_CBE_Name) %>% 
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name', 
      'Region', 
      'Province', 
      'District', 
      'Village', 
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )

# 2 
tool.infra_injuries <- tool.infra_monit$Environmental_And_Social_Sta... %>% 
  filter(
    Have_Any_Injuries_Been_Reported_In_The_Sub_Project == 1 | Have_There_Been_Any_Security_Incidents_Affecting_The_School_The_Students_Teachers_The_Workers_In_The_Last_12_Months == 1
  ) %>% 
  left_join(tool.infra_monit$data %>%  select(
    KEY, Site_Visit_ID, Region, Province, District, Village, School_Name_Based_On_Mis, School_Name_Based_On_On_Site_Assessment, EMIS_School_ID_CBE_KEY = School_Id_Based_On_On_Site_Assessment,
    ends_with("Latitude"),
    ends_with("Longitude")), by = c("PARENT_KEY" = "KEY")) %>% 
  mutate(
    Tool = "WASH/Ifrastructure Monitoring",
    RF = "Injuries and/or security incidents reported",
    Type = "Critical"
  ) %>% 
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name',
      'Region', 
      'Province', 
      'District', 
      'Village',
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )

# 3
tool.infra_gbv_complaints <- tool.infra_monit$Environmental_And_Social_Sta... %>% 
  mutate(
    How_Many_Complaints_Are_Recorded_In_The_Complaints_System = case_when(
      How_Many_Complaints_Are_Recorded_In_The_Complaints_System == "No complaint recorded yet" ~ "0",
      TRUE ~ How_Many_Complaints_Are_Recorded_In_The_Complaints_System
    )
  ) %>% 
  filter(
    Have_Any_Complaints_Related_To_Gbv_Or_Sexual_Exploitation_And_Abuse_Sexual_Harassment_Sea_Sh_Been_Received == 1 | as.integer(How_Many_Complaints_Are_Recorded_In_The_Complaints_System) > 0
    # FIXME: How_Many_Complaints_Are_Recorded_In_The_Complaints_System  (the question is an open-ended question) > 0
  ) %>% 
  left_join(tool.infra_monit$data %>%  select(
    KEY, Site_Visit_ID, Region, Province, District, Village, School_Name_Based_On_Mis, School_Name_Based_On_On_Site_Assessment, EMIS_School_ID_CBE_KEY = School_Id_Based_On_On_Site_Assessment,
    ends_with("Latitude"),
    ends_with("Longitude")), by = c("PARENT_KEY" = "KEY")) %>% 
  mutate(
    Tool = "WASH/Ifrastructure Monitoring",
    RF = "Complaints related to gender-based violence (GBV), sexual harassment, or abuse/exploitation been received ",
    Type = "Critical"
  ) %>% 
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name',
      'Region', 
      'Province', 
      'District', 
      'Village',
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )


## Major
# 4
tool8.islamic_edu <- tool8.data$Subjects_Added %>% 
  left_join(
    tool8.data$data %>% select(Site_Visit_ID, EMIS_School_ID_CBE_KEY, Region, Province, District, Village, School_CBE_Name, D10, Y4_N, KEY, ends_with("Latitude"),
                               ends_with("Longitude")), 
    by = c("PARENT_KEY"="KEY")) %>% 
  filter(
    # Is the CBE an Islamic education center? AND How many subjects have been added? Can you please name the (1) subject?
    # 1. Yes AND 1. YES AND Religious topics
    #FIXME: The Response for "How many subjects have been added?" is a numeric value (Not Y/N)
    #FIXME: The Response for "Can you please name the subject?"is Y/N and does not provide the subject name, 
    #FIXME: To check the name of subject we need to include "Subject name:" question but it has an Open-ended value and "Religious Topics" could be reflected in multiple ways, 
    # it must be checked frequently
    (D10 == 1) | (D10 == 0 & Y4_N > 0 & Y5 == 1)
    # & (str_detect(Y5_Subject_Name, "Religious") | str_detect(Y5_Subject_Name, "قاعده") | str_detect(Y5_Subject_Name, "Islam")  | str_detect(Y5_Subject_Name, "اسلام"))
  ) %>% 
  mutate(
    Tool = "CBE Class Level Tool",
    RF = "CBE is an Islamic education school on the day of observation",
    Type = "Major"
  ) %>% 
  select(
    Site_Visit_ID, 
    EMIS_School_ID_CBE_KEY, 
    Region, Province, 
    District, 
    Village,
    School_CBE_Name,
    Tool,
    RF,
    Type,
    KEY,
    # ends_with("Latitude"),
    # ends_with("Longitude")
    )


# 5
tool8.grm_not_established <- tool8.data$data %>% 
  filter(
    (W1 %in% c(0, 9999)) | ((W1 == 1) & (W14 == 0 & W17 == 0 & W20 == 0))
  ) %>% 
  mutate(
    Tool = "CBE Class Level Tool",
    RF = "GRM not established",
    Type = "Major"
  ) %>% 
  select(
    Site_Visit_ID, 
    EMIS_School_ID_CBE_KEY, 
    Region, Province, 
    District, 
    Village,
    School_CBE_Name,
    Tool,
    RF,
    Type,
    KEY,
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )

# 6
tool9.grm_not_established <- tool9.data$Questions_Repeat %>% 
  filter(A1 == 0) %>% 
  mutate(
    Tool = "CBE IP Level Tool",
    RF = "Grievance Redress Mechanism (GRM) not established ",
    Type = "Major"
  ) %>%
  left_join(
    tool9.data$data %>% select(KEY, Site_Visit_ID, IP_Name, Region, Province, District, Village), by = c("PARENT_KEY" = "KEY")
  ) %>% 
  rename(EMIS_School_ID_CBE_KEY = CBE_EMIS_School_ID_CBE_KEY, School_CBE_Name = CBE_School_CBE_Name) %>%  
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name',
      'Region', 
      'Province', 
      'District', 
      'Village',
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )

# 7
tool9.grm_established_no_logbook <- tool9.data$Questions_Repeat %>%         
  filter(
    (A7 %in% c(0, 1))  & (is.na(A7_Photo1) & is.na(A7_Photo2_QA_Photo))
  ) %>% 
  mutate(
    Tool = "CBE IP Level Tool",
    RF = "GRM was established, but no GRM logbook was found.",
    Type = "Major"
  ) %>%
  left_join(
    tool9.data$data %>% select(KEY, Site_Visit_ID, IP_Name, Region, Province, District, Village), by = c("PARENT_KEY" = "KEY")
  ) %>% 
  rename(EMIS_School_ID_CBE_KEY = CBE_EMIS_School_ID_CBE_KEY, School_CBE_Name = CBE_School_CBE_Name) %>% 
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name',
      'Region', 
      'Province', 
      'District', 
      'Village',
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )

# 8 
tool9.coc_not_available <- tool9.data$Questions_Repeat %>%      
  filter(
    B2 == 0
  ) %>% 
  mutate(
    Tool = "CBE IP Level Tool",
    RF = "Codes of Conduct (CoC) not available",
    Type = "Major"
  ) %>%
  left_join(
    tool9.data$data %>% select(KEY, Site_Visit_ID, IP_Name, Region, Province, District, Village), by = c("PARENT_KEY" = "KEY")
  ) %>% 
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name',
      'Region', 
      'Province', 
      'District', 
      'Village',
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )

# 9 
tool.infra_esmp_not_available <- tool.infra_monit$Documents %>% 
  filter(
    Environmental_and_Social_Screening_Checklist == 2 | Environmental_And_Social_Management_Plan_Esmp == 3
  ) %>% 
  left_join(tool.infra_monit$data %>%  select(
    KEY, Site_Visit_ID, Region, Province, District, Village, School_Name_Based_On_Mis, School_Name_Based_On_On_Site_Assessment, EMIS_School_ID_CBE_KEY = School_Id_Based_On_On_Site_Assessment,
    ends_with("Latitude"),
    ends_with("Longitude")), by = c("PARENT_KEY" = "KEY")) %>% 
  mutate(
    Tool = "WASH/Ifrastructure Monitoring",
    RF = "Site-screening checklists and ESMPs not available",
    Type = "Major"
  ) %>% 
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name',
      'Region', 
      'Province', 
      'District', 
      'Village',
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )

# 10
tool.infra_coc_not_available <- tool.infra_monit$Environmental_And_Social_Sta...%>% 
  filter(
    Is_There_A_Copy_Of_The_Coc_On_Site_For_Me_To_Photograph == 2
  ) %>% 
  left_join(tool.infra_monit$data %>%  select(
    # KEY, Site_Visit_ID, Region, Province, District, Village, School_Name_Based_On_Mis, School_Name_Based_On_On_Site_Assessment, ends_with("Latitude"),
    # ends_with("Longitude")), by = c("PARENT_KEY" = "KEY")) %>% 
    KEY, Site_Visit_ID, Region, Province, District, Village, School_Name_Based_On_Mis, School_Name_Based_On_On_Site_Assessment, EMIS_School_ID_CBE_KEY = School_Id_Based_On_On_Site_Assessment), 
    by = c("PARENT_KEY" = "KEY")) %>% 
  mutate(
    Tool = "WASH/Ifrastructure Monitoring",
    RF = "Code of Conduct not available",
    Type = "Major"
  ) %>% 
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name',
      'Region', 
      'Province', 
      'District', 
      'Village',
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )

## Significant Negative Findings
# 16
tool2.not_received_kits <- tool2.data$data %>% 
  filter( H1 == 0 & i1 == 0 & J1 == 0 ) %>% 
  mutate(
    Tool = "Public School - Light Tool",
    RF = "School did not receive any TLM (classroom, student, and teacher kit) in the academic year",
    Type = "Significant Negative Findings"
  ) %>% 
  select(
    Site_Visit_ID, 
    EMIS_School_ID_CBE_KEY, 
    Region, Province, 
    District, 
    Village,
    School_CBE_Name,
    Tool,
    RF,
    Type,
    KEY,
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )


## 17
tool8.not_received_kits <- tool8.data$data %>% 
  filter(
    N1 == 0 & P1 == 0 & R1 == 0
  ) %>% 
  mutate(
    Tool = "CBE Class Level Tool",
    RF = "CBE did not receive any TLM (classroom, student, and teacher kit) in the academic year",
    Type = "Significant Negative Findings"
  ) %>% 
  select(
    Site_Visit_ID, 
    EMIS_School_ID_CBE_KEY, 
    Region, Province, 
    District, 
    Village,
    School_CBE_Name,
    Tool,
    RF,
    Type,
    KEY,
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )


# 18
tool8.not_received_salary <- tool8.data$data %>% 
  filter( Paid_Last_Two_Months == 0) %>% 
  mutate(
    Tool = "CBE Class Level Tool",
    RF = "CBE teacher did not receive a salary in the past two calendar months from the data collection",
    Type = "Significant Negative Findings"
  ) %>% 
  select(
    Site_Visit_ID, 
    EMIS_School_ID_CBE_KEY, 
    Region, Province, 
    District, 
    Village,
    School_CBE_Name,
    Tool,
    RF,
    Type,
    KEY,
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )


# 19
tool.infra_const_not_started <- tool.infra_monit$data %>% 
  select(-Ask_The_Reason_For_The_Temporary_Stoppage_Other) %>% 
  filter(
    Subproject_Overall_Status_Established_Condition_Sa == 18
  ) %>% 
  mutate(
    Tool = "WASH/Ifrastructure Monitoring",
    RF = "Construction not started",
    Type = "Significant Negative Findings"
  ) %>% 
  rename(EMIS_School_ID_CBE_KEY = School_Id_Based_On_On_Site_Assessment) %>% 
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name',
      'Region', 
      'Province', 
      'District', 
      'Village',
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )


# 20
tool.infra_const_stopped <- tool.infra_monit$data %>% 
  select(-Ask_The_Reason_For_The_Temporary_Stoppage_Other) %>% 
  filter(
    Subproject_Overall_Status_Established_Condition_Sa == 2
  ) %>% 
  mutate(
    Tool = "WASH/Ifrastructure Monitoring",
    RF = "Construction stopped",
    Type = "Significant Negative Findings"
  ) %>% 
  rename(EMIS_School_ID_CBE_KEY = School_Id_Based_On_On_Site_Assessment) %>% 
  select(
    any_of(c(
      'Site_Visit_ID', 
      'EMIS_School_ID_CBE_KEY', 
      'IP_Name',
      'Region', 
      'Province', 
      'District', 
      'Village',
      'School_CBE_Name',
      'Tool',
      'RF',
      'Type',
      'KEY')),
    # ends_with("Latitude"),
    # ends_with("Longitude")
  )
  

all_rfs <- plyr::rbind.fill(
  tool9.gbv_complaints,
  tool.infra_injuries,
  tool.infra_gbv_complaints,
  tool8.islamic_edu,
  tool8.grm_not_established,
  tool9.grm_not_established,
  tool9.grm_established_no_logbook,
  tool9.coc_not_available,
  tool.infra_esmp_not_available,
  tool.infra_coc_not_available,
  tool2.not_received_kits,
  tool8.not_received_kits,
  tool8.not_received_salary,
  tool.infra_const_not_started,
  tool.infra_const_stopped
) %>% 
  rename(RF_Type = RF, RF_Category = Type)

# all_rfs <- all_rfs %>% 
  # select(
  #   `Geopoint0-Latitude`, `Geopoint0-Longitude`,
  #   `Geopoint1-Latitude`, `Geopoint1-Longitude`,
  #   `Geopoint2-Latitude`, `Geopoint2-Longitude`,
  #   `Geopoint3-Latitude`, `Geopoint3-Longitude`,
  #   `Geopoint4-Latitude`, `Geopoint4-Longitude`,
  #   `Geopoint5-Latitude`, `Geopoint5-Longitude`,
  #   `Geopoint6-Latitude`, `Geopoint6-Longitude`,
  #   everything(),
  #   -`geopoint_feature-Latitude`,
  #   -`geopoint_feature-Longitude`
  # )



writexl::write_xlsx(all_rfs, paste0("./output/RFs/EERA_R4_Red_Flags_",Sys.Date(),".xlsx"))
