library(atRfunctions)

replace_values <- function(values, list_name, options) {
  sapply(values, function(value) {
    if (is.na(value)) return(NA_character_)
    matches <- options$value[options$list_name == list_name & options$label == value]
    if (length(matches) == 0) return(NA_character_)
    return(matches)
  })
}


# OLD
old_t1 <- read_xlsx_sheets("./archive/archive/Tool1_EERA_R4_PS_Headmaster_clean_12182024.xlsx")
old_t2 <- read_xlsx_sheets("./archive/archive/Tool2_EERA_R4_PS_Light_Tool_clean_12182024.xlsx")
old_t3 <- read_xlsx_sheets("./archive/archive/Tool3_EERA_R4_PS_Headcount_clean_12182024.xlsx")
old_t4 <- read_xlsx_sheets("./archive/archive/Tool4_EERA_R4_PS_Teacher_Tool_clean_12182024.xlsx")
old_t5 <- read_xlsx_sheets("./archive/archive/Tool5_EERA_R4_PS_WASH_Observation_clean_12182024.xlsx")
old_t6 <- read_xlsx_sheets("./archive/archive/Tool6_EERA_R4_PS_Parent_Tool_clean_12182024.xlsx")
old_t7 <- read_xlsx_sheets("./archive/archive/Tool7_EERA_R4_PS_Shura_Tool_clean_12182024.xlsx")


# Updated
new_t1 <- read_xlsx_sheets("./archive/Tool1_EERA_R4_PS_Headmaster_clean.xlsx")
new_t2 <- read_xlsx_sheets("./archive/Tool2_EERA_R4_PS_Light_Tool_clean.xlsx")
new_t3 <- read_xlsx_sheets("./archive/Tool3_EERA_R4_PS_Headcount_clean.xlsx")
new_t4 <- read_xlsx_sheets("./archive/Tool4_EERA_R4_PS_Teacher_Tool_clean.xlsx")
new_t5 <- read_xlsx_sheets("./archive/Tool5_EERA_R4_PS_WASH_Observation_clean.xlsx")
new_t6 <- read_xlsx_sheets("./archive/Tool6_EERA_R4_PS_Parent_Tool_clean.xlsx")
new_t7 <- read_xlsx_sheets("./archive/Tool7_EERA_R4_PS_Shura_Tool_clean.xlsx")


# Tools
kobo_t1 <- read_xlsx_sheets("./input/tools/Tool 1.EERA Public School - Headmaster_Principle Interview - R4.xlsx")
kobo_t2 <- read_xlsx_sheets("./input/tools/Tool 2.EERA Public School - Light Tool - R4.xlsx")
kobo_t3 <- read_xlsx_sheets("./input/tools/Tool 3.EERA Public School - Student Document & Headcount - R4.xlsx")
kobo_t4 <- read_xlsx_sheets("./input/tools/Tool 4.EERA Public School - Teacher Tool - R4.xlsx")
kobo_t5 <- read_xlsx_sheets("./input/tools/Tool 5.EERA Public School - WASH Observation - R4.xlsx")
kobo_t6 <- read_xlsx_sheets("./input/tools/Tool 6.EERA Public School_CBE - Parent Tool - R4.xlsx")
kobo_t7 <- read_xlsx_sheets("./input/tools/Tool 7.EERA Public School_CBE - Shura Tool - R4.xlsx")

# Tool1 
difference_log <- data.frame()
# sheet = "data"
for(sheet in names(old_t1)){
  difference_log <- rbind(
    difference_log,
    compare_dt(df1 = old_t1[[sheet]], df2 = new_t1[[sheet]], unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>% 
      mutate(Tab_name = sheet, Tool = "Tool 1 - Headmaster") %>% 
      left_join(new_t1[[sheet]] %>% select(KEY, Province), by = "KEY")
  )
}

#Tool2
for(sheet in names(old_t2)){
  difference_log <- rbind(
    difference_log,
    compare_dt(df1 = old_t2[[sheet]], df2 = new_t2[[sheet]], unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>% 
      mutate(Tab_name = sheet, Tool = "Tool 2 - Light") %>% 
      left_join(new_t2[[sheet]] %>% select(KEY, Province), by = "KEY")
  )
}

#Tool3
for(sheet in names(old_t3)){
  difference_log <- rbind(
    difference_log,
    compare_dt(df1 = old_t3[[sheet]], df2 = new_t3[[sheet]], unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>% 
      mutate(Tab_name = sheet, Tool = "Tool 3 - Headcount") %>% 
      left_join(new_t3[[sheet]] %>% select(KEY, Province), by = "KEY")
  )
}

#Tool4
for(sheet in names(old_t4)){
  difference_log <- rbind(
    difference_log,
    compare_dt(df1 = old_t4[[sheet]], df2 = new_t4[[sheet]], unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>% 
      mutate(Tab_name = sheet, Tool = "Tool 4 - Teacher") %>% 
      left_join(new_t4[[sheet]] %>% select(KEY, Province), by = "KEY")
  )
}

#Tool5
for(sheet in names(old_t5)){
  difference_log <- rbind(
    difference_log,
    compare_dt(df1 = old_t5[[sheet]], df2 = new_t5[[sheet]], unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>% 
      mutate(Tab_name = sheet, Tool = "Tool 5 - WASH") %>% 
      left_join(new_t5[[sheet]] %>% select(KEY, Province), by = "KEY")
  )
}

#Tool6
for(sheet in names(old_t6)){
  difference_log <- rbind(
    difference_log,
    compare_dt(df1 = old_t6[[sheet]], df2 = new_t6[[sheet]], unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>% 
      mutate(Tab_name = sheet, Tool = "Tool 6 - Parent") %>% 
      left_join(new_t6[[sheet]] %>% select(KEY, Province), by = "KEY")
  )
}

#Tool7
for(sheet in names(old_t7)){
  difference_log <- rbind(
    difference_log,
    compare_dt(df1 = old_t7[[sheet]], df2 = new_t7[[sheet]], unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>% 
      mutate(Tab_name = sheet, Tool = "Tool 7 - Shura") %>% 
      left_join(new_t7[[sheet]] %>% select(KEY, Province), by = "KEY")
  )
}

# writexl::write_xlsx(difference_log, "./output/Changes_log_harmonization of other specifiers.xlsx")


difference_log_filter <- difference_log %>% 
  filter(!((is.na(old_value) & new_value %in% c("1", "0", 1, 0)) | (old_value %in% c("1", "0", 1, 0)))) %>% # Filter out binary questions
  filter(!str_detect(question, "_Other")) # Filter out other specifiers

difference_log_filter_r <- difference_log %>% 
  filter(str_detect(question, "_Other")) # ((is.na(old_value) & new_value %in% c("1", "0", 1, 0)) | (old_value %in% c("1", "0", 1, 0))) |

# tools <- unique(difference_log_filter$Tool)

# Tool 1
dd_t1 <- difference_log_filter %>% 
  filter(Tool == "Tool 1 - Headmaster") %>% 
  left_join(
    kobo_t1$survey %>% select(name, type), by = c("question" = "name")
  ) %>% 
  separate(type, into = c("type", "list_name"), sep = " ")

kobo_t1_ch <- kobo_t1$choices %>% 
  select(list_name, value, label) %>% 
  filter(!is.na(list_name)) %>%
  mutate(
    value = as.numeric(value),
    value = as.character(value)
    ) %>% 
  filter(!is.na(value))

dd_t1 <- dd_t1 %>% 
  rowwise() %>% 
  mutate(
    old_value_o = old_value,
    new_value_o = new_value,
    old_value = paste(replace_values(strsplit(old_value, ";")[[1]], list_name, kobo_t1_ch), collapse = ";"),
    new_value = paste(replace_values(strsplit(new_value, ";")[[1]], list_name, kobo_t1_ch), collapse = ";")
  ) %>% 
  relocate(old_value_o, .after = old_value) %>% 
  relocate(new_value_o, .after = new_value)

# write.xlsx(dd_t1, "./output/labeltovalue_test.xlsx")
  
# Tool 2
dd_t2 <- difference_log_filter %>% 
  filter(Tool == "Tool 2 - Light") %>% 
  left_join(
    kobo_t2$survey %>% select(name, type), by = c("question" = "name")
  ) %>% 
  separate(type, into = c("type", "list_name"), sep = " ")

kobo_t2_ch <- kobo_t2$choices %>% 
  select(list_name, value, label) %>% 
  filter(!is.na(list_name)) %>%
  mutate(
    value = as.numeric(value),
    value = as.character(value)
  ) %>% 
  filter(!is.na(value))

dd_t2_filtered <- dd_t2 %>% 
  filter(!type %in% c("select_one", "select_multiple"))

dd_t2 <- dd_t2 %>% 
  filter(type %in% c("select_one", "select_multiple")) %>% 
  rowwise() %>% 
  mutate(
    old_value_o = old_value,
    new_value_o = new_value,
    old_value = paste(replace_values(strsplit(old_value, ";")[[1]], list_name, kobo_t2_ch), collapse = ";"),
    new_value = paste(replace_values(strsplit(new_value, ";")[[1]], list_name, kobo_t2_ch), collapse = ";")
  ) %>% 
  relocate(old_value_o, .after = old_value) %>% 
  relocate(new_value_o, .after = new_value)

dd_t2 <- bind_rows(dd_t2, dd_t2_filtered)


# Tool 3
dd_t3 <- difference_log_filter %>% 
  filter(Tool == "Tool 3 - Headcount") %>% 
  left_join(
    kobo_t3$survey %>% select(name, type), by = c("question" = "name")
  ) %>% 
  separate(type, into = c("type", "list_name"), sep = " ")

kobo_t3_ch <- kobo_t3$choices %>% 
  select(list_name, value, label) %>% 
  filter(!is.na(list_name)) %>%
  mutate(
    value = as.numeric(value),
    value = as.character(value)
  ) %>% 
  filter(!is.na(value))


dd_t3 <- dd_t3 %>% 
  filter(type %in% c("select_one", "select_multiple")) %>% 
  rowwise() %>% 
  mutate(
    old_value_o = old_value,
    new_value_o = new_value,
    old_value = paste(replace_values(strsplit(old_value, ";")[[1]], list_name, kobo_t3_ch), collapse = ";"),
    new_value = paste(replace_values(strsplit(new_value, ";")[[1]], list_name, kobo_t3_ch), collapse = ";")
  ) %>% 
  relocate(old_value_o, .after = old_value) %>% 
  relocate(new_value_o, .after = new_value)

# Tool 4
dd_t4 <- difference_log_filter %>% 
  filter(Tool == "Tool 4 - Teacher") %>% 
  left_join(
    kobo_t4$survey %>% select(name, type), by = c("question" = "name")
  ) %>% 
  separate(type, into = c("type", "list_name"), sep = " ")

kobo_t4_ch <- kobo_t4$choices %>% 
  select(list_name, value, label) %>% 
  filter(!is.na(list_name)) %>%
  mutate(
    value = as.numeric(value),
    value = as.character(value)
  ) %>% 
  filter(!is.na(value))

dd_t4_filtered <- dd_t4 %>% 
  filter(!type %in% c("select_one", "select_multiple"))

dd_t4 <- dd_t4 %>% 
  filter(type %in% c("select_one", "select_multiple")) %>% 
  rowwise() %>% 
  mutate(
    old_value_o = old_value,
    new_value_o = new_value,
    old_value = paste(replace_values(strsplit(old_value, ";")[[1]], list_name, kobo_t4_ch), collapse = ";"),
    new_value = paste(replace_values(strsplit(new_value, ";")[[1]], list_name, kobo_t4_ch), collapse = ";")
  ) %>% 
  relocate(old_value_o, .after = old_value) %>% 
  relocate(new_value_o, .after = new_value)

dd_t4 <- bind_rows(dd_t4, dd_t4_filtered)


# Tool 5
dd_t5 <- difference_log_filter %>% 
  filter(Tool == "Tool 5 - WASH") %>% 
  left_join(
    kobo_t5$survey %>% select(name, type), by = c("question" = "name")
  ) %>% 
  separate(type, into = c("type", "list_name"), sep = " ")

kobo_t5_ch <- kobo_t5$choices %>% 
  select(list_name, value, label) %>% 
  filter(!is.na(list_name)) %>%
  mutate(
    value = as.numeric(value),
    value = as.character(value)
  ) %>% 
  filter(!is.na(value))

dd_t5_filtered <- dd_t5 %>% 
  filter(!type %in% c("select_one", "select_multiple"))

dd_t5 <- dd_t5 %>% 
  filter(type %in% c("select_one", "select_multiple")) %>% 
  rowwise() %>% 
  mutate(
    old_value_o = old_value,
    new_value_o = new_value,
    old_value = paste(replace_values(strsplit(old_value, ";")[[1]], list_name, kobo_t5_ch), collapse = ";"),
    new_value = paste(replace_values(strsplit(new_value, ";")[[1]], list_name, kobo_t5_ch), collapse = ";")
  ) %>% 
  relocate(old_value_o, .after = old_value) %>% 
  relocate(new_value_o, .after = new_value)

# Tool 6
dd_t6 <- difference_log_filter %>% 
  filter(Tool == "Tool 6 - Parent") %>% 
  left_join(
    kobo_t6$survey %>% select(name, type), by = c("question" = "name")
  ) %>% 
  separate(type, into = c("type", "list_name"), sep = " ")

kobo_t6_ch <- kobo_t6$choices %>% 
  select(list_name, value, label) %>% 
  filter(!is.na(list_name)) %>%
  mutate(
    value = as.numeric(value),
    value = as.character(value)
  ) %>% 
  filter(!is.na(value))

dd_t6_filtered <- dd_t6 %>% 
  filter(!type %in% c("select_one", "select_multiple"))

dd_t6 <- dd_t6 %>% 
  filter(type %in% c("select_one", "select_multiple")) %>% 
  rowwise() %>% 
  mutate(
    old_value_o = old_value,
    new_value_o = new_value,
    old_value = paste(replace_values(strsplit(old_value, ";")[[1]], list_name, kobo_t6_ch), collapse = ";"),
    new_value = paste(replace_values(strsplit(new_value, ";")[[1]], list_name, kobo_t6_ch), collapse = ";")
  ) %>% 
  relocate(old_value_o, .after = old_value) %>% 
  relocate(new_value_o, .after = new_value)

# Tool 7
dd_t7 <- difference_log_filter %>% 
  filter(Tool == "Tool 7 - Shura") %>% 
  left_join(
    kobo_t7$survey %>% select(name, type), by = c("question" = "name")
  ) %>% 
  separate(type, into = c("type", "list_name"), sep = " ")

kobo_t7_ch <- kobo_t7$choices %>% 
  select(list_name, value, label) %>% 
  filter(!is.na(list_name)) %>%
  mutate(
    value = as.numeric(value),
    value = as.character(value)
  ) %>% 
  filter(!is.na(value))


dd_t7 <- dd_t7 %>% 
  filter(type %in% c("select_one", "select_multiple")) %>% 
  rowwise() %>% 
  mutate(
    old_value_o = old_value,
    new_value_o = new_value,
    old_value = paste(replace_values(strsplit(old_value, ";")[[1]], list_name, kobo_t7_ch), collapse = ";"),
    new_value = paste(replace_values(strsplit(new_value, ";")[[1]], list_name, kobo_t7_ch), collapse = ";")
  ) %>% 
  relocate(old_value_o, .after = old_value) %>% 
  relocate(new_value_o, .after = new_value)

all_so_sm <- bind_rows(
  dd_t1,
  dd_t2,
  dd_t3,
  dd_t4,
  dd_t5,
  dd_t6,
  dd_t7
) %>% 
  mutate(
    old_value = str_replace_all(old_value, ";", " "),
    new_value = str_replace_all(new_value, ";", " "),
    Sample_Type = "Public School"
  )


all_logs_final <- bind_rows(
  all_so_sm,
  difference_log_filter_r
) %>% 
select(-old_value_o, -new_value_o)

write.xlsx(difference_log, "./output/harmonization/difference_log_harmonization of other specifiers.xlsx")
write.xlsx(all_so_sm, "./output/harmonization/excluding others and binary - label to value converted.xlsx")
write.xlsx(difference_log_filter_r, "./output/harmonization/difference_log_harmonization_only other specifiers.xlsx")
write.xlsx(all_logs_final, "./output/harmonization/difference_log_harmonization_ready for apply.xlsx")
write.xlsx(all_logs_final, "./output/harmonization/difference_log_harmonization_ready for apply.xlsx")
