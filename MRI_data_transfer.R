# MRI_data_transfer.R

# Load useful libraries

library(dplyr)
library(readxl)
library(readr)
library(stringr)


# Helpful globals and functions
source("~/Desktop/config.R")
source("~/Desktop/helpers.R")


# Get Data

# _ MRI data

df_mri_1 <-
  read_excel("UMMAP_MRI_Tracker_MostRecent_2019-04-01.xlsx",
             sheet = 1)
# df_mri_2 <-
#   read_excel("UMMAP_MRI_Tracker_MostRecent_2019-04-01.xlsx",
#              sheet = 2)

# _ MiNDSet data

fields_ms_raw <-
  c(
    "subject_id"
    , "exam_date"
  )
fields_ms <- fields_ms_raw %>% paste(collapse = ",")

json_ms <- rc_api_get(token = REDCAP_API_TOKEN_MINDSET,
                      fields = fields_ms)
df_ms <- jsonlite::fromJSON(json_ms) %>% na_if("")


# Clean Data

df_mri_1_cln <- df_mri_1 %>% 
  select(mri_sub_id    = fMRI_ID,
         seq_num       = fMRI_Exam_No,
         ra_name       = Handler,
         mri_date      = Scan_Date,
         server_status = Server_Status,
         reason_not    = Notes) %>% 
  filter(!is.na(mri_sub_id)) %>% 
  mutate(subject_id = paste0("UM0000", str_sub(mri_sub_id, -4))) %>% 
  mutate(mri_date = as.Date(mri_date)) %>% 
  mutate(mri_completed = case_when(
    !is.na(mri_date) ~ 1L,
    TRUE ~ NA_integer_
  ))

# df_mri_2_cln <- df_mri_2 %>% 
#   select(mri_sub_id = fMRI_ID,
#          mri_date = Scan_Date,
#          notes_2 = `Notes!!`) %>% 
#   filter(!is.na(mri_sub_id))

# df_mri_cln <-
#   full_join(df_mri_1_cln, df_mri_2_cln,
#             by = c("mri_sub_id" = "mri_sub_id",
#                    "mri_date" = "mri_date"))

df_ms_cln <- df_ms %>% 
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>% 
  filter(subject_id >= "UM00000543") %>% 
  filter(!is.na(exam_date)) %>% 
  mutate(exam_date = as.Date(exam_date))


# Process Data

df_mri_ms <- 
  FuzzyDateJoin::outer_left(x = df_mri_1_cln, y = df_ms_cln,
                            x_id_col = "subject_id", y_id_col = "subject_id",
                            x_date_col = "mri_date", y_date_col = "exam_date",
                            x_intvl_less = 365L, x_intvl_more = 0L,
                            keep_y_id = FALSE) %>% 
  select(subject_id = subject_id_x, 
         exam_date, 
         mri_date, 
         redcap_event_name, 
         everything())

# Fix Errors 

# UM00001340
df_mri_ms[which(df_mri_ms$subject_id == "UM00001340"), 
          "redcap_event_name"] <-
  "baseline_arm_1"

# UM00001373
df_mri_ms[which(df_mri_ms$subject_id == "UM00001373"), 
          "redcap_event_name"] <-
  "baseline_arm_1"

df_mri_ms_cln <- df_mri_ms %>% 
  filter(!is.na(redcap_event_name)) %>% 
  select(-exam_date) %>% 
  mutate(server_status = case_when(
    server_status == "On_Server" ~ 1L,
    TRUE ~ NA_integer_
  ))

write_csv(df_mri_ms_cln, 
          paste0("df_mri_ms_cln_", Sys.Date(), ".csv"),
          na = "")










