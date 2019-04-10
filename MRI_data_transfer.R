# MRI_data_transfer.R

# Load useful libraries

library(dplyr)
library(readxl)
library(readr)
library(stringr)


# Helpful globals and functions
# source("~/Desktop/config.R")
# source("~/Desktop/helpers.R")
source("~/Box Sync/Documents/R_helpers/config.R")
source("~/Box Sync/Documents/R_helpers/helpers.R")


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
  )) %>% 
  mutate(mri_imaging_form_complete = 2L)

write_csv(df_mri_ms_cln, 
          paste0("df_mri_ms_cln_", Sys.Date(), ".csv"),
          na = "")


## Look for redundant MRI visit records

# _ MiNDSet data

fields_ms_mri_raw <-
  c(
    "subject_id"
    , "exam_date"
    , "mri_date"
  )
fields_ms_mri <- fields_ms_mri_raw %>% paste(collapse = ",")

json_ms_mri <- rc_api_get(token = REDCAP_API_TOKEN_MINDSET,
                          fields = fields_ms_mri)
df_ms_mri <- jsonlite::fromJSON(json_ms_mri) %>% na_if("")

df_ms_mri_cln <- df_ms_mri %>% 
  filter(!is.na(exam_date)) %>% 
  filter(!is.na(mri_date)) %>% 
  arrange(subject_id, exam_date, mri_date)

# distinct(df_ms_mri_cln, subject_id, mri_date, .keep_all = TRUE)

df_ms_mri_cln$to_delete <- logical(nrow(df_ms_mri_cln))

for (i in 1:(nrow(df_ms_mri_cln)-1)) {
  # cat(i)
  if (df_ms_mri_cln[[i, "subject_id"]] == df_ms_mri_cln[[i+1, "subject_id"]] &&
      df_ms_mri_cln[[i, "mri_date"]] == df_ms_mri_cln[[i+1, "mri_date"]]) {
    df_ms_mri_cln[[i, "to_delete"]] <- TRUE
  }
}

sum(df_ms_mri_cln$to_delete)

df_ms_mri_cln_flt <- df_ms_mri_cln %>% 
  filter(to_delete)

readr::write_csv(df_ms_mri_cln_flt,
                 "MRI_visits_to_delete_in_MiNDSet.csv", na = "")


# MRI Report for Scheduling

fields_ms_mri_raw <-
  c(
    "subject_id"
    , "exam_date"
    , "mri_date"
    , "mri_completed"
    , "uds_dx"
    # , "consensus_date"
  )
fields_ms_mri <- fields_ms_mri_raw %>% paste(collapse = ",")

json_ms_mri <- rc_api_get(token = REDCAP_API_TOKEN_MINDSET,
                          fields = fields_ms_mri)
df_ms_mri <- jsonlite::fromJSON(json_ms_mri) %>% na_if("")

df_ms_mri_cln <- df_ms_mri %>% 
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>% 
  filter(subject_id >= "UM00000543") %>% 
  filter(!is.na(exam_date)) %>% 
  filter(exam_date >= lubridate::as_date("2017-03-01")) %>% 
  # filter(!is.na(mri_date)) %>% 
  arrange(subject_id, exam_date, mri_date) %>% 
  write_csv("df_ms_mri_cln.csv", na = "")


