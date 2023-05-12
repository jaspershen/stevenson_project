no_source()

setwd(masstools::get_project_wd())

rm(list = ls())

source("code/tools.R")

setwd("data_analysis/data_preparation/rplc_pos/")

load("object")

dim(object)

###remove the features who have more than 20% NA in QC samples or 50% NA in subject samples
object@sample_info$class

object %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "QC") %>%
  pull(sample_id)

qc_id <-
  object %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "QC") %>%
  pull(sample_id)
qc_id <-
  qc_id[!stringr::str_detect(qc_id, "dln")]

subject_id <-
  object %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Subject") %>%
  pull(sample_id)

object <-
  object %>%
  mutate_variable_na_freq(according_to_samples = qc_id) %>%
  mutate_variable_na_freq(according_to_samples = subject_id) %>%
  activate_mass_dataset(what = "variable_info") %>%
  filter(na_freq < 0.2 & na_freq.1 < 0.5)


##imputate MV with 0 in blank samples
blank_id <-
  object %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class == "Blank") %>%
  pull(sample_id)

object <-
  impute_mv(object = object,
            sample_id = blank_id,
            method = "zero")

####remove some samples with more than 50% NA
remove_idx <-
  object %>%
  mutate_sample_na_freq(according_to_variables = "all") %>%
  activate_mass_dataset(what = "sample_info") %>%
  pull(na_freq) %>%
  `>`(0.5) %>%
  which()
remove_idx
if (length(remove_idx) > 0) {
  object <-
    object[, -remove_idx]
}

###imputate mv
non_blank_id <-
  object %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(class != "Blank") %>%
  pull(sample_id)

object <-
  impute_mv(object, sample_id = non_blank_id, method = "knn")

object

###data normalization



