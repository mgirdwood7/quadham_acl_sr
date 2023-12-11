# Univariate Meta-Analyses for Eccentric 

library(meta)

ecc_within <- within_data %>%
  filter(study != "Tengman 2014b") %>%
  rename(acl_timepoint_mean = timepoint_mean) %>%
  mutate(n_acl_2 = as.character(as.integer(acl_n))) %>%
  filter(str_detect(measure, "ecc"),
         str_detect(measure, "quad|hs")) %>%
  mutate(measure_2 = case_when(
    str_detect(measure, "150|180|230|240|300") ~ "Fast Isokinetic",
    str_detect(measure, "30|60|90") ~ "Slow Isokinetic",
    str_detect(measure, "nordboard") ~ "Nordboard",
  )) %>%
  mutate(measure_2 = case_when(
    acl_timepoint_mean <= 12 & measure_2 == "Slow Isokinetic" ~ "Slow Isokinetic <= 12 months",
    acl_timepoint_mean > 12 & measure_2 == "Slow Isokinetic" ~ "Slow Isokinetic > 12 months",
    TRUE ~ measure_2),
    measure_2 = factor(measure_2, levels = c("Slow Isokinetic <= 12 months", "Slow Isokinetic > 12 months", 
                                            "Fast Isokinetic", "Nordboard"))
  )

ecc_within$sei[ecc_within$study == "Gauthier 2022"] <- 0.01209894


forest_cc_function <- function(model, ...){
  forest.meta(model,
              sortvar = acl_timepoint_mean,
              common = FALSE,
              prediction = FALSE,
              at = c(0.6, 0.8, 1, 1.2),
              #xlab = label,
              #xlab.pos = xlab,
              xlim = c(0.5, 1.3),
              smlab = "",
              leftcols = c("study", "acl_timepoint_mean", "n_acl_2"),
              leftlabs = c("Study", "Months\npost ACLR", "n\nACLR"),
              rightcols = c("effect.ci"),
              rightlabs = c("RoM [95% CI]"),
              just.addcols = "left",
              addrows.below.overall = 0,
              digits.addcols = 1,
              print.pval.Q = FALSE,
              fontfamily = "Karla",
              ff.predict = 1,
              ref = 1,
              col.diamond = "black",
              print.subgroup.labels = TRUE, 
              subgroup.name = "Type",
              test.subgroup = FALSE, 
              #subgroup.hetstat = FALSE, 
              col.random = "grey",
              ...)
}


hs_ecc_meta <- ecc_within %>%
  filter(str_detect(measure, "hs")) %>%
  metagen(TE = yi, seTE = sei, studlab = study, data = ., subgroup = measure_2, sm = "ROM")

forest_cc_function(hs_ecc_meta, overall = FALSE, prediction.subgroup = TRUE, weight.study = "same", hetstat = FALSE)

quad_ecc_meta <- ecc_within %>%
  filter(str_detect(measure, "quad")) %>%
  metagen(TE = yi, seTE = sei, studlab = study, data = ., subgroup = measure_2, sm = "ROM")

forest_cc_function(quad_ecc_meta, overall = FALSE, prediction.subgroup = TRUE, weight.study = "same", hetstat = FALSE)



ecc_casecontrol <- casecontrol %>%
  mutate(n_acl_2 = as.character(as.integer(acl_n))) %>%
  filter(str_detect(measure, "ecc"),
         str_detect(measure, "quad|hs")) %>%
  mutate(measure_2 = case_when(
    str_detect(measure, "150|180|230|240|300") ~ "Fast Isokinetic",
    str_detect(measure, "30|60|90") ~ "Slow Isokinetic",
    str_detect(measure, "nordboard") ~ "Nordboard",
  )) %>%
  mutate(measure_2 = case_when(
    str_detect(measure, "quad") ~ paste0("Quadriceps - ", measure_2),
    str_detect(measure, "hs") ~ paste0("Hamstrings - ", measure_2)
  ))


ecc_casecontrol_meta <- ecc_casecontrol %>%
  metagen(TE = yi, seTE = sei, studlab = study, data = ., subgroup = measure_2, sm = "ROM")

forest_cc_function(ecc_casecontrol_meta, overall = FALSE, prediction.subgroup = TRUE, weight.study = "same", hetstat = FALSE)

