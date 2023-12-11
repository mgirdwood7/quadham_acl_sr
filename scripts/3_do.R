library(metafor)
library(clubSandwich)
library(rms)
library(mgfunctions)

within_data <- read_csv("data/processed/within_data.csv") %>%
  filter(!study %in% c("Geoghegan 2007", "Karanikas 2004", "Karanikas 2005", "Nicholas 2001",
                       "Urabe 2002", "Tate 2017", "Dalton 2011", "Hall 2015", "Noehren 2014", "Rahova 2020",
                       "Thomas 2013", "Balki 2019", "Rhatomy", "Sullivan 2022"))

# filtering studies that were included in hip review (where no limit on publication year), but need to remove forthis review
casecontrol <- read_csv("data/processed/casecontrol.csv") %>%
  filter(!study %in% c("Geoghegan 2007", "Karanikas 2004", "Karanikas 2005", "Nicholas 2001",
                       "Urabe 2002", "Tate 2017", "Dalton 2011", "Hall 2015", "Noehren 2014", "Rahova 2020",
                       "Thomas 2013", "Balki 2019", "Rhatomy 2019", "Sullivan 2022"))

quad <- within_data %>%
  rename(acl_graft_group = graft_group) %>% 
  filter(str_detect(measure, "quad")) %>% # take all quad based outcomes
  filter(timepoint_mean >2, # remove pre-operative data
         timepoint_mean < 120, # remove >10 year data
         str_detect(graft, "contralateral|Contralateral", negate = TRUE)) %>%  # remove any contralateral grafts
  mutate(es_id = row_number()) %>% # effect size id number for use in random effects
  mutate(measure_2 = case_when( # classify different outcomes into subgroups:
    str_detect(measure, "mvic|hhd") ~ "Isometric",
    str_detect(measure, "isk con 180|isk con 230|isk con 240|isk con 300") ~ "Fast Isokinetic",
    str_detect(measure, "isk con 30|isk con 60|isk con 90|isk con 120") ~ "Slow Isokinetic"
  )) %>%
  filter(!is.na(measure_2)) 

# Reducing timepoint down to categories (3, 6, 12, 24, 48, 96 months post)
# not using this at the moment but keeping code here for now
quad_cat <- quad %>%
  mutate(timepoint_cut = cut(timepoint_mean, 
                             breaks = c(0, 4.5, 9, 18, 36, 72, Inf), 
                             labels = c(3, 6, 12, 24, 48, 96))) 
#group_by(cohort, timepoint_cut, group, measure_2) %>%
# if multiple timepoints allocated to same category, take the closest to the categorical timepoint
#slice(which.min(abs(timepoint_mean - as.numeric(as.character(timepoint_cut))))) %>% 
#ungroup()

# separate subgroups of data based on contraction type
# Some studies report multiple outcomes in the same subgroup
# Case by case removal of data 
# Generally:
# for fast if they have isk con 180 use that if possible, otherwise use speed closest to this.
# for slow use isk con 60, or 90

fastdata <- quad_cat %>% filter(measure_2 == "Fast Isokinetic") %>%
  filter(!(cohort == "Bailey 2019" & measure == "quad isk con 300"),
         !(cohort == "Drocco 2017" & measure == "quad isk con 240"),
         !(cohort == "Kyritsis 2016" & measure == "quad isk con 300"),
         !(cohort == "Laudner 2015" & measure == "quad isk con 300"),
         !(cohort == "Tourville 2014" & measure == "quad isk con 300"),
         !(cohort == "Welling 2020" & measure == "quad isk con 300"),
         !(cohort == "Welling 2018b" & measure == "quad isk con 300")
  ) 
slowdata <- quad_cat %>% filter(measure_2 == "Slow Isokinetic") %>%
  filter(!(cohort == "Li 2022" & measure == "quad isk con 30"),
         !(cohort == "Siney 2010" & measure == "quad isk con 90"),
         !(cohort == "Zult 2017" & measure == "quad isk con 120"),
         !(cohort == "Pamukoff 2018" & measure == "quad isk con 120"))
isodata <- quad_cat %>% filter(measure_2 == "Isometric") %>%
  filter(!(cohort == "Karanikas 2005" & measure == "quad mvic 0"),
         !(cohort == "Labanca 2018" & measure == "quad mvic 30"),
         !(cohort == "Labanca 2016" & measure == "quad mvic 30"),
         !(cohort == "Wongcharoenwatana 2019" & measure == "quad mvic 90 hhd prone"))


# impute covariance matrix, with help of clubSandwich package
# Assuming a correlation of 0.85

fastV <- impute_covariance_matrix(vi = fastdata$vi, 
                                  cluster = fastdata$cohort, # cluster is cohort (not study)
                                  ti = fastdata$timepoint_mean, # timepoint 
                                  ar1 = 0.85, # auto-correlation between timepoints
                                  check_PD = TRUE, # check positive definite afterwards
                                  smooth_vi = TRUE,
                                  return_list = FALSE) # return the full matrix

slowV <- impute_covariance_matrix(vi = slowdata$vi, 
                                  cluster = slowdata$cohort,
                                  ti = slowdata$timepoint_mean,
                                  ar1 = 0.85,
                                  smooth_vi = TRUE,
                                  return_list = FALSE)

isoV <- impute_covariance_matrix(vi = isodata$vi, 
                                 cluster = isodata$cohort,
                                 ti = isodata$timepoint_mean,
                                 ar1 = 0.85,
                                 check_PD = TRUE,
                                 smooth_vi = TRUE,
                                 return_list = FALSE)

# Base models no moderators

slow_mv_empty <- rma.mv(yi, slowV, 
                        data = slowdata, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

fast_mv_empty <- rma.mv(yi, fastV, 
                        data = fastdata, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

iso_mv_empty <- rma.mv(yi, isoV, 
                       data = isodata, 
                       random = list(~ timepoint_mean|cohort),
                       struct = c("CAR"))


# 4 knot spline best fit.
slow_mv <- rma.mv(yi, slowV, 
                  mods = ~rcs(timepoint_mean, 4), 
                  data = slowdata, 
                  random = list(~ timepoint_mean|cohort),
                  struct = c("CAR"))

# 4 knot spline best fit
fast_mv <- rma.mv(yi, fastV, 
                  mods = ~rcs(timepoint_mean, 4), 
                  data = fastdata, 
                  random = list(~ timepoint_mean|cohort),
                  struct = c("CAR"))

# log timepoint best fit, rcs(4) is visually more pleasing?
iso_mv <- rma.mv(yi, isoV, 
                 mods = ~rcs(timepoint_mean, 4), 
                 data = isodata, 
                 random = list(~ timepoint_mean|cohort),
                 struct = c("CAR"))

iso_mv <- rma.mv(yi, isoV, 
                 mods = ~rcs(timepoint_mean, 4), 
                 data = isodata, 
                 random = list(~ timepoint_mean|cohort),
                 struct = c("CAR"))


###################
### Hamstring #####
###################


# Data for Hamstring
hs <- within_data %>%
  rename(acl_graft_group = graft_group) %>%
  filter(str_detect(measure, "hs")) %>%
  filter(timepoint_mean >2) %>% # no pre-operative data
  mutate(es_id = row_number()) %>%
  mutate(measure_2 = case_when(
    str_detect(measure, "mvic|hhd") ~ "Isometric",
    str_detect(measure, "isk con 180|isk con 230|isk con 240|isk con 300") ~ "Fast Isokinetic",
    str_detect(measure, "isk con 30|isk con 60|isk con 90|isk con 120") ~ "Slow Isokinetic"
  )) %>%
  filter(!is.na(measure_2))

# Reducing timepoint down to categories (3, 6, 12, 24, 48, 96 months post)
hs_cat <- hs %>%
  mutate(timepoint_cut = cut(timepoint_mean, 
                             breaks = c(0, 4.5, 9, 18, 36, 72, Inf), 
                             labels = c(3, 6, 12, 24, 48, 96))) 
  #group_by(cohort, timepoint_cut, measure_2) %>%
  # if multiple timepoints allocated to same category, take the closest to the categorical timepoint
  #slice(which.min(abs(timepoint_mean - as.numeric(as.character(timepoint_cut))))) %>% 
  #ungroup()

hs_fastdata <- hs_cat %>% filter(measure_2 == "Fast Isokinetic") %>%
  filter(!(cohort == "Drocco 2017" & measure == "hs isk con 240"),
         !(cohort == "Kyritsis 2016" & measure == "hs isk con 300"),
         !(cohort == "Laudner 2015" & measure == "hs isk con 300"),
         !(cohort == "Tourville 2014" & measure == "hs isk con 300"),
         !(cohort == "Welling 2020" & measure == "hs isk con 300"),
         !(cohort == "Welling 2018b" & measure == "hs isk con 300"))
hs_slowdata <- hs_cat %>% filter(measure_2 == "Slow Isokinetic") %>%
  filter(!(cohort == "Lee 2019" & measure == "hs isk con 60 - deep"),
         !(cohort == "Kim 2011" & measure == "hs isk con 90 hyperflex"),
         !(cohort == "Siney 2010" & measure == "hs isk con 90"),
         !(cohort == "McRae 2013" & measure == "hs isk con 60 supine"),
         !(cohort == "Li 2022" & measure == "hs isk con 30"),
         !(cohort == "Jiang 2012" & measure == "hs isk con 120"),
         !(cohort == "Zult 2017" & measure == "hs isk con 120")
         )
hs_isodata <- hs_cat %>% filter(measure_2 == "Isometric") %>%
  filter(!(cohort == "Hu 2020" & measure == "hs mvic 30 hhd"),
         !(cohort == "Hu 2020" & measure == "hs mvic 60 hhd"),
         !(cohort == "Ardern 2010" & measure == "hs mvic 30"),
         !(cohort == "Ardern 2010" & measure == "hs mvic 105"))


hs_fastV <- impute_covariance_matrix(vi = hs_fastdata$vi, 
                                  cluster = hs_fastdata$cohort,
                                  ti = hs_fastdata$timepoint_mean,
                                  ar1 = 0.85,
                                  check_PD = TRUE,
                                  smooth_vi = TRUE, 
                                  return_list = FALSE)

hs_slowV <- impute_covariance_matrix(vi = hs_slowdata$vi, 
                                  cluster = hs_slowdata$cohort,
                                  ti = hs_slowdata$timepoint_mean,
                                  ar1 = 0.85,
                                  smooth_vi = TRUE,
                                  return_list = FALSE)

hs_isoV <- impute_covariance_matrix(vi = hs_isodata$vi, 
                                 cluster = hs_isodata$cohort,
                                 ti = hs_isodata$timepoint_mean,
                                 ar1 = 0.85,
                                 check_PD = TRUE,
                                 smooth_vi = TRUE,
                                 return_list = FALSE)

# Base plots no moderators

hs_fast_mv_empty <- rma.mv(yi, hs_fastV, 
                        data = hs_fastdata, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

hs_slow_mv_empty <- rma.mv(yi, hs_slowV, 
                        data = hs_slowdata, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

hs_iso_mv_empty <- rma.mv(yi, hs_isoV, 
                       data = hs_isodata, 
                       random = list(~ timepoint_mean|cohort),
                       struct = c("CAR"))

# 4 knot spline best fit
hs_fast_mv <- rma.mv(yi, hs_fastV, 
                     mods = ~rcs(timepoint_mean, 4),
                     data = hs_fastdata, 
                     random = list(~ timepoint_mean|cohort),
                     struct = c("CAR"))

# log timepoint best fit - 3 knot spline also close?
hs_iso_mv <- rma.mv(yi, hs_isoV, 
                     mods = ~log(timepoint_mean),
                     data = hs_isodata, 
                     random = list(~ timepoint_mean|cohort),
                     struct = c("CAR"))

# 4 knot spline also close?
hs_slow_mv <- rma.mv(yi, hs_slowV, 
                    mods = ~rcs(timepoint_mean, 4),
                    data = hs_slowdata, 
                    random = list(~ timepoint_mean|cohort),
                    struct = c("CAR"))


####################
### Case Control ###
####################


quadcont <- casecontrol %>%
  filter(str_detect(measure, "quad")) %>%
  mutate(es_id = row_number(),
         timepoint_mean = acl_timepoint_mean,
         group = 1) %>%
  filter(timepoint_mean >2, # no pre-operative data
         timepoint_mean < 120) %>%
  mutate(measure_2 = case_when(
    str_detect(measure, "mvic|hhd") ~ "Isometric",
    str_detect(measure, "isk con 180|isk con 230|isk con 240|isk con 300") ~ "Fast Isokinetic",
    str_detect(measure, "isk con 30|isk con 60|isk con 90|isk con 120") ~ "Slow Isokinetic"
  )) %>%
  filter(!is.na(measure_2)) %>%
  #filter(timepoint_mean < 100) %>%
  filter(!(cohort == "Laudner 2015" & measure == "quad isk con 300"), # remove where multiple effect sizes in same measure group
         !(cohort == "Pamukoff 2018" & measure == "quad isk con 120"),
         !(cohort == "Tourville 2014" & measure == "quad isk con 300"),
         !(cohort == "Zult 2017" & measure == "quad isk con 120"))

# Reducing timepoint down to categories (3, 6, 12, 24, 48  months post)
quadcont_cat <- quadcont %>%
  mutate(timepoint_cut = cut(timepoint_mean, 
                             breaks = c(0, 4.5, 9, 18, 36, 72, Inf), 
                             labels = c(3, 6, 12, 24, 48, 96)))
  #group_by(cohort, timepoint_cut, measure) %>%
  #slice(which.min(abs(timepoint_mean - as.numeric(as.character(timepoint_cut))))) %>%
  #ungroup()

fastcont <- quadcont_cat %>% filter(measure_2 == "Fast Isokinetic")
slowcont <- quadcont_cat %>% filter(measure_2 == "Slow Isokinetic")
isocont <- quadcont_cat %>% filter(measure_2 == "Isometric")

qcont_fastV <- impute_covariance_matrix(vi = fastcont$vi, 
                                        cluster = fastcont$cohort,
                                        ti = fastcont$timepoint_mean,
                                        ar1 = 0.85,
                                        check_PD = TRUE,
                                        return_list = FALSE)

qcont_slowV <- impute_covariance_matrix(vi = slowcont$vi, 
                                        cluster = slowcont$cohort,
                                        ti = slowcont$timepoint_mean,
                                        ar1 = 0.85,
                                        check_PD = TRUE,
                                        return_list = FALSE)

qcont_isoV <- impute_covariance_matrix(vi = isocont$vi, 
                                       cluster = isocont$cohort,
                                       ti = isocont$timepoint_mean,
                                       ar1 = 0.85,
                                       check_PD = TRUE,
                                       return_list = FALSE)
## Empty model


qcont_fast_mv_empty <- rma.mv(yi, qcont_fastV, 
                        data = fastcont, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

qcont_slow_mv_empty <- rma.mv(yi, qcont_slowV, 
                        data = slowcont, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

qcont_iso_mv_empty <- rma.mv(yi, qcont_isoV, 
                       data = isocont, 
                       random = list(~ timepoint_mean|cohort),
                       struct = c("CAR"))


# log best fit
qcont_fast_mv <- rma.mv(yi, qcont_fastV, 
                              mods = ~log(timepoint_mean),
                              data = fastcont, 
                              random = list(~ timepoint_mean|cohort),
                              struct = c("CAR"))

# log best fit
qcont_slow_mv <- rma.mv(yi, qcont_slowV, 
                        mods = ~log(timepoint_mean),
                        data = slowcont, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

# log best fit
qcont_iso_mv <- rma.mv(yi, qcont_isoV, 
                        mods = ~log(timepoint_mean),
                        data = isocont, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

####
## HS Case Control
#####


hscont <- casecontrol %>%
  filter(str_detect(measure, "hs")) %>%
  filter(acl_timepoint_mean >2) %>% # no pre-operative data
  mutate(es_id = row_number(),
         timepoint_mean = acl_timepoint_mean,
         group = 1) %>%
  filter(timepoint_mean >0.2, # no pre-operative data
         timepoint_mean < 120) %>%
  mutate(measure_2 = case_when(
    str_detect(measure, "mvic|hhd") ~ "Isometric",
    str_detect(measure, "isk con 180|isk con 230|isk con 240|isk con 300") ~ "Fast Isokinetic",
    str_detect(measure, "isk con 30|isk con 60|isk con 90|isk con 120") ~ "Slow Isokinetic"
  )) %>%
  filter(!is.na(measure_2)) %>%
  filter(!(cohort == "Laudner 2015" & measure == "hs isk con 300"), # remove where multiple effect sizes in same measure group
         !(cohort == "Tourville 2014" & measure == "hs isk con 300"),
         !(cohort == "Zult 2017" & measure == "hs isk con 120"))

# Reducing timepoint down to categories (3, 6, 12, 24, 48  months post)
hscont_cat <- hscont %>%
  mutate(timepoint_cut = cut(timepoint_mean, 
                             breaks = c(0, 4.5, 9, 18, 36, 72, Inf), 
                             labels = c(3, 6, 12, 24, 48, 96))) 
  #group_by(cohort, timepoint_cut, measure) %>%
  #slice(which.min(abs(timepoint_mean - as.numeric(as.character(timepoint_cut))))) %>%
  #ungroup()

# Fast data is ALL isk con 180
hs_fastcont <- hscont_cat %>% filter(measure_2 == "Fast Isokinetic")
hs_slowcont <- hscont_cat %>% filter(measure_2 == "Slow Isokinetic")
hs_isocont <- hscont_cat %>% filter(measure_2 == "Isometric")


hcont_fastV <- impute_covariance_matrix(vi = hs_fastcont$vi, 
                                        cluster = hs_fastcont$cohort,
                                        ti = hs_fastcont$timepoint_mean,
                                        ar1 = 0.85,
                                        check_PD = TRUE,
                                        smooth_vi = TRUE, 
                                        return_list = FALSE)

hcont_slowV <- impute_covariance_matrix(vi = hs_slowcont$vi, 
                                        cluster = hs_slowcont$cohort,
                                        ti = hs_slowcont$timepoint_mean,
                                        ar1 = 0.85,
                                        smooth_vi = TRUE,
                                        return_list = FALSE)

hcont_isoV <- impute_covariance_matrix(vi = hs_isocont$vi, 
                                       cluster = hs_isocont$cohort,
                                       ti = hs_isocont$timepoint_mean,
                                       ar1 = 0.85,
                                       check_PD = TRUE,
                                       smooth_vi = TRUE,
                                       return_list = FALSE)

## Empty model


hcont_fast_mv_empty <- rma.mv(yi, hcont_fastV, 
                              data = hs_fastcont, 
                              random = list(~ timepoint_mean|cohort),
                              struct = c("CAR"))

hcont_slow_mv_empty <- rma.mv(yi, hcont_slowV, 
                              data = hs_slowcont, 
                              random = list(~ timepoint_mean|cohort),
                              struct = c("CAR"))

hcont_iso_mv_empty <- rma.mv(yi, hcont_isoV, 
                             data = hs_isocont, 
                             random = list(~ timepoint_mean|cohort),
                             struct = c("CAR"))

## Best fitting:


# log best fit
hcont_fast_mv <- rma.mv(yi, hcont_fastV, 
                        mods = ~log(timepoint_mean),
                        data = hs_fastcont, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

# log best fit
hcont_slow_mv <- rma.mv(yi, hcont_slowV, 
                        mods = ~log(timepoint_mean),
                        data = hs_slowcont, 
                        random = list(~ timepoint_mean|cohort),
                        struct = c("CAR"))

# polynomial best fit? a lot of variability here.
hcont_iso_mv <- rma.mv(yi, hcont_isoV, 
                       mods = ~poly(timepoint_mean, degree = 2, raw = TRUE),
                       data = hs_isocont, 
                       random = list(~ timepoint_mean|cohort),
                       struct = c("CAR"))


