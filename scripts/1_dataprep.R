
library(tidyverse)
library(metafor)
library(simputation)

# 3 separate data files
# Data contains information on effects
# Demographics contains all info about each study/groups
# Methods is highest level information about type of study, country and linking studies

data <- read.csv("data/processed/data_20231128.csv")
demographics <- read.csv("data/processed/demographics_20231128.csv")
methods <- read.csv("data/processed/methods_20231128.csv")

# Get combo information
# This is information about how groups/timepoints can be combined within studies (completed during data extraction)
combo <- demographics %>%
  select(study, name, graft, female, group_indep, group_indep_notes, timepoint_combo, combination_notes) %>%
  rename(group = name)

# join combo information to data
data <- left_join(data, combo, by = c("study", "group"))


# Get studies with longitudinal data across multiple different papers
longitudinal <- data %>% filter(study != cohort)

longitudinal <- bind_rows(longitudinal, data %>% filter(study %in% longitudinal$cohort)) 

long_codes <- longitudinal %>% select(study_id) %>% distinct()

#### Step 1: Organise studies with cohorts across multiple papers ####

## For longitudinal studies across different papers:
# Ahlden 2013 - ok
# Arundale 2017 - ok
# Aune 2001; Risberg 1999a; Risberg 1999b - see below, exclude Holm 2010 stair hop and triple hop data as ?duplicated in Oiestad 2013, and baseline included in Aune 2001
# Beischer 2019 - remove all Sundemo 2022 - overlap with Piussi 2020, other outcomes ok.
# Culvenor 2016 - drop single leg hop 12M data frmo Patterson 2020a; Perraton 2017 - exclude all data as included in Patterson 2020a which has longitudinal
# Ebert 2021a - no overlap as hop data not extracted from #9533. Need to combine male/female for Ebert 2021a.
# Eitzen 2010; Hartigan 2009 - drop baseline measures (Hartigan 2009 & Eitzen 2010c) - given crossovers from ACLD to ACLR, also too hard to track longitudinally over time due to n changes. Keep long term data in (Wellsandt 2018)
# Ejerhed 2003; Laxdal 2005 - see below
# Ericsson 2013 - drop Flosadottir 2018 ACL-X group (delayed reconstruction)
# Eriksson 2001 - ok
# Fleming 2013 - drop DeFroda 2021 outcomes as overlaps with Akelman 2016
# Grant 2005 - ok
# Heijne 2007 - ok
# Johnston 2020 - drop 6m timepoint from #10922 as slightly lower n than in #355.
# King 2018 - ok
# McRae 2013 - ok
# Tengman 2014a - ok
# Delaware Oslo Cohorts: Need to link together later, see below
## Grindem 2012 - Exclude - due to overlap with Grindem 2014/2018
## Grindem 2014 - ok hop data included in Grindem 2018
## Logerstedt 2014 - ok
## Grindem 2016 - exclude - due to overlap with other studies
## Nawasreh 2017 - exclude - overlap with Logerstedt 2014 (larger n)
## Grindem 2018 - ok, strength data in Grindem 2014
## Pedersen 2021 - ok (ACLR and delayed ACLR not extracted as no time post surgery given)


longitudinal <- longitudinal %>%
  filter(!(study == "Holm 2010" & measure %in% c("stair hop", "triple hop")), # drop the stair and triple hop data
         !(study == "Patterson 2020a" & measure == "single hop" & timepoint == 12), # drop 12M timepoint
         !(study == "Bjornsson 2016" & timepoint_mean == 0.10), # drop pre-op timepoint
         !(study == "Flosadottir 2018" & group == "ACLX"), # drop delayed reconstruction group
         !(study == "Johnston 2022" & timepoint_mean == 6), # drop 6m timepoint
         !study %in% c("Perraton 2017", "Hart 2020", "Sundemo 2019", "Hartigan 2009", "Eitzen 2010c", "DeFroda 2021",
                       "Grindem 2012", "Grindem 2016", "Nawasreh 2017")) # drop these data altogether as included elsewhere

## Need to 'split' Delaware/Oslo cohort links to the ACLR group and ACLD group.
# Grindem 2014 Non-surgical group, Grindem 2018 and Pedersen 2021 are all ACLD group
# Grindem 2014 Surgical group and Logerstedt 2014 are ACLR group - some overlap but unclear how much. Linking to be conservative.
longitudinal <- longitudinal %>%
  mutate(cohort = case_when(
    study == "Grindem 2014" & group == "Surgical" ~ "Logerstedt 2014",
    study == "Logerstedt 2014" ~ "Logerstedt 2014",
    TRUE ~ cohort))


# Mutli-study baseline studies to combine
## Aune 2001, Risberg 1999a, Risberg 1999b
## Combine across 6, 12 24 timepoint
aune2001 <- longitudinal %>%
  filter(cohort == "Aune 2001; Risberg 1999a; Risberg 1999b" & timepoint_mean %in% c(6, 12, 24)) %>%
  group_by(measure, timepoint) %>%
  mutate(timepoint_mean_new = combine_mean(n, timepoint_mean),
         n_new = sum(n),
         female_new = sum(female),
         inj_mean_new = combine_mean(n, inj_mean),
         inj_sd_new = combine_sd(n, inj_mean, inj_sd),
         noninj_mean_new = combine_mean(n, noninj_mean),
         noninj_sd_new = combine_sd(n, noninj_mean, noninj_sd),  
         lsi_mean_new = combine_mean(n, lsi_mean),
         lsi_sd_new = combine_sd(n, lsi_mean, lsi_sd), 
         group_new = paste(group, collapse = " + "),
         graft_new = case_when( # combine graft names - if all the same then preserve the original, if different then mixed
           length(unique(graft)) == 1 ~ graft,
           TRUE ~ "mixed"
         )) %>%
  distinct(measure, timepoint, .keep_all = TRUE) %>%
  ungroup() %>%
  # delete the old columns
  select(-c(group, n, inj_mean, inj_sd, noninj_mean, noninj_sd, lsi_mean, lsi_sd, timepoint_mean, female, graft)) %>%
  rename_with(~str_replace(., "_new", ""), ends_with("_new")) %>% # rename the new columns, removing "_new"
  mutate(group = "Combined",
         study = "Aune 2001; Risberg 1999a; Risberg 1999b",
         study_id = "#317640044001")

## Holm 2010
holm2010 <- longitudinal %>%
  filter(study == "Holm 2010") %>%
  group_by(measure) %>%
  mutate(timepoint_mean_new = combine_mean(n, timepoint_mean),
         n_new = sum(n),
         female_new = sum(female),
         inj_mean_new = combine_mean(n, inj_mean),
         inj_sd_new = combine_sd(n, inj_mean, inj_sd),
         noninj_mean_new = combine_mean(n, noninj_mean),
         noninj_sd_new = combine_sd(n, noninj_mean, noninj_sd),  
         lsi_mean_new = combine_mean(n, lsi_mean),
         lsi_sd_new = combine_sd(n, lsi_mean, lsi_sd), 
         group_new = paste(group, collapse = " + "),
         graft_new = case_when( # combine graft names - if all the same then preserve the original, if different then mixed
           length(unique(graft)) == 1 ~ graft,
           TRUE ~ "mixed"
         )) %>%
  distinct(measure, .keep_all = TRUE) %>%
  ungroup() %>%
  # delete the old columns
  select(-c(group, n, inj_mean, inj_sd, noninj_mean, noninj_sd, lsi_mean, lsi_sd, timepoint_mean, female, graft)) %>%
  rename_with(~str_replace(., "_new", ""), ends_with("_new")) %>% # rename the new columns, removing "_new"
  mutate(group = "Combined")

## Arundale 2017 / Capin 2019
## Study split across 2 papers, men in one, women in the other
## Arhos 2021 is same cohort, but different timepoint, no need to use here
arundalecapin <- longitudinal %>%
  filter(cohort == "Arundale 2017; Capin 2019") %>%
  filter(study != "Arhos 2021") %>%
  group_by(timepoint, measure) %>%
  mutate(timepoint_mean_new = combine_mean(n, timepoint_mean),
         n_new = sum(n),
         inj_mean_new = combine_mean(n, inj_mean),
         inj_sd_new = combine_sd(n, inj_mean, inj_sd),
         noninj_mean_new = combine_mean(n, noninj_mean),
         noninj_sd_new = combine_sd(n, noninj_mean, noninj_sd),  
         lsi_mean_new = combine_mean(n, lsi_mean),
         lsi_sd_new = combine_sd(n, lsi_mean, lsi_sd), 
         group_new = paste(group, collapse = " + ")) %>%
  distinct(measure, .keep_all = TRUE) %>%
  ungroup() %>%
  # delete the old columns
  select(-c(group, n, inj_mean, inj_sd, noninj_mean, noninj_sd, lsi_mean, lsi_sd, timepoint_mean)) %>%
  rename_with(~str_replace(., "_new", ""), ends_with("_new")) %>% # rename the new columns, removing "_new"
  mutate(group = "Combined",
         study = "Arundale 2017; Capin 2019",
         study_id = "#29043224")

## Ejerhed Laxdal
## Combine all groups across pre-specified timepoint, measure

ejerhed <- longitudinal %>%
  filter(cohort %in% c("Ejerhed 2003; Laxdal 2005")) %>%
  mutate(timepoint = case_when(study == "Bjornsson 2016" ~ as.integer(3), TRUE ~ timepoint), # change timemepoint to 3rd
         study = case_when(study != "Bjornsson 2016" ~ "Ejerhed 2003; Laxdal 2005", TRUE ~ study),
         study_id = case_when(study != "Bjornsson 2016" ~ "#13346560", TRUE ~ study_id)) %>% 
  group_by(measure, timepoint) %>%
  mutate(timepoint_mean_new = combine_mean(n, timepoint_mean),
         n_new = sum(n),
         female_new = sum(female),
         inj_mean_new = combine_mean(n, inj_mean),
         inj_sd_new = combine_sd(n, inj_mean, inj_sd),
         noninj_mean_new = combine_mean(n, noninj_mean),
         noninj_sd_new = combine_sd(n, noninj_mean, noninj_sd),  
         lsi_mean_new = combine_mean(n, lsi_mean),
         lsi_sd_new = combine_sd(n, lsi_mean, lsi_sd), 
         group_new = paste(group, collapse = " + "),
         graft_new = case_when( # combine graft names - if all the same then preserve the original, if different then mixed
           length(unique(graft)) == 1 ~ graft,
           TRUE ~ "mixed"
         )) %>%
  distinct(measure, timepoint, .keep_all = TRUE) %>%
  ungroup() %>%
  # delete the old columns
  select(-c(group, n, inj_mean, inj_sd, noninj_mean, noninj_sd, lsi_mean, lsi_sd, timepoint_mean, female, graft)) %>%
  rename_with(~str_replace(., "_new", ""), ends_with("_new")) %>% # rename the new columns, removing "_new"
  mutate(group = "Combined")

# Join these back to the main longitudinal data frame
longitudinal <- bind_rows(
  longitudinal %>%
  filter(!(cohort == "Aune 2001; Risberg 1999a; Risberg 1999b" & timepoint_mean %in% c(6, 12, 24)),
         study != "Holm 2010",
         cohort != "Ejerhed 2003; Laxdal 2005",
         study != "Arundale 2017",
         study != "Capin 2019"),
  aune2001,
  holm2010,
  ejerhed,
  arundalecapin) %>%
  mutate(group_indep = case_when(study == "Ebert 2021a" ~ 0,
                                 TRUE ~ 1),
         timepoint_combo = 1)

# Finally join these back to the main dataframe
data <- bind_rows(
  data %>% filter(!study_id %in% c(long_codes$study_id)),
  longitudinal
)

# Logerstedt studies have overlapping data with other studies from Delaware Oslo Cohort
data <- data %>%
  filter(!study %in% c("Logerstedt 2013", "Logerstedt 2012"))

#### Step 2: Groups that have non-indepedent groups ####

## Non-independent group studies
# Individual decisions for each

# Bodkin 2020 - Repeat measure group - uses two timepoints so take that one
# Dempsey 2019 - All included - split based on graft, drop the all group
# Kuenze 2019b - Male, Female - multiple different splits, using the male female split
# Shibata 2019 - Male, Female - use the all group
# Blakeney 2018 - G6M, Healthy Control - dropping the smaller test-retest group (only 2 months apart)
# Siney 2010- DIDT, KJ - splits on graft type, drop the All group
# Ebert 2021a - Male, Female - use the entire cohort group
# Hogberg 2023 - different timepoints with overlapping participants. Take the latest timepoint (12m)
# Schwery 2022 - multiple splits - use the graft split (not M/F)
# Logerstedt 2014 - combine below normal IKDC 6m, Normal IKDC 6m; 
# then separately below normal IKDC 12m, Normal IKDC 12m, then use as separate timepoints

nonindep <- data %>%
  filter(group_indep == 0) %>%
  filter((study == "Bodkin 2020" & group == "Repeat measure group") | 
           (study == "Dempsey 2019" & group != "All included") | # don't use the combined group
           (study == "Kuenze 2019b" & group %in% c("Male", "Female")) | 
           (study == "Shibata 2019" & group == "All") | # use the all group
           (study == "Blakeney 2018" & group %in% c("G6M", "Healthy Control")) |
           (study == "Siney 2010" & group != "All") |
           (study == "Ebert 2021a" & group == "Entire cohort") |
           (study == "Schwery 2022" & group %in% c("BPTB", "HT", "QT")) |
           (study == "Hogberg 2023" & group == "12 months"))

# Logerstedt 2014 is unique case where at each timepoint, groups are split by IKDC
# Need to combine the groups at each timepoint to get the total group value for each timepoint
logerstedt <- data %>%
  filter(study == "Logerstedt 2014") %>%
  group_by(study, measure, timepoint) %>%
  mutate(timepoint_mean_new = combine_mean(n, timepoint_mean),
         n_new = sum(n),
         female_new = sum(female),
         inj_mean_new = combine_mean(n, inj_mean),
         inj_sd_new = combine_sd(n, inj_mean, inj_sd),
         noninj_mean_new = combine_mean(n, noninj_mean),
         noninj_sd_new = combine_sd(n, noninj_mean, noninj_sd),  
         lsi_mean_new = combine_mean(n, lsi_mean),
         lsi_sd_new = combine_sd(n, lsi_mean, lsi_sd), 
         row = row_number(),
         group_new = "All",
         graft = "mixed") %>%
  distinct(measure, study, .keep_all = TRUE) %>%
  ungroup() %>%
  # delete the old columns
  select(-c(row, group, n, inj_mean, inj_sd, noninj_mean, noninj_sd, lsi_mean, lsi_sd, timepoint_mean, female, graft)) %>%
  rename_with(~str_replace(., "_new", ""), ends_with("_new")) # rename the new columns, removing "_new"

# Final data for studies with non-independent groups
nonindep <- bind_rows(nonindep, logerstedt) %>%
  mutate(group_indep = 1,
         timepoint_combo =1)

# join back to main data-frame
newdata <- bind_rows(
  data %>% filter(group_indep == 1 | is.na(group_indep)),
  nonindep
) %>%
  mutate(group_indep = case_when( # change the single group to be a 1 instead of NA
    is.na(group_indep) ~ 1,
    TRUE ~ group_indep
  ))

# remove contralateral grafts and ACLD groups
newdata <- newdata %>%
  filter(is.na(graft) | !str_detect(graft, "Contralateral|contralateral")) %>% # need to keep the NA graft in as they are the Healthy Controls
  filter(is.na(graft) | !str_detect(graft, "ACLD")) # need to keep the NA graft in as they are the Healthy Controls

newdata <- newdata %>%
  mutate(graft_group = case_when(
    str_detect(graft, "Contralateral") ~ "other",
    str_detect(graft, "LARS") ~ "other",
    str_detect(graft, "HS") ~ "hs",
    str_detect(graft, "BPTB|PT|QT") ~ "quad",
    is.na(graft) ~ NA_character_,
    str_detect(graft, "mixed|Mixed") ~ "mixed",
    TRUE ~ "other"
  ))

#### Step 3: Combining groups to have one effect size per timepoint per study ####

## Combining groups across different timepoints
## E.g. a study following two independant groups across time (at same pre-specified timepoints)
## Combine these in order to have one effect size per study per timepoint

data_tocombine_1 <- newdata %>%
  filter(group_indep == 1, # studies with independent groups
         # studies with confirmed combination across groups *
         # timepoint or groups with a single timepoint only and independant groups
         timepoint_combo == 1 | (n_timepoints = 1 & timepoint_combo == 0), 
         str_detect(group, "Healthy Control", negate = TRUE)) %>% # not healthy control groups
  rowwise() %>%
  mutate(x = sum(is.na(pick(c(inj_mean, inj_sd, noninj_mean, noninj_sd))))) %>% # create a column to count amout of missing data
  ungroup() %>% # note the example for this is Engelen-VanMelick 2017 due to strange missing data in text
  group_by(study, measure, timepoint) %>%
  filter(x == min(x)) %>% # filter out rows where there is different amounts of missing data in groups to be combined (would affect calc)
  select(-x) %>% # remove temporary variable
  mutate(timepoint_mean_new = combine_mean(n, timepoint_mean),
         n_new = sum(n),
         female_new = sum(female),
         inj_mean_new = combine_mean(n, inj_mean),
         inj_sd_new = combine_sd(n, inj_mean, inj_sd),
         noninj_mean_new = combine_mean(n, noninj_mean),
         noninj_sd_new = combine_sd(n, noninj_mean, noninj_sd),  
         lsi_mean_new = combine_mean(n, lsi_mean),
         lsi_sd_new = combine_sd(n, lsi_mean, lsi_sd), 
         group_new = paste(group, collapse = " + "),
         graft_new = case_when( # combine graft names - if all the same then preserve the original, if different then mixed
           length(unique(graft)) == 1 ~ graft,
           TRUE ~ "mixed"
         )) %>%
  distinct(measure, study, .keep_all = TRUE) %>%
  ungroup() %>%
  # delete the old columns
  select(-c(group, n, inj_mean, inj_sd, noninj_mean, noninj_sd, lsi_mean, lsi_sd, timepoint_mean, female, graft, group_indep, group_indep_notes, timepoint_combo, combination_notes)) %>%
  rename_with(~str_replace(., "_new", ""), ends_with("_new")) # rename the new columns, removing "_new"

# Combining groups across 1 timepoint with different times for each group
# E.g. independant groups measured once but at different timepoints (e.g. 6 month, a 12 month group)
# Combine across the 1 timepoint to get an average effect size for the study.

data_tocombine_2 <- newdata %>%
  filter(group_indep == 1, # studies with independant groups
         timepoint_combo == 2, # combination of different groups across 1 timepoint (i.e. independant groups, each with a different timepoint)
         str_detect(group, "Healthy Control", negate = TRUE)) %>% # not for healthy control
  group_by(study, measure) %>%
  mutate(timepoint_mean_new = combine_mean(n, timepoint_mean),
         n_new = sum(n),
         female_new = sum(female),
         inj_mean_new = combine_mean(n, inj_mean),
         inj_sd_new = combine_sd(n, inj_mean, inj_sd),
         noninj_mean_new = combine_mean(n, noninj_mean),
         noninj_sd_new = combine_sd(n, noninj_mean, noninj_sd),  
         lsi_mean_new = combine_mean(n, lsi_mean),
         lsi_sd_new = combine_sd(n, lsi_mean, lsi_sd), 
         group_new = paste(group, collapse = " + "),
         graft_new = case_when( # combine graft names - if all the same then preserve the original, if different then mixed
           length(unique(graft)) == 1 ~ graft,
           TRUE ~ "mixed"
         )) %>%
  distinct(measure, study, .keep_all = TRUE) %>%
  ungroup() %>%
  # delete the old columns
  select(-c(group, n, inj_mean, inj_sd, noninj_mean, noninj_sd, lsi_mean, lsi_sd, timepoint_mean, female, graft, group_indep, group_indep_notes, timepoint_combo, combination_notes)) %>%
  rename_with(~str_replace(., "_new", ""), ends_with("_new")) # rename the new columns, removing "_new"

# All data now with timepoints combined where needed, independant groups and ready for analysis
alldata <- bind_rows(data_tocombine_1, data_tocombine_2) %>%
  bind_rows(., newdata %>% filter(str_detect(group, "Healthy Control"))) # join the healthy control data back too


notsplit_alldata <- bind_rows(
  newdata %>% filter(!study %in% c(data_tocombine_2$study)), 
  data_tocombine_2) %>%
  bind_rows(., newdata %>% filter(str_detect(group, "Healthy Control"))) # join the healthy control data back too

#### Step 5: Combining multiple control groups as needed ####

## Some groups have multiple healthy control groups, need to combine them
# similar approach to above

multicontrol <- alldata %>% filter(str_detect(group, "Healthy Control")) %>%
  group_by(cohort) %>%
  distinct(group, .keep_all = TRUE) %>%
  summarise(n = length(n)) %>%
  filter(n >1) # take those which have more than 1 healthy control group

control_combine <- alldata %>%
  filter(cohort %in% multicontrol$cohort,
         str_detect(group, "Healthy Control")) %>%
  filter(!(study == "Engelen-VanMelick 2017" & measure == "hs isk con 180" & group == "Healthy Control - Women")) %>% # remove this to not bias comparison against men
  group_by(study, measure, timepoint) %>%
  mutate(timepoint_mean_new = combine_mean(n, timepoint_mean),
         n_new = sum(n),
         female_new = sum(female),
         inj_mean_new = combine_mean(n, inj_mean),
         inj_sd_new = combine_sd(n, inj_mean, inj_sd),
         noninj_mean_new = combine_mean(n, noninj_mean),
         noninj_sd_new = combine_sd(n, noninj_mean, noninj_sd),  
         lsi_mean_new = combine_mean(n, lsi_mean),
         lsi_sd_new = combine_sd(n, lsi_mean, lsi_sd), 
         group_new = paste(group, collapse = " + "),
         graft_new = case_when( # combine graft names - if all the same then preserve the original, if different then mixed
           length(unique(graft)) == 1 ~ graft,
           TRUE ~ "mixed"
         )) %>%
  distinct(measure, study, .keep_all = TRUE) %>%
  ungroup() %>%
  # delete the old columns
  select(-c(group, n, inj_mean, inj_sd, noninj_mean, noninj_sd, lsi_mean, lsi_sd, female, graft, timepoint_mean, group_indep, group_indep_notes, timepoint_combo, combination_notes)) %>%
  rename_with(~str_replace(., "_new", ""), ends_with("_new")) # rename the new columns, removing "_new"

# Join back to main dataframe
alldata <- bind_rows(
  alldata %>% filter(!(cohort %in% multicontrol$cohort & str_detect(group,"Healthy Control")),
                     !(study == "Engelen-VanMelick 2017" & measure == "hs isk con 180" & group == "Healthy Control - Women")), # remove this to not bias comparison against men),
  control_combine
)

# remove data > 10 years post surgery.
alldata <- alldata %>%
  filter(timepoint_mean <= 120,
         !cohort %in% c("Aune 2001; Risberg 1999a; Risberg 1999b", 
                        "Ejerhed 2003; Laxdal 2005"))
  
#### Step 6: Calculate effect sizes ####

# Calcualte RoM based on available data
# For studies with only LSI, calculate RoM effect sizes using LSI mean and SD
# log(LSI Mean) =~ RoM
# for variance, need to convert SD to 95%CI, then log transform and then from that calculate SE and variance

# Within person comparison data frame:
within_data <- alldata %>%
  filter(str_detect(group, "Healthy Control", negate = TRUE)) %>% # remove the healthy control data for within person
  rename(acl_n = n) %>%
  mutate(acl_n = as.integer(acl_n),
         lsi_sd2 = lsi_sd) %>% # create temporary copy of sd column
  impute_pmm(lsi_sd2 ~ lsi_mean + acl_n) %>% # impute sd using predictive mean matching based on lsi mean and acl_n
  rowwise() %>%
  mutate(lsi_sd = case_when(
    is.na(inj_sd) & !is.na(lsi_mean) ~ lsi_sd2, # if only lsi_mean is given (and no inj/noninj data) then use imputed lsi_sd
    TRUE ~ lsi_sd # otherwise retain original lsi_sd
  )) %>%
  ungroup %>% 
  select(-lsi_sd2) # remove temporary column

within_data <- within_data %>%
  mutate(ri = 0.85) %>% # need correlation between sides for calculation of effect. Estimating based on data from our own studies
  escalc(ni = acl_n, m1i = inj_mean, sd1i = inj_sd, m2i = noninj_mean, sd2i = noninj_sd, ri = ri, 
         data = ., measure = "ROMC") %>%
  mutate(lsi_mean = lsi_mean / 100,
         lsi_sd = lsi_sd / 100,
         yi = case_when(
           is.na(yi) & !is.na(lsi_mean) ~ log(lsi_mean),
           TRUE ~ yi),
         vi = case_when(
           is.na(vi) & !is.na(lsi_mean) ~ lsi_sd^2/(acl_n * lsi_mean^2),
           TRUE ~ vi)) %>% 
  summary 

## Case Control comparison Data frame
casecontrol <- alldata %>%
  select(-c(timepoint_median, timepoint_iqr, timepoint_range, n_timepoints, group_indep, 
            group_indep_notes, timepoint_combo, combination_notes)) %>%
  group_by(cohort) %>%
  filter(any(str_detect(group,"Healthy Control"))) %>% # filter all cohorts that have a healthy control to compare to
  ungroup() %>%
  mutate(group2 = case_when(
    str_detect(group, "Healthy Control") ~ "Healthy Control",
    TRUE ~ group
  )) %>%
  pivot_longer(-c(study, study_id, cohort, measure, group2, units, timepoint),
               names_to = "names",
               values_to = "val",
               values_transform = list(val = as.character)) %>%
  mutate(population_group = ifelse(group2 == "Healthy Control", "con", "acl")) %>%
  select(-group2) %>%
  pivot_wider(id_cols = c(study, study_id, cohort, measure, units, timepoint),
              names_from = c(population_group, names),
              names_sep = "_",
              values_from = "val") %>%
  mutate(across(matches("mean|sd|_n$"), as.numeric)) %>%
  arrange(cohort, measure, timepoint) %>%
  group_by(cohort, measure) %>%
  fill(con_inj_med:con_group, .direction = "updown") %>%
  ungroup() %>%
  mutate(acl_n = as.integer(acl_n)) %>%
  escalc(n1i = acl_n, m1i = acl_inj_mean, sd1i = acl_inj_sd, n2i = con_n, m2i = con_inj_mean, sd2i = con_inj_sd, 
         data = ., measure = "ROM") %>% 
  filter(!is.na(vi)) %>%
  summary

## Data is now ready for analysis! ##



write_csv(within_data, "data/processed/within_data.csv")
write_csv(casecontrol, "data/processed/casecontrol.csv")
  
##########################

