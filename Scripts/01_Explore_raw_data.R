## Script: 01_Explore_raw_data.R ------------------------------------


# 0. Working setup ---------------------------------
library(data.table)
library(ggplot2)
library(haven) # read .dta files
library(statar)

# 1. Load data ------------------------------------
SC_Data <- haven::read_dta('Data/Raw_Data/AlanBaysanGumrenKubilay_final.dta')
SC_Data <- as.data.table(SC_Data)

# 2. Clean data -----------------------------------------
#str(SC_Data)

# Check missing values
#colSums(is.na(SC_Data))

# Define controls
# Note: In do file they define 'age' instead of 'ageinm'
controls <- 'ageinm + male + refugee + astudent + b_schoolsize + braven_sd + beyes_sd + f_csize'

# 2.1 Recreate child-level variables ---------------------------------
# Age
# TODO: How is this created from ageinm?

# Raven Score braven_sd
SC_Data <- SC_Data[, braven_sd := (braven - mean(braven, na.rm=TRUE)) /sd(braven, na.rm=TRUE)]

# Eyes Test Score beyes_sd
SC_Data <- SC_Data[, beyes_sd := (beyes_correct - mean(beyes_correct, na.rm=TRUE)) /
                     sd(beyes_correct, na.rm=TRUE)]

# Willingness to donate at baseline
SC_Data <- SC_Data[, bdonation_sd := (bdonation - mean(bdonation, na.rm=TRUE)) /
                     sd(bdonation, na.rm=TRUE)]

# Create donation variables that account for missings
SC_Data <- SC_Data[, fdonation_a1_m := ifelse(is.na(fdonation_a1), 0, fdonation_a1)]
SC_Data <- SC_Data[, fdonation_a2_m := ifelse(is.na(fdonation_a2), 0, fdonation_a2)]

# Use of missing keyword in original do file indicates that if both cols are
# missing, we assign missing to fdonation
SC_Data <- SC_Data[, fdonation := ifelse(is.na(fdonation_a1) & is.na(fdonation_a2),
                                         NA, fdonation_a1_m + fdonation_a2_m)]
SC_Data <- SC_Data[, fdonation_perc := fdonation / 4]

# Ethnicity reference dummy
SC_Data <- SC_Data[, a2 := ifelse(!is.na(fdonation_a2), 1, 0)] 
SC_Data <- SC_Data[, a2 := ifelse(!is.na(fdonation_a1), 0, a2)] 

# Willingness to donate at Endline
SC_Data <- SC_Data[, fdonate := ifelse(!is.na(fdonation), 0, NA)]
SC_Data <- SC_Data[, fdonate := ifelse(!is.na(fdonation) & fdonation > 0, 1, fdonate)]

# Willingness to donate at Basline
SC_Data <- SC_Data[, bdonate := ifelse(!is.na(bdonation), 0, NA)]
SC_Data <- SC_Data[, bdonate := ifelse(!is.na(bdonation) & bdonation > 0, 1, bdonate)]

# 2.2 Recreate school and class-level variables ------------------------
# Correct factor variables
SC_Data <- SC_Data[, b_schoolid := as.factor(b_schoolid)]
SC_Data <- SC_Data[, b_classid := as.factor(b_classid)]

# Create aggregated data at the school level
SC_Data_schools <- SC_Data[, .(
  perpetrator = head(perpetrator, 1),
  victim = head(victim, 1),
  events = head(events, 1),
  treatment = head(treatment, 1), 
  bstudentnum_2 = max(bstudentnum_2, na.rm = TRUE),
  bstudentnum_3 = max(bstudentnum_3, na.rm = TRUE),
  n_class = uniqueN(b_classid),
  bactive_syrian_2 = head(bactive_syrian_2, 1), 
  bactive_syrian_3 = head(bactive_syrian_3, 1),
  b_provinceid = head(b_provinceid, 1),
  b_districtid = head(b_districtid, 1)
  ), by = .(b_schoolid)]

# Construct additional school-level covariates used in regressions
# School size
SC_Data_schools <- SC_Data_schools[, b_schoolsize := bstudentnum_2 + bstudentnum_3]

# Endline Class size
SC_Data_classes <- SC_Data[, .(fgonesum = sum(fgone),
                               f_csize = .N), by = .(f_classid)]
SC_Data_classes <- SC_Data_classes[, f_csize := f_csize - fgonesum]
SC_Data <- merge(SC_Data, SC_Data_classes[, .(f_classid, f_csize)], by ='f_classid')
SC_Data <- SC_Data[, f_csize := ifelse(fgone == 1, NA, f_csize)]

# Share of Syrian refugees in school
SC_Data_schools <- SC_Data_schools[, srefshare := (bactive_syrian_2 + bactive_syrian_3) / b_schoolsize]

# Absenteeism
SC_absenteeism <- SC_Data[, .(absent_sumschool = head(babsent_official, 1)),
                          by = .(b_schoolid, b_classid)]
SC_absenteeism <- SC_absenteeism[, .(absent_sumschool = sum(absent_sumschool)), by = .(b_schoolid)]
SC_Data_schools <- merge(SC_Data_schools, SC_absenteeism, by = 'b_schoolid')
SC_Data_schools <- SC_Data_schools[, absenteeism_school := absent_sumschool / b_schoolsize]
SC_Data_schools <- 
  SC_Data_schools[, absent_tert := xtile(absenteeism_school, n = 3), by = .(b_provinceid)]
SC_Data_schools <- SC_Data_schools[, bstrata := b_provinceid * 10 + absent_tert]

# Spillovers
SC_Data_schools <- SC_Data_schools[, spill := events - perpetrator]

# Add to recreated variables to main data.table
SC_Data <- merge(SC_Data, SC_Data_schools[, .(b_schoolid, bstrata, b_schoolsize, spill)], by ='b_schoolid')
rm(SC_absenteeism)

# 2.3 Imputation ----------------------------------------------------
# Authors mpute missing values using the median for the following variables:
# age bdonation_sd braven_sd beyes_sd bimpulsivity_sd bethnicbias_sd bempathy_pt_sd bempathy_ec_sd 
# bmath_sd bturk_sd bcpayoff bnode_in_friend bnode_in_support bnode_in_study bnode_in_supportself 
# bnode_in_studyself bstudyself_total_host bfriend_total_host bsupportself_total_host
SC_Data <- SC_Data[, bdonation_sd := ifelse(is.na(bdonation_sd), median(bdonation_sd, na.rm=TRUE), bdonation_sd)]
SC_Data <- SC_Data[, braven_sd := ifelse(is.na(braven_sd), median(braven_sd, na.rm=TRUE), braven_sd)]
SC_Data <- SC_Data[, beyes_sd := ifelse(is.na(beyes_sd), median(beyes_sd, na.rm=TRUE), beyes_sd)]

# 2.4 Export processed data -------------------------
fwrite(SC_Data, file='Data/Processed_data/ABGK_recreated_variables.csv')

# 3. Replicate tables ----------------------------
# Table 3: Treatment effects of Peer Violence and Victimization-Diary Records -----
m.table3.1 <- lm(formula = 'perpetrator ~ treatment + b_schoolsize + n_class + srefshare + 
                 factor(b_provinceid) + factor(bstrata) + factor(b_districtid)',
                 data = SC_Data_schools)

m.table3.2 <- lm(formula = 'victim ~ treatment + b_schoolsize + n_class + srefshare + 
                 factor(b_provinceid) + factor(bstrata) + factor(b_districtid)',
                 data = SC_Data_schools)

m.table3.3 <- lm(formula = 'events ~ treatment + b_schoolsize + n_class + srefshare + 
                 factor(b_provinceid) + factor(bstrata) + factor(b_districtid)',
                 data = SC_Data_schools)

m.table3.4 <- lm(formula = 'spill ~ treatment + b_schoolsize + n_class + srefshare + 
                 factor(b_provinceid) + factor(bstrata) + factor(b_districtid)',
                 data = SC_Data_schools)

# Table 8:  Treatment Effects on Altruism --------------------
# TODO Note: Pending to add clustered Standard Errors for SEs to match table.
m.table8.1 <- lm(formula  = paste0('fdonate ~ a2*treatment + factor(bstrata) + bdonation_sd +', controls),
                    data = SC_Data)

m.table8.2 <- lm(formula  = paste0('fdonation_perc ~ a2*treatment + factor(bstrata) + bdonation_sd +', controls),
                 data = SC_Data)

# Table 12: HTE on Altruism ---------------------------------
# TODO Pending to add clustered Standard Errors for SEs to match table.
m.table12.1 <- lm(formula = paste0(
  'fdonate ~ a2*treatment + factor(bstrata) + factor(b_districtid) + bdonation_sd +', controls),
                 data = SC_Data, subset = SC_Data$refugee == 0)

m.table12.2 <- lm(formula = paste0(
  'fdonate ~ a2*treatment + factor(bstrata) + factor(b_districtid)  + bdonation_sd +', controls),
                 data = SC_Data, subset = SC_Data$refugee == 1)

m.table12.3 <- lm(formula = paste0(
  'fdonation_perc ~ a2*treatment + factor(bstrata) + factor(b_districtid) + bdonation_sd +', controls),
                  data = SC_Data, subset = SC_Data$refugee == 0)

m.table12.4 <- lm(formula = paste0(
  'fdonation_perc ~ a2*treatment + factor(bstrata) + factor(b_districtid) + bdonation_sd +', controls),
                  data = SC_Data, subset = SC_Data$refugee == 1)
