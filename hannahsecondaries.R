
library(readr)
library(metafor)
library(ggplot2)
library(meta)
library(forestplot)
library(stringr)

# Read data from file
trials <- read_csv("/Users/hannahmoyer/Desktop/finalallsearches.csv")

#Make subset of trials for all different analyses 
per_arm_g34_trials <- subset(trials, Isthere4data == "yes" )
per_arm_g5_trials <- subset(trials, Isthere5data == "yes" )
os_analysis <- subset(trials, IsthereOSdata == "yes" )
pfs_analysis <- subset(trials, IstherePFSdata=="yes")
graduation_patients <- subset(trials, Anygraduation=="Graduated")
graduationnot_patients <- subset(trials, Anygraduation=="Did not graduate")
industrynumber <- subset(trials, funding_industry=="Industry funded")
academicnumber <- subset(trials, funding_industry=="Not industry funded")
biomarkernumber <- subset(trials, biomarker=="Biomarker enriched")
nbiomarkernumbernot <- subset(trials, biomarker=="Not biomarker enriched")
gradpat<-sum (graduation_patients$Total)

per_arm_g34_trials$studylabel <- per_arm_g34_trials$`NCT ID`
funding_perarmg34 <- metabin(
  g4ABevents,
  safetynab,
  g4Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g34_trials, 
  comb.fixed = FALSE,
  byvar = funding_industry
)

per_arm_g5_trials$studylabel <- per_arm_g5_trials$`NCT ID`
funding_perarmg5 <- metabin(
  g5ABevents,
  safetynab,
  g5Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g5_trials, 
  comb.fixed = FALSE,
  byvar = funding_industry
)


funding_os <- metagen(
  log(OShazard_ratio),
  OS_SE,
  `NCT ID`,
  data = os_analysis,
  sm = "HR",
  comb.fixed = FALSE,
  byvar = funding_industry
)

funding_pfs <- metagen(
  log(PFShazard_ratio),
  PFS_SE,
  `NCT ID`,
  data = pfs_analysis,
  sm = "HR",
  comb.fixed = FALSE,
  byvar = funding_industry
)
#trajectory

per_arm_g34_trials$studylabel <- per_arm_g34_trials$`NCT ID`
trajectory_perarmg34 <- metabin(
  g4ABevents,
  safetynab,
  g4Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g34_trials, 
  comb.fixed = FALSE,
  byvar = trajectory_type
)


per_arm_g5_trials$studylabel <- per_arm_g5_trials$`NCT ID`
trajectory_perarmg5 <- metabin(
  g5ABevents,
  safetynab,
  g5Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g5_trials, 
  comb.fixed = FALSE,
  byvar = trajectory_type
)

trajectory_os <- metagen(
  log(OShazard_ratio),
  OS_SE,
  `NCT ID`,
  data = os_analysis,
  sm = "HR",
  comb.fixed = FALSE,
  byvar = trajectory_type
)
summary(trajectory_os)
trajectory_pfs <- metagen(
  log(PFShazard_ratio),
  PFS_SE,
  `NCT ID`,
  data = pfs_analysis,
  sm = "HR",
  comb.fixed = FALSE,
  byvar = trajectory_type
)


#Graduation 



per_arm_g34_trials$studylabel <- per_arm_g34_trials$`NCT ID`
graduation_perarmg34 <- metabin(
  g4ABevents,
  safetynab,
  g4Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g34_trials, 
  comb.fixed = FALSE,
  byvar = Anygraduation
)


per_arm_g5_trials$studylabel <- per_arm_g5_trials$`NCT ID`
graduation_perarmg5 <- metabin(
  g5ABevents,
  safetynab,
  g5Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g5_trials, 
  comb.fixed = FALSE,
  byvar = Anygraduation
)

graduation_os <- metagen(
  log(OShazard_ratio),
  OS_SE,
  `NCT ID`,
  data = os_analysis,
  sm = "HR",
  comb.fixed = FALSE,
  byvar = Anygraduation
)

graduation_pfs <- metagen(
  log(PFShazard_ratio),
  PFS_SE,
  `NCT ID`,
  data = pfs_analysis,
  sm = "HR",
  comb.fixed = FALSE, 
  byvar = Anygraduation
)

#Type of Drug Analysis 
sum (trials$drug_type == "Cytotoxic")
sum (trials$drug_type == "Targeted")
sum (trials$drug_type == "Immunotherapy")
sum (trials$drug_type == "Other")

per_arm_g5_trials$studylabel <- per_arm_g34_trials$`NCT ID`
drugtype_os <- metagen(
  log(OShazard_ratio),
  OS_SE,
  `NCT ID`,
  data = os_analysis,
  sm = "HR",
  comb.fixed = FALSE,
  byvar = drug_type,
  print.byvar= TRUE,
)




#Biomarker 
per_arm_g34_trials$studylabel <- per_arm_g34_trials$`NCT ID`
biomarker_perarmg34 <- metabin(
  g4ABevents,
  safetynab,
  g4Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g34_trials, 
  comb.fixed = FALSE,
  byvar = biomarker
)

per_arm_g5_trials$studylabel <- per_arm_g5_trials$`NCT ID`
biomarker_perarmg5 <- metabin(
  g5ABevents,
  safetynab,
  g5Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g5_trials, 
  comb.fixed = FALSE,
  byvar = biomarker
)

biomarker_os <- metagen(
  log(OShazard_ratio),
  OS_SE,
  `NCT ID`,
  data = os_analysis,
  sm = "HR",
  comb.fixed = FALSE,
  byvar = biomarker
)

biomarker_pfs <- metagen(
  log(PFShazard_ratio),
  PFS_SE,
  `NCT ID`,
  data = pfs_analysis,
  sm = "HR",
  comb.fixed = FALSE, 
  byvar = biomarker
)


summary(funding_pfs)
summary(funding_os)
summary(funding_perarmg34) 
summary(funding_perarmg5)

summary(graduation_pfs)
summary(graduation_os)
summary(graduation_perarmg34) 
summary(graduation_perarmg5)

summary(trajectory_pfs)
summary(trajectory_os)
summary(trajectory_perarmg34)
summary(trajectory_perarmg5)

summary(drugtype_pfs)
summary(drugtype_os)
summary(drugtype_perarmg34) 
summary(drugtype_perarmg5)

summary(biomarker_pfs)
summary(biomarker_os)
summary(biomarker_perarmg34) 
summary(biomarker_perarmg5)








