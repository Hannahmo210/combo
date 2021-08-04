
library(readr)
library(ggplot2)
library(meta)
library(forestplot)
library(stringr)

trials <- read_csv("/Users/hannahmoyer/Desktop/finalallsearches.csv")
totalpatients <- sum(trials$Total)
#Make a subset for the g34 analysis, where g34s are reported
per_arm_g34_trials <- subset(trials, Isthere4data == "yes" )
g34total_patients <- sum(per_arm_g34_trials$safetyNtotals)
g34total_combination <- sum(per_arm_g34_trials$safetynab)
g34total_comparator <- sum(per_arm_g34_trials$safetynb)

per_arm_g34_trials$studylabel <- per_arm_g34_trials$`NCT ID`

per_arm_g34 <- metabin(
  g4ABevents,
  safetynab,
  g4Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g34_trials, 
  comb.fixed = FALSE
)

summary(per_arm_g34)

pdf(
  "g34_forest.pdf",
  18,
  5.5
)

#Make a forest plot

forest(per_arm_g34,
       leftcols = c("NCT ID", "A","AB", "indication", "g4ABevents", "safetynab","g4Bevents", "safetynb"),
       leftlabs = c("Study","A","Combination", "Indication","Events","Total", "Events", "Total"),
       lab.e.attach.to.col = "safetynab",
       lab.c.attach.to.col = "safetynb",
       just.addcols = "left", 
       label.left = "<---Combination has fewer AEs---", 
       label.right = "---Comparator has fewer AEs--->", 
       fs.lr= 10
)

dev.off()

#Make a subset for the g5 analysis, where g5s are reported 
per_arm_g5_trials <- subset(trials, Isthere5data == "yes" )
g5total_patients <- sum(per_arm_g5_trials$safetyNtotals)
g5total_combination <- sum(per_arm_g5_trials$safetynab)
g5total_comparator <- sum(per_arm_g5_trials$safetynb)

per_arm_g5_trials$studylabel <- per_arm_g5_trials$`NCT ID`

per_arm_g5 <- metabin(
  g5ABevents,
  safetynab,
  g5Bevents,
  safetynb,
  studlab = studylabel,
  data = per_arm_g5_trials, 
  comb.fixed = FALSE
)

summary(per_arm_g5)

pdf(
  "g5_forest.pdf",
  18,
7.5
)

#Make a forest plot

forest(per_arm_g5,
       leftcols = c("NCT ID", "A","AB", "indication","g5ABevents", "safetynab","g5Bevents", "safetynb"),
       leftlabs = c("Study","A", "Combination","Indication", "Events","Total ", "Events", "Total"),
       lab.e.attach.to.col = "safetynab",
       lab.c.attach.to.col = "safetynb",
       just.addcols = "left",
       label.left = "<---Combination has fewer AEs---", 
       label.right = "---Comparator has fewer AEs--->", 
       fs.lr= 10
)

dev.off()

# Make a subset for the OS analysis, where the HR is not null and the SE is not null
os_analysis <- subset(trials, IsthereOSdata == "yes" )

ostotal_patients <- sum(os_analysis$efficacyNpatients)
ostotal_combination <- sum(os_analysis$n_a_b)
ostotal_comparator <- sum(os_analysis$n_b_c)

os_analysis <- os_analysis[order(os_analysis$OShazard_ratio),] 
os_meta_analysis <- metagen(
  log(OShazard_ratio),
  OS_SE,
  `NCT ID`,
  data = os_analysis,
  sm = "HR",
  comb.fixed = FALSE
)

summary(os_meta_analysis)

pdf(
  "os_forest.pdf",
  17,
  9
)

forest(
  os_meta_analysis,
  leftcols = c("NCT ID", "A","AB", "indication"),
  leftlabs = c("Study","A","Combination","Indication"),
  just.addcols = "left", 
  label.left = "<---Favouring Combination---", 
  label.right = "---Favouring Comparator--->", 
  fs.lr= 10
)

dev.off()

# Make a subset for the PFS analysis, where the HR is not null and the SE is not null
pfs_analysis <- subset(trials, IstherePFSdata == "yes" )

print (pfstotal_patients <- sum(pfs_analysis$efficacyNpatients))
print (pfstotal_combination <- sum(pfs_analysis$n_a_b))
print (pfstotal_comparator <- sum(pfs_analysis$n_b_c))
pfs_analysis <- pfs_analysis[order(pfs_analysis$PFShazard_ratio),] 
pfs_meta_analysis <- metagen(
  log(PFShazard_ratio),
  PFS_SE,
  `NCT ID`,
  data = pfs_analysis,
  sm = "HR",
  comb.fixed = FALSE
)

summary(pfs_meta_analysis)


pdf(
  "pfs_forest.pdf",
  17,
  10
)

# Plot a forest plot
forest(
  pfs_meta_analysis, 
  leftcols = c("NCT ID", "A","AB", "indication"), 
  leftlabs = c("Study","A","Combination", "Indication"),
  just.addcols = "left", 
  label.left = "<---Favouring Combination---", 
  label.right = "---Favouring Comparator--->", 
  fs.lr= 10,
  comb.random = TRUE,
  hrzl_lines=TRUE,
)



dev.off()

summary(pfs_meta_analysis) #much better to be in the combination arm 
summary(os_meta_analysis) #slightly sig in that the combination group is better
summary(per_arm_g34) #sig less likely in comparator but by alot less
summary(per_arm_g5) #sig much less likely in comparator


