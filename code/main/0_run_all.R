# Wrapper calling scripts 01_ to 06_ in 0_construct_variables sequentially

library("here")
source(here("code", "_load_packages.R"))
Sys.info()
sessionInfo()

DailyFreq <- TRUE
source(here("code", "main", "1_tab1n2.R"))
DailyFreq <- FALSE
source(here("code", "main", "1_tab1n2.R"))

DailyFreq <- TRUE
source(here("code", "main", "2_regs_main.R"))
DailyFreq <- FALSE
source(here("code", "main", "2_regs_main.R"))

DailyFreq <- TRUE
source(here("code", "main", "3_tabnfig_main.R"))
DailyFreq <- FALSE
source(here("code", "main", "3_tabnfig_main.R"))

OVvsINT <- TRUE; COVID <- FALSE; MKTCAP <- FALSE  
source(here("code", "main", "4_regs_subgroup.R"))
OVvsINT <- FALSE; COVID <- TRUE; MKTCAP <- FALSE  
source(here("code", "main", "4_regs_subgroup.R"))
OVvsINT <- FALSE; COVID <- FALSE; MKTCAP <- TRUE  
source(here("code", "main", "4_regs_subgroup.R"))

OVvsINT <- TRUE; COVID <- FALSE; MKTCAP <- FALSE 
source(here("code", "main", "5_regs_subgroup_testhyp.R"))
OVvsINT <- FALSE; COVID <- TRUE; MKTCAP <- FALSE  
source(here("code", "main", "5_regs_subgroup_testhyp.R"))
OVvsINT <- FALSE; COVID <- FALSE; MKTCAP <- TRUE  
source(here("code", "main", "5_regs_subgroup_testhyp.R"))

OVvsINT <- TRUE; COVID <- FALSE; MKTCAP <- FALSE 
source(here("code", "main", "6_tabnfig_subgroup.R"))
OVvsINT <- FALSE; COVID <- TRUE; MKTCAP <- FALSE
source(here("code", "main", "6_tabnfig_subgroup.R"))
OVvsINT <- FALSE; COVID <- FALSE; MKTCAP <- TRUE  
source(here("code", "main", "6_tabnfig_subgroup.R"))
