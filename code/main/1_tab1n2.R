# Generates summary statistics (Tables 1 & 2)

library("here")
source(here("code", "_load_packages.R"))
source(here("code", "main", "f.R"))

if (!exists("DailyFreq")) {
    # if this file is run from the 01_Run_all.R, DailyFreq is already defined
    # if this file is run independently, run twice with DailyFreq = FALSE and TRUE
    DailyFreq <- FALSE  
}

# Load high-frequency or daily DS and prepare for summary stats
if (DailyFreq) {
    DT <- readRDS(here("data","DT4reg_daily.rds"))
} else {
    DT <- readRDS(here("data","DT4reg.rds"))
}
# drop first observation of Delta N stock series that are NA
drop <- which(is.na(DT$diffrh)); DT <- DT[-drop]

# Summary stats: DELTA N 
ss_RH <- f_sumstat(DT, var = 'diffrh', DailyFreq = DailyFreq)
ss_RET <- f_sumstat(DT, var = 'logretadj', DailyFreq = DailyFreq)
ss_RH
ss_RET

# Output TABs 1 and 2
if (!DailyFreq) {
    fwrite(round(ss_RH,2), file = here('outputs','TAB1A.txt'), sep = "\t")
    fwrite(round(ss_RET,4), file = here('outputs','TAB1B.txt'), sep = "\t")
}

# table of returns distributions by group
tmp <- f_TableRetDistByGroup(DT, DailyFreq)
tmp$quantvalues
tmp$Nobspergroup

if (!DailyFreq) {
    fwrite(round(tmp$quantvalues,2), file = here('outputs','TAB2A.txt'), sep = "\t")
    TAB2B <- tmp$Nobspergroup; TAB2B[, nobs_prct := NULL]
    fwrite(TAB2B, file = here('outputs','TAB2B.txt'), sep = "\t")
}

