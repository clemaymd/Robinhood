# Estimates main regressions (Equation (4))
# Use DailyFreq = TRUE/FALSE for daily/high-frequency settings.
# !!! IS VERY TIME CONSUMING !!!

library("here")
source(here("code", "_load_packages.R"))
source(here("code", "main", "f.R"))

if (!exists("DailyFreq")) {
    # if this file is run from the 01_Run_all.R, DailyFreq is already defined
    # if this file is run independently, run twice with DailyFreq = FALSE and TRUE
    DailyFreq <- FALSE  
}

# Load high-frequency or daily DS
if (DailyFreq) {
    DT <- readRDS(here("data","DT4reg_daily.rds"))
} else {
    DT <- readRDS(here("data","DT4reg.rds"))
}

# Prepare outputs
lagnam <- c("lag0","lag1","lag2","lag3","lag4","lag5") # L=0 to 5
n_lag <- length(lagnam)
groupnam <- levels(DT$group)
n_grp <- length(groupnam)
col_lag <- c("L=0","L=1","L=2","L=3","L=4","L=5")
out4tab <- matrix(data = NA, nrow = (n_grp*2)+2, ncol = n_lag,
                  dimnames = list(c(groupnam,
                                    paste0("t(",groupnam,")"),
                                    c("R2","Nobs")),
                                  col_lag))
out4fig <- data.table()
# Estimation of regressions all L
for(i in 0:(n_lag-1)){
    # build matrix and formula for regression
    RegInput <- f_BuildX_forreg(data = DT,
                              dummyvar = lagnam[i+1],
                              COVID = FALSE,
                              COVIDcutoff = NA,
                              OVvsINT = FALSE,
                              MKTCAP = FALSE,
                              DailyFreq = DailyFreq)

    # estimate reg 
    m <- plm(formula = RegInput$fmla,      
             index = RegInput$indexcols,
             model = "pooling", 
             data = RegInput$DT_forreg)
    # print(summary(m)) 
    adjr2 <- summary(m)$r.squared["adjrsq"]
    nobs <- nobs(m)

    # clustered standard errors
    res <- coeftest(m, vcov. = vcovHC(m, type = "HC3", cluster = "group"))

    # populate outputs
    out4tab[1:n_grp,i+1] <- res[1:n_grp,1]                  # betas
    out4tab[(n_grp+1):(n_grp*2),i+1] <- res[1:n_grp,3]      # tstat
    out4tab[c("R2","Nobs"),i+1] <- c(adjr2,nobs)   
    out4fig <- rbind(out4fig, cbind(res[1:n_grp,1], groupnam, rep(i, n_grp)))
    if(i==(n_lag-1)){names(out4fig) <- c("Estimate","RetGroup","L")}

    if(DailyFreq){print(paste0("L=",i," day(s)"))}
    if(!DailyFreq){print(paste0("L=",i," time-step(s)"))}
}
out4tab; out4fig

# Save results
if(DailyFreq){
    saveRDS(list(out4tab,out4fig), here("outputs","REGRES_MAIN_DAILY.rds"))
}else{
    saveRDS(list(out4tab,out4fig), here("outputs","REGRES_MAIN.rds"))
}

