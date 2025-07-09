# Estimates subgroup regressions (Equation (5))
# Use OVvsINT, COVID, and MKTCAP options.
# !!! IS VERY TIME CONSUMING !!!

library("here")
source(here("code", "_load_packages.R"))
source(here("code", "main", "f.R"))

DT <- readRDS(here("data","DT4reg.rds"))

# choose subgroup
if (!exists("OVvsINT")) {
    # if this file is run from the 01_Run_all.R, OVvsINT is already defined
    # if this file is run independently, run twice with OVvsINT = FALSE and TRUE
    OVvsINT <- TRUE  # Overnight vs Intraday
}

if (!exists("COVID")) {
    # if this file is run from the 01_Run_all.R, COVID is already defined
    # if this file is run independently, run twice with COVID = FALSE and TRUE
    COVID <- TRUE  # Overnight vs Intraday
}

if (!exists("MKTCAP")) {
    # if this file is run from the 01_Run_all.R, MKTCAP is already defined
    # if this file is run independently, run twice with MKTCAP = FALSE and TRUE
    MKTCAP <- TRUE  # Overnight vs Intraday
}

if(COVID){COVIDcutoff<-as.Date("2020-03-11")}else{COVIDcutoff<-NA}

# if subgroup is MKTCAP, reduce sample to companies with available mktcap info
if(MKTCAP){
    infomktcap <- readRDS(here("data","db_mktcap.rds"))
    nsec_bef <- length(unique(DT$RH_symbol))
    DT <- merge(x=DT, y=infomktcap, by = c("date","RH_symbol"))
    nsec_aft <- length(unique(DT$RH_symbol))
    print(paste(nsec_bef-nsec_aft, "securities removed (no mkt cap info)"))
}

# Prepare outputs
lagnam <- c("lag0","lag1","lag2","lag3","lag4","lag5") # L=0 to 5
n_lag <- length(lagnam)
groupnam_tmp <- levels(DT$group)
if(OVvsINT){
    groupnam <- c(paste0(groupnam_tmp, ":OV"),
                  paste0(groupnam_tmp, ":INT"))
    CRIT <- "OVvsINT"
}
if(COVID){
    groupnam <- c(paste0(groupnam_tmp, ":COV0"),
                  paste0(groupnam_tmp, ":COV1"))
    CRIT <- "COVID"
}
if(MKTCAP){
    sizelist <- levels(DT$SIZE)
    groupnam <- c()
    for(s in 1:length(sizelist)){
        groupnam <- c(groupnam, paste0(groupnam_tmp, ":", sizelist[s]))
    }
    CRIT <- "MKTCAP"
}
n_grp <- length(groupnam)
col_lag <- c("L=0","L=1","L=2","L=3","L=4","L=5")
out4tab <- matrix(data = NA, nrow = (n_grp*2)+2, ncol = n_lag,
                  dimnames = list(c(groupnam,
                                    paste0("t(",groupnam,")"),
                                    c("R2","Nobs")),
                                  col_lag))
out4fig <- data.table()
Allcoeff <- AllCOV <- c()
# Estimation of regressions all L
for(i in 0:(n_lag-1)){
    # build matrix and formula for regression
    RegInput <- f_BuildX_forreg(data = DT,
                                dummyvar = lagnam[i+1],
                                COVID = COVID,
                                COVIDcutoff = COVIDcutoff,
                                OVvsINT = OVvsINT,
                                MKTCAP = MKTCAP,
                                DailyFreq = F)
    # estimate reg 
    m <- plm(formula=RegInput$fmla,      
             index = RegInput$indexcols,
             model = "pooling", 
             data = RegInput$DT_forreg)
    # print(summary(m)) 
    adjr2 <- summary(m)$r.squared["adjrsq"]
    nobs <- nobs(m)
    
    # clustered standard errors + store VCOV matrix to use later for hyp. tests
    if(OVvsINT){pars <- which(grepl("OVINT", names(m$coefficients), fixed=T))}
    if(COVID){pars <- which(grepl("COV", names(m$coefficients), fixed=T))}
    if(MKTCAP){pars <- which(grepl("SIZE", names(m$coefficients), fixed=T))}
    VCOV_i <- vcovHC(m, type="HC3", cluster= "group")
    res <- coeftest(m, vcov = VCOV_i)

    # Also store all coefs to use later for hyp. tests
    coeff_i <- res[,"Estimate"]
    names(coeff_i) <- paste0("L",i,".",names(coeff_i))
    Allcoeff <- c(Allcoeff,coeff_i)
    
    # Stack the COV matrices for each reg, to use later for hyp. tests
    dimVCOV <- dim(VCOV_i)[1]
    zero_COV <- matrix(0, nrow = dimVCOV, ncol = dimVCOV)
    if(i==0){
        COV_row_i <- cbind(VCOV_i, zero_COV, zero_COV, zero_COV, zero_COV, zero_COV)
    }else if(i==1){
        COV_row_i <- cbind(zero_COV, VCOV_i, zero_COV, zero_COV, zero_COV, zero_COV)
    }else if(i==2){
        COV_row_i <- cbind(zero_COV, zero_COV, VCOV_i, zero_COV, zero_COV, zero_COV)
    }else if(i==3){
        COV_row_i <- cbind(zero_COV, zero_COV, zero_COV, VCOV_i, zero_COV, zero_COV)
    }else if(i==4){
        COV_row_i <- cbind(zero_COV, zero_COV, zero_COV, zero_COV, VCOV_i, zero_COV)
    }else if(i==5){
        COV_row_i <- cbind(zero_COV, zero_COV, zero_COV, zero_COV, zero_COV, VCOV_i)
    }
    AllCOV <- rbind(AllCOV, COV_row_i)
    
    # populate output: betas and CI
    out4tab[1:n_grp,i+1] <- res[pars,1]                  # betas
    out4tab[(n_grp+1):(n_grp*2),i+1] <- res[pars,3]      # tstat
    out4tab[c("R2","Nobs"),i] <- c(adjr2,nobs)   
    
    out4fig <- rbind(out4fig, cbind(res[pars,1], groupnam, rep(i, n_grp)))
    if(i==(n_lag-1)){names(out4fig) <- c("Estimate","RetGroup","L")}
    
    print(paste0("Subgroup Reg= ",CRIT," / L=",i," time-step(s)"))
}
out4tab; out4fig

# Save results
if(OVvsINT){outnam <- c("REGRES_OVINT.rds")}
if(COVID){outnam <- c("REGRES_COVID.rds")}
if(MKTCAP){outnam <- c("REGRES_MKTCAP.rds")}
saveRDS(list(out4tab,out4fig,Allcoeff,AllCOV), here("outputs",outnam))
