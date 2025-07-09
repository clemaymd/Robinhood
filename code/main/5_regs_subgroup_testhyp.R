# Performs hypothesis testing on the three identified behaviors
# Use OVvsINT, COVID, and MKTCAP options.
# !!! IS VERY TIME CONSUMING !!!

library("here")
source(here("code", "_load_packages.R"))

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

if(OVvsINT){filenam <- c("REGRES_OVINT.rds")}
if(COVID){filenam <- c("REGRES_COVID.rds")}
if(MKTCAP){filenam <- c("REGRES_MKTCAP.rds")}
DT <- readRDS(here("outputs",filenam))

Allcoeff <- DT[[3]]; AllCOV <- DT[[4]]
n_lag <- ncol(DT[[1]])

# give COV matrix same names as coeff
dimnames(AllCOV) <- list(names(Allcoeff), names(Allcoeff))
if(OVvsINT){pars <- which(grepl("OVINT", names(Allcoeff), fixed = T))}
if(COVID){pars <- which(grepl("COV", names(Allcoeff), fixed = T))}
if(MKTCAP){pars <- which(grepl("SIZE", names(Allcoeff), fixed = T))}
Bnames <- names(Allcoeff)[pars]

# prepare output
if(OVvsINT){
    n_ctg <- 2
    rnam <- c("OV","INT","diff", "pval.OV","pval.INT","pval.diff")}
if(COVID){
    n_ctg <- 2
    rnam <- c("preCOV","postCOV","diff", "pval.preCOV","pval.postCOV","pval.diff")}
if(MKTCAP){
    n_ctg <- 3
    ctgnam <- c("SMA","MID","LAR")
    rnam1 <- c(ctgnam, c("MID-SMA","LAR-MID","LAR-SMA"))
    rnam2 <- paste0("pval.",rnam1)
    rnam <- c(rnam1,rnam2)
}

## (1) Test behavior #1
B1 <- matrix(data=NA, nrow=length(rnam), ncol=n_lag, dimnames=list(rnam,0:5))
# (1.1) individual tests: significance for each category and each lag 
for(c in 1:n_ctg){ 
    for(i in 1:n_lag){  
        idxL <- 1+n_lag*(c-1) + (n_lag*n_ctg*(i-1)) # idx of the <5% estimate
        idxH <- idxL + 5                            # idx of the >95% estimate
        # 'value' of the behavior
        B1[c,i] <- (0.5*(Allcoeff[Bnames[idxL]]+Allcoeff[Bnames[idxH]])
                    - 0.5*(Allcoeff[Bnames[idxL+2]]+Allcoeff[Bnames[idxL+3]]))
        # formulate and test the hypothesis
        h0 <- paste0("0.5*",Bnames[idxL], "+0.5*",Bnames[idxH], " = ",
                     "0.5*",Bnames[idxL+2],"+0.5*",Bnames[idxL+3])
        idx_pval <- length(rnam)/2 + c
        B1[idx_pval,i] <- linearHypothesis(model=NULL,
                                           coef.=Allcoeff,
                                           vcov.=AllCOV,
                                           hypothesis.matrix=h0)$`Pr(>Chisq)`[2]
    }
}
# (1.2) pairwise tests: test significance of the difference between categories
if(n_ctg == 2){   
    # if 2 categories, e.g OVvsINT, COVID, only one pair of difference
    for(i in 1:n_lag){
        B1[3,i] <- B1[1,i] - B1[2,i] # value of the difference
        idxL <- 1+(n_lag*n_ctg*(i-1))  
        idxH <- idxL+5
        # formulate and test the hypothesis
        h0 <- paste0("0.5*",Bnames[idxL], "+0.5*",Bnames[idxH],
                     " - 0.5*",Bnames[idxL+2]," - 0.5*",Bnames[idxL+3],
                     " = ",
                     "0.5*",Bnames[idxL+6], "+0.5*",Bnames[idxH+6],
                     " - 0.5*",Bnames[idxL+8]," - 0.5*",Bnames[idxL+9])
        B1[6,i] <- linearHypothesis(model=NULL,
                                    coef.=Allcoeff,
                                    vcov.=AllCOV, 
                                    hypothesis.matrix=h0)$`Pr(>Chisq)`[2]
    }
}else if(n_ctg == 3){
    # if 3 categories, e.g MKTCAP, => 3 pairs of difference
    for(i in 1:n_lag){
        B1[4,i] <- B1[2,i] - B1[1,i]  # mid - small
        B1[5,i] <- B1[3,i] - B1[2,i]  # large - mid
        B1[6,i] <- B1[3,i] - B1[1,i]  # large - small

        idxL_SMA <- 1+(n_lag*n_ctg*(i-1))  
        idxL_MID <- idxL_SMA+6  
        idxL_LAR <- idxL_MID+6
        idxH_SMA <- idxL_SMA+5
        idxH_MID <- idxL_MID+5
        idxH_LAR <- idxL_LAR+5
        
        # formulate and test the hypotheses
        SMA <- paste0("0.5*",Bnames[idxL_SMA],"+0.5*",Bnames[idxH_SMA],
                      " - 0.5*",Bnames[idxL_SMA+2]," - 0.5*",Bnames[idxL_SMA+3])
        MID <- paste0("0.5*",Bnames[idxL_MID],"+0.5*",Bnames[idxH_MID],
                      " - 0.5*",Bnames[idxL_MID+2]," - 0.5*",Bnames[idxL_MID+3])
        LAR <- paste0("0.5*",Bnames[idxL_LAR],"+0.5*",Bnames[idxH_LAR],
                      " - 0.5*",Bnames[idxL_LAR+2]," - 0.5*",Bnames[idxL_LAR+3])
        h0_MIDmSMA <- paste0(MID, " = ", SMA)
        h0_LARmMID <- paste0(LAR, " = ", MID)
        h0_LARmSMA <- paste0(LAR, " = ", SMA)
        B1[10,i] <- linearHypothesis(model=NULL,
                                     coef.=Allcoeff,
                                     vcov.=AllCOV,
                                     hypothesis.matrix=h0_MIDmSMA)$`Pr(>Chisq)`[2]
        B1[11,i] <- linearHypothesis(model=NULL,
                                      coef.=Allcoeff,
                                      vcov.=AllCOV,
                                      hypothesis.matrix=h0_LARmMID)$`Pr(>Chisq)`[2]
        B1[12,i] <- linearHypothesis(model=NULL,
                                      coef.=Allcoeff,
                                      vcov.=AllCOV,
                                      hypothesis.matrix=h0_LARmSMA)$`Pr(>Chisq)`[2]
    }
}

## (2) Test behavior #2
B2 <- B1; B2[] <- NA
# (2.1) individual tests: significance for each category and each lag 
for(c in 1:n_ctg){ 
    for(i in 1:n_lag){  
        idxL <- 1+n_lag*(c-1) + (n_lag*n_ctg*(i-1)) # idx of the <5% estimate
        idxH <- idxL + 5                            # idx of the >95% estimate
        # 'value' of the behavior
        B2[c,i] <- (Allcoeff[Bnames[idxL]] - Allcoeff[Bnames[idxH]])
        # formulate and test the hypothesis
        h0 <- paste0(Bnames[idxL], " = ", Bnames[idxH])
        idx_pval <- length(rnam)/2 + c
        B2[idx_pval,i] <- linearHypothesis(model=NULL,
                                           coef.=Allcoeff,
                                           vcov.=AllCOV,
                                           hypothesis.matrix = h0)$`Pr(>Chisq)`[2]
    }
}
# (2.2) pairwise tests: test significance of the difference between categories
if(n_ctg == 2){   
    # if 2 categories, e.g OVvsINT, COVID, only one pair of difference
    for(i in 1:n_lag){
        B2[3,i] <- B2[1,i] - B2[2,i] # value of the difference
        idxL <- 1+(n_lag*n_ctg*(i-1))  
        idxH <- idxL+5
        # formulate and test the hypothesis
        h0 <- paste0(Bnames[idxL],"-",Bnames[idxH],
                     "= ",
                     Bnames[idxL+6],"-",Bnames[idxH+6])
        B2[6,i] <- linearHypothesis(model=NULL,
                                    coef.=Allcoeff,
                                    vcov.=AllCOV, 
                                    hypothesis.matrix=h0)$`Pr(>Chisq)`[2]
    }
}else if(n_ctg == 3){
    # if 3 categories, e.g MKTCAP, => 3 pairs of difference
    for(i in 1:n_lag){
        B2[4,i] <- B2[2,i] - B2[1,i]  # mid - small
        B2[5,i] <- B2[3,i] - B2[2,i]  # large - mid
        B2[6,i] <- B2[3,i] - B2[1,i]  # large - small
        
        idxL_SMA <- 1+(n_lag*n_ctg*(i-1))  
        idxL_MID <- idxL_SMA+6  
        idxL_LAR <- idxL_MID+6
        idxH_SMA <- idxL_SMA+5
        idxH_MID <- idxL_MID+5
        idxH_LAR <- idxL_LAR+5
        
        
        #test difference
        SMA <- paste0(Bnames[idxL_SMA]," - ",Bnames[idxH_SMA])
        MID <- paste0(Bnames[idxL_MID]," - ",Bnames[idxH_MID])
        LAR <- paste0(Bnames[idxL_LAR]," - ",Bnames[idxH_LAR])
        h0_MIDmSMA <- paste0(MID, " = ", SMA)
        h0_LARmMID <- paste0(LAR, " = ", MID)
        h0_LARmSMA <- paste0(LAR, " = ", SMA)
        B2[10,i] <- linearHypothesis(model=NULL,
                                      coef.=Allcoeff,
                                      vcov.=AllCOV,
                                      hypothesis.matrix=h0_MIDmSMA)$`Pr(>Chisq)`[2] 
        B2[11,i] <- linearHypothesis(model=NULL,
                                      coef.=Allcoeff,
                                      vcov.=AllCOV,
                                      hypothesis.matrix=h0_LARmMID)$`Pr(>Chisq)`[2] 
        B2[12,i] <- linearHypothesis(model=NULL,
                                      coef.=Allcoeff,
                                      vcov.=AllCOV,
                                      hypothesis.matrix=h0_LARmSMA)$`Pr(>Chisq)`[2]
    } 
}

## (3) Test behavior #3
B3 <- matrix(data = NA, nrow = length(rnam), ncol = 1, dimnames = list(rnam,NULL))
# (3.1) individual tests: significance for each category and each lag 
for(c in 1:n_ctg){
    idx1 <- n_lag*n_ctg + 1 + n_lag*(c-1) # COMMENT HERE
    idx2 <- idx1 + n_lag*n_ctg*4          # COMMENT HERE
    
    B3[c,] <- (Allcoeff[Bnames[idx1]] - Allcoeff[Bnames[idx2]])
    h0 <- paste0(Bnames[idx1], " = ", Bnames[idx2]) 
    idx_pval <- length(rnam)/2 + c
    B3[idx_pval,] <- linearHypothesis(model=NULL,
                                      coef.=Allcoeff,
                                      vcov.=AllCOV,
                                      hypothesis.matrix=h0)$`Pr(>Chisq)`[2]

}
# (3.2) pairwise tests: test significance of the difference between categories
if(n_ctg == 2){   
    B3[3,] <- B3[1,] - B3[2,]
    idx1 <- n_lag*n_ctg+1
    idx2 <- idx1 + n_lag*n_ctg*4
    idx3 <- idx1 + n_lag
    idx4 <- idx3 + n_lag*n_ctg*4
    # formulate and test the hypothesis
    h0 <- paste0(Bnames[idx1],"-",Bnames[idx2],
                 "= ",
                 Bnames[idx3],"-",Bnames[idx4])
    B3[6,] <- linearHypothesis(model=NULL, 
                               coef.=Allcoeff, 
                               vcov.=AllCOV, 
                               hypothesis.matrix=h0)$`Pr(>Chisq)`[2]
}else if(n_ctg == 3){
    # if 3 categories, e.g MKTCAP, => 3 pairs of difference
    B3[4,] <- B3[2,] - B3[1,]  # mid - small
    B3[5,] <- B3[3,] - B3[2,]  # large - mid
    B3[6,] <- B3[3,] - B3[1,]  # large - small
    #test difference
    idx_SMA1 <- n_lag*n_ctg+1
    idx_SMA2 <- idx_SMA1 + n_lag*n_ctg*4
    idx_MID1 <- idx_SMA1 + n_lag
    idx_MID2 <- idx_MID1 + n_lag*n_ctg*4
    idx_LAR1 <- idx_MID1 + n_lag
    idx_LAR2 <- idx_LAR1 + n_lag*n_ctg*4
    SMA <- paste0(Bnames[idx_SMA1]," - ",Bnames[idx_SMA2])
    MID <- paste0(Bnames[idx_MID1]," - ",Bnames[idx_MID2])
    LAR <- paste0(Bnames[idx_LAR1]," - ",Bnames[idx_LAR2])
    h0_MIDmSMA <- paste0(MID, " = ", SMA)
    h0_LARmMID <- paste0(LAR, " = ", MID)
    h0_LARmSMA <- paste0(LAR, " = ", SMA)
    B3[10,] <- linearHypothesis(model=NULL, 
                                coef.=Allcoeff, 
                                vcov.=AllCOV, 
                                hypothesis.matrix=h0_MIDmSMA)$`Pr(>Chisq)`[2]
    B3[11,] <- linearHypothesis(model=NULL, 
                                coef.=Allcoeff, 
                                vcov.=AllCOV, 
                                hypothesis.matrix=h0_LARmMID)$`Pr(>Chisq)`[2]
    B3[12,] <- linearHypothesis(model=NULL, 
                                coef.=Allcoeff, 
                                vcov.=AllCOV, 
                                hypothesis.matrix=h0_LARmSMA)$`Pr(>Chisq)`[2]
}

# Save Results
DT[[length(DT)+1]] <- B1; DT[[length(DT)+1]] <- B2; DT[[length(DT)+1]] <- B3

if(OVvsINT){outnam <- c("REGRES_OVINT_TESTHYP.rds")}
if(COVID){outnam <- c("REGRES_COVID_TESTHYP.rds")}
if(MKTCAP){outnam <- c("REGRES_MKTCAP_TESTHYP.rds")}
saveRDS(DT, here("outputs",outnam))
