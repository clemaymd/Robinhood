# Generates Tables 5–7 and Figures 3–5 from regression and hypothesis test results
# Use OVvsINT, COVID, and MKTCAP options.

library("here")
source(here("code", "_load_packages.R"))
source(here("code", "main", "f.R"))

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

if(OVvsINT){filenam <- c("REGRES_OVINT_TESTHYP.rds")}
if(COVID){filenam <- c("REGRES_COVID_TESTHYP.rds")}
if(MKTCAP){filenam <- c("REGRES_MKTCAP_TESTHYP.rds")}
DT <- readRDS(here("outputs",filenam))

# prepare output
if(OVvsINT | COVID){n_ctg <- 2}else{n_ctg <- 3}
DT4TAB <- DT[(length(DT)-2):length(DT)]

# Tables
panels <- c("A","B","C")
for(i in 1:length(DT4TAB)){
    if(n_ctg == 2){
        pv <- DT4TAB[[i]][4:6,]
        signif  <- symnum(pv, corr = FALSE, na = FALSE,
                          cutpoints = c(0, 0.01, 0.05, 0.1, 1),
                          symbols = c("***", "**", "*", " "))
        out <- matrix(paste0(round(DT4TAB[[i]][1:3,],2),signif),
                      nrow = 3,
                      ncol = ncol(DT4TAB[[i]]),
                      dimnames = NULL)
        out <- data.table(x=rownames(DT4TAB[[i]])[1:3], out)
        if(i %in% c(1,2)){names(out)[-1] <- paste0("L=",0:5)}
        
        if(OVvsINT){
            outnam <- paste0('TAB5',panels[i],'.txt')
            fwrite(out, file=here('outputs',outnam), sep = "\t")
        }
        if(COVID){
            outnam <- paste0('TAB6',panels[i],'.txt')
            fwrite(out, file=here('outputs',outnam), sep = "\t")
        }
    }else{
        pv <- DT4TAB[[i]][7:12,]
        signif  <- symnum(pv, corr = FALSE, na = FALSE,
                          cutpoints = c(0, 0.01, 0.05, 0.1, 1),
                          symbols = c("***", "**", "*", " "))
        out <- matrix(paste0(round(DT4TAB[[i]][1:6,],2),signif),
                      nrow = 6,
                      ncol = ncol(DT4TAB[[i]]),
                      dimnames = NULL)
        out <- data.table(x=rownames(DT4TAB[[i]])[1:6], out)
        if(i %in% c(1,2)){names(out)[-1] <- paste0("L=",0:5)}
        
        outnam <- paste0('TAB7',panels[i],'.txt')
        fwrite(out, file=here('outputs',outnam), sep = "\t")
    }
}

# Figures 
dt4fig <- DT[[2]]
dt4fig[, RetGroup:=as.character(RetGroup)]

if(OVvsINT){
    OV <- which(grepl("OV",dt4fig$RetGroup, fixed = T))
    INT <- which(grepl("INT",dt4fig$RetGroup, fixed = T))
    dt4fig[, CRIT := 1]; dt4fig[OV, CRIT := 0]
    dt4fig[OV, RetGroup := substr(RetGroup, 1, nchar(RetGroup)-3)]
    dt4fig[INT, RetGroup := substr(RetGroup, 1, nchar(RetGroup)-4)]
}
if(COVID){
    COV0 <- which(grepl("COV0",dt4fig$RetGroup, fixed = T))
    COV1 <- which(grepl("COV1",dt4fig$RetGroup, fixed = T))
    dt4fig[COV0, CRIT := 0]; dt4fig[COV1, CRIT := 1]
    dt4fig[, RetGroup := substr(RetGroup, 1, nchar(RetGroup)-5)]
}
if(MKTCAP){
    SMA <- which(grepl("SMA",dt4fig$RetGroup, fixed = T))
    MID <- which(grepl("MID",dt4fig$RetGroup, fixed = T))
    LAR <- which(grepl("LAR",dt4fig$RetGroup, fixed = T))
    dt4fig[SMA, CRIT := 0]; dt4fig[MID, CRIT := 1]; dt4fig[LAR, CRIT := 2]
    dt4fig[, RetGroup := substr(RetGroup, 1, nchar(RetGroup)-4)]
}
groupnam <- unique(dt4fig$RetGroup)
n_grp <- length(groupnam)
dt4fig[, RetGroup := ordered(factor(RetGroup), levels = groupnam)]
dt4fig[, Estimate := as.numeric(Estimate)]

## Generate Plots
out <- f_Plot_Result_Subgroup(data = dt4fig, 
                            OVvsINT = OVvsINT, 
                            COVID = COVID, 
                            MKTCAP = MKTCAP, 
                            SameScale = TRUE)

# print plots
# as_ggplot(arrangeGrob(out$p_left[[1]], out$p_right[[1]], nrow=1))
# as_ggplot(arrangeGrob(out$p_left[[2]], out$p_right[[2]], nrow=1)) 
# as_ggplot(arrangeGrob(out$p_left[[3]], out$p_right[[3]], nrow=1)) 

# Save pair of plots in output
p <- vector("list", length = n_ctg)
dim_p <- data.table(width=rep(9,n_ctg),
                    height=c(rep(3.5,(n_ctg-1)),4),
                    dpi=rep(700,n_ctg))
for(i in 1:n_ctg){
    p[[i]] <- as_ggplot(arrangeGrob(out$p_left[[i]], out$p_right[[i]], nrow=1))
    if(OVvsINT){pnam <- "FIG3"}
    if(COVID){pnam <- "FIG4"}
    if(MKTCAP){pnam <- "FIG5"}
    ggsave(plot = p[[i]], 
           filename = here("outputs",paste0(pnam,panels[i],".png")),
           width = dim_p$width[i], height = dim_p$height[i], dpi = 700)
}
