# Contains all the functions for 1_perform_analyses

# ----------------------------------------------------------------------------
f_sumstat <- function(DT, var, DailyFreq=c(TRUE,FALSE)){
    
    if(DailyFreq){
        out <- DT[,list(mean=mean(get(var)),
                        std=sd(get(var)),
                        P5 = quantile(get(var), prob = 0.05),
                        Q1=quantile(get(var), prob = 0.25),
                        median=median(get(var)),
                        Q3=quantile(get(var), prob = 0.75),
                        P95 = quantile(get(var), prob = 0.95),
                        Nobs=.N,
                        T=length(unique(date)),
                        Nstock=length(unique(RH_symbol)))]
    }else{
        # split intraday and overnight
        out1 <- DT[,list(mean=mean(get(var)),
                         std=sd(get(var)),
                         P5 = quantile(get(var), prob = 0.05),
                         Q1=quantile(get(var), prob = 0.25),
                         median=as.double(median(get(var))),
                         Q3=quantile(get(var), prob = 0.75),
                         P95 = quantile(get(var), prob = 0.95),
                         Nobs=.N,
                         T=length(unique(date)),
                         Nstock=length(unique(RH_symbol))),
                   by=type]
        # whole series
        out2 <- DT[,list(mean=mean(get(var)),
                         std=sd(get(var)),
                         P5 = quantile(get(var), prob = 0.05),
                         Q1=quantile(get(var), prob = 0.25),
                         median=median(get(var)),
                         Q3=quantile(get(var), prob = 0.75),
                         P95 = quantile(get(var), prob = 0.95),
                         Nobs=.N,
                         T=length(unique(date)),
                         Nstock=length(unique(RH_symbol)))]
        out <- rbind(out1, cbind(data.table(type = "all"),out2))
        rownam <- out$type
        out[, "type" := NULL]
    }    

    if(!DailyFreq){rownames(out) <- rownam}
    
    return(out)
}

# ----------------------------------------------------------------------------
f_TableRetDistByGroup <- function(data, DailyFreq){
    
    if(DailyFreq){
        drop <- which(is.na(data[, "group"]))
        if(length(drop) != 0){data <- data[-drop]}
        n_row <- nrow(data)
        freq <- data[, list(nobs = .N, nobs_prct= .N/n_row*100), by="group"]
        freq <- freq[order(group)]
    }else{
        # all
        drop <- which(is.na(data[, "group"]))
        if(length(drop) != 0){data <- data[-drop]}
        n_row <- nrow(data)
        freq <- data[, list(nobs = .N, nobs_prct= .N/n_row*100,type="all"),by="group"]
        freq <- freq[order(group)]
        
        # decompose by intraday or overnight 
        typeret <- c("intraday","overnight")
        for(i in 1:2){
            data_i <- data[type == typeret[i]]
            drop <- which(is.na(data_i[, "group"]))
            if(length(drop) != 0){data_i <- data_i[-drop]}
            n_row <- nrow(data_i)
            freq_i <- data_i[, list(nobs = .N,nobs_prct= .N/n_row*100,type=typeret[i]),by="group"]
            freq_i <- freq_i[order(group)]
            freq <- rbind(freq, freq_i)
        }    
    }
    
    # identify cutoff quantiles
    cutoffs <- c()
    q_seq <- c(5,25,NA,75,95)
    for(i in 1:length(levels(data$group))-1){
        cutoffs[i] <- quantile(data[, logretadj], probs = q_seq[i]/100, na.rm=T)
    }
    cutoffs[which(is.na(cutoffs))] <- 0
    cutoffs <- data.table(cutoffs); names(cutoffs) <- "Higher.Bound"
    cutoffs[, G:=1:(length(levels(data$group))-1)]; setcolorder(cutoffs, "G")
    return(list(Nobspergroup=freq, quantvalues=cutoffs))
}

# --------------------------------------------------------------------------
f_BuildX_forreg <- function(data,
                           dummyvar=c("lag0","lag1","lag2","lag3","lag4","lag5"),
                           COVID = c(TRUE,FALSE),
                           COVIDcutoff=NA,    
                           OVvsINT = c(TRUE,FALSE),
                           MKTCAP = c(TRUE,FALSE),
                           DailyFreq = c(TRUE,FALSE)){
    
    # setup
    groupnam <- levels(data[, lag1.group])
    refcol_grp <- paste0(dummyvar,".group")
    k <- as.numeric(substr(dummyvar, nchar(dummyvar), nchar(dummyvar)))
    
    # create columns of control vars
    collag <- collagsqr <- collagSPY <- collagSPYsqr <- c()
    for(i in 0:5){
        # SPY lag ret squared
        data[, c(paste0("SPY.lag",i,"squared")) := get(paste0("SPY.lag",i))^2]
        collagSPY <- c(collagSPY, paste0("SPY.lag",i))
        collagSPYsqr <- c(collagSPYsqr, paste0("SPY.lag",i,"squared"))
        # STOCK lag ret squared
        if(i != k){
            data[, c(paste0("lag",i,"squared")) := get(paste0("lag",i))^2]
            collag <- c(collag, paste0("lag",i))
            collagsqr <- c(collagsqr, paste0("lag",i,"squared"))
        }
    }
    xcols <- c(refcol_grp, collag, collagsqr, collagSPY, collagSPYsqr)

    # create factor based on the defined criterion, if applicable
    if(OVvsINT){
        data[get(paste0(dummyvar,".type")) == "overnight", OVINT:= 0]
        data[get(paste0(dummyvar,".type")) == "intraday", OVINT:= 1]
        data[, OVINT := as.factor(OVINT)]
        xcols <- c(xcols, "OVINT")
    }
    if(COVID){
        data[date <= COVIDcutoff, COV:=0]
        data[date > COVIDcutoff, COV:=1]
        data[, COV := as.factor(COV)]
        xcols <- c(xcols, "COV")
    }
    if(MKTCAP){xcols <- c(xcols, "SIZE")}
    # Panel identifiers: t is day for daily, t is Date-time for HF
    if(DailyFreq){
        indexcols <- c("RH_symbol","date")
    } else {
        data <- data[type == "overnight", idx := 1]
        data <- data[type == "intraday", idx := (1:.N)+1, by=c("date","RH_symbol")]
        data[, DateTime := paste0(date, "-", idx)]
        indexcols <- c("RH_symbol","DateTime")
    }
    # save DS and formula to use for estimation
    X_reg <- data[, .SD, .SDcols=c(indexcols, 'diffrh', xcols)]
    fmla <- formula(X_reg[, c('diffrh',..xcols)])
    fmla <- update(fmla , ~. +0) # no intercept
    if(OVvsINT){
        # remove individual lag.group factors and keep interaction terms
        # between lag.group factors and OVINT
        fmla <- update(fmla , 
                       paste0("~. - OVINT - ",
                              paste0(dummyvar,".group"),
                              " + ",
                              paste0(dummyvar,".group:OVINT")))
    }
    if(COVID){ 
        # remove individual lag.group factors and keep interaction terms
        # between lag.group factors and COV
        fmla <- update(fmla , 
                       paste0("~. - COV - ",
                              paste0(dummyvar,".group"),
                              " + ",
                              paste0(dummyvar,".group:COV")))
    }
    if(MKTCAP){
        # remove individual lag.group factors and keep interaction terms
        # between lag.group factors and SIZE
        fmla <- update(fmla , 
                       paste0("~. - SIZE - ",
                              paste0(dummyvar,".group"),
                              " + ",
                              paste0(dummyvar,".group:SIZE ")))
    }    
    X_reg <- na.omit(X_reg) # get rid of first observations where ret is NA
    
    # use demeaned control variables
    # stocks lags control demeaned
    for(i in 0:5){
        if(i != k){
            X_reg[, c(paste0("lag",i)) := 
                     get(paste0("lag",i))-mean(get(paste0("lag",i)),na.rm=T), 
                 by=RH_symbol]
            X_reg[, c(paste0("lag",i,"squared")) :=
                      get(paste0("lag",i,"squared"))-mean(get(paste0("lag",i,"squared")),na.rm=T), 
                  by=RH_symbol]
        }
    }
    # SPY controls demeaned
    X_reg[, SPY.lag0:=SPY.lag0-mean(SPY.lag0,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag1:=SPY.lag1-mean(SPY.lag1,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag2:=SPY.lag2-mean(SPY.lag2,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag3:=SPY.lag3-mean(SPY.lag3,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag4:=SPY.lag4-mean(SPY.lag4,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag5:=SPY.lag5-mean(SPY.lag5,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag0squared:=SPY.lag0squared-mean(SPY.lag0squared,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag1squared:=SPY.lag1squared-mean(SPY.lag1squared,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag2squared:=SPY.lag2squared-mean(SPY.lag2squared,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag3squared:=SPY.lag3squared-mean(SPY.lag3squared,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag4squared:=SPY.lag4squared-mean(SPY.lag4squared,na.rm=T), by=RH_symbol]
    X_reg[, SPY.lag5squared:=SPY.lag5squared-mean(SPY.lag5squared,na.rm=T), by=RH_symbol]

    return(list(DT_forreg = X_reg, fmla = fmla, indexcols = indexcols))
}

# --------------------------------------------------------------------------
f_Plot_Result_Subgroup <- function(data,
                                   OVvsINT=c(TRUE,FALSE),
                                   COVID=c(TRUE,FALSE),
                                   MKTCAP=c(TRUE,FALSE),
                                   SameScale=c(TRUE)){
    
    # Preset Scale
    maxy <- ceiling(max(data$Estimate))
    miny <- floor(min(data$Estimate))
    
    n_ctg <- length(unique(data$CRIT))
    p_left <- p_right <- vector(mode = "list", length = n_ctg)
    
    # plots without legend for all but last level
    for(i in 1:n_ctg){
        
        if(SameScale==FALSE){
            maxy <- ceiling(max(data[CRIT==(i-1)]$Estimate))
            miny <- floor(min(data[CRIT==(i-1)]$Estimate))
        } 
        
        p_left[[i]] <- ggplot(data[CRIT==(i-1)], 
                              aes(x = RetGroup, y=Estimate, group=L, colour=L, 
                                  linetype=L, shape=L)) +
            geom_line(linewidth=0.75) +
            geom_point(size=3) +
            ylim(miny,maxy) +
            labs(title = NULL, y = NULL, x = "Return Group", colour="Time-Lag") +
            theme_bw() +
            theme(legend.position="bottom", 
                  legend.text = element_text(size = 8),
                  legend.title = element_text(size=8),
                  legend.key.size = unit(0.05, 'cm'),
                  panel.grid = element_blank()) + 
            guides(color=guide_legend(nrow = 1), shape = "none", linetype="none") + 
            scale_x_discrete(expand = c(0.05, 0.05)) +
            scale_color_manual(values=c("#000000", "#00CC00", "#0000FF",
                                        "#FF00FF", "#FF0000", "cyan3")) +
            scale_linetype_manual(values = c("dotted", "solid", "twodash", 
                                             "longdash", "dotdash", "dashed")) +
            scale_shape_manual(values = c(15,16,17,18,19,8))
        
        p_right[[i]] <- ggplot(data[CRIT==(i-1)],
                               aes(x=L, y=Estimate, group=RetGroup, colour=RetGroup,
                                   linetype=RetGroup, shape=RetGroup)) +
            geom_line(linewidth=0.75) +
            geom_point(size=3) +
            ylim(miny,maxy) +
            labs(title = NULL, y = NULL, x = "Time-Lag", colour="Return Group") +
            theme_bw() +
            theme(legend.position="bottom", 
                  legend.text = element_text(size = 8),
                  legend.title = element_text(size=8),
                  legend.key.size = unit(0.05, 'cm'),
                  panel.grid = element_blank()) + 
            guides(color=guide_legend(nrow = 2), shape = "none", linetype="none") + 
            scale_x_discrete(expand = c(0.05, 0.05)) +
            scale_color_manual(values=c("#990000", "#FF6600", "#000000",
                                        "#666666", "#00CC00", "#006600")) +
            scale_linetype_manual(values = c("solid", "dotted", "twodash", 
                                             "longdash", "dotdash", "dashed")) +
            scale_shape_manual(values = c(15,16,17,18,19,8))
    }
    
    return(list(p_left=p_left, p_right=p_right))
}

