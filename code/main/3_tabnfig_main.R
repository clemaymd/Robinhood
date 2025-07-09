# Outputs regression results for Table 3 & Figure 1 (HF) and Table 4 & Figure 2 (Daily) 
# Use DailyFreq = TRUE/FALSE for daily/high-frequency settings.

library("here")
source(here("code", "_load_packages.R"))
source(here("code", "main", "f.R"))

if (!exists("DailyFreq")) {
    # if this file is run from the 01_Run_all.R, DailyFreq is already defined
    # if this file is run independently, run twice with DailyFreq = FALSE and TRUE
    DailyFreq <- FALSE  
}

if(DailyFreq){
    DT <- readRDS(here("outputs","REGRES_MAIN_DAILY.rds"))
}else{
    DT <- readRDS(here("outputs","REGRES_MAIN.rds"))
}

# Tables
n_grp <- length(unique(DT[[2]]$RetGroup))
dt4tab <- DT[[1]]
groupnam <- rownames(DT[[1]])[1:n_grp]

betas <- dt4tab[groupnam,]
betas <- matrix(round(betas,2), nrow=n_grp, ncol=ncol(dt4tab),
                dimnames = list(rownames(betas),colnames(betas)))
tstat <- dt4tab[(n_grp+1):(n_grp*2),]
tstat <- matrix(paste0("(",round(tstat,2),")"), nrow=n_grp, ncol=ncol(dt4tab),
                dimnames = list(rownames(tstat),colnames(tstat)))
R2 <- round(dt4tab["R2",],3)
Nobs <- as.integer(dt4tab["Nobs",])

for(i in 1:n_grp){
    if(i==1){
        out <- rbind(betas[i,],tstat[i,])
    } else {
        out <- rbind(out,betas[i,],tstat[i,])
    }
}
out <- rbind(out,R2,Nobs)
idx_rownames_betas <- seq(from = 1,to = n_grp*2, by = 2)
rownames(out)[idx_rownames_betas] <- groupnam
idx_rownames_tstat <- seq(from = 2,to = n_grp*2, by = 2)
rownames(out)[idx_rownames_tstat] <- " "
out <- data.table(out, keep.rownames = T)

if(DailyFreq){
    fwrite(out, file = here('outputs','TAB4.txt'), sep = "\t")
}else{
    fwrite(out, file = here('outputs','TAB3.txt'), sep = "\t")
}

# Figures
dt4fig <- DT[[2]]
dt4fig[, RetGroup := ordered(factor(RetGroup),levels = groupnam)]
dt4fig[, Estimate := as.numeric(Estimate)]
maxy <- ceiling(max(dt4fig$Estimate))
miny <- floor(min(dt4fig$Estimate))
ifelse(DailyFreq, laglabel <- "Daily-Lag", laglabel <- "Time-Lag")

# by return group (Panel A)
pA <- ggplot(dt4fig, aes(x=RetGroup, y=Estimate, group=L, colour=L,
                         linetype=L,shape=L)) +
    geom_line(linewidth=0.75) +
    geom_point(size=3) + 
    ylim(miny,maxy) +
    theme_bw() +
    theme(legend.position="bottom", panel.grid = element_blank()) +
    guides(colour = guide_legend(nrow = 1)) +
    scale_x_discrete(expand = c(0.05, 0.05)) +
    scale_color_manual(name = laglabel,
                       values=c("#000000", "#00CC00", "#0000FF",
                                "#FF00FF", "#FF0000", "cyan3")) +
    scale_linetype_manual(name = laglabel, 
                          values = c("dotted", "solid", "twodash", 
                                     "longdash", "dotdash", "dashed")) +
    scale_shape_manual(name = laglabel, values = c(15,16,17,18,19,8)) +
    labs(title = NULL, y = NULL, x = "Return Group", colour=paste0(laglabel))
pA
if(DailyFreq){
    ggsave(plot=pA, filename=here("outputs","FIG2A.png"), width=7, height=4, dpi=700)
}else{
    ggsave(plot=pA, filename=here("outputs","FIG1A.png"), width=7, height=4, dpi=700)
}

# by lag (Panel B)
pB <- ggplot(dt4fig, aes(x=L, y=Estimate, group=RetGroup, colour=RetGroup,
                         linetype=RetGroup, shape=RetGroup)) +
    geom_line(linewidth=0.75) +
    geom_point(size=3) + 
    ylim(miny,maxy) +
    theme_bw() +
    theme(legend.position="bottom", panel.grid = element_blank()) +
    guides(colour = guide_legend(nrow = 1)) +
    scale_x_discrete(expand = c(0.05, 0.05)) +
    scale_color_manual(name = laglabel,
                       values=c("#990000", "#FF6600", "#000000",
                                "#666666", "#00CC00", "#006600")) +
    scale_linetype_manual(name = laglabel, 
                          values = c("solid", "dotted", "twodash", 
                                     "longdash", "dotdash", "dashed")) +
    scale_shape_manual(name = laglabel, values = c(15,16,17,18,19,8)) +
    labs(title = NULL, y = NULL, x = paste0(laglabel), colour="Return Group")
pB
if(DailyFreq){
    ggsave(plot=pB, filename=here("outputs","FIG2B.png"), width=7, height=4, dpi=700)
}else{
    ggsave(plot=pB, filename=here("outputs","FIG1B.png"), width=7, height=4, dpi=700)
}