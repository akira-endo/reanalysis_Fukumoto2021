## Plot relative ATC
# import figlabels: specify figure labels of outcome and ATC, e.g. c("1_a", "1_b")
# effecttype: either "ATC" or "ATT"
relATplot <- function(figlabels, effecttype, linecolor=1, shadecolor="gray90",dir="Fukumoto2021/output/", titlewithdate=T,returnmatch=T){
    files=paste0(dir, "Fukumoto_Fig",figlabels,"_source.csv")
    DF=cbind(fread(files[1],),fread(files[2]))
    names(DF)<-c("date","outcome_matched_ctrl","outcome_matched_treated","outcome_all_treated","date2","AT","LB","UB")
    DF$outcome_matched_ctrl<-DF$outcome_matched_ctrl+.Machine$double.eps
    
    title=paste("Relative", effecttype)
    titledate=ifelse(titlewithdate, paste0(" (", format(DF$date[8],"%B %d"),")"), "")
    
    plot(DF$date, DF$AT/DF$outcome_matched_ctrl,
         xlim=c(DF$date[1], DF$date[length(DF$date)]),
         ylim=c(-1,1),
         xlab="Day", ylab=title,
         type="n",
         xaxt="n",
         main = paste0(title,titledate), yaxs="i")
    axis(1, at = DF$date[seq(1,29,by=7)],
         labels = format(DF$date[seq(1,29,by=7)], "%b %d"))
    polygon(c(DF$date, rev(DF$date)), c(DF$UB/DF$outcome_matched_ctrl, rev(DF$LB/DF$outcome_matched_ctrl)), col = shadecolor, border = NA)
    lines(DF$date, DF$AT/DF$outcome_matched_ctrl, col = linecolor, lty = 1, lwd = 3)
    abline(v=DF$date[8], col = "turquoise")
    abline(0,0,lty=2)
    if(returnmatch){
        mout<-readRDS(paste0("Fukumoto2021/output/mout_",effecttype,format(DF$date[8],"%m%d"),".rds"))
        list(matchingsummary=summary(mout),matchingoutput=mout)
    }
}