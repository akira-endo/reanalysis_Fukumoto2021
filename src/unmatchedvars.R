# April 6
p_path <- "../Fukumoto2021/preprocess/preprocess.0406.R"
m_path <- "../Fukumoto2021/matching_results/alt_atc_0406_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0406")
source("gen.0406.unmatch.R",chdir=T)
unmatchedsamples=sapply(1:25,function(col){sum(pref.dummy[gm$id[1:(nrow(gm)/2)*2]%>%as.numeric ,col]!=pref.dummy[gm$id[1:(nrow(gm)/2)*2-1]%>%as.numeric ,col])}) # count the number of unmatches for each pref dummy. The sum will be 2 * # unmatched pairs as the unmatches are counted twice for each of the pair
unmatchedid0406=(sapply(1:25,function(col){(pref.dummy[gm$id[1:(nrow(gm)/2)*2]%>%as.numeric ,col]!=pref.dummy[gm$id[1:(nrow(gm)/2)*2-1]%>%as.numeric ,col])%>%rep(.,each=2) })%>%rowSums)!=0

cat("April 6:",sum(unmatchedsamples)/2, "matched pairs out of", nrow(gm)/2,"had their prefecture dummy variables unmatched for.\n")

# April 10
p_path <- "../Fukumoto2021/preprocess/preprocess.0410.R"
m_path <- "../Fukumoto2021/matching_results/alt_atc_0410_1000_1000.RDS"
lm_form <- as.formula("gm[, part[p]] ~ shutdown.0410")
source("gen.0410.unmatch.R",chdir=T)
unmatchedsamples=sapply(1:25,function(col){sum(pref.dummy[gm$id[1:(nrow(gm)/2)*2]%>%as.numeric ,col]!=pref.dummy[gm$id[1:(nrow(gm)/2)*2-1]%>%as.numeric ,col])})
unmatchedid0410=(sapply(1:25,function(col){(pref.dummy[gm$id[1:(nrow(gm)/2)*2]%>%as.numeric ,col]!=pref.dummy[gm$id[1:(nrow(gm)/2)*2-1]%>%as.numeric ,col])%>%rep(.,each=2) })%>%rowSums)!=0

cat("April 10:",sum(unmatchedsamples)/2, "matched pairs out of", nrow(gm)/2,"had their prefecture dummy variables unmatched for.")