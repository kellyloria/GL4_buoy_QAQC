# 



long_DF <- RA.zoop2 %>% 
gather(taxa, RA.all, B.RA.DAROS:D.RA.CONO)
head(long_DF, 6)  # note, for brevity, I only show the data for the first two years 
#long_DF %>% gather(year, taxa, RA.all,-DAPUL.ave, -HESP.ave, -NEO.ave, -NAUP.ave, -BOSCHY.ave, -DITH.ave)
