#------------------------------------------------------------------------------
# GHCN_SCRAPE
#
# Scrapes the min, max and avg datasets from this website:
# 
# ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/
# Replace the ".qcu" with ".qca" to download raw and unadjusted data sets
#------------------------------------------------------------------------------
setwd("~/Documents/Yale//Yale 2014-2015/Spring - STAT 230/Final Project/RAW_DATA/")

baseurl <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/"
for (ttype in c("tavg","tmin","tmax")){
  fn0 <- paste0("ghcnm.",ttype,".latest.qcu.tar.gz")
  timestamp <- Sys.time()
  temp <- tempfile()
  download.file(paste0(baseurl,fn0),temp)
  fnames <- untar(temp,list=TRUE)[1:2]
  untar(temp)
  unlink(temp)
  dat <- read.fwf(fnames[1],widths = c(3,8,4,4,rep(c(5,1,1,1),12)))
  names(dat) <- c("CNTRYCODE","ID","YEAR","EVENT",
                  paste0(rep(c("VALUE","DMFLAG","QCFLAG","DSFLAG"),12),rep(1:12,each=4)))
  dat[dat==-9999] <- NA
  inv <- read.fwf(fnames[2],widths = c(3,8,-1,8,-1,9,-1,6,-1,30,-1,
                                       4,1,5,2,2,2,2,1,2,16,1),fill=TRUE)
  names(inv) <- c("CNTRYCODE","ID","LATITUDE","LONGITUDE","STNELEV","NAME",
                  "GRELEV","POPCLS","POPSIZ","TOPO","STVEG",
                  "STLOC","OCNDIS","AIRSTN","TOWNDIS","GRVEG",
                  "POPCSS")
  save(file=paste0(ttype,"-qcu.RData"),list=c("dat","inv","timestamp"))
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
