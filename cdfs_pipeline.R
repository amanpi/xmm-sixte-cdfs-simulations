################################################################################
## To do:
## - include manual RAWX, RAWY shifts
##
## Note:
## source HEASOFT and SIXTE scripts before!
##
## Edit:
## - `workDir`		where to run the simulations (default ./cdfs_sim)
## - args(doATT,...)	list of 'y' or 'n' (see comments in code)
##
## Needs:
## - cdfs_functions.R	where are defined all needed functions for the pipeline
################################################################################

################################################################################
## 0 - Libraries
################################################################################
library(FITSio)
library(stringr)

################################################################################
## 1 - Functions
################################################################################
funcFile <- "cdfs_functions.R"
workDir <- getwd()
if ( !file.exists(paste(workDir, "cdfs_sim", sep = "/")) ) { 
   system("mkdir cdfs_sim") 
}
setwd(paste(workDir, "cdfs_sim", sep = "/"))
source(paste(workDir, "cdfs_sim", funcFile, sep = "/"))

################################################################################
## 2 - Arguments
################################################################################
args <- list(
  doAtt="y",	## create attitude file for SIXTE?
  do_PN="y",	## create pn simulations?
  do_M1="y",	## create MOS1 simulations?
  do_M2="y",	## create MOS2 simulations? 
  doCor="y",	## correct events for SAS compliance?
  doDet="y")	## run test run source detection?

## The list of OBSID to create with SIXTE (check cdfs_functions.R)
obsId <- read.table("obs.lis", header=F, as.is = T, colClasses = "character")

################################################################################
## 3 - Start code
################################################################################
options(digits = 12)
message("CDFS pipeline will be run over OBSIDs:")
print(obsId)

#for ( iobs in 3:length(obsId$V1)) {
for ( iobs in 1 ) {

  ## Create simulation directory
  obsSim <- obsId$V1[iobs]
  if ( !file.exists(obsSim) ) {
    system(paste("mkdir", obsSim))
  }
  message("OBSID ", obsSim)
  setwd(paste(workDir, "cdfs_sim", obsSim, sep="/"))

  ## Copy SIMPUT files
  if ( !file.exists(simpFil1) ) {
    system(paste0("cp -f ", workDir, 
                  "/smp_files/CDFS_combined_simput.tgz ", getwd()))
    system("tar zxf CDFS_combined_simput.tgz")
    system("rm -f CDFS_combined_simput.tgz")
  }
    
  ## Copy event and attitude files 
  evLis <- list.files(paste0(dataPath, "/detection/", obsSim), 
                      pattern = "EVLI", full.names = T)
  copy.evfiles(evLis)
  copy.attitude(obsSim)
  pnEvFile <- "PN_EVT.FIT"
  m1EvFile <- "M1_EVT.FIT"
  m2EvFile <- "M2_EVT.FIT"
  attFile <- "ATT.FIT"
    
  ## TSTART and DURATION from attitude file
  tStart <- get.hkey(attFile, "+1 T column=TIME rows=1 rownum=no colheader=no", 2)
  nRows <- get.hkey(attFile, "+1 K include=NAXIS2", 3)
  tStop <- get.hkey(attFile, paste0("+1 T column=TIME rows=", nRows, " rownum=no colheader=no"), 2)
  dateObs <- get.hstr(pnEvFile, "+1 K include=DATE-OBS", 2)
  
  ## Get pointing coordinates
  sixteRaPnt <- get.hkey(pnEvFile, "+1 K include=RA_PNT", 3)
  sixteDePnt <- get.hkey(pnEvFile, "+1 K include=DEC_PNT", 3)
  sixtePaPnt <- get.hkey(pnEvFile, "+1 K include=PA_PNT", 3)
  sixteRollA <- 360 - sixtePaPnt
  sixteExpos <- tStop - tStart
  write.key2ascii(sixteRaPnt, sixteDePnt, sixtePaPnt, pnEvFile)
    
  ## Create attitude file for SIXTE simulations
  if ( args$doAtt == "y" ) {
    message("Creating attitude file...")
    create.attitude(attFile, sixteRaPnt, sixteDePnt, sixteRollA, tStart, tStop)
    system(paste("cp -f", attFile, "ATTSIXTE.FIT"))
    attFile <- "ATTSIXTE.FIT"
  }
    
  ## runsixt: EPIC-pn
  if ( args$do_PN == "y" ) {
    message("Running SIXTE for ", obsSim, " (EPN): Texp=", sixteExpos, " s")
    for ( ii in 1:12 ) {
       create.chippn(ii, sixteExpos, attFile, tStart, sixteRaPnt, sixteDePnt)
    }
  }

  ## runsixt: EPIC-M1
  if ( args$do_M1 == "y" ) {
    message("Running SIXTE for ", obsSim, " (EM1): Texp=", sixteExpos, " s")
    for ( ii in 1:6 ) {
      create.chipm1(ii, sixteExpos, attFile, tStart, sixteRaPnt, sixteDePnt)
    }
  }
  
  ## runsixt: EPIC-M2
  if ( args$do_M2 == "y" ) {
    message("Running SIXTE for ", obsSim, " (EM2): Texp=", sixteExpos, " s")
    for ( ii in 1:7 ) {
      create.chipm2(ii, sixteExpos, attFile, tStart, sixteRaPnt, sixteDePnt)
    }
  }
      
  ## Correct events
  if ( args$doCor == "y" ) {
    
    ## Sort out input and output
    chipPn <- str_pad(1:12, 2, pad="0")
    chipM1 <- str_pad(c(1:5,7), 2, pad="0")
    chipM2 <- str_pad(1:7, 2, pad="0")
    
    inputPn <- paste0(simfPn, "_ccd", chipPn, "_", simpBase, ".fits")
    inputM1 <- paste0(simfM1, "_ccd", chipM1, "_", simpBase, ".fits")
    inputM2 <- paste0(simfM2, "_ccd", chipM2, "_", simpBase, ".fits")
    
    outPn <- paste0(simfPn, "_", simpBase, ".fits")
    outM1 <- paste0(simfM1, "_", simpBase, ".fits")
    outM2 <- paste0(simfM2, "_", simpBase, ".fits")
    
    ## Fresh local copy of attitude file
    copy.attitude(obsSim)
    attFile <- "ATT.FIT"
    
    ## Load environment and events
    message("Loading environment ...")
    if ( file.exists("fixepic.rda") ) { load("fixepic.rda") }
    if ( !exists("eventsPn") ) { eventsPn <- readFITS(pnEvFile, hdu=1) }
    if ( !exists("eventsM1") ) { eventsM1 <- readFITS(m1EvFile, hdu=1) }
    if ( !exists("eventsM2") ) { eventsM2 <- readFITS(m2EvFile, hdu=1) }

    ## Shift pointing coordinates
    if ( !file.exists("shift_pointing_boresight.py") ) {
      system(paste0("cp -f ", workDir,  
                    "/scripts/shift_pointing_boresight.py ", getwd()))
    }
    shiftCoord <- compute.boresight(sixteRaPnt, sixteDePnt, sixtePaPnt, obsSim)
    
    ## cifbuild and odfingest
    sas1 <- create.cifOdf()
    sumFile <- list.files(getwd(), pattern="SUM.SAS")
    
    ## Merge event lists: evlistcomb
    message("Merging events ...")
    sas2Pn <- merge.events(sumFile, "epn", inputPn, outPn)
    sas2M1 <- merge.events(sumFile, "emos", inputM1, outM1)
    sas2M2 <- merge.events(sumFile, "emos", inputM2, outM2)
    
    ## Fix header keywords
    fix.hkeyPn(outPn)
    fix.hkeyMos(outM1, outM2)

    ## Compute the viewing direction: strbs (bstools)
    message("Computing the viewing direction ...")
    sas3Pn <- comp.view(sumFile, "EPN", shiftCoord[1,], sixtePaPnt, "stPN.out")
    sas3M1 <- comp.view(sumFile, "EMOS1", shiftCoord[2,], sixtePaPnt, "stM1.out")
    sas3M2 <- comp.view(sumFile, "EMOS2", shiftCoord[3,], sixtePaPnt, "stM2.out")
    strbsPn <- get.strbscoords("stPN.out")
    strbsM1 <- get.strbscoords("stM1.out")
    strbsM2 <- get.strbscoords("stM2.out")
    
    ## Correct detector coordinates: edet2sky
    message("Correcting detector coordinates ...")
    sas4Pn <- fix.detcoords(sumFile, outPn, "EPN", dateObs, strbsPn)
    sas4M1 <- fix.detcoords(sumFile, outM1, "EMOS1", dateObs, strbsM1)
    sas4M2 <- fix.detcoords(sumFile, outM2, "EMOS2", dateObs, strbsM2)
    
    ## Fix data types
    message("Fixing data types ...")
    sPn <- readFITS(outPn, hdu=1) 
    sM1 <- readFITS(outM1, hdu=1) 
    sM2 <- readFITS(outM2, hdu=1) 
    
    newPn <- paste0(simfPn, "_fixed_", simpBase, ".fits")
    newM1 <- paste0(simfM1, "_fixed_", simpBase, ".fits")
    newM2 <- paste0(simfM2, "_fixed_", simpBase, ".fits")
    
    fixCmdPn <- fix.dtypes("TMP_EPN.FIT", sPn, eventsPn, newPn)
    fixCmdM1 <- fix.dtypes("TMP_EMOS1.FIT", sM1, eventsM1, newM1)
    fixCmdM2 <- fix.dtypes("TMP_EMOS2.FIT", sM2, eventsM2, newM2)

    ## Compute sky coordinates: attcalc
    message("Computing sky coordinates ...")
    sas5Pn <- comp.skycoords(sumFile, newPn, strbsPn)
    sas5M1 <- comp.skycoords(sumFile, newM1, strbsM1)
    sas5M2 <- comp.skycoords(sumFile, newM2, strbsM2)
    
    ## Write SAS commands in scripts/camera
    unlink(paste0("script", 1:5, ".sas"))
    write.allsas(sas1, sas2Pn, sas3Pn, sas4Pn, sas5Pn, "script_fixpn.sas")
    write.allsas(sas1, sas2M1, sas3M1, sas4M1, sas5M1, "script_fixm1.sas")
    write.allsas(sas1, sas2M2, sas3M2, sas4M2, sas5M2, "script_fixm2.sas")
    
    ## Save environment
    unlink(list.files(getwd(), "TMP"))
    save(list=ls(all=TRUE), file="fixepic.rda")

  }
  
  ## Run test detection
  if ( args$doDet == "y" ) {
    
    message("Running test detection chain ...")
    run.detchain(sumFile, newPn, attFile, "epn", 1)
    run.detchain(sumFile, newM1, attFile, "em1", 2)
    run.detchain(sumFile, newM2, attFile, "em2", 3)
    
  }

  setwd(paste(workDir, "cdfs_sim", sep = "/"))

}
