################################################################################
## Setup
##
## Edit:
##  - `dataPath` 	path where the real XMM Newton observations are stored
##  - `sasCmd` 		command to source SAS installation scripts 
##  - `obsLis`		by default all OBSIDs in dataPath unless "obs.lis" exists 
##
## Note:
## source HEASOFT and SIXTE scripts before! but not SAS
################################################################################
host <- system("hostname", intern = T)
## Default values for dataPath and sasCmd (machine-dependent !!)
dataPath <- paste0(getwd(), "/data")
sasCmd <- "/usr/local/xmm-sas/xmmsas_20211130_0941/sas-init.sh"
if ( host == "lina" ) {
   dataPath <- "/home/apires/Work/data/xmm/cdfs"
   sasCmd <- "/usr/local/xmm-sas/xmmsas_20211130_0941/sas-init.sh"
   } else {
   if ( host == "kanab" ) {
   dataPath <- "/home/apires/Work/data2/xmm/cdfs"
   sasCmd <- "/net/konraid/xray/setxmmsas.sh"
   } else {
   message("Unknown environment")
   q()
   }
}

## SIXTE-related
sixtePath <- system("echo $SIXTE", intern = T)
xmlDir <- paste0(sixtePath, "/share/sixte/instruments/xmm")

## SIMPUT-related
simpBase <- "CDFS"
simpFil1 <- "CDFS_cat_lehmer.fits"
simpFil2 <- "CDFS_cat_galaxies.fits"

## Labels for simulations
simfPn <- "epn_att_evt"
simfM1 <- "em1_att_evt"
simfM2 <- "em2_att_evt"

## Instrument-related
xmmInstr1 <- "epicpn"
xmmInstr2 <- "epicmos"
xmmFilte1 <- "thin"
xmmFilte2 <- "thin"
xmmBkgmo1 <- "lowcfbkg"
xmmBkgmo2 <- "lowflatbkg"
xmmCcdid1 <- rep(0:2, 4)
xmmCcdid2 <- 1:7
xmmQuadra <- c(rep(0, 3), rep(1, 3), rep(2, 3), rep(3, 3))

## OBSIDs to process
obsLis <- "obs.lis"
if ( !file.exists(obsLis) ) {
   obsId <- list.files(paste(dataPath, "detection", sep = "/"))
   write.table(obsId, obsLis, quote = F, row.names = F, col.names = F)
}
################################################################################
## Get fits header keyword value as float
################################################################################
get.hkey <- function(file, cmd, pos) {
   string1 <- system(paste0("ftlist ", file, cmd), intern = T)
   string2 <- strsplit(string1, " +")
   return(as.double(string2[[1]][pos]))
}
################################################################################
## Get fits header keyword as string
################################################################################
get.hstr <- function(file, cmd, pos) {
   string1 <- system(paste0("ftlist ", file, cmd), intern = T)
   string2 <- strsplit(string1, " +")
   return(str_replace_all(string2[[1]][pos], "'", ""))
}
################################################################################
## Copy event files for simulation
################################################################################
copy.evfiles <- function(evlis) {
   pn <- evlis[which(str_detect(evlis, "PN"))]
   m1 <- evlis[which(str_detect(evlis, "M1"))]
   m2 <- evlis[which(str_detect(evlis, "M2"))]
   system(paste("cp -f", pn, "PN_EVT.FIT"))
   system(paste("cp -f", m1, "M1_EVT.FIT"))
   system(paste("cp -f", m2, "M2_EVT.FIT"))
}
################################################################################
## Copy attitude file
################################################################################
copy.attitude <- function(obs) {
   att <- list.files(paste0(dataPath, "/detection/", obs), pattern = "ATT", full.names = T)
   system(paste("cp -f", att, "ATT.FIT"))
}
################################################################################
## Write keywords we need to ascii 
################################################################################
write.key2ascii <- function(ra, de, pa, pn) {
   keys <- "REVOLUT,DATAMODE,SUBMODE,FILTER,OBS_ID,EXP_ID,EXPIDSTR"
   l1 <- "CREATOR = SIXTE"
   l2 <- "OBSERVER = SIXTE"
   l3 <- paste("RA_NOM =", ra)
   l4 <- paste("DEC_NOM =", de)
   l5 <- paste("RA_PNT =", ra)
   l6 <- paste("DEC_PNT =", de)
   l7 <- paste("PA_PNT =", pa)
   lis1 <- rbind(l1,l2,l3,l4,l5,l6,l7)
   write.table(lis1, "keywds.txt", quote = F, row.names = F, col.names = F)
   lis2 <- system(
      paste0("ftlist ", pn, "+0 K include=", keys), 
      intern = T)
   write.table(lis2, "keywds.txt", append = T, quote = F, row.names = F, col.names = F)
}
################################################################################
## Create ATT file for SIXTE simulation
################################################################################
create.attitude <- function(att, ra, de, roll, start, stop) {
   l1 <- "TIME D sec"
   l2 <- "RA D deg"
   l3 <- "DEC D deg"
   l4 <- "ROLLANG D deg"
   lis <- rbind(l1,l2,l3,l4)
   write.table(lis, "attcd.txt", quote = F, row.names = F, col.names = F)
   c1 <- "fdump"
   c2 <- paste0("infile=", att, "+1")
   c3 <- "pagewidth=256"
   c4 <- "outfile=att.dat"
   c5 <- "columns=TIME,AHFRA,AHFDEC,AHFPA"
   c6 <- "rows=- showcol=no showunit=no showrow=no clobber=yes prhead=no"
   cmd1 <- paste(c1,c2,c3,c4,c5,c6)
   cmd2 <- "fcreate cdfile=attcd.txt datafile=att.dat outfile=ATT.FIT clobber=yes"
   c1 <- paste0("fthedit ", att, "+1")
   c2 <- "EXTNAME add ATT"
   cmd3 <- paste(c1,c2)
   c1 <- paste0("fcalc infile=", att, "+1")
   c2 <- paste0("outfile=", att)
   c3 <- paste0("clname=RA expr=", ra, " clobber=yes")
   cmd4 <- paste(c1,c2,c3)
   c3 <- paste0("clname=DEC expr=", de, " clobber=yes")
   cmd5 <- paste(c1,c2,c3)
   c3 <- paste0("clname=ROLLANG expr=", roll, " clobber=yes")
   cmd6 <- paste(c1,c2,c3)
   cmd7 <- paste0("fthedit ", att, "+1 MJDREF add 50814.")
   cmd8 <- paste0("fthedit ", att, "+1 TSTART add ", start)
   cmd9 <- paste0("fthedit ", att, "+1 TSTOP add ", stop)
   head <- "#!/bin/bash"
   cmdLis <- rbind(head,cmd1,cmd2,cmd3,cmd4,cmd5,cmd6,cmd7,cmd8,cmd9)
   write.table(cmdLis, "create_att.sh", quote = F, row.names = F, col.names = F)
   system("chmod +x create_att.sh")
   system(paste0(". ", getwd(), "/create_att.sh"))      
}
################################################################################
## Create SIXTE EPIC-pn simulations
################################################################################
create.chippn <- function(ind, exp, att, start, ra, de) {
   script <- "create_epn.sh"
   if ( file.exists(script) ) { unlink(script) }
   chip <- str_pad(1:12, 2, pad="0")
   pref <- "spn_att_"
   suffi <- paste0("_ccd", chip[ind], "_", simpBase, ".fits")
   suffE <- paste0("evt", suffi)
   suffR <- paste0("raw", suffi)
   ccdid <- xmmCcdid1[ind]
   quadr <- xmmQuadra[ind]
   xml <- paste0(xmlDir, "/", xmmInstr1, "/fullframe_ccd", chip[ind],
      "_", xmmFilte1, "filter_", xmmBkgmo1, ".xml")
   head <- "#!/bin/bash"
   c1 <- "runsixt"
   c2 <- paste0("Exposure=", format(exp, scientific = T))
   c3 <- "MJDREF=50814.0"
   c4 <- paste0("TSTART=", format(start, scientific = T))
   c5 <- paste0("Attitude=", att)
   c6 <- paste0("RawData=", suffR)
   c7 <- paste0("EvtFile=", suffE)
   c8 <- paste0("XMLFile=", xml)
   c9 <- paste0("Simput=", simpFil1)
   c10 <- paste0("Simput2=", simpFil2)
   c11 <- "clobber=yes chatter=3"
   c12 <- paste0("Prefix=", pref)
   cmd1 <- paste(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12)
   c13 <- paste0("fthedit ", pref, suffE, " RA_PNT add ", ra)
   c14 <- paste0("fthedit ", pref, suffE, " DEC_PNT add ", de)
   cmd2 <- rbind(c13,c14)      
   c15 <- paste0("epicpn_events EvtFile=", pref, suffE)
   c16 <- paste0("EPICpnEventList=", simfPn, suffi)
   cmd3 <- paste(c15,c16)
   cmd4 <- paste0("fappend ", pref, suffE, "+2 ", simfPn, suffi)
   cmdLis <- rbind(head,cmd1,cmd2,cmd3,cmd4)
   write.table(cmdLis, script, quote = F, row.names = F, col.names = F)
   for ( iext in 0:2 ) {
      c17 <- paste0("fthedit ", simfPn, suffi, "+", iext, 
         " @keywds.txt operation=add")
      c18 <- paste0("fthedit ", simfPn, suffi, "+", iext, 
         " CCDID add ", ccdid)
      c19 <- paste0("fthedit ", simfPn, suffi, "+", iext, 
         " QUADRANT add ", quadr)
      cmd5 <- rbind(c17,c18,c19)
      write.table(cmd5, script, append = T, quote = F, row.names = F, col.names = F)
      rm(c17,c18,c19,cmd5)
   }
   message("... chip ", chip[ind])
   system(paste("chmod +x", script))
   system(paste0(". ", getwd(), "/", script))
}
################################################################################
## Create SIXTE EPIC-M1 simulations
################################################################################
create.chipm1 <- function(ind, exp, att, start, ra, de) {
   script <- "create_em1.sh"
   if ( file.exists(script) ) { unlink(script) }
   chip <- str_pad(c(1:5,7), 2, pad="0")
   pref <- "sm1_att_"
   suffi <- paste0("_ccd", chip[ind], "_", simpBase, ".fits")
   suffE <- paste0("evt", suffi)
   suffR <- paste0("raw", suffi)
   if ( ind == 6 ) {
      ccdid <- xmmCcdid2[ind+1]
   } else {
      ccdid <- xmmCcdid2[ind]
   }
   xml <- paste0(xmlDir, "/", xmmInstr2, "/mos1_fullframe_ccd", chip[ind],
      "_", xmmFilte2, "filter_", xmmBkgmo2, ".xml")
   head <- "#!/bin/bash"
   c1 <- "runsixt"
   c2 <- paste0("Exposure=", format(exp, scientific = T))
   c3 <- "MJDREF=50814.0"
   c4 <- paste0("TSTART=", format(start, scientific = T))
   c5 <- paste0("Attitude=", att)
   c6 <- paste0("RawData=", suffR)
   c7 <- paste0("EvtFile=", suffE)
   c8 <- paste0("XMLFile=", xml)
   c9 <- paste0("Simput=", simpFil1)
   c10 <- paste0("Simput2=", simpFil2)
   c11 <- "clobber=yes chatter=3"
   c12 <- paste0("Prefix=", pref)
   cmd1 <- paste(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12)
   c13 <- paste0("fthedit ", pref, suffE, " RA_PNT add ", ra)
   c14 <- paste0("fthedit ", pref, suffE, " DEC_PNT add ", de)
   cmd2 <- rbind(c13,c14)      
   c15 <- paste0("epicmos1_events EvtFile=", pref, suffE)
   c16 <- paste0("EPICmos1EventList=", simfM1, suffi)
   cmd3 <- paste(c15,c16)
   cmd4 <- paste0("fappend ", pref, suffE, "+2 ", simfM1, suffi)
   cmdLis <- rbind(head,cmd1,cmd2,cmd3,cmd4)
   write.table(cmdLis, script, quote = F, row.names = F, col.names = F)
   for ( iext in 0:2 ) {
      c17 <- paste0("fthedit ", simfM1, suffi, "+", iext, 
         " @keywds.txt operation=add")
      c18 <- paste0("fthedit ", simfM1, suffi, "+", iext, 
         " INSTRUME add EMOS1")
      c19 <- paste0("fthedit ", simfM1, suffi, "+", iext, 
         " CCDID add ", ccdid)
      c20 <- paste0("fthedit ", simfM1, suffi, "+", iext, 
         " CCDNODE add 0")
      cmd5 <- rbind(c17,c18,c19,c20)
      write.table(cmd5, script, append = T, quote = F, row.names = F, col.names = F)
      rm(c17,c18,c19,c20,cmd5)
   }
   message("... chip ", chip[ind])
   system(paste("chmod +x", script))
   system(paste0(". ", getwd(), "/", script))
}
################################################################################
## Create SIXTE EPIC-M2 simulations
################################################################################
create.chipm2 <- function(ind, exp, att, start, ra, de) {
   script <- "create_em2.sh"
   if ( file.exists(script) ) { unlink(script) }
   chip <- str_pad(1:7, 2, pad="0")
   pref <- "sm2_att_"
   suffi <- paste0("_ccd", chip[ind], "_", simpBase, ".fits")
   suffE <- paste0("evt", suffi)
   suffR <- paste0("raw", suffi)
   ccdid <- xmmCcdid2[ind]
   xml <- paste0(xmlDir, "/", xmmInstr2, "/mos2_fullframe_ccd", chip[ind],
      "_", xmmFilte2, "filter_", xmmBkgmo2, ".xml")
   head <- "#!/bin/bash"
   c1 <- "runsixt"
   c2 <- paste0("Exposure=", format(exp, scientific = T))
   c3 <- "MJDREF=50814.0"
   c4 <- paste0("TSTART=", format(start, scientific = T))
   c5 <- paste0("Attitude=", att)
   c6 <- paste0("RawData=", suffR)
   c7 <- paste0("EvtFile=", suffE)
   c8 <- paste0("XMLFile=", xml)
   c9 <- paste0("Simput=", simpFil1)
   c10 <- paste0("Simput2=", simpFil2)
   c11 <- "clobber=yes chatter=3"
   c12 <- paste0("Prefix=", pref)
   cmd1 <- paste(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12)
   c13 <- paste0("fthedit ", pref, suffE, " RA_PNT add ", ra)
   c14 <- paste0("fthedit ", pref, suffE, " DEC_PNT add ", de)
   cmd2 <- rbind(c13,c14)      
   c15 <- paste0("epicmos2_events EvtFile=", pref, suffE)
   c16 <- paste0("EPICmos2EventList=", simfM2, suffi)
   cmd3 <- paste(c15,c16)
   cmd4 <- paste0("fappend ", pref, suffE, "+2 ", simfM2, suffi)
   cmdLis <- rbind(head,cmd1,cmd2,cmd3,cmd4)
   write.table(cmdLis, script, quote = F, row.names = F, col.names = F)
   for ( iext in 0:2 ) {
      c17 <- paste0("fthedit ", simfM2, suffi, "+", iext, 
         " @keywds.txt operation=add")
      c18 <- paste0("fthedit ", simfM2, suffi, "+", iext, 
         " INSTRUME add EMOS2")
      c19 <- paste0("fthedit ", simfM2, suffi, "+", iext, 
         " CCDID add ", ccdid)
      c20 <- paste0("fthedit ", simfM2, suffi, "+", iext, 
         " CCDNODE add 0")
      cmd5 <- rbind(c17,c18,c19,c20)
      write.table(cmd5, script, append = T, quote = F, row.names = F, col.names = F)
      rm(c17,c18,c19,c20,cmd5)
   }
   message("... chip ", chip[ind])
   system(paste("chmod +x", script))
   system(paste0(". ", getwd(), "/", script))
}
################################################################################
## Compute boresight shifts
################################################################################
compute.boresight <- function(ra, de, pa, obs) {
   system(paste("python3 shift_pointing_boresight.py", ra, de, pa, "> pyout"))
   system(paste0("grep '    (' pyout >", obs, ".shift_coords"))
   shift <- read.table(paste0(obs, ".shift_coords"))
   shift$V1 <- str_replace_all(shift$V1, "\\(", "")
   shift$V1 <- as.double(str_replace_all(shift$V1, ",", ""))
   shift$V2 <- as.double(str_replace_all(shift$V2, "\\)>", ""))
   return(shift)
}
################################################################################
## Create and run XMM-Newton SAS script
################################################################################
run.xmmSas <- function(cmd, script) {
   write.table(cmd, script, row.names=F, col.names=F, quote=F)
   system(paste("chmod +x", script))
   if ( host == "kanab" ) { 
      system(paste0("source ./", script))
   } else {
      system(paste0(". ", getwd(), "/", script)) 
   }
}
################################################################################
## cifbuild and odfingest
################################################################################
create.cifOdf <- function() {
   head <- "#!/bin/sh"
   if ( host == "kanab" ) { cmd1 <- paste("source", sasCmd) }
   if ( host == "lina") { cmd1 <- paste(".", sasCmd) }
   if ( !file.exists("ccf.cif") ) {
      cmd2 <- paste0("export SAS_ODF=", dataPath, "/xmmdata/", obsSim, "/odf")
      cmd3 <- "cifbuild"
      cmd4 <- "export SAS_CCF=ccf.cif"
      cmd5 <- "odfingest"
      cmd <- rbind(head,cmd1,cmd2,cmd3,cmd4,cmd5)
   } else {
      cmd2 <- "export SAS_CCF=ccf.cif"
      cmd <- rbind(head,cmd1,cmd2)
   }
   run.xmmSas(cmd, "script1.sas")
   return(cmd)
}
################################################################################
## Merge events
################################################################################
merge.events <- function(summaryf, instr, input, output) {
   head <- "#!/bin/sh"
   if ( host == "kanab" ) { cmd1 <- paste("source", sasCmd) }
   if ( host == "lina") { cmd1 <- paste(".", sasCmd) }
   cmd2 <- "export SAS_CCF=ccf.cif"
   cmd3 <- paste0("export SAS_ODF=", summaryf)
   cmd4 <- "SAS_VERBOSITY=5"
   cmd5 <- paste("evlistcomb",
      paste0("eventsets=", paste0("\'", paste(input, collapse=" "), "\'")),
      paste0("instrument=", instr),
      paste0("imagingset=", output), 
      "othertables=STDGTI")
   cmd <- rbind(head,cmd1,cmd2,cmd3,cmd4,cmd5)
   run.xmmSas(cmd, "script2.sas")
   return(cmd)
}
################################################################################
## Fix header keywords
################################################################################
fix.hkeyPn <- function(pn) {
   iext <- 0:1
   for (i in 1:length(iext)) {
      system(paste0("fthedit ", pn, "+", iext[i], " CCDID delete"))
      system(paste0("fthedit ", pn, "+", iext[i], " QUADRANT delete"))
   }
   iext <- 2:13
   for (i in 1:length(iext)) {
      ccdid <- xmmCcdid1[i]
      quadr <- xmmQuadra[i]
      system(paste0("fthedit ", pn, "+", iext[i], " CCDID add ", ccdid))
      system(paste0("fthedit ", pn, "+", iext[i], " QUADRANT add ", quadr))
   }
}

fix.hkeyMos <- function(mos1, mos2) {
   iext <- 0:1
   for (i in 1:length(iext)) {
      system(paste0("fthedit ", mos1, "+", iext[i], " CCDID delete"))
      system(paste0("fthedit ", mos1, "+", iext[i], " CCDNODE delete"))
      system(paste0("fthedit ", mos2, "+", iext[i], " CCDID delete"))
      system(paste0("fthedit ", mos2, "+", iext[i], " CCDNODE delete"))
   }
   iext <- 2:7
   for (i in 1:length(iext)) {
      if ( i == 6 ) {
         ccdid <- xmmCcdid2[i+1]
      } else {
         ccdid <- xmmCcdid2[i]
      }
      system(paste0("fthedit ", mos1, "+", iext[i], " CCDID add ", ccdid))
   }
   iext <- 2:8
   for (i in 1:length(iext)) {
      ccdid <- xmmCcdid2[i]
      system(paste0("fthedit ", mos2, "+", iext[i], " CCDID add ", ccdid))
   }
}
################################################################################
## Compute the viewing direction
################################################################################
comp.view <- function(summaryf, instr, shift, pa, outf) {
   head <- "#!/bin/sh"
   if ( host == "kanab" ) { cmd1 <- paste("source", sasCmd) }
   if ( host == "lina" ) { cmd1 <- paste(".", sasCmd) }
   cmd2 <- "export SAS_CCF=ccf.cif"
   cmd3 <- paste0("export SAS_ODF=", summaryf)
   cmd4 <- "SAS_VERBOSITY=5"
   cmd5 <- paste("strbs",
      paste0("instrument=", instr),
      paste0("ra=", shift$V1),
      paste0("dec=", shift$V2),
      paste0("apos=", pa),
      "odffixout=no",
      paste0("bstoolsout=yes > ", outf))
   cmd <- rbind(head,cmd1,cmd2,cmd3,cmd4,cmd5)
   run.xmmSas(cmd, "script3.sas")
   return(cmd)
}
################################################################################
## Get star track boresight coordinates
################################################################################
get.strbscoords <- function(outf) {
   system(paste("awk '/ended/ {print $0}'", outf, "> line"))
   line <- read.table("line", header=F)
   raStr <- as.double(str_replace_all(line$V1, "ra=", ""))
   deStr <- as.double(str_replace_all(line$V2, "dec=", ""))
   paStr <- as.double(str_replace_all(str_replace_all(line$V3, "apos=", ""), "strbs\\:-", ""))
   if ( paStr < 0 ) { paStr <- paStr + 360 }
   return(data.frame(raStr, deStr, paStr))
}
################################################################################
## Correct detector coordinates
################################################################################
fix.detcoords <- function(summaryf, evf, instr, date, dfcoord) {
   system(paste0("cp -f ", evf, " TMP_", instr, ".FIT"))
   head <- "#!/bin/sh"
   if ( host == "kanab" ) { cmd1 <- paste("source", sasCmd) }
   if ( host == "lina" ) { cmd1 <- paste(".", sasCmd) }
   cmd2 <- "export SAS_CCF=ccf.cif"
   cmd3 <- paste0("export SAS_ODF=", summaryf)
   cmd4 <- "SAS_VERBOSITY=5"
   cmd5 <- paste("edet2sky", 
      paste0("intab=TMP_", instr, ".FIT"),
      paste0("instrument=", instr),
      paste0("datetime=", date),
      paste0("scattra=", dfcoord[1,1]),
      paste0("scattdec=", dfcoord[1,2]),
      paste0("scattapos=", dfcoord[1,3]),
      "calinfostyle=user inputunit=raw")
   cmd <- rbind(head,cmd1,cmd2,cmd3,cmd4,cmd5)
   run.xmmSas(cmd, "script4.sas")
   return(cmd)
}
################################################################################
## Fix data types
################################################################################
fix.dtypes <- function(tmpEv, outEv, refEv, label) {
   
   return.hcoldesc <- function(fitsfile, column) {
      kk <- which(fitsfile$hdr==column)
      return(paste(fitsfile$hdr[seq(kk, kk+4, 2)], collapse=" "))
   }
   
   edit.hkey <- function(fitsref, fitsfix, col, key) {
      kk <- which(fitsref$colNames==col)
      uu <- which(fitsfix$colNames==col)
      jj <- which(str_detect(fitsref$header, paste0(key, kk)))
      fix <- fitsref$header[jj]
      return(str_replace_all(fix, paste0(key, kk), paste0(key, uu)))
   }
   
   fits <- readFITS(tmpEv)
   system(paste("cp -f", tmpEv, "TMP1.FIT"))
   rawx <- round(fits$col[[which(fits$colNames == "RAWX")]], digits=0)
   rawy <- round(fits$col[[which(fits$colNames == "RAWY")]], digits=0)
   detx <- round(fits$col[[which(fits$colNames == "DETX")]], digits=0)
   dety <- round(fits$col[[which(fits$colNames == "DETY")]], digits=0)
   x <- fits$col[[which(fits$colNames == "X")]]
   y <- fits$col[[which(fits$colNames == "Y")]]
   dat <- data.frame(rawx, rawy, detx, dety, x, y)
   write.table(dat, "coords_xy.dat", quote=F, row.names=F, col.names=F)
   c1 <- return.hcoldesc(outEv, "RAWX")
   c2 <- return.hcoldesc(outEv, "RAWY")
   c3 <- return.hcoldesc(outEv, "DETX")
   c4 <- return.hcoldesc(outEv, "DETY")
   c5 <- return.hcoldesc(outEv, "X")
   c6 <- return.hcoldesc(outEv, "Y")
   cmd1 <- rbind(c1,c2,c3,c4,c5,c6)
   write.table(cmd1, "xy.cdesc", quote=F, row.names=F, col.names=F)
   system("fcreate xy.cdesc coords_xy.dat TMP2.FIT clobber=yes")
   system("fdelcol TMP1.FIT+1 RAWX Y Y")
   system("fdelcol TMP1.FIT+1 RAWY Y Y")
   system("fdelcol TMP1.FIT+1 DETX Y Y")
   system("fdelcol TMP1.FIT+1 DETY Y Y")
   system("fdelcol TMP1.FIT+1 X Y Y")
   system("fdelcol TMP1.FIT+1 Y Y Y")
   system("ftpaste TMP1.FIT+1 TMP2.FIT TMP3.FIT clobber=yes")
   fitsFix <- readFITS("TMP3.FIT", hdu=1) 
   fix1 <- edit.hkey(refEv, fitsFix, "RAWX", "TLMIN")
   fix2 <- edit.hkey(refEv, fitsFix, "RAWX", "TLMAX")
   fixrawX <- rbind(fix1,fix2)
   fix1 <- edit.hkey(refEv, fitsFix, "RAWY", "TLMIN")
   fix2 <- edit.hkey(refEv, fitsFix, "RAWY", "TLMAX")
   fixrawY <- rbind(fix1,fix2)
   fix1 <- edit.hkey(refEv, fitsFix, "DETX", "TLMIN")
   fix2 <- edit.hkey(refEv, fitsFix, "DETX", "TLMAX")
   fix3 <- edit.hkey(refEv, fitsFix, "DETX", "TCRPX")
   fix4 <- edit.hkey(refEv, fitsFix, "DETX", "TCDLT")
   fixdetX <- rbind(fix1,fix2,fix3,fix4)
   fix1 <- edit.hkey(refEv, fitsFix, "DETY", "TLMIN")
   fix2 <- edit.hkey(refEv, fitsFix, "DETY", "TLMAX")
   fix3 <- edit.hkey(refEv, fitsFix, "DETY", "TCRPX")
   fix4 <- edit.hkey(refEv, fitsFix, "DETY", "TCDLT")
   fixdetY <- rbind(fix1,fix2,fix3,fix4)
   fix1 <- edit.hkey(outEv, fitsFix, "X", "TCRPX")
   fix2 <- edit.hkey(outEv, fitsFix, "X", "TCTYP")
   fix3 <- edit.hkey(outEv, fitsFix, "X", "TCRVL")
   fix4 <- edit.hkey(outEv, fitsFix, "X", "TCDLT")
   fix5 <- edit.hkey(outEv, fitsFix, "X", "TCUNI")
   fixX <- rbind(fix1,fix2,fix3,fix4,fix5)
   fix1 <- edit.hkey(outEv, fitsFix, "Y", "TCRPX")
   fix2 <- edit.hkey(outEv, fitsFix, "Y", "TCTYP")
   fix3 <- edit.hkey(outEv, fitsFix, "Y", "TCRVL")
   fix4 <- edit.hkey(outEv, fitsFix, "Y", "TCDLT")
   fix5 <- edit.hkey(outEv, fitsFix, "Y", "TCUNI")
   fixY <- rbind(fix1,fix2,fix3,fix4,fix5)
   fixAll <- rbind(fixrawX,fixrawY,fixdetX,fixdetY,fixX,fixY)
   write.table(fixAll, "fix_key.txt", quote=F, row.names=F, col.names=F)
   system("fthedit TMP3.FIT+1 @fix_key.txt operation=add")
   idescriptor <- which(str_detect(refEv$header, "data subspace descriptor"))
   write.table(refEv$header[idescriptor], "fix_gti.txt", quote=F, row.names=F, col.names=F)
   system("fthedit TMP3.FIT+1 @fix_gti.txt operation=add")
   system(paste("cp -f TMP3.FIT", label))
   return(fixAll)
}
################################################################################
## Compute sky coordinates
################################################################################
comp.skycoords <- function(summaryf, evt, dfcoords) {
   head <- "#!/bin/sh"
   if ( host == "kanab" ) { cmd1 <- paste("source", sasCmd) }
   if ( host == "lina" ) { cmd1 <- paste(".", sasCmd) }
   cmd2 <- "export SAS_CCF=ccf.cif"
   cmd3 <- paste0("export SAS_ODF=", summaryf)
   cmd4 <- "SAS_VERBOSITY=5"
   cmd5 <- paste("attcalc", 
      paste0("eventset=", evt),
      "refpointlabel=nom",
      "attitudelabel=fixed",
      paste0("fixedra=", dfcoords[1,1]),
      paste0("fixeddec=", dfcoords[1,2]),
      paste0("fixedposangle=", dfcoords[1,3]))
   cmd <- rbind(head,cmd1,cmd2,cmd3,cmd4,cmd5)
   run.xmmSas(cmd, "script5.sas")
   return(cmd)
}
################################################################################
## Write SAS scripts
################################################################################
write.allsas <- function(s1,s2,s3,s4,s5, script) {
   a2 <- s2[which(row.names(s2)=="cmd3")]
   a3 <- s2[which(row.names(s2)=="cmd4")]
   r2 <- s2[which(row.names(s2)=="cmd5")]
   r3 <- s3[which(row.names(s3)=="cmd5")]
   r4 <- s4[which(row.names(s4)=="cmd5")]
   r5 <- s5[which(row.names(s5)=="cmd5")]
   lis <- rbind(s1,a2,a3,r2,r3,r4,r5)
   write.table(lis, script, quote = F, row.names = F, col.names = F)
}
################################################################################
## Run detection chain for testing SAS compliance
################################################################################
run.detchain <- function(summaryf, evt, att, instr, index) {
   
   obsid <- strsplit(summaryf, split = "_")[[1]][2]
   label <- paste(instr, obsid, sep = "_")
   
   pimin <- 500
   pimax <- 2000
   ecf <- c(6.577, 1.930, 1.893)
   mlmin <- 6
   
   selPi <- paste0("(PI in [", pimin, ":", pimax, "])")
   if ( instr == "epn" ) {
      selPa <- paste0("&& (PATTERN in [0:4]) && (FLAG==0)")
   } else { 
      selPa <- paste0("&& (PATTERN in [0:12]) && (FLAG==0)")
   }
   selections <- paste(selPi, selPa)
   
   message("Instrument: ", instr, "\nBand ", paste(pimin, pimax, sep=":"), " eV")
   
   head <- "#!/bin/sh"
   if ( host == "kanab" ) { cmd1 <- paste("source", sasCmd) }
   if ( host == "lina" ) { cmd1 <- paste(".", sasCmd) }
   cmd2 <- "export SAS_CCF=ccf.cif"
   cmd3 <- paste0("export SAS_ODF=", summaryf)
   cmd4 <- "SAS_VERBOSITY=5"
   
   task1 <- paste("evselect", 
      paste0("table=", evt, ":EVENTS"),
      "imagebinning=binSize",
      paste0("imageset=", label, "_img.fits"),
      "withimageset=yes xcolumn=X ycolumn=Y",
      "ximagebinsize=80 yimagebinsize=80",
      paste0("expression='", selections, "'"))
   
   task2 <- paste("eexpmap",
      paste0("attitudeset=", att),
      paste0("eventset=", evt),
      paste0("imageset=", label, "_img.fits"),
      paste0("expimageset=", label, "_exp.fits"),
      paste0("pimin=", pimin, " pimax=", pimax))
   
   task3 <- paste("emask",
      paste0("expimageset=", label, "_exp.fits"),
      "threshold1=0.5 threshold2=1.0",
      paste0("detmaskset=", label, "_msk.fits"))
   
   task4 <- paste("eboxdetect",
      "usemap=no likemin=5 withdetmask=yes nruns=1",
      paste0("detmasksets=", label, "_msk.fits"),
      paste0("imagesets=", label, "_img.fits"),
      paste0("expimagesets=", label, "_exp.fits"),
      paste0("pimin=", pimin, " pimax=", pimax),
      paste0("ecf=", ecf[index]),
      paste0("boxlistset=", label, "_eboxlist_local.fits"))
   
   task5 <- paste("esplinemap",
      paste0("bkgimageset=", label, "_bkg.fits"),
      "scut=0.002",
      paste0("imageset=", label, "_img.fits"),
      "smoothsigma=15 nsplinenodes=13 withdetmask=yes",
      paste0("detmaskset=", label, "_msk.fits"),
      "nfitrun=4 withcheese=yes",
      paste0("cheeseimageset=", label, "_cheese.fits"),
      "withexpimage=yes",
      paste0("expimageset=", label, "_exp.fits"),
      paste0("boxlistset=", label, "_eboxlist_local.fits"))
   
   task6 <- paste("eboxdetect",
      "usemap=yes", 
      paste0("bkgimagesets=", label, "_bkg.fits"),
      "likemin=5 withdetmask=yes nruns=3",
      paste0("detmasksets=", label, "_msk.fits"),
      paste0("imagesets=", label, "_img.fits"),
      paste0("expimagesets=", label, "_exp.fits"),
      paste0("pimin=", pimin, " pimax=", pimax),
      paste0("ecf=", ecf[index]), 
      paste0("boxlistset=", label, "_eboxlist_map.fits"))
   
   task7 <- paste("emldetect",
      paste0("imagesets=", label, "_img.fits"),
      paste0("expimagesets=", label, "_exp.fits"),
      paste0("bkgimagesets=", label, "_bkg.fits"),
      paste0("boxlistset=", label, "_eboxlist_map.fits"),
      paste0("ecf=", ecf[index]),
      "nmaxfit=1 nmulsou=2 fitposition=yes ecut=15 scut=0.9",
      "withthreshold=yes threshold=10 threshcolumn=LIKE",
      "withtwostage=yes extentmodel=beta determineerrors=yes",
      "dmlextmin=4 fitextent=yes minextent=1.5 maxextent=20",
      paste0("pimin=", pimin, " pimax=", pimax), 
      paste0("mlmin=", mlmin),
      paste0("mllistset=", label, "_emllist.fits"))
   
   tasks <- rbind(task1,task2,task3,task4,task5,task6,task7)
   detCmd <- rbind(head,cmd1,cmd2,cmd3,cmd4,tasks)
   run.xmmSas(detCmd, paste0("script_dettest_", instr, ".sas"))
}

################################################################################
