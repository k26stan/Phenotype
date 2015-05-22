## Check out Full-Time Response Data and Make Phenotype/Cov Files for Assoc Tests ##
## Janssen RA Cohort ##
## June 03, 2014 ##
## Updated February 26, 2015 ##
## Kristopher Standish ##

######################################
## LOAD DATA #########################
######################################

# Set Date
DATE <- "20150520"

# TSCC Paths
PathToData <- "/projects/janssen/clinical/"
PathToSave <- "/projects/janssen/clinical/Plots/20140522/"

# Mac Paths
PathToData <- "/Users/kstandis/Data/Burn/Data/Phenos/"
PathToSave <- paste("/Users/kstandis/Data/Burn/Plots/",DATE,sep="")
PathToPheno <- "/Users/kstandis/Data/Burn/Phenos/"

## Previously Compiled Data
DR <- read.table(paste(PathToData,"Raw_Files/DRSA436EE.txt",sep=""),sep="\t",header=T)
FT <- read.table(paste(PathToData,"Raw_Files/20140507_FULL_RESP.csv",sep=""),sep=",",header=T)
NEW <- read.table(paste(PathToData,"Raw_Files/DAS_Observed_wk20_wk24.csv",sep=""),sep=",",header=T)
RAD <- read.table(paste(PathToData,"Raw_Files/20150306_Radiograph_Data.csv",sep=""),sep=",",header=T)
EIGEN <- read.table(paste(PathToData,"EIGEN/HC_FULL.eigenvec",sep=""),header=T)

## Compare Older FT Files
FT.609 <- read.table(paste(PathToData,"20141227_and_Before/20140609_Full_Table.txt",sep=""),sep="\t",header=T)
FT.610 <- read.table(paste(PathToData,"20141227_and_Before/20140610_Full_Table.txt",sep=""),sep="\t",header=T)
FT.1106 <- read.table(paste(PathToData,"20141227_and_Before/20141106_Full_Table.txt",sep=""),sep="\t",header=T)
FT.1119 <- read.table(paste(PathToData,"20141227_and_Before/20141119_Full_Table.txt",sep=""),sep="\t",header=T)
setdiff( names(FT.610),names(FT.609) )

## Merge Tables
MG.a <- merge(x=DR,y=FT,by.x="ID_2",by.y="Custom_ID")

## Pull out and Sort Eigen Patients in Full Table
EIGEN <- EIGEN[ which(EIGEN[,1] %in% MG.a[,1]), 2:ncol(EIGEN) ]
rownames( EIGEN ) <- as.character(EIGEN[,1])
EIGEN <- EIGEN[ order(as.character(EIGEN[,1])), 2:ncol(EIGEN) ]

######################################
## INCLUDE NEW DAS DATA (WK20,24) ####
######################################

## Convert 3 new DAS columns to "numeric"
for ( col in 2:4 ) {
	NEW[,col] <- as.numeric(as.character( NEW[,col] ))
}
 # Check correlation w/ previous DAS values
par(mfrow=c(1,3))
plot( NEW[,c(2,54)] )
plot( NEW[,c(3,55)] )
plot( NEW[,c(4,56)] )
pairs( NEW[,c(2:4,54:56)] )
cor( NEW[,c(2:4,54:56)], use="pairwise.complete.obs", method="pearson" )

## Pull out New Values into minimal data frame
NEW.min <- data.frame( NEW[,1:4] )

## Merge NEW minimal data frame w/ MG & Filter
MG.b <- merge( x=MG.a, y=NEW.min, by.x="ID_2", by.y="Custom_ID" )
MG.c <- MG.b
MG.c[,"DAS_16wk"] <- MG.c[,"DAS_16wk_ob"]
MG.c[,"DAS_20wk"] <- MG.c[,"DAS_20wk_ob"]
MG.c[,"DAS_24wk"] <- MG.c[,"DAS_24wk_ob"]

## Set new MG data.frame
MG <- MG.c[ , 1:(ncol(MG.c)-3) ]

######################################
## INCLUDE NEW DAS DATA (WK20,24) #### FT.610
######################################

## Merge NEW minimal data frame w/ MG & Filter
MG.610.b <- merge( x=FT.610, y=NEW.min, by.x="ID_2", by.y="Custom_ID" )
MG.610.c <- MG.610.b
MG.610.c[,"DAS_16wk"] <- MG.610.c[,"DAS_16wk_ob"]
MG.610.c[,"DAS_20wk"] <- MG.610.c[,"DAS_20wk_ob"]
MG.610.c[,"DAS_24wk"] <- MG.610.c[,"DAS_24wk_ob"]

## Set new MG data.frame
MG.610 <- MG.610.c[ , 1:(ncol(MG.610.c)-3) ]

######################################
## SUMMARIZE TABLES ################## NAMES.0610.MG <- setdiff( names(FT.610), names(MG) )
######################################

## Full-Time Table ##
 # 1:8 - Subject Data
 # 9:24 - CRP Levels
 # 25:40 - CRPPI Levels...% Change from Wk0 - Negative Values are INCREASES in CRP
 # 41:56 - DAS Scores
 #*57 - Scores shifted so Baseline is immediately before receiving Drug (See exact description in email)
 #*58:61 - DAS/Delta-DAS Shifted so Time Indicated is After first Golimumab Treatment
 # 62:77 - Change in DAS
 # 78:87,91:93,97:99 - EULAR Response
 #*88:90,94:96 - EULAR Response Shifted so Time Indicated is After first Golimumab Treatment
 # 100:115 - DASFLG_?wk: Y/N - Patient have "Good" or "Moderate" Response at this time?
 # 116:131 - DASREM_?wk: Y/N - Patient in Clinical Remission at this time?
 # 132:147 - SJC_?wk: Swollen Joint Count
 # 148:163 - TJC_?wk: Tender Joint Count
 # 164:179 - SJC28_?wk: Swollen Joint Count (out of 28)
 # 180:195 - TJC28_?wk: Tender Joint Count (out of 28)
 # 196:211 - ACR20_?wk
 # 212:227 - ACR50_?wk
 # 228:243 - ACR70_?wk
 # 244:259 - ACR90_?wk

## Specify Column Numbers for Pertinent Columns in MG
DAS_COLS <- 115:130 #41:56+74 # DAS Scores in MG Table
CRP_COLS <- 83:98 # CRP Levels in MG Table
SJC_COLS <- 206:221 # Swollen Joint Count in MG Table
TJC_COLS <- 222:237 # Tender Joint Count in MG Table

## Fill in Missing Data (NA Values)
for ( c in 2:length(DAS_COLS) ) {
	col <- DAS_COLS[c]
	WHICH <- which(is.na( MG[,col] ))
	if (length(WHICH)>0) { MG[WHICH,col] <- MG[WHICH,col-1] }
	WHICH <- which(is.na( MG.610[,col] ))
	if (length(WHICH)>0) { MG.610[WHICH,col] <- MG.610[WHICH,col-1] }
}

## Specify Weeks for each Column and Such
WKS <- c(0,2,4,8,12,14,16,20,24,28,36,44,52,68,84,100)
COL_WK <- c(rep(NA,8),rep(WKS,3),rep(NA,5),WKS,WKS[1:10],rep(NA,3),WKS[11:13],rep(NA,3),WKS[14:16],rep(WKS,10) )
MG_WK <- c(rep(NA,74),COL_WK)

## Specify Arms/Groups: Gol, Plac, PlacEE
G <- which(MG$Drug=="Golimumab")
P <- which(MG$Drug=="Placebo" & MG$EE=="N")
PE <- which(MG$Drug=="Placebo" & MG$EE=="Y")
GRP <- rep("G",nrow(MG)) ; GRP[P] <- "P" ; GRP[PE] <- "PE"

## Specify Baseline Week for each Group
BL <- rep(0,nrow(MG))
BL[P] <- 24
BL[PE] <- 16

## How To Specify Columns at Baseline
for (row in 430:nrow(MG)) {
	COLNAME <- paste("DAS_",BL[row],"wk",sep="")
	print(COLNAME) ; #print( MG$Combined.Baseline.DAS[row]==MG[row,COLNAME] )
}

######################################
## DROP-OUTS #########################
######################################

## Identify week that each person dropped out of study
DP <- array(0,dim=c(nrow(MG),4)) ; colnames(DP) <- c("DAS","CRP","SJ","TJ") ; rownames(DP) <- MG$ID
for (row in 1:nrow(MG)) {
	if ( length(which(duplicated(c(MG[row,DAS_COLS]))))!=(length(DAS_COLS)-1) ) {
		DP[row,1] <- WKS[tail(which(MG[row,DAS_COLS]!=MG[row,DAS_COLS[length(DAS_COLS)]]),1)+1] }
	DP[row,2] <- WKS[tail(which(!is.na(MG[row,CRP_COLS])),1)]
	DP[row,3] <- WKS[tail(which(!is.na(MG[row,SJC_COLS])),1)]
	DP[row,4] <- WKS[tail(which(!is.na(MG[row,TJC_COLS])),1)]
}

## Some variables for Dropping Out of Study
FN_DAS <- DP[,1] # Final DAS measurement
FN_CRP <- DP[,2] # Final CRP measurement
FN_SJC <- DP[,3] # Final SJ measurement
FN_TJC <- DP[,4] # Final TJ measurement
IN <- FN_DAS-BL # Number of Weeks on Golimumab (based on DAS)

######################################
## WHICH INDIVS TO REMOVE (DROPOUT) ##
######################################
attach(MG)

## For Early Dropout
LT12 <- which(IN<12) # Anybody w/ less than 12 weeks of Response data post-Gol treatment
LT8 <- which(IN<8) # Chris Huang's Recommendation
LT24 <- which(IN<28) # Anybody without new DAS at wk 28
data.frame(MG$ID[LT8],BL[LT8],IN[LT8])

################################################################################################
################################################################################################
## DISEASE ACTIVITY SCORE (DAS) ################################################################
################################################################################################
################################################################################################

######################################
## CALCULATE INITIAL DAS #############
######################################
## Get DAS at & before Baseline for each Sample (BASELINE=START GOLIMUMAB)
DAS_BL <- DAS_BL_MN <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get single baseline DAS score
	COLNAME <- paste("DAS_",BL[row],"wk",sep="")
	DAS_BL[row] <- MG[row,COLNAME]
	# Get mean DAS score before Gol treatment
	COLNAMES <- paste("DAS_",WKS[which(WKS<=BL[row])],"wk",sep="")
	if (length(COLNAMES)==1) { DAS_BL_MN[row] <- MG[row,COLNAMES] }
	else { DAS_BL_MN[row] <- rowMeans(MG[row,COLNAMES]) }
}

# DAS_BL = DAS at week of first Golimumab Treatment
# DAS_BL_MN = Mean DAS before first Golimumab Treatment

######################################
## CALCULATE POST-TREATMENT DAS ######
######################################

## Times to use: (WAG=Weeks after 1st Golimumab)
 # 4WAG, 12WAG, 20WAG, *28WAG, *52WAG
   # *Adjusted.DAS_??wk
DAS_ADJ_28WAG <- MG$Adjusted.DAS_28wk
DAS_ADJ_52WAG <- MG$Adjusted.DAS_52wk
DAS_ADJ_MN <- (DAS_ADJ_28WAG+DAS_ADJ_52WAG) / 2
   # *Adjusted.DASCH_??wk

## Get DAS after Golimumab Treatment
DAS_PG_MNe <- DAS_PG_MNi <- DAS_PG_4WAG <- DAS_PG_12WAG <- DAS_PG_20WAG <- DAS_PG_28WAG <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get DAS score 4WAG
	COLNAME <- paste("DAS_",BL[row]+4,"wk",sep="")
	DAS_PG_4WAG[row] <- MG[row,COLNAME]
	# Get DAS score 12WAG
	COLNAME <- paste("DAS_",BL[row]+12,"wk",sep="")
	DAS_PG_12WAG[row] <- MG[row,COLNAME]
	# Get DAS score 20WAG
	COLNAME <- paste("DAS_",BL[row]+20,"wk",sep="")
	DAS_PG_20WAG[row] <- MG[row,COLNAME]
	# Get DAS score 28WAG
	COLNAME <- paste("DAS_",BL[row]+28,"wk",sep="")
	DAS_PG_28WAG[row] <- MG[row,COLNAME]
	# Get mean DAS score after Gol treatment (including All data)
	COLNAMES <- paste("DAS_",WKS[which(WKS>BL[row])],"wk",sep="")
	DAS_PG_MNi[row] <- rowMeans(MG[row,COLNAMES])
	# Get mean DAS score after Gol treatment (excluding Censored data)
	if (length(WKS[which(WKS>BL[row] & WKS<=FN_DAS[row])])>0) {
		COLNAMES <- paste("DAS_",WKS[which(WKS>BL[row] & WKS<=FN_DAS[row])],"wk",sep="")
		if (length(COLNAMES)==1) { DAS_PG_MNe[row] <- MG[row,COLNAMES] }
		else { DAS_PG_MNe[row] <- rowMeans(MG[row,COLNAMES]) }
	}
	else { DAS_PG_MNe[row] <- NA }
}

# Plot Post-Gol measurements vs each other
library(gplots)
# # pairs(data.frame(DAS_PG_MNe,DAS_PG_MNi,DAS_PG_4WAG,DAS_PG_12WAG,DAS_PG_20WAG,DAS_PG_28WAG,DAS_ADJ_28WAG,DAS_ADJ_52WAG),pch="+",col=c("green","blue","red")[factor(GRP)])
DAS_PG_COR <- cor(data.frame(DAS_PG_MNe,DAS_PG_MNi,DAS_PG_4WAG,DAS_PG_12WAG,DAS_PG_20WAG,DAS_PG_28WAG,DAS_ADJ_28WAG,DAS_ADJ_52WAG), use="complete.obs")
# heatmap.2(DAS_PG_COR, col=colorRampPalette(c("purple","black","green"))(30), trace="none")
# plot(DAS_PG_MNi,DAS_PG_MNe, main="Mean DAS after Gol Treatment - w/ or w/o Censored", xlab="Indluding",ylab="Excluding")

# DAS_PG_MNi = Mean DAS after first Golimumab Treatment (All Timepoints)
# DAS_PG_MNe = Mean DAS after first Golimumab Treatment (Excluding Censored)
# DAS_PG_4WAG = DAS score 4 weeks after first Golimumab Treatment
# DAS_PG_12WAG = DAS score 12 weeks after first Golimumab Treatment
# DAS_PG_20WAG = DAS score 20 weeks after first Golimumab Treatment
# DAS_PG_28WAG = DAS score 28 weeks after first Golimumab Treatment
# DAS_ADJ_28WAG = DAS score 28 weeks after first Golimumab Treatment (Same as above)
# DAS_ADJ_52WAG = DAS score 52 weeks after first Golimumab Treatment (Technically no 52 for P group)

######################################
## CALCULATE DELTA DAS ###############
######################################

## vs Baseline DAS (just prior to initial Gol treatment)
DEL_MNi_BL <- DAS_PG_MNi - DAS_BL
DEL_MNe_BL <- DAS_PG_MNe - DAS_BL
DEL_4_BL <- DAS_PG_4WAG - DAS_BL
DEL_12_BL <- DAS_PG_12WAG - DAS_BL
DEL_20_BL <- DAS_PG_20WAG - DAS_BL
DEL_28_BL <- DAS_ADJ_28WAG - DAS_BL
DEL_52_BL <- DAS_ADJ_52WAG - DAS_BL

## vs Mean DAS prior to initial Gol treatment
DEL_MNi_MN <- DAS_PG_MNi - DAS_BL_MN
DEL_MNe_MN <- DAS_PG_MNe - DAS_BL_MN
DEL_4_MN <- DAS_PG_4WAG - DAS_BL_MN
DEL_12_MN <- DAS_PG_12WAG - DAS_BL_MN
DEL_20_MN <- DAS_PG_20WAG - DAS_BL_MN
DEL_28_MN <- DAS_ADJ_28WAG - DAS_BL_MN
DEL_52_MN <- DAS_ADJ_52WAG - DAS_BL_MN

## Compile all these Phenotypes
PG_DAS <- data.frame( DAS_PG_MNi, DAS_PG_MNe, DAS_PG_4WAG, DAS_PG_12WAG, DAS_PG_20WAG, DAS_PG_28WAG )
DEL_DAS <- data.frame( DAS_BL, DAS_BL_MN, DEL_MNi_BL, DEL_MNe_BL, DEL_4_BL, DEL_12_BL, DEL_20_BL, DEL_28_BL, DEL_52_BL,
	DEL_MNi_MN, DEL_MNe_MN, DEL_4_MN, DEL_12_MN, DEL_20_MN, DEL_28_MN, DEL_52_MN )
DEL_DAS_PG_COR <- cor( DEL_DAS, use="pairwise.complete.obs")
COLS <- colorRampPalette(c("firebrick1","sienna1","black","chartreuse1","steelblue1"))(50)
BRKS <- seq(-1,1,length.out=51)
heatmap.2(DEL_DAS_PG_COR, col=COLS, breaks=BRKS, trace="none")

################################################################################################
################################################################################################
## SWOLLEN JOINT COUNT (SJC) ###################################################################
################################################################################################
################################################################################################

######################################
## CALCULATE INITIAL SJC #############
######################################
## Get SJC at & before Baseline for each Sample (BASELINE=START GOLIMUMAB)
SJC_BL <- SJC_BL_MN <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get single baseline SJC
	COLNAME <- paste("SJC_",BL[row],"wk",sep="")
	SJC_BL[row] <- MG[row,COLNAME]
	# Get mean SJC before Gol treatment
	COLNAMES <- paste("SJC_",WKS[which(WKS<=BL[row])],"wk",sep="")
	if (length(COLNAMES)==1) { SJC_BL_MN[row] <- MG[row,COLNAMES] }
	else { SJC_BL_MN[row] <- rowMeans(MG[row,COLNAMES]) }
}

# SJC_BL = SJC at week of first Golimumab Treatment
# SJC_BL_MN = Mean SJC before first Golimumab Treatment

######################################
## CALCULATE POST-TREATMENT SJC ######
######################################

## Get SJC after Golimumab Treatment
SJC_PG_MNe <- SJC_PG_MNi <- SJC_PG_4WAG <- SJC_PG_12WAG <- SJC_PG_20WAG <- SJC_PG_28WAG <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get SJC score 4WAG
	COLNAME <- paste("SJC_",BL[row]+4,"wk",sep="")
	SJC_PG_4WAG[row] <- MG[row,COLNAME]
	# Get SJC score 12WAG
	COLNAME <- paste("SJC_",BL[row]+12,"wk",sep="")
	SJC_PG_12WAG[row] <- MG[row,COLNAME]
	# Get SJC score 20WAG
	COLNAME <- paste("SJC_",BL[row]+20,"wk",sep="")
	SJC_PG_20WAG[row] <- MG[row,COLNAME]
	# Get SJC score 28WAG
	COLNAME <- paste("SJC_",BL[row]+28,"wk",sep="")
	SJC_PG_28WAG[row] <- MG[row,COLNAME]
	# Get mean SJC score after Gol treatment (including All data)
	COLNAMES <- paste("SJC_",WKS[which(WKS>BL[row])],"wk",sep="")
	SJC_PG_MNi[row] <- rowMeans(MG[row,COLNAMES])
	# Get mean SJC score after Gol treatment (excluding Censored data)
	if (length(WKS[which(WKS>BL[row] & WKS<=FN_SJC[row])])>0) {
		COLNAMES <- paste("SJC_",WKS[which(WKS>BL[row] & WKS<=FN_SJC[row])],"wk",sep="")
		if (length(COLNAMES)==1) {
			SJC_PG_MNe[row] <- MG[row,COLNAMES]
		}else{ SJC_PG_MNe[row] <- rowMeans(MG[row,COLNAMES]) }
	}else{ SJC_PG_MNe[row] <- NA }
}

# Plot Post-Gol measurements vs each other
# library(gplots)
# pairs(data.frame(SJC_PG_MNe,SJC_PG_MNi,SJC_PG_4WAG,SJC_PG_12WAG,SJC_PG_20WAG,SJC_PG_28WAG),pch="+",col=c("green","blue","red")[factor(GRP)])
SJC_PG_COR <- cor(data.frame(SJC_PG_MNe,SJC_PG_MNi,SJC_PG_4WAG,SJC_PG_12WAG,SJC_PG_20WAG,SJC_PG_28WAG), use="pairwise.complete.obs", method="spearman")
# heatmap.2(SJC_PG_COR, col=colorRampPalette(c("purple","black","green"))(30), trace="none")
# plot(SJC_PG_MNi,SJC_PG_MNe, main="Mean SJC after Gol Treatment - w/ or w/o Censored", xlab="Indluding",ylab="Excluding")

# SJC_PG_MNi = Mean SJC after first Golimumab Treatment (All Timepoints)
# SJC_PG_MNe = Mean SJC after first Golimumab Treatment (Excluding Censored)
# SJC_PG_4WAG = SJC score 4 weeks after first Golimumab Treatment
# SJC_PG_12WAG = SJC score 12 weeks after first Golimumab Treatment
# SJC_PG_20WAG = SJC score 20 weeks after first Golimumab Treatment
# SJC_PG_28WAG = SJC score 28 weeks after first Golimumab Treatment

######################################
## CALCULATE DELTA SJC ###############
######################################

## vs Baseline SJC (just prior to initial Gol treatment)
DEL_SJC_MNi_BL <- SJC_PG_MNi - SJC_BL
DEL_SJC_MNe_BL <- SJC_PG_MNe - SJC_BL
DEL_SJC_4_BL <- SJC_PG_4WAG - SJC_BL
DEL_SJC_12_BL <- SJC_PG_12WAG - SJC_BL
DEL_SJC_20_BL <- SJC_PG_20WAG - SJC_BL
DEL_SJC_28_BL <- SJC_PG_28WAG - SJC_BL

## vs Mean SJC prior to initial Gol treatment
DEL_SJC_MNi_MN <- SJC_PG_MNi - SJC_BL_MN
DEL_SJC_MNe_MN <- SJC_PG_MNe - SJC_BL_MN
DEL_SJC_4_MN <- SJC_PG_4WAG - SJC_BL_MN
DEL_SJC_12_MN <- SJC_PG_12WAG - SJC_BL_MN
DEL_SJC_20_MN <- SJC_PG_20WAG - SJC_BL_MN
DEL_SJC_28_MN <- SJC_PG_28WAG - SJC_BL_MN

## Compile all these Phenotypes
PG_SJC <- data.frame( SJC_PG_MNi, SJC_PG_MNe, SJC_PG_4WAG, SJC_PG_12WAG, SJC_PG_20WAG, SJC_PG_28WAG )
DEL_SJC <- data.frame( SJC_BL, SJC_BL_MN, DEL_SJC_MNi_BL, DEL_SJC_MNe_BL, DEL_SJC_4_BL, DEL_SJC_12_BL, DEL_SJC_20_BL, DEL_SJC_28_BL,
	DEL_SJC_MNi_MN, DEL_SJC_MNe_MN, DEL_SJC_4_MN, DEL_SJC_12_MN, DEL_SJC_20_MN, DEL_SJC_28_MN )
DEL_SJC_PG_COR <- cor( DEL_SJC, use="pairwise.complete.obs")
COLS <- colorRampPalette(c("firebrick1","sienna1","black","chartreuse1","steelblue1"))(50)
BRKS <- seq(-1,1,length.out=51)
heatmap.2(DEL_SJC_PG_COR, col=COLS, breaks=BRKS, trace="none")

################################################################################################
################################################################################################
## SQUARE ROOT of SWOLLEN JOINT COUNT (rSJC) ###################################################
################################################################################################
################################################################################################

## Transformations
# Windsorize
 # Like clipping
# Box Cox
# OX <- function(DATA,param) { NEW_DATA <- (DATA^param - 1)/param }

######################################
## CALCULATE INITIAL rSJC ############
######################################
## Get rSJC at & before Baseline for each Sample (BASELINE=START GOLIMUMAB)
rSJC_BL <- rSJC_BL_MN <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get single baseline rSJC
	COLNAME <- paste("SJC_",BL[row],"wk",sep="")
	rSJC_BL[row] <- sqrt(MG[row,COLNAME])
	# Get mean rSJC before Gol treatment
	COLNAMES <- paste("SJC_",WKS[which(WKS<=BL[row])],"wk",sep="")
	if (length(COLNAMES)==1) { rSJC_BL_MN[row] <- sqrt(MG[row,COLNAMES]) }
	else { rSJC_BL_MN[row] <- rowMeans(sqrt(MG[row,COLNAMES])) }
}

# rSJC_BL = rSJC at week of first Golimumab Treatment
# rSJC_BL_MN = Mean rSJC before first Golimumab Treatment

######################################
## CALCULATE POST-TREATMENT rSJC #####
######################################

## Get rSJC after Golimumab Treatment
rSJC_PG_MNe <- rSJC_PG_MNi <- rSJC_PG_4WAG <- rSJC_PG_12WAG <- rSJC_PG_20WAG <- rSJC_PG_28WAG <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get rSJC score 4WAG
	COLNAME <- paste("SJC_",BL[row]+4,"wk",sep="")
	rSJC_PG_4WAG[row] <- sqrt(MG[row,COLNAME])
	# Get rSJC score 12WAG
	COLNAME <- paste("SJC_",BL[row]+12,"wk",sep="")
	rSJC_PG_12WAG[row] <- sqrt(MG[row,COLNAME])
	# Get rSJC score 20WAG
	COLNAME <- paste("SJC_",BL[row]+20,"wk",sep="")
	rSJC_PG_20WAG[row] <- sqrt(MG[row,COLNAME])
	# Get rSJC score 28WAG
	COLNAME <- paste("SJC_",BL[row]+28,"wk",sep="")
	rSJC_PG_28WAG[row] <- sqrt(MG[row,COLNAME])
	# Get mean rSJC score after Gol treatment (including All data)
	COLNAMES <- paste("SJC_",WKS[which(WKS>BL[row])],"wk",sep="")
	rSJC_PG_MNi[row] <- rowMeans(sqrt(MG[row,COLNAMES]))
	# Get mean rSJC score after Gol treatment (excluding Censored data)
	if (length(WKS[which(WKS>BL[row] & WKS<=FN_SJC[row])])>0) {
		COLNAMES <- paste("SJC_",WKS[which(WKS>BL[row] & WKS<=FN_SJC[row])],"wk",sep="")
		if (length(COLNAMES)==1) {
			rSJC_PG_MNe[row] <- sqrt(MG[row,COLNAMES])
		}else{ rSJC_PG_MNe[row] <- rowMeans(sqrt(MG[row,COLNAMES])) }
	}else{ rSJC_PG_MNe[row] <- NA }
}

# Plot Post-Gol measurements vs each other
# library(gplots)
# pairs(data.frame(rSJC_PG_MNe,rSJC_PG_MNi,rSJC_PG_4WAG,rSJC_PG_12WAG,rSJC_PG_20WAG,rSJC_PG_28WAG),pch="+",col=c("green","blue","red")[factor(GRP)])
rSJC_PG_COR <- cor(data.frame(rSJC_PG_MNe,rSJC_PG_MNi,rSJC_PG_4WAG,rSJC_PG_12WAG,rSJC_PG_20WAG,rSJC_PG_28WAG), use="pairwise.complete.obs", method="spearman")
heatmap.2(rSJC_PG_COR, col=colorRampPalette(c("purple","black","green"))(30), trace="none")
plot(rSJC_PG_MNi,rSJC_PG_MNe, main="Mean SJC after Gol Treatment - w/ or w/o Censored", xlab="Indluding",ylab="Excluding")

# rSJC_PG_MNi = Mean rSJC after first Golimumab Treatment (All Timepoints)
# rSJC_PG_MNe = Mean rSJC after first Golimumab Treatment (Excluding Censored)
# rSJC_PG_4WAG = rSJC score 4 weeks after first Golimumab Treatment
# rSJC_PG_12WAG = rSJC score 12 weeks after first Golimumab Treatment
# rSJC_PG_20WAG = rSJC score 20 weeks after first Golimumab Treatment
# rSJC_PG_28WAG = rSJC score 28 weeks after first Golimumab Treatment

######################################
## CALCULATE DELTA rSJC ##############
######################################

## vs Baseline rSJC (just prior to initial Gol treatment)
DEL_rSJC_MNi_BL <- rSJC_PG_MNi - rSJC_BL
DEL_rSJC_MNe_BL <- rSJC_PG_MNe - rSJC_BL
DEL_rSJC_4_BL <- rSJC_PG_4WAG - rSJC_BL
DEL_rSJC_12_BL <- rSJC_PG_12WAG - rSJC_BL
DEL_rSJC_20_BL <- rSJC_PG_20WAG - rSJC_BL
DEL_rSJC_28_BL <- rSJC_PG_28WAG - rSJC_BL

## vs Mean rSJC prior to initial Gol treatment
DEL_rSJC_MNi_MN <- rSJC_PG_MNi - rSJC_BL_MN
DEL_rSJC_MNe_MN <- rSJC_PG_MNe - rSJC_BL_MN
DEL_rSJC_4_MN <- rSJC_PG_4WAG - rSJC_BL_MN
DEL_rSJC_12_MN <- rSJC_PG_12WAG - rSJC_BL_MN
DEL_rSJC_20_MN <- rSJC_PG_20WAG - rSJC_BL_MN
DEL_rSJC_28_MN <- rSJC_PG_28WAG - rSJC_BL_MN

## Compile all these Phenotypes
PG_rSJC <- data.frame( rSJC_PG_MNi, rSJC_PG_MNe, rSJC_PG_4WAG, rSJC_PG_12WAG, rSJC_PG_20WAG, rSJC_PG_28WAG )
DEL_rSJC <- data.frame( rSJC_BL, rSJC_BL_MN, DEL_rSJC_MNi_BL, DEL_rSJC_MNe_BL, DEL_rSJC_4_BL, DEL_rSJC_12_BL, DEL_rSJC_20_BL, DEL_rSJC_28_BL,
	DEL_rSJC_MNi_MN, DEL_rSJC_MNe_MN, DEL_rSJC_4_MN, DEL_rSJC_12_MN, DEL_rSJC_20_MN, DEL_rSJC_28_MN )
DEL_rSJC_PG_COR <- cor( DEL_rSJC, use="pairwise.complete.obs")
COLS <- colorRampPalette(c("firebrick1","sienna1","black","chartreuse1","steelblue1"))(50)
BRKS <- seq(-1,1,length.out=51)
heatmap.2(DEL_rSJC_PG_COR, col=COLS, breaks=BRKS, trace="none")

################################################################################################
################################################################################################
## TENDER JOINT COUNT (TJC) ####################################################################
################################################################################################
################################################################################################

######################################
## CALCULATE INITIAL TJC #############
######################################
## Get TJC at & before Baseline for each Sample (BASELINE=START GOLIMUMAB)
TJC_BL <- TJC_BL_MN <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get single baseline TJC
	COLNAME <- paste("TJC_",BL[row],"wk",sep="")
	TJC_BL[row] <- MG[row,COLNAME]
	# Get mean TJC before Gol treatment
	COLNAMES <- paste("TJC_",WKS[which(WKS<=BL[row])],"wk",sep="")
	if (length(COLNAMES)==1) { TJC_BL_MN[row] <- MG[row,COLNAMES] }
	else { TJC_BL_MN[row] <- rowMeans(MG[row,COLNAMES]) }
}

# TJC_BL = TJC at week of first Golimumab Treatment
# TJC_BL_MN = Mean TJC before first Golimumab Treatment

######################################
## CALCULATE POST-TREATMENT TJC ######
######################################

## Get TJC after Golimumab Treatment
TJC_PG_MNe <- TJC_PG_MNi <- TJC_PG_4WAG <- TJC_PG_12WAG <- TJC_PG_20WAG <- TJC_PG_28WAG <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get TJC score 4WAG
	COLNAME <- paste("TJC_",BL[row]+4,"wk",sep="")
	TJC_PG_4WAG[row] <- MG[row,COLNAME]
	# Get TJC score 12WAG
	COLNAME <- paste("TJC_",BL[row]+12,"wk",sep="")
	TJC_PG_12WAG[row] <- MG[row,COLNAME]
	# Get TJC score 20WAG
	COLNAME <- paste("TJC_",BL[row]+20,"wk",sep="")
	TJC_PG_20WAG[row] <- MG[row,COLNAME]
	# Get TJC score 28WAG
	COLNAME <- paste("TJC_",BL[row]+28,"wk",sep="")
	TJC_PG_28WAG[row] <- MG[row,COLNAME]
	# Get mean TJC score after Gol treatment (including All data)
	COLNAMES <- paste("TJC_",WKS[which(WKS>BL[row])],"wk",sep="")
	TJC_PG_MNi[row] <- rowMeans(MG[row,COLNAMES])
	# Get mean TJC score after Gol treatment (excluding Censored data)
	if (length(WKS[which(WKS>BL[row] & WKS<=FN_TJC[row])])>0) {
		COLNAMES <- paste("TJC_",WKS[which(WKS>BL[row] & WKS<=FN_TJC[row])],"wk",sep="")
		if (length(COLNAMES)==1) {
			TJC_PG_MNe[row] <- MG[row,COLNAMES]
		}else{ TJC_PG_MNe[row] <- rowMeans(MG[row,COLNAMES]) }
	}else{ TJC_PG_MNe[row] <- NA }
}

# Plot Post-Gol measurements vs each other
# library(gplots)
# pairs(data.frame(TJC_PG_MNe,TJC_PG_MNi,TJC_PG_4WAG,TJC_PG_12WAG,TJC_PG_20WAG,TJC_PG_28WAG),pch="+",col=c("green","blue","red")[factor(GRP)])
TJC_PG_COR <- cor(data.frame(TJC_PG_MNe,TJC_PG_MNi,TJC_PG_4WAG,TJC_PG_12WAG,TJC_PG_20WAG,TJC_PG_28WAG), use="pairwise.complete.obs", method="spearman")
heatmap.2(TJC_PG_COR, col=colorRampPalette(c("purple","black","green"))(30), trace="none")
plot(TJC_PG_MNi,TJC_PG_MNe, main="Mean TJC after Gol Treatment - w/ or w/o Censored", xlab="Indluding",ylab="Excluding")

# TJC_PG_MNi = Mean TJC after first Golimumab Treatment (All Timepoints)
# TJC_PG_MNe = Mean TJC after first Golimumab Treatment (Excluding Censored)
# TJC_PG_4WAG = TJC score 4 weeks after first Golimumab Treatment
# TJC_PG_12WAG = TJC score 12 weeks after first Golimumab Treatment
# TJC_PG_20WAG = TJC score 20 weeks after first Golimumab Treatment
# TJC_PG_28WAG = TJC score 28 weeks after first Golimumab Treatment

######################################
## CALCULATE DELTA TJC ###############
######################################

## vs Baseline TJC (just prior to initial Gol treatment)
DEL_TJC_MNi_BL <- TJC_PG_MNi - TJC_BL
DEL_TJC_MNe_BL <- TJC_PG_MNe - TJC_BL
DEL_TJC_4_BL <- TJC_PG_4WAG - TJC_BL
DEL_TJC_12_BL <- TJC_PG_12WAG - TJC_BL
DEL_TJC_20_BL <- TJC_PG_20WAG - TJC_BL
DEL_TJC_28_BL <- TJC_PG_28WAG - TJC_BL

## vs Mean TJC prior to initial Gol treatment
DEL_TJC_MNi_MN <- TJC_PG_MNi - TJC_BL_MN
DEL_TJC_MNe_MN <- TJC_PG_MNe - TJC_BL_MN
DEL_TJC_4_MN <- TJC_PG_4WAG - TJC_BL_MN
DEL_TJC_12_MN <- TJC_PG_12WAG - TJC_BL_MN
DEL_TJC_20_MN <- TJC_PG_20WAG - TJC_BL_MN
DEL_TJC_28_MN <- TJC_PG_28WAG - TJC_BL_MN

## Compile all these Phenotypes
PG_TJC <- data.frame( TJC_PG_MNi, TJC_PG_MNe, TJC_PG_4WAG, TJC_PG_12WAG, TJC_PG_20WAG, TJC_PG_28WAG )
DEL_TJC <- data.frame( TJC_BL, TJC_BL_MN, DEL_TJC_MNi_BL, DEL_TJC_MNe_BL, DEL_TJC_4_BL, DEL_TJC_12_BL, DEL_TJC_20_BL, DEL_TJC_28_BL,
	DEL_TJC_MNi_MN, DEL_TJC_MNe_MN, DEL_TJC_4_MN, DEL_TJC_12_MN, DEL_TJC_20_MN, DEL_TJC_28_MN )
DEL_TJC_PG_COR <- cor( DEL_TJC, use="pairwise.complete.obs")
COLS <- colorRampPalette(c("firebrick1","sienna1","black","chartreuse1","steelblue1"))(50)
BRKS <- seq(-1,1,length.out=51)
heatmap.2(DEL_TJC_PG_COR, col=COLS, breaks=BRKS, trace="none")

################################################################################################
################################################################################################
## SQUARE ROOT of SWOLLEN JOINT COUNT (rTJC) ###################################################
################################################################################################
################################################################################################

######################################
## CALCULATE INITIAL rTJC ############
######################################
## Get rTJC at & before Baseline for each Sample (BASELINE=START GOLIMUMAB)
rTJC_BL <- rTJC_BL_MN <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get single baseline rTJC
	COLNAME <- paste("TJC_",BL[row],"wk",sep="")
	rTJC_BL[row] <- sqrt(MG[row,COLNAME])
	# Get mean rTJC before Gol treatment
	COLNAMES <- paste("TJC_",WKS[which(WKS<=BL[row])],"wk",sep="")
	if (length(COLNAMES)==1) { rTJC_BL_MN[row] <- sqrt(MG[row,COLNAMES]) }
	else { rTJC_BL_MN[row] <- rowMeans(sqrt(MG[row,COLNAMES])) }
}

# rTJC_BL = rTJC at week of first Golimumab Treatment
# rTJC_BL_MN = Mean rTJC before first Golimumab Treatment

######################################
## CALCULATE POST-TREATMENT rTJC #####
######################################

## Get rTJC after Golimumab Treatment
rTJC_PG_MNe <- rTJC_PG_MNi <- rTJC_PG_4WAG <- rTJC_PG_12WAG <- rTJC_PG_20WAG <- rTJC_PG_28WAG <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get rTJC score 4WAG
	COLNAME <- paste("TJC_",BL[row]+4,"wk",sep="")
	rTJC_PG_4WAG[row] <- sqrt(MG[row,COLNAME])
	# Get rTJC score 12WAG
	COLNAME <- paste("TJC_",BL[row]+12,"wk",sep="")
	rTJC_PG_12WAG[row] <- sqrt(MG[row,COLNAME])
	# Get rTJC score 20WAG
	COLNAME <- paste("TJC_",BL[row]+20,"wk",sep="")
	rTJC_PG_20WAG[row] <- sqrt(MG[row,COLNAME])
	# Get rTJC score 28WAG
	COLNAME <- paste("TJC_",BL[row]+28,"wk",sep="")
	rTJC_PG_28WAG[row] <- sqrt(MG[row,COLNAME])
	# Get mean rTJC score after Gol treatment (including All data)
	COLNAMES <- paste("TJC_",WKS[which(WKS>BL[row])],"wk",sep="")
	rTJC_PG_MNi[row] <- rowMeans(sqrt(MG[row,COLNAMES]))
	# Get mean rTJC score after Gol treatment (excluding Censored data)
	if (length(WKS[which(WKS>BL[row] & WKS<=FN_TJC[row])])>0) {
		COLNAMES <- paste("TJC_",WKS[which(WKS>BL[row] & WKS<=FN_TJC[row])],"wk",sep="")
		if (length(COLNAMES)==1) {
			rTJC_PG_MNe[row] <- sqrt(MG[row,COLNAMES])
		}else{ rTJC_PG_MNe[row] <- rowMeans(sqrt(MG[row,COLNAMES])) }
	}else{ rTJC_PG_MNe[row] <- NA }
}

# Plot Post-Gol measurements vs each other
# library(gplots)
# pairs(data.frame(rTJC_PG_MNe,rTJC_PG_MNi,rTJC_PG_4WAG,rTJC_PG_12WAG,rTJC_PG_20WAG,rTJC_PG_28WAG),pch="+",col=c("green","blue","red")[factor(GRP)])
rTJC_PG_COR <- cor(data.frame(rTJC_PG_MNe,rTJC_PG_MNi,rTJC_PG_4WAG,rTJC_PG_12WAG,rTJC_PG_20WAG,rTJC_PG_28WAG), use="pairwise.complete.obs", method="spearman")
heatmap.2(rTJC_PG_COR, col=colorRampPalette(c("purple","black","green"))(30), trace="none")
plot(rTJC_PG_MNi,rTJC_PG_MNe, main="Mean TJC after Gol Treatment - w/ or w/o Censored", xlab="Indluding",ylab="Excluding")

# rTJC_PG_MNi = Mean rTJC after first Golimumab Treatment (All Timepoints)
# rTJC_PG_MNe = Mean rTJC after first Golimumab Treatment (Excluding Censored)
# rTJC_PG_4WAG = rTJC score 4 weeks after first Golimumab Treatment
# rTJC_PG_12WAG = rTJC score 12 weeks after first Golimumab Treatment
# rTJC_PG_20WAG = rTJC score 20 weeks after first Golimumab Treatment
# rTJC_PG_28WAG = rTJC score 28 weeks after first Golimumab Treatment

######################################
## CALCULATE DELTA rTJC ##############
######################################

## vs Baseline rTJC (just prior to initial Gol treatment)
DEL_rTJC_MNi_BL <- rTJC_PG_MNi - rTJC_BL
DEL_rTJC_MNe_BL <- rTJC_PG_MNe - rTJC_BL
DEL_rTJC_4_BL <- rTJC_PG_4WAG - rTJC_BL
DEL_rTJC_12_BL <- rTJC_PG_12WAG - rTJC_BL
DEL_rTJC_20_BL <- rTJC_PG_20WAG - rTJC_BL
DEL_rTJC_28_BL <- rTJC_PG_28WAG - rTJC_BL

## vs Mean rTJC prior to initial Gol treatment
DEL_rTJC_MNi_MN <- rTJC_PG_MNi - rTJC_BL_MN
DEL_rTJC_MNe_MN <- rTJC_PG_MNe - rTJC_BL_MN
DEL_rTJC_4_MN <- rTJC_PG_4WAG - rTJC_BL_MN
DEL_rTJC_12_MN <- rTJC_PG_12WAG - rTJC_BL_MN
DEL_rTJC_20_MN <- rTJC_PG_20WAG - rTJC_BL_MN
DEL_rTJC_28_MN <- rTJC_PG_28WAG - rTJC_BL_MN

## Compile all these Phenotypes
PG_rTJC <- data.frame( rTJC_PG_MNi, rTJC_PG_MNe, rTJC_PG_4WAG, rTJC_PG_12WAG, rTJC_PG_20WAG, rTJC_PG_28WAG )
DEL_rTJC <- data.frame( rTJC_BL, rTJC_BL_MN, DEL_rTJC_MNi_BL, DEL_rTJC_MNe_BL, DEL_rTJC_4_BL, DEL_rTJC_12_BL, DEL_rTJC_20_BL, DEL_rTJC_28_BL,
	DEL_rTJC_MNi_MN, DEL_rTJC_MNe_MN, DEL_rTJC_4_MN, DEL_rTJC_12_MN, DEL_rTJC_20_MN, DEL_rTJC_28_MN )
DEL_rTJC_PG_COR <- cor( DEL_rTJC, use="pairwise.complete.obs")
COLS <- colorRampPalette(c("firebrick1","sienna1","black","chartreuse1","steelblue1"))(50)
BRKS <- seq(-1,1,length.out=51)
heatmap.2(DEL_rTJC_PG_COR, col=COLS, breaks=BRKS, trace="none")

################################################################################################
################################################################################################
## C-REACTIVE PROTEIN (CRP) ####################################################################
################################################################################################
################################################################################################

######################################
## CALCULATE INITIAL CRP #############
######################################
## Get CRP at & before Baseline for each Sample (BASELINE=START GOLIMUMAB)
CRP_BL <- CRP_BL_MN <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get single baseline CRP
	COLNAME <- paste("CRP_",BL[row],"wk",sep="")
	CRP_BL[row] <- MG[row,COLNAME]
	# Get mean CRP before Gol treatment
	COLNAMES <- paste("CRP_",WKS[which(WKS<=BL[row])],"wk",sep="")
	if (length(COLNAMES)==1) { CRP_BL_MN[row] <- MG[row,COLNAMES] }
	else { CRP_BL_MN[row] <- rowMeans(MG[row,COLNAMES]) }
}
plot( CRP_BL, CRP_BL_MN, col=c("green","blue","red")[factor(GRP)] )

# CRP_BL = CRP at week of first Golimumab Treatment
# CRP_BL_MN = Mean CRP before first Golimumab Treatment

######################################
## CALCULATE POST-TREATMENT CRP ######
######################################

## Get CRP after Golimumab Treatment
CRP_PG_MNe <- CRP_PG_MNi <- CRP_PG_4WAG <- CRP_PG_12WAG <- CRP_PG_20WAG <- CRP_PG_28WAG <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get CRP score 4WAG
	COLNAME <- paste("CRP_",BL[row]+4,"wk",sep="")
	CRP_PG_4WAG[row] <- MG[row,COLNAME]
	# Get CRP score 12WAG
	COLNAME <- paste("CRP_",BL[row]+12,"wk",sep="")
	CRP_PG_12WAG[row] <- MG[row,COLNAME]
	# Get CRP score 20WAG
	COLNAME <- paste("CRP_",BL[row]+20,"wk",sep="")
	CRP_PG_20WAG[row] <- MG[row,COLNAME]
	# Get CRP score 28WAG
	COLNAME <- paste("CRP_",BL[row]+28,"wk",sep="")
	CRP_PG_28WAG[row] <- MG[row,COLNAME]
	# Get mean CRP score after Gol treatment (including All data)
	COLNAMES <- paste("CRP_",WKS[which(WKS>BL[row])],"wk",sep="")
	CRP_PG_MNi[row] <- rowMeans(MG[row,COLNAMES])
	# Get mean CRP score after Gol treatment (excluding Censored data)
	if (length(WKS[which(WKS>BL[row] & WKS<=FN_CRP[row])])>0) {
		COLNAMES <- paste("CRP_",WKS[which(WKS>BL[row] & WKS<=FN_CRP[row])],"wk",sep="")
		if (length(COLNAMES)==1) {
			CRP_PG_MNe[row] <- MG[row,COLNAMES]
		}else{ CRP_PG_MNe[row] <- rowMeans(MG[row,COLNAMES]) }
	}else{ CRP_PG_MNe[row] <- NA }
}

# Plot Post-Gol measurements vs each other
# library(gplots)
# # pairs(data.frame(CRP_PG_MNe,CRP_PG_MNi,CRP_PG_4WAG,CRP_PG_12WAG,CRP_PG_20WAG,CRP_PG_28WAG),pch="+",col=c("green","blue","red")[factor(GRP)])
CRP_PG_COR <- cor(data.frame(CRP_PG_MNe,CRP_PG_MNi,CRP_PG_4WAG,CRP_PG_12WAG,CRP_PG_20WAG,CRP_PG_28WAG), use="pairwise.complete.obs", method="pearson")
heatmap.2(CRP_PG_COR, col=colorRampPalette(c("purple","black","green"))(30), trace="none")
plot(CRP_PG_MNi,CRP_PG_MNe, main="Mean CRP after Gol Treatment - w/ or w/o Censored", xlab="Indluding",ylab="Excluding")

# CRP_PG_MNi = Mean CRP after first Golimumab Treatment (All Timepoints)
# CRP_PG_MNe = Mean CRP after first Golimumab Treatment (Excluding Censored)
# CRP_PG_4WAG = CRP score 4 weeks after first Golimumab Treatment
# CRP_PG_12WAG = CRP score 12 weeks after first Golimumab Treatment
# CRP_PG_20WAG = CRP score 20 weeks after first Golimumab Treatment
# CRP_PG_28WAG = CRP score 28 weeks after first Golimumab Treatment

######################################
## CALCULATE DELTA CRP ###############
######################################

## vs Baseline CRP (just prior to initial Gol treatment)
DEL_CRP_MNi_BL <- CRP_PG_MNi - CRP_BL
DEL_CRP_MNe_BL <- CRP_PG_MNe - CRP_BL
DEL_CRP_4_BL <- CRP_PG_4WAG - CRP_BL
DEL_CRP_12_BL <- CRP_PG_12WAG - CRP_BL
DEL_CRP_20_BL <- CRP_PG_20WAG - CRP_BL
DEL_CRP_28_BL <- CRP_PG_28WAG - CRP_BL

## vs Mean CRP prior to initial Gol treatment
DEL_CRP_MNi_MN <- CRP_PG_MNi - CRP_BL_MN
DEL_CRP_MNe_MN <- CRP_PG_MNe - CRP_BL_MN
DEL_CRP_4_MN <- CRP_PG_4WAG - CRP_BL_MN
DEL_CRP_12_MN <- CRP_PG_12WAG - CRP_BL_MN
DEL_CRP_20_MN <- CRP_PG_20WAG - CRP_BL_MN
DEL_CRP_28_MN <- CRP_PG_28WAG - CRP_BL_MN

## Compile all these Phenotypes
PG_CRP <- data.frame( CRP_PG_MNi, CRP_PG_MNe, CRP_PG_4WAG, CRP_PG_12WAG, CRP_PG_20WAG, CRP_PG_28WAG )
DEL_CRP <- data.frame( CRP_BL, CRP_BL_MN, DEL_CRP_MNi_BL, DEL_CRP_MNe_BL, DEL_CRP_4_BL, DEL_CRP_12_BL, DEL_CRP_20_BL, DEL_CRP_28_BL,
	DEL_CRP_MNi_MN, DEL_CRP_MNe_MN, DEL_CRP_4_MN, DEL_CRP_12_MN, DEL_CRP_20_MN, DEL_CRP_28_MN )
DEL_CRP_PG_COR <- cor( DEL_CRP, use="pairwise.complete.obs")
COLS <- colorRampPalette(c("firebrick1","sienna1","black","chartreuse1","steelblue1"))(50)
BRKS <- seq(-1,1,length.out=51)
heatmap.2(DEL_CRP_PG_COR, col=COLS, breaks=BRKS, trace="none")

################################################################################################
################################################################################################
## LOG TRANSFORM of C-REACTIVE PROTEIN (CRP) ###################################################
################################################################################################
################################################################################################

######################################
## CALCULATE INITIAL CRP #############
######################################
## Get lCRP at & before Baseline for each Sample (BASELINE=START GOLIMUMAB)
lCRP_BL <- lCRP_BL_MN <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get single baseline lCRP
	COLNAME <- paste("CRP_",BL[row],"wk",sep="")
	lCRP_BL[row] <- log10(MG[row,COLNAME])
	# Get mean lCRP before Gol treatment
	COLNAMES <- paste("CRP_",WKS[which(WKS<=BL[row])],"wk",sep="")
	if (length(COLNAMES)==1) { lCRP_BL_MN[row] <- log10(MG[row,COLNAMES]) }
	else { lCRP_BL_MN[row] <- rowMeans(log10(MG[row,COLNAMES])) }
}
plot( lCRP_BL, lCRP_BL_MN, col=c("green","blue","red")[factor(GRP)] )

# lCRP_BL = lCRP at week of first Golimumab Treatment
# lCRP_BL_MN = Mean lCRP before first Golimumab Treatment

######################################
## CALCULATE POST-TREATMENT CRP ######
######################################

## Get CRP after Golimumab Treatment
lCRP_PG_MNe <- lCRP_PG_MNi <- lCRP_PG_4WAG <- lCRP_PG_12WAG <- lCRP_PG_20WAG <- lCRP_PG_28WAG <- numeric(nrow(MG))
for (row in 1:nrow(MG)) {
	# Get CRP score 4WAG
	COLNAME <- paste("CRP_",BL[row]+4,"wk",sep="")
	lCRP_PG_4WAG[row] <- log10(MG[row,COLNAME])
	# Get CRP score 12WAG
	COLNAME <- paste("CRP_",BL[row]+12,"wk",sep="")
	lCRP_PG_12WAG[row] <- log10(MG[row,COLNAME])
	# Get CRP score 20WAG
	COLNAME <- paste("CRP_",BL[row]+20,"wk",sep="")
	lCRP_PG_20WAG[row] <- log10(MG[row,COLNAME])
	# Get CRP score 28WAG
	COLNAME <- paste("CRP_",BL[row]+28,"wk",sep="")
	lCRP_PG_28WAG[row] <- log10(MG[row,COLNAME])
	# Get mean CRP score after Gol treatment (including All data)
	COLNAMES <- paste("CRP_",WKS[which(WKS>BL[row])],"wk",sep="")
	lCRP_PG_MNi[row] <- rowMeans(log10(MG[row,COLNAMES]))
	# Get mean CRP score after Gol treatment (excluding Censored data)
	if (length(WKS[which(WKS>BL[row] & WKS<=FN_CRP[row])])>0) {
		COLNAMES <- paste("CRP_",WKS[which(WKS>BL[row] & WKS<=FN_CRP[row])],"wk",sep="")
		if (length(COLNAMES)==1) {
			lCRP_PG_MNe[row] <- log10(MG[row,COLNAMES])
		}else{ lCRP_PG_MNe[row] <- rowMeans(log10(MG[row,COLNAMES])) }
	}else{ lCRP_PG_MNe[row] <- NA }
}

# Plot Post-Gol measurements vs each other
# library(gplots)
# # pairs(data.frame(lCRP_PG_MNe,lCRP_PG_MNi,lCRP_PG_4WAG,lCRP_PG_12WAG,lCRP_PG_20WAG,lCRP_PG_28WAG),pch="+",col=c("green","blue","red")[factor(GRP)])
lCRP_PG_COR <- cor(data.frame(lCRP_PG_MNe,lCRP_PG_MNi,lCRP_PG_4WAG,lCRP_PG_12WAG,lCRP_PG_20WAG,lCRP_PG_28WAG), use="pairwise.complete.obs", method="pearson")
heatmap.2(lCRP_PG_COR, col=colorRampPalette(c("purple","black","green"))(30), trace="none")
plot(lCRP_PG_MNi,lCRP_PG_MNe, main="Mean CRP after Gol Treatment - w/ or w/o Censored", xlab="Indluding",ylab="Excluding")

# lCRP_PG_MNi = Mean CRP after first Golimumab Treatment (All Timepoints)
# lCRP_PG_MNe = Mean CRP after first Golimumab Treatment (Excluding Censored)
# lCRP_PG_4WAG = CRP score 4 weeks after first Golimumab Treatment
# lCRP_PG_12WAG = CRP score 12 weeks after first Golimumab Treatment
# lCRP_PG_20WAG = CRP score 20 weeks after first Golimumab Treatment
# lCRP_PG_28WAG = CRP score 28 weeks after first Golimumab Treatment

######################################
## CALCULATE DELTA CRP ###############
######################################

## vs Baseline CRP (just prior to initial Gol treatment)
DEL_lCRP_MNi_BL <- lCRP_PG_MNi - lCRP_BL
DEL_lCRP_MNe_BL <- lCRP_PG_MNe - lCRP_BL
DEL_lCRP_4_BL <- lCRP_PG_4WAG - lCRP_BL
DEL_lCRP_12_BL <- lCRP_PG_12WAG - lCRP_BL
DEL_lCRP_20_BL <- lCRP_PG_20WAG - lCRP_BL
DEL_lCRP_28_BL <- lCRP_PG_28WAG - lCRP_BL

## vs Mean CRP prior to initial Gol treatment
DEL_lCRP_MNi_MN <- lCRP_PG_MNi - lCRP_BL_MN
DEL_lCRP_MNe_MN <- lCRP_PG_MNe - lCRP_BL_MN
DEL_lCRP_4_MN <- lCRP_PG_4WAG - lCRP_BL_MN
DEL_lCRP_12_MN <- lCRP_PG_12WAG - lCRP_BL_MN
DEL_lCRP_20_MN <- lCRP_PG_20WAG - lCRP_BL_MN
DEL_lCRP_28_MN <- lCRP_PG_28WAG - lCRP_BL_MN

## Compile all these Phenotypes
PG_lCRP <- data.frame( lCRP_PG_MNi, lCRP_PG_MNe, lCRP_PG_4WAG, lCRP_PG_12WAG, lCRP_PG_20WAG, lCRP_PG_28WAG )
DEL_lCRP <- data.frame( lCRP_BL, lCRP_BL_MN, DEL_lCRP_MNi_BL, DEL_lCRP_MNe_BL, DEL_lCRP_4_BL, DEL_lCRP_12_BL, DEL_lCRP_20_BL, DEL_lCRP_28_BL,
	DEL_lCRP_MNi_MN, DEL_lCRP_MNe_MN, DEL_lCRP_4_MN, DEL_lCRP_12_MN, DEL_lCRP_20_MN, DEL_lCRP_28_MN )
DEL_lCRP_PG_COR <- cor( DEL_lCRP, use="pairwise.complete.obs")
COLS <- colorRampPalette(c("firebrick1","sienna1","black","chartreuse1","steelblue1"))(50)
BRKS <- seq(-1,1,length.out=51)
heatmap.2(DEL_lCRP_PG_COR, col=COLS, breaks=BRKS, trace="none")

################################################################################################
################################################################################################
## EULAR RESPONSE PHENOTYPE ####################################################################
################################################################################################
################################################################################################

## Specify EULAR Response for various DAS Measures
 # NONE: DEL<0.6 | (DAS_PG>5.1 & 0.6<=DEL<=1.2)
 # GOOD: (DEL>1.2 & DAS_PG<3.2)
EUL_MNe_MN <- EUL_28_BL <- EUL_52_BL <- rep("Moderate",nrow(MG))
EUL_MNe_MN[which(-DEL_MNe_MN<0.6)] <- "None"
EUL_28_BL[which(-DEL_28_BL<0.6)] <- "None"
EUL_52_BL[which(-DEL_52_BL<0.6)] <- "None"
EUL_MNe_MN[which(-DEL_MNe_MN<=1.2 & DAS_PG_MNe>5.1)] <- "None"
EUL_28_BL[which(-DEL_28_BL<=1.2 & DAS_ADJ_28WAG>5.1)] <- "None"
EUL_52_BL[which(-DEL_52_BL<=1.2 & DAS_ADJ_52WAG>5.1)] <- "None"
EUL_MNe_MN[which(-DEL_MNe_MN>1.2 & DAS_PG_MNe<3.2)] <- "Good"
EUL_28_BL[which(-DEL_28_BL>1.2 & DAS_ADJ_28WAG<3.2)] <- "Good"
EUL_52_BL[which(-DEL_52_BL>1.2 & DAS_ADJ_52WAG<3.2)] <- "Good"

EUL_COMPILE <- data.frame( EUL_MNe_MN, EUL_52_BL, EUL_28_BL )

######################################
## WHICH INDIVS TO REMOVE ############
######################################

## For Early Dropout
LT12 <- which(IN<12) # Anybody w/ less than 12 weeks of Response data post-Gol treatment
LT8 <- which(IN<8) # Chris Huang's Recommendation
LT24 <- which(IN<28) # Anybody without new DAS at wk 28
data.frame(MG$ID[LT8],BL[LT8],IN[LT8])

## Responders to Placebo
EULAR_BL <- REM_BL <- character(nrow(MG))
for (row in 1:nrow(MG)) {
	COLNAME <- paste("DASTXT_",BL[row],"wk",sep="")
	EULAR_BL[row] <- as.character(MG[row,COLNAME])
	COLNAME <- paste("DASREM_",BL[row],"wk",sep="")
	REM_BL[row] <- as.character(MG[row,COLNAME])
}
GOOD <- which(EULAR_BL=="Good Response") # Patients with "Good" Response to Placebo
REMS <- which(REM_BL=="Y") # Remission before Golimumab - DAS<2.6

######################################
## FILTER/FORMAT TABLES ##############
######################################
# Folders: LT8, LT8_GOOD, LT8_REM
LT8_GOOD <- union(LT8,GOOD)
LT8_REM <- union(LT8,REMS)

## EUL_MNe_MN ##
LT8_EUL_MNe_MN <- data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=EUL_MNe_MN[-LT8])
LT8_EUL_MNe_MN <- LT8_EUL_MNe_MN[which(LT8_EUL_MNe_MN$Pheno!="Moderate"),]
LT8_EUL_MNe_MN$Pheno <- 3-as.numeric(as.factor(as.character(LT8_EUL_MNe_MN$Pheno)))
LT8_REM_EUL_MNe_MN <- data.frame(IID=MG$ID_2[-LT8_REM],FID=MG$ID_2[-LT8_REM],Pheno=EUL_MNe_MN[-LT8_REM])
LT8_REM_EUL_MNe_MN <- LT8_REM_EUL_MNe_MN[which(LT8_REM_EUL_MNe_MN$Pheno!="Moderate"),]
LT8_REM_EUL_MNe_MN$Pheno <- 3-as.numeric(as.factor(as.character(LT8_REM_EUL_MNe_MN$Pheno)))
LT8_GOOD_EUL_MNe_MN <- data.frame(IID=MG$ID_2[-LT8_GOOD],FID=MG$ID_2[-LT8_GOOD],Pheno=EUL_MNe_MN[-LT8_GOOD])
LT8_GOOD_EUL_MNe_MN <- LT8_GOOD_EUL_MNe_MN[which(LT8_GOOD_EUL_MNe_MN$Pheno!="Moderate"),]
LT8_GOOD_EUL_MNe_MN$Pheno <- 3-as.numeric(as.factor(as.character(LT8_GOOD_EUL_MNe_MN$Pheno)))

## EUL_28_BL ##
LT8_EUL_28_BL <- data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=EUL_28_BL[-LT8])
LT8_EUL_28_BL <- LT8_EUL_28_BL[which(LT8_EUL_28_BL$Pheno!="Moderate"),]
LT8_EUL_28_BL$Pheno <- 3-as.numeric(as.factor(as.character(LT8_EUL_28_BL$Pheno)))
LT8_REM_EUL_28_BL <- data.frame(IID=MG$ID_2[-LT8_REM],FID=MG$ID_2[-LT8_REM],Pheno=EUL_28_BL[-LT8_REM])
LT8_REM_EUL_28_BL <- LT8_REM_EUL_28_BL[which(LT8_REM_EUL_28_BL$Pheno!="Moderate"),]
LT8_REM_EUL_28_BL$Pheno <- 3-as.numeric(as.factor(as.character(LT8_REM_EUL_28_BL$Pheno)))
LT8_GOOD_EUL_28_BL <- data.frame(IID=MG$ID_2[-LT8_GOOD],FID=MG$ID_2[-LT8_GOOD],Pheno=EUL_28_BL[-LT8_GOOD])
LT8_GOOD_EUL_28_BL <- LT8_GOOD_EUL_28_BL[which(LT8_GOOD_EUL_28_BL$Pheno!="Moderate"),]
LT8_GOOD_EUL_28_BL$Pheno <- 3-as.numeric(as.factor(as.character(LT8_GOOD_EUL_28_BL$Pheno)))

## EUL_52_BL ##
LT8_EUL_52_BL <- data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=EUL_52_BL[-LT8])
LT8_EUL_52_BL <- LT8_EUL_52_BL[which(LT8_EUL_52_BL$Pheno!="Moderate"),]
LT8_EUL_52_BL$Pheno <- 3-as.numeric(as.factor(as.character(LT8_EUL_52_BL$Pheno)))
LT8_REM_EUL_52_BL <- data.frame(IID=MG$ID_2[-LT8_REM],FID=MG$ID_2[-LT8_REM],Pheno=EUL_52_BL[-LT8_REM])
LT8_REM_EUL_52_BL <- LT8_REM_EUL_52_BL[which(LT8_REM_EUL_52_BL$Pheno!="Moderate"),]
LT8_REM_EUL_52_BL$Pheno <- 3-as.numeric(as.factor(as.character(LT8_REM_EUL_52_BL$Pheno)))
LT8_GOOD_EUL_52_BL <- data.frame(IID=MG$ID_2[-LT8_GOOD],FID=MG$ID_2[-LT8_GOOD],Pheno=EUL_52_BL[-LT8_GOOD])
LT8_GOOD_EUL_52_BL <- LT8_GOOD_EUL_52_BL[which(LT8_GOOD_EUL_52_BL$Pheno!="Moderate"),]
LT8_GOOD_EUL_52_BL$Pheno <- 3-as.numeric(as.factor(as.character(LT8_GOOD_EUL_52_BL$Pheno)))

################################################################################################
################################################################################################
## FILTER AND SAVE PHENOTYPE TABLES ############################################################
################################################################################################
################################################################################################

######################################
## WHICH INDIVS TO REMOVE (RESPONSE) #
######################################

## Responders to Placebo
EULAR_BL <- REM_BL <- character(nrow(MG))
for (row in 1:nrow(MG)) {
	COLNAME <- paste("DASTXT_",BL[row],"wk",sep="")
	EULAR_BL[row] <- as.character(MG[row,COLNAME])
	COLNAME <- paste("DASREM_",BL[row],"wk",sep="")
	REM_BL[row] <- as.character(MG[row,COLNAME])
}
GOOD <- which(EULAR_BL=="Good Response") # Patients with "Good" Response to Placebo
REMS <- which(REM_BL=="Y") # Remission before Golimumab - DAS<2.6

## Union of Different Exclusion Criteria
# Folders: LT8, LT8_GOOD, LT8_REM
LT8_GOOD <- union(LT8,GOOD)
LT8_REM <- union(LT8,REMS)

######################################
## WRITE TABLES ######################
######################################

############################################################################
## Write tables filtering Chris Huang's recommendations
 # Only those who dropout before 8 weeks (post-Gol)
# DAS vs Baseline DAS 
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_MNi_BL[-LT8]),paste(PathToPheno,"LT8_DEL_MNi_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_MNe_BL[-LT8]),paste(PathToPheno,"LT8_DEL_MNe_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_4_BL[-LT8]),paste(PathToPheno,"LT8_DEL_4_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_12_BL[-LT8]),paste(PathToPheno,"LT8_DEL_12_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_20_BL[-LT8]),paste(PathToPheno,"LT8_DEL_20_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_28_BL[-LT8]),paste(PathToPheno,"LT8_DEL_28_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_52_BL[-LT8]),paste(PathToPheno,"LT8_DEL_52_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# DAS vs Mean Pre-Gol DAS
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_MNi_MN[-LT8]),paste(PathToPheno,"LT8_DEL_MNi_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_MNe_MN[-LT8]),paste(PathToPheno,"LT8_DEL_MNe_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_4_MN[-LT8]),paste(PathToPheno,"LT8_DEL_4_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_12_MN[-LT8]),paste(PathToPheno,"LT8_DEL_12_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_20_MN[-LT8]),paste(PathToPheno,"LT8_DEL_20_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_28_MN[-LT8]),paste(PathToPheno,"LT8_DEL_28_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_52_MN[-LT8]),paste(PathToPheno,"LT8_DEL_52_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# SJC vs Baseline SJC 
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_MNi_BL[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_MNi_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_MNe_BL[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_MNe_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_4_BL[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_4_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_12_BL[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_12_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_20_BL[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_20_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_28_BL[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_28_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# SJC vs Mean Pre-Gol SJC
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_MNi_MN[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_MNi_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_MNe_MN[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_MNe_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_4_MN[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_4_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_12_MN[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_12_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_20_MN[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_20_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_SJC_28_MN[-LT8]),paste(PathToPheno,"LT8_DEL_SJC_28_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# TJC vs Baseline TJC 
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_MNi_BL[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_MNi_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_MNe_BL[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_MNe_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_4_BL[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_4_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_12_BL[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_12_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_20_BL[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_20_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_28_BL[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_28_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# TJC vs Mean Pre-Gol TJC
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_MNi_MN[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_MNi_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_MNe_MN[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_MNe_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_4_MN[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_4_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_12_MN[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_12_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_20_MN[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_20_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_TJC_28_MN[-LT8]),paste(PathToPheno,"LT8_DEL_TJC_28_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# CRP vs Baseline CRP 
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_MNi_BL[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_MNi_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_MNe_BL[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_MNe_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_4_BL[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_4_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_12_BL[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_12_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_20_BL[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_20_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_28_BL[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_28_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# CRP vs Mean Pre-Gol CRP
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_MNi_MN[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_MNi_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_MNe_MN[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_MNe_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_4_MN[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_4_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_12_MN[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_12_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_20_MN[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_20_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_CRP_28_MN[-LT8]),paste(PathToPheno,"LT8_DEL_CRP_28_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# rSJC vs Baseline rSJC 
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_MNi_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_MNi_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_MNe_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_MNe_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_4_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_4_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_12_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_12_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_20_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_20_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_28_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_28_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# rSJC vs Mean Pre-Gol rSJC
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_MNi_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_MNi_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_MNe_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_MNe_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_4_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_4_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_12_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_12_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_20_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_20_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rSJC_28_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rSJC_28_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# rTJC vs Baseline rTJC 
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_MNi_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_MNi_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_MNe_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_MNe_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_4_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_4_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_12_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_12_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_20_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_20_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_28_BL[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_28_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# rTJC vs Mean Pre-Gol rTJC
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_MNi_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_MNi_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_MNe_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_MNe_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_4_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_4_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_12_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_12_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_20_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_20_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_rTJC_28_MN[-LT8]),paste(PathToPheno,"LT8_DEL_rTJC_28_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# lCRP vs Baseline lCRP 
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_MNi_BL[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_MNi_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_MNe_BL[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_MNe_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_4_BL[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_4_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_12_BL[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_12_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_20_BL[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_20_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_28_BL[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_28_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# lCRP vs Mean Pre-Gol lCRP
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_MNi_MN[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_MNi_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_MNe_MN[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_MNe_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_4_MN[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_4_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_12_MN[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_12_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_20_MN[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_20_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(data.frame(IID=MG$ID_2[-LT8],FID=MG$ID_2[-LT8],Pheno=DEL_lCRP_28_MN[-LT8]),paste(PathToPheno,"LT8_DEL_lCRP_28_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
# EULAR vs Baseline/Pre-Col DAS
write.table(LT8_EUL_MNe_MN,paste(PathToPheno,"LT8_EUL_MNe_MN.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(LT8_EUL_28_BL,paste(PathToPheno,"LT8_EUL_28_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)
write.table(LT8_EUL_52_BL,paste(PathToPheno,"LT8_EUL_52_BL.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)

################################################################################################
################################################################################################
## WRITE "FULL_TABLE" WITH ALL PHENOS ##########################################################
################################################################################################
################################################################################################

## Throw Additional Pheno Columns onto Table
RAND_COMPILE <- data.frame( DAS_BL, DAS_BL_MN, EULAR_BL, REM_BL, GRP, IN )
PG_COMPILE <- data.frame( PG_DAS, PG_SJC, PG_TJC, PG_CRP, PG_rSJC, PG_rTJC, PG_lCRP )
DEL_COMPILE <- data.frame( DEL_DAS, DEL_SJC, DEL_TJC, DEL_CRP, DEL_rSJC, DEL_rTJC, DEL_lCRP )
# Full_Table.old <- data.frame( MG.610, DEL_SJC, DEL_TJC, DEL_CRP, DEL_lCRP) 
# Full_Table.old.2 <- data.frame( MG.610, PG_COMPILE, DEL_COMPILE )
Full_Table <- data.frame( MG, RAND_COMPILE, EIGEN, PG_COMPILE, DEL_COMPILE, EUL_COMPILE )
# RM.COLS <- which( colnames(Full_Table) %in% c("SJC_BL.1","SJC_BL_MN.1","TJC_BL.1","TJC_BL_MN.1","CRP_BL.1","CRP_BL_MN.1") )
# Full_Table <- Full_Table[, -RM.COLS]
## Just for kicks, run correlation of each phenotype (and PCs)
# PC_COLS.FT <- grep("PC",colnames(Full_Table) ) # 335:354
# INIT_COLS.FT <- which( names(Full_Table) %in% names(Full_Table.old)[c(356:367,433,444)] )
# DAS_COLS.FT <- which( names(Full_Table) %in% names(Full_Table.old)[c(379:392)] ) # 379:392
# PHE_COLS.FT <- which( names(Full_Table) %in% names(Full_Table.old)[c(356:367,433,444)] ) # c(397:432,435:444)
# COLS.FT <- c( PC_COLS.FT, INIT_COLS.FT, DAS_COLS.FT, PHE_COLS.FT )
# COR_FULL.tab <- Full_Table[, COLS.FT ]
# COR_FULL <- cor ( COR_FULL.tab, method="spearman", use="pairwise.complete.obs" )
# heatmap.2(COR_FULL, col=COLS, breaks=BRKS, trace="none" )
# heatmap.2(COR_FULL, col=COLS, breaks=BRKS, Colv=F, Rowv=F, dendrogram="none", trace="none" )

## Write Table
write.table(Full_Table,paste(PathToData,"/Full_Tables/",DATE,"_Full_Table.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)


################################################################################################
################################################################################################
## COVARIATE FILES #############################################################################
################################################################################################
################################################################################################

## Write tables of pre-Gol response
 # EULAR_BL # EULAR response to Placebo at time of first Gol treatment
 # REM_BL # In clinical remission at time of first Gol treatment?
wREM_BL <- as.numeric(as.factor(REM_BL))
 # DAS_BL # DAS Score immediately before first Gol treatment
 # DAS_BL_MN # Mean DAS Score prior to Gol treatment
 # ARM # To which Arm of the study do they belong?
ARM <- as.numeric(GRP=="G")+1
ESC <- as.numeric(as.factor(EE))

################################################
## Parse Through Phenotypes ####################

## Make Output File, then reformat columns to appropriate Covariate File formats ##
OUT <- MG[,c("ID_2","ID_2","AGE","SEX","HT","WT","BMI","DIS_DUR","ANAT_ST","FN_CLS","SURG","JNT_S","JNT_T","RF","ACPA","RF_ACPA","CRP_LVL","CRP_SCRN","CRP_0","DAS_0","SE_0","SJ_0","SVDH_0")]

# Fix "SEX" by making it binary (1,2) variable
SEX <- -as.numeric(as.factor(OUT$SEX))+3

# Fix "ANAT_ST" by separating levels into different columns
 # STATE I, II, III, IV & UNKNOWN
ANAT_I <- as.numeric(OUT$ANAT_ST=="STAGE I")+1
ANAT_II <- as.numeric(OUT$ANAT_ST=="STAGE II")+1
ANAT_III <- as.numeric(OUT$ANAT_ST=="STAGE III")+1
ANAT_IV <- as.numeric(OUT$ANAT_ST=="STAGE IV")+1
ANAT <- data.frame(ANAT_I,ANAT_II,ANAT_III,ANAT_IV)

# Fix "FN_CLS" by separating levels into different columns
 # CLASS I, II , III
FN_I <- as.numeric(OUT$ANAT_ST=="CLASS I")+1
FN_II <- as.numeric(OUT$ANAT_ST=="CLASS II")+1
FN <- data.frame(FN_I,FN_II)

# Fix "SURG" by making it binary variable
SURG <- as.numeric(as.factor(OUT$SURG))

# Fix "RF" - binary
RF <- as.numeric(as.factor(OUT$RF))

# Fix "ACPA" - binary
ACPA_P <- as.numeric(OUT$ACPA=="Positive")+1
ACPA_N <- as.numeric(OUT$ACPA=="Negative")+1
ACPA <- data.frame(ACPA_P,ACPA_N)

# Fix "RF" - binary
RF_ACPA <- as.numeric(as.factor(OUT$RF_ACPA))

# Fix "CRP_LVL" - binary
CRP_LVL <- as.numeric(as.factor(OUT$CRP_LVL))

OUT2 <- data.frame(MG[,c("ID_2","ID_2","AGE","HT","WT","BMI","DIS_DUR","JNT_S","JNT_T","CRP_SCRN","CRP_0","DAS_0","SE_0","SJ_0","SVDH_0")],SEX,ANAT,FN,SURG,RF,ACPA,RF_ACPA,CRP_LVL,DAS_BL,DAS_BL_MN,REM_BL=wREM_BL,DRUG=ARM,EE=ESC)
colnames(OUT2)[1:2] <- c("IID","FID")

## Add a few more "Initial" values for covariates
OUT3 <- data.frame( OUT2, lCRP_BL, lCRP_BL_MN, rSJC_BL, rSJC_BL_MN, rTJC_BL, rTJC_BL_MN )

## Write Table
write.table(OUT3,paste(PathToPheno,"COV.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)

################################################
## Make Ancestral Output File ##################
 # Not really to be used as Covariate, but for reference
OUT_ANC <- MG[,c("ID_2","ID_2","RACE","ETHN","COUN","JNJ_ANC","europe","africa","amerind","eastAsian","oceania","centralAsia")]
colnames(OUT_ANC)[1:2] <- c("IID","FID")
write.table(OUT_ANC,paste(PathToPheno,"COV_ANC.txt",sep=""),sep="\t",row.names=F,col.names=T,quote=F)






################################################################################################
################################################################################################

################################################################################################
################################################################################################
## END OF DOC ##################################################################################
################################################################################################
################################################################################################

COLUMNS <- c( 275:277, 284:295 ) #, 401:409 )
COLUMNS <- COLUMNS[ c( seq(1,length(COLUMNS),3),seq(2,length(COLUMNS),3),seq(3,length(COLUMNS),3) ) ]
 # Hist
png( paste(PathToSave,"Structural_Hists.png",sep="/"), width=2000,height=1500, pointsize=24 )
par(mfrow=c(3,length(COLUMNS)/3))
COLORS <- rep( c( "cadetblue1","tomato1","mediumpurple1" ), rep(length(COLUMNS)/3,3) )
for ( c in 1:length(COLUMNS) ) { 
	col <- COLUMNS[c]
	hist( as.numeric(RAD[,col]), main=colnames(RAD)[col], col=COLORS[c], xlab=colnames(RAD)[col] )
}
dev.off()
 # Heatmap
png( paste(PathToSave,"Structural_Heat.png",sep="/"), width=2000,height=2000, pointsize=34 )
COLS.list <- c("black","slateblue3","steelblue2","springgreen2","gold2","chocolate2","firebrick2")
COLS <- colorRampPalette(COLS.list)(100)
CORR <- cor( data.matrix(RAD[,COLUMNS]), use="pairwise.complete.obs", method="spearman" )
heatmap.2( CORR, scale="none",trace="none", main="Correlation b/n Structural Phenotypes", col=COLS, margins=c(8,8) )
dev.off()
 # Scatter
png( paste(PathToSave,"Structural_Scatter.png",sep="/"), width=2000,height=1000, pointsize=24 )
par(mfrow=c(2,length(COLUMNS)/3))
COLORS <- rep( c( "cadetblue1","tomato1","mediumpurple1" ), rep(length(COLUMNS)/3,3) )
for ( c in (length(COLUMNS)/3+1):length(COLUMNS) ) { 
	col <- COLUMNS[c]
	col.0 <- gsub( "24","0", colnames(RAD)[col] )
	col.0 <- gsub( "52","0", col.0 )
	plot( as.numeric(RAD[,col]) ~ as.numeric(RAD[,col.0]), main=colnames(RAD)[col], col=COLORS[c], xlab=col.0, ylab=colnames(RAD)[col] )
	abline( 0,1, col="black" )
}
dev.off()

################################################################################################
################################################################################################
