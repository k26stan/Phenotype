## Make Time-Course Association Table ##
## Janssen Data using Multiple-Measures ##
## July 29, 2014 ##
## Updated February 26, 2015 ##
## Kristopher Standish ##

######################################
## LOAD DATA #########################
######################################

## Set Date
DATE <- "20150226"

# ## TSCC Paths
# PathToData <- "/projects/janssen/clinical/"
# PathToSave <- "/projects/janssen/clinical/Plots/20140522/"

## Mac Paths
PathToRawFiles <- "/Users/kstandis/Data/Burn/Data/Phenos/Raw_Files/"
PathToPlot <- paste("/Users/kstandis/Data/Burn/Plots/",DATE,sep="" )
PathToTableOut <- "/Users/kstandis/Data/Burn/Data/Phenos/"

## Previously Compiled Data
DR <- read.table(paste(PathToRawFiles,"DRSA436EE.txt",sep=""),sep="\t",header=T)
FT <- read.table(paste(PathToRawFiles,"20140507_FULL_RESP.csv",sep=""),sep=",",header=T)
NEW <- read.table(paste(PathToRawFiles,"DAS_Observed_wk20_wk24.csv",sep=""),sep=",",header=T)

## Load Genotypes Files
# GT <- read.table(INSERT_GENO_FILE_HERE,...,...)
# SNP_COUNT <- 5
# GT <- array(sample(c(0,1,2),SNP_COUNT*nrow(MG),prob=c(.25,.5,.25),replace=T), dim=c(nrow(MG),SNP_COUNT))
# rownames(GT) <- MG$ID_2 ; colnames(GT) <- paste("SNP",1:SNP_COUNT,sep="_")

######################################
## INCLUDE NEW DAS DATA (WK20,24) ####
######################################

## Merge Tables
MG.a <- merge(x=DR,y=FT,by.x="ID_2",by.y="Custom_ID")

## Convert 3 new DAS columns to "numeric"
for ( col in 2:4 ) {
	NEW[,col] <- as.numeric(as.character( NEW[,col] ))
}
 # Check correlation w/ previous DAS values
# par(mfrow=c(1,3))
# plot( NEW[,c(2,54)] )
# plot( NEW[,c(3,55)] )
# plot( NEW[,c(4,56)] )
# pairs( NEW[,c(2:4,54:56)] )
cor( NEW[,c(2:4,54:56)], use="pairwise.complete.obs", method="pearson" )

## Pull out New Values into minimal data frame
NEW.min <- data.frame( NEW[,1:4] )
NEW.min[ which(is.na(NEW.min),arr.ind=T) ] <- 99

## Merge NEW minimal data frame w/ MG & Filter
MG.b <- merge( x=MG.a, y=NEW.min, by.x="ID_2", by.y="Custom_ID" )
MG.c <- MG.b
MG.c[,"DAS_16wk"] <- MG.c[,"DAS_16wk_ob"]
MG.c[,"DAS_20wk"] <- MG.c[,"DAS_20wk_ob"]
MG.c[,"DAS_24wk"] <- MG.c[,"DAS_24wk_ob"]

## Set new MG data.frame
MG <- MG.c[ , 1:(ncol(MG.c)-3) ]

######################################
## SET UP SOME VARIABLES #############
######################################

## Specify Column Numbers for Pertinent Columns in MG
DAS_COLS <- 115:130 #41:56+74 # DAS Scores in MG Table
CRP_COLS <- 83:98 # CRP Levels in MG Table
SJC_COLS <- 206:221 # Swollen Joint Count in MG Table
TJC_COLS <- 222:237 # Tender Joint Count in MG Table
SJC28_COLS <- 238:253 # Swollen Joint Count (out of 28) in MG Table
TJC28_COLS <- 254:269 # Tender Joint Count (out of 28) in MG Table

## Specify Weeks for each Column and Such
WKS <- c(0,2,4,8,12,14,16,20,24,28,36,44,52,68,84,100)
# COL_WK <- c(rep(NA,8),rep(WKS,3),rep(NA,5),WKS,WKS[1:10],rep(NA,3),WKS[11:13],rep(NA,3),WKS[14:16],rep(WKS,10) )
# MG_WK <- c(rep(NA,74),COL_WK)

## Specify Arms/Groups: Gol, Plac, PlacEE
G <- which(MG$Drug=="Golimumab")
P <- which(MG$Drug=="Placebo" & MG$EE=="N")
PE <- which(MG$Drug=="Placebo" & MG$EE=="Y")
GRP <- rep("G",nrow(MG)) ; GRP[P] <- "P" ; GRP[PE] <- "PE"

## Specify Baseline Week for each Group
BL <- rep(0,nrow(MG))
BL[P] <- 24
BL[PE] <- 16

######################################
## DROP-OUTS #########################
######################################

## Identify week that each person dropped out of study
DP <- array(0,dim=c(nrow(MG),4)) ; colnames(DP) <- c("DAS","CRP","SJ","TJ") ; rownames(DP) <- MG$ID
for (row in 1:nrow(MG)) {
	if ( length(which(duplicated(c(MG[row,DAS_COLS]))))!=(length(DAS_COLS)-1) ) {
		DP[row,1] <- WKS[tail(which(MG[row,DAS_COLS]!=MG[row,DAS_COLS[length(DAS_COLS)]]),1)+1]
	}
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
## SET UP FOR TIME-SERIES ANALYSIS ###
######################################
attach(MG)

## Make Array for Time-Series Analysis
REP <- sort(rep(1:nrow(MG),16))
TAB <- data.frame(IID=ID_2[REP], FID=ID_2[REP], WK=rep(WKS,nrow(MG)), SEX=SEX[REP], AGE=AGE[REP], HT=HT[REP], WT=WT[REP], BMI=BMI[REP], DIS_DUR=DIS_DUR[REP], RF_ACPA=RF_ACPA[REP], ACPA=ACPA[REP], RF=RF[REP])

## Include Treatment for each person for each week
ARM <- Drug[REP]
ESC <- EE[REP]
 # Drug
DRUG <- rep(1,nrow(TAB))
DRUG[which( TAB$WK==0 )] <- 0
DRUG[which( ARM=="Placebo" & ESC=="N" & TAB$WK<=24 )] <- 0
DRUG[which( ARM=="Placebo" & ESC=="Y" & TAB$WK<=16 )] <- 0
 # Placebo
PLAC <- rep(0,nrow(TAB))
PLAC[which( ARM=="Placebo" & ESC=="N" & TAB$WK<=24 )] <- 1
PLAC[which( ARM=="Placebo" & ESC=="Y" & TAB$WK<=16 )] <- 1
PLAC[which( TAB$WK==0 )] <- 0
head(data.frame( DRUG, PLAC ), 50 )
TAB <- data.frame(TAB,DRUG,PLAC)

## Put Response Metrics in the Table
# CRP (CRP_COLS <- 83:98)
CRP_VEC <- c(t(as.matrix(MG[,CRP_COLS])))
# DAS
DAS_VEC <- c(t(as.matrix(MG[,DAS_COLS])))
# SJC
SJC_VEC <- c(t(as.matrix(MG[,SJC_COLS])))
# TJC
TJC_VEC <- c(t(as.matrix(MG[,TJC_COLS])))
# SJC28
SJC28_VEC <- c(t(as.matrix(MG[,SJC28_COLS])))
# TJC28
TJC28_VEC <- c(t(as.matrix(MG[,TJC28_COLS])))

TAB <- data.frame(TAB, CRP=CRP_VEC, DAS=DAS_VEC, SJC=SJC_VEC, TJC=TJC_VEC, SJC28=SJC28_VEC, TJC28=TJC28_VEC)
detach(MG)

## Remove Timepoints after Patient Dropped Out
TAB.2 <- array( ,c(0,ncol(TAB)) )
colnames(TAB.2) <- colnames(TAB)
for ( i in 1:length(IN) ) {
	samp <- names(IN)[i]
	TEMP_TAB <- TAB[ grep(samp,TAB[,"IID"]), ]
	final_wk <- IN[i]
	TEMP_TAB <- TEMP_TAB[ which(TEMP_TAB[,"WK"]<=final_wk), ]
	TAB.2 <- rbind(TAB.2,TEMP_TAB)
	# which_rm <- intersect( grep(samp,TAB[,"IID"]), which(TAB[,"WK"]>final_wk) )
	# if ( length(which_rm)>0 ) { TAB <- TAB[ -which_rm, ] }
}

## Remove the Missing DAS values from NEW table
TAB.3 <- TAB.2[ -which(TAB.2[,"DAS"]==99), ]

# ## Add in Genotype Data
# TAB <- data.frame(TAB, GT[REP,])

######################################
## WRITE OUTPUT TABLE ################
######################################

## Write output table
write.table(TAB.3, paste(PathToTableOut,"/Time_Series/",DATE,"_Resp_v_Time.txt",sep=""),sep="\t",col.names=T,row.names=F,quote=F)








