## First Look at Vectra Molecular DAS Data ##
## May 28, 2015 ##
## Kristopher Standish ##

## Get a feel for the data and what I'm dealing with ##

#############################################################
## LOAD DATA ################################################
#############################################################
library(xlsx)

## Set Date
DATE <- "20150528"

## Set Paths to Data Sets & Save Locations (TSCC)
PathToVectra <- "/Users/kstandis/Data/Burn/Data/Phenos/Raw_Files/20150526_Vectra_DA_ART3001.csv"
PathToFT <- "/Users/kstandis/Data/Burn/Data/Phenos/Full_Tables/20150520_Full_Table.txt"
PathToPlot <- paste("/Users/kstandis/Data/Burn/Plots/",DATE,"_Vectra/",sep="")

## Load Data
VEC.l <- read.table( PathToVectra, sep=",",header=T) ; VEC <- VEC.l
FT <- read.table( PathToFT, sep="\t",header=T)

#############################################################
## ORGANIZE DATA ############################################
#############################################################

## Pull out Relevant Columns
 # Demographic/Identifier Columns
DEMO_COLS <- 1:19
 # Score Column
SCOR_COL <- grep("Score",colnames(VEC))
SCOR <- VEC[,SCOR_COL]
 # Molecular Results Columns
RES_COLS <- grep("Result",colnames(VEC))
MOL <- VEC[,RES_COLS]
 # DAS/CRP/ACR Columns
DAS_COLS <- grep("DASEEwk",colnames(VEC)) # which( colnames(VEC) %in% paste("DASEEwk",c(0,2,4,14),sep="") )
CRP_COLS <- grep("CRPwk",colnames(VEC)) # which( colnames(VEC) %in% paste("CRPwk",c(0,2,4,14),sep="") )
ACR_COLS <- grep("ACR",colnames(VEC))
 # Week Column
WKS.1 <- gsub( "WK ","",VEC$Time_Text )
WKS <- as.numeric(gsub(" BIOMARKER","",WKS.1))

## Convert VEC data to tall table
 # Pull Demo & Vectra Columns
VEC.2 <- data.frame( VEC[,DEMO_COLS],WKS,VEC[,c(SCOR_COL,RES_COLS)] )# VEC[ , c(DEMO_COLS,SCOR_COL,RES_COLS) ]
VEC.2 <- data.frame( TAG=paste(VEC.2$SubjectID,VEC.2$WKS,sep="_"), VEC.2 )
 # Reformat other Data
WHICH <- which(!duplicated( VEC[,"SubjectID"] ))
VEC.3a <- data.frame( ID=VEC[WHICH,"SubjectID"], WKS=0, DAS=VEC[WHICH,DAS_COLS[1]], CRP=VEC[WHICH,CRP_COLS[1]])
VEC.3b <- data.frame( ID=VEC[WHICH,"SubjectID"], WKS=2, DAS=VEC[WHICH,DAS_COLS[2]], CRP=VEC[WHICH,CRP_COLS[1]])
VEC.3c <- data.frame( ID=VEC[WHICH,"SubjectID"], WKS=4, DAS=VEC[WHICH,DAS_COLS[5]], CRP=VEC[WHICH,CRP_COLS[5]])
VEC.3d <- data.frame( ID=VEC[WHICH,"SubjectID"], WKS=14, DAS=VEC[WHICH,DAS_COLS[3]], CRP=VEC[WHICH,CRP_COLS[3]])
VEC.3e <- data.frame( ID=VEC[WHICH,"SubjectID"], WKS=24, DAS=VEC[WHICH,DAS_COLS[4]], CRP=VEC[WHICH,CRP_COLS[4]])
VEC.3 <- rbind( VEC.3a, VEC.3b, VEC.3c, VEC.3d, VEC.3e )
VEC.3 <- data.frame( TAG=paste(VEC.3$ID,VEC.3$WKS,sep="_"), VEC.3 )
 # Merge Vectra & DAS Values
VEC.4 <- merge( VEC.2, VEC.3, by="TAG" )
dim(VEC.4)
 # Re-order Columns
VEC.4 <- VEC.4[order(VEC.4$WKS.x),] ; VEC.4 <- VEC.4[order(VEC.4$ID),] ; 

pairs( data.frame(VEC.4$Score,VEC.4$DAS,log10(VEC.4$CRP)) )

## Compare to FT
length(unique(VEC$SubjectID))
length(unique(VEC$Custom_ID))
length(unique(VEC$Sample_Collection_Number))
length(which( as.character(VEC$Sample_Collection_Number) %in% as.character(FT$ID) ))

#############################################################
## CHECK OUT VECTRA RESULTS #################################
#############################################################

## Distribution of Score
hist(SCOR, main=names(VEC)[SCOR_COL],breaks=seq(0,100,5) )
polygon( c(0,0,30,30),c(0,100,100,0), density=20,col="springgreen2" )
polygon( c(30,30,45,45),c(0,100,100,0), density=20,col="gold1" )
polygon( c(45,45,100,100),c(0,100,100,0), density=20,col="firebrick2" )
hist(SCOR, main=names(VEC)[SCOR_COL],breaks=seq(0,100,5),add=T,col="white" )
abline( v=c(30,45),col=c("gold1","red"),lty=2,lwd=2)

## Check out Distributions of Molecular Markers
par(mfrow=c(3,4))
for ( i in 1:length(RES_COLS) ) {
	col <- RES_COLS[i]
	hist( VEC[,col], main=names(VEC)[col] )
}

## Transform Molecular Data
MOL.t <- log10( VEC[,RES_COLS] )
par(mfrow=c(3,4))
for ( i in 1:ncol(MOL.t) ) {
	hist( MOL.t[,i], main=names(MOL.t)[i] )
}

## Pairs w/ Score & Molecular Results
pairs( VEC[,c(SCOR_COL,RES_COLS)] )
 # Transformed
pairs( data.frame( SCOR, MOL.t ) )






















#############################################################
## END OF DOC ###############################################
#############################################################