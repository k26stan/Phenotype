## Run LMM Models on Time-Series Data ##
## Janssen Data using Multiple-Measures ##
## July 29, 2014 ##
## Kristopher Standish ##

library(nlme)
library(gplots)

##############################################################
## LOAD DATA #################################################
##############################################################

## Set Date
DATE <- gsub("-","",Sys.Date())

## Mac Paths
PathToRawFiles <- "/Users/kstandis/Data/Burn/Data/Phenos/Raw_Files/"
PathToData <- "/Users/kstandis/Data/Burn/Data/Phenos/Time_Series/20150530_Resp_v_Time.txt"
PathToFT <- "/Users/kstandis/Data/Burn/Data/Phenos/Full_Tables/20150520_Full_Table.txt"
PathToWAG <- "/Users/kstandis/Data/Burn/Data/Phenos/Full_Tables/20150520_Single_Pheno_Table.txt"
PathToDER <- "/Users/kstandis/Data/Burn/Data/Phenos/Full_Tables/20150619_Derived_Pheno_Table.txt"
# PathToPlot <- paste("/Users/kstandis/Data/Burn/Plots/",DATE,"_LME_ModSel/",sep="" )
# dir.create( PathToPlot )

## Previously Compiled Data
TAB.l <- read.table( PathToData, sep="\t",header=T, stringsAsFactors=F )
FUL <- read.table( PathToFT,sep="\t",header=T)
WAG <- read.table( PathToWAG,sep="\t",header=T )
DER <- read.table( PathToDER,sep="\t",header=T )

## Get unique Weeks for plotting purposes
WKS <- unique( TAB.l$WK )

##############################################################
## FILTER DATA ###############################################
##############################################################

## Take out Patients who left before getting DRUG
RM.exit.id <- as.character( FUL$ID_2[which(FUL$IN<=4)] )
RM.exit <- which( TAB.l$IID %in% RM.exit.id )
TAB <- TAB.l[-RM.exit,c(1:15,17)]

## Remove DAS values that are NA
RM.na <- which(is.na( TAB$DAS ))
if (length(RM.na) > 0 ) { TAB <- TAB[-RM.na, ] }

## Add PC's onto Table for later use
TAB.PC <- merge( TAB, FUL[,c("ID_2",paste("PC",1:3,sep=""))], by.x="IID",by.y="ID_2")

##############################################################
## COMPILE WAG DATA FOR PCA ##################################
##############################################################

## Compile & Standardize Delta Values for each Phenotype
WAG.lCRP <- matrix( unlist(WAG[ , intersect( grep("^DEL_WAG",colnames(WAG)), grep("lCRP",colnames(WAG)) ) ]), nrow=nrow(WAG), byrow=F )
WAG.lCRP <- ( WAG.lCRP - mean(WAG.lCRP,na.rm=T) ) / sd(WAG.lCRP,na.rm=T)
WAG.rSJC <- matrix( unlist(WAG[ , intersect( grep("^DEL_WAG",colnames(WAG)), grep("rSJC",colnames(WAG)) ) ]), nrow=nrow(WAG), byrow=F )
WAG.rSJC <- ( WAG.rSJC - mean(WAG.rSJC,na.rm=T) ) / sd(WAG.rSJC,na.rm=T)
WAG.rTJC <- matrix( unlist(WAG[ , intersect( grep("^DEL_WAG",colnames(WAG)), grep("rTJC",colnames(WAG)) ) ]), nrow=nrow(WAG), byrow=F )
WAG.rTJC <- ( WAG.rTJC - mean(WAG.rTJC,na.rm=T) ) / sd(WAG.rTJC,na.rm=T)

## Compile into single Matrix
WAG.2 <- rbind( t(WAG.lCRP), t(WAG.rSJC), t(WAG.rTJC) )

## Remove Patients w/ All Missing Data
WAG.2 <- WAG.2[ , -which(unlist(apply( WAG.2, 2, function(x) all(is.na(x)) ))) ]

## Impute Missing Data
WAG.mn.col <- colMeans( WAG.2, na.rm=T )
WAG.mn.row <- rowMeans( WAG.2, na.rm=T )

WHICH.NA <- which(is.na(WAG.2),arr.ind=T)
WAG.2[WHICH.NA] <- apply( WHICH.NA, 1, function(x) mean( WAG.mn.row[x[1]], WAG.mn.col[x[2]]) )

TEST <- prcomp( WAG.2 )
PCS <- TEST$rotation

barplot( t(TEST$x[,1:4]), col=1:4, beside=T )

PCS.bin <- apply( PCS, 2, function(x) as.numeric(cut( x, quantile( x, c(0,.25,.75,1) ),include.lowest=T )) )








