
library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(shinyWidgets)
library(igraph)
library(RColorBrewer)
library(networkD3)
library(htmlwidgets)
library(visNetwork)
library(shinyalert)
library(shinycssloaders)
library(shinycustomloader)
library(sqldf)
library(readr)
library(ggplot2)
library(formattable)
#library(viridis)
library(shinyBS)
library(purrr)
library(bslib)
library(spsComps)
library(bsplus)
library(reshape)
library(data.table)
library(readxl)
library(heatmaply)
library(plotly)



infoBtn <- function(id) {
    actionButton(id,
                 label = "",
                 icon = icon("question"),
                 style="color: #fff; background-color: #074487; border-color: #074487; border-radius: 4px;
                 height:17px; width:17px;padding:0px 0.5px 0.5px 0.5px;
                 font-size:70%"
                 #size = "small",

                 
    )
}




cancerNames <- c("ACC","BLCA","BRCA","CESC","CHOL","COAD","DLBC","ESCA","HNSC","KICH","KIRC","KIRP",
                 "LGG","LIHC","LUAD","LUSC","MESO","OV","PAAD","PCPG","PRAD","READ","SARC","SKCM",
                 "STAD","TGCT","THCA","THYM","UCEC","UCS","UVM")


####################################################################################################
ACC_allData <- read_csv("finalDTData/ACC.csv",show_col_types = FALSE)
ACC_filtered <- ACC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]
ACCWBenjaminiHochbergCorrection <- read_csv("BenjaminiHochberg/ACCWBenjaminiHochbergCorrection.csv")
ACC <- sqldf::sqldf("SELECT ACC_filtered.*, BH_rejected, BH_pvalues_adjusted from ACC_filtered LEFT JOIN ACCWBenjaminiHochbergCorrection ON 
                          (ACC_filtered.entrezgene_id = ACCWBenjaminiHochbergCorrection.mrna AND ACC_filtered.mirna1 = ACCWBenjaminiHochbergCorrection.mirna1 AND ACC_filtered.mirna2 = ACCWBenjaminiHochbergCorrection.mirna2) 
                          OR (ACC_filtered.entrezgene_id = ACCWBenjaminiHochbergCorrection.mrna AND ACC_filtered.mirna1 = ACCWBenjaminiHochbergCorrection.mirna2 AND ACC_filtered.mirna2 = ACCWBenjaminiHochbergCorrection.mirna1)")


BLCA_allData <- read_csv("finalDTData/BLCA.csv",show_col_types = FALSE)
BLCA <- BLCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

# BRCA_allData <- read_csv("finalDTData/BRCA.csv")
# BRCA <- BRCA_allData[,c()]

CESC_allData <- read_csv("finalDTData/CESC.csv",show_col_types = FALSE)
CESC <- CESC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

CHOL_allData <- read_csv("finalDTData/CHOL.csv",show_col_types = FALSE)
CHOL <- CHOL_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

COAD_allData <- read_csv("finalDTData/COAD.csv",show_col_types = FALSE)
COAD <- COAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC")]

DLBC_allData <- read_csv("finalDTData/DLBC.csv",show_col_types = FALSE)
DLBC <- DLBC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

ESCA_allData <- read_csv("finalDTData/ESCA.csv",show_col_types = FALSE)
ESCA <- ESCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

HNSC_allData <- read_csv("finalDTData/HNSC.csv",show_col_types = FALSE)
HNSC <- HNSC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

KICH_allData <- read_csv("finalDTData/KICH.csv",show_col_types = FALSE)
KICH <- KICH_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

KIRC_allData <- read_csv("finalDTData/KIRC.csv",show_col_types = FALSE)
KIRC <- KIRC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

KIRP_allData <- read_csv("finalDTData/KIRP.csv",show_col_types = FALSE)
KIRP <- KIRP_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

LGG_allData <- read_csv("finalDTData/LGG.csv",show_col_types = FALSE)
LGG <- LGG_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

LIHC_allData <- read_csv("finalDTData/LIHC.csv",show_col_types = FALSE)
LIHC <- LIHC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

LUAD_allData <- read_csv("finalDTData/LUAD.csv",show_col_types = FALSE)
LUAD <- LUAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

LUSC_allData <- read_csv("finalDTData/LUSC.csv",show_col_types = FALSE)
LUSC <- LUSC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

MESO_allData <- read_csv("finalDTData/MESO.csv",show_col_types = FALSE)
MESO <- MESO_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

OV_allData <- read_csv("finalDTData/OV.csv",show_col_types = FALSE)
OV <- OV_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

PAAD_allData <- read_csv("finalDTData/PAAD.csv",show_col_types = FALSE)
PAAD <- PAAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

PCPG_allData <- read_csv("finalDTData/PCPG.csv",show_col_types = FALSE)
PCPG <- PCPG_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

PRAD_allData <- read_csv("finalDTData/PRAD.csv",show_col_types = FALSE)
PRAD <- PRAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

READ_allData <- read_csv("finalDTData/READ.csv",show_col_types = FALSE)
READ <- READ_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC")]

SARC_allData <- read_csv("finalDTData/SARC.csv",show_col_types = FALSE)
SARC <- SARC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","mRNA_pvalue","mRNA_logFC")]

SKCM_allData <- read_csv("finalDTData/SKCM.csv",show_col_types = FALSE)
SKCM <- SKCM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

STAD_allData <- read_csv("finalDTData/STAD.csv",show_col_types = FALSE)
STAD <- STAD_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

TGCT_allData <- read_csv("finalDTData/TGCT.csv",show_col_types = FALSE)
TGCT <- TGCT_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

THCA_allData <- read_csv("finalDTData/THCA.csv",show_col_types = FALSE)
THCA <- THCA_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

THYM_allData <- read_csv("finalDTData/THYM.csv",show_col_types = FALSE)
THYM <- THYM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

UCEC_allData <- read_csv("finalDTData/UCEC.csv",show_col_types = FALSE)
UCEC <- UCEC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB","miRNA1_pvalue","miRNA1_logFC","miRNA2_pvalue","miRNA2_logFC","mRNA_pvalue","mRNA_logFC")]

UCS_allData <- read_csv("finalDTData/UCS.csv",show_col_types = FALSE)
UCS <- UCS_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

UVM_allData <- read_csv("finalDTData/UVM.csv",show_col_types = FALSE)
UVM <- UVM_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

####################################################################################################

concated <- data.frame(source=c(),target=c())

ACC_source_target <- read.table("networkData/ACC_source_target.csv", header = TRUE, sep = ";")
ACC_node_attr <- read.table("networkData/ACC_generalNetwork_default_node.csv", header = TRUE, sep = ",")

BLCA_source_target <- read.table("networkData/BLCA_source_target_new.csv", header=TRUE, sep = ";" )
BLCA_node_attr <- read.table("networkData/BLCA_node_attr_significantmirnas.csv", header = T, sep = ",")

CESC_source_target <- read.table("networkData/CESC_source_target_info.csv", header=TRUE, sep = ";" )
CESC_node_attr <- read.table("networkData/CESC_node_attr_significantmirnas.csv", header = T, sep = ",")

CHOL_source_target <- read.table("networkData/CHOL_source_target.csv", header=TRUE, sep = ";" )
CHOL_node_attr <- read.table("networkData/CHOL_node_attr_significantmirnas.csv", header = T, sep = ",")

COAD_source_target <- read.table("networkData/COAD_source_target_info.csv", header=TRUE, sep = ";" )
COAD_node_attr <- read.table("networkData/COADNetwork_default_node.csv", header = T, sep = ",")

DLBC_source_target <- read.table("networkData/DLBC_source_target.csv", header=TRUE, sep = ";" )
DLBC_node_attr <- read.table("networkData/DLBC_generalNetwork_default_node.csv", header = T, sep = ",")

ESCA_source_target <- read.table("networkData/ESCA_source_target.csv", header=TRUE, sep = ";" )
ESCA_node_attr <- read.table("networkData/ESCA_node_attr_significantmirnas.csv", header = T, sep = ",")

HNSC_source_target <- read.table("networkData/HNSC_source_target.csv", header=TRUE, sep = ";" )
HNSC_node_attr <- read.table("networkData/HNSC_node_attr_significantmirnas.csv", header = T, sep = ",")

KICH_source_target <- read.table("networkData/KICH_source_target.csv", header=TRUE, sep = ";" )
KICH_node_attr <- read.table("networkData/KICH_node_attr_significantmirnas.csv", header = T, sep = ",")

KIRC_source_target <- read.table("networkData/KIRC_source_target.csv", header=TRUE, sep = ";" )
KIRC_node_attr <- read.table("networkData/KIRC_node_attr_significantmirnas.csv", header = T, sep = ",")

KIRP_source_target <- read.table("networkData/KIRP_source_target.csv", header=TRUE, sep = ";" )
KIRP_node_attr <- read.table("networkData/KIRP_node_attr_significantmirnas.csv", header = T, sep = ",")

LIHC_source_target <- read.table("networkData/LIHC_source_target.csv", header=TRUE, sep = ";" )
LIHC_node_attr <- read.table("networkData/LIHC_node_attr_significantmirnas.csv", header = T, sep = ",")

LGG_source_target <- read.table("networkData/LGG_source_target_info.csv", header=TRUE, sep = ";" )
LGG_node_attr <- read.table("networkData/LGG_GeneralNetwork_default_node.csv", header = T, sep = ",")

LUAD_source_target <- read.table("networkData/LUAD_source_target.csv", header=TRUE, sep = ";" )
LUAD_node_attr <- read.table("networkData/LUAD_node_attr_significantmirnas.csv", header = T, sep = ",")

LUSC_source_target <- read.table("networkData/LUSC_source_target.csv", header=TRUE, sep = ";" )
LUSC_node_attr <- read.table("networkData/LUSC_node_attr_significantmirnas.csv", header = T, sep = ",")

MESO_source_target <- read.table("networkData/MESO_source_target.csv", header=TRUE, sep = ";" )
MESO_node_attr <- read.table("networkData/MESO_generalNetwork_default_node.csv", header = T, sep = ",")

OV_source_target <- read.table("networkData/OV_source_target.csv", header=TRUE, sep = ";" )
OV_node_attr <- read.table("networkData/OV_generalNetwork_default_node.csv", header = T, sep = ",")

PAAD_source_target <- read.table("networkData/PAAD_source_target.csv", header=TRUE, sep = ";" )
PAAD_node_attr <- read.table("networkData/PAAD_node_attr_significantmirnas.csv", header = T, sep = ",")

PCPG_source_target <- read.table("networkData/PCPG_source_target.csv", header=TRUE, sep = ";" )
PCPG_node_attr <- read.table("networkData/PCPG_node_attr_significantmirnas.csv", header = T, sep = ",")

PRAD_source_target <- read.table("networkData/PRAD_source_target.csv", header = TRUE, sep = ";")
PRAD_node_attr <- read.table("networkData/PRAD_node_attr_significantmirnas.csv", header = TRUE, sep = ",")

READ_source_target <- read.table("networkData/READ_source_target.csv", header=TRUE, sep = ";" )
READ_node_attr <- read.table("networkData/READ_generalNetwork_default_node.csv", header = T, sep = ",")

SARC_source_target <- read.table("networkData/SARC_source_target.csv", header=TRUE, sep = ";" )
SARC_node_attr <- read.table("networkData/SARC_generalNetwork_default_node.csv", header = T, sep = ",")

SKCM_source_target <- read.table("networkData/SKCM_source_target.csv", header=TRUE, sep = ";" )
SKCM_node_attr <- read.table("networkData/SKCM_node_attr_significantmirnas.csv", header = T, sep = ",")

STAD_source_target <- read.table("networkData/STAD_source_target_info.csv", header=TRUE, sep = ";" )
STAD_node_attr <- read.table("networkData/STAD_node_attr_significantmirnas.csv", header = T, sep = ",")

TGCT_source_target <- read.table("networkData/TGCT_source_target.csv", header=TRUE, sep = ";" )
TGCT_node_attr <- read.table("networkData/TGCT_generalNetwork_default_node.csv", header = T, sep = ",")

THCA_source_target <- read.table("networkData/THCA_source_target.csv", header=TRUE, sep = ";" )
THCA_node_attr <- read.table("networkData/THCA_node_attr_significantmirnas.csv", header = T, sep = ",")

THYM_source_target <- read.table("networkData/THYM_source_target.csv", header=TRUE, sep = ";" )
THYM_node_attr <- read.table("networkData/THYM_node_attr_significantmirnas.csv", header = T, sep = ",")

UCEC_source_target <- read.table("networkData/UCEC_source_target.csv", header=TRUE, sep = ";" )
UCEC_node_attr <- read.table("networkData/UCEC_node_attr_significantmirnas.csv", header = T, sep = ",")

UCS_source_target <- read.table("networkData/UCS_source_target.csv", header=TRUE, sep = ";" )
UCS_node_attr <- read.table("networkData/UCS_generalNetwork_default_node.csv", header = TRUE, sep = ",")

UVM_source_target <- read.table("networkData/UVM_source_target.csv", header=TRUE, sep = ";" )
UVM_node_attr <- read.table("networkData/UVM_generalNetwork_default_node.csv", header = T, sep = ",")


commonMirnaPairs_node_attr <- read_csv("networkData/commonMirnaPairsAfterReRunNode.csv")
commonTriplets_node_attr <- read_csv("networkData/commonTripletsAfterReRunNode.csv")

commonTriplet_source_target <- read_delim("networkData/commonTriplet_source_target.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

commonMirnaPair_source_target <- read_delim("networkData/commonMirnaPair_source_target.csv", 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
####################################################################################################
ACCwMedian1 <- read_csv("dataset/medians/ACCwMedian1.csv",show_col_types = FALSE)
ACCwMedian2 <- read_csv("dataset/medians/ACCwMedian2.csv",show_col_types = FALSE)
BLCAwMedian1 <- read_csv("dataset/medians/BLCAwMedian1.csv",show_col_types = FALSE)
BLCAwMedian2 <- read_csv("dataset/medians/BLCAwMedian2.csv",show_col_types = FALSE)
# BRCAwMedian1 <- read_csv("dataset/medians/BRCAwMedian1.csv",show_col_types = FALSE)
# BRCAwMedian2 <- read_csv("dataset/medians/BRCAwMedian2.csv"),show_col_types = FALSE
CESCwMedian1 <- read_csv("dataset/medians/CESCwMedian1.csv",show_col_types = FALSE)
CESCwMedian2 <- read_csv("dataset/medians/CESCwMedian2.csv",show_col_types = FALSE)
CHOLwMedian1 <- read_csv("dataset/medians/CHOLwMedian1.csv",show_col_types = FALSE)
CHOLwMedian2 <- read_csv("dataset/medians/CHOLwMedian2.csv",show_col_types = FALSE)
COADwMedian1 <- read_csv("dataset/medians/COADwMedian1.csv",show_col_types = FALSE)
COADwMedian2 <- read_csv("dataset/medians/COADwMedian2.csv",show_col_types = FALSE)
DLBCwMedian1 <- read_csv("dataset/medians/DLBCwMedian1.csv",show_col_types = FALSE)
DLBCwMedian2 <- read_csv("dataset/medians/DLBCwMedian2.csv",show_col_types = FALSE)
ESCAwMedian1 <- read_csv("dataset/medians/ESCAwMedian1.csv",show_col_types = FALSE)
ESCAwMedian2 <- read_csv("dataset/medians/ESCAwMedian2.csv",show_col_types = FALSE)
HNSCwMedian1 <- read_csv("dataset/medians/HNSCwMedian1.csv",show_col_types = FALSE)
HNSCwMedian2 <- read_csv("dataset/medians/HNSCwMedian2.csv",show_col_types = FALSE)
KICHwMedian1 <- read_csv("dataset/medians/KICHwMedian1.csv",show_col_types = FALSE)
KICHwMedian2 <- read_csv("dataset/medians/KICHwMedian2.csv",show_col_types = FALSE)
KIRCwMedian1 <- read_csv("dataset/medians/KIRCwMedian1.csv",show_col_types = FALSE)
KIRCwMedian2 <- read_csv("dataset/medians/KIRCwMedian2.csv",show_col_types = FALSE)
KIRPwMedian1 <- read_csv("dataset/medians/KIRPwMedian1.csv",show_col_types = FALSE)
KIRPwMedian2 <- read_csv("dataset/medians/KIRPwMedian2.csv",show_col_types = FALSE)
LGGwMedian1 <- read_csv("dataset/medians/LGGwMedian1.csv",show_col_types = FALSE)
LGGwMedian2 <- read_csv("dataset/medians/LGGwMedian2.csv",show_col_types = FALSE)
LIHCwMedian1 <- read_csv("dataset/medians/LIHCwMedian1.csv",show_col_types = FALSE)
LIHCwMedian2 <- read_csv("dataset/medians/LIHCwMedian2.csv",show_col_types = FALSE)
LUADwMedian1 <- read_csv("dataset/medians/LUADwMedian1.csv",show_col_types = FALSE)
LUADwMedian2 <- read_csv("dataset/medians/LUADwMedian2.csv",show_col_types = FALSE)
LUSCwMedian1 <- read_csv("dataset/medians/LUSCwMedian1.csv",show_col_types = FALSE)
LUSCwMedian2 <- read_csv("dataset/medians/LUSCwMedian2.csv",show_col_types = FALSE)
MESOwMedian1 <- read_csv("dataset/medians/MESOwMedian1.csv",show_col_types = FALSE)
MESOwMedian2 <- read_csv("dataset/medians/MESOwMedian2.csv",show_col_types = FALSE)
OVwMedian1 <- read_csv("dataset/medians/OVwMedian1.csv",show_col_types = FALSE)
OVwMedian2 <- read_csv("dataset/medians/OVwMedian2.csv",show_col_types = FALSE)
PAADwMedian1 <- read_csv("dataset/medians/PAADwMedian1.csv",show_col_types = FALSE)
PAADwMedian2 <- read_csv("dataset/medians/PAADwMedian2.csv",show_col_types = FALSE)
PCPGwMedian1 <- read_csv("dataset/medians/PCPGwMedian1.csv",show_col_types = FALSE)
PCPGwMedian2 <- read_csv("dataset/medians/PCPGwMedian2.csv",show_col_types = FALSE)
PRADwMedian1 <- read_csv("dataset/medians/PRADwMedian1.csv",show_col_types = FALSE)
PRADwMedian2 <- read_csv("dataset/medians/PRADwMedian2.csv",show_col_types = FALSE)
READwMedian1 <- read_csv("dataset/medians/READwMedian1.csv",show_col_types = FALSE)
READwMedian2 <- read_csv("dataset/medians/READwMedian2.csv",show_col_types = FALSE)
SARCwMedian1 <- read_csv("dataset/medians/SARCwMedian1.csv",show_col_types = FALSE)
SARCwMedian2 <- read_csv("dataset/medians/SARCwMedian2.csv",show_col_types = FALSE)
SKCMwMedian1 <- read_csv("dataset/medians/SKCMwMedian1.csv",show_col_types = FALSE)
SKCMwMedian2 <- read_csv("dataset/medians/SKCMwMedian2.csv",show_col_types = FALSE)
STADwMedian1 <- read_csv("dataset/medians/STADwMedian1.csv",show_col_types = FALSE)
STADwMedian2 <- read_csv("dataset/medians/STADwMedian2.csv",show_col_types = FALSE)
TGCTwMedian1 <- read_csv("dataset/medians/TGCTwMedian1.csv",show_col_types = FALSE)
TGCTwMedian2 <- read_csv("dataset/medians/TGCTwMedian2.csv",show_col_types = FALSE)
THCAwMedian1 <- read_csv("dataset/medians/THCAwMedian1.csv",show_col_types = FALSE)
THCAwMedian2 <- read_csv("dataset/medians/THCAwMedian2.csv",show_col_types = FALSE)
THYMwMedian1 <- read_csv("dataset/medians/THYMwMedian1.csv",show_col_types = FALSE)
THYMwMedian2 <- read_csv("dataset/medians/THYMwMedian2.csv",show_col_types = FALSE)
UCECwMedian1 <- read_csv("dataset/medians/UCECwMedian1.csv",show_col_types = FALSE)
UCECwMedian2 <- read_csv("dataset/medians/UCECwMedian2.csv",show_col_types = FALSE)
UCSwMedian1 <- read_csv("dataset/medians/UCSwMedian1.csv",show_col_types = FALSE)
UCSwMedian2 <- read_csv("dataset/medians/UCSwMedian2.csv",show_col_types = FALSE)
UVMwMedian1 <- read_csv("dataset/medians/UVMwMedian1.csv",show_col_types = FALSE)
UVMwMedian2 <- read_csv("dataset/medians/UVMwMedian2.csv",show_col_types = FALSE)

####################################################################################################
TripletsInWhichCancerWCount<- read_csv("dataset/commonTripletsFinalAfterReRun.csv",show_col_types = FALSE)
TripletsInWhichCancerWCount <-filter(TripletsInWhichCancerWCount, Count> 1)
mrnaFilterCommonTriplets <- filter(commonTriplets_node_attr, info %in% "mrna")
mirnaFilterCommonTriplets <- filter(commonTriplets_node_attr, info %in% "mirna")

MirnaPairsInWhichCancerWCount <- read_csv("dataset/commonMirnaPairsFinalAfterReRun.csv",show_col_types = FALSE)
MirnaPairsInWhichCancerWCount <-filter(MirnaPairsInWhichCancerWCount, Count> 1)
mirnaListCommonMirnaPairs <- unique(rbind(commonMirnaPair_source_target$source, commonMirnaPair_source_target$target))


TCGA_abbreviations <- read.table("dataset/TCGA_abbreviations.csv", header = T, sep = ";")

Glossary <- read.table("dataset/Glossary.csv", header = T, sep = ";")


miRCoopTotalCounts <- read_delim("dataset/stats/miRCoopTotalCounts.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

commonMrnaAbove20 <- read_excel("dataset/stats/commonMrnaAbove20NormalizedForRHeatMap.xlsx")
commonMrnaAbove20 <- as.data.frame(commonMrnaAbove20)

commonMirnaAbove50 <- read_excel("dataset/stats/commonMirnaAbove50NormalizedForRHeatMap.xlsx")
commonMirnaAbove50 <- as.data.frame(commonMirnaAbove50)

miRCoopTotalCounts <- read_delim("dataset/stats/miRCoopTotalCounts.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

mRNACountsScatter <- read_delim("dataset/stats/mRNACountsScatter.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

miRNACountsScatter <- read_delim("dataset/stats/miRNACountsScatter.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

####################################################################################################
bslib_mircooptriplet_theme <- bs_theme(
    #version = 5, 
    bg = "#FFFFFF",
    fg = "#000000",
    bootswatch = "united",
    primary = "#AC2E2C",
    secondary = "#ED403D",
    success = "#00A5DF",
    info = "#00A5DF",
    warning = "#FFF100",
    danger = "#FF00E3",
    base_font = "Readex Pro",
    heading_font = "Readex Pro",
    code_font = "Readex Pro",
    "input-border-color" = "#ED403D",
    "border-radius" =  ".70rem"
)

bslib_sabanci20_theme <- bs_theme(
    #version = 5,  
    bg = "#FFFFFF",
    fg = "#000000",
    bootswatch = "united",
    primary = "#074487",
    secondary = "#179E93",
    success = "#00A5DF",
    info = "#00A5DF",
    warning = "#FFF100",
    danger = "#FF00E3",
    base_font = font_google("Ubuntu", local = TRUE) ,
    heading_font = font_google("Ubuntu", local = TRUE),
    code_font = font_google("Ubuntu", local = TRUE) ,
    "input-border-color" = "#179E93",
    "border-radius" =  ".70rem"
)

bslib_locus_theme <- bs_theme(
    #version = 5, 
    bg = "#F6F6F7",
    fg = "#000000",
    bootswatch = "united",
    primary = "#357EBDFF",
    secondary = "#8491B4FF",
    success = "#00FBF4",
    info = "#00FBF4",
    warning = "#FFF100",
    danger = "#FF00E3",
    base_font = c("Titillium Web"),
    heading_font = c("Titillium Web"),
    code_font = c("Titillium Web"),
    "input-border-color" = "#EEA236FF",
    "border-radius" =  ".70rem"
)


ui <- fluidPage(
    
    tags$head(tags$script(src="js/index.js")),
    
    tags$head( # must include css
        tags$style(HTML("
        .img-local {
        }
        .small-box .img-local {
        position: absolute;
        top: auto;
        bottom: 5px;
        right: 5px;
        z-index: 0;
        font-size: 70px;
        color: rgba(0, 0, 0, 0.15);
        }"
        ))
    ),
    
    #includeCSS("www/styles.css"),
    
    tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #179E93 !important;}')),
    tags$style(HTML(".shiny-notification {position:fixed;top: 30%;left: 30%;right: 30%;}")),
    
    setSliderColor(c("#9F9F9F", "#9F9F9F", "#9F9F9F", "#9F9F9F", "#9F9F9F", "#9F9F9F"), c(1, 2, 3, 4, 5, 6)), #depends on the number of sliders, change if you add or remove sliders.
    
    navbarPage("miRCoop",id = "mirCoop", theme = bslib_sabanci20_theme, 
               tabPanel('Home', icon = icon("home"),
                        includeHTML("miRCoop_Intro.html"), br(), br(), 
                        fluidRow(
                            column(3, align = "center", #column width 
                                   actionLink("switch_triplets_tab", icon = icon("globe", "fa-5x"), br(),br(),
                                              "Cancer Specific Triplets") ,
                                   tags$label(tags$style(HTML('#switch_triplets_tab{color:black; font-size: 18px;}'))),
                                   shinyBS::bsTooltip("switch_triplets_tab", "Shows results with the specific p-values.", placement = "top", trigger = "hover", options = NULL),
                                   tags$hr(),
                                   br(),br()),
                            column(3,align = "center", #column width
                                   actionLink("switch_commontriplets_tab",icon = icon("th-list", "fa-5x"), br(),br(),
                                              "Pan-cancer Triplets"),
                                   tags$head(tags$style(HTML("#show{background-color: darkslategray2;display:block;height: 25px;width: 25px;border-radius: 50%;border:}"))),
                                   shinyBS::bsTooltip("switch_commontriplets_tab", "Common triplets found in more than one cancer.", placement = "top", trigger = "hover", options = NULL),
                                   tags$hr(),
                                   tags$label(tags$style(HTML('#switch_commontriplets_tab{color:black; font-size: 18px;}'))),
                                   br(),br()),
                            column(3,align = "center", #column width
                                   actionLink("switch_commonmirnapairs_tab",icon = icon("th-list", "fa-5x"), br(),br(),
                                              "Pan-cancer miRNA Pairs"),
                                   tags$head(tags$style(HTML("#show{background-color: darkslategray2;display:block;height: 25px;width: 25px;border-radius: 50%;border:}"))),
                                   shinyBS::bsTooltip("switch_commonmirnapairs_tab", "Common miRNA Pairs found in more than one cancer.", placement = "top", trigger = "hover", options = NULL),
                                   tags$hr(),
                                   tags$label(tags$style(HTML('#switch_commonmirnapairs_tab{color:black; font-size: 18px;}'))),
                                   br(),br()),
                            
                            column(3,align = "center", #column width
                                   actionLink("switch_statistics_tab",icon = icon("bar-chart","fa-5x",lib = "font-awesome"), br(),br(),
                                              "Statistics"),
                                   tags$head(tags$style(HTML("#show{background-color: darkslategray2;display:block;height: 25px;width: 25px;border-radius: 50%;border:}"))),
                                   shinyBS::bsTooltip("switch_statistics_tab", "Statistics", placement = "top", trigger = "hover", options = NULL),
                                   tags$hr(),
                                   tags$label(tags$style(HTML('#switch_statistics_tab{color:black; font-size: 18px;}'))),
                                   br(),br()),
                            br(),br(),
                        ),
                        hr(style=" width: 100%; height: 3px; margin-left: auto;  margin-right: auto; margin-top:27vh ;background-color: #074487; border: 0 none;"),
                        
                        tags$footer(tags$div(style="font-size:12px",align="center",
                                             
                                             tags$a(href="https://www.sabanciuniv.edu/en/", target="_blank",
                                                    tags$img(src="sabanci.png", style="width:10%; height:auto; float: left; margin: 8px")
                                             ),
                                             tags$a(href="https://w3.bilkent.edu.tr/bilkent/", target="_blank",
                                                    tags$img(src="bilkent.png", style="width:15%; height:auto; float: right; margin: 8px")),
                                             tags$div("miRCoop is a product of the collaboration between the Department of Computer Science and Engineering from Sabanci University and Department of Molecular Biology and Genetics from Bilkent University",align="center"),
                                             tags$br(),
                                             tags$div("The project was developed with the funding from TUSEB(Health Institutes of Turkey) through the project .............",align="center"),
                                             tags$br(),
                                             tags$div("For any enquiries or bug reports please send us an e-mail:", align="center"),
                                             tags$a(href="mailto:xyz@xyz.com?subject=About_miRCoop", "xyz@xyz.com")
                        )
                        )
                        ),
               tabPanel("Cancer Specific Triplets",value="CancerSpecificTriplets",fluid = TRUE, icon = icon("globe"),
                        sidebarLayout(
                            sidebarPanel(width = 3,
                                         selectInput(inputId = "dataset",
                                                     label = p("Cancer Type ",infoBtn('workingPop') %>% 
                                                                   spsComps::bsTooltip(
                                                                             title = "TCGA Abbreviations of the cancer types",
                                                                             placement = "right",
                                                                             trigger = "hover"
                                                                             
                                                                  )),
                                                     choices = cancerNames,
                                                     selected = "50 Free",
                                                     width = "220px"
                                         ),
                                         uiOutput("dataset"),
                                         
                                         hr(),
                                         selectizeInput("mrnaFilter", "mRNA Filter", choices=NULL, multiple=TRUE),
                                         br(),
                                         selectizeInput("mirnaFilter", "miRNA Filter", choices=NULL, multiple=TRUE),
                                         hr(),
                                         
                                         # conditionalPanel(condition = "input.dataset == 'ACC'",
                                         #                  sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)),
                                         # 
                                         # conditionalPanel(condition = "input.dataset == 'PRAD'",
                                         #                 sliderInput("breakCount", "Break Count", min=50, max=5000, value=500)),
                                         

                                         sliderInput("Lancaster_XY_Z_range",
                                                     label = p("Triplet pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                     value = c(0,0.01),
                                                     min = 0,
                                                     max = 0.01),
                                         
                                         hr(),
                                         
                                         checkboxGroupInput(inputId = "is_mrna_tf",
                                                            label = "Is mRNA TF?",
                                                            choiceNames = c("mRNA is TF", "mRNA is not TF"),
                                                            choiceValues = c("True","False"),
                                                            selected = c("True","False")),
                                        downloadButton("downloadData", "Download")
                                         
                                         
                                         
                                         
                            ),
                            mainPanel(

                                tabsetPanel(type = "tabs",
                                            tabPanel("Table", br(),
                                                     DT::dataTableOutput("table", height = "800px")),
                                            
                                            tabPanel("Network", shinycustomloader::withLoader(visNetworkOutput("vNetwork", height = "100vh"),type = "html",loader="loader3"))
                                )
                            )
                        )
               ),
               
               
               tabPanel("Pan-cancer Triplets", value="Pan-cancerTriplets",fluid = TRUE,icon = icon("th-list"),
                        sidebarLayout(
                            sidebarPanel(
                                
                                # checkboxGroupInput(inputId = "CommonCancer", 
                                #                    label = "Select Cancer Names",
                                #                    choices = cancerNames,
                                #                    selected=cancerNames),
                                pickerInput(inputId = "CommonTripletCancer", label = "Select Cancer Names",choices = cancerNames, selected=cancerNames, multiple = TRUE),
                                hr(),
                                selectizeInput("mrnaCommonTriplet", "mRNA Filter", choices=unique(mrnaFilterCommonTriplets$name), multiple=TRUE),
                                br(),
                                selectizeInput("mirnaCommonTriplet", "miRNA Filter", choices=unique(mirnaFilterCommonTriplets$name), multiple=TRUE),
                                hr(),
                                br()
                                
                              
                                
                            ),
                            
                            mainPanel(
                                useShinyalert(),
                                tabsetPanel(type = "tabs",
                                            tabPanel("Table", br(),
                                                     DT::dataTableOutput("tableCommonTriplet")),
                                            tabPanel("Network", shinycustomloader::withLoader(visNetworkOutput("commonTripletNetwork", height = "100vh"),type = "html",loader="loader3"))
                                )
                            )
                            
                        )
               ),
               
               tabPanel("Pan-cancer miRNA Pairs", value="Pan-cancermiRNAPairs",fluid = TRUE,icon = icon("th-list"),
                        sidebarLayout(
                          sidebarPanel(
                            
                            # checkboxGroupInput(inputId = "CommonCancer", 
                            #                    label = "Select Cancer Names",
                            #                    choices = cancerNames,
                            #                    selected=cancerNames),
                            pickerInput(inputId = "CommonMirnaPairCancer", label = "Select Cancer Names",choices = cancerNames, selected=cancerNames, multiple = TRUE),
                            hr(),
                            selectizeInput("mirnaCommonMirnaPair", "miRNA Filter", choices=mirnaListCommonMirnaPairs, multiple=TRUE),
                            hr(),
                            pickerInput(inputId = "CommonMirnaPairCancerCount", label = "Filter with Count",choices = c(1,2,3,4,5,6,7,8), selected=c(1,2,3,4,5,6,7,8), multiple = TRUE),
                            br(),
                            br()
                            
                            
                          ),
                          
                          mainPanel(
                            useShinyalert(),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Common miRNA Pair Table", DT::dataTableOutput("tableCommonmiRNAPair")),
                                        tabPanel("Network", shinycustomloader::withLoader(visNetworkOutput("commonMirnaNetwork", height = "100vh"),type = "html",loader="loader3"))
                                        
                                        
                            )
                          )
                          
                        )
               ),
               
               tabPanel("Statistics", value = "Statistics", fluid= TRUE, icon = icon("bar-chart",lib = "font-awesome"),
                        tags$div(
                          tags$br(),
                          shinycustomloader::withLoader(plotlyOutput("totalCountsPlot"),type = "html",loader="loader3"),
                          tags$p(style="margin-left:45px; margin-top:10px;font-size:9pt","Each bar represents statics for one cancer , from left to right: number of triplets found in the cancer, the number of unique miRNA pairs that participate in these triplets, number of miRNAs unique in the triplets, number of unique mRNAs."),
                          
                          tags$hr()
                          
                        ),
                        tags$br(),
                        tags$div(style="max-width:65vw; display: block; margin-left: auto; margin-right: auto;",
                                shinycustomloader::withLoader(plotlyOutput("commonMrnaHeatmap",height=600),type = "html",loader="loader3"),
                                tags$p(style="margin-left:45px; margin-top:10px;font-size:9pt","The heatmap shows the normalized number of triplets the mRNA participates in each cancer. The numbers are normalized by the number of triplets found in the cancer. mRNAs that have more than 20 total participation are shown.")
                        ),
                        tags$br(),
                        tags$div(
                          shinycustomloader::withLoader(plotlyOutput("commonMirnaHeatmap",height=800),type = "html",loader="loader3"),
                          tags$p(style="margin-left:40px;margin-top:10px;font-size:9pt","The heatmap shows the normalized number of triplets the miRNA participates in each cancer. The numbers are normalized by the number of triplets found in the cancer. miRNAs that have more than 50 total participation are shown."),
                          tags$hr()

                        ),
                        # tags$div(style="height:800px",
                        #   tags$div(style="float: left; max-width:45vw; width:100%; margin-top:100px",
                        #            shinycustomloader::withLoader(plotlyOutput("commonMrnaHeatmap",height=600),type = "html",loader="loader3")
                        #            ),
                        #   tags$div(style="float: right; max-width:45vw; width:100%;",
                        #            shinycustomloader::withLoader(plotlyOutput("commonMirnaHeatmap",height=800),type = "html",loader="loader3")
                        #            )
                        # 
                        # ),
                        tags$br(),
                        tags$div(
                          tags$div(style="float: left; width:100%; height:auto; max-width:45vw; margin-bottom:15px; margin-top:15px",
                                   shinycustomloader::withLoader(plotlyOutput("MrnaScatterPlot"),type = "html",loader="loader3"),
                                   tags$p(style="margin-left:40px;margin-top:10px;font-size:9pt","The number of mRNAs the miRNA targets plotted against the number of triplets the mRNA is found in.  The Pearson correlation is found X"),
                                   ),
                          tags$div(style="float: right ; width:100%; height:auto; max-width:45vw; margin-bottom:15px; margin-top:15px ",
                                   shinycustomloader::withLoader(plotlyOutput("MirnaScatterPlot"),type = "html",loader="loader3"),
                                   tags$p(style="margin-left:40px;margin-top:10px;font-size:9pt","The number of miRNAs that target the mRNA plotted against the number of triplets the miRNA is found in. The Pearson correlation is found X"),
                                  ),
                 
                        ),
                        tags$br()
                        ),
               
               tabPanel("About",value="About",fluid = TRUE,icon = icon("info-circle"),
                        fluidRow(
                            column (12,
                                    h4(p("About miRCoop")),
                                    h5(p(align = "justify;",style="font-size:16px;","miRCoop identifies synergistic miRNA pairs which have weak or no repression on the target mRNA, but when bound together induce strong repression of their target's. To achieve this, a three-step method was proposed. First, miRNA pairs targeting the common mRNA were identified. For this, experimentally validated databases(miRTarBase, TarBase v7.0, miRecords ) and a prediction algorithm (TargetScan) were resorted. A miRNA-mRNA target catalogue was generated by intersecting miRNA-mRNA pairs from 3 experimentally validated databases with those from TargetScan. The triplets where miRNA pairs have an overlapping binding site on the target mRNA were filtered. miRNA pairs targeting common mRNA composed a potential triplet. Secondly, potential triplets obtained from Step 1 were eliminated according to the expression profiles of miRNAs and mRNAs. The rationale for the exclusion of the potential triplets is based on this assumption: The mRNA expression level is expected to be lower when both miRNAs are upregulated compared to when both miRNAs are downregulated. In the thid step, statistical interaction tests were performed on miRNA and mRNA expression data for each potential triplet candidate that passes through Step2. We are interested in cases where miRNAs are pairwise independent with mRNAs but form a mutually dependent triplet. ")
                                       ),
                                    tags$img(src="miRCoopSteps.png",style="width:100%; height:auto; max-width:50vw; display: block; margin-left: auto; margin-right: auto; margin-top:5px"),
                                    h5(p(align = "justify;",style="font-size:16px;","The triplets detected for 31 different cancers can be examined in the ‘Cancer Specific Triplets’ screen. It takes pre-computed results as data source and presents themit to the user with a datatable in a structured manner. This section essentially builds on the identified triplets, miRNA pairs and their target mRNA, which is represented with both HGNC symbol and Entrez Gene ID, and triplet p-values. The data has been enriched with the following additional information:")),
                                    tags$ul(tags$li("Experimental data source of miRNA and mRNA relationships"),
                                            tags$li("Differential expression analysis results: Differential expression analysis was performed by comparing the expression of each miRNAs and mRNAs between Primary Tumor samples and Solid Tissue Normal samples. Positive logFC values indicate upregulation in the primary tumor samples compared to solid tissue normal samples. In contrast, a negative logFC value shows downregulation in the primary tumor samples compared to solid tissue normal. A p-value < 0.05 is selected as cut-off criteria to define statistically significant difference for the logFC"),
                                            tags$li("Literature support of cancer-miRNA and cancer-mRNA relationship"),
                                            tags$li("mRNA expressions of patients grouped by miRNA expression levels"),
                                            tags$li("Transcription factor information")
                                            ),
                                    #h5(p(align = "justify;",style="font-size:16px;","An important aspect of the application is its capability of producing interactive triplet networks.")),
                                    tags$br(),
                                    tags$br(),
                                    tags$br(),
                                    tags$div(style="padding:10px; padding-left:20px; line-height:1.5em; background: #fff; border-radius:5px; border:1px solid #ccc; font-family:Ubuntu",
                                             "If you find miRCoop useful for your research, please cite the following papers:",
                                             tags$br(),
                                             tags$div(style= "padding-bottom:10px"),
                                             tags$a(href="https://ieeexplore.ieee.org/document/9311836",
                                                    tags$i("miRCoop: Identifying Cooperating miRNAs via Kernel Based Interaction Tests")),
                                             tags$br(),
                                             "G. Olgun and O. Tastan, IEEE/ACM Transactions on Computational Biology and Bioinformatics, doi: 10.1109/TCBB.2020.3047901."
                                             
                                    )
                                    ),

                            # column(6, 
                            #        h4(p("Abbreviations and Full Names of TCGA Projects")),
                            #        tags$div(style="font-size:16px;",tableOutput("TCGAAbbrv"))
                            # )
                        ),
                        
                        hr(style=" width: 100%; height: 3px; margin-left: auto;  margin-right: auto; background-color: #074487; border: 0 none;"),

                        tags$footer(tags$div(style="font-size:12px;",align="center",
                                             
                                             tags$a(href="https://www.sabanciuniv.edu/en/", target="_blank",
                                                    tags$img(src="sabanci.png", style="width:10%; height:auto; float: left; margin: 8px")
                                                    ),
                                             tags$a(href="https://w3.bilkent.edu.tr/bilkent/", target="_blank",
                                                    tags$img(src="bilkent.png", style="width:15%; height:auto; float: right; margin: 8px")),
                                             tags$div("miRCoop is a product of the collaboration between the Department of Computer Science and Engineering from Sabanci University and Department of Molecular Biology and Genetics from Bilkent University",align="center"),
                                             tags$br(),
                                             tags$div("The project was developed with the funding from TUSEB(Health Institutes of Turkey) through the project .............",align="center"),
                                             tags$br(),
                                             tags$div("For any enquiries or bug reports please send us an e-mail:", align="center"),
                                             tags$a(href="mailto:xyz@xyz.com?subject=About_miRCoop", "xyz@xyz.com")
                                             )
                          )
                        ),
               tabPanel("Glossary", value="Glossary",fluid = TRUE,icon = icon("info-circle"),
                        fluidRow(
                          column(6,
                                 h4(p("Glossary")),
                                 tags$div(style="font-size:16px;",tableOutput("Glossary"))
                                 ),
                          column(6, 
                                 h4(p("Abbreviations and Full Names of TCGA Projects")),
                                 tags$div(style="font-size:16px;",tableOutput("TCGAAbbrv"))
                          )
                        )
                        
                        )
               
               
               
    )
    
    
)






# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #source("override.R", local = TRUE) # override 'icon' and 'valueBox'
    clrs <- c("yellow", "orange", "purple", "red", "blue", "navy",
              "light-blue", "teal", "olive", "green", "fuchsia", "maroon")
    pTextSize <- function(x, value) tags$p(x, style=paste0("font-size: ", value, "%;"))
    
    #icon files 
    gen_network <- "www/project_diagram.png"
    
    observeEvent(input$switch_triplets_tab, {
        updateTabsetPanel(session, "mirCoop",selected = "CancerSpecificTriplets")
    })
    
    observeEvent(input$switch_commontriplets_tab, {
        updateTabsetPanel(session, "mirCoop",selected = "Pan-cancerTriplets")
    })
    
    observeEvent(input$switch_commonmirnapairs_tab, {
      updateTabsetPanel(session, "mirCoop",selected = "Pan-cancermiRNAPairs")
    })
    
    observeEvent(input$switch_statistics_tab, {
      updateTabsetPanel(session, "mirCoop",selected = "Statistics")
    })

    datasetInput <- reactive({
        switch(input$dataset, 
               "ACC" = ACC,
               "BLCA"=BLCA,
               #"BRCA"=BRCA,
               "CESC"=CESC,
               "CHOL"=CHOL,
               "COAD"=COAD,
               "DLBC"=DLBC,
               "ESCA"=ESCA,
               "HNSC"=HNSC,
               "KICH"=KICH,
               "KIRC"=KIRC,
               "KIRP"=KIRP,
               "LGG"=LGG,
               "LIHC"=LIHC,
               "LUAD"=LUAD,
               "LUSC"=LUSC,
               "MESO"=MESO,
               "OV"=OV,
               "PAAD"=PAAD,
               "PCPG"=PCPG,
               "PRAD"=PRAD,
               "READ"=READ,
               "SARC"=SARC,
               "SKCM"=SKCM,
               "STAD"=STAD,
               "TGCT"=TGCT,
               "THCA"=THCA,
               "THYM"=THYM,
               "UCEC"=UCEC,
               "UCS"=UCS,
               "UVM"=UVM
        )
    })
    
    
    observeEvent(input$dataset,{
        updateSelectizeInput(session,"mrnaFilter",choices = unique(datasetInput()$hgnc_symbol))
    })
    
    observeEvent(input$dataset,{
        mirnaList <- stringr::str_replace(union_all(datasetInput()$mirna1,datasetInput()$mirna2),"mir","miR")
        updateSelectizeInput(session,"mirnaFilter",choices = unique(mirnaList))
    })
    
    
    datasetInput2 <- reactive({
    
        mirnaFilter <- NULL
        mrnaFilter <- NULL
        TFFilter <- NULL
        if(nrow(datasetInput()) >0 & !is.null(datasetInput())){
          if(length(input$mirnaFilter) == 0 & length(input$mrnaFilter) == 0 ){
            concated <- datasetInput()
          }
          
          if(length(input$mrnaFilter) > 0){
            mrnaFilter <- filter(datasetInput(),hgnc_symbol %in% input$mrnaFilter)
          }
          
          if(length(input$mirnaFilter) >0){
            mirnaFilter <- filter(datasetInput(), mirna1 %in% tolower(input$mirnaFilter) | mirna2 %in% tolower(input$mirnaFilter))
          }
          
          if(length(input$mirnaFilter) != 0 || length(input$mrnaFilter) != 0){
            concated <- distinct(rbind(mrnaFilter, mirnaFilter))
          }
          
        }
        
        return (concated)
      
    })
    
    datasetInput3 <- reactive({
      
        minLanc = min(datasetInput2()$Lancaster_XY_Z)
        maxLanc = max(datasetInput2()$Lancaster_XY_Z)
      
        dataset <- datasetInput2()
        
        # filteredWithTests <-filter(datasetInput2(),
        #                            Lancaster_XY_Z >=input$Lancaster_XY_Z_range[1], Lancaster_XY_Z <=input$Lancaster_XY_Z_range[2]
        # )
        # 
        #print(nrow(dataset))
        
        if(nrow(dataset >0) & !is.null(dataset)){
          if(length(tolower(input$is_mrna_tf)) !=2){
            dataset <- filter(datasetInput2(),tolower(is_mrna_tf) %in% tolower(input$is_mrna_tf))
          }
          
          else{
            dataset <- datasetInput2()
            
          }
          
        }
        else{
          dataset <- NULL
        }

       
        return (dataset)
        

      
    })
    
    datasetInput4<- reactive({
      
      dataset <- datasetInput3()
      
      if(nrow(dataset)>0 & !is.null(dataset)){
        filteredWithTests <-filter(dataset,
                                   Lancaster_XY_Z >=input$Lancaster_XY_Z_range[1], Lancaster_XY_Z <=input$Lancaster_XY_Z_range[2]
        )
        
      }
      else{
        filteredWithTests <- NULL
      }
      return(filteredWithTests)
      
    })
    
    true_false_formatter <-
      formatter("span",
                style = x ~ style(
                  font.weight = "bold",
                  color = ifelse(x == 'true', "forestgreen", ifelse(x == 'false', "red", "black"))
                ))
    
    
    
    button <- function(tbl){
      function(i){
        sprintf(
          '<button id="button_%s_%d" type="button" style="background-color:#074487;border-radius:7px; border:1px solid #124d77; color:#ffffff; padding:4px 4px;font-family:Ubuntu;font-size:16px" onclick="%s">Box Plot</button>', tbl,
          i, "Shiny.setInputValue('button', this.id);")
      }
    }
    
    
    DatasetRoundDigits <-reactive({
      
        dataset <-datasetInput4()
        
        if(!is.null(dataset) & nrow(dataset) >0){
          dataset$mirna1 <- stringr::str_remove(dataset$mirna1, "hsa-")
          dataset$mirna2 <- stringr::str_remove(dataset$mirna2, "hsa-")
          dataset$mirna1 <- stringr::str_replace(dataset$mirna1,"mir","miR")
          dataset$mirna2 <- stringr::str_replace(dataset$mirna2,"mir","miR")
          # dataset <- dataset %>%
          #   dplyr::mutate(across(where(is.numeric),round,3))
          
        }
        else{
          #dataset <- datatable(data.frame(Nachricht = "Die ausgewählte Schnittstelle enthält hierfür keine Daten."))
          #shinyalert::shinyalert("Warning", "No Matching Records Based on Your Filter!", type = "info")
          #showNotification(paste("Notification message"), duration = 60,type="message")
          dataset <- NULL
        }
        return (dataset)
        
    })
    
    
    
output$table <- DT::renderDataTable({
  
  print(!is.null(DatasetRoundDigits()))
  print(nrow(DatasetRoundDigits()) >0)
  
  if(!is.null(DatasetRoundDigits()) & nrow(DatasetRoundDigits()) >0){
    print("GİRDİ")
    DT1 <- DatasetRoundDigits()
    DT <- cbind(DT1,
                button = sapply(1:nrow(DT1), button("table")),
                stringsAsFactors = FALSE)
    
    DT[is.na(DT)] <- " "
    DT1$mirna1Literature <- stringr::str_replace(DT1$mirna1Literature,"NA"," ")
    DT1$mirna2Literature <- stringr::str_replace(DT1$mirna2Literature,"NA"," ")
    DT1$mrnaLiterature <- stringr::str_replace(DT1$mrnaLiterature,"NA"," ")
    
    
    
    
    hideList1 <- c(7,8,9,10,11,12,13)
    hideList2 <- c(7,8,9,10,11,12,13,14,15,16,17)
    hideList3 <- c(7,8,9,10,11,12,13)
    hideList4 <- c(7,8,9)
    hideList5 <-c(7,8,9,10,11,12,13,14,15)
    
    ifelse(input$dataset=="ACC" || input$dataset=="DLBC" || input$dataset=="LGG" || input$dataset=="MESO" || input$dataset=="OV" || input$dataset=="UCS" || input$dataset=="UVM", columnHideList <-hideList1,
           ifelse(input$dataset=="BLCA" || input$dataset=="CESC" || input$dataset=="CHOL" || input$dataset=="ESCA" || input$dataset=="HNSC" || input$dataset=="KICH" || input$dataset=="KIRC" || input$dataset=="KIRP" || input$dataset=="LIHC" || input$dataset=="LUAD" || input$dataset=="LUSC" || input$dataset=="PAAD" || input$dataset=="PCPG" || input$dataset=="PRAD" || input$dataset=="SKCM" || input$dataset=="STAD" || input$dataset=="THCA" || input$dataset=="UCEC", columnHideList <-hideList2,
                  ifelse(input$dataset=="COAD" || input$dataset=="READ" || input$dataset=="SARC", columnHideList <-hideList3,
                         ifelse(input$dataset=="TGCT", columnHideList <-hideList4,
                                ifelse(input$dataset=="THYM", columnHideList <-hideList5, columnHideList <-c())))))
    
    
    # tripletvalue <- tags$span(
    #   "Triplet pvalue",
    #   infoBtn('notWorking')%>%
    #     spsComps::bsTooltip(title = "Kernel Three-Variable Lancaster Interaction Test.",
    #                         placement = "top",
    #                         trigger = "hover")
    #   
    # ) %>% as.character()
    
    
    
    tripletvalue <- tags$span(
      "Triplet pvalue",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    )%>% as.character()
    
    mirna1Literature <- tags$span(
      "miRNA1 Literature",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    mirna2Literature <- tags$span(
      "miRNA2 Literature",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    mRNALiterature <- tags$span(
      "mRNA Literature",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    miRNA1mRNADatabase <- tags$span(
      "miRNA1-mRNA Database",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    miRNA2mRNADatabase <- tags$span(
      "miRNA2-mRNA Database",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    miRNA1pvalue <- tags$span(
      " miRNA1 pvalue",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    miRNA1LogFC <- tags$span(
      "miRNA1 LogFC",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    miRNA2pvalue <- tags$span(
      " miRNA2 pvalue",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    miRNA2LogFC <- tags$span(
      "miRNA2 LogFC",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    mRNApvalue <- tags$span(
      "mRNA pvalue",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()

    mRNALogFC <- tags$span(
      "mRNA LogFC",
      a(infoBtn('question'), onclick="customHref('Glossary')")
    ) %>% as.character()
    

    nameList1 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", tripletvalue,"is mRNA TF", mirna1Literature, mirna2Literature, mRNALiterature, miRNA1mRNADatabase, miRNA2mRNADatabase,"BH_rejected","BH_pvalues_adjusted", "miRNA-mRNA Expressions")
    nameList2 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", tripletvalue, " is mRNA TF", mirna1Literature, mirna2Literature, mRNALiterature, miRNA1mRNADatabase, miRNA2mRNADatabase, miRNA1pvalue, miRNA1LogFC, miRNA2pvalue, miRNA2LogFC, mRNApvalue, mRNALogFC,"miRNA-mRNA Expressions")
    nameList3 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", tripletvalue, " is mRNA TF", mirna1Literature, mirna2Literature, mRNALiterature, miRNA1mRNADatabase, miRNA2mRNADatabase,mRNApvalue,mRNALogFC,"miRNA-mRNA Expressions" )
    nameList4 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", tripletvalue, " is mRNA TF", mirna1Literature, miRNA1mRNADatabase, miRNA2mRNADatabase, "miRNA-mRNA Expressions")
    nameList5 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", tripletvalue, " is mRNA TF", mirna1Literature, miRNA1mRNADatabase, miRNA2mRNADatabase, miRNA1pvalue, miRNA1LogFC, miRNA2pvalue, miRNA2LogFC, mRNApvalue, mRNALogFC,"miRNA-mRNA Expressions")

    # nameList1 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", "tripletvalue","is mRNA TF", "mirna1Literature", "mirna2Literature", "mRNALiterature", "miRNA1mRNADatabase", "miRNA2mRNADatabase","miRNA-mRNA Expressions")
    # nameList2 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", "tripletvalue", " is mRNA TF", "mirna1Literature", "mirna2Literature", "mRNALiterature", "miRNA1mRNADatabase", "miRNA2mRNADatabase", "miRNA1pvalue", "miRNA1LogFC", "miRNA2pvalue", "miRNA2LogFC", "mRNApvalue", "mRNALogFC","miRNA-mRNA Expressions")
    # nameList3 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", "tripletvalue", " is mRNA TF", "mirna1Literature", "mirna2Literature", "mRNALiterature", "miRNA1mRNADatabase", "miRNA2mRNADatabase","mRNApvalue", "mRNALogFC","miRNA-mRNA Expressions" )
    # nameList4 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", "tripletvalue", " is mRNA TF", "mirna1Literature", "miRNA1mRNADatabase", "miRNA2mRNADatabase", "miRNA-mRNA Expressions")
    # nameList5 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", "tripletvalue", " is mRNA TF", "mirna1Literature", "miRNA1mRNADatabase", "miRNA2mRNADatabase", "miRNA1pvalue", "miRNA1LogFC", "miRNA2pvalue", "miRNA2LogFC", "mRNApvalue", "mRNALogFC","miRNA-mRNA Expressions")
    # 

    
    ifelse(input$dataset=="ACC" || input$dataset=="DLBC" || input$dataset=="LGG" || input$dataset=="MESO" || input$dataset=="OV" || input$dataset=="UCS" || input$dataset=="UVM", columnNameList <-nameList1,
           ifelse(input$dataset=="BLCA" || input$dataset=="CESC" || input$dataset=="CHOL" || input$dataset=="ESCA" || input$dataset=="HNSC" || input$dataset=="KICH" || input$dataset=="KIRC" || input$dataset=="KIRP" || input$dataset=="LIHC" || input$dataset=="LUAD" || input$dataset=="LUSC" || input$dataset=="PAAD" || input$dataset=="PCPG" || input$dataset=="PRAD" || input$dataset=="SKCM" || input$dataset=="STAD" || input$dataset=="THCA" || input$dataset=="UCEC", columnNameList <-nameList2,
                  ifelse(input$dataset=="COAD" || input$dataset=="READ" || input$dataset=="SARC", columnNameList <-nameList3,
                         ifelse(input$dataset=="TGCT", columnNameList <-nameList4,
                                ifelse(input$dataset=="THYM", columnNameList <-nameList5, columnNameList <-c())))))
    
    # 
    # headerCallback1 <- c(
    #   "function(thead, data, start, end, display){",
    #   "  var tooltips = ['Kernel Three-Variable Lancaster Interaction Test p-value'];",
    #   "  for(var i=5; i<6; i++){",
    #   "    $('th:eq('+i+')',thead).attr('title', tooltips[0]);",
    #   "    $('th:eq('+i+')', thead).css('cursor', 'help');",
    #   "  }",
    # 
    #   "}"
    # )
    # 
   
    icon_formatter <- function() {
      formatter("span", 
                style = x ~ formattable::style(color = ifelse(x, "#179E93", "red")), x ~ icontext(ifelse(x, "ok", "remove"), "")
      )	 	 
    }
    
    
    significant_bold <- formatter("span", 
                                  style = x ~ formattable::style("font-weight" = ifelse(x <0.05, "bold", NA)))
    
    sign_formatter <- formatter("span", 
                                style = x ~ formattable::style(color = ifelse(x > 0, "red", 
                                                                              ifelse(x < 0, "#074487", "black"))))
    
    
    as.datatable(formattable(DT, list(
      Lancaster_XY_Z = color_tile("transparent", "lightpink"),
      is_mrna_tf = icon_formatter(),
      miRNA1_logFC = sign_formatter,
      miRNA2_logFC = sign_formatter,
      miRNA1_pvalue = significant_bold,
      miRNA2_pvalue = significant_bold

    )),escape = F, fillContainer = TRUE,
    colnames=columnNameList,
    extensions = 'Buttons',
    options = list(dom = 'Bfrtip',
                   buttons=list(list(extend = 'colvis', columns = c(5:ncol(DT)))),
                   columnDefs = list(list(visible=FALSE, targets=columnHideList)),
                   #headerCallback = JS(headerCallback),
                   searching=FALSE, paging=TRUE
                   ))
    
    # DT::datatable(DT,escape = F, fillContainer = TRUE,
    #               rownames=T
    #               # colnames=columnNameList,
    #               # extensions = 'Buttons',
    #               # options = list(dom = 'Bfrtip',
    #               #                buttons=list(list(extend = 'colvis', columns = c(5:ncol(DT)))),
    #               #                columnDefs = list(list(visible=FALSE, targets=columnHideList))
    #               #                #headerCallback = JS(headerCallback)
    #               # 
    #               #                )
    # )
    
    
  }
  else{
    dataset <- datatable(data.frame(Nachricht = "Die ausgewählte Schnittstelle enthält hierfür keine Daten."))
    DT::datatable(dataset,escape = F, fillContainer = TRUE
    )
  }

      
    

    

})

observeEvent(input$button, {
  splitID <- strsplit(input$button, "_")[[1]]
  DT <- DatasetRoundDigits()
  tbl <- splitID[2]
  row <- splitID[3]


  #############################################################################################################################################
  ACCwMedian1JustMedians <- as.double(ACCwMedian1[strtoi(row),15:ncol(ACCwMedian1)][grepl("[0-9]+",ACCwMedian1[strtoi(row),15:ncol(ACCwMedian1)])])
  ACCwMedian2JustMedians <- as.double(ACCwMedian2[strtoi(row),15:ncol(ACCwMedian2)][grepl("[0-9]+",ACCwMedian2[strtoi(row),15:ncol(ACCwMedian2)])])
  
  BLCAwMedian1JustMedians <- as.double(BLCAwMedian1[strtoi(row),21:ncol(BLCAwMedian1)][grepl("[0-9]+",BLCAwMedian1[strtoi(row),21:ncol(BLCAwMedian1)])])
  BLCAwMedian2JustMedians <- as.double(BLCAwMedian2[strtoi(row),21:ncol(BLCAwMedian2)][grepl("[0-9]+",BLCAwMedian2[strtoi(row),21:ncol(BLCAwMedian2)])])

  # BRCAwMedian1JustMedians <- as.double(BRCAwMedian1[strtoi(row),15:ncol(BRCAwMedian1)][grepl("[0-9]+",BRCAwMedian1[strtoi(row),15:ncol(BRCAwMedian1)])])
  # BRCAwMedian2JustMedians <- as.double(BRCAwMedian2[strtoi(row),15:ncol(BRCAwMedian2)][grepl("[0-9]+",BRCAwMedian2[strtoi(row),15:ncol(BRCAwMedian2)])])
  
  CESCwMedian1JustMedians <- as.double(CESCwMedian1[strtoi(row),21:ncol(CESCwMedian1)][grepl("[0-9]+",CESCwMedian1[strtoi(row),21:ncol(CESCwMedian1)])])
  CESCwMedian2JustMedians <- as.double(CESCwMedian2[strtoi(row),21:ncol(CESCwMedian2)][grepl("[0-9]+",CESCwMedian2[strtoi(row),21:ncol(CESCwMedian2)])])
  
  CHOLwMedian1JustMedians <- as.double(CHOLwMedian1[strtoi(row),21:ncol(CHOLwMedian1)][grepl("[0-9]+",CHOLwMedian1[strtoi(row),21:ncol(CHOLwMedian1)])])
  CHOLwMedian2JustMedians <- as.double(CHOLwMedian2[strtoi(row),21:ncol(CHOLwMedian2)][grepl("[0-9]+",CHOLwMedian2[strtoi(row),21:ncol(CHOLwMedian2)])])
  
  COADwMedian1JustMedians <- as.double(COADwMedian1[strtoi(row),17:ncol(COADwMedian1)][grepl("[0-9]+",COADwMedian1[strtoi(row),17:ncol(COADwMedian1)])])
  COADwMedian2JustMedians <- as.double(COADwMedian2[strtoi(row),17:ncol(COADwMedian2)][grepl("[0-9]+",COADwMedian2[strtoi(row),17:ncol(COADwMedian2)])])
  
  DLBCwMedian1JustMedians <- as.double(DLBCwMedian1[strtoi(row),15:ncol(DLBCwMedian1)][grepl("[0-9]+",DLBCwMedian1[strtoi(row),15:ncol(DLBCwMedian1)])])
  DLBCwMedian2JustMedians <- as.double(DLBCwMedian2[strtoi(row),15:ncol(DLBCwMedian2)][grepl("[0-9]+",DLBCwMedian2[strtoi(row),15:ncol(DLBCwMedian2)])])
  
  ESCAwMedian1JustMedians <- as.double(ESCAwMedian1[strtoi(row),21:ncol(ESCAwMedian1)][grepl("[0-9]+",ESCAwMedian1[strtoi(row),21:ncol(ESCAwMedian1)])])
  ESCAwMedian2JustMedians <- as.double(ESCAwMedian2[strtoi(row),21:ncol(ESCAwMedian2)][grepl("[0-9]+",ESCAwMedian2[strtoi(row),21:ncol(ESCAwMedian2)])])
  
  HNSCwMedian1JustMedians <- as.double(HNSCwMedian1[strtoi(row),21:ncol(HNSCwMedian1)][grepl("[0-9]+",HNSCwMedian1[strtoi(row),21:ncol(HNSCwMedian1)])])
  HNSCwMedian2JustMedians <- as.double(HNSCwMedian2[strtoi(row),21:ncol(HNSCwMedian2)][grepl("[0-9]+",HNSCwMedian2[strtoi(row),21:ncol(HNSCwMedian2)])])
  
  KICHwMedian1JustMedians <- as.double(KICHwMedian1[strtoi(row),21:ncol(KICHwMedian1)][grepl("[0-9]+",KICHwMedian1[strtoi(row),21:ncol(KICHwMedian1)])])
  KICHwMedian2JustMedians <- as.double(KICHwMedian2[strtoi(row),21:ncol(KICHwMedian2)][grepl("[0-9]+",KICHwMedian2[strtoi(row),21:ncol(KICHwMedian2)])])
  
  KIRCwMedian1JustMedians <- as.double(KIRCwMedian1[strtoi(row),21:ncol(KIRCwMedian1)][grepl("[0-9]+",KIRCwMedian1[strtoi(row),21:ncol(KIRCwMedian1)])])
  KIRCwMedian2JustMedians <- as.double(KIRCwMedian2[strtoi(row),21:ncol(KIRCwMedian2)][grepl("[0-9]+",KIRCwMedian2[strtoi(row),21:ncol(KIRCwMedian2)])])
  
  KIRPwMedian1JustMedians <- as.double(KIRPwMedian1[strtoi(row),21:ncol(KIRPwMedian1)][grepl("[0-9]+",KIRPwMedian1[strtoi(row),21:ncol(KIRPwMedian1)])])
  KIRPwMedian2JustMedians <- as.double(KIRPwMedian2[strtoi(row),21:ncol(KIRPwMedian2)][grepl("[0-9]+",KIRPwMedian2[strtoi(row),21:ncol(KIRPwMedian2)])])
  
  LGGwMedian1JustMedians <- as.double(LGGwMedian1[strtoi(row),15:ncol(LGGwMedian1)][grepl("[0-9]+",LGGwMedian1[strtoi(row),15:ncol(LGGwMedian1)])])
  LGGwMedian2JustMedians <- as.double(LGGwMedian2[strtoi(row),15:ncol(LGGwMedian2)][grepl("[0-9]+",LGGwMedian2[strtoi(row),15:ncol(LGGwMedian2)])])
  
  LIHCwMedian1JustMedians <- as.double(LIHCwMedian1[strtoi(row),21:ncol(LIHCwMedian1)][grepl("[0-9]+",LIHCwMedian1[strtoi(row),21:ncol(LIHCwMedian1)])])
  LIHCwMedian2JustMedians <- as.double(LIHCwMedian2[strtoi(row),21:ncol(LIHCwMedian2)][grepl("[0-9]+",LIHCwMedian2[strtoi(row),21:ncol(LIHCwMedian2)])])
  
  LUADwMedian1JustMedians <- as.double(LUADwMedian1[strtoi(row),21:ncol(LUADwMedian1)][grepl("[0-9]+",LUADwMedian1[strtoi(row),21:ncol(LUADwMedian1)])])
  LUADwMedian2JustMedians <- as.double(LUADwMedian2[strtoi(row),21:ncol(LUADwMedian2)][grepl("[0-9]+",LUADwMedian2[strtoi(row),21:ncol(LUADwMedian2)])])
  
  LUSCwMedian1JustMedians <- as.double(LUSCwMedian1[strtoi(row),21:ncol(LUSCwMedian1)][grepl("[0-9]+",LUSCwMedian1[strtoi(row),21:ncol(LUSCwMedian1)])])
  LUSCwMedian2JustMedians <- as.double(LUSCwMedian2[strtoi(row),21:ncol(LUSCwMedian2)][grepl("[0-9]+",LUSCwMedian2[strtoi(row),21:ncol(LUSCwMedian2)])])
  
  MESOwMedian1JustMedians <- as.double(MESOwMedian1[strtoi(row),15:ncol(MESOwMedian1)][grepl("[0-9]+",MESOwMedian1[strtoi(row),15:ncol(MESOwMedian1)])])
  MESOwMedian2JustMedians <- as.double(MESOwMedian2[strtoi(row),15:ncol(MESOwMedian2)][grepl("[0-9]+",MESOwMedian2[strtoi(row),15:ncol(MESOwMedian2)])])
  
  OVwMedian1JustMedians <- as.double(OVwMedian1[strtoi(row),15:ncol(OVwMedian1)][grepl("[0-9]+",OVwMedian1[strtoi(row),15:ncol(OVwMedian1)])])
  OVwMedian2JustMedians <- as.double(OVwMedian2[strtoi(row),15:ncol(OVwMedian2)][grepl("[0-9]+",OVwMedian2[strtoi(row),15:ncol(OVwMedian2)])])
  
  PAADwMedian1JustMedians <- as.double(PAADwMedian1[strtoi(row),21:ncol(PAADwMedian1)][grepl("[0-9]+",PAADwMedian1[strtoi(row),21:ncol(PAADwMedian1)])])
  PAADwMedian2JustMedians <- as.double(PAADwMedian2[strtoi(row),21:ncol(PAADwMedian2)][grepl("[0-9]+",PAADwMedian2[strtoi(row),21:ncol(PAADwMedian2)])])
  
  PCPGwMedian1JustMedians <- as.double(PCPGwMedian1[strtoi(row),21:ncol(PCPGwMedian1)][grepl("[0-9]+",PCPGwMedian1[strtoi(row),21:ncol(PCPGwMedian1)])])
  PCPGwMedian2JustMedians <- as.double(PCPGwMedian2[strtoi(row),21:ncol(PCPGwMedian2)][grepl("[0-9]+",PCPGwMedian2[strtoi(row),21:ncol(PCPGwMedian2)])])
  
  PRADwMedian1JustMedians <- as.double(PRADwMedian1[strtoi(row),21:ncol(PRADwMedian1)][grepl("[0-9]+",PRADwMedian1[strtoi(row),21:ncol(PRADwMedian1)])])
  PRADwMedian2JustMedians <- as.double(PRADwMedian2[strtoi(row),21:ncol(PRADwMedian2)][grepl("[0-9]+",PRADwMedian2[strtoi(row),21:ncol(PRADwMedian2)])])
  
  READwMedian1JustMedians <- as.double(READwMedian1[strtoi(row),17:ncol(READwMedian1)][grepl("[0-9]+",READwMedian1[strtoi(row),17:ncol(READwMedian1)])])
  READwMedian2JustMedians <- as.double(READwMedian2[strtoi(row),17:ncol(READwMedian2)][grepl("[0-9]+",READwMedian2[strtoi(row),17:ncol(READwMedian2)])])
  
  SARCwMedian1JustMedians <- as.double(SARCwMedian1[strtoi(row),17:ncol(SARCwMedian1)][grepl("[0-9]+",SARCwMedian1[strtoi(row),17:ncol(SARCwMedian1)])])
  SARCwMedian2JustMedians <- as.double(SARCwMedian2[strtoi(row),17:ncol(SARCwMedian2)][grepl("[0-9]+",SARCwMedian2[strtoi(row),17:ncol(SARCwMedian2)])])
  
  SKCMwMedian1JustMedians <- as.double(SKCMwMedian1[strtoi(row),21:ncol(SKCMwMedian1)][grepl("[0-9]+",SKCMwMedian1[strtoi(row),21:ncol(SKCMwMedian1)])])
  SKCMwMedian2JustMedians <- as.double(SKCMwMedian2[strtoi(row),21:ncol(SKCMwMedian2)][grepl("[0-9]+",SKCMwMedian2[strtoi(row),21:ncol(SKCMwMedian2)])])
  
  STADwMedian1JustMedians <- as.double(STADwMedian1[strtoi(row),21:ncol(STADwMedian1)][grepl("[0-9]+",STADwMedian1[strtoi(row),21:ncol(STADwMedian1)])])
  STADwMedian2JustMedians <- as.double(STADwMedian2[strtoi(row),21:ncol(STADwMedian2)][grepl("[0-9]+",STADwMedian2[strtoi(row),21:ncol(STADwMedian2)])])
  
  TGCTwMedian1JustMedians <- as.double(TGCTwMedian1[strtoi(row),13:ncol(TGCTwMedian1)][grepl("[0-9]+",TGCTwMedian1[strtoi(row),13:ncol(TGCTwMedian1)])])
  TGCTwMedian2JustMedians <- as.double(TGCTwMedian2[strtoi(row),13:ncol(TGCTwMedian2)][grepl("[0-9]+",TGCTwMedian2[strtoi(row),13:ncol(TGCTwMedian2)])])
  
  THCAwMedian1JustMedians <- as.double(THCAwMedian1[strtoi(row),21:ncol(THCAwMedian1)][grepl("[0-9]+",THCAwMedian1[strtoi(row),21:ncol(THCAwMedian1)])])
  THCAwMedian2JustMedians <- as.double(THCAwMedian2[strtoi(row),21:ncol(THCAwMedian2)][grepl("[0-9]+",THCAwMedian2[strtoi(row),21:ncol(THCAwMedian2)])])
  
  THYMwMedian1JustMedians <- as.double(THYMwMedian1[strtoi(row),19:ncol(THYMwMedian1)][grepl("[0-9]+",THYMwMedian1[strtoi(row),19:ncol(THYMwMedian1)])])
  THYMwMedian2JustMedians <- as.double(THYMwMedian2[strtoi(row),19:ncol(THYMwMedian2)][grepl("[0-9]+",THYMwMedian2[strtoi(row),19:ncol(THYMwMedian2)])])
  
  UCECwMedian1JustMedians <- as.double(UCECwMedian1[strtoi(row),21:ncol(UCECwMedian1)][grepl("[0-9]+",UCECwMedian1[strtoi(row),21:ncol(UCECwMedian1)])])
  UCECwMedian2JustMedians <- as.double(UCECwMedian2[strtoi(row),21:ncol(UCECwMedian2)][grepl("[0-9]+",UCECwMedian2[strtoi(row),21:ncol(UCECwMedian2)])])
  
  UCSwMedian1JustMedians <- as.double(UCSwMedian1[strtoi(row),15:ncol(UCSwMedian1)][grepl("[0-9]+",UCSwMedian1[strtoi(row),15:ncol(UCSwMedian1)])])
  UCSwMedian2JustMedians <- as.double(UCSwMedian2[strtoi(row),15:ncol(UCSwMedian2)][grepl("[0-9]+",UCSwMedian2[strtoi(row),15:ncol(UCSwMedian2)])])
  
  UVMwMedian1JustMedians <- as.double(UVMwMedian1[strtoi(row),15:ncol(UVMwMedian1)][grepl("[0-9]+",UVMwMedian1[strtoi(row),15:ncol(UVMwMedian1)])])
  UVMwMedian2JustMedians <- as.double(UVMwMedian2[strtoi(row),15:ncol(UVMwMedian2)][grepl("[0-9]+",UVMwMedian2[strtoi(row),15:ncol(UVMwMedian2)])])
  
  
  #############################################################################################################################################


  ifelse(input$dataset=="ACC", data <-data.frame(
    name=c(rep("Both miRNA's Downregulated",length(ACCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(ACCwMedian2JustMedians))),
    value=c(ACCwMedian1JustMedians,ACCwMedian2JustMedians)
  ),
  
  ifelse(input$dataset=="BLCA", data <-data.frame(
    name=c(rep("Both miRNA's Downregulated",length(BLCAwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(BLCAwMedian2JustMedians))),
    value=c(BLCAwMedian1JustMedians,BLCAwMedian2JustMedians)
  ),
  
  # ifelse(input$dataset=="BRCA", data <-data.frame(
  #   name=c(rep("Both miRNA's Downregulated",length(BRCAwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(BRCAwMedian2JustMedians))),
  #   value=c(BRCAwMedian1JustMedians,BRCAwMedian2JustMedians)
  # ),
  # 
  ifelse(input$dataset=="CESC", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(CESCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(CESCwMedian2JustMedians))),
    value=c(CESCwMedian1JustMedians,CESCwMedian2JustMedians)
  ),
  ifelse(input$dataset=="CHOL", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(CHOLwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(CHOLwMedian2JustMedians))),
    value=c(CHOLwMedian1JustMedians,CHOLwMedian2JustMedians)
  ),
  ifelse(input$dataset=="COAD", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(COADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(COADwMedian2JustMedians))),
    value=c(COADwMedian1JustMedians,COADwMedian2JustMedians)
  ),
  ifelse(input$dataset=="DLBC", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(DLBCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(DLBCwMedian2JustMedians))),
    value=c(DLBCwMedian1JustMedians,DLBCwMedian2JustMedians)
  ),
  ifelse(input$dataset=="ESCA", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(ESCAwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(ESCAwMedian2JustMedians))),
    value=c(ESCAwMedian1JustMedians,ESCAwMedian2JustMedians)
  ),
  ifelse(input$dataset=="HNSC", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(HNSCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(HNSCwMedian2JustMedians))),
    value=c(HNSCwMedian1JustMedians,HNSCwMedian2JustMedians)
  ),
  ifelse(input$dataset=="KICH", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(KICHwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(KICHwMedian2JustMedians))),
    value=c(KICHwMedian1JustMedians,KICHwMedian2JustMedians)
  ),
  ifelse(input$dataset=="KIRC", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(KIRCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(KIRCwMedian2JustMedians))),
    value=c(KIRCwMedian1JustMedians,KIRCwMedian2JustMedians)
  ),
  ifelse(input$dataset=="KIRP", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(KIRPwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(KIRPwMedian2JustMedians))),
    value=c(KIRPwMedian1JustMedians,KIRPwMedian2JustMedians)
  ),
  ifelse(input$dataset=="LGG", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(LGGwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(LGGwMedian2JustMedians))),
    value=c(LGGwMedian1JustMedians,LGGwMedian2JustMedians)
  ),

  ifelse(input$dataset=="LIHC", data <- data.frame(
  name=c(rep("Both miRNA's Downregulated",length(LIHCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(LIHCwMedian2JustMedians))),
  value=c(LIHCwMedian1JustMedians,LIHCwMedian2JustMedians)
  ),

  ifelse(input$dataset=="LUAD", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(LUADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(LUADwMedian2JustMedians))),
    value=c(LUADwMedian1JustMedians,LUADwMedian2JustMedians)
  ),
  ifelse(input$dataset=="LUSC", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(LUSCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(LUSCwMedian2JustMedians))),
    value=c(LUSCwMedian1JustMedians,LUSCwMedian2JustMedians)
  ),
  ifelse(input$dataset=="MESO", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(MESOwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(MESOwMedian2JustMedians))),
    value=c(MESOwMedian1JustMedians,MESOwMedian2JustMedians)
  ),
  ifelse(input$dataset=="OV", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(OVwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(OVwMedian2JustMedians))),
    value=c(OVwMedian1JustMedians,OVwMedian2JustMedians)
  ),
  ifelse(input$dataset=="PAAD", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(PAADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(PAADwMedian2JustMedians))),
    value=c(PAADwMedian1JustMedians,PAADwMedian2JustMedians)
  ),
  ifelse(input$dataset=="PCPG", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(PCPGwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(PCPGwMedian2JustMedians))),
    value=c(PCPGwMedian1JustMedians,PCPGwMedian2JustMedians)
  ),
  ifelse(input$dataset=="PRAD", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(PRADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(PRADwMedian2JustMedians))),
    value=c(PRADwMedian1JustMedians,PRADwMedian2JustMedians)
  ),
  ifelse(input$dataset=="READ", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(READwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(READwMedian2JustMedians))),
    value=c(READwMedian1JustMedians,READwMedian2JustMedians)
  ),
  ifelse(input$dataset=="SARC", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(SARCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(SARCwMedian2JustMedians))),
    value=c(SARCwMedian1JustMedians,SARCwMedian2JustMedians)
  ),
  ifelse(input$dataset=="SKCM", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(SKCMwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(SKCMwMedian2JustMedians))),
    value=c(SKCMwMedian1JustMedians,SKCMwMedian2JustMedians)
  ),
  ifelse(input$dataset=="STAD", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(STADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(STADwMedian2JustMedians))),
    value=c(STADwMedian1JustMedians,STADwMedian2JustMedians)
  ),
  ifelse(input$dataset=="TGCT", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(TGCTwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(TGCTwMedian2JustMedians))),
    value=c(TGCTwMedian1JustMedians,TGCTwMedian2JustMedians)
  ),
  ifelse(input$dataset=="THCA", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(THCAwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(THCAwMedian2JustMedians))),
    value=c(THCAwMedian1JustMedians,THCAwMedian2JustMedians)
  ),
  ifelse(input$dataset=="THYM", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(THYMwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(THYMwMedian2JustMedians))),
    value=c(THYMwMedian1JustMedians,THYMwMedian2JustMedians)
  ),
  ifelse(input$dataset=="UCEC", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(UCECwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(UCECwMedian2JustMedians))),
    value=c(UCECwMedian1JustMedians,UCECwMedian2JustMedians)
  ),
  ifelse(input$dataset=="UCS", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(UCSwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(UCSwMedian2JustMedians))),
    value=c(UCSwMedian1JustMedians,UCSwMedian2JustMedians)
  ),
  ifelse(input$dataset=="UVM", data <- data.frame(
    name=c(rep("Both miRNA's Downregulated",length(UVMwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(UVMwMedian2JustMedians))),
    value=c(UVMwMedian1JustMedians,UVMwMedian2JustMedians)
  ),
  data <- data.frame(
    name=c(),
    value=c()
  )
  )))))))))))))))))))))))))))))
  )


  #############################################################################################################################################


  showModal(modalDialog(
    renderPlot({
      data %>%
        ggplot( aes(x=name,y=value, fill=name))+
        geom_boxplot()+
        geom_jitter(color="black", size=0.4, alpha=0.9)+
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 14,color="black"),
          axis.title.y = element_text(size = 14) #, face="bold")
        )+
        scale_x_discrete(labels=c("Both miRNA's\nDownregulated", "Both miRNA's\nUpregulated"))+
        scale_fill_manual(values=c("#99ccff", "#ff6666"))+
        # theme(axis.line = element_line(colour = "black", 
        #                                 size = 0.4, linetype = "solid"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black",size = 0.4, linetype = "solid"),
              axis.text.y = element_text(color="black", 
                                         size=12))+
        xlab("")+
        ylab("mRNA Expression Level")

    }),
    footer = tagList(
      modalButton("Close")
    ),
    easyClose = TRUE,
  ))
})


##################################################################      

sourceTargetInput <- reactive({
    switch (input$dataset,
            "ACC"=ACC_source_target,
            "BLCA" = BLCA_source_target,
            "CESC"=CESC_source_target,
            "CHOL"=CHOL_source_target,
            "COAD"=COAD_source_target,
            "DLBC"=DLBC_source_target,
            "ESCA"=ESCA_source_target,
            "HNSC"=HNSC_source_target,
            "KICH"=KICH_source_target,
            "KIRC"=KIRC_source_target,
            "KIRP"=KIRP_source_target,
            "LGG"=LGG_source_target,
            "LIHC"=LIHC_source_target,
            "LUAD"=LUAD_source_target,
            "LUSC"=LUSC_source_target,
            "MESO"=MESO_source_target,
            "OV"=OV_source_target,
            "PAAD"=PAAD_source_target,
            "PCPG"=PCPG_source_target,
            "PRAD"=PRAD_source_target,
            "READ"=READ_source_target,
            "SARC"=SARC_source_target,
            "SKCM"=SKCM_source_target,
            "STAD"=STAD_source_target,
            "TGCT"=TGCT_source_target,
            "THCA"=THCA_source_target,
            "THYM"=THYM_source_target,
            "UCEC"=UCEC_source_target,
            "UCS"=UCS_source_target,
            "UVM"=UVM_source_target
            
    )
})

nodeAttributeInput <- reactive({
    switch (input$dataset,
            "ACC" = ACC_node_attr,
            "BLCA" = BLCA_node_attr,
            "CESC"=CESC_node_attr,
            "CHOL"=CHOL_node_attr,
            "COAD"=COAD_node_attr,
            "DLBC"=DLBC_node_attr,
            "ESCA"=ESCA_node_attr,
            "HNSC"=HNSC_node_attr,
            "KICH"=KICH_node_attr,
            "KIRC"=KIRC_node_attr,
            "KIRP"=KIRP_node_attr,
            "LGG"=LGG_node_attr,
            "LIHC"=LIHC_node_attr,
            "LUAD"=LUAD_node_attr,
            "LUSC"=LUSC_node_attr,
            "MESO"=MESO_node_attr,
            "OV"=OV_node_attr,
            "PAAD"=PAAD_node_attr,
            "PCPG"=PCPG_node_attr,
            "PRAD"=PRAD_node_attr,
            "READ"=READ_node_attr,
            "SARC"=SARC_node_attr,
            "SKCM"=SKCM_node_attr,
            "STAD"=STAD_node_attr,
            "TGCT"=TGCT_node_attr,
            "THCA"=THCA_node_attr,
            "THYM"=THYM_node_attr,
            "UCEC"=UCEC_node_attr,
            "UCS"=UCS_node_attr,
            "UVM"=UVM_node_attr
    )
    
})

output$vNetwork <- renderVisNetwork({
  
  if(!is.null(DatasetRoundDigits())){
    combmi1mi2mrna <- unique(c(gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna1)),gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna2)),DatasetRoundDigits()$hgnc_symbol))
    orListForNetworkFiltering <- rep("|",length(combmi1mi2mrna))
    networkFilteringList <- paste(c(rbind(orListForNetworkFiltering, matrix(combmi1mi2mrna,ncol = length(orListForNetworkFiltering)))[-1]),collapse = '')
    sourceTargetFiltering <- paste(gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna1)),gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna2)),DatasetRoundDigits()$hgnc_symbol)
    
    splittedSourceTargetFiltering <- strsplit(sourceTargetFiltering,split = " ")
    splittedSourceTargetFilteringMRNA <- sapply(splittedSourceTargetFiltering,'[',3)
    splittedSourceTargetFilteringMIRNA1 <- sapply(splittedSourceTargetFiltering,'[',1)
    splittedSourceTargetFilteringMIRNA2 <- sapply(splittedSourceTargetFiltering,'[',2)
    splittedSourceTargetFilteringDUMMY1_2 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA1,"/",splittedSourceTargetFilteringMIRNA2))
    splittedSourceTargetFilteringDUMMY2_1 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA2,"/",splittedSourceTargetFilteringMIRNA1))
    
    for (i in 1:nrow(DatasetRoundDigits())){
      concated <- rbind(concated, filter(sourceTargetInput(), ((tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringMIRNA1[i]) & (tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY1_2[i]) |tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY2_1[i]) ))|
                                                                 (tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringMIRNA2[i]) & (tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY1_2[i]) | tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY2_1[i])))|
                                                                 ((tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringDUMMY1_2[i])| tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringDUMMY2_1[i])) & tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringMRNA[i]))
      )))
      
    }
    
    concatedUnique <- unique(concated[,c("source","target")])
    forNodeSharedName <- unique(c(concated$source,concated$target))
    forNodeName <- forNodeSharedName
    forNodeName[grepl("/",forNodeName)] <- " "
    intersectionSharedName <- intersect(filter(nodeAttributeInput(),stringr::str_detect(nodeAttributeInput()$shared.name,networkFilteringList))$shared.name,forNodeSharedName)
    

    nodes <- data.frame(id=intersectionSharedName, label=filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$name)
    edges <- data.frame(from= concatedUnique$source , to=concatedUnique$target)
    
    nodes$size <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mrna",25,ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="dummy",3,20))
    edges$color <- "rgb(153,153,153)"
    edges$length <- 3
    
    nodes$color.background <- "rgb(153,153,153)"
    nodes$color.border <- "rgb(153,153,153)"
    
    if(length(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown)) > 0){
      
      nodes$borderWidth <- ifelse(!is.na(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$significance) &
                                    filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$significance < 0.05, 3,1)
      
      nodes$color.border <- ifelse(!is.na(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$significance) &
                                     filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$significance < 0.05, "black","rgb(153,153,153)")
      
      nodes$color.background <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="up", "rgb(255,102,102)",
                                       ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="down","rgb(153,204,255)",
                                              "rgb(153,153,153)"))
      nodes$color.border <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="up", "rgb(255,102,102)",
                                   ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="down","rgb(153,204,255)",
                                          "rgb(153,153,153)"))
      
    }
    
    
    if(length(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$is_mrna_tf)) > 0){
      nodes$shape <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$is_mrna_tf)=="true","square",
                            ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mrna","diamond",
                                   ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mirna","dot",
                                          ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="dummy","dot","dot"))))
      
      
      nodes$size <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$is_mrna_tf)=="true",20,
                           ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mrna",25,
                                  ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mirna",20,
                                         ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="dummy",3,3))))
      
    }
    
    else {
      nodes$shape <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mrna","diamond",
                            ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mirna","dot",
                                   ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="dummy","dot","dot")))
    }
    
    
    if(length(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown)) > 0){ 
      lnodes <- data.frame(label=c("Legend","mRNA","mRNA is TF","miRNA","Up Regulated","Down Regulated","Significant"),
                           shape=c("text","diamond","square","dot","box","box","dot"),
                           size =c(25,25,20,25,25,15,25),
                           font.size=c(50,25,25,25,25,25,25),
                           font.face=c("Ubuntu","Ubuntu","Ubuntu","Ubuntu","Ubuntu","Ubuntu","Ubuntu"),
                           color.background=c("rgb(153,153,153)","rgb(153,153,153)",  "rgb(153,153,153)", "rgb(153,153,153)","rgb(255,102,102)","rgb(153,204,255)","white"),
                           borderWidth=c(1,1,1,1,1,1,2),
                           color.border=c("rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)","rgb(255,102,102)","rgb(153,204,255)","black")
      )
      
      
      
      visNetwork(nodes, edges) %>%
        visLegend(addNodes = lnodes,width = 0.1, position = "right",zoom=F,stepY = 180,useGroups = F)%>%visIgraphLayout()
    }
    else{
      lnodes <- data.frame(label=c("Legend","mRNA","mRNA is TF","miRNA"),
                           shape=c("text","diamond","square","dot"),
                           size =c(25,30,30,25),
                           font.size=c(50,25,25,25),
                           font.face=c("Ubuntu","Ubuntu","Ubuntu","Ubuntu"),
                           color.background=c("rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)"),
                           color.border=c("rgb(153,153,153)", "rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)")
      )
      visNetwork(nodes, edges) %>%
        visLegend(addNodes = lnodes,width = 0.1, position = "right",zoom=F,stepY = 120,useGroups = F)%>%visIgraphLayout()
    }
    
  }
    
    
    
})
##################################################################################################

output$commonTripletNetwork <- renderVisNetwork({
  
  if(!is.null(commonTripletFilter()) || nrow(commonTripletFilter()) >0){
    networkFilteringSplit <- unique(unlist(strsplit(commonTripletFilter()$Triplet, split = "/")))
    orListForNetworkFilteringSplit<- rep("|",length(networkFilteringSplit))
    networkFilteringList <- paste(c(rbind(orListForNetworkFilteringSplit, matrix(networkFilteringSplit,ncol = length(orListForNetworkFilteringSplit)))[-1]),collapse = '')
    
    
    splittedMrnaFilteredSource <- strsplit(commonTripletFilter()$Triplet, split = "/")
    splittedSourceTargetFilteringMRNA <- sapply(splittedMrnaFilteredSource,'[',3)
    splittedSourceTargetFilteringMIRNA1 <- sapply(splittedMrnaFilteredSource,'[',1)
    splittedSourceTargetFilteringMIRNA2 <- sapply(splittedMrnaFilteredSource,'[',2)
    splittedSourceTargetFilteringDUMMY1_2 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA1,"/",splittedSourceTargetFilteringMIRNA2))
    splittedSourceTargetFilteringDUMMY2_1 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA2,"/",splittedSourceTargetFilteringMIRNA1))
    
    
    for (i in 1:nrow(commonTripletFilter())){
      concated <- rbind(concated, filter(commonTriplet_source_target, ((commonTriplet_source_target$source==splittedSourceTargetFilteringMIRNA1[i] & (commonTriplet_source_target$target == splittedSourceTargetFilteringDUMMY1_2[i] |commonTriplet_source_target$target == splittedSourceTargetFilteringDUMMY2_1[i] ))|
                                                                         (commonTriplet_source_target$source==splittedSourceTargetFilteringMIRNA2[i] & (commonTriplet_source_target$target == splittedSourceTargetFilteringDUMMY1_2[i] | commonTriplet_source_target$target == splittedSourceTargetFilteringDUMMY2_1[i]))|
                                                                         ((commonTriplet_source_target$source==splittedSourceTargetFilteringDUMMY1_2[i]| commonTriplet_source_target$source==splittedSourceTargetFilteringDUMMY2_1[i]) & commonTriplet_source_target$target == splittedSourceTargetFilteringMRNA[i])
      )))
      
    }
    
    
    concatedUnique <- unique(concated[,c("source","target","whichcancer")])
    forNodeSharedName <- unique(c(concated$source,concated$target))
    forNodeName <- forNodeSharedName
    forNodeName[grepl("/",forNodeName)] <- " "
    intersectionSharedName <- intersect(filter(commonTriplets_node_attr,stringr::str_detect(commonTriplets_node_attr$shared.name,networkFilteringList))$shared.name,forNodeSharedName)
    
    
    nodes <- data.frame(id=intersectionSharedName, label=filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$visname)
    edges <- data.frame(from = concatedUnique$source , to = concatedUnique$target, label = concatedUnique$whichcancer )
    
    
    nodes$shape <- ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="mrna","diamond",
                          ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="mirna","dot",
                                 ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="dummy","dot","dot")))
    
    nodes$size <- ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="mrna",50,
                         ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="mirna",40,
                                ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="dummy",8,3)))
    
    
    nodes$color.background <- "rgb(153,153,153)"
    nodes$color.border <- "rgb(153,153,153)"

    edges$color <- "rgb(153,153,153)"
    edges$length <- 15

    visNetwork(nodes, edges)%>%
      visIgraphLayout()%>% 
      visExport(type = "pdf", name = "export-network",
                                    float = "left", label = "Save network", background = "white", style= "")%>% 
      visEdges(font = list(align="horizontal", color="black", size=30, face="Ubuntu"))%>%
      visNodes(font = list(color="black", size=40, face="Ubuntu"))
    
  }
  else{
    NULL
  }
  
  
  
})


output$commonMirnaNetwork <- renderVisNetwork({
  
  nodeList <- data.frame("name"= unique(c(commonMirnaPair_source_target$source,commonMirnaPair_source_target$target)))
  
  if(length(input$CommonMirnaPairCancer) >0 ){
    orListForCommonCancer <- rep("|",length(input$CommonMirnaPairCancer))
    cancerListForCommonTripletAndPair <- paste(c(rbind(orListForCommonCancer, matrix(input$CommonMirnaPairCancer,ncol = length(orListForCommonCancer)))[-1]),collapse = '')
    
    filteredWithCancer <- commonMirnaPair_source_target%>%
      filter(stringr::str_detect(CancerTypes,cancerListForCommonTripletAndPair))
    combmi1mi2 <- unique(c(filteredWithCancer$source,filteredWithCancer$target))
    orListForCombmi1mi2 <- rep("|",length(combmi1mi2))
    mirnaListForCombmi1mi2 <- paste(c(rbind(orListForCombmi1mi2, matrix(combmi1mi2,ncol = length(orListForCombmi1mi2)))[-1]),collapse = '')
    filteredWithCancerLabel <- nodeList%>%
      filter(stringr::str_detect(name,mirnaListForCombmi1mi2))
    
    
  }
  else{
    filteredWithCancer <- NULL
    filteredWithCancerLabel <-NULL
    
  }
  
  if(length(input$mirnaCommonMirnaPair) >0){
    
    orListForMirna <- rep("|",length(input$mirnaCommonMirnaPair))
    mirnaList <- paste(c(rbind(orListForMirna, matrix(input$mirnaCommonMirnaPair,ncol = length(orListForMirna)))[-1]),collapse = '')
    filteredWithMirnaSource <- filteredWithCancer%>%
      filter(stringr::str_detect(source,mirnaList))
    filteredWithMirnaTarget <- filteredWithCancer%>%
      filter(stringr::str_detect(target,mirnaList))
    
    concat <- distinct(rbind(filteredWithMirnaSource, filteredWithMirnaTarget))

    combmi1mi2 <- unique(c(concat$source,concat$target))
    orListForCombmi1mi2 <- rep("|",length(combmi1mi2))
    mirnaListForCombmi1mi2 <- paste(c(rbind(orListForCombmi1mi2, matrix(combmi1mi2,ncol = length(orListForCombmi1mi2)))[-1]),collapse = '')
    filteredWithMirnaLabel <- filteredWithCancerLabel%>%
      filter(stringr::str_detect(name,mirnaListForCombmi1mi2))
    
  }
  else{
    concat <- filteredWithCancer
    filteredWithMirnaLabel <- filteredWithCancerLabel
  }
  
  
  nodes <- data.frame(id=filteredWithMirnaLabel$name, label=filteredWithMirnaLabel$name)
  edges <- data.frame(from= concat$source , to=concat$target, label=concat$CancerTypes, font.size =8, length=50)
  
  nodes$shape <- "dot"
  nodes$size <- 15
  nodes$color.background <- "rgb(153,153,153)"
  nodes$color.border <- "rgb(153,153,153)"
  
  edges$color <- "rgb(153,153,153)"
  edges$length <- 10
  visNetwork(nodes, edges)%>%
    visIgraphLayout() 
 
  
})

##################################################################################################    

commonTripletFilter <- reactive({
  
  dataset <- TripletsInWhichCancerWCount
  filteredWithMrna <- NULL
  filteredWithMirna <- NULL
  concated <-  NULL
  
  if(length(input$mrnaCommonTriplet) == 0 && length(input$mirnaCommonTriplet) ==0 ){
    concated <-  dataset
  }
  if(length(input$mrnaCommonTriplet) >0){
    orListForMrna <- rep("|",length(input$mrnaCommonTriplet))
    mrnaList <- paste(c(rbind(orListForMrna, matrix(input$mrnaCommonTriplet,ncol = length(orListForMrna)))[-1]),collapse = '')
    filteredWithMrna <- dataset%>%
      filter(stringr::str_detect(Triplet,mrnaList))
    
  }
  if(length(input$mirnaCommonTriplet) >0){
    orListForMirna <- rep("|",length(input$mirnaCommonTriplet))
    mirnaList <- paste(c(rbind(orListForMirna, matrix(input$mirnaCommonTriplet,ncol = length(orListForMirna)))[-1]),collapse = '')
    filteredWithMirna <- dataset%>%
      filter(stringr::str_detect(Triplet,mirnaList))
    
  }
  if(length(input$mrnaCommonTriplet) != 0 || length(input$mirnaCommonTriplet) !=0 ){
    concated <- distinct(rbind(filteredWithMrna, filteredWithMirna))
  }
  
  if(length(input$CommonTripletCancer) >0 ){
    orListForCommonCancer <- rep("|",length(input$CommonTripletCancer))
    cancerListForCommonTripletAndPair <- paste(c(rbind(orListForCommonCancer, matrix(input$CommonTripletCancer,ncol = length(orListForCommonCancer)))[-1]),collapse = '')
    
    filteredWithCancer <- concated%>%
      filter(stringr::str_detect(CancerTypes,cancerListForCommonTripletAndPair))
    
  }
  else{
    filteredWithCancer <- NULL
    
  }
  return(filteredWithCancer)
  
})

commonMirnaPairFilter <- reactive({
  dataset <-MirnaPairsInWhichCancerWCount
  
  
  if(length(input$mirnaCommonMirnaPair) >0){
    orListForMirna <- rep("|",length(input$mirnaCommonMirnaPair))
    mirnaList <- paste(c(rbind(orListForMirna, matrix(input$mirnaCommonMirnaPair,ncol = length(orListForMirna)))[-1]),collapse = '')
    filteredWithMirna <- dataset%>%
      filter(stringr::str_detect(miRNAPair,mirnaList))
    
  }
  else{
    filteredWithMirna <- dataset
    
  }
  
  if(length(input$CommonMirnaPairCancer) >0 ){
    orListForCommonCancer <- rep("|",length(input$CommonMirnaPairCancer))
    cancerListForCommonTripletAndPair <- paste(c(rbind(orListForCommonCancer, matrix(input$CommonMirnaPairCancer,ncol = length(orListForCommonCancer)))[-1]),collapse = '')
    
    filteredWithCancer <- filteredWithMirna%>%
      filter(stringr::str_detect(CancerTypes,cancerListForCommonTripletAndPair))
    
  }
  else{
    filteredWithCancer <- NULL
    
  }
  
  if(nrow(filteredWithCancer) >0 & !is.null(filteredWithCancer)){
   orListForCommonMirnaPairCancerCount <- rep("|",length(input$CommonMirnaPairCancerCount))
   countListForCommonTripletAndPair <- paste(c(rbind(orListForCommonMirnaPairCancerCount, matrix(input$CommonMirnaPairCancerCount,ncol = length(orListForCommonMirnaPairCancerCount)))[-1]),collapse = '')
   filteredWithCount <- filteredWithMirna%>%
     filter(stringr::str_detect(Count,countListForCommonTripletAndPair))
    
  }
  else{
    filteredWithCount <- NULL
  }
  
})


output$tableCommonTriplet <- DT::renderDataTable({
    
  
    DT::datatable(commonTripletFilter(),
                  options = list(
                    columnDefs = list(
                      list(className = "dt-center", targets = "_all")
                    )
                  ))
    
  
    
    
})

output$tableCommonmiRNAPair <- DT::renderDataTable({
  
  
  
  
    DT::datatable(commonMirnaPairFilter(),
                  options = list(
                      columnDefs = list(
                          list(className = "dt-center", targets = "_all")
                      )
                  ))
})

##################################################################################################    

output$TCGAAbbrv <- renderTable({
    TCGA_abbreviations
}, sanitize.text.function = function(x) x)

output$Glossary <- renderTable({
  Glossary
}, sanitize.text.function = function(x) x)

##################################################################################################    


output$totalCountsPlot <- renderPlotly({
  
  t <- list(
    family = "Ubuntu",
    size = 13)
  
  fig <- plot_ly(miRCoopTotalCounts, x = ~CancerType, y = ~N.Triplets, type = 'bar', name = '# Triplets', marker = list(color = 'rgb(7,68,135)'))
  fig <- fig %>% add_trace(y = ~N.miRNAPairs, name = '# miRNA Pairs', marker = list(color = 'rgb(251,85,100)'))
  fig <- fig %>% add_trace(y = ~N.miRNAs, name = '# miRNAs',marker = list(color = 'rgb(255,166,0)'))
  fig <- fig %>% add_trace(y = ~N.mRNAs, name = '# mRNAs', marker = list(color = 'rgb(165,67,149)'))
  fig <- fig %>% layout(title = 'Counts Across All Cancer Types',yaxis = list(title = 'Count'), barmode = 'group', xaxis = list(title = 'Cancer Type'),font=t)
  
})

output$MrnaScatterPlot <- renderPlotly({
  
  fig <- plot_ly(
    mRNACountsScatter,
    y = ~mRNAinTriplets,
    x = ~TargetInteractionsofthemRNA, 
    marker = list(color="black",size=5),
    text = ~paste('mRNA:', mRNA)
  ) %>% layout(yaxis = list(title = '# mRNA in Triplets'), 
               xaxis = list(title = '# Target Interactions of the mRNA'),font=t1)
  
})
t1 <- list(
  family = "Ubuntu",
  size = 15)
output$MirnaScatterPlot <- renderPlotly({
  
  fig <- plot_ly(
    miRNACountsScatter, 
    y = ~miRNAinTriplets,
    x = ~miRNATargets,
    marker = list(color="black",size=5),
    text = ~paste('miRNA:', miRNA)
  ) %>% layout(yaxis = list(title = '# miRNA in Triplets'), 
               xaxis = list(title = '# miRNA Targets'),font=t1)
})

output$commonMrnaHeatmap <- renderPlotly({

  p <- heatmaply::heatmaply(as.matrix(as.data.table(commonMrnaAbove20),rownames = 1),
                       margins = c(0,0,50,0),
                       grid_color = "white",
                       grid_width = 0.0001,
                       fontsize_row = 8, fontsize_col = 8,
                       branches_lwd = 0.08,
                       xlab = "Cancer Type", ylab = "mRNA",
                       #color= colorRampPalette(brewer.pal(3, "Greys"))(256),
                       scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                         low = "white",
                         high = "black"
                       ))%>% layout(title = list(text=' Most frequent mRNAs across all cancers'))
  


})

output$commonMirnaHeatmap <- renderPlotly({
  
  p <- heatmaply::heatmaply(as.matrix(as.data.table(commonMirnaAbove50),rownames = 1),
                            margins = c(5,5,50,0),
                            grid_color = "white",
                            grid_width = 0.0001,
                            #plot_method= "plotly",
                            fontsize_row = 8, fontsize_col = 8,
                            branches_lwd = 0.08,
                            xlab = "Cancer Type", ylab = "miRNA",
                            #color= colorRampPalette(brewer.pal(3, "Greys"))(256)
                            scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                              low = "white",
                              high = "black"
                            ))%>% layout(title = ' Most frequent miRNAs across all cancers')
  
  
  
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$dataset, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(DatasetRoundDigits(), file, row.names = FALSE)
  })


}
##################################################################################################    




# Run the application 
shinyApp(ui = ui, server = server)
