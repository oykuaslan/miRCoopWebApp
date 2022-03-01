
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
library(car)
library(shinyalert)
library(shinycssloaders)
library(shinycustomloader)
library(sqldf)
library(readr)
library(ggplot2)
library(formattable)
library(viridis)
library(shinyBS)
library(purrr)
library(bslib)
library(formattable)
library(spsComps)
library(bsplus)
library(readr)



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

ACC_allData <- read_csv("finalDTData/ACC.csv",show_col_types = FALSE)
ACC <- ACC_allData[,c("entrezgene_id","hgnc_symbol","mirna1","mirna2","Lancaster_XY_Z","is_mrna_tf","mirna1Literature","mirna2Literature","mrnaLiterature","miRNA1_mRNA_DB","miRNA2_mRNA_DB")]

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

####################################################################################################

TripletsInWhichCancerWCount<- read_csv("dataset/commonTripletsFinalAfterReRun.csv")
MirnaPairsInWhichCancerWCount <- read_csv("dataset/commonMirnaPairsFinalAfterReRun.csv")

TCGA_abbreviations <- read.table("dataset/TCGA_abbreviations.csv", header = T, sep = ";")
miRCoopTotalCounts <- read_delim("dataset/stats/miRCoopTotalCounts.csv", 
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
    base_font = c("Ubuntu"),
    heading_font = c("Ubuntu"),
    code_font = c("Ubuntu"),
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
    
    
    tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #179E93 !important;}')),
    
    setSliderColor(c("#9F9F9F", "#9F9F9F", "#9F9F9F", "#9F9F9F", "#9F9F9F", "#9F9F9F"), c(1, 2, 3, 4, 5, 6)), #depends on the number of sliders, change if you add or remove sliders.
    
    navbarPage("miRCoop",id = "mirCoop", theme = bslib_sabanci20_theme, 
               tabPanel('Home', icon = icon("home"),
                        includeHTML("miRCoop_Intro.html"), br(), br(), 
                        fluidRow(
                            column(6, align = "center", #column width 
                                   actionLink("switch_triplets_tab", icon = icon("globe", "fa-5x"), br(),br(),
                                              "Triplets") ,
                                   tags$label(tags$style(HTML('#switch_triplets_tab{color:black; font-size: 18px;}'))),
                                   shinyBS::bsTooltip("switch_triplets_tab", "Shows results with the specific p-values.", placement = "top", trigger = "hover", options = NULL),
                                   tags$hr(),
                                   br(),br()),
                            column(6,align = "center", #column width
                                   actionLink("switch_commontriplets_tab",icon = icon("th-list", "fa-5x"), br(),br(),
                                              "Common Triplets"),
                                   tags$head(tags$style(HTML("#show{background-color: darkslategray2;display:block;height: 25px;width: 25px;border-radius: 50%;border:}"))),
                                   shinyBS::bsTooltip("switch_commontriplets_tab", "Common triplets found in more than one cancer.", placement = "top", trigger = "hover", options = NULL),
                                   tags$hr(),
                                   tags$label(tags$style(HTML('#switch_commontriplets_tab{color:black; font-size: 18px;}'))),
                                   br(),br()),
                            br(),br(),
                        )),
               tabPanel("Triplets",value="Triplets",fluid = TRUE, icon = icon("globe"),
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
                                         selectizeInput("mrnaFilter", "mRNA Filter", choices=NULL,multiple=TRUE),
                                         br(),
                                         selectizeInput("mirnaFilter", "miRNA Filter", choices=NULL,multiple=TRUE),
                                         hr(),
                                         

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
               
               
               tabPanel("Common Triplets", value="CommonTriplets",fluid = TRUE,icon = icon("file-alt"),
                        sidebarLayout(
                            sidebarPanel(
                                checkboxGroupInput(inputId = "CommonCancer", 
                                                   label = "Select Cancer Names",
                                                   choices = cancerNames,
                                                   selected=cancerNames),
                                hr(),
                              
                                
                            ),
                            
                            mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Common Triplet Table", DT::dataTableOutput("tableCommonTriplet")),
                                            tabPanel("Common miRNA Pair Table", DT::dataTableOutput("tableCommonmiRNAPair"))
                                            
                                )
                            )
                            
                        )
               ),
               
               tabPanel("Statistics", value = "Statistics", fluid= TRUE, icon = icon("bar-chart",lib = "font-awesome"),
                        tabsetPanel(type = "tabs",
                                    tabPanel("Counts", br()
                                             ),
                                    
                                    tabPanel("Heatmaps")
                        )
                        ),
               
               tabPanel("About",value="About",fluid = TRUE,icon = icon("info-circle"),
                        fluidRow(
                            column (6,
                                    h4(p("About miRCoop")),
                                    h5(p(align = "justify;",style="font-size:16px;","miRCoop identifies synergistic miRNA pairs which have weak or no repression on the target mRNA, but when bound together induce strong repression of their target's expression. MiRCoop integrates miRNA-mRNA target interactions with kernel interaction tests on gene expression profiles of the miRNA and mRNAs. MirCoop has three main steps. In the first one, for every mRNA, it curates a list of miRNAs that can target the mRNA. To this end, we use the existing miRNA-mRNA prediction algorithms and datasets: miRTarBase, miRecords, Tarbase and Targetscan algorithm. We eliminate miRNA pairs if they have overlapping binding sites on the mRNA. These form our list of possible miRCoop triplets. In the next step of the analysis, we expect that if both miRNAs are upregulated, the mRNA expression will be lower compared to the case where both miRNAs are down-regulated and we apply a filter based on the expression profiles of the miRNAs and mRNAs. In the third step, for every triplet in our list, miRCoop conducts interaction tests based on gene expression levels of the triplet constituents. Here, we assume that miRNAs and mRNA are random variables and we conduct Lancaster based three variable interaction test which test for dependence among three random variables.")
                                       ),
                                    tags$img(src="miRCoopSteps.png",style="width:100%; height:auto; max-width:50vw"),
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

                            column(6, 
                                   h4(p("Abbreviations and Full Names of TCGA Projects")),
                                   tags$div(style="font-size:16px;",tableOutput("TCGAAbbrv"))
                            )
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
        updateTabsetPanel(session, "mirCoop",selected = "Triplets")
    })
    
    observeEvent(input$switch_commontriplets_tab, {
        updateTabsetPanel(session, "mirCoop",selected = "CommonTriplets")
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
        mirnaList <- union_all(datasetInput()$mirna1,datasetInput()$mirna2)
        updateSelectizeInput(session,"mirnaFilter",choices = unique(mirnaList))
    })
    
    
    datasetInput2 <- reactive({
        
        filteredWithTests <-filter(datasetInput(),
                                   Lancaster_XY_Z >=input$Lancaster_XY_Z_range[1], Lancaster_XY_Z <=input$Lancaster_XY_Z_range[2],
                                   tolower(is_mrna_tf) %in% tolower(input$is_mrna_tf)
        )
        if(length(input$mrnaFilter) > 0){
            filteredWithMrna <-filter(filteredWithTests,hgnc_symbol %in% input$mrnaFilter)
        }
        else{
            filteredWithMrna <- filteredWithTests
        }
        
        if(length(input$mirnaFilter) > 0){
            filteredWithMirna <-filter(filteredWithMrna, mirna1 %in% input$mirnaFilter | mirna2 %in% input$mirnaFilter)
        }
        
        else{
            filteredWithMirna <- filteredWithMrna
        }
        
    })
    
    
    DatasetRoundDigits <-reactive({
        dataset <-datasetInput2()
        dataset$mirna1 <- stringr::str_remove(dataset$mirna1, "hsa-")
        dataset$mirna2 <- stringr::str_remove(dataset$mirna2, "hsa-")
        dataset %>% 
            dplyr::mutate(across(where(is.numeric),round,3))
        
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
                '<button id="button_%s_%d" type="button" onclick="%s">Box Plot</button>', tbl,
                i, "Shiny.setInputValue('button', this.id);")
        }
    }
    

output$table <- DT::renderDataTable({
    print(input$dataset)
    DT1 <- DatasetRoundDigits()
    DT <- cbind(DT1,
                button = sapply(1:nrow(DT1), button("table")),
                stringsAsFactors = FALSE)
    
    DT2<- cbind(DT1,
                button = sapply(1:nrow(DT1), button("table")),
                stringsAsFactors = FALSE)
    

    hideList1 <- c(7,8,9,10,11)
    hideList2 <- c(7,8,9,10,11,12,13,14,15,16,17)
    hideList3 <- c(7,8,9,10,11,12,13)
    hideList4 <- c(7,8,9)
    hideList5 <-c(7,8,9,10,11,12,13,14,15)
    hideList6 <-c(7,8,9,10,11,12)
    
    
    ifelse(input$dataset=="ACC", columnHideList <-hideList1,
    ifelse(input$dataset=="BLCA", columnHideList <-hideList2,
    ifelse(input$dataset=="CESC", columnHideList <-hideList2,
    ifelse(input$dataset=="CHOL", columnHideList <-hideList2,
    ifelse(input$dataset=="COAD", columnHideList <-hideList3,
    ifelse(input$dataset=="DLBC", columnHideList <-hideList1,
    ifelse(input$dataset=="ESCA", columnHideList <-hideList2,
    ifelse(input$dataset=="HNSC", columnHideList <-hideList2,
    ifelse(input$dataset=="KICH", columnHideList <-hideList2,
    ifelse(input$dataset=="KIRC", columnHideList <-hideList2,
    ifelse(input$dataset=="KIRP", columnHideList <-hideList2,
    ifelse(input$dataset=="LGG", columnHideList <-hideList1,
    ifelse(input$dataset=="LIHC", columnHideList <-hideList2,
    ifelse(input$dataset=="LUAD", columnHideList <-hideList2,
    ifelse(input$dataset=="LUSC", columnHideList <-hideList2,
    ifelse(input$dataset=="MESO", columnHideList <-hideList1,
    ifelse(input$dataset=="OV", columnHideList <-hideList1,
    ifelse(input$dataset=="PAAD", columnHideList <-hideList2,
    ifelse(input$dataset=="PCPG",columnHideList <-hideList2,
    ifelse(input$dataset=="PRAD", columnHideList <-hideList2,
    ifelse(input$dataset=="READ", columnHideList <-hideList3,
    ifelse(input$dataset=="SARC", columnHideList <-hideList3,
    ifelse(input$dataset=="SKCM", columnHideList <-hideList2,
    ifelse(input$dataset=="STAD", columnHideList <-hideList2,
    ifelse(input$dataset=="TGCT", columnHideList <-hideList4,
    ifelse(input$dataset=="THCA", columnHideList <-hideList2,
    ifelse(input$dataset=="THYM", columnHideList <-hideList5,
    ifelse(input$dataset=="UCEC", columnHideList <-hideList2,
    ifelse(input$dataset=="UCS", columnHideList <-hideList6,
    ifelse(input$dataset=="UVM", columnHideList <-hideList6,columnHideList <-c()))))))))))))))))))))))))))))))

    tripletvalue <- tags$span(
        "Triplet pvalue",
        infoBtn('notWorking')%>%
            spsComps::bsTooltip(title = "Kernel Three-Variable Lancaster Interaction Test.",
                                placement = "top",
                                trigger = "hover")
            
    ) %>% as.character()
    
  
    
    
    nameList1 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2",tripletvalue,"is mRNA TF" ,"miRNA1 Literature","miRNA2 Literature", "mRNA Literature","miRNA1-mRNA Database","miRNA2-mRNA Database","miRNA-mRNA Expressions")
    nameList2 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2", tripletvalue, " is mRNA TF","miRNA1 Literature","miRNA2 Literature", "mRNA Literature","miRNA1-mRNA Database","miRNA2-mRNA Database","miRNA1 pvalue","miRNA1 LogFC","miRNA2 pvalue","miRNA2 LogFC","mRNA pvalue","mRNA LogFC","miRNA-mRNA Expressions")
    nameList3 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2",tripletvalue, " is mRNA TF","miRNA1 Literature","miRNA2 Literature", "mRNA Literature","miRNA1-mRNA Database","miRNA2-mRNA Database","mRNA pvalue","mRNA LogFC","miRNA-mRNA Expressions" )
    nameList4 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2",tripletvalue, " is mRNA TF", "mRNA Literature", "miRNA1-mRNA Database","miRNA2-mRNA Database", "miRNA-mRNA Expressions")
    nameList5 <- c("Entrez ID", "HGNC Symbol","miRNA1", "miRNA2",tripletvalue, " is mRNA TF", "mRNA Literature", "miRNA1-mRNA Database","miRNA2-mRNA Database", "miRNA1 pvalue","miRNA1 LogFC","miRNA2 pvalue","miRNA2 LogFC","mRNA pvalue","mRNA LogFC","miRNA-mRNA Expressions")
    
    ifelse(input$dataset=="ACC", columnNameList <-nameList1,
    ifelse(input$dataset=="BLCA", columnNameList <-nameList2,
    ifelse(input$dataset=="CESC", columnNameList <-nameList2,
    ifelse(input$dataset=="CHOL", columnNameList <-nameList2,
    ifelse(input$dataset=="COAD", columnNameList <-nameList3,
    ifelse(input$dataset=="DLBC", columnNameList <-nameList1,
    ifelse(input$dataset=="ESCA", columnNameList <-nameList2,
    ifelse(input$dataset=="HNSC", columnNameList <-nameList2,
    ifelse(input$dataset=="KICH", columnNameList <-nameList2,
    ifelse(input$dataset=="KIRC", columnNameList <-nameList2,
    ifelse(input$dataset=="KIRP", columnNameList <-nameList2,
    ifelse(input$dataset=="LGG", columnNameList <-nameList1,
    ifelse(input$dataset=="LIHC", columnNameList <-nameList2,
    ifelse(input$dataset=="LUAD", columnNameList <-nameList2,
    ifelse(input$dataset=="LUSC", columnNameList <-nameList2,
    ifelse(input$dataset=="MESO", columnNameList <-nameList1,
    ifelse(input$dataset=="OV", columnNameList <-nameList1,
    ifelse(input$dataset=="PAAD", columnNameList <-nameList2,
    ifelse(input$dataset=="PCPG",columnNameList <-nameList2,
    ifelse(input$dataset=="PRAD", columnNameList <-nameList2,
    ifelse(input$dataset=="READ", columnNameList <-nameList3,
    ifelse(input$dataset=="SARC", columnNameList <-nameList3,
    ifelse(input$dataset=="SKCM", columnNameList <-nameList2,
    ifelse(input$dataset=="STAD", columnNameList <-nameList2,
    ifelse(input$dataset=="TGCT", columnNameList <-nameList4,
    ifelse(input$dataset=="THCA", columnNameList <-nameList2,
    ifelse(input$dataset=="THYM", columnNameList <-nameList5,
    ifelse(input$dataset=="UCEC", columnNameList <-nameList2,
    ifelse(input$dataset=="UCS", columnNameList <-nameList1,
    ifelse(input$dataset=="UVM", columnNameList <-nameList1,
    columnNameList <-c()))))))))))))))))))))))))))))))
    
    icon_formatter <- function() {
        formatter("span", 
                  style = x ~ style(color = ifelse(x, "#179E93", "red")), x ~ icontext(ifelse(x, "ok", "remove"), "")
        )	 	 
    }
    
    significant_bold <- formatter("span", 
                                style = x ~ style("font-weight" = ifelse(x <0.05, "bold", NA)))
    
    sign_formatter <- formatter("span", 
                                style = x ~ style(color = ifelse(x > 0, "red", 
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
                   columnDefs = list(list(visible=FALSE, targets=columnHideList)
                   )))
    
    # DT::datatable(DT  %>% rename(!!hp_text:=mirna1),escape = F, fillContainer = TRUE,
    #               rownames=T,
    #               colnames=columnNameList,
    #               extensions = 'Buttons',
    #               options = list(dom = 'Bfrtip',
    #                              buttons=list(list(extend = 'colvis', columns = c(5:ncol(DT)))),
    #                              columnDefs = list(list(visible=FALSE, targets=columnHideList))
    #                              #headerCallback = JS(headerCallback)
    #                              
    #                              )
    # )

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
            #"LIHC"=LIHC_source_target,
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
            #"BLCA" = BLCA_node_attr,
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
            #"LIHC"=LIHC_node_attr,
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
    
    combmi1mi2mrna <- unique(c(gsub("hsa-", "",DatasetRoundDigits()$mirna1),gsub("hsa-", "",DatasetRoundDigits()$mirna2),DatasetRoundDigits()$hgnc_symbol))
    orListForNetworkFiltering <- rep("|",length(combmi1mi2mrna))
    networkFilteringList <- paste(c(rbind(orListForNetworkFiltering, matrix(combmi1mi2mrna,ncol = length(orListForNetworkFiltering)))[-1]),collapse = '')
    sourceTargetFiltering <- paste(gsub("hsa-", "",DatasetRoundDigits()$mirna1),gsub("hsa-", "",DatasetRoundDigits()$mirna2),DatasetRoundDigits()$hgnc_symbol)
    
    splittedSourceTargetFiltering <- strsplit(sourceTargetFiltering,split = " ")
    splittedSourceTargetFilteringMRNA <- sapply(splittedSourceTargetFiltering,'[',3)
    splittedSourceTargetFilteringMIRNA1 <- sapply(splittedSourceTargetFiltering,'[',1)
    splittedSourceTargetFilteringMIRNA2 <- sapply(splittedSourceTargetFiltering,'[',2)
    splittedSourceTargetFilteringDUMMY1_2 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA1,"/",splittedSourceTargetFilteringMIRNA2))
    splittedSourceTargetFilteringDUMMY2_1 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA2,"/",splittedSourceTargetFilteringMIRNA1))

    
    for (i in 1:nrow(DatasetRoundDigits())){
        concated <- rbind(concated, filter(sourceTargetInput(), ((sourceTargetInput()$source==splittedSourceTargetFilteringMIRNA1[i] & (sourceTargetInput()$target == splittedSourceTargetFilteringDUMMY1_2[i] |sourceTargetInput()$target == splittedSourceTargetFilteringDUMMY2_1[i] ))|
                                                                     (sourceTargetInput()$source==splittedSourceTargetFilteringMIRNA2[i] & (sourceTargetInput()$target == splittedSourceTargetFilteringDUMMY1_2[i] | sourceTargetInput()$target == splittedSourceTargetFilteringDUMMY2_1[i]))|
                                                                     ((sourceTargetInput()$source==splittedSourceTargetFilteringDUMMY1_2[i]| sourceTargetInput()$source==splittedSourceTargetFilteringDUMMY2_1[i]) & sourceTargetInput()$target == splittedSourceTargetFilteringMRNA[i])
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
    
    if(length(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown)) > 0){ 
        nodes$color.background <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="up", "rgb(255,102,102)",
                                         ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="down","rgb(153,204,255)",
                                                "rgb(153,153,153)"))
        
        nodes$color.border <- ifelse((filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$significance !="" & 
                                          filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$significance < 0.05), "black",
                                     ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="down","rgb(153,204,255)",
                                            ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="up", "rgb(255,102,102)",
                                                   "rgb(153,153,153)")))
        
        nodes$borderWidth <- ifelse((filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$significance !="" & 
                                         filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$significance < 0.05), 3,1)
        
    }
    
    else {
        nodes$color.background <- "rgb(153,153,153)"
        nodes$color.border <- "rgb(153,153,153)"
        nodes$borderWidth <- 1
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
        lnodes <- data.frame(label=c("mRNA","mRNA is TF","miRNA","Up Regulated","Down Regulated","Significant"),
                             shape=c("diamond","square","dot","box","box","dot"),
                             size =c(25,20,25,25,15,25),
                             color.background=c("rgb(153,153,153)",  "rgb(153,153,153)", "rgb(153,153,153)","rgb(255,102,102)","rgb(153,204,255)","white"),
                             borderWidth=c(1,1,1,1,1,2),
                             color.border=c("rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)","rgb(255,102,102)","rgb(153,204,255)","black")
        )
        
        
        
        visNetwork(nodes, edges) %>%
            visLegend(addNodes = lnodes,main = "Legend",width = 0.1, position = "right",zoom=F,stepY = 120,useGroups = F)
    }
    else{
        lnodes <- data.frame(label=c("mRNA","mRNA is TF","miRNA"),
                             shape=c("diamond","square","dot"),
                             size =c(30,30,25),
                             color.background=c("rgb(153,153,153)"),
                             color.border=c("rgb(153,153,153)", "rgb(153,153,153)","rgb(153,153,153)")
        )
        visNetwork(nodes, edges) %>%
            visLegend(addNodes = lnodes,main = "Legend",width = 0.1, position = "right",zoom=F,stepY = 120,useGroups = F)
    }
    
})



##################################################################################################    

TripletsInWhichCancerWCount2 <- reactive({
    orListForCommonCancer <- rep("|",length(input$CommonCancer))
    cancerListForCommonTripletAndPair <- paste(c(rbind(orListForCommonCancer, matrix(input$CommonCancer,ncol = length(orListForCommonCancer)))[-1]),collapse = '')
    
    if(length(input$CommonCancer) >0 ){
        TripletsInWhichCancerWCount%>%
            filter(stringr::str_detect(CancerTypes,cancerListForCommonTripletAndPair))  
        
    }
    else{ #BURAYA BİR DAHA BAK!
        filter(TripletsInWhichCancerWCount, WhichCancer %in% input$CommonCancer) 
    }                                                  
})

output$tableCommonTriplet <- DT::renderDataTable({
    
    filtered <-filter(TripletsInWhichCancerWCount2(), Count> 1)
    DT::datatable(filtered,
                  options = list(
                      columnDefs = list(
                          list(className = "dt-center", targets = "_all")
                      )
                  ))
})

MirnaPairsInWhichCancerWCount2 <- reactive({
    orListForCommonCancer <- rep("|",length(input$CommonCancer))
    cancerListForCommonTripletAndPair <- paste(c(rbind(orListForCommonCancer, matrix(input$CommonCancer,ncol = length(orListForCommonCancer)))[-1]),collapse = '')
    MirnaPairsInWhichCancerWCount %>%
        filter(stringr::str_detect(CancerTypes, cancerListForCommonTripletAndPair))
    
})

output$tableCommonmiRNAPair <- DT::renderDataTable({
    filtered <-filter(MirnaPairsInWhichCancerWCount2(), Count> 1)
    DT::datatable(filtered,
                  options = list(
                      columnDefs = list(
                          list(className = "dt-center", targets = "_all")
                      )
                  ))
})

output$TCGAAbbrv <- renderTable({
    TCGA_abbreviations
})

}


# Run the application 
shinyApp(ui = ui, server = server)
