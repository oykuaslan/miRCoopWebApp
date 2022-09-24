fluidPage(
  
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
                                     
                                     
                                     sliderInput("Lancaster_XY_Z_range",
                                                 label = p("Triplet pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                 value = c(0,0.01),
                                                 min = 0,
                                                 max = 0.01),
                                     
                                     hr(),
                                     
                                     #uiOutput("BH_pval"),
                                     # sliderInput("BH_pvalue_adjusted",
                                     #             label = p("Adjusted pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                     #             value = c(0,1),
                                     #             min = 0,
                                     #             max = 1),
                                     
                                     
                                     conditionalPanel(condition = "input.dataset == 'ACC'",
                                                      sliderInput("BH_pvalue_adjusted_ACC",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=ACC_BH_pvalues_adjusted_min,
                                                                  max=ACC_BH_pvalues_adjusted_max,
                                                                  value=c(ACC_BH_pvalues_adjusted_min,ACC_BH_pvalues_adjusted_max),
                                                      )),
                                     
                                     conditionalPanel(condition = "input.dataset == 'BLCA'",
                                                      sliderInput("BH_pvalue_adjusted_BLCA",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=BLCA_BH_pvalues_adjusted_min,
                                                                  max=BLCA_BH_pvalues_adjusted_max,
                                                                  value=c(BLCA_BH_pvalues_adjusted_min,BLCA_BH_pvalues_adjusted_max),
                                                      )),
                                     
                                     conditionalPanel(condition = "input.dataset == 'BRCA'",
                                                      sliderInput("BH_pvalue_adjusted_BRCA",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=BRCA_BH_pvalues_adjusted_min,
                                                                  max=BRCA_BH_pvalues_adjusted_max,
                                                                  value=c(BRCA_BH_pvalues_adjusted_min,BRCA_BH_pvalues_adjusted_max),
                                                      )),
                                     
                                     conditionalPanel(condition = "input.dataset == 'CESC'",
                                                      sliderInput("BH_pvalue_adjusted_CESC",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=CESC_BH_pvalues_adjusted_min,
                                                                  max=CESC_BH_pvalues_adjusted_max,
                                                                  value=c(CESC_BH_pvalues_adjusted_min,CESC_BH_pvalues_adjusted_max),
                                                                  round = FALSE)),
                                     
                                     conditionalPanel(condition = "input.dataset == 'CHOL'",
                                                      sliderInput("BH_pvalue_adjusted_CHOL",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=CHOL_BH_pvalues_adjusted_min,
                                                                  max=CHOL_BH_pvalues_adjusted_max,
                                                                  value=c(CHOL_BH_pvalues_adjusted_min,CHOL_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'COAD'",
                                                      sliderInput("BH_pvalue_adjusted_COAD",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=COAD_BH_pvalues_adjusted_min,
                                                                  max=COAD_BH_pvalues_adjusted_max,
                                                                  value=c(COAD_BH_pvalues_adjusted_min,COAD_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'DLBC'",
                                                      sliderInput("BH_pvalue_adjusted_DLBC",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=DLBC_BH_pvalues_adjusted_min,
                                                                  max=DLBC_BH_pvalues_adjusted_max,
                                                                  value=c(DLBC_BH_pvalues_adjusted_min,DLBC_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'ESCA'",
                                                      sliderInput("BH_pvalue_adjusted_ESCA",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=ESCA_BH_pvalues_adjusted_min,
                                                                  max=ESCA_BH_pvalues_adjusted_max,
                                                                  value=c(ESCA_BH_pvalues_adjusted_min,ESCA_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'HNSC'",
                                                      sliderInput("BH_pvalue_adjusted_HNSC",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=HNSC_BH_pvalues_adjusted_min,
                                                                  max=HNSC_BH_pvalues_adjusted_max,
                                                                  value=c(HNSC_BH_pvalues_adjusted_min,HNSC_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'KICH'",
                                                      sliderInput("BH_pvalue_adjusted_KICH",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=KICH_BH_pvalues_adjusted_min,
                                                                  max=KICH_BH_pvalues_adjusted_max,
                                                                  value=c(KICH_BH_pvalues_adjusted_min,KICH_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'KIRC'",
                                                      sliderInput("BH_pvalue_adjusted_KIRC",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=KIRC_BH_pvalues_adjusted_min,
                                                                  max=KIRC_BH_pvalues_adjusted_max,
                                                                  value=c(KIRC_BH_pvalues_adjusted_min,KIRC_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'KIRP'",
                                                      sliderInput("BH_pvalue_adjusted_KIRP",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=KIRP_BH_pvalues_adjusted_min,
                                                                  max=KIRP_BH_pvalues_adjusted_max,
                                                                  value=c(KIRP_BH_pvalues_adjusted_min,KIRP_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'LGG'",
                                                      sliderInput("BH_pvalue_adjusted_LGG",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=LGG_BH_pvalues_adjusted_min,
                                                                  max=LGG_BH_pvalues_adjusted_max,
                                                                  value=c(LGG_BH_pvalues_adjusted_min,LGG_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'LIHC'",
                                                      sliderInput("BH_pvalue_adjusted_LIHC",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=LIHC_BH_pvalues_adjusted_min,
                                                                  max=LIHC_BH_pvalues_adjusted_max,
                                                                  value=c(LIHC_BH_pvalues_adjusted_min,LIHC_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'LUAD'",
                                                      sliderInput("BH_pvalue_adjusted_LUAD",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=LUAD_BH_pvalues_adjusted_min,
                                                                  max=LUAD_BH_pvalues_adjusted_max,
                                                                  value=c(LUAD_BH_pvalues_adjusted_min,LUAD_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'LUSC'",
                                                      sliderInput("BH_pvalue_adjusted_LUSC",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=LUSC_BH_pvalues_adjusted_min,
                                                                  max=LUSC_BH_pvalues_adjusted_max,
                                                                  value=c(LUSC_BH_pvalues_adjusted_min,LUSC_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'MESO'",
                                                      sliderInput("BH_pvalue_adjusted_MESO",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=MESO_BH_pvalues_adjusted_min,
                                                                  max=MESO_BH_pvalues_adjusted_max,
                                                                  value=c(MESO_BH_pvalues_adjusted_min,MESO_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'OV'",
                                                      sliderInput("BH_pvalue_adjusted_OV",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=OV_BH_pvalues_adjusted_min,
                                                                  max=OV_BH_pvalues_adjusted_max,
                                                                  value=c(OV_BH_pvalues_adjusted_min,OV_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'PAAD'",
                                                      sliderInput("BH_pvalue_adjusted_PAAD",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=PAAD_BH_pvalues_adjusted_min,
                                                                  max=PAAD_BH_pvalues_adjusted_max,
                                                                  value=c(PAAD_BH_pvalues_adjusted_min,PAAD_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'PCPG'",
                                                      sliderInput("BH_pvalue_adjusted_PCPG",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=PCPG_BH_pvalues_adjusted_min,
                                                                  max=PCPG_BH_pvalues_adjusted_max,
                                                                  value=c(PCPG_BH_pvalues_adjusted_min,PCPG_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'PRAD'",
                                                      sliderInput("BH_pvalue_adjusted_PRAD",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=PRAD_BH_pvalues_adjusted_min,
                                                                  max=PRAD_BH_pvalues_adjusted_max,
                                                                  value=c(PRAD_BH_pvalues_adjusted_min,PRAD_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'READ'",
                                                      sliderInput("BH_pvalue_adjusted_READ",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=READ_BH_pvalues_adjusted_min,
                                                                  max=READ_BH_pvalues_adjusted_max,
                                                                  value=c(READ_BH_pvalues_adjusted_min,READ_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'SARC'",
                                                      sliderInput("BH_pvalue_adjusted_SARC",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=SARC_BH_pvalues_adjusted_min,
                                                                  max=SARC_BH_pvalues_adjusted_max,
                                                                  value=c(SARC_BH_pvalues_adjusted_min,SARC_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'SKCM'",
                                                      sliderInput("BH_pvalue_adjusted_SKCM",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=SKCM_BH_pvalues_adjusted_min,
                                                                  max=SKCM_BH_pvalues_adjusted_max,
                                                                  value=c(SKCM_BH_pvalues_adjusted_min,SKCM_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'STAD'",
                                                      sliderInput("BH_pvalue_adjusted_STAD",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=STAD_BH_pvalues_adjusted_min,
                                                                  max=STAD_BH_pvalues_adjusted_max,
                                                                  value=c(STAD_BH_pvalues_adjusted_min,STAD_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'TGCT'",
                                                      sliderInput("BH_pvalue_adjusted_TGCT",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=TGCT_BH_pvalues_adjusted_min,
                                                                  max=TGCT_BH_pvalues_adjusted_max,
                                                                  value=c(TGCT_BH_pvalues_adjusted_min,TGCT_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'THCA'",
                                                      sliderInput("BH_pvalue_adjusted_THCA",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=THCA_BH_pvalues_adjusted_min,
                                                                  max=THCA_BH_pvalues_adjusted_max,
                                                                  value=c(THCA_BH_pvalues_adjusted_min,THCA_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'THYM'",
                                                      sliderInput("BH_pvalue_adjusted_THYM",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=THYM_BH_pvalues_adjusted_min,
                                                                  max=THYM_BH_pvalues_adjusted_max,
                                                                  value=c(THYM_BH_pvalues_adjusted_min,THYM_BH_pvalues_adjusted_max))),
                                     conditionalPanel(condition = "input.dataset == 'UCEC'",
                                                      sliderInput("BH_pvalue_adjusted_UCEC",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=UCEC_BH_pvalues_adjusted_min,
                                                                  max=UCEC_BH_pvalues_adjusted_max,
                                                                  value=c(UCEC_BH_pvalues_adjusted_min,UCEC_BH_pvalues_adjusted_max)
                                                      )),
                                     conditionalPanel(condition = "input.dataset == 'UCS'",
                                                      sliderInput("BH_pvalue_adjusted_UCS",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=UCS_BH_pvalues_adjusted_min,
                                                                  max=UCS_BH_pvalues_adjusted_max,
                                                                  value=c(UCS_BH_pvalues_adjusted_min,UCS_BH_pvalues_adjusted_max)
                                                      )),
                                     conditionalPanel(condition = "input.dataset == 'UVM'",
                                                      sliderInput("BH_pvalue_adjusted_UVM",
                                                                  label = p("Corrected pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                  min=UVM_BH_pvalues_adjusted_min,
                                                                  max=UVM_BH_pvalues_adjusted_max,
                                                                  value=c(UVM_BH_pvalues_adjusted_min,UVM_BH_pvalues_adjusted_max)
                                                      )),
                                     
                                     hr(),
                                     # checkboxGroupInput(inputId = "filter_BH_rejected",
                                     #                    label = " ",
                                     #                    choiceNames = c("Filter out BH not rejected?"),
                                     #                    choiceValues = c("True")
                                     #                    ),
                                     # 
                                     # hr(),
                                     checkboxGroupInput(inputId = "is_mrna_tf",
                                                        label = "Filter out:",
                                                        choiceNames = c("mRNAs that are not TF"),
                                                        choiceValues = c("True"),
                                                        selected = NULL),
                                     downloadButton("downloadData", "Download")
                                     
                                     
                                     
                                     
                        ),
                        mainPanel(
                          
                          tabsetPanel(type = "tabs",
                                      tabPanel("Table", br(),
                                               DT::dataTableOutput("table", height = "800px")),
                                      
                                      tabPanel("Network",
                                               conditionalPanel(condition = "input.dataset == 'ACC' || input.dataset == 'DLBC' ||
                                                                input.dataset == 'LGG' || input.dataset == 'MESO' || input.dataset == 'OV' || input.dataset == 'TGCT'  ||
                                                                input.dataset == 'UCS' || input.dataset == 'UVM'",
                                                                selectInput("colorGroup1", "Color Nodes Based on :",
                                                                            c("miRNA Family", "miRNA Cluster"),selected = NULL)
                                                                ),
                                               
                                               conditionalPanel(condition = "input.dataset == 'BLCA' || input.dataset == 'BRCA' || input.dataset == 'CESC' ||
                                                                input.dataset == 'CHOL' ||  input.dataset == 'COAD' ||  input.dataset == 'ESCA' || input.dataset == 'HNSC' ||
                                                                input.dataset == 'KICH' || input.dataset == 'KIRC' || input.dataset == 'KIRP' || input.dataset == 'LIHC' ||
                                                                input.dataset == 'LUAD' || input.dataset == 'LUSC' || input.dataset == 'PAAD' || input.dataset == 'PCPG' ||
                                                                input.dataset == 'PRAD' || input.dataset == 'READ' || input.dataset == 'SARC' || input.dataset == 'SKCM' ||
                                                                input.dataset == 'STAD' || input.dataset == 'THCA' || input.dataset == 'THYM' || input.dataset == 'UCEC'
                                                                ",
                                                                selectInput("colorGroup2", "Color Nodes Based on :",
                                                                            c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"),selected = NULL)
                                               ),

                                               
                                               # conditionalPanel(condition = "input.dataset == 'ACC'",
                                               #                  selectInput("colorGroup1", "Color Nodes Based on :",
                                               #                              c("miRNA Family", "miRNA Cluster"),selected = NULL,multiple = F)
                                               # ),
                                               # 
                                               # conditionalPanel(condition = "input.dataset == 'BRCA'",
                                               #                  selectInput("colorGroup2", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"),selected = NULL,multiple = F)
                                               # ),
                                               # 
                                               # conditionalPanel(condition = "input.dataset == 'ACC'",
                                               #                  selectInput("color_ACC", "Color Nodes Based on :",
                                               #                              c("","miRNA Family", "miRNA Cluster"),selected = NULL,multiple = F)
                                               # ),
                                               # 
                                               # conditionalPanel(condition = "input.dataset == 'BRCA'",
                                               #                  selectInput("color_BRCA", "Color Nodes Based on :",
                                               #                              c("","Differential Expression Analysis","miRNA Family", "miRNA Cluster"),selected = NULL,multiple = F)
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'CESC'",
                                               #                  selectInput("color_CESC", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'CHOL'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'COAD'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'DLBC'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'ESCA'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'HNSC'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'KICH'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'KIRC'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'KIRP'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'LGG'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'LIHC'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'LUAD'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'LUSC'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'MESO'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'OV'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'PAAD'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'PCPG'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'PRAD'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'READ'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'SARC'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'SKCM'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'STAD'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'TGCT'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'THCA'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'THYM'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'UCEC'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'UCS'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # conditionalPanel(condition = "input.dataset == 'UVM'",
                                               #                  selectInput("color", "Color Nodes Based on :",
                                               #                              c("miRNA Family", "miRNA Cluster"))
                                               # ),
                                               # selectInput("color", "Color Nodes Based on :",
                                               #             c("Differential Expression Analysis", "miRNA Family", "miRNA Cluster")),
                                               shinycustomloader::withLoader(visNetworkOutput("vNetwork", height = "100vh"),type = "html",loader="loader3"),
                                               div(align = "right", downloadButton("downloadNodeTable", "Download Node Table"),
                                                   downloadButton("downloadEdgeTable", "Download Edge Table")),
                                               br(),
                                               br())
                          )
                        )
                      )
             ),
             
             
             tabPanel("Pan-cancer Triplets", value="Pan-cancerTriplets",fluid = TRUE,icon = icon("th-list"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          pickerInput(inputId = "CommonTripletCancer", label = "Select Cancer Names",choices = cancerNames, selected=cancerNames, multiple = TRUE),
                          hr(),
                          selectizeInput("mrnaCommonTriplet", "mRNA Filter", choices=unique(mrnaFilterCommonTriplets$name), multiple=TRUE),
                          br(),
                          selectizeInput("mirnaCommonTriplet", "miRNA Filter", choices=unique(mirnaFilterCommonTriplets$name), multiple=TRUE),
                          hr(),
                          br(),
                          downloadButton("downloadCommonTripletsData", "Download")
                          
                          
                          
                          
                        ),
                        
                        mainPanel(
                          useShinyalert(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("Table", br(),
                                               DT::dataTableOutput("tableCommonTriplet")),
                                      tabPanel("Network", shinycustomloader::withLoader(visNetworkOutput("commonTripletNetwork", height = "100vh"),type = "html",loader="loader3"),
                                               br(),
                                               div(align = "right",downloadButton("downloadNodeTableCommonTriplet", "Download Node Table"),
                                                   downloadButton("downloadEdgeTableCommonTriplet", "Download Edge Table")),
                                               br(),
                                               br()
                                      )
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
                          pickerInput(inputId = "CommonMirnaPairCancerCount", label = "Select with Count Above:",choices = c(1,2,3,4), selected=c(1), multiple = FALSE),
                          # checkboxGroupInput(inputId = "CommonMirnaPairCancerCount",
                          #                    label = "Filter with Count",
                          #                    choiceNames = c("Above 3","Above 4","Above 5","Above 6", "Above 7","Above 8"),
                          #                    choiceValues = c("Above3","Above4","Above5","Above6", "Above7","Above8"),
                          #                    selected = c("Above3","Above4","Above5","Above6", "Above7","Above8")),
                          #numericInput("CommonMirnaPairCancerCount2", "Select with Count Above:", 2, min = 2, max = 8, step = 1),
                          br(),
                          br(),
                          downloadButton("downloadCommonMirnaPairsData", "Download")
                          
                        ),
                        
                        mainPanel(
                          useShinyalert(),
                          tabsetPanel(type = "tabs",
                                      tabPanel("Common miRNA Pair Table", DT::dataTableOutput("tableCommonmiRNAPair")),
                                      tabPanel("Network", selectInput("colorCommonMirna", "Color Nodes Based on :",
                                                                      c("miRNA Family", "miRNA Cluster"),selected = NULL),
                                               shinycustomloader::withLoader(visNetworkOutput("commonMirnaNetwork", height = "100vh"),type = "html",loader="loader3"),
                                               br(),
                                               div(align = "right", downloadButton("downloadNodeTableCommonMirnaPair", "Download Node Table"),
                                                   downloadButton("downloadEdgeTableCommonMirnaPair", "Download Edge Table")),
                                               br(),
                                               br())
                                      
                                      
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


