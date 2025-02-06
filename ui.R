source("modules/stats_module.R")
source("modules/hypothesis_module.R")
ui <- dashboardPage(
  dark = FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  #-------------------------------- Header
  dashboardHeader(
    skin = "light",
    title = "TP ANOVA",
    titleWidth = 450
  ),
  
  #------------------------------------- SliderBar
  dashboardSidebar(
    
    skin = "light",
    status = "primary",
    elevation = 3,
    sidebarMenu(
      menuItem(
        text = "Données",
        tabName = "data",
        icon = icon("database")
      ),
      menuItem(
        text = "Tests d'hypothèses",
        tabName = "hypotheses",
        icon = icon("check-square")
      ),
      menuItem(
        text = "Analyse descriptive",
        tabName = "descriptive",
        icon = icon("chart-bar")
      ),
      menuItem(
        text = "ANOVA sans interaction",
        tabName = "anova_simple",
        icon = icon("calculator")
      ),
      menuItem(
        text = "ANOVA avec interactions",
        tabName = "anova_interaction",
        icon = icon("project-diagram")
      )
     
    )
  ),
  
  #-----------------------------------------------  Body
  dashboardBody(
    
    tabItems(
      ## -----------------------------------  ## La page data
      tabItem(
        tabName = "data",
        p(tags$i(icon("upload"), " Commencez par sélectionner votre fichier CSV ainsi que les variables dont vous aurez besoin ...",
                 style="font-size: 20px;color: #f07167;")),
        fluidRow(
          
          box(
           
            width = 10, # recouvrir presque entierement la page
            title = "Import et selection des variables",
            status = "info",
            fileInput("datafile", "Importer le fichier csv", accept = ".csv"),
            uiOutput("select_vars"),
            #  voir server pour select_vars
            actionButton("validate_data", "Valider les choix", 
                         class = "btn-success", icon = icon("check"))
          )
        ),
        fluidRow(
          box(
            title = "Aperçu des variables",
            status = "primary",
            textOutput("var1"),
            textOutput("var2"),
            textOutput("var3"),
            textOutput("var_cible")
          )
        ),
       
        fluidRow(
          box(
            width = 10,
            title = "Aperçu des données",
            status = "info",
            DTOutput("data_preview")
          )
        )
      ),
      
      ## -----------------------------------  ## La page Analyse descriptive
      tabItem(
        tabName = "descriptive",
        statsUI("stats")
      ),
      
      ## -----------------------------------  ## La page Anova sans interaction
      tabItem(
        tabName = "anova_simple",
        fluidRow(
          box(
            width = 10,
            title = "ANOVA à effets principaux",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("anova_simple_result")
          )
        ),
        fluidRow(
          tabBox(
            width = 10,
            title = "Analyses post-hoc",
            tabPanel(
              title = "Facteur 1",
              plotlyOutput("posthoc_f1_plot"),
              verbatimTextOutput("posthoc_f1_result")
            ),
            tabPanel(
              title = "Facteur 2",
              plotlyOutput("posthoc_f2_plot"),
              verbatimTextOutput("posthoc_f2_result")
            ),
            tabPanel(
              title = "Facteur 3",
              plotlyOutput("posthoc_f3_plot"),
              verbatimTextOutput("posthoc_f3_result")
            )
          )
        )
      ),
      
      ## -----------------------------------  ## La page ANOVA avec interraction
      tabItem(
        tabName = "anova_interaction",
        fluidRow(
          box(
            width = 10,
            title = "Sélection des interactions",
            status = "primary",
            checkboxGroupInput("interactions",
                               "Sélectionner les interactions à inclure:",
                               choices = NULL)
          )
        ),
        fluidRow(
          box(
            width = 10,
            title = "Résultats ANOVA avec interactions",
            status = "info",
            verbatimTextOutput("anova_interaction_result")
          )
        ),
        fluidRow(
          box(
            width = 10,
            title = "Visualisation des interactions",
            status = "warning",
            plotlyOutput("interaction_plot")
          )
        ),
        fluidRow(
          box(
            width = 10,
            title = "Interprétation des résultats",
            status = "success",
            verbatimTextOutput("interaction_interpretation")
          )
        )
      ),
      
      ## -----------------------------------  ## La page teste d'hypotheses
      tabItem(
        tabName = "hypotheses",
        hypothesisUI("hypothesis_simple"),
        hypothesisUI("hypothesis_interaction")
      )
    )
  )
)