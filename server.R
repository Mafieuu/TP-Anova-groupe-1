server <- function(input, output, session) {
  
  #------------------------------ chargement des data
  data_input <- reactive({
    req(input$datafile)
    # req () pour verifier que le fichier a ete charge
    read.csv(input$datafile$datapath, stringsAsFactors = TRUE)
  })
  
  #-------------------------- Selection des variables 
  output$select_vars <- renderUI({
    req(data_input())
    data <- data_input()
    var_names <- names(data)
    
    tagList(
      selectInput("cible", "Variable quantitative", 
                  choices = var_names,
                  selected = NULL),
      selectInput("facteur1", "Facteur 1", 
                  choices = var_names,
                  selected = NULL),
      selectInput("facteur2", "Facteur 2", 
                  choices = var_names,
                  selected = NULL),
      selectInput("facteur3", "Facteur 3", 
                  choices = var_names,
                  selected = NULL)
    )
  })
  #----------- Aperçu des variables sélectionnées
  
  output$var1 <- renderText({
    req(input$facteur1,data_input(),input$validate_data > 0)
    paste("Facteur 1:", paste(unique(data_input()[[input$facteur1]]), collapse = ", "))
  })
  
  output$var2 <- renderText({
    req(input$facteur2)
    paste("Facteur 2:", paste(unique(data_input()[[input$facteur2]]), collapse = ", "))
  })
  
  output$var3 <- renderText({
    req(input$facteur3)
    paste("Facteur 3:", paste(unique(data_input()[[input$facteur3]]), collapse = ", "))
  })
  
  output$var_cible <- renderText({
    req(input$cible)
    paste("Cible - Min:", min(data_input()[[input$cible]], na.rm = TRUE), 
          "Max:", max(data_input()[[input$cible]], na.rm = TRUE))
  })
  
  #---------------- Visualisation avec renderDT
  
  output$data_preview <- renderDT({
    req(data_input(),input$validate_data > 0)
    selected_cols <- c(input$cible, input$facteur1, input$facteur2, input$facteur3)
    datatable(data_input()[, selected_cols, drop = FALSE],
              options = list(pageLength = 5,
                             scrollX = TRUE,
                             dom = 'Bfrtip'),
              rownames = FALSE)
  })
  
  # Mise à jour des choix d'interactions
  observe({
    req(input$facteur1, input$facteur2, input$facteur3)
    f1 <- input$facteur1
    f2 <- input$facteur2
    f3 <- input$facteur3
    
    interactions <- list(
      paste(f1, ":", f2),
      paste(f1, ":", f3),
      paste(f2, ":", f3),
      paste(f1, ":", f2, ":", f3)
    )
    
    updateCheckboxGroupInput(session, "interactions",
                             choices = interactions,
                             selected = NULL)
  })
  
  # ANOVA sans interaction
  simple_model <- eventReactive(input$validate_data, {
    req(input$cible, input$facteur1, input$facteur2, input$facteur3)
    
    formule <- as.formula(paste(input$cible, "~",
                                input$facteur1, "+",
                                input$facteur2, "+",
                                input$facteur3))
    
    lm(formule, data = data_input())
  })
  
  # Résultats ANOVA simple
  output$anova_simple_result <- renderPrint({
    req(simple_model())
    Anova(simple_model(), type = 3)
  })
  
  # Tests post-hoc et visualisations pour chaque facteur
  output$posthoc_f1_result <- renderPrint({
    req(simple_model())
    TukeyHSD(aov(simple_model()), input$facteur1)
  })
  
  output$posthoc_f1_plot <- renderPlotly({
    req(simple_model())
    data <- data_input()
    
    plot_ly(data, x = as.formula(paste0("~", input$facteur1)),
            y = as.formula(paste0("~", input$cible)),
            type = "box",
            color = as.formula(paste0("~", input$facteur1))) %>%
      layout(title = paste("Distribution par", input$facteur1))
  })
  
  # Répéter pour facteur 2 et 3...
  
  # ANOVA avec interactions
  interaction_model <- reactive({
    req(input$interactions)
    
    # Construction de la formule avec les interactions sélectionnées
    base_terms <- c(input$facteur1, input$facteur2, input$facteur3)
    interaction_terms <- input$interactions
    
    formule_str <- paste(input$cible, "~",
                         paste(c(base_terms, interaction_terms),
                               collapse = " + "))
    
    formule <- as.formula(formule_str)
    lm(formule, data = data_input())
  })
  
  # Résultats ANOVA avec interactions
  output$anova_interaction_result <- renderPrint({
    req(interaction_model())
    Anova(interaction_model(), type = 3)
  })
  
  # Visualisation des interactions
  output$interaction_plot <- renderPlotly({
    req(interaction_model())
    
    if (length(input$interactions) == 0) {
      return(NULL)
    }
    
    # Sélection de la première interaction pour la visualisation
    interaction <- strsplit(input$interactions[1], ":")[[1]]
    
    if (length(interaction) == 2) {
      # Interaction double
      data <- data_input()
      
      plot_ly(data, x = as.formula(paste0("~", interaction[1])),
              y = as.formula(paste0("~", input$cible)),
              color = as.formula(paste0("~", interaction[2])),
              type = "box") %>%
        layout(title = paste("Interaction entre", 
                             interaction[1], "et", interaction[2]))
    } else {
      # Interaction triple
      plotly_empty() %>%
        layout(title = "Visualisation non disponible pour les interactions triples")
    }
  })
  
  # Interprétation des résultats
  output$interaction_interpretation <- renderPrint({
    req(interaction_model())
    
    anova_results <- Anova(interaction_model(), type = 3)
    
    cat("Interprétation des résultats :\n\n")
    
    for (term in rownames(anova_results)) {
      p_value <- anova_results[term, "Pr(>F)"]
      cat(sprintf("- %s : ", term))
      
      if (p_value < 0.05) {
        cat("Effet significatif (p < 0.05)\n")
      } else {
        cat("Effet non significatif (p > 0.05)\n")
      }
    }
  })
  
  #---------------------- Statistiques desriptives
  statsServer(
    "stats",
    data = data_input,
    response_var = reactive(input$cible),
    factors = reactive(c(input$facteur1, input$facteur2, input$facteur3))
  )
  
  # Modules tests d'hypothèses
  hypothesisServer(
    "hypothesis_simple",
    model = simple_model
  )
  
  hypothesisServer(
    "hypothesis_interaction",
    model = interaction_model
  )
  
}