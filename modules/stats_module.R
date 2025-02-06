
statsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Statistiques descriptives par groupe",
        DTOutput(ns("summary_stats"))
      ),
      box(
        width = 12,
        title = "Valeurs aberrantes",
        DTOutput(ns("outliers_table"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Distribution par groupes",
        plotlyOutput(ns("group_boxplot"), height = "600px")
      )
    )
  )
}

# ----- MODULE SERVER -----
statsServer <- function(id, data, response_var, factors) {
  moduleServer(id, function(input, output, session) {
    
    # Calcul des statistiques descriptives par groupe
    
    summary_stats <- reactive({
      req(data(), response_var(), factors())
      
      df <- data()
      response <- response_var()
      
      df %>%
        group_by(across(all_of(factors()))) %>%
        summarise(
          n = n(),
          mean = mean(!!sym(response), na.rm = TRUE),
          sd = sd(!!sym(response), na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    # Identification des valeurs aberrantes par groupe
    outliers <- reactive({
      req(data(), response_var(), factors())
      
      df <- data()
      response <- response_var()
      
      df %>%
        group_by(across(all_of(factors()))) %>%
        identify_outliers(!!sym(response)) %>%
        filter(is.outlier | is.extreme) %>%
        ungroup()
    })
    
    # Rendu du tableau des statistiques descriptives
    output$summary_stats <- renderDT({
      req(summary_stats())
      datatable(
        summary_stats(),
        options = list(dom = 't'),
        caption = "Statistiques descriptives par groupe"
      )
    })
    
    # Rendu du tableau des valeurs aberrantes
    output$outliers_table <- renderDT({
      req(outliers())
      datatable(
        outliers(),
        options = list(dom = 't'),
        caption = "Valeurs aberrantes détectées"
      )
    })
    
    # Création et affichage du boxplot interactif
    output$group_boxplot <- renderPlotly({
      req(data(), response_var(), factors())
      
      df <- data()
      response <- response_var()
      f1 <- factors()[1]  # par exemple, "treatment"
      f2 <- factors()[2]  # par exemple, "risk"
      f3 <- factors()[3]  # par exemple, "gender"
      
      p <- ggplot(df, aes(x = !!sym(f1), y = !!sym(response), color = !!sym(f2))) +
        geom_boxplot() +
        facet_wrap(as.formula(paste("~", f3))) +
        scale_color_manual(values = colorRampPalette(c("#1f77b4", "#ff7f0e"))(length(unique(df[[f2]])))) +
        theme_minimal() +
        labs(
          title = "Distribution par groupes",
          x = f1,
          y = response,
          color = f2
        )
      
      ggplotly(p, height = 600) %>%
        layout(
          boxmode = "group",
          showlegend = TRUE,
          legend = list(title = list(text = f2))
        )
    })
    
    # Retour des réactifs utiles
    return(list(
      stats = summary_stats,
      outliers = outliers
    ))
  })
}
