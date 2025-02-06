# Module UI pour les tests d'hypothèses
hypothesisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Tests des hypothèses de l'ANOVA",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        fluidRow(
          column(
            width = 6,
            h4("Normalité des résidus"),
            plotlyOutput(ns("qq_plot")),
            verbatimTextOutput(ns("shapiro_test")),
            tags$hr(),
            h5("Interprétation :"),
            textOutput(ns("normalite_interpretation"))
          ),
          column(
            width = 6,
            h4("Homogénéité des variances"),
            plotlyOutput(ns("residuals_plot")),
            verbatimTextOutput(ns("levene_test")),
            tags$hr(),
            h5("Interprétation :"),
            textOutput(ns("homogeneite_interpretation"))
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            h4("Indépendance des observations"),
            plotlyOutput(ns("independence_plot")),
            verbatimTextOutput(ns("runs_test")),
            tags$hr(),
            h5("Interprétation :"),
            textOutput(ns("independence_interpretation"))
          )
        )
      )
    )
  )
}

# Module Server pour les tests d'hypothèses
hypothesisServer <- function(id, model) {
  moduleServer(id, function(input, output, session) {
    
    # Test de normalité et Q-Q plot
    output$qq_plot <- renderPlotly({
      req(model())
      
      residuals <- residuals(model())
      qqnorm_data <- qqnorm(residuals, plot.it = FALSE)
      
      plot_ly() %>%
        add_markers(x = qqnorm_data$x, y = qqnorm_data$y,
                    name = "Résidus") %>%
        add_lines(x = qqnorm_data$x, y = qqnorm_data$x,
                  name = "Ligne théorique",
                  line = list(dash = "dash")) %>%
        layout(title = "Q-Q Plot des résidus",
               xaxis = list(title = "Quantiles théoriques"),
               yaxis = list(title = "Quantiles observés"))
    })
    
    output$shapiro_test <- renderPrint({
      req(model())
      shapiro.test(residuals(model()))
    })
    
    output$normalite_interpretation <- renderText({
      req(model())
      test_result <- shapiro.test(residuals(model()))
      
      if (test_result$p.value > 0.05) {
        "Les résidus suivent une distribution normale (p > 0.05)."
      } else {
        "Les résidus ne suivent pas une distribution normale (p < 0.05)."
      }
    })
    
    # Test d'homogénéité et graphique des résidus
    output$residuals_plot <- renderPlotly({
      req(model())
      
      fitted_vals <- fitted(model())
      residuals_vals <- residuals(model())
      
      plot_ly() %>%
        add_markers(x = fitted_vals, y = residuals_vals) %>%
        add_lines(x = range(fitted_vals), y = c(0,0),
                  line = list(dash = "dash")) %>%
        layout(title = "Résidus vs Valeurs ajustées",
               xaxis = list(title = "Valeurs ajustées"),
               yaxis = list(title = "Résidus"))
    })
    
    output$levene_test <- renderPrint({
      req(model())
      leveneTest(model())
    })
    
    output$homogeneite_interpretation <- renderText({
      req(model())
      test_result <- leveneTest(model())
      
      if (test_result$`Pr(>F)`[1] > 0.05) {
        "Les variances sont homogènes entre les groupes (p > 0.05)."
      } else {
        "Les variances ne sont pas homogènes entre les groupes (p < 0.05)."
      }
    })
    
    # Test d'indépendance et graphique temporel
    output$independence_plot <- renderPlotly({
      req(model())
      
      residuals_vals <- residuals(model())
      index <- seq_along(residuals_vals)
      
      plot_ly() %>%
        add_markers(x = index, y = residuals_vals) %>%
        add_lines(x = range(index), y = c(0,0),
                  line = list(dash = "dash")) %>%
        layout(title = "Résidus vs Ordre des observations",
               xaxis = list(title = "Ordre des observations"),
               yaxis = list(title = "Résidus"))
    })
    
    # Fonction pour le test des runs (test d'indépendance)
    runs_test <- function(residuals) {
      signs <- sign(residuals)
      runs <- rle(signs)
      n1 <- sum(signs > 0)
      n2 <- sum(signs < 0)
      R <- length(runs$lengths)
      mu <- 1 + (2 * n1 * n2)/(n1 + n2)
      var <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2))/
        ((n1 + n2)^2 * (n1 + n2 - 1))
      z <- (R - mu)/sqrt(var)
      p_value <- 2 * pnorm(-abs(z))
      
      list(statistic = z,
           p.value = p_value,
           method = "Runs test for randomness")
    }
    
    output$runs_test <- renderPrint({
      req(model())
      runs_test(residuals(model()))
    })
    
    output$independence_interpretation <- renderText({
      req(model())
      test_result <- runs_test(residuals(model()))
      
      if (test_result$p.value > 0.05) {
        "Les observations semblent être indépendantes (p > 0.05)."
      } else {
        "Il pourrait y avoir une dépendance entre les observations (p < 0.05)."
      }
    })
    
  })
}