# Opgave 4

# Samlet Brøndby IF Shiny App med xG & Clustre
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
library(RColorBrewer)

# CSS for Brøndby-style (blå og gul)
brondby_style <- "
  body {
    background-color: #002F6C; /* Brøndby blå */
    color: #FFD700; /* Brøndby gul */
    font-family: 'Arial';
  }
  h1, h2, h3, h4, .tab-panel {
    color: #FFD700;
  }
  .well {
    background-color: #001E3D;
    border: none;
  }
"

# UI
ui <- fluidPage(
  fluidRow(
    column(2,
           tags$img(src = "https://upload.wikimedia.org/wikipedia/en/6/65/Br%C3%B8ndby_IF_logo.svg", height = "80px")
    ),
    column(10,
           h1("Brøndby IF Analyse Dashboard", style = "padding-top:20px;")
    )
  ),
  br(),
  tabsetPanel(
    # Faner fra Opgave 1
    tabPanel("xG: Klassificeringsmodeller",
             sidebarLayout(
               sidebarPanel(
                 selectInput("modelvalg", "Vælg model:",
                             choices = c("Logistisk Regression", "Random Forest", "Gradient Boosting")),
                 checkboxInput("vis_alle", "Vis alle modeller samtidigt", value = FALSE),
                 verbatimTextOutput("auc_output")
               ),
               mainPanel(plotOutput("roc_plot", height = "500px"))
             )
    ),
    tabPanel("xG: Spiller Rangering",
             h4("Rangering af spillere baseret på forskellen mellem xG og faktiske mål"),
             DTOutput("player_table")
    ),
    tabPanel("xG: Heatmap",
             plotOutput("xg_heatmap", height = "600px")
    ),
    
    # Faner fra Opgave 3
    tabPanel("Afleverings-Cluster",
             fluidRow(
               column(3,
                      checkboxGroupInput("selected_clusters", "Vælg klynger der skal vises:",
                                         choices = list(
                                           "1 – Midtbanespiller" = "1",
                                           "2 – Forsvar" = "2",
                                           "3 – Angriber" = "3",
                                           "4 – Målmand" = "4"
                                         ),
                                         selected = c("1", "2", "3", "4")),
                      helpText("Beskrivelse af klynger:",
                               tags$ul(
                                 tags$li("1 – Midtbanespiller: Lange afleveringer, lav volumen"),
                                 tags$li("2 – Forsvar: Korte afleveringer, høj nøjagtighed"),
                                 tags$li("3 – Angriber: Kreative afleveringer, mellemvolumen"),
                                 tags$li("4 – Målmand: Få afleveringer, lav nøjagtighed")
                               ))
               ),
               column(9,
                      plotlyOutput("cluster_plot", height = "700px")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Klassificeringsmodeller (fra Opgave 1)
  selected_model <- reactive({
    switch(input$modelvalg,
           "Logistisk Regression" = list(roc = roc_glm, color = "blue"),
           "Random Forest" = list(roc = roc_rf, color = "red"),
           "Gradient Boosting" = list(roc = roc_gbm, color = "green"))
  })
  
  output$auc_output <- renderText({
    if (!input$vis_alle) {
      auc_val <- auc(selected_model()$roc)
      paste("AUC:", round(auc_val, 3))
    } else {
      auc_glm <- round(auc(roc_glm), 3)
      auc_rf <- round(auc(roc_rf), 3)
      auc_gbm <- round(auc(roc_gbm), 3)
      paste(
        "Logistisk Regression AUC:", auc_glm, "\n",
        "Random Forest AUC:       ", auc_rf, "\n",
        "Gradient Boosting AUC:   ", auc_gbm
      )
    }
  })
  
  output$roc_plot <- renderPlot({
    if (input$vis_alle) {
      plot(roc_glm, col = "blue", lwd = 2, main = "ROC-kurver for alle modeller")
      plot(roc_rf, col = "red", add = TRUE, lwd = 2)
      plot(roc_gbm, col = "green", add = TRUE, lwd = 2)
      legend("bottomright",
             legend = c("Logistisk Regression", "Random Forest", "Gradient Boosting"),
             col = c("blue", "red", "green"),
             lwd = 2)
    } else {
      plot(selected_model()$roc,
           col = selected_model()$color,
           lwd = 3,
           main = paste("ROC-kurve:", input$modelvalg))
    }
    abline(a = 0, b = 1, lty = 2, col = "gray")
  })
  
  # Spiller rangering
  output$player_table <- renderDT({
    datatable(
      player_xg_summary_vis,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Billede", "Spiller", "Mål", "xG", "Forskel (mål - xG)"),
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  # Heatmap
  output$xg_heatmap <- renderPlot({
    req(all_shots)
    ggplot(all_shots, aes(x = locationx, y = locationy)) +
      football_pitch() +
      stat_density_2d(
        aes(fill = ..level..),
        geom = "polygon",
        contour = TRUE,
        n = 150,
        linewidth = 0.3,
        alpha = 0.8
      ) +
      scale_fill_viridis_c(option = "plasma", name = "Skud-intensitet") +
      coord_fixed(xlim = c(0, 105), ylim = c(0, 68)) +
      labs(title = "Alle kampe – Skud Heatmap på fodboldbane", x = "", y = "") +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#228B22", color = NA),
        legend.position = "right",
        plot.title = element_text(color = "white", face = "bold", hjust = 0.5, size = 16)
      )
  })
  
  # Afleverings-cluster
  output$cluster_plot <- renderPlotly({
    plot_data <- Training %>% filter(cluster %in% input$selected_clusters)
    
    plot_ly(data = plot_data,
            x = ~avg_pass_length,
            y = ~pass_accuracy,
            z = ~avg_pass_angle,
            color = ~cluster_label,
            text = ~shortname,
            hoverinfo = "text",
            colors = brewer.pal(4, "Set1"),
            type = "scatter3d",
            mode = "markers") %>%
      layout(title = "3D Cluster Visualisering af Spillere",
             scene = list(
               xaxis = list(title = "Afleveringslængde"),
               yaxis = list(title = "Afleveringsnøjagtighed"),
               zaxis = list(title = "Afleveringvinkel")
             ))
  })
}

shinyApp(ui, server)



#######
# Tilføj labels
Training$cluster_label <- dplyr::recode(Training$cluster,
                                        "1" = "1 – Midtbanespiller",
                                        "2" = "2 – Forsvar",
                                        "3" = "3 – Angriber",
                                        "4" = "4 – Målmand")

# Tilføj spiller-navne
Training$shortname <- pass_summary$shortname




#Med Brøndby Farver
# UI
ui <- fluidPage(
  tags$style(HTML("
  body {
    background-color: #002F6C;
    color: #FFD700;
  }
  h1, h2, h3, h4, label {
    color: #FFD700;
  }
  .dataTables_wrapper {
    color: #FFD700;
  }
  table.dataTable thead {
    color: #FFD700;
    background-color: #001E3D;
  }
  table.dataTable tbody {
    color: #FFD700;
  }
  .dataTables_filter input {
    background-color: #001E3D;
    color: #FFD700;
  }
  .dataTables_length select {
    background-color: #001E3D;
    color: #FFD700;
  }
")
  ),
  fluidRow(
    column(2,
           tags$img(src = "brondby_logo.png", height = "80px", alt = "Brøndby IF logo")
    ),
    column(10,
           h1("Brøndby IF Analyse Dashboard", style = "padding-top:20px;")
    )
  ),
  br(),
  tabsetPanel(
    # Faner fra Opgave 1
    tabPanel("xG: Klassificeringsmodeller",
             sidebarLayout(
               sidebarPanel(
                 selectInput("modelvalg", "Vælg model:",
                             choices = c("Logistisk Regression", "Random Forest", "Gradient Boosting")),
                 checkboxInput("vis_alle", "Vis alle modeller samtidigt", value = FALSE),
                 verbatimTextOutput("auc_output")
               ),
               mainPanel(plotOutput("roc_plot", height = "500px"))
             )
    ),
    tabPanel("xG: Spiller Rangering",
             h4("Rangering af spillere baseret på forskellen mellem xG og faktiske mål"),
             DTOutput("player_table")
    ),
    tabPanel("xG: Heatmap",
             plotOutput("xg_heatmap", height = "600px")
    ),
    
    # Faner fra Opgave 3
    tabPanel("Afleverings-Cluster",
             fluidRow(
               column(3,
                      checkboxGroupInput("selected_clusters", "Vælg klynger der skal vises:",
                                         choices = list(
                                           "1 – Midtbanespiller" = "1",
                                           "2 – Forsvar" = "2",
                                           "3 – Angriber" = "3",
                                           "4 – Målmand" = "4"
                                         ),
                                         selected = c("1", "2", "3", "4")),
                      helpText("Beskrivelse af klynger:",
                               tags$ul(
                                 tags$li("1 – Midtbanespiller: Lange afleveringer, lav volumen"),
                                 tags$li("2 – Forsvar: Korte afleveringer, høj nøjagtighed"),
                                 tags$li("3 – Angriber: Kreative afleveringer, mellemvolumen"),
                                 tags$li("4 – Målmand: Få afleveringer, lav nøjagtighed")
                               ))
               ),
               column(9,
                      plotlyOutput("cluster_plot", height = "700px")
               )
             )
    )
  )
)
# Server
server <- function(input, output, session) {
  # Klassificeringsmodeller (fra Opgave 1)
  selected_model <- reactive({
    switch(input$modelvalg,
           "Logistisk Regression" = list(roc = roc_glm, color = "blue"),
           "Random Forest" = list(roc = roc_rf, color = "red"),
           "Gradient Boosting" = list(roc = roc_gbm, color = "green"))
  })
  
  output$auc_output <- renderText({
    if (!input$vis_alle) {
      auc_val <- auc(selected_model()$roc)
      paste("AUC:", round(auc_val, 3))
    } else {
      auc_glm <- round(auc(roc_glm), 3)
      auc_rf <- round(auc(roc_rf), 3)
      auc_gbm <- round(auc(roc_gbm), 3)
      paste(
        "Logistisk Regression AUC:", auc_glm, "\n",
        "Random Forest AUC:       ", auc_rf, "\n",
        "Gradient Boosting AUC:   ", auc_gbm
      )
    }
  })
  
  output$roc_plot <- renderPlot({
    if (input$vis_alle) {
      plot(roc_glm, col = "blue", lwd = 2, main = "ROC-kurver for alle modeller")
      plot(roc_rf, col = "red", add = TRUE, lwd = 2)
      plot(roc_gbm, col = "green", add = TRUE, lwd = 2)
      legend("bottomright",
             legend = c("Logistisk Regression", "Random Forest", "Gradient Boosting"),
             col = c("blue", "red", "green"),
             lwd = 2)
    } else {
      plot(selected_model()$roc,
           col = selected_model()$color,
           lwd = 3,
           main = paste("ROC-kurve:", input$modelvalg))
    }
    abline(a = 0, b = 1, lty = 2, col = "gray")
  })
  
  # Spiller rangering
  output$player_table <- renderDT({
    datatable(
      player_xg_summary_vis,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Billede", "Spiller", "Mål", "xG", "Forskel (mål - xG)"),
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  # Heatmap
  output$xg_heatmap <- renderPlot({
    req(all_shots)
    ggplot(all_shots, aes(x = locationx, y = locationy)) +
      football_pitch() +
      stat_density_2d(
        aes(fill = ..level..),
        geom = "polygon",
        contour = TRUE,
        n = 150,
        linewidth = 0.3,
        alpha = 0.8
      ) +
      scale_fill_viridis_c(option = "plasma", name = "Skud-intensitet") +
      coord_fixed(xlim = c(0, 105), ylim = c(0, 68)) +
      labs(title = "Alle kampe – Skud Heatmap på fodboldbane", x = "", y = "") +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#228B22", color = NA),
        legend.position = "right",
        plot.title = element_text(color = "white", face = "bold", hjust = 0.5, size = 16)
      )
  })
  
  # Afleverings-cluster
  output$cluster_plot <- renderPlotly({
    plot_data <- Training %>% filter(cluster %in% input$selected_clusters)
    
    plot_ly(data = plot_data,
            x = ~avg_pass_length,
            y = ~pass_accuracy,
            z = ~avg_pass_angle,
            color = ~cluster_label,
            text = ~shortname,
            hoverinfo = "text",
            colors = brewer.pal(4, "Set1"),
            type = "scatter3d",
            mode = "markers") %>%
      layout(title = "3D Cluster Visualisering af Spillere",
             scene = list(
               xaxis = list(title = "Afleveringslængde"),
               yaxis = list(title = "Afleveringsnøjagtighed"),
               zaxis = list(title = "Afleveringvinkel")
             ))
  })
}
shinyApp(ui, server)

