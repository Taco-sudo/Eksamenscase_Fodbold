#### OPGAVE 1 - Expected Goals Model (xG model) ####

# Emne: Udvikling og evaluering af Expected Goals (xG) modeller for Superligaen 2023/2024  
# Dette script demonstrerer, hvordan man:  
#   1. Henter, filtrerer og forbereder fodbolddata fra en SQL-database (Wyscout via MariaDB)  
#   2. Konstruerer relevante variable som skudafstand og skudvinkel til brug i modeller  
#   3. Sammenfletter eventdata (skud, fælles info, possession) med hold- og spillerdata  
#   4. Visualiserer distributionsmønstre for skud med afstand, vinkel og kropsdel  
#   5. Træner og sammenligner tre klassificeringsmodeller:  
#        - Logistisk regression  
#        - Random Forest  
#        - Gradient Boosting  
#   6. Beregner og tilføjer xG til hver observation (skud) i datasættet  
#   7. Evaluerer modellerne med ROC-kurver og AUC-score  
#   8. Bygger et Shiny-dashboard til interaktiv visning af resultater, heatmaps og spillerstatistik  
#   9. Overfører modellen til sæson 2024/2025 og vurderer Brøndbys over-/underpræstation  
#  10. Visualiserer forskellen mellem faktisk scorede mål og forventede mål (xG) per spiller  

# Indlæser alle pakker
library(tidyverse) #Databehandling
library(dplyr) #Databehandling
library(DBI) #SQL-forbindelse
library(RMariaDB) #SQL-forbindelse
library(readr) # Til indlæsning af data
library(lubridate) # Til at arbejde me datoer
library(keyring) #SQL-forbindelse
library(data.table) #Databehandling
library(jsonlite) # Indlæs Json-filer
library(caret) #Logistisk Regression
library(viridis) # Visualisering og App
library(shiny) # Visualisering og App
library(randomForest) #Machine learning og modeller - Random Forest
library(pROC) #Machine learning og modeller - ROC Kurv
library(DT)
library(purrr) # Numerisk Vektor - Opgave 1.6
# Disse pakker danner grundlaget for hele xG-analysen og gør det muligt at hente data, bearbejde det og træne modeller.

# --------------------------------------------------------------------------------------
#### Data retrieval ####
# Hent loginoplysninger fra keyring
mysql_user <- "root"
mysql_password <- "0"

# Forbind til databasen
connection <- dbConnect(MariaDB(),
                        dbname = "bif",
                        host = "localhost",
                        user = mysql_user,
                        password = "0")

# Upload data fra CSV-filer
csv_folder <- "~/Documents/2._semester_eksamen/Data/"
csv_files <- list.files(csv_folder, pattern = "\\.csv$", full.names = TRUE)

## --------------------------------------------------------------------------------------
#### Henter alle tabeller ind i R fra SQL
connection <- dbConnect(MariaDB(),
                        dbname = "bif",
                        host = "localhost",
                        port = 3306,
                        user = mysql_user,
                        password = mysql_password)

# Lav dataframe med connection fra MySQL

df_players <- dbReadTable(connection, "wyscout_players_sl")
df_teams <- dbReadTable(connection, "wyscout_teams_sl")
df_matches <- dbReadTable(connection, "wyscout_teammatches_sl")
df_shots <- dbReadTable(connection, "wyscout_matchevents_shots_sl")
df_passes <- dbReadTable(connection, "wyscout_matchevents_passes_sl")
df_duels <- dbReadTable(connection, "wyscout_matchevents_groundduel_sl")
df_infractions <- dbReadTable(connection, "wyscout_matchevents_infractions_sl")
df_carry <- dbReadTable(connection, "wyscout_matchevents_carry_sl")
df_possession <- dbReadTable(connection, "wyscout_matchevents_possessiontypes_sl")
df_secondary <- dbReadTable(connection, "wyscout_matchevents_secondarytype_sl")
df_common <- dbReadTable(connection, "wyscout_matchevents_common_sl")
df_matchdetail <- dbReadTable(connection, "wyscout_matchdetail_base_sl")
df_matchformations <- dbReadTable(connection, "wyscout_matchformations_sl")
df_playermatches <- dbReadTable(connection, "wyscout_playermatches_sl")
df_playercareer <- dbReadTable(connection, "wyscout_playercareer_sl")
df_substitutions <- dbReadTable(connection, "wyscout_matchdetail_substitutions_sl")

# Filrerer på sæson 2023/2024 - konvertere datoer 
# Allbif_2025 bliver lavet til senere brug. 
allbif_2025 <- df_matches %>% mutate(date = as.Date(date)) %>% filter(date >= as.Date("2023-07-01") & date <= as.Date("2025-06-30"))
# DF_matches er den vi arbejder med nu
df_matches <- df_matches %>% mutate(date = as.Date(date)) %>% filter(date >= as.Date("2023-07-01") & date <= as.Date("2024-06-30"))

# Kamp-ID'er for fra sæson 2023/2024
kamp_ids_2324 <- df_matches$match_wyid

# Tjek antal kampe
print(paste("Antal unikke kampe:", length(unique(df_matches$match_wyid))))

# Filtrere øvrige tabeller på kamp-ID'er 
df_shots <- df_shots %>% filter(match_wyid %in% kamp_ids_2324)
df_passes <- df_passes %>% filter(match_wyid %in% kamp_ids_2324)
#df_duels <- df_duels %>% filter(match_wyid %in% kamp_ids_2324)
df_infractions <- df_infractions %>% filter(match_wyid %in% kamp_ids_2324)
df_carry <- df_carry %>% filter(match_wyid %in% kamp_ids_2324)
df_possession <- df_possession %>% filter(match_wyid %in% kamp_ids_2324)
df_secondary <- df_secondary %>% filter(match_wyid %in% kamp_ids_2324)
df_common <- df_common %>% filter(match_wyid %in% kamp_ids_2324)
df_matchdetail <- df_matchdetail %>% filter(match_wyid %in% kamp_ids_2324)
df_matchformations <- df_matchformations %>% filter(match_wyid %in% kamp_ids_2324)
df_playermatches <- df_playermatches %>% filter(match_wyid %in% kamp_ids_2324)
df_substitutions <- df_substitutions %>% filter(match_wyid %in% kamp_ids_2324)

# --------------------------------------------------------------------------------------
#### Opgave 1.1 - Opdeling i trænings- og testdata for skud #### 

# Opdel i trænings- og testdata
set.seed(123)
train_indices <- sample(1:nrow(df_matches), 0.8 * nrow(df_matches))
train_data <- df_matches[train_indices, ]
test_data <- df_matches[-train_indices, ]

# --------------------------------------------------------------------------------------
#### Opgave 1.2 - Forklarende variable, forklaring og grafiske illustrationer beskrivende statistik #### 
# Saml data i én dataframe
Allteams <- df_shots %>%
  inner_join(df_common, by = c("match_wyid", "event_wyid")) %>%
  inner_join(df_possession, by = c("match_wyid", "event_wyid")) %>%
  # Join for team_wyid → holdet der laver skuddet
  inner_join(
    df_teams %>% select(team_wyid, teamname) %>% 
    distinct(team_wyid, .keep_all = TRUE),
    by = "team_wyid"
  ) %>%
  inner_join(
    df_teams %>% select(team_wyid, teamname) %>% 
    distinct(team_wyid, .keep_all = TRUE) %>%
    dplyr::rename(teamname_opponent = teamname),
    by = c("opponentteam_wyid" = "team_wyid")
  )

# Laver en dataframe kun med Brøndby
allbif <- Allteams %>%
  filter(teamname == "Brøndby")

# Beregn længde og bredde
# Udregningen af variablen angle
get_angle <- function(x, y, goal_x = 105, goal_width = 7.32) {
  # Midtpunktet for målet (typisk i midten af banen)
  goal_y_left <- 34 - goal_width / 2
  goal_y_right <- 34 + goal_width / 2
  
  # Afstande fra skud til hver målstolpe
  a <- sqrt((goal_x - x)^2 + (goal_y_left - y)^2)
  b <- sqrt((goal_x - x)^2 + (goal_y_right - y)^2)
  c <- goal_width
  
  # Brug cosinusrelation til at beregne vinkel
  angle <- acos((a^2 + b^2 - c^2) / (2 * a * b))
  
  return(angle * (180 / pi))  # omregnet til grader
}

# sqrt kvadratroden 
Allteams$distance <- sqrt((105 - Allteams$locationx)^2 + (34 - Allteams$locationy)^2)
# mapply for at få angle på alle teams locations, kald getangle på hver locations, 
#get angel er en funktion (for i in), # istedet for for loop bruger du mapply
Allteams$angle <- mapply(get_angle, Allteams$locationx, Allteams$locationy)

# Visualiser vores udvalgte variabler
# Distance histogram og plot
# Kan plot distance og mål så man kan se om det koliderer 
# Lav histogrammet først
hist(Allteams$distance,
     breaks = seq(0, 75, by = 5),
     col = "lightblue",
     main = "Histogram af afstand til mål",
     xlab = "Afstand (meter)",
     xaxt = "n"  # Fjern standard x-aksen
)

# Tilføj x-aksen manuelt i trin af 5
axis(side = 1, at = seq(0, 75, by = 5))
# Angle histogram og plot
# Kan plot distance og mål og se om de koliderer
hist(Allteams$angle,
     breaks = seq( 0, 60, by = 5),
     col = "lightblue",
     main = "Histogram af vinkel til mål",
     xlab = "Vinkel/grader",
     #xaxt = "n"  # Fjern standard x-aksen
)
# Bodypart og mål
#Histogram og plot
# Lav en frekvenstabel over skud fordelt på kropsdel
bodypart_counts <- table(Allteams$shotbodypart)

# Lav barplot
barplot(bodypart_counts,
        col = "lightblue",
        main = "Antal skud pr. kropsdel",
        xlab = "Kropsdel",
        ylab = "Antal skud",
        las = 1)  # las = 1 gør x-aksen labels vandrette



# --------------------------------------------------------------------------------------
#### Opgave 1.3 - Forklarende variable og effekt på om et skud bliver til mål ####
#Distance og Mål
ggplot(Allteams, aes(x = distance, fill = as.factor(shotisgoal))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  scale_fill_manual(
    name = "Mål",
    values = c("0" = "red", "1" = "limegreen"),
    labels = c("Ingen mål", "Mål")
  ) +
  labs(
    title = "De fleste mål scores fra kort afstand",
    x = "Distance til mål (meter)",
    y = "Antal skud"
  ) +
  theme_minimal(base_size = 16) +  # Øger generel tekststørrelse
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# Vinkel og Mål
ggplot(Allteams, aes(x = angle, fill = as.factor(shotisgoal))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  scale_fill_manual(
    name = "Mål",
    values = c("0" = "red", "1" = "green"),
    labels = c("Ingen mål", "Mål")
  ) +
  labs(
    title = "De fleste mål og skud kommer fra 8–10 meters afstand.",
    x = "Vinkel til mål (grader)",
    y = "Antal skud"
  ) +
  theme_minimal(base_size = 16) +  # Gør al tekst større
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

# Kropsdel og Mål
ggplot(Allteams, aes(x = shotbodypart, fill = as.factor(shotisgoal))) +
  geom_bar(position = "identity", alpha = 0.6) +
  scale_fill_manual(
    name = "Mål",
    values = c("0" = "red", "1" = "green"),
    labels = c("Ingen mål", "Mål")
  ) +
  labs(
    title = "Højrefodsskud er de mest effektive afslutninger",
    x = "Krop til mål (grader)",
    y = "Antal skud"
  ) +
  theme_minimal(base_size = 16) +  # Gør al tekst større
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13))

# --------------------------------------------------------------------------------------
#### Opgave 1.4 - Forudsige om et givent skud bliver til et mål ####
# Opdel i trænings- og testdata med xG, distance, angle
set.seed(123)
# Klassificerings modeller og plots 
# Beregn xG - Brug shotbodypart, distance og angle
xg_data <- Allteams %>%
  filter(!is.na(shotisgoal), 
         !is.na(distance), 
         !is.na(angle), 
         !is.na(shotbodypart))
xg_data$shotisgoal <- as.factor(xg_data$shotisgoal) #Sørger for klassifikation (mål/ikke mål)

# Træner modellen med train - genereal machine learning metode 
# Vi kender dataen fordi vi ved hvornår der er mål og ikke
xg_model <- train(
  shotisgoal ~ distance + angle + shotbodypart,
  data = xg_data,
  method = "glm",
  family = binomial,
  trControl = trainControl(method = "cv", number = 5) #Antallet af opdelingen af data, den træner 5 modeller
)

# Brug modellen til at forudsige xG for alle skud
Allteams$xG <- predict(xg_model, newdata = Allteams, type = "prob")[, "1"]

#Sørg for at shotisgoal er en factor 
Allteams$shotisgoal <- as.factor(Allteams$shotisgoal)


# Logistisk Regression
bif_model_glm <- train(
  shotisgoal ~ distance + angle + shotbodypart,
  data = Allteams,
  method = "glm",
  family = binomial,
  trControl = trainControl(method = "cv", number = 5)
)
# Jeg prøvede at lave en anden glm model som ikke er fra chat
prøve = glm(shotisgoal ~ distance, data=Allteams, family=binomial)

# Lav manuelt en tabel som vi kan tilføje til sidst i vores shiny app
glm_tabel <- data_frame(Metric = c("Accuracy", "Kappa"), 
                        Værdi= c("88.8%", "0.101"))

#########################################################
# Random forrest
bif_model_rf <- train(
  shotisgoal ~ distance + angle + shotbodypart,
  data = Allteams,
  importance = TRUE,
  ntree = 10,
  do.trace = 100)

  #method = "rf",
  #trControl = trainControl(method = "cv", number = 5)
#)

# Ny model hvor vi har tilføjet angle og shotbodypart
model_rf <- randomForest(shotisgoal ~ distance + angle + shotbodypart, data = Allteams, importance = TRUE, ntree = 500, mtry = 2, do.trace = 100)

# Lav manuelt en tabel som man derefter kan blive tilføjet i en shiny app
rf_tabel <- data.frame(
  Ntree = c("100", "200", "300", "400", "500"),
  OOB = c("15.00%", "14.69%", "14.73%", "14.71%", "14.69%"),
  Mål = c("5.50%", "5.04%", "5.12%", "5.12%", "5.09%"),
  Ikke.mål = c("87.06%", "87.89%", "87.68%", "87.47%", "87.47%")
)
#Den vigtigeste variabel? 
varImpPlot(model_rf, main = "Vigtighed af variabler – Random Forest")

#########################################################
#Gradient Boosting
bif_model_gbm <- train(
  shotisgoal ~ distance + angle + shotbodypart,
  data = Allteams,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 5),
  verbose = FALSE
)
#Gradient Boosting
bif_model_gbm <- train(
  shotisgoal ~ distance + angle + shotbodypart + attackflank,
  data = Allteams,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 5),
  verbose = FALSE
)

#Gradient Boosting
bif_model_gbm <- train(
  shotisgoal ~ distance + angle + shotbodypart + shotgoalzone,
  data = Allteams,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 5),
  verbose = FALSE
)

# Sammenlign machine learning/klassificerings modellerne med en ROC-kurv
split <- sample(1:nrow(Allteams), 0.8 * nrow(Allteams))
train_data <- Allteams[split, ]
test_data <- Allteams[-split, ]

# Foudsig test data
probs_glm <- predict(bif_model_glm, newdata = test_data, type = "prob")[, "1"]
probs_rf  <- predict(bif_model_rf, newdata = test_data, type = "prob")[, "1"]
probs_gbm <- predict(bif_model_gbm, newdata = test_data, type = "prob")[, "1"]

# Beregn ROC og AUC
roc_glm <- roc(test_data$shotisgoal, probs_glm)
roc_rf  <- roc(test_data$shotisgoal, probs_rf)
roc_gbm <- roc(test_data$shotisgoal, probs_gbm)

# Plot ROC
plot(roc_glm, col = "blue", lwd = 2, main = "Variablerne følges ad")
plot(roc_rf, col = "red", add = TRUE, lwd = 2)
plot(roc_gbm, col = "green", add = TRUE, lwd = 2)
legend("bottomright",
       legend = c(
         paste("Logistisk Regression (AUC:", round(auc(roc_glm), 3), ")"),
         paste("Random Forest (AUC:", round(auc(roc_rf), 3), ")"),
         paste("Gradient Boosting (AUC:", round(auc(roc_gbm), 3), ")")
       ),
       col = c("blue", "red", "green"),
       lwd = 2)

# ROC curves for 3 modeller (vi antager de allerede er kørt)

roc_glm <- roc(test_data$shotisgoal, probs_glm)
roc_rf <- roc(test_data$shotisgoal, probs_rf)
roc_gbm <- roc(test_data$shotisgoal, probs_gbm)
# UI
ui <- fluidPage(
  titlePanel("xG – Sammenligning af klassificeringsmodeller"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("modelvalg", "Vælg model:",
                  choices = c("Logistisk Regression", "Random Forest", "Gradient Boosting")),
      checkboxInput("vis_alle", "Vis alle modeller samtidigt", value = FALSE),
      verbatimTextOutput("auc_output"),
      br(),
      p("Modellerne er trænet på samme datasæt og testet mod det samme testdatasæt."),
      p("Alle modeller bruger variablerne: distance, angle og shotbodypart.")
    ),
    
    mainPanel(
      plotOutput("roc_plot", height = "500px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
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
      plot(
        selected_model()$roc,
        col = selected_model()$color,
        lwd = 3,
        main = paste("ROC-kurve:", input$modelvalg)
      )
    }
    abline(a = 0, b = 1, lty = 2, col = "gray")
  })
}

# Kør appen
shinyApp(ui, server)

# Grafisk illustration af de forklarende variabler - heatmap 
# Filtrér skud
all_shots <- Allteams %>%
  filter(!is.na(locationx), !is.na(locationy), !is.na(xG))

# Simpel
ggplot(all_shots, aes(x = locationx, y = locationy)) +
  football_pitch() +  # Tegn banen i baggrunden
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
  labs(
    title = "Alle kampe – Skud tættere på mål har højere sandsynlighed for scoring",
    x = "", y = ""
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#228B22", color = NA),
    legend.position = "right",
    plot.title = element_text(color = "white", face = "bold", hjust = 0.5, size = 16)
  )


# --------------------------------------------------------------------------------------
#### Opgave 1.5 - Konklusioner på xG model
# Shiny UI og Server struktur
ui <- fluidPage(
  titlePanel("Brøndby IF - xG Analyse Dashboard"),
  sidebarLayout(
    sidebarPanel(
      actionButton("load_data", "Indlæs Data og Beregn xG"),
      br(),
      br(),
      h4("Modeller"),
      verbatimTextOutput("model_info")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("xG Heatmap", plotOutput("xg_heatmap", height = "600px")),
        tabPanel("Afstand & Vinkel", 
                 plotOutput("plot_distance"),
                 plotOutput("plot_angle")),
        tabPanel("Kropsdel & Mål", 
                 plotOutput("plot_bodypart"),
                 plotOutput("plot_bodypart_ratio")),
        tabPanel("Random Forest Vigtighed", plotOutput("rf_importance"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  rf_model <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    connection <- dbConnect(MariaDB(),
                            dbname = "bif",
                            host = "localhost",
                            user = "root",
                            password = "Beckenborg15")
    
    df_shots <- dbReadTable(connection, "wyscout_matchevents_shots_sl")
    df_common <- dbReadTable(connection, "wyscout_matchevents_common_sl")
    df_possession <- dbReadTable(connection, "wyscout_matchevents_possessiontypes_sl")
    df_teams <- dbReadTable(connection, "wyscout_teams_sl")
    
    Allteams <- df_shots %>%
      inner_join(df_common, by = c("match_wyid", "event_wyid")) %>%
      inner_join(df_possession, by = c("match_wyid", "event_wyid")) %>%
      inner_join(
        df_teams %>%
          select(team_wyid, teamname) %>%
          distinct(team_wyid, .keep_all = TRUE),
        by = "team_wyid"
      ) %>%
      inner_join(
        df_teams %>%
          select(team_wyid, teamname) %>%
          distinct(team_wyid, .keep_all = TRUE) %>%
          as_tibble()
)
        by = c("opponentteam_wyid" = "team_wyid")
    
    get_angle <- function(x, y, goal_x = 105, goal_width = 7.32) {
      goal_y_left <- 34 - goal_width / 2
      goal_y_right <- 34 + goal_width / 2
      a <- sqrt((goal_x - x)^2 + (goal_y_left - y)^2)
      b <- sqrt((goal_x - x)^2 + (goal_y_right - y)^2)
      c <- goal_width
      angle <- acos((a^2 + b^2 - c^2) / (2 * a * b))
      return(angle * (180 / pi))
    }
    
    Allteams$distance <- sqrt((105 - Allteams$locationx)^2 + (34 - Allteams$locationy)^2)
    Allteams$angle <- mapply(get_angle, Allteams$locationx, Allteams$locationy)
    Allteams$shotisgoal <- as.factor(Allteams$shotisgoal)
    
    xg_data <- Allteams %>% filter(!is.na(shotisgoal), !is.na(distance), !is.na(angle), !is.na(shotbodypart))
    
    xg_model <- train(
      shotisgoal ~ distance + angle + shotbodypart,
      data = xg_data,
      method = "glm",
      family = binomial,
      trControl = trainControl(method = "cv", number = 5)
    )
    
    Allteams$xG <- predict(xg_model, newdata = Allteams, type = "prob")[, "1"]
    
    rf <- randomForest(shotisgoal ~ distance + angle + shotbodypart, data = Allteams, ntree = 500, importance = TRUE)
    rf_model(rf)
    
    data(Allteams)
    dbDisconnect(connection)
  })
  
  output$xg_heatmap <- renderPlot({
    req(data())
    ggplot(data(), aes(x = locationx, y = locationy)) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE, weight = data()$xG) +
      scale_fill_viridis_c(option = "magma") +
      coord_fixed() +
      theme_minimal() +
      labs(title = "xG Heatmap", x = "X", y = "Y", fill = "xG-intensitet")
  })
}


shinyApp(ui, server)

# --------------------------------------------------------------------------------------
#### Opgave 1.6 - I virkeligheden ####
# Formålet med denne del af opgaven er at overføre vores xG-model til en praktisk, virkelighedsnær kontekst, hvor Brøndby IF potentielt kan bruge resultaterne til performanceanalyse og strategisk beslutningstagning.
# Indhent data forfra, og få 2025 med
df_players_2025 <- dbReadTable(connection, "wyscout_players_sl")
df_teams_2025 <- dbReadTable(connection, "wyscout_teams_sl")
df_matches_2025 <- dbReadTable(connection, "wyscout_teammatches_sl")
df_shots_2025 <- dbReadTable(connection, "wyscout_matchevents_shots_sl")
df_passes_2025 <- dbReadTable(connection, "wyscout_matchevents_passes_sl")
df_duels_2025 <- dbReadTable(connection, "wyscout_matchevents_groundduel_sl")
df_infractions_2025 <- dbReadTable(connection, "wyscout_matchevents_infractions_sl")
df_carry_2025 <- dbReadTable(connection, "wyscout_matchevents_carry_sl")
df_possession_2025 <- dbReadTable(connection, "wyscout_matchevents_possessiontypes_sl")
df_secondary_2025 <- dbReadTable(connection, "wyscout_matchevents_secondarytype_sl")
df_common_2025 <- dbReadTable(connection, "wyscout_matchevents_common_sl")
df_matchdetail_2025 <- dbReadTable(connection, "wyscout_matchdetail_base_sl")
df_matchformations_2025 <- dbReadTable(connection, "wyscout_matchformations_sl")
df_playermatches_2025 <- dbReadTable(connection, "wyscout_playermatches_sl")
df_playercareer_2025 <- dbReadTable(connection, "wyscout_playercareer_sl")
df_substitutions_2025 <- dbReadTable(connection, "wyscout_matchdetail_substitutions_sl")

# Filrerer på sæson 2023/2024 - konvertere datoer 
# Allbif_2025 bliver lavet til senere brug. 
allbif_2025 <- df_matches_2025 %>% mutate(date = as.Date(date)) %>% filter(date >= as.Date("2023-07-01") & date <= as.Date("2025-06-30"))

# Kamp-ID'er for fra sæson 2023/2024
kamp_ids_2025 <- df_matches_2025$match_wyid

# Tjek antal kampe
print(paste("Antal unikke kampe:", length(unique(df_matches_2025$match_wyid))))

# Filtrere øvrige tabeller på kamp-ID'er 
df_shots_2025 <- df_shots_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_passes_2025 <- df_passes_2025 %>% filter(match_wyid %in% kamp_ids_2324)
#df_duels_2025 <- df_duels_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_infractions_2025 <- df_infractions_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_carry_2025 <- df_carry_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_possession_2025 <- df_possession_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_secondary_2025 <- df_secondary_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_common_2025 <- df_common_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_matchdetail_2025 <- df_matchdetail_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_matchformations_2025 <- df_matchformations_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_playermatches_2025 <- df_playermatches_2025 %>% filter(match_wyid %in% kamp_ids_2324)
df_substitutions_2025 <- df_substitutions_2025 %>% filter(match_wyid %in% kamp_ids_2324)

# Saml data i én dataframe for 2025
Allteams_2025 <- df_shots_2025 %>%
  inner_join(df_common_2025, by = c("match_wyid", "event_wyid")) %>%
  inner_join(df_possession_2025, by = c("match_wyid", "event_wyid")) %>%
  # Join for team_wyid → holdet der laver skuddet
  inner_join(
    df_teams_2025 %>% select(team_wyid, teamname) %>% 
      distinct(team_wyid, .keep_all = TRUE),
    by = "team_wyid"
  ) %>%
  inner_join(
    df_teams_2025 %>% select(team_wyid, teamname) %>% 
      distinct(team_wyid, .keep_all = TRUE) %>%
      dplyr::rename(teamname_opponent = teamname),
    by = c("opponentteam_wyid" = "team_wyid")
  )

#FIltrer Brøndby og lav distance/angle
Allteams_2025$distance <- sqrt((105 - Allteams_2025$locationx)^2 + (34 - Allteams_2025$locationy)^2)
#get angel er en funktion (for i in), # istedet for for loop bruger du mapply
Allteams_2025$angle <- mapply(get_angle, Allteams_2025$locationx, Allteams_2025$locationy)

#Brug din tidligere trænede xg_model fra opgave 1.4
Allteams_2025$xG <- predict(xg_model, newdata = Allteams_2025, type = "prob")[, "1"]

# Laver en dataframe kun med Brøndby
allbif_2025 <- Allteams_2025 %>%
  filter(teamname == "Brøndby")

brondby_summary <- allbif_2025 %>%
  summarise(
    total_skud = n(),
    faktiske_maal = sum(as.numeric(as.character(shotisgoal)), na.rm = TRUE),
    samlet_xG = sum(xG, na.rm = TRUE),
    xG_per_goal = samlet_xG / faktiske_maal
  )

# Visualisering 
plot_data <- tibble(
  Kategori = c("Faktiske mål", "xG"),
  Antal = c(55, 40.25)
)

ggplot(plot_data, aes(x = Kategori, y = Antal, fill = Kategori)) +
  geom_col(width = 0.4) +
  scale_fill_manual(values = c("red", "green")) +
  labs(
    title = "Brøndby IF har scoret flere mål, end forventet (2023–2025)",
    y = "Antal mål", x = "", fill = ""
  ) +
  theme_minimal(base_size = 16) +  # Øger generel tekststørrelse
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

# Ranger fra over- til underpræstationer
# Gruppér per spiller
# Tilføj spillerens navn fra df_players_2025
allbif_2025 <- allbif_2025 %>%
  left_join(
    df_players_2025 %>%
      select(player_wyid, shortname) %>%
      distinct(player_wyid, .keep_all = TRUE),
    by = "player_wyid"
  )

# Først: Lav en opsummering per spiller
player_xg_summary <- allbif_2025 %>%
  group_by(player_wyid, spiller_navn = shortname) %>%
  summarise(
    mål = sum(as.numeric(as.character(shotisgoal)), na.rm = TRUE),
    samlet_xG = sum(xG, na.rm = TRUE),
    difference = mål - samlet_xG,
    .groups = "drop"
  )

# xG for spillere
# Tilføj spillerbilleder
player_xg_summary <- player_xg_summary %>%
  left_join(
    df_players_2025 %>% 
      select(player_wyid, imagedataurl) %>%
      distinct(player_wyid, .keep_all = TRUE),
    by = "player_wyid"
  )


# Vis de 10 største overpræstationer
head(player_xg_summary, 5)

# Vis de 10 største underpræstationer
tail(player_xg_summary, 5)

# Lav en shiny app hvor man kan se rangeringen og billeder
# Slå spillerbilleder på
player_xg_summary_vis <- player_xg_summary %>%
  left_join(
    df_players_2025 %>%
      select(player_wyid, billede_url = imagedataurl) %>%
      distinct(player_wyid, .keep_all = TRUE),
    by = "player_wyid"
  ) %>%
  mutate(
    Billede = paste0('<img src="', billede_url, '" height="60"></img>')
  ) %>%
  select(Billede, spiller_navn, mål, samlet_xG, difference) %>%
  arrange(desc(difference))



# -------------------------------
# SHINY APP
# -------------------------------

ui <- fluidPage(
  titlePanel("Brøndby IF – xG vs. Faktiske Mål (2023–2025)"),
  h4("Rangering af spillere baseret på forskellen mellem xG og faktiske mål"),
  DTOutput("player_table")
)

server <- function(input, output, session) {
  output$player_table <- renderDT({
    datatable(
      player_xg_summary_vis,
      escape = FALSE,  # Så billeder kan vises
      rownames = FALSE,
      colnames = c("Billede", "Spiller", "Mål", "xG", "Forskel (mål - xG)"),
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      )
    )
  })
}

shinyApp(ui, server)

#Shiny app samlet
ui <- fluidPage(
  titlePanel("Brøndby IF xG Dashboard"),
  
  tabsetPanel(
    tabPanel("Klassificeringsmodeller",
             sidebarLayout(
               sidebarPanel(
                 selectInput("modelvalg", "Vælg model:",
                             choices = c("Logistisk Regression", "Random Forest", "Gradient Boosting")),
                 checkboxInput("vis_alle", "Vis alle modeller samtidigt", value = FALSE),
                 verbatimTextOutput("auc_output"),
                 br(),
                 p("Modellerne er trænet på samme datasæt og testet mod det samme testdatasæt."),
                 p("Alle modeller bruger variablerne: distance, angle og shotbodypart.")
               ),
               mainPanel(
                 plotOutput("roc_plot", height = "500px")
               )
             )
    ),
    
    tabPanel("Spiller Rangering",
             h4("Rangering af spillere baseret på forskellen mellem xG og faktiske mål"),
             DTOutput("player_table")
    ),
    
    tabPanel("xG Heatmap",
             plotOutput("xg_heatmap", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  # Klassificeringsmodeller
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
      plot(
        selected_model()$roc,
        col = selected_model()$color,
        lwd = 3,
        main = paste("ROC-kurve:", input$modelvalg)
      )
    }
    abline(a = 0, b = 1, lty = 2, col = "gray")
  })
  
  # Spillerbilleder og rangering
  output$player_table <- renderDT({
    datatable(
      player_xg_summary_vis,
      escape = FALSE,
      rownames = FALSE,
      colnames = c("Billede", "Spiller", "Mål", "xG", "Forskel (mål - xG)"),
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      )
    )
  })
  
  # xG Heatmap
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
      labs(
        title = "Alle kampe – Skud Heatmap på fodboldbane",
        x = "", y = ""
      ) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#228B22", color = NA),
        legend.position = "right",
        plot.title = element_text(color = "white", face = "bold", hjust = 0.5, size = 16)
      )
  })
}

shinyApp(ui, server)

# --------------------------------------------------------------------------------------
