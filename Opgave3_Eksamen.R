#### Opgave 3 ####
#Kombinér dataframes df_passes, df_players, df_possession, 
library(tidyverse)
library(dplyr)
library(DBI)
library(RMariaDB) # Skab forbindelse til MySQL
library(readr) 
library(keyring)
#install.packages("keyring")
library(data.table)
library(jsonlite) # Indlæs Json-filer
library(caret) # Logistisk Regression
library(viridis)
library(shiny) # Genere Shiny App
library(randomForest) # Jeg ved ikke om denne giver mening at bruge
library(pROC) # Lav en ROC kurv
#install.packages("fmsb")
library(fmsb)
library(readr) #Bruges til indlæsning af csv-filer
library(jsonlite)
#install.packages("ggsoccer")
library(ggsoccer)

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

# Filrerer på sæson 2023 til 2025 - konvertere datoer 
df_matches23_25 <- df_matches %>% mutate(date = as.Date(date)) %>% filter(date >= as.Date("2023-07-01") & date <= as.Date("2025-06-01"))


# Kamp-ID'er for fra sæson 2023 til igangværende sæson 2025
kamp_ids_2325 <- df_matches23_25$match_wyid

# Tjek antal kampe
print(paste("Antal unikke kampe:", length(unique(df_matches23_25$match_wyid))))

# Filtrere øvrige tabeller på kamp-ID'er 
df_shots <- df_shots %>% filter(match_wyid %in% kamp_ids_2325)
df_passes <- df_passes %>% filter(match_wyid %in% kamp_ids_2325)
#df_duels <- df_duels %>% filter(match_wyid %in% kamp_ids_2325)
df_infractions <- df_infractions %>% filter(match_wyid %in% kamp_ids_2325)
df_carry <- df_carry %>% filter(match_wyid %in% kamp_ids_2325)
df_possession <- df_possession %>% filter(match_wyid %in% kamp_ids_2325)
df_secondary <- df_secondary %>% filter(match_wyid %in% kamp_ids_2325)
df_common <- df_common %>% filter(match_wyid %in% kamp_ids_2325)
df_matchdetail <- df_matchdetail %>% filter(match_wyid %in% kamp_ids_2325)
df_matchformations <- df_matchformations %>% filter(match_wyid %in% kamp_ids_2325)
df_playermatches <- df_playermatches %>% filter(match_wyid %in% kamp_ids_2325)
df_substitutions <- df_substitutions %>% filter(match_wyid %in% kamp_ids_2325)
df_players_season <- df_players %>% filter(player_wyid %in% df_playermatches$player_wyid)
#Check om antal unikke kampe i df_passes stemmer overens med df_matches23_25
print(paste("Antal unikke kampe:", length(unique(df_passes$match_wyid))))
  #NOTE - Der mangler to kampe. Linje 31 siger 325 kampe og linje 48 siger 323 kampe.
  #Vi fortsætter men det skal med i overvejelserne!


#Vi undersøgte om det kun var afleveringer der manglede eller om det var alt info der mangler for de to kampe ved at undersøge shots. 
#Vi kan afgøre at al data mangler for de pågældende kampe og det vil kræve en større undersøgelse for at få mere info omkring de to kampe. 

## Nu laves der et datasæt for afleveringer
#Da df_common og df_possession er ret store datasæt filtrere vi nu på relevant data for afleveringer
df_passes_filtered <- df_passes %>% 
  filter(primarytype %in% c("pass", "corner", "free_kick","goal_kick"))
df_common_filtered <- df_common %>%
  filter(primarytype %in% c("pass", "corner", "free_kick","goal_kick"))
df_possession_filtered <- df_possession %>% 
  filter(primarytype %in% c("pass", "corner", "free_kick","goal_kick"))
df_players_filtered <- df_players %>% 
  select("shortname", "rolename", "player_wyid", "competition_wyid", "season_wyid")
df_teams_filtered <- df_teams %>% 
  select("team_wyid", "season_wyid", "competition_wyid", "officialname")

#Spillere og klubber gentages kun 1 gang da det ellers vil resulterer i mange duplikationer
df_players_filtered <- df_players_season %>%
  group_by(player_wyid) %>%
  slice(1)

df_teams_filtered <- df_teams_filtered %>%
  group_by(team_wyid) %>%
  slice(1)

### Nu laves der et datasæt for afleveringer

df_pass_complete <- df_passes_filtered %>%
  left_join(df_common_filtered, by = c("match_wyid", "event_wyid")) %>%
  left_join(df_possession_filtered, by = c("match_wyid", "event_wyid")) %>%
  left_join(df_players_filtered, by = "player_wyid") %>%
  left_join(df_teams_filtered, by = "team_wyid")

#Grundet join måden der er blevet brugt er der duplikationer i kolonnerne så dem fjernes nu. 
df_pass_complete <- df_pass_complete %>%
  select(-matches("\\.x$"), -matches("\\.y$"))




# Fjerner rækker med NA eller 0 i player_wyid aka spillere der ikke findes
df_pass_complete <- df_pass_complete %>% filter(!is.na(player_wyid) & player_wyid !=0)

#Oversigt af relevante informationer til visualiseringer senere
pass_summary <- df_pass_complete %>%
  group_by(player_wyid, shortname, rolename, team_wyid, officialname) %>%
  summarise(
    avg_pass_length = mean(length, na.rm = TRUE),
    avg_pass_angle = mean(angle, na.rm = TRUE),
    pass_accuracy = mean(accurate, na.rm = TRUE),
    total_passes = n(),
    .groups = "drop"
  )


#Fjerner spillere med under 20 afleveringer da det kan give et afvisende virkelighedsbillede af holdets accuracy %. 
#(Hvis en spiller har 1 aflevering der nåede frem har han en 100% accuracy hvilket påvirker den samlede hold-accuracy)
pass_summary <- pass_summary %>% filter(total_passes >=20)


##### SABINE LAV VISUALISERINGER #####  

##### Opgave 3.1 #####

#Lav en Cluster model for afleveringer. 
#Sæt sigende overskrift på hvert cluster

# Udvælg numeriske variabler fra afleveringsdata

df_pass_complete$primarytype_num <- as.numeric(as.factor(df_pass_complete$primarytype))
df_pass_complete$rolename_num <- as.numeric(as.factor(df_pass_complete$rolename))

cor_matrix_pass <- cor(df_pass_complete %>% select(accurate, angle, length, primarytype_num), use = "pairwise.complete.obs")
print(cor_matrix_pass)

cor_matrix_pass <- cor(pass_summary %>% select(avg_pass_length,avg_pass_angle,pass_accuracy,total_passes, rolename_num), use = "pairwise.complete.obs")
print(cor_matrix_pass)

pass_summary$rolename_num <- as.numeric(as.factor(pass_summary$rolename))

#Normalisering
funk_norm <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
 
rm(Training)
Training <- pass_summary %>% select(avg_pass_length)
Training$avg_pass_length <- funk_norm(pass_summary$avg_pass_length)
#Training$avg_pass_angle <- funk_norm(pass_summary$avg_pass_angle)
Training$pass_accuracy <- funk_norm(pass_summary$pass_accuracy)
Training$total_passes <- funk_norm(pass_summary$total_passes)

set.seed(123)
pass_clusters <- kmeans(Training, centers = 4, nstart = 10)

# Tilføjer cluster labels til dataframe
Training$cluster <- as.factor(pass_clusters$cluster)
Training$rolename <- pass_summary$rolename


ggplot() +
  geom_point(data = Training, aes(x = total_passes, y = pass_accuracy, color = cluster), alpha = 0.6, size = 3) +
  geom_point(data = as.data.frame(pass_clusters$centers), aes(x = total_passes, y = pass_accuracy), 
             color = "black", size = 5, shape = 23, fill = "gold") +  # Cluster centers markeret anderledes
  labs(title = "Cluster af Spillere baseret på Afleveringsnøjagtighed og Afleveringer i alt",
       x = "Afleveringer i alt", y = "Afleveringsnøjagtighed") +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = "Set1")  # Bruger kvalitative farver

library(plotly)

# Opret 3D scatter plot
centers_df <- as.data.frame(pass_clusters$centers)
names(centers_df) <- c("avg_pass_length", "pass_accuracy", "avg_pass_angle")  # Tilpas disse navne efter dine faktiske datakolonner

plot_ly(data = Training, x = ~avg_pass_length, y = ~pass_accuracy, z = ~avg_pass_angle, color = ~cluster, 
        colors = RColorBrewer::brewer.pal(4, "Set1"), type = "scatter3d", mode = 'markers') %>%
  layout(title = "3D Cluster Visualisering af Spillere",
         scene = list(xaxis = list(title = "Afleveringslængde"),
                      yaxis = list(title = "Afleveringsnøjagtighed"),
                      zaxis = list(title = "Afleveringvinkel")))



##### Opgave 3.2 #####

#Start med indlæsning af filerne


vbob.csv <- read_csv("Documents/2._semester_eksamen/Data/vbob.csv", locale = locale(encoding = "UTF-8"))
#Indhenter meta data'en fra de udleverede filer
json_meta <- fromJSON(paste(readLines("Documents/2._semester_eksamen/Data/vbob-meta.json"), collapse=""))
#
json_data <- fromJSON(paste(readLines("Documents/2._semester_eksamen/Data/vbob-data.json"), collapse=""))

###### Første mål ######

# Isolere det første mål i kampen 
#Minut 41 via online research. Isolere data i min. 41 og 42 via 
#min*sek = gameClock sekund.
Foerste_maal <- json_data %>%
  filter(gameClock >= 2460, gameClock < 2520)

# Finder specifik frame frem hvor bolden går ind.
specifik_frame <- Foerste_maal %>%
  filter(frameIdx == 62598) #mellem frame 62573 og 62598. 

# Forbered spillerdata for hjemme- og udehold
OB_spillere <- specifik_frame$homePlayers[[1]] %>%
  mutate(
    PlayerX = sapply(xyz, function(pos) (pos[1] + 52.5)/105*100),
    PlayerY = sapply(xyz, function(pos) (pos[2] + 34)/68*100),
    team = "OB"
  ) %>%
  left_join(json_meta$homePlayers, by = c("playerId" = "ssiId"))

Viborg_spillere <- specifik_frame$awayPlayers[[1]] %>%
  mutate(
    PlayerX = sapply(xyz, function(pos) (pos[1] + 52.5)/105*100),
    PlayerY = sapply(xyz, function(pos) (pos[2] + 34)/68*100),
    team = "Viborg"
  ) %>%
  left_join(json_meta$awayPlayers, by = c("playerId" = "ssiId"))

Spiller_positioner <- bind_rows(OB_spillere, Viborg_spillere)

# Tilføjer boldens position
bold_position <- specifik_frame$ball$xyz[[1]]

# Plotter fodboldbanen ved hjælp af ggsoccer pakken
pitch <- ggplot() +
  annotate_pitch(colour = "white", fill = "forestgreen") +
  geom_point(data = Spiller_positioner, aes(x = PlayerX, y = PlayerY, color = team), size = 4) +
  geom_text(data = Spiller_positioner, aes(x = PlayerX, y = PlayerY, label = name), vjust = 1.5, color = "white", size = 3) +
  geom_point(aes(x = (bold_position[1] + 52.5)/105*100, y = (bold_position[2] + 34)/68*100), color = "yellow", size = 5) +
  scale_color_manual(values = c("OB" = "blue", "Viborg" = "red")) +
  theme_minimal() +
  labs(title = "Positioner for 1. mål") +
  theme_void() +
  theme(legend.position = "right", legend.title = element_blank())  # Viser hjemme- og udehold i højre side

print(pitch)

###### 3.2. 5) Lav funktioner til at tegne skygger ######

#Hældningsformlen til trekant udregnes og implementeres i funktion (get_linje)
#Funktionen er til at beregne hældningen og skæringspunktet for en linje givet to punkter

get_linje <- function(x0, y0, x1, y1) {
  a <- (y0 - y1) / (x0 - x1) 
  b <- y1 - (a * x1)
  c <- (-1 / a)
  return(c(a, b, c))
}

# Funktion til at finde de to punkter i trekanten ud for medspilleren. 
find_point_on_line <- function(x1, y1, m, s) {
  dx <- s / sqrt(1 + m^2)
  dy <- m * dx
  x2_pos <- x1 + dx
  y2_pos <- y1 + dy
  x2_neg <- x1 - dx
  y2_neg <- y1 - dy
  return(c(x2_pos, y2_pos, x2_neg, y2_neg))
}

# Funktion trækker data'en ud fra mine df'er som de ovenstående hjælpefunktioner skal bruge. 
#Den kalder på de to funktioner og sætter dem 'pænt op'. 
get_trekant <- function(afsender, modtager) {
  linje <- get_linje(afsender$PlayerX, afsender$PlayerY, modtager$PlayerX, modtager$PlayerY)
  punkter <- find_point_on_line(modtager$PlayerX, modtager$PlayerY, linje[3], 2)
  return(data.frame(
    x = c(afsender$PlayerX, punkter[1], punkter[3]),
    y = c(afsender$PlayerY, punkter[2], punkter[4])
  ))
}

# Hjælpefunktion til at finde tegn af det produkt, der angiver, om punkterne er på samme side af en kant
#Den kigger på er det her punkt (modspiller) inden i trekanten (den som er udarbejdet ovenstående) som den så siger ja eller nej
same_side <- function(px1, py1, px2, py2, ax, ay, bx, by) {
  # Find to vektorprodukter mellem trekantkanten ab og punkterne p1, p2
  cp1 <- (bx - ax) * (py1 - ay) - (by - ay) * (px1 - ax)
  cp2 <- (bx - ax) * (py2 - ay) - (by - ay) * (px2 - ax)
  # Hvis produktet er positivt er punkterne på samme side, og hvis det er 0 er de på linjen
  return(cp1 * cp2 >= 0) #Hvis den er negativ ligger den udenfor, er den lig med 0 ligger den på linje og hvis den er positiv er den indenfor. 
}

# Funktion til at tjekke, om punktet er indenfor trekanten
#Nedenfor kode fremhæver de spilningsmuligheder hvor der er en modspiller i vejen. 
#Hvis man ønsker at finde de spilninger som er frie skal man negere resultatet. Dette kommenteres i linje 332. 
punkt_within <- function(punkt, trekant) {
  # Punktskoordinater aka modspillerens position. 
  px <- punkt$PlayerX
  py <- punkt$PlayerY
  
  # Trekantens hjørner dannes. 3 punkter og hver punkt har 2 koordinator. 
  x1 <- trekant$x[1]
  y1 <- trekant$y[1]
  x2 <- trekant$x[2]
  y2 <- trekant$y[2]
  x3 <- trekant$x[3]
  y3 <- trekant$y[3]
  
  # Tjek om punktet ligger på den samme side for alle tre kanter af trekanten
  # Kaldet på same_side for at se om punktet ligger inden for trekanten der er dannet. 
  # En trekant kan dannes på tre forskellige måder og derfor tjekkes same_side på 3 gange. 
  side1 <- same_side(px, py, x3, y3, x1, y1, x2, y2)
  side2 <- same_side(px, py, x1, y1, x2, y2, x3, y3)
  side3 <- same_side(px, py, x2, y2, x3, y3, x1, y1)
  
  # Hvis punktet (modspilleren) er på den samme side for alle tre trekantsider, og eller direkte på nogen af linjerne, er det indenfor
  return(side1 && side2 && side3)
}


# Funktion til at hente trekanter for en given spiller
get_trekanter_for_spiller <- function(afsender) {
  result <- data.frame(x = numeric(), y = numeric(), group_id =integer(), stringsAsFactors = FALSE)
  group_counter <- 1 #starter en tæller for grupperId'er 
  for (modtager in 1:nrow(OB_spillere)) {
    if (modtager == afsender) next
    
    trekant <- get_trekant(OB_spillere[afsender, ], OB_spillere[modtager, ])
    trekant$group_id <- group_counter 
    group_counter <- group_counter + 1
  
    #for (modstander in 1:nrow(Viborg_spillere)) {
     # if (punkt_within(Viborg_spillere[modstander, ], trekant)) { 
        #Hvis man vil have de spilninger uden modspiller i vejen 
      #  result <- rbind(result, trekant)
      #  break #Der skal bare være 1 modspiller i trekanten. Ikke nødvendigt at finde flere. 
      #}
    #}
    alle_udenfor <- TRUE
    for (modstander in 1:nrow(Viborg_spillere)) {
      alle_udenfor <- alle_udenfor && !punkt_within(Viborg_spillere[modstander, ], trekant)
    }
    if (alle_udenfor) {
      result <- rbind(result, trekant)
    }
  }
  return(result)
}

# Eksekver funktionen for en specifik spiller
trekanter <- get_trekanter_for_spiller(1)

#Nu plottes der med shadowtrekanter.
#
plot_shadow_trekanter <- function(OB_spillere, Viborg_spillere, trekanter, bold_position) {
  # Start med at oprette plot med en fodboldbane
  p <- ggplot() +
    annotate_pitch(colour = "white", fill = "forestgreen") +
    theme_void() +
    labs(title = "Spilmuligheder ud fra OB hjørnespark")
  
  # Kombiner spillernes data til én dataframe og tilføj holdfarver
  all_players <- bind_rows(
    OB_spillere %>% mutate(team = "OB"),
    Viborg_spillere %>% mutate(team = "Viborg")
  )
  
  # Tilføj spillere og deres navne til plottet
  if (!is.null(all_players) && "PlayerX" %in% names(all_players)) {
    p <- p +
      geom_point(data = all_players, aes(x = PlayerX, y = PlayerY, color = team), size = 4) +
      geom_text(data = all_players, aes(x = PlayerX, y = PlayerY, label = name), vjust = -1.5, color = "white", size = 3)
  }
  
  # Tilføj boldens position som et gult punkt
  if (!is.null(bold_position)) {
    p <- p +
      geom_point(aes(x = (bold_position[1] + 52.5)/105*100, y = (bold_position[2] + 34)/68*100), color = "yellow", size = 5)
  }
  
  # Tilføj trekanter for skyggeeffekt, hvis de findes
  if (!is.null(trekanter) && nrow(trekanter) > 0) {
    p <- p +
      geom_polygon(data = trekanter, aes(x = x, y = y, group = group_id), fill = "grey", alpha = 0.5)
  }
  
  # Definer farver for hvert hold og tilpas legendens placering og udseende
  p <- p +
    scale_color_manual(values = c("OB" = "blue", "Viborg" = "red")) +
    theme(legend.position = "right", legend.title = element_blank())
  
  # Vis plottet
  print(p)
}

# Gør brug af denne opdaterede funktion
plot_shadow_trekanter(OB_spillere, Viborg_spillere, trekanter, bold_position)

