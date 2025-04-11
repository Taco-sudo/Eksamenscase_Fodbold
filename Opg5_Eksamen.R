#### Opgave 5  - Eksamen ####
# Load nødvendige biblioteker
library(shiny) # Visualisering i Shiny-app
library(ggplot2) # Plot
library(dplyr) # Data manipulation
library(DT) # Data tabeller
library(StatsBombR) # Indhent statsbomb data
library(ggsoccer) # Plot soccer event data
library(patchwork) # Placere plots ved siden af hinanden
# Opgave 5.1 --------------------------------------------------------------------------------------
# Beskrivende statistik og visualisering  ---------------------------------------------------------
### Find statistisk belæg for, at der er forskel på mænd og kvinder når det drejer sig om fodbold.
# Indlæs data fra Statsbomb - Turneringer og kampe
competitions <- FreeCompetitions() # Henter alle turneringer
matches <- FreeMatches(competitions) # Henter alle kampe

# Kvinder
# Filtrer VM 2023 relateret til kvinder
vm_kvinder <- competitions %>% 
  filter(competition_name == "Women's World Cup" & season_name == "2023")
# Henter alle VM kampe for kvinder
vm_kampe_kvinder <- FreeMatches(vm_kvinder)
# Afleveringer & Skud - Henter events som vi skal bruge for kvindernes VM kampe
vm_events_kvinder <- free_allevents(MatchesDF = vm_kampe_kvinder)

# Mænd
# Filtrer VM 2023 relateret til mænd
vm_mænd <- competitions %>% 
  filter(competition_name == "FIFA World Cup" & season_name == "2022")
# Henter alle VM kampe for mænd
vm_kampe_mænd <- FreeMatches(vm_mænd)
# Afleveringer & Skud - Henter events som vi skal bruge for mændenes VM kampe
vm_events_mænd <- free_allevents(MatchesDF = vm_kampe_mænd)

# Filtrering af pass (afleveringer) for kvinder og mænd 
# Kvinder
afleveringer_kvinder <- subset(vm_events_kvinder, type.name == "Pass" & (is.na(pass.outcome.name) | pass.outcome.name != "Unknown"), select = c(type.name, pass.outcome.name))
# Sammenligning og fordelingen på gode og dårlige afleveringer
# Acurate/Gode afleveringer
kvinder_accurate_afleveringer <- sum(is.na(afleveringer_kvinder$pass.outcome.name))
# Accurate/Dårlige afleveringer
kvinder_inaccurate_afleveringer <- sum(!is.na(afleveringer_kvinder$pass.outcome.name))
# Vis resultaterne
print(paste("Antal Præcise Afleveringer for Kvinder:", kvinder_accurate_afleveringer))
print(paste("Antal Fejlafleveringer for Kvinder:", kvinder_inaccurate_afleveringer))

# Mænd
afleveringer_mænd <- subset(vm_events_mænd, type.name == "Pass" & (is.na(pass.outcome.name) | pass.outcome.name != "Unknown"), select = c(type.name, pass.outcome.name))
# Sammenligning og fordelingen på gode og dårlige afleveringer
# Acurate/Gode afleveringer
afleveringer_accurate_mænd <- sum(is.na(afleveringer_mænd$pass.outcome.name))
afleveringer_inaccurate_mænd <- sum(!is.na(afleveringer_mænd$pass.outcome.name))
# Vis resultaterne
print(paste("Antal Præcise Afleveringer for Mænd:", afleveringer_accurate_mænd))
print(paste("Antal Fejlafleveringer for Mænd:", afleveringer_inaccurate_mænd))

# Sammenlign hvor mange missede skud der er hos kvinder kontra mænd
# Kvinder
skud_kvinder <- subset(vm_events_kvinder, type.name == "Shot", select = c(type.name, shot.outcome.name))
# Vis resultatet 
summary(skud_kvinder)
# Visuelt resultat af resultaterne af kategorierne missede skud
skud_resultat_kvinder <- table(skud_kvinder$shot.outcome.name)
# Vis resultaterne 
print(skud_resultat_kvinder)
# Antal missede skud for kvinder
missede_skud_kvinder <- sum(skud_resultat_kvinder[c("Off T", "Post", "Saved Off Target", "Wayward", "Blocked")])
succesfulde_skud_kvinder <- sum(skud_resultat_kvinder[c("Goal", "Saved", "Saved to Post")])
print(paste("Antal missede skud for kvinder:", missede_skud_kvinder))
print(paste("Antal skud på mål for kvinder:", succesfulde_skud_kvinder))

# Mænd 
skud_mænd <- subset(vm_events_mænd, type.name == "Shot", select = c(type.name, shot.outcome.name))
# Vis resultatet 
summary(skud_mænd$shot.outcome.name)
# Visuelt resultat af resultaterne af kategorierne missede skud
skud_resultat_mænd <- table(skud_mænd$shot.outcome.name)
# Vis resultatet
print(skud_resultat_mænd)
# Antal missede skud for mænd
missede_skud_mænd <- sum(skud_resultat_mænd[c("Off T", "Post", "Saved Off Target", "Wayward", "Blocked")])
succesfulde_skud_mænd <- sum(skud_resultat_mænd[c("Goal", "Saved", "Saved to Post")])
print(paste("Antal missede skud for mænd:", missede_skud_mænd))
print(paste("Antal skud på mål for mænd:", succesfulde_skud_mænd))

# Lav en dataframe med skuddata
skud_data <- data.frame(
  Køn = c(rep("Kvinder", 2), rep("Mænd", 2), "Kvinder", "Mænd"),
  Skudtype = c(rep(c("Missede skud", "Skud på mål"), 2), "Samlet antal skud", "Samlet antal skud"),
  Antal = c(missede_skud_kvinder, succesfulde_skud_kvinder, missede_skud_mænd, succesfulde_skud_mænd, sum(c(missede_skud_kvinder, succesfulde_skud_kvinder)), sum(c(missede_skud_mænd, succesfulde_skud_mænd)))
)

# Plot - Skud
ggplot(skud_data, aes(x = Køn, y = Antal, fill = Skudtype, label = Antal)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +
  geom_text(position = position_dodge(width = 0.9), aes(label = paste(Antal)), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Kvinder afgiver flere skud end mænd",
       x = "Køn", y = "Antal") +
  scale_fill_manual(values = c("Missede skud" = "#FF5733", "Skud på mål" = "#2E8B57", "Samlet antal skud" = "gray")) + # Definerer farven for den samlede antal skud søjle
  ylim(0, 1800) +  # Angiver y-aksegrænser
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
        axis.title = element_text(family = "Arial", size = 14, color = "black"),
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_text(family = "Arial", size = 12, color = "black"),
        legend.text = element_text(family = "Arial", size = 10, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"),
        legend.position = "bottom")

# Beregn procenter af skuddata
# Fjern 'Samlet antal skud' for at beregne procent af rigtige skud
skud_data_procent <- skud_data %>%
  filter(Skudtype != "Samlet antal skud") %>%
  group_by(Køn) %>%
  mutate(Procent = round((Antal / sum(Antal)) * 100, 1)) %>%
  ungroup()
# Lav et plot med procenter
ggplot(skud_data_procent, aes(x = Køn, y = Procent, fill = Skudtype, label = paste0(Procent, "%"))) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.85) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 8
            , color = "black") +
  labs(
    title = "Præcisionsraten er næsten identisk hos kvinder og mænd",
    x = "Køn",
    y = "Andel i %"
  ) +
  scale_fill_manual(values = c("Missede skud" = "#FF5733", "Skud på mål" = "#2E8B57")) +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
    axis.title = element_text(family = "Arial", size = 14, color = "black"),
    axis.text = element_text(family = "Arial", size = 12, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Arial", size = 10, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F4F4F4"),
    plot.background = element_rect(fill = "#F4F4F4"),
    legend.position = "bottom"
  )

# Afleveringer
# Lav en dataframe med afleveringer 
afleveringer_data <- data.frame(
  Køn = c(rep("Kvinder", 2), rep("Mænd", 2), "Kvinder", "Mænd"),
  Afleveringstype = c(rep(c("Fejlafleveringer", "Præcise afleveringer"), 2), "Samlet antal afleveringer", "Samlet antal afleveringer"),
  Antal = c(kvinder_inaccurate_afleveringer, kvinder_accurate_afleveringer, afleveringer_inaccurate_mænd, afleveringer_accurate_mænd, kvinder_accurate_afleveringer + kvinder_inaccurate_afleveringer, afleveringer_accurate_mænd + afleveringer_inaccurate_mænd)
)

# Beregn procenter for fejlafleveringer og præciseafleveringer med samlet afleveringer
afleveringer_data <- afleveringer_data %>%
  group_by(Køn) %>%
  mutate(Procent = round((Antal / max(Antal)) * 100, 1)) %>%
  ungroup()

# Lav et plot af aleveringer
# Plot - Afleveringer
ggplot(afleveringer_data, aes(x = Køn, y = Antal, fill = Afleveringstype, label = paste0(Procent, "%"))) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 6, color = "black") +  # Gør teksten større
  labs(title = "Mænd har højere præcisionsrate end kvinder",
       x = "Køn", y = "Antal afleveringer") +
  scale_fill_manual(values = c("Præcise afleveringer" = "#2E8B57", 
                               "Fejlafleveringer" = "#FF5733", 
                               "Samlet antal afleveringer" = "gray")) + 
  ylim(0, 70000) +
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
        axis.title = element_text(family = "Arial", size = 14, color = "black"),
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 10, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"),
        legend.position = "bottom")

# Antal frispark (pass.type.name)
# Mænd - frispark
frispark_mænd <- subset(vm_events_mænd, pass.type.name == "Free Kick", select = c(pass.type.name))
antal_frispark_mænd <- nrow(frispark_mænd)

# Kvinder - frispark
frispark_kvinder <- subset(vm_events_kvinder, pass.type.name == "Free Kick", select = c(pass.type.name))
antal_frispark_kvinder <- nrow(frispark_kvinder)

# Dataframe KUN med frispark
frispark_data <- data.frame(
  Køn = c("Kvinder", "Mænd"),
  Antal = c(antal_frispark_kvinder, antal_frispark_mænd)
)

# Plot - Frispark
ggplot(frispark_data, aes(x = Køn, y = Antal, fill = Køn, label = Antal)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(vjust = -0.5, size = 5, color = "black") +
  labs(title = "Mænd har flere frispark",
       x = "Køn", y = "Antal frispark") +
  ylim(0, max(frispark_data$Antal) * 1.2) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
    axis.title = element_text(family = "Arial", size = 14),
    axis.text = element_text(family = "Arial", size = 12),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F4F4F4"),
    plot.background = element_rect(fill = "#F4F4F4")
  )

# Lad os kigge lidt på gule og røde kort? 
#Mænd
kort_mænd <- subset(vm_events_mænd, !is.na(foul_committed.card.name), select = c(foul_committed.card.name))
kort_mænd_tabel <- table(kort_mænd$foul_committed.card.name)
# Kvinder
# Kvinder - Kort
kort_kvinder <- subset(vm_events_kvinder, !is.na(foul_committed.card.name), select = c(foul_committed.card.name))
kort_kvinder_tabel <- table(kort_kvinder$foul_committed.card.name)
# Ændre kolonne navne
colnames(kort_kvinder) <- c("Kort")
# Antal af gule og røde kort 
# Mænd 
Gule_kort_mænd <- kort_mænd_tabel["Yellow Card"]
Røde_kort_mænd <- kort_mænd_tabel["Red Card"]
#Ændre kolonne navne
colnames(kort_mænd) <- c("Kort")
# Kvinder 
Gule_kort_kvinder <- kort_kvinder_tabel["Yellow Card"]
Røde_kort_kvinder <- kort_kvinder_tabel["Red Card"]
#Lav en sammenligning og DF med kortene 
Kort_for_begge_køn <- rbind(Gule_kort_kvinder, Røde_kort_kvinder, Gule_kort_mænd, Røde_kort_mænd)
Kort_for_begge_køn <- as.data.frame(Kort_for_begge_køn)

# Fejl (Fouls) - Kvinder
fejl_kvinder <- subset(vm_events_kvinder, type.name == "Foul Committed")
antal_fejl_kvinder <- nrow(fejl_kvinder)

# Fejl (Fouls) - Mænd
fejl_mænd <- subset(vm_events_mænd, type.name == "Foul Committed")
antal_fejl_mænd <- nrow(fejl_mænd)



# Tacklinger for kvinder og mænd
# Kvinder
tacklinger_kvinder <- subset(vm_events_kvinder, duel.type.name == "Tackle", select = c(duel.type.name))
antal_tacklinger_kvinder <- nrow(tacklinger_kvinder)
# Mænd
tacklinger_mænd <- subset(vm_events_mænd, duel.type.name == "Tackle", select = c(duel.type.name))
antal_tacklinger_mænd <- nrow(tacklinger_mænd)

# Lav en dataframe for røde og gule kort, mænd og kvinder
Tacklinger_og_kort <- data.frame(
  Køn = c("Kvinder", "Mænd", "Kvinder", "Mænd", "Kvinder", "Mænd", "Kvinder", "Mænd"),
  Type = c("Tackles", "Tackles", "Fouls", "Fouls", "Gule kort", "Gule kort", "Røde kort", "Røde kort"),
  Antal = c(antal_tacklinger_kvinder, antal_tacklinger_mænd, antal_fejl_kvinder, antal_fejl_mænd, Gule_kort_kvinder, Gule_kort_mænd, Røde_kort_kvinder, Røde_kort_mænd)
)

# Plot af tacklinger, fejl og kort med mænd og kvinder
ggplot(Tacklinger_og_kort, aes(x = Køn, y = Antal, fill = factor(Type, levels = c("Tackles", "Fouls", "Gule kort", "Røde kort")), label = Antal)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 8, color = "black") +
  labs(title = "Kvinder foretager flere tacklinger, men mænd begår flere fejl",
       x = "Køn", y = "Antal", fill = "Type") +
  ylim(0, max(Tacklinger_og_kort$Antal) * 1.2) +  # Angiver y-aksegrænser
  scale_fill_manual(values = c("Tackles" = "#2E8B57", "Fouls" = "gray", "Gule kort" = "yellow", "Røde kort" = "#FF5733")) + # Definerer farver for hver type
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
        axis.title = element_text(family = "Arial", size = 14, color = "black"),
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 10, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"))

## Vi placerer plots ved siden af hinanden, for et bedre indblik på fejl, og tacklinger
# Plot 1: Frispark
plot_frispark <- ggplot(frispark_data, aes(x = Køn, y = Antal, fill = Køn, label = Antal)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(vjust = -0.5, size = 5, color = "black") +
  labs(title = "Mænd har flere frispark",
       x = "Køn", y = "Antal frispark") +
  ylim(0, max(frispark_data$Antal) * 1.2) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
    axis.title = element_text(family = "Arial", size = 14),
    axis.text = element_text(family = "Arial", size = 12),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F4F4F4"),
    plot.background = element_rect(fill = "#F4F4F4")
  )

# Plot 2: Tacklinger, fejl og kort
plot_kort <- ggplot(Tacklinger_og_kort, aes(x = Køn, y = Antal, fill = factor(Type, levels = c("Tackles", "Fouls", "Gule kort", "Røde kort")), label = Antal)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 8, color = "black") +
  labs(title = "Kvinder foretager flere tacklinger, men mænd begår flere fejl",
       x = "Køn", y = "Antal", fill = "Type") +
  ylim(0, max(Tacklinger_og_kort$Antal) * 1.2) +
  scale_fill_manual(values = c("Tackles" = "steelblue1", "Fouls" = "steelblue2", "Gule kort" = "steelblue3", "Røde kort" = "steelblue4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
    axis.title = element_text(family = "Arial", size = 14, color = "black"),
    axis.text = element_text(family = "Arial", size = 12, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Arial", size = 10, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F4F4F4"),
    plot.background = element_rect(fill = "#F4F4F4")
  )
plot_kort <- plot_kort + 
  theme(
    plot.title = element_text(size = 16, lineheight = 1.1)
  )

# Placer dem ved siden af hinanden med patchwork biblioteket
plot_frispark + plot_kort 

## Vi laver en freeze frame for at belyse 
Kvinder_freezeframe <- vm_events_kvinder %>% filter(type.name == "Shot")
# Filtrer skud til at lave freezeframe for kvinder
skud_kvinderFF <- Kvinder_freezeframe %>%
  filter(type.name %in% c("Shot")) %>%
  select(where(~ !all(is.na(.))))
# Split location og shot.end_location til to kolonner
Kvinder_freezeframe <- Kvinder_freezeframe %>%
  filter(!is.null(location)) %>%
  rowwise() %>%
  mutate(
    location_x = location[[1]],
    location_y = location[[2]],
    shot_end_location_x = if (!is.null(shot.end_location)) shot.end_location[[1]] else NA_real_,
    shot_end_location_y = if (!is.null(shot.end_location)) shot.end_location[[2]] else NA_real_
  )
# Beregn afstanden mellem location og shot.end_location
Kvinder_freezeframe <- Kvinder_freezeframe %>%
  mutate(afstand = sqrt((location_x - shot_end_location_x)^2 + (location_y - shot_end_location_y)^2))
# Behold samme kolonner som i dataframen Kvinder_freezeframe
Kvinder_freezeframe <- select(Kvinder_freezeframe,
                              shot.end_location, shot.freeze_frame, player.id, player.name, 
                              position.id, position.name, team.id, team.name, location, 
                              location_x, location_y, shot_end_location_x, shot_end_location_y, afstand)


# Freeze Frame --------------------------------------------------------------------------------------
# Freeze frame for kvinder
skud_ramme_kvinder <- Kvinder_freezeframe[905,]
FF_kvinder <- skud_ramme_kvinder$shot.freeze_frame[[1]] %>% 
  rowwise() %>% mutate(location.x = location[1], location.y = location[2])

# Lav en funktion til at udtrække efternavne
Efternavne_kvinder <- function(full_name) {
  Efternavne <- tail(strsplit(full_name, " ")[[1]], 1)
  return(Efternavne)
}
# Tilføjer kolonnen efternavne til FF_kvinder dataframen
FF_kvinder$last.name <- sapply(FF_kvinder$player.name, Efternavne_kvinder)

# Trekant - skud for kvinder
Skud_triangle_kvinder <- function(Kvinder_freezeframe) {
  g1 <- c(120, 36)
  g2 <- c(120, 44)
  x <- c(Kvinder_freezeframe[1], g1[1], g2[1], Kvinder_freezeframe[1])
  y <- c(Kvinder_freezeframe[2], g1[2], g2[2], Kvinder_freezeframe[2])
  res_skud <- data.frame(x, y)
  colnames(res_skud) <- c("sx", "sy")
  return(res_skud)
}
# Opret et trekantobjekt baseret på skudlokationerne for kvinder
Trekant_Skud_kvinder <- Skud_triangle_kvinder(unlist(Kvinder_freezeframe$location))

# Funktion til at oprette trekantdata for en given spiller baseret på spillerens position
Skud_spiller_triangle_kvinder <- function(player_location_x, player_location_y) {
  g1 <- c(120, 36)
  g2 <- c(120, 44)
  x <- c(player_location_x, g1[1], g2[1], player_location_x)
  y <- c(player_location_y, g1[2], g2[2], player_location_y)
  data.frame(sx = x, sy = y)
}

# Opret trekantdata for alle medspillere (teammates)
# Tilføj en unik identifikator til hver trekant baseret på spillerens navn eller en anden unik egenskab
Spiller_trekant_kvinder <- lapply(1:nrow(FF_kvinder), function(i) {
  if (FF_kvinder$teammate[i]) {
    Trekant_kvinder <- Skud_spiller_triangle_kvinder(FF_kvinder$location.x[i], FF_kvinder$location.y[i])
    Trekant_kvinder$player_name <- FF_kvinder$player.name[i] # Brug spillerens navn som identifikator
    return(Trekant_kvinder)
  } else {
    return(NULL)
  }
})
# Fjern NULL værdier fra listen og konverter til en data.frame, derefter fjern duplikater
Spiller_trekant_kvinder <- lapply(Spiller_trekant_kvinder, function(df) if (!is.null(df)) df[!duplicated(df), ] else NULL)
Spiller_trekant_kvinder_DF <- do.call(rbind, Spiller_trekant_kvinder)
Spiller_trekant_kvinder_DF$triangle_id <- rep(1:nrow(FF_kvinder[FF_kvinder$teammate, ]), each = 3)

Trekants_område <- function(x1, y1, x2, y2, x3, y3) {
  # Calculate the area of the triangle using the coordinates
  area <- 0.5 * abs(x1*(y2 - y3) + x2*(y3 - y1) + x3*(y1 - y2))
  return(area)
}

# Ny funktion til at tælle modstandere inde i trekanten
Modstander_trekant_kvinder <- function(ff_data_kvinder, shooter_location) {
  Total_trekant_kvinder <- Trekants_område(120, 44, 120, 36, shooter_location[1], shooter_location[2])
  
  Modstander_i_trekantsområde_kvinder <- sum(sapply(1:nrow(ff_data_kvinder), function(i) {
    if (!ff_data_kvinder$teammate[i]) {
      Modstander_lokation_kvinder <- c(ff_data_kvinder$location.x[i], ff_data_kvinder$location.y[i])
      area1 <- Trekants_område(Modstander_lokation_kvinder[1], Modstander_lokation_kvinder[2], 120, 36, shooter_location[1], shooter_location[2])
      area2 <- Trekants_område(120, 44, Modstander_lokation_kvinder[1], Modstander_lokation_kvinder[2], shooter_location[1], shooter_location[2])
      area3 <- Trekants_område(120, 44, 120, 36, Modstander_lokation_kvinder[1], Modstander_lokation_kvinder[2])
      trisum <- area1 + area2 + area3
      return(abs(trisum - Total_trekant_kvinder) < 0.1)
    }
    return(FALSE)
  }))
  return(Modstander_i_trekantsområde_kvinder)
}


# Tilføjer antallet af modstandere inden for trekanten til hver spiller i FF_kvinder
FF_kvinder$Modstander_i_trekantsområde_kvinder <- sapply(1:nrow(FF_kvinder), function(i) {
  if (FF_kvinder$teammate[i]) {
    Modstander_trekant_kvinder(FF_kvinder, c(FF_kvinder$location.x[i], FF_kvinder$location.y[i]))
  } else {
    0
  }
})


# Beregner antallet af modstandere inden for trekanten for den skydende spiller
Spiller_modstander_i_trekant_kvinder <- Modstander_trekant_kvinder(FF_kvinder, c(102, 38))
# Find den laveste værdi af antallet af modspillere for alle medspillere
Svageste_modstander_kvinder <- min(FF_kvinder$Modstander_i_trekantsområde_kvinder[FF_kvinder$teammate])

# Plot medspiller med færrest modspiller i en trekant 
shooter_position <- c(102, 38)
shooter_opponents <- Spiller_modstander_i_trekant_kvinder

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "green4") +
  
  geom_polygon(data = Trekant_Skud_kvinder, aes(x = sx, y = sy),
               alpha = 0.2, fill = "white", color = alpha("lightblue", 0.35)) +
  
  geom_polygon(data = Spiller_trekant_kvinder_DF,
               aes(x = sx, y = sy, group = triangle_id),
               alpha = 0.1, fill = "white", color = alpha("lightblue", 0.35)) +
  
  geom_point(data = FF_kvinder, aes(x = location.x, y = location.y, color = teammate), size = 3) +
  geom_point(aes(x = shooter_position[1], y = shooter_position[2]), size = 5, color = "blue3") +
  
  theme_pitch() +
  labs(
    title = "FreezeFrame - Overblik over mest fordelagtige position",
    subtitle = "Navne vises KUN, hvis medspilleren har færre modspillere i sin trekant"
  ) +
  coord_flip(xlim = c(85, 130)) +
  
  # Vis navne kun hvis medspilleren har færre modspillere end skytten
  geom_text(
    data = FF_kvinder[
      (FF_kvinder$teammate & FF_kvinder$Modstander_i_trekantsområde_kvinder < shooter_opponents) |
        FF_kvinder$position.name == "Goalkeeper", ],
    aes(x = location.x, y = location.y, label = last.name),
    size = 2.5, vjust = 2
  ) +
  
  # Label skytten
  geom_text(aes(x = shooter_position[1], y = shooter_position[2], label = Kvinder_freezeframe$player.name[905]),
            size = 4, vjust = 2) +
  
  scale_color_manual(values = c("red", "lightblue1"), labels = c("Modspiller", "Medspiller")) +
  guides(color = guide_legend(title = "")) +
  
  # Antal modspillere i trekant for hver medspiller
  geom_text(data = FF_kvinder[FF_kvinder$teammate, ],
            aes(x = location.x, y = location.y, label = Modstander_i_trekantsområde_kvinder),
            size = 3, vjust = 0.5, color = "black") +
  
  # Antal modspillere i skyttens trekant
  geom_text(aes(x = shooter_position[1], y = shooter_position[2], label = shooter_opponents),
            size = 4, vjust = 0.5, color = "black") +
  
  # Pil fra skytten til bedste medspiller
  geom_segment(
    data = FF_kvinder[FF_kvinder$teammate & FF_kvinder$Modstander_i_trekantsområde_kvinder == Svageste_modstander_kvinder, ],
    aes(xend = location.x, yend = location.y,
        x = shooter_position[1], y = shooter_position[2]),
    arrow = arrow(type = "open", length = unit(0, "inches")),
    color = "yellow", linetype = "dotted", alpha = 1
  ) +
  theme(legend.position = c(0.033, 0.98))


# Plot færre spillere end spiller
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "steelblue4") +
  geom_polygon(data = Trekant_Skud_kvinder, aes(x = sx, y = sy), alpha = 0.2, fill = "white", color = alpha("lightblue", 0.35)) +
  geom_polygon(data = Spiller_trekant_kvinder_DF, aes(x = sx, y = sy, group = triangle_id), alpha = 0.1, fill = "white", color = alpha("lightblue", 0.35)) +
  geom_point(data = FF_kvinder, aes(x = location.x, y = location.y, color = teammate), size = 3) +
  geom_point(data = skud_ramme_kvinder, aes(x = location_x, y = location_y), size = 4, color = "green2") +
  theme_pitch() +
  labs(
    title = "FreezeFrame - Overblik over mest fordelagtige position",
    subtitle = "Navne vises KUN, hvis medspilleren har færre modspillere i sin trekant"
  ) +
  coord_flip(xlim = c(85, 130)) +
  geom_text(data = FF_kvinder[(FF_kvinder$teammate & FF_kvinder$Modstander_i_trekantsområde_kvinder < Spiller_modstander_i_trekant_kvinder) |
                                FF_kvinder$position.name == "Goalkeeper",],
            aes(x = location.x, y = location.y, label = last.name),
            size = 2.5, vjust = 2) +
  geom_text(data = skud_ramme_kvinder, aes(x = location_x, y = location_y, label = player.name), size = 4, vjust = 2) +
  scale_color_manual(
    values = c("red", "lightblue1"),
    labels = c("Modspiller", "Medspiller")
  ) +
  guides(color = guide_legend(title = "", override.aes = list(size = 4))) +  # Større prikker i legend
  geom_text(data = FF_kvinder[FF_kvinder$teammate,],
            aes(x = location.x, y = location.y, label = Modstander_i_trekantsområde_kvinder),
            size = 3, vjust = 0.5, color = "black") +
  geom_text(data = skud_ramme_kvinder,
            aes(x = location_x, y = location_y, label = Spiller_modstander_i_trekant_kvinder),
            size = 4, vjust = 0.5, color = "black") +
  geom_segment(data = FF_kvinder[FF_kvinder$teammate & FF_kvinder$Modstander_i_trekantsområde_kvinder == Svageste_modstander_kvinder,],
               aes(xend = location.x, yend = location.y,
                   x = skud_ramme_kvinder$location_x, y = skud_ramme_kvinder$location_y),
               arrow = arrow(type = "open", length = unit(0, "inches")),
               color = "yellow", linetype = "dotted", alpha = 1) +
  theme(
    legend.text = element_text(size = 20),           # Gør "Modspiller" og "Medspiller" større
    plot.subtitle = element_text(size = 20),         # Gør underteksten større
    plot.title = element_text(size = 22, face = "bold"),
    legend.position = c(0.033, 0.98)
  )

# Funktion: Tæl antal modstandere i trekanten foran den skydende spiller
count_opponents_for_all_shots_kvinder <- function(Kvinder_freezeframe) {
  totals <- numeric(nrow(Kvinder_freezeframe))  # Vektor til antal modstandere for hvert skud
  
  for (i in seq_len(nrow(Kvinder_freezeframe))) {
    freeze_frame <- Kvinder_freezeframe$shot.freeze_frame[[i]]
    
    if (!is.null(freeze_frame) && nrow(freeze_frame) > 0) {
      # Tilføj x/y lokationer for hver spiller i freeze frame
      current_ff <- freeze_frame %>%
        mutate(
          location.x = map_dbl(location, 1),
          location.y = map_dbl(location, 2)
        )
      
      # Skyttens lokation
      shooter_location <- c(Kvinder_freezeframe$location_x[i], Kvinder_freezeframe$location_y[i])
      
      # Tæl antallet af modstandere i skyttens trekant
      totals[i] <- Modstander_trekant_kvinder(current_ff, shooter_location)
      
    } else {
      totals[i] <- 0
    }
  }
  
  return(totals)
}

# Freeze Frame --------------------------------------------------------------------------------------
# Kvinder
# Anvender funktionen på 'Kvinder_freezeframe' dataframen
opponents_count_kvinder <- tryCatch({
  count_opponents_for_all_shots_kvinder(Kvinder_freezeframe)
}, error = function(e) {
  message("Der opstod en fejl for kvinderne: ", e$message)
  return(rep(NA, nrow(Kvinder_freezeframe)))  # Returnér en vektor med NA'er ved fejl
})

# Tilføjer resultaterne til 'Kvinder_freezeframe' som en ny kolonne
if (!is.null(opponents_count_kvinder)) {
  Kvinder_freezeframe$Modstandere_i_trekant <- opponents_count_kvinder
}

# Beregner gennemsnittet og totalen
gennemsnit_modstandere_kvinder <- mean(Kvinder_freezeframe$Modstandere_i_trekant, na.rm = TRUE)
total_modstandere_kvinder <- sum(opponents_count_kvinder, na.rm = TRUE)

# Udskriv resultater
cat("Gennemsnit af antal modstandere i trekanten:", gennemsnit_modstandere_kvinder, "\n")
cat("Total antal modstandere i trekanten:", total_modstandere_kvinder, "\n")

# Funktion til at tælle medspillere med færre modstandere i deres trekant
count_teammates_in_better_positions_kvinder <- function(Kvinder_freezeframe) {
  total_bedre_positioner <- 0
  for (i in 1:nrow(Kvinder_freezeframe)) {
    if (!is.null(Kvinder_freezeframe$shot.freeze_frame[[i]])) {
      current_ff <- Kvinder_freezeframe$shot.freeze_frame[[i]] %>%
        mutate(
          location.x = map_dbl(location, 1),
          location.y = map_dbl(location, 2)
        )
      shooter_location <- c(Kvinder_freezeframe$location_x[i], Kvinder_freezeframe$location_y[i])
      
      for (j in 1:nrow(current_ff)) {
        if (current_ff$teammate[j]) {
          teammate_location <- c(current_ff$location.x[j], current_ff$location.y[j])
          teammate_opponents <- Modstander_trekant_kvinder(current_ff, teammate_location)
          if (teammate_opponents < Kvinder_freezeframe$Modstandere_i_trekant[i]) {
            total_bedre_positioner <- total_bedre_positioner + 1
          }
        }
      }
    }
  }
  return(total_bedre_positioner)
}

# Beregner total og gennemsnit af medspillere i bedre position
total_bedre_positioner_kvinder <- count_teammates_in_better_positions_kvinder(Kvinder_freezeframe)
cat("Samlet antal medspillere med færre modstandere i deres trekant:", total_bedre_positioner_kvinder, "\n")

gennemsnit_bedre_positioner_kvinder <- total_bedre_positioner_kvinder / nrow(Kvinder_freezeframe)
cat("Gennemsnit af medspillere med færre modstandere i deres trekant:", round(gennemsnit_bedre_positioner_kvinder, 2), "\n")
  
-------------------------------------------------------------------------------------------------
### Kig på shots og blocked ####
# Kvinder - skud
skud_kvinder <- subset(vm_events_kvinder, type.name == "Shot", select = c(shot.outcome.name))
antal_skud_kvinder <- nrow(skud_kvinder)
blokerede_skud_kvinder <- nrow(subset(skud_kvinder, shot.outcome.name == "Blocked"))

# Mænd - skud
skud_mænd <- subset(vm_events_mænd, type.name == "Shot", select = c(shot.outcome.name))
antal_skud_mænd <- nrow(skud_mænd)
blokerede_skud_mænd <- nrow(subset(skud_mænd, shot.outcome.name == "Blocked"))

# Sammenligningsdataframe
blokerede_skud_df <- data.frame(
  Køn = c("Kvinder", "Mænd"),
  Skud_total = c(antal_skud_kvinder, antal_skud_mænd),
  Blokerede_skud = c(blokerede_skud_kvinder, blokerede_skud_mænd),
  Andel_blokeret = round(c(blokerede_skud_kvinder / antal_skud_kvinder,
                           blokerede_skud_mænd / antal_skud_mænd) * 100, 1)
)

library(ggplot2)

ggplot(blokerede_skud_df, aes(x = Køn, y = Andel_blokeret, fill = Køn)) +
  geom_col(width = 0.6) +
  labs(title = "Andel af blokerede skud pr. køn",
       y = "Blokerede skud (%)",
       x = "Køn") +
  theme_minimal()

### xA Kvinder vs Mænd  --------------------------------------------------------------------------------------
#Beregn xA
# Kvinder
# 1. Find alle afleveringer som assisterede skud
pass_med_shot <- subset(vm_events_kvinder,
                        type.name == "Pass" &
                          pass.shot_assist == TRUE &
                          !is.na(pass.assisted_shot_id),
                        select = c(id, pass.assisted_shot_id, player.name, team.name))

# 2. Find alle skud, der har en xG værdi
alle_skud <- subset(vm_events_kvinder,
                    type.name == "Shot" &
                      !is.na(shot.statsbomb_xg),
                    select = c(id, shot.statsbomb_xg))

# 3. Join afleveringerne med skuddene via ID
library(dplyr)
xA_data <- left_join(pass_med_shot, alle_skud, by = c("pass.assisted_shot_id" = "id"))

# 4. Summér xA for hele holdet eller en spiller
xA_per_spiller <- xA_data %>%
  group_by(player.name) %>%
  summarise(xA = sum(shot.statsbomb_xg, na.rm = TRUE)) %>%
  arrange(desc(xA))

# 1. Filtrér afleveringer med shot_assist
pass_shot_kvinder <- subset(vm_events_kvinder,
                            type.name == "Pass" &
                              pass.shot_assist == TRUE &
                              !is.na(pass.assisted_shot_id),
                            select = c(id, pass.assisted_shot_id, player.name, team.name))

# 2. Match skud og få xG + mål
skud_kvinder <- subset(vm_events_kvinder,
                       type.name == "Shot" & !is.na(shot.statsbomb_xg),
                       select = c(id, shot.statsbomb_xg, shot.outcome.name))

# 3. Join afleveringer med skud
library(dplyr)
xa_join_kvinder <- left_join(pass_shot_kvinder, skud_kvinder,
                             by = c("pass.assisted_shot_id" = "id"))

# 4. Tilføj kategorier
xa_join_kvinder <- xa_join_kvinder %>%
  mutate(xA_kategori = ifelse(shot.statsbomb_xg > 0.3, "Høj xA", "Lav xA"),
         blev_mål = ifelse(shot.outcome.name == "Goal", "Mål", "Ikke mål"))

# 5. Tæl antal i hver kombination
tælling_kvinder <- xa_join_kvinder %>%
  group_by(xA_kategori, blev_mål) %>%
  summarise(Antal = n())

library(ggplot2)

ggplot(tælling_kvinder, aes(x = xA_kategori, y = Antal, fill = blev_mål)) +
  geom_col(position = "dodge") +
  labs(title = "Kvinder: Høj/Lav xA og om det blev til mål",
       x = "xA-kategori", y = "Antal afleveringer") +
  theme_minimal()

#Mænd

# 1. Find alle afleveringer som assisterede skud
pass_shot_mænd <- subset(vm_events_mænd,
                         type.name == "Pass" &
                           pass.shot_assist == TRUE &
                           !is.na(pass.assisted_shot_id),
                         select = c(id, pass.assisted_shot_id, player.name, team.name))

# 2. Find alle skud, der har en xG værdi
skud_mænd <- subset(vm_events_mænd,
                    type.name == "Shot" & !is.na(shot.statsbomb_xg),
                    select = c(id, shot.statsbomb_xg, shot.outcome.name))

# 3. Join afleveringer med skud
library(dplyr)
xa_join_mænd <- left_join(pass_shot_mænd, skud_mænd,
                          by = c("pass.assisted_shot_id" = "id"))

# 4. Tilføj kategorier
xa_join_mænd <- xa_join_mænd %>%
  mutate(xA_kategori = ifelse(shot.statsbomb_xg > 0.3, "Høj xA", "Lav xA"),
         blev_mål = ifelse(shot.outcome.name == "Goal", "Mål", "Ikke mål"))

# 5. Tæl antal i hver kombination
tælling_mænd <- xa_join_mænd %>%
  group_by(xA_kategori, blev_mål) %>%
  summarise(Antal = n())

# 6. Plot for mænd
ggplot(tælling_mænd, aes(x = xA_kategori, y = Antal, fill = blev_mål)) +
  geom_col(position = "dodge") +
  labs(title = "Mænd: Høj/Lav xA og om det blev til mål",
       x = "xA-kategori", y = "Antal afleveringer") +
  theme_minimal()

tælling_kvinder$Køn <- "Kvinder"
tælling_mænd$Køn <- "Mænd"

tælling_samlet <- rbind(tælling_kvinder, tælling_mænd)

tælling_samlet_fix <- tælling_samlet %>%
  tidyr::complete(Køn, xA_kategori, blev_mål, fill = list(Antal = 0)) %>%
  dplyr::group_by(Køn, xA_kategori) %>%
  dplyr::mutate(Procent = round((Antal / sum(Antal)) * 100, 1)) %>%
  dplyr::ungroup()

ggplot(tælling_samlet, aes(x = xA_kategori, y = Antal, fill = blev_mål)) +
  geom_col(position = "dodge") +
  facet_wrap(~Køn) +
  labs(title = "Høj/Lav xA og mål – Mænd vs. Kvinder",
       x = "xA-kategori", y = "Antal afleveringer") +
  theme_minimal()

# Beregn procent inden for hver xA_kategori og køn
# Tilføj 0'er til manglende kombinationer
tælling_samlet_fix <- tælling_samlet %>%
  complete(Køn, xA_kategori, blev_mål, fill = list(Antal = 0)) %>%
  group_by(Køn, xA_kategori) %>%
  mutate(Procent = round((Antal / sum(Antal)) * 100, 1))


ggplot(tælling_samlet_fix, aes(x = xA_kategori, y = Antal, fill = blev_mål)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(Procent, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3.5) +
  facet_wrap(~Køn) +
  labs(title = "Høj/Lav xA og mål – Mænd vs. Kvinder",
       x = "xA-kategori", y = "Antal afleveringer") +
  theme_minimal()



# Opgave 5.2 --------------------------------------------------------------------------------------
#### Forklarende variable, forklaring og grafiske illustrationer beskrivende statistik #### 
# Saml data i én dataframe
kun_skud_kvinder <- subset(vm_events_kvinder, type.name == "Shot")

# Find første skud hvor der ER freeze_frame data
skud_med_ff <- kun_skud_kvinder[!sapply(kun_skud_kvinder$shot.freeze_frame, is.null), ]
freeze_frame_eksempel <- skud_med_ff$shot.freeze_frame[[1]] # Tager udgangspunkt i den første kamp i dataen

# Se hvordan det ser ud
str(freeze_frame_eksempel, max.level = 1)

library(tibble)
library(purrr)

ff_df <- freeze_frame_eksempel
skytte_pos <- skud_med_ff$location[[1]]  # [x, y]
målstolpe_venstre <- c(120, 36)
målstolpe_højre <- c(120, 44)

trekant_skytten <- rbind(
  c(skytte_pos[1], skytte_pos[2]),
  målstolpe_venstre,
  målstolpe_højre
)

# Antag: ff_df, skytte_pos, trekant_skytten allerede findes

# === Klargør spillernes positioner ===
ff_df <- ff_df %>%
  mutate(x = map_dbl(location, 1),
         y = map_dbl(location, 2))

medspillere <- ff_df %>% filter(teammate == TRUE)
modspillere <- ff_df %>% filter(teammate == FALSE)

# === Find modstandere i skyttens trekant ===
modspillere$indenfor_trekant <- mapply(
  function(x, y) {
    point.in.polygon(x, y, trekant_skytten[, 1], trekant_skytten[, 2]) == 1
  },
  modspillere$x, modspillere$y
)

# === Tegn en fodboldbane (StatsBomb målestok: 0–120 længde, 0–80 bredde) ===
tegn_bane <- function() {
  list(
    # Yderbanen
    geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80), fill = "palegreen3", color = "white", linewidth = 1),
    
    # Midterlinje
    geom_segment(aes(x = 60, xend = 60, y = 0, yend = 80), color = "white", linewidth = 1),
    
    # Midtercirkel
    geom_circle(aes(x0 = 60, y0 = 40, r = 9.15), color = "white", linewidth = 1, inherit.aes = FALSE),
    
    # Straffesparksfelt (højre side = angrebsmål)
    geom_rect(aes(xmin = 102, xmax = 120, ymin = 18, ymax = 62), fill = NA, color = "white", linewidth = 1),
    
    # Målområde
    geom_rect(aes(xmin = 114, xmax = 120, ymin = 30, ymax = 50), fill = NA, color = "white", linewidth = 1),
    
    # Mål
    geom_rect(aes(xmin = 120, xmax = 122, ymin = 36, ymax = 44), fill = "white", color = "black", linewidth = 1)
  )
}

# === Plot det hele ===
ggplot() +
  # Tegn banen
  tegn_bane() +
  
  # Skyttens skudvinkel (trekant)
  geom_polygon(data = as.data.frame(trekant_skytten),
               aes(x = V1, y = V2), fill = "deepskyblue", alpha = 0.3) +
  
  # Modspillere
  geom_point(data = modspillere, aes(x = x, y = y, fill = indenfor_trekant),
             shape = 21, size = 4, color = "black") +
  
  # Medspillere
  geom_point(data = medspillere, aes(x = x, y = y), color = "green", size = 4, shape = 21, fill = "green") +
  
  # Skytten
  geom_point(aes(x = skytte_pos[1], y = skytte_pos[2]), size = 5, shape = 4, color = "black") +
  
  labs(title = "Freeze Frame: Skud og placering af spillere",
       subtitle = "Blå trekant = skyttens skudvinkel | Lilla modstandere = i trekanten",
       x = "", y = "") +
  scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "red"), guide = "none") +
  coord_fixed(xlim = c(95, 125), ylim = c(20, 60)) +  # zoom ind omkring målfelt
  theme_void()

### Mænd ###
# Saml skud fra mænd
kun_skud_mænd <- subset(vm_events_mænd, type.name == "Shot")

# Find første skud med freeze_frame
skud_med_ff_mænd <- kun_skud_mænd[!sapply(kun_skud_mænd$shot.freeze_frame, is.null), ]
freeze_frame_eksempel_mænd <- skud_med_ff_mænd$shot.freeze_frame[[1]] # Tager udgangspunkt i den første kamp i dataen

# Lav data frame med spillerdata
ff_df_mænd <- freeze_frame_eksempel_mænd
skytte_pos_mænd <- skud_med_ff_mænd$location[[1]]

målstolpe_venstre <- c(120, 36)
målstolpe_højre <- c(120, 44)

trekant_skytten_mænd <- rbind(
  c(skytte_pos_mænd[1], skytte_pos_mænd[2]),
  målstolpe_venstre,
  målstolpe_højre
)

# Klargør positionsdata
ff_df_mænd <- ff_df_mænd %>%
  mutate(x = map_dbl(location, 1),
         y = map_dbl(location, 2))

medspillere_mænd <- ff_df_mænd %>% filter(teammate == TRUE)
modspillere_mænd <- ff_df_mænd %>% filter(teammate == FALSE)

# Tjek modstandere i trekant
modspillere_mænd$indenfor_trekant <- mapply(
  function(x, y) {
    point.in.polygon(x, y, trekant_skytten_mænd[, 1], trekant_skytten_mænd[, 2]) == 1
  },
  modspillere_mænd$x, modspillere_mænd$y
)

# Plot for mænd
ggplot() +
  tegn_bane() +  # samme bane-funktion som før
  
  # Skyttens skudvinkel (trekant)
  geom_polygon(data = as.data.frame(trekant_skytten_mænd),
               aes(x = V1, y = V2), fill = "deepskyblue", alpha = 0.3) +
  
  # Modspillere
  geom_point(data = modspillere_mænd, aes(x = x, y = y, fill = indenfor_trekant),
             shape = 21, size = 4, color = "black") +
  
  # Medspillere
  geom_point(data = medspillere_mænd, aes(x = x, y = y),
             color = "green", size = 4, shape = 21, fill = "green") +
  
  # Skytten
  geom_point(aes(x = skytte_pos_mænd[1], y = skytte_pos_mænd[2]),
             size = 5, shape = 4, color = "black") +
  
  labs(title = "Freeze Frame: Skud og placering af spillere (Mænd)",
       subtitle = "Blå trekant = skyttens skudvinkel | Lilla modstandere = i trekanten",
       x = "", y = "") +
  scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "red"), guide = "none") +
  coord_fixed(xlim = c(95, 125), ylim = c(20, 60)) +
  theme_void()

# --------------------------------------------------------------------------------------
# Visualisering i Shiny ---------------------------------------------------------

# shiny_app.R
ui <- fluidPage(
  titlePanel("Analyse af kvinde- og herrefodbold (VM)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Vælg plot type:",
                  choices = c("Skudstatistik", "Afleveringer", "Fejl & Frispark", "Tacklinger & Kort", "xA Analyse", "Freeze Frame")),
      radioButtons("køn", "Vælg køn:", choices = c("Kvinder", "Mænd"))
    ),
    
    mainPanel(
      plotOutput("outputPlot"),
      textOutput("beskrivelse")
    )
  )
)

server <- function(input, output) {
  
  output$outputPlot <- renderPlot({
    if (input$plotType == "Skudstatistik") {
      # Filtrér efter køn
      if (input$køn == "Kvinder") {
        ggplot(skud_data[skud_data$Køn == "Kvinder", ], aes(x = Køn, y = Antal, fill = Skudtype)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Skudstatistik - Kvinder")
      } else {
        ggplot(skud_data[skud_data$Køn == "Mænd", ], aes(x = Køn, y = Antal, fill = Skudtype)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Skudstatistik - Mænd")
      }
    }
    
    else if (input$plotType == "Afleveringer") {
      ggplot(afleveringer_data[afleveringer_data$Køn == input$køn, ],
             aes(x = Køn, y = Antal, fill = Afleveringstype)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Afleveringer -", input$køn))
    }
    
    else if (input$plotType == "Fejl & Frispark") {
      ggplot(fejl_frispark_data[fejl_frispark_data$Køn == input$køn, ],
             aes(x = Køn, y = Antal, fill = Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Fejl og Frispark -", input$køn))
    }
    
    else if (input$plotType == "Tacklinger & Kort") {
      ggplot(Tacklinger_og_kort[Tacklinger_og_kort$Køn == input$køn, ],
             aes(x = Køn, y = Antal, fill = Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Tacklinger og Kort -", input$køn))
    }
    
    else if (input$plotType == "xA Analyse") {
      ggplot(tælling_samlet_fix[tælling_samlet_fix$Køn == input$køn, ],
             aes(x = xA_kategori, y = Antal, fill = blev_mål)) +
        geom_col(position = "dodge") +
        geom_text(aes(label = paste0(Procent, "%")),
                  position = position_dodge(width = 0.9),
                  vjust = -0.3, size = 3.5) +
        labs(title = paste("xA Analyse –", input$køn),
             x = "xA-kategori", y = "Antal afleveringer")
    }
    
    else if (input$plotType == "Freeze Frame") {
      if (input$køn == "Kvinder") {
        # Brug det freeze frame plot du allerede har lavet
        # Eksempel (kan indsættes direkte): 
        ggplot() +
          tegn_bane() +
          geom_polygon(data = as.data.frame(trekant_skytten),
                       aes(x = V1, y = V2), fill = "deepskyblue", alpha = 0.3) +
          geom_point(data = modspillere, aes(x = x, y = y, fill = indenfor_trekant),
                     shape = 21, size = 4, color = "black") +
          geom_point(data = medspillere, aes(x = x, y = y), color = "green", size = 4, shape = 21, fill = "green") +
          geom_point(aes(x = skytte_pos[1], y = skytte_pos[2]), size = 5, shape = 4, color = "black") +
          scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "red"), guide = "none") +
          coord_fixed(xlim = c(95, 125), ylim = c(20, 60)) +
          theme_void() +
          labs(title = "Freeze Frame - Kvinder")
        
      } else {
        ggplot() +
          tegn_bane() +
          geom_polygon(data = as.data.frame(trekant_skytten_mænd),
                       aes(x = V1, y = V2), fill = "deepskyblue", alpha = 0.3) +
          geom_point(data = modspillere_mænd, aes(x = x, y = y, fill = indenfor_trekant),
                     shape = 21, size = 4, color = "black") +
          geom_point(data = medspillere_mænd, aes(x = x, y = y), color = "green", size = 4, shape = 21, fill = "green") +
          geom_point(aes(x = skytte_pos_mænd[1], y = skytte_pos_mænd[2]), size = 5, shape = 4, color = "black") +
          scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "red"), guide = "none") +
          coord_fixed(xlim = c(95, 125), ylim = c(20, 60)) +
          theme_void() +
          labs(title = "Freeze Frame - Mænd")
      }
    }
  })
  
  output$beskrivelse <- renderText({
    switch(input$plotType,
           "Skudstatistik" = "Her vises en sammenligning af skud på mål og missede skud.",
           "Afleveringer" = "Analyse af præcise og fejlafleveringer.",
           "Fejl & Frispark" = "Viser hvor mange fejl og frispark hvert køn begår.",
           "Tacklinger & Kort" = "Tacklinger, gule og røde kort sammenlignes.",
           "xA Analyse" = "xA (expected assist) sammenholdt med om det blev til mål.",
           "Freeze Frame" = "Visualisering af spillerpositioner omkring et skud.")
  })
}

shinyApp(ui = ui, server = server)

# --------------------------------------------------------------------------------------

