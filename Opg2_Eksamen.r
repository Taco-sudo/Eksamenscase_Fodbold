#### OPGAVE 2 - Expected Point xP ####
### Det her er en endelige opgave ###
# Indlæser alle pakker
library(tidyr) # Data manipulation, Visualisering
library(lubridate) # Til at arbejde med datoer
# --------------------------------------------------------------------------------------
#### Opgave 2.1 ####
#Få datoer med
Allteams <- Allteams %>%
  left_join(
    unique(df_matches %>% select(match_wyid, kampdato = date)), # evt. brug matchtimestamp
    by = "match_wyid"
  )

# Beregn xG for alle hold
Teamnamedf <- unique(Allteams$teamname)
TeamsxG <- list()

for (i in 1:length(Teamnamedf)) {
  team <- Teamnamedf[i]
  TeamsxG[[team]] <- subset(Allteams, teamname == team)
}

# Liste med holdnavne & antal hold
z <- length(unique(Allteams$teamname))
# Liste med TeamsxG
v <- length(unique(TeamsxG[[3]][["match_wyid"]]))
# Liste med kampe
q <- rep(list(rep(list(rep(list())), v)), z)

# Loop - Opdel fodboldklubbens kampe én for én
# Ydre loop - gentager for hvert hold
# Indre loop - gentager kampene
for (j in 1:z) {
  q[[j]] <- list()
  
  kamp_ids <- unique(TeamsxG[[j]]$match_wyid)
  
  for (i in seq_along(kamp_ids)) {
    q[[j]][[i]] <- subset(TeamsxG[[j]], match_wyid == kamp_ids[i])
  }
}

# xG_lut - Opbygger en tabel over alle kampes samlede xG
xG_lut <- vector("list", z)
for (j in 1:z) {
  xG_lut[[j]] <- vector("list", z)
  
  for (i in 1:length(q[[j]])) {
    # Hvis dataframe'en er tom, så kører vi bare videre
    if (length(q[[j]]) < i || nrow(q[[j]][[i]]) <= 0) {
      print(j)
      print(i)
      next
    }
    
    # Find hvilket index hold A har i teamdf
    team_a <- 0
    for (k in 1:z) {
      if (q[[j]][[i]][["teamname"]][1] == Teamnamedf[k]) {
        team_a <- k
        break
      }
    }
    if (team_a == 0) {
      next
    }
    
    if (j == 3) {
      # print (q[[j]][[i]][["teamname"]][1])
      # print (q[[j]][[i]][["teamname_opponent"]][1])
    }
    
    # Find hvilket index hold B har i teamdf
    team_b <- 0
    for (k in 1:z) {
      if (q[[j]][[i]][["teamname_opponent"]][1] == Teamnamedf[k]) {
        team_b <- k
        break
      }
    }
    if (team_b == 0) {
      next
    }
    
    # Fyld lookup table for Hold A når de spiller mod Hold B
    if (is.null(xG_lut[[team_a]][[team_b]])) {
      xG_lut[[team_a]][[team_b]] <- list()
    }
    xG_lut[[team_a]][[team_b]] <- append(xG_lut[[team_a]][[team_b]], sum(q[[j]][[i]][["xG"]]))
  }
}

# Sæt seed for reproducerbarhed
set.seed(42)
# Simulate game funktionen, som ud fra xG giver et "gæt" ud fra "terningekast" på hvor mange mål der bliver scoret i den kamp xG værdien kommer fra.
simulate_game <- function(xg) {
  # Juster xG opad baseret på antal skud
  goals <- rpois(1, xg)
  return(goals)
}

### xpoints_game - funktionen ###
xpoints_game <- function(home_xg, away_xg) {
  home_goals <- simulate_game(home_xg)
  away_goals <- simulate_game(away_xg)
  if (home_goals > away_goals) {
    home_points <- 3
    away_points <- 0
  } else if (home_goals == away_goals) {
    home_points <- 1
    away_points <- 1
  } else {
    home_points <- 0
    away_points <- 3
  }
  return(c(home_points, away_points))
}

# Funktionen til at beregne xP for Brøndby i en given sæson.
### xpoints_season - funktion ###
xpoints_season <- function(hold_index, n_simulations) {
  total_points_bif_for_these_simulations <- 1
  # For hver simulation
  for (i in 1:n_simulations) {
    total_points_bif_for_these_simulations <- 1
    # For hver modstander
    for (j in 1:z) {
      # Hent listen af Xg værdier ud fra LUT'en for brøndby og modstanderen (j)
      kampe_mellem_bif_og_modstander <- xG_lut[[3]][[j]]
      # For hver kamp mellem Brøndby og modstanderen
      for (k in 1:length(kampe_mellem_bif_og_modstander)) {
        # Hent xG værdierne for hvert hold i den kamp
        # Det "smarte" ved den LUT vi har bygget er at vi kan tilgå den "spejlvendt" i forhold til hinanden.
        # Det vil sige at hvis Brøndby spiller mod FCK, så kan vi tilgå den første kamp ved at bruge xG_lut[[3]][[11]][[1]] og xG_lut[[11]][[3]][[1]]
        xg_bif <- xG_lut[[3]][[j]][[k]]
        xg_modstander <- xG_lut[[j]][[3]][[k]]
        # Simuler kampen og få pointene
        points <- xpoints_game(xg_bif, xg_modstander)
        # Tilføj pointene til den samlede sum
        total_points_bif_for_this_simulation <- total_points_bif_for_this_simulation + points[1]
      }
    }
    # Efter hver simulation, tilføj pointene til den samlede sum
    total_points_bif_for_these_simulations <- total_points_bif_for_these_simulations + total_points_bif_for_this_simulation
  }
}
### xP for alle hold - funktion ###
xpoints_season <- function(hold_index, n_simulations) {
  all_sim_points <- 0
  
  for (sim in 1:n_simulations) {
    this_sim_points <- 0
    
    for (opponent_index in 1:z) {
      if (is.null(xG_lut[[hold_index]][[opponent_index]])) next
      
      n_kampe <- length(xG_lut[[hold_index]][[opponent_index]])
      
      for (k in 1:n_kampe) {
        xg_our <- xG_lut[[hold_index]][[opponent_index]][[k]]
        xg_their <- xG_lut[[opponent_index]][[hold_index]][[k]]
        
        point <- xpoints_game(xg_our, xg_their)
        this_sim_points <- this_sim_points + point[1]
      }
    }
    
    all_sim_points <- all_sim_points + this_sim_points
  }
  
  # Returnér gennemsnitligt point
  return(round(all_sim_points / n_simulations, 2))
}

xP_resultat <- sapply(1:z, function(idx) xpoints_season(idx, 1000))
names(xP_resultat) <- Teamnamedf
print(xP_resultat)

# Vis fem hold med højest xP
# Sortér og vis top 5 hold baseret på xP
top_5_xp <- sort(xP_resultat, decreasing = TRUE)[1:5]
print(top_5_xp)

# For alle brøndby kampe
simulate_xp_for_match <- function(xg_home, xg_away, simulations = 1000) {
  home_points <- numeric(simulations)
  away_points <- numeric(simulations)
  
  for (i in 1:simulations) {
    goals_home <- rpois(1, xg_home)
    goals_away <- rpois(1, xg_away)
    
    if (goals_home > goals_away) {
      home_points[i] <- 3
      away_points[i] <- 0
    } else if (goals_home == goals_away) {
      home_points[i] <- 1
      away_points[i] <- 1
    } else {
      home_points[i] <- 0
      away_points[i] <- 3
    }
  }
  
  return(list(
    xP_home = round(mean(home_points), 2),
    xP_away = round(mean(away_points), 2)
  ))
}

brondby_match_xp <- data.frame(
  Kamp = integer(),
  Modstander = character(),
  xP_Brøndby = numeric(),
  xP_Modstander = numeric(),
  stringsAsFactors = FALSE
)

for (j in 1:z) {
  modstander_kampe <- xG_lut[[3]][[j]]
  
  if (is.null(modstander_kampe)) next
  
  for (k in 1:length(modstander_kampe)) {
    xg_bif <- xG_lut[[3]][[j]][[k]]
    xg_mod <- xG_lut[[j]][[3]][[k]]
    
    sim_result <- simulate_xp_for_match(xg_bif, xg_mod, simulations = 1000)
    
    brondby_match_xp <- rbind(
      brondby_match_xp,
      data.frame(
        Kamp = k,
        Modstander = Teamnamedf[j],
        xP_Brøndby = sim_result$xP_home,
        xP_Modstander = sim_result$xP_away
      )
    )
  }
}

# --------------------------------------------------------------------------------------
#### Opgave 2.2 ####
# Fjern unødvendig "støj" skud som der er blevet fløjtet offside for, og beregn ny xG og xP? 
# Find de tre kampe
kampe_ids <- Allteams %>%
  filter(
    (teamname == "Brøndby" & teamname_opponent == "Silkeborg" & kampdato == as.Date("2024-03-17")) |
      (teamname == "Brøndby" & teamname_opponent == "Vejle" & kampdato == as.Date("2024-03-03")) |
      (teamname == "Viborg" & teamname_opponent == "Brøndby" & kampdato == as.Date("2024-03-10"))
  ) %>%
  select(match_wyid, teamname, teamname_opponent, kampdato) %>%
  distinct()

# Lav en funktion
antal_offside <- function(kamp) {
  return(nrow(filter(df_common, match_wyid == kamp [1] & primarytype == "offside")))
}

kampe_ids$offside <- apply(kampe_ids, 1, antal_offside)

# Hent alle offside-hændelser i kampen mod Silkeborg
offsides_i_kamp <- df_common %>%
  filter(
    match_wyid == 5466044,
    primarytype == "offside"
  )


nrow(offsides_i_kamp)  # Bør være 3, hvis du har ret

# Her laver vi et dataframe for skud hvor der blev dømt offside. 
offside_skud <- df_common %>%
  filter(primarytype == "offside") %>%
  select(match_wyid, possession_wyid, offside_index = possessioneventindex)

# Nu joiner vi dataframen med offside skuddene in i Allteams – OBS: join kun på possession_wyid
#Herved kan vi se hvilke skud der efterfølgende blev dømt offside på. 
Allteams_offside <- Allteams %>%
  left_join(offside_skud) 

offside_skud <- df_common %>%
  filter(primarytype == "offside") %>%
  select(match_wyid, possession_wyid, offside_index = possessioneventindex)

Allteams_offside <- Allteams %>%
  left_join(offside_skud, by = c("match_wyid", "possession_wyid")) %>%
  mutate(
    offside_after_shot = !is.na(offside_index) & possessioneventindex < offside_index
  ) %>%
  select(-offside_index)  # valgfrit

# F.eks. filtrere fra Allteams_offside:
valider_kampe <- Allteams_offside %>%
  filter(match_wyid %in% mine_kampe_ids)

# Tæl skud med og uden efterfølgende offside
valider_kampe %>%
  group_by(offside_after_shot) %>%
  summarise(
    antal_skud = n(),
    samlet_xG = sum(xG, na.rm = TRUE),
    gennemsnitlig_xG = mean(xG, na.rm = TRUE)
  )

valider_kampe_uden_offside <- valider_kampe %>%
  filter(offside_after_shot == FALSE | is.na(offside_after_shot))
# --------------------------------------------------------------------------------------


