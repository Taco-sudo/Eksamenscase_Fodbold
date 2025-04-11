#### Opgave 6  - Eksamen ####
# Load nødvendige biblioteker
library(shiny) # Visualisering i Shiny-app
library(ggplot2) # Plot
library(dplyr) # Data manipulation
library(StatsBombR) # Indhent statsbomb data
library(ggsoccer) # Plot soccer event data
library(patchwork) # Placere plots ved siden af hinanden
# Opgave 6.1  --------------------------------------------------------------------------------------
# Videnskabsteori – tæt på virkeligheden  ---------------------------------------------------------
### Hermeneutik – mennesket bag facaden
# Filtre det danske kvindehold, og lav en dataframe kun for dem
top_scorer_kvinder <- vm_events_kvinder %>%
  filter(type.name == "Shot",
         shot.outcome.name == "Goal") %>%
  select(player.name, team.name, match_id, minute, shot.statsbomb_xg)

top_10_maal_bedst_xg <- top_scorer_kvinder %>%
  arrange(desc(shot.statsbomb_xg)) %>%
  slice(1:10)

# Resten af opgave er besvaret i rapporten
# --------------------------------------------------------------------------------------

