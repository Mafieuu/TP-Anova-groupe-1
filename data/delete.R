library(dplyr)
data <- read.csv("final_matches.csv")

# Filtrer les lignes
filtered_data <- data %>% filter(Division %in% c("SC0", "E2", "E0", "D1", "SP1", "D2"))




write.csv(filtered_data, "fichier_filtre.csv", row.names = FALSE)


