# Installer (si nécessaire) et charger les bibliothèques
library(readxl)

Data <- read_excel("C:/Users/HP/Documents/Markov_group/weather_data.xlsx")
Data <- data.frame(Data)
Data
# Exemple de fonction pour discrétiser les données en états avec ajustements
discretize_weather_v3 <- function(temp, precipitation, wind_speed, humidity) {
  
  # Ensoleillé : Température élevée et faible vitesse du vent
  if (temp > 30 & wind_speed < 5) {
    return("Ensoleillé")
  } 
  
  # Nuageux : Température modérée, humidité modérée et vent léger
  else if (temp >= 20 & temp <= 25 & humidity >= 50 & humidity <= 70 & wind_speed >= 3 & wind_speed <= 7) {
    return("Nuageux")
  }
  
  # Pluvieux : Précipitations présentes ou température basse avec humidité élevée
  else if (precipitation > 0) {
    return("Pluvieux")
  } else if (temp < 20 & humidity > 70) {
    return("Pluvieux")
  }
  
  # Autre : Cas divers ne rentrant dans aucune autre catégorie
  else {
    return("Autre")
  }
}

# Appliquer la fonction discrétisation sur chaque ligne du DataFrame
Data$weather_state <- apply(Data, 1, function(row) discretize_weather_v3(row["Temperature_C"], 
                                                                         row["Precipitation_mm"], 
                                                                         row["Wind_Speed_kmh"], 
                                                                         row["Humidity_pct"]))

# Vérifier les résultats
head(Data$weather_state)

# Étape 3 : Observer les transitions
# Créer un tableau de fréquences pour observer les transitions entre états
transition_data <- data.frame(from = head(Data$weather_state, -1), to = tail(Data$weather_state, -1))

# Étape 4 : Calculer les probabilités de transition
transition_matrix <- table(transition_data$from, transition_data$to) # Matrice de transition brute
transition_matrix_prob <- prop.table(transition_matrix, 1) # Matrice de transition en probabilités

# Afficher la matrice de transition
print("Matrice de transition des états (probabilités) :")
print(transition_matrix_prob)

# Étape 5 : Tester les hypothèses markoviennes
# Hypothèse : les transitions dépendent uniquement de l'état précédent, on peut effectuer un test du chi carré.
chi_test <- chisq.test(transition_matrix)
print("Test du chi carré pour tester les hypothèses markoviennes :")
#
print(chi_test)

library(expm) # Pour résoudre les équations de la matrice
pi <- solve(t(transition_matrix_prob) - diag(1, nrow(transition_matrix_prob)), rep(1, nrow(transition_matrix_prob)))
pi <- pi / sum(pi) # Normalisation pour que la somme des probabilités soit 1
print("Distribution stationnaire :")
print(pi)


# Étape 8 : Faire des simulations
# Simulation de la chaîne de Markov pour voir son évolution au fil du temps
set.seed(123)


# Fonction de simulation
simulate_markov_chain <- function(transition_matrix_prob, n_steps, initial_state) {
  states <- colnames(transition_matrix_prob)
  current_state <- initial_state
  state_sequence <- current_state
  
  for (i in 1:n_steps) {
    next_state <- sample(states, 1, prob = transition_matrix_prob[current_state,])
    state_sequence <- c(state_sequence, next_state)
    current_state <- next_state
  }
  
  return(state_sequence)
}
# Simuler la chaîne de Markov pendant 50 étapes
simulated_states <- simulate_markov_chain(transition_matrix_prob, 50, "Ensoleillé")

library(ggplot2)
# Visualiser les résultats
ggplot(data.frame(steps = 1:51, state = simulated_states), aes(x = steps, fill = state)) +
  geom_bar(stat = "count", position = "stack") +
  labs(title = "Simulation de la chaîne de Markov", x = "Étapes", y = "Nombre d'occurrences") +
  theme_minimal()
# Visualiser les résultats
ggplot(data.frame(steps = 1:50, state = simulated_states), aes(x = steps, fill = state)) +
  geom_bar(stat = "count", position = "stack") +
  labs(title = "Simulation de la chaîne de Markov", x = "Étapes", y = "Nombre d'occurrences") +
  theme_minimal()






