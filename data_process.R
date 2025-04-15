# ================================================
# ENS 2SEP
# Traitement et analyse des données mémoire 1A SHS 2024-2025
# MARTINIE Romain & ROUANET Léonine
# ================================================

# -------------------------------------------------------------------
# 1. CHARGEMENT DES BIBLIOTHÈQUES
# -------------------------------------------------------------------
library(FactoMineR)  # Pour l'Analyse des Correspondances Multiples (ACM)
library(factoextra)  # Pour la visualisation des résultats de l'ACM
library(dplyr)       # Pour la manipulation des données
library(ggplot2)     # Pour les visualisations graphiques
library(rstatix)     # Pour les tests statistiques
library(openxlsx)    # Pour exporter les données quali

# -------------------------------------------------------------------
# 2. IMPORTATION DES DONNÉES
# -------------------------------------------------------------------
# Importation du fichier de données CSV
data <- read.csv("user/path/raw_data.csv", sep=',')
head(data)  # Aperçu des premières lignes du jeu de données

# -------------------------------------------------------------------
# 3. DÉFINITION DES GROUPES DE VARIABLES
# -------------------------------------------------------------------
# Définition des groupes de colonnes à analyser ensemble
groupes <- list(
  c("Course.CPlan.", "Course.CFam.", "Course.CJust."),  # Variables liées aux courses
  c("Marche.MPlan.", "Marche.MFam.", "Marche.MJust.")   # Variables liées à la marche
)

# Noms simplifiés pour les régimes d'engagement
noms_simples <- c("Plan", "Familiarités", "Justification")

# -------------------------------------------------------------------
# 4. NETTOYAGE ET PRÉPARATION DES DONNÉES
# -------------------------------------------------------------------
# 4.1 Supprimer les lignes où le maximum est partagé par plusieurs colonnes dans un même groupe
for (groupe in groupes) {
  # Identifier les lignes où le maximum est partagé par au moins deux colonnes
  a_supprimer <- apply(data[, groupe], 1, function(ligne) {
    val_max <- max(ligne, na.rm = TRUE)
    sum(ligne == val_max, na.rm = TRUE) > 1
  })
  
  # Supprimer les lignes correspondantes
  data <- data[!a_supprimer, ]
}

# 4.2 Création des variables de régime pour chaque groupe
for (i in seq_along(groupes)) {
  cols <- groupes[[i]]  # Colonnes associées à ce groupe
  nom_regime <- paste0("regime_", i)  # Nom de la colonne régime pour ce groupe
  
  # Calculer le régime pour chaque ligne dans ce groupe
  data[[nom_regime]] <- apply(data[, cols], 1, function(x) {
    noms_simples[which.max(x)]
  })
}

# 4.3 Fusion des régimes en une seule variable
data$regime <- apply(data[, c("regime_1", "regime_2")], 1, function(ligne) {
  # Retirer les valeurs vides ou 'character(0)'
  valeurs_valides <- ligne[ligne != "character(0)" & ligne != ""]
  # Fusionner les valeurs restantes (si elles existent)
  if (length(valeurs_valides) > 0) {
    paste(valeurs_valides, collapse = " ")
  } else {
    NA  # Si aucune valeur n'est valide
  }
})

# 4.4 Identification et suppression des outliers
outliers <- data %>% identify_outliers(Engagement)
data <- data %>%
  filter(!Engagement %in% outliers$Engagement[outliers$is.extreme])

#tester si age, genre, CSP, diplome, CapEco, EnvCS, EnvPSubj,EnvPObj, Engagament
#différents selon random = 1 ou 0
#sachant que age, envCS, envPSubj et Engagement variables quanti
#sinon variables quali
# a décommenter si necessaire
#source("user/path/diff_CM.R") #a decommenter pour faire l'analyse entre Marce et course

data$groupe <- data$random
# 4.5 Sélection des colonnes pertinentes (à partir de la colonne 14)
data <- data[, 14:ncol(data)]

# 4.6 Comptage des réponses "autre"
n_autre <- nrow(data[data$autre != "", ])
data_qual <- data[data$autre != "", c("regime", "autre")]
#write.xlsx(data_qual, file = "data_qual.xlsx")


# 4.7 Suppression des colonnes non nécessaires pour l'analyse
colonnes_a_exclure <- c("CSP.other.", "Diplome.other.", "regime_1", "regime_2", 'autre')
data <- data[, setdiff(names(data), colonnes_a_exclure)]

# 4.8 Suppression des lignes avec valeurs manquantes
data <- na.omit(data)
print(paste('course n =', nrow(data[data$groupe == 0,])))
print(paste('marche n =', nrow(data[data$groupe == 1,])))


#DIFFENCE ACM MARCE/COURSE
data_course <- data[data$groupe == 0,]
data_marche <- data[data$groupe == 1,]


contingence <- data %>%
  group_by(groupe, regime) %>%
  summarise(Frequence = n(), .groups = "drop") %>%
  group_by(groupe) %>%
  mutate(Pourcentage = round(100 * Frequence / sum(Frequence), 1)) %>%
  arrange(groupe, regime)
contingence

colonnes_a_exclure <- c('groupe')
data <- data[, setdiff(names(data), colonnes_a_exclure)]
data_course <- data_course[, setdiff(names(data), colonnes_a_exclure)]
data_marche <- data_marche[, setdiff(names(data), colonnes_a_exclure)]
# -------------------------------------------------------------------
# 5. ANALYSE DE L'ENGAGEMENT PAR RÉGIME
# -------------------------------------------------------------------
# 5.1 Préparation d'un sous-ensemble pour l'analyse
table_engagement <- data[, c("regime", "Engagement")] #all
table_engagement <- data_course[, c("regime", "Engagement")] #course
table_engagement <- data_marche[, c("regime", "Engagement")] #marche


# 5.2 Visualisation de la distribution de l'engagement
# Histogramme de l'engagement
hist(table_engagement$Engagement, breaks=10, 
     main="Distribution de l'engagement", 
     xlab="Niveau d'engagement", 
     ylab="Fréquence")

# 5.3 Densité de l'engagement par régime
ggplot(data, aes(x = Engagement, fill = regime)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Engagement", y = "Densité", fill = "Régime") +
  scale_fill_brewer(palette = "Set2")

# 5.4 Densité de l'engagement par régime (un graphique par régime)
ggplot(data, aes(x = Engagement, fill = regime)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Engagement", y = "Densité", fill = "Régime") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~regime, ncol = 1)

# 5.5 Discrétisation de l'engagement pour l'analyse chi-carré
table_engagement$Engagement <- cut(
  table_engagement$Engagement,
  breaks = c(-Inf, 2, 4, 6, Inf),
  labels = c("2-", "3-4", "5-6", "7+"),
  right = TRUE
)

# 5.6 Test du chi-carré pour l'association régime/engagement
# Création du tableau de contingence
table_contingence <- table(table_engagement$regime, table_engagement$Engagement)
# Test du chi-carré pour course, fisher pour marche
fishe <- fisher_test(table_contingence)
test_chi2 <- chisq.test(table_contingence)
v_cramer <- cramer_v(table_contingence)  # Force de l'association

print(fishe)
print(test_chi2)
print(paste("V de Cramer:", v_cramer))

# -------------------------------------------------------------------
# 6. PRÉPARATION DES DONNÉES POUR L'ACM
# -------------------------------------------------------------------
# 6.1 Exclure la variable d'engagement pour l'ACM
#décommenter le bon
data_acm <- data_marche[, setdiff(names(data), c('Engagement'))] #marche
#data_acm <- data_course[, setdiff(names(data), c('Engagement'))] #course
#data_acm <- data[, setdiff(names(data), c('Engagement'))] #all

# 6.2 Recodage de l'âge en tranches
data_acm$age <- cut(
  data_acm$age,
  breaks = c(18, 25, 45, 65, Inf),
  labels = c("18-25", "26-45", "46-65", "65+"),
  right = FALSE
)

# 6.3 Recodage de l'environnement culturel et social
data_acm$EnvCS.SQ001. <- cut(
  data_acm$EnvCS.SQ001.,
  breaks = c(-Inf, 25, 50, 75, Inf),
  labels = c("25-%", "26-50%", "51-75%", "76+%"),
  right = TRUE
)

# 6.4 Recodage de l'environnement perçu subjectif
data_acm$EnvPSubj.SQ001. <- cut(
  data_acm$EnvPSubj.SQ001., 
  breaks = c(-Inf, 25, 50, 75, Inf),
  labels = c("25-%", "26-50%", "51-75%", "76+%"),
  right = TRUE
)

# 6.5 Recodage de l'environnement physique objectif
data_acm$EnvPObj <- case_when(
  data_acm$EnvPObj %in% c("Village : moins de 2 000 habitants", 
                          "Bourg : de 2 000 à 5 000 habitants") ~ "Rural",
  data_acm$EnvPObj %in% "Petite ville : de 5 000 à 20 000 habitants" ~ "Petite ville",
  data_acm$EnvPObj %in% c("Ville moyenne : de 20 000 à 50 000 habitants",
                          "Grande ville : de 50 000 à 200 000 habitants") ~ "Ville moyenne",
  data_acm$EnvPObj %in% "Métropole : plus de 200 000 habitants" ~ "Métropole"
)

# 6.6 Recodage du niveau de diplôme
data_acm$Diplome <- case_when(
  data_acm$Diplome %in% c("Bac (Baccalauréat, DAEU, Capacité en droit, DSP)", 
                          "CAP, BEP") ~ "Niveau 3_4",
  data_acm$Diplome %in% c("Bac + 4 (Maîtrise)", "Bac + 5 (Master, DEA, DESS, diplôme d'ingénieur)",
                          "Bac + 8 (Doctorat, habilitation à diriger des recherches)") ~ "Niveau 6_7_8",
  data_acm$Diplome %in% c("Bac + 2 (DEUG, BTS, DUT, DEUST)",
                          "Bac + 3 (Licence, Licence LMD, licence professionnelle, BUT)") ~ "Niveau 5_6",
  data_acm$Diplome %in% c('Sans diplôme', 'Autre') ~ 'Sans Diplôme'
)

# 6.7 Conversion de l'environnement physique objectif en facteur
data_acm$EnvPObj <- factor(data_acm$EnvPObj)

# 6.8 Renommage des variables pour plus de clarté
names(data_acm)[names(data_acm) == 'EnvCS.SQ001.'] <- 'EnvCulturelSocial'
names(data_acm)[names(data_acm) == 'EnvPSubj.SQ001.'] <- 'EnvReelSubj'
names(data_acm)[names(data_acm) == 'EnvPObj'] <- 'EnvReelObj'


data_acm <- data_acm %>%
  mutate(CSP = case_when(
    CSP == "Etudiant(e)s" ~ "Étudiants",
    CSP == "Cadres et professions intellectuelles supérieures" ~ "Cadres",
    CSP == "Ouvriers / Ouvrières" ~ "Ouvriers",
    CSP == "Artisans / Artisanes, commerçants / commerçantes et chefs / cheffes d’entreprise" ~ "Indépendants",
    CSP == "Retraité(e)s" ~ "Retraités",
    CSP == "Employé(e)s" ~ "Employés",
    CSP == "Professions intermédiaires" ~ "Intermédiaires",
    TRUE ~ "Autre"
  ))

# 6.9 Conversion de toutes les variables en facteurs pour l'ACM
data_acm[] <- lapply(data_acm, as.factor)

# -------------------------------------------------------------------
# 7. ANALYSE DES CORRESPONDANCES MULTIPLES (ACM)
# -------------------------------------------------------------------
# 7.1 Réalisation de l'ACM avec le régime comme variable qualitative supplémentaire
resultat_acm <- MCA(data_acm, quali.sup = 'regime')
summary(resultat_acm)

# 7.2 Visualisation des contributions des variables à la dimension 1
fviz_contrib(
  resultat_acm,
  choice = "var",
  axes = 1, 
  title = "Contribution des variables - Dimension 1"
)

# 7.3 Visualisation des contributions des variables à la dimension 2
fviz_contrib(
  resultat_acm,
  choice = "var",
  axes = 2,
  title = "Contribution des variables - Dimension 2"
)

# 7.4 Biplot (graphique combinant variables et individus)
# Décommenter si nécessaire
# fviz_mca_biplot(
#   resultat_acm,
#   repel = TRUE, 
#   title = "ACM - Représentation des variables et des individus"
# )


