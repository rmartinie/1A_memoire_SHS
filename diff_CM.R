# ================================================
# ENS 2SEP
# Traitement et analyse des données mémoire 1A SVS 2024-2025
# MARTINIE Romain & ROUANET Léonine
# ================================================

# Analyse des différences entre groupes (random = 1 ou 0)
# Script qui teste automatiquement la normalité pour les variables quantitatives
# et utilise les tests appropriés pour comparer les groupes
# + tableaux descriptifs

# -------------------------------------------------------------------
# 1. CHARGEMENT DES BIBLIOTHÈQUES
# -------------------------------------------------------------------
library(tidyverse) #Collection de packages R pour la manipulation et visualisation des données
#library(car)       #Package pour les analyses de régression et diagnostics
library(ggplot2)   #Package pour la création de graphiques élégants
#library(nortest)   #Package pour les tests de normalité
#library(gmodels)   #Package pour les modèles et tableaux croisés

# Fonction pour analyser une variable quantitative
analyze_quant_var <- function(data, var_name, group_var = "random") {
  cat("\n\n======================================================\n")
  cat(paste0("ANALYSE DE LA VARIABLE QUANTITATIVE: ", var_name, "\n"))
  cat("======================================================\n\n")
  
  # Extraire la variable
  var_data <- data[[var_name]]
  groups <- data[[group_var]]
  
  # Statistiques descriptives par groupe
  cat("Statistiques descriptives par groupe:\n")
  stats <- aggregate(var_data, by = list(groups), 
                     FUN = function(x) c(n = length(x), 
                                         moyenne = mean(x, na.rm = TRUE), 
                                         écart_type = sd(x, na.rm = TRUE),
                                         médiane = median(x, na.rm = TRUE),
                                         min = min(x, na.rm = TRUE),
                                         max = max(x, na.rm = TRUE)))
  print(stats)
  cat("\n")
  
  # Test de normalité (Shapiro-Wilk) pour chaque groupe
  cat("Tests de normalité (Shapiro-Wilk):\n")
  # Groupe 1
  group1 <- var_data[groups == 1]
  sw_test1 <- tryCatch({
    shapiro.test(group1)
  }, error = function(e) {
    return(list(p.value = NA, error = e$message))
  })
  
  cat("Groupe 1: ")
  if (!is.na(sw_test1$p.value)) {
    cat(paste0("W = ", round(sw_test1$statistic, 4), 
               ", p-value = ", round(sw_test1$p.value, 4)))
    normality1 <- sw_test1$p.value > 0.05
    cat(paste0(" - Distribution ", ifelse(normality1, "normale", "non normale"), "\n"))
  } else {
    cat(paste0("Erreur lors du test: ", sw_test1$error, "\n"))
    normality1 <- FALSE
  }
  
  # Groupe 2
  group2 <- var_data[groups == 0]
  sw_test2 <- tryCatch({
    shapiro.test(group2)
  }, error = function(e) {
    return(list(p.value = NA, error = e$message))
  })
  
  cat("Groupe 2: ")
  if (!is.na(sw_test2$p.value)) {
    cat(paste0("W = ", round(sw_test2$statistic, 4), 
               ", p-value = ", round(sw_test2$p.value, 4)))
    normality2 <- sw_test2$p.value > 0.05
    cat(paste0(" - Distribution ", ifelse(normality2, "normale", "non normale"), "\n"))
  } else {
    cat(paste0("Erreur lors du test: ", sw_test2$error, "\n"))
    normality2 <- FALSE
  }
  
  # Test d'homogénéité des variances (Levene)
  levene_data <- data.frame(value = var_data, group = as.factor(groups))
  levene_test <- car::leveneTest(value ~ group, data = levene_data)
  cat("\nTest d'homogénéité des variances (Levene):\n")
  print(levene_test)
  homogeneity <- levene_test[1, "Pr(>F)"] > 0.05
  cat(paste0("Homogénéité des variances: ", ifelse(homogeneity, "Oui", "Non"), "\n\n"))
  
  # Décision sur le test à utiliser
  use_parametric <- normality1 && normality2 && homogeneity
  
  # Effectuer le test approprié
  if (use_parametric) {
    # Test t pour échantillons indépendants
    t_test <- t.test(var_data ~ groups, var.equal = TRUE)
    cat("Test t pour échantillons indépendants (variances égales):\n")
    print(t_test)
  } else {
    # Test de Mann-Whitney (non paramétrique)
    mw_test <- wilcox.test(var_data ~ groups)
    cat("Test de Mann-Whitney (non paramétrique):\n")
    print(mw_test)
  }
  
  # Représentation graphique
  cat("\nCréation d'un graphique pour visualiser les différences...\n")
  p <- ggplot(levene_data, aes(x = as.factor(group), y = value, fill = as.factor(group))) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    theme_minimal() +
    labs(title = paste0("Comparaison de ", var_name, " entre les groupes"),
         x = "Groupe",
         y = var_name,
         fill = "Groupe") +
    scale_fill_brewer(palette = "Set1")
  
  print(p)
  
  cat("\nAnalyse terminée pour", var_name, "\n")
  
  # Retourner un résumé des résultats
  return(list(
    variable = var_name,
    normalite_g1 = normality1,
    normalite_g2 = normality2,
    homogeneite = homogeneity,
    test_utilise = ifelse(use_parametric, "t-test", "Mann-Whitney"),
    p_value = ifelse(use_parametric, t_test$p.value, mw_test$p.value)
  ))
}

# Fonction pour analyser une variable qualitative
analyze_qual_var <- function(data, var_name, group_var = "random") {
  cat("\n\n======================================================\n")
  cat(paste0("ANALYSE DE LA VARIABLE QUALITATIVE: ", var_name, "\n"))
  cat("======================================================\n\n")
  
  # Extraire les variables
  var_data <- as.factor(data[[var_name]])
  groups <- as.factor(data[[group_var]])
  
  # Tableau de contingence
  cont_table <- table(groups, var_data)
  cat("Tableau de contingence:\n")
  print(cont_table)
  
  # Pourcentages par groupe
  prop_table <- prop.table(cont_table, margin = 1) * 100
  cat("\nPourcentages par groupe (%):\n")
  print(round(prop_table, 2))
  
  # Test du Chi-2 ou test exact de Fisher
  expected <- chisq.test(cont_table)$expected
  use_fisher <- any(expected < 5)
  
  if (use_fisher) {
    cat("\nUtilisation du test exact de Fisher (certaines fréquences attendues < 5):\n")
    test_result <- fisher.test(cont_table, simulate.p.value = TRUE, B = 10000)
  } else {
    cat("\nTest du Chi-2:\n")
    test_result <- chisq.test(cont_table)
  }
  
  print(test_result)
  
  # Représentation graphique avec intervalles de confiance
  cat("\nCréation d'un graphique pour visualiser les différences...\n")
  
  # Préparer les données pour le graphique
  plot_data <- as.data.frame(cont_table)
  colnames(plot_data) <- c("Groupe", "Catégorie", "Fréquence")
  
  # Calculer les proportions et leurs intervalles de confiance
  prop_data <- data.frame()
  for (g in unique(plot_data$Groupe)) {
    for (cat in unique(plot_data$Catégorie)) {
      # Extraire les données pour ce groupe et cette catégorie
      freq <- plot_data[plot_data$Groupe == g & plot_data$Catégorie == cat, "Fréquence"]
      if (length(freq) == 0) freq <- 0
      
      # Calculer le total pour ce groupe
      total <- sum(plot_data[plot_data$Groupe == g, "Fréquence"])
      
      # Calculer la proportion
      prop <- freq / total
      
      # Calculer l'intervalle de confiance (approximation normale)
      se <- sqrt(prop * (1 - prop) / total)
      ci_lower <- max(0, prop - 1.96 * se)
      ci_upper <- min(1, prop + 1.96 * se)
      
      # Ajouter à notre dataframe
      prop_data <- rbind(prop_data, data.frame(
        Groupe = g,
        Catégorie = cat,
        Proportion = prop,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper,
        Total = total
      ))
    }
  }
  
  # Créer le graphique
  p <- ggplot(prop_data, aes(x = Catégorie, y = Proportion, fill = Groupe)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                  position = position_dodge(width = 0.9), 
                  width = 0.25) +
    theme_minimal() +
    labs(title = paste0("Comparaison de ", var_name, " entre les groupes"),
         x = var_name,
         y = "Proportion",
         fill = "Groupe",
         caption = "Les barres d'erreur représentent l'intervalle de confiance à 95%") +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = scales::percent_format())
  
  print(p)
  
  cat("\nAnalyse terminée pour", var_name, "\n")
  
  # Retourner un résumé des résultats
  return(list(
    variable = var_name,
    test_utilise = ifelse(use_fisher, "Test exact de Fisher", "Test du Chi-2"),
    p_value = test_result$p.value
  ))
}

# Fonction principale pour analyser toutes les variables
analyze_all_variables <- function(data) {
  # Vérifier que la variable random existe
  if (!"random" %in% names(data)) {
    stop("La variable 'random' n'est pas présente dans les données!")
  }
  
  # S'assurer que random ne contient que 0 et 1
  if (!all(unique(na.omit(data$random)) %in% c(0, 1))) {
    warning("La variable 'random' devrait contenir uniquement les valeurs 0 et 1!")
  }
  
  # Variables quantitatives à analyser
  quant_vars <- c("age", "EnvCS.SQ001.", "EnvPSubj.SQ001.", "Engagement")
  
  # Variables qualitatives à analyser
  qual_vars <- c("Genre", "CSP", "Diplome", "CapEco", "EnvPObj")
  
  # Résultats
  results_quant <- list()
  results_qual <- list()
  
  # Analyser les variables quantitatives
  cat("ANALYSE DES VARIABLES QUANTITATIVES\n\n")
  for (var in quant_vars) {
    if (var %in% names(data)) {
      tryCatch({
        results_quant[[var]] <- analyze_quant_var(data, var)
      }, error = function(e) {
        cat(paste0("Erreur lors de l'analyse de ", var, ": ", e$message, "\n"))
      })
    } else {
      cat(paste0("Variable ", var, " non trouvée dans les données!\n"))
    }
  }
  
  # Analyser les variables qualitatives
  cat("\n\nANALYSE DES VARIABLES QUALITATIVES\n\n")
  for (var in qual_vars) {
    if (var %in% names(data)) {
      tryCatch({
        results_qual[[var]] <- analyze_qual_var(data, var)
      }, error = function(e) {
        cat(paste0("Erreur lors de l'analyse de ", var, ": ", e$message, "\n"))
      })
    } else {
      cat(paste0("Variable ", var, " non trouvée dans les données!\n"))
    }
  }
  
  # Résumé des résultats
  cat("\n\n======================================================\n")
  cat("RÉSUMÉ DES RÉSULTATS\n")
  cat("======================================================\n\n")
  
  cat("Variables quantitatives:\n")
  quant_summary <- data.frame(
    Variable = character(),
    Test = character(),
    p_value = numeric(),
    Significatif = character(),
    stringsAsFactors = FALSE
  )
  
  for (res in results_quant) {
    quant_summary <- rbind(quant_summary, data.frame(
      Variable = res$variable,
      Test = res$test_utilise,
      p_value = round(res$p_value, 4),
      Significatif = ifelse(res$p_value < 0.05, "Oui", "Non"),
      stringsAsFactors = FALSE
    ))
  }
  
  print(quant_summary)
  
  cat("\nVariables qualitatives:\n")
  qual_summary <- data.frame(
    Variable = character(),
    Test = character(),
    p_value = numeric(),
    Significatif = character(),
    stringsAsFactors = FALSE
  )
  
  for (res in results_qual) {
    qual_summary <- rbind(qual_summary, data.frame(
      Variable = res$variable,
      Test = res$test_utilise,
      p_value = round(res$p_value, 4),
      Significatif = ifelse(res$p_value < 0.05, "Oui", "Non"),
      stringsAsFactors = FALSE
    ))
  }
  
  print(qual_summary)
  
  # Retourner les résultats
  return(list(quantitative = quant_summary, qualitative = qual_summary))
}

# Exécution de l'analyse
results <- analyze_all_variables(data)

#============
# Génération de tableaux descriptifs
#============

# Fonction pour afficher moyenne ± écart-type
describe_quant <- function(data, vars_quant) {
  result <- data.frame(Variable = character(),
                       Moyenne_ET = character(),
                       stringsAsFactors = FALSE)
  for (var in vars_quant) {
    if (var %in% names(data)) {
      mean_val <- round(mean(data[[var]], na.rm = TRUE), 2)
      sd_val <- round(sd(data[[var]], na.rm = TRUE), 2)
      result <- rbind(result, data.frame(Variable = var,
                                         Moyenne_ET = paste0(mean_val, " ± ", sd_val)))
    } else {
      cat(paste0("Variable ", var, " non trouvée dans les données!\n"))
    }
  }
  return(result)
}

# Variables quantitatives 
vars_quant <- c("age", "EnvCS.SQ001.", "EnvPSubj.SQ001.", "Engagement")
describe_quant(data, vars_quant)

# Fonction pour tables de contingence des variables qualitatives
describe_qual <- function(data, vars_qual) {
  result_list <- list()
  for (var in vars_qual) {
    if (var %in% names(data)) {
      # Tableau de fréquence
      freq_table <- table(data[[var]])
      # Pourcentages
      percent_table <- round(prop.table(freq_table) * 100, 1)
      # Combiner fréquence et pourcentage
      result <- data.frame(
        Modalité = names(freq_table),
        Fréquence = as.vector(freq_table),
        Pourcentage = as.vector(percent_table)
      )
      result_list[[var]] <- result
    } else {
      cat(paste0("Variable ", var, " non trouvée dans les données!\n"))
    }
  }
  return(result_list)
}

# Variables qualitatives
vars_qual <- c("Genre", "CSP", "Diplome", "CapEco", "EnvPObj", "regime")
qual_tables <- describe_qual(data, vars_qual)

# Afficher chaque table
for (var_name in names(qual_tables)) {
  cat("\nTableau de contingence pour", var_name, ":\n")
  print(qual_tables[[var_name]])
}