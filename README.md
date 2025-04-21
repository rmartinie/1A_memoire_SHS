# Mémoire 1A SVS 2024-2025
## Les déterminants sociaux et moraux de l'activité physique régulière
### Une opérationnalisation des régimes d'engagement

Romain MARTINIE & Léonine ROUANET (ENS 2SEP) sous la direction de Jean-Nicolas RENAUD (ENS 2SEP)

---

## Description
Ce mémoire vise à comprendre les déterminants sociaux et moraux de l'engagement dans une activité physique régulière (APR) à travers les 3 régimes d'engagement (plan, justification, familiarité) définis par Thévenot en 2006 dans L'Action au pluriel. 

Un questionnaire en ligne basé sur la méthode des scénarios a été réalisé par des personnes majeures (n=441), aléatoirement affectés à un groupe "marche" ou "course". Chaque participant évaluait sa probabilité d'engagement (0-100) pour trois propositions d'APR correspondant aux différents régimes d'engagement. En suivant, des variables sociologiques et la fréquence hebdomadaire de pratique effective ont été recueillies. Les analyses incluaient des tests d'indépendance et des Analyses des Correspondances Multiples.

## Fichiers
### Scripts

`data_process.R`: Traitement des données, ACM, tests Chi², visualisations
`diff_CM.R`: Tests statistiques comparant les groupes "marche" et "course"

### Données

`raw_data.csv`: Données brutes du questionnaire
`structure_questionnaire.lss`: Structure du questionnaire pour importation sur LimeSurvey

## Prérequis

- R : `tidyverse`, `dplyr`, `ggplot2`, `rstatix`, `openxlsx`, `FactoMineR`, `factoextra`
