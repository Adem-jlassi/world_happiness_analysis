# ============================================================
# WORLD HAPPINESS REPORT – ANALYSE STATISTIQUE
# ============================================================

packages <- c(
  "dplyr", "ggplot2", "corrplot", "car",
  "FactoMineR", "factoextra",
  "lmtest", "sandwich", "countrycode"
)

installed <- rownames(installed.packages())

for (p in packages) {
  if (!p %in% installed) install.packages(p, dependencies = TRUE)
}

library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(FactoMineR)
library(factoextra)
library(lmtest)
library(sandwich)
library(countrycode)

# ------------------------------------------------------------
# Chargement des fichiers
# ------------------------------------------------------------

d2015 <- read.csv("2015.csv", stringsAsFactors = FALSE)
d2016 <- read.csv("2016.csv", stringsAsFactors = FALSE)
d2017 <- read.csv("2017.csv", stringsAsFactors = FALSE)
d2018 <- read.csv("2018.csv", stringsAsFactors = FALSE)
d2019 <- read.csv("2019.csv", stringsAsFactors = FALSE)

# ------------------------------------------------------------
# Standardisation
# ------------------------------------------------------------

standardize_world_happiness <- function(df, year) {

  names(df) <- make.names(names(df))

  pick_first <- function(possible_names) {
    for (n in possible_names) {
      if (n %in% names(df)) return(df[[n]])
    }
    return(rep(NA, nrow(df)))
  }

  out <- data.frame(
    Year       = year,
    Country    = pick_first(c("Country","Country.or.region","Country.or.Region")),
    Region     = pick_first(c("Region","Regional.indicator")),
    Happiness  = pick_first(c("Happiness.Score","Score","Ladder.score")),
    GDP        = pick_first(c("Economy..GDP.per.Capita.","GDP.per.capita","Logged.GDP.per.capita")),
    Family     = pick_first(c("Family","Social.support")),
    Health     = pick_first(c("Health..Life.Expectancy.","Healthy.life.expectancy")),
    Freedom    = pick_first(c("Freedom","Freedom.to.make.life.choices")),
    Generosity = pick_first(c("Generosity")),
    Corruption = pick_first(c("Trust..Government.Corruption.","Perceptions.of.corruption")),
    stringsAsFactors = FALSE
  )

  out$Region[out$Region == "" | is.na(out$Region)] <- NA

  return(out)
}

d2015_c <- standardize_world_happiness(d2015, 2015)
d2016_c <- standardize_world_happiness(d2016, 2016)
d2017_c <- standardize_world_happiness(d2017, 2017)
d2018_c <- standardize_world_happiness(d2018, 2018)
d2019_c <- standardize_world_happiness(d2019, 2019)

# ------------------------------------------------------------
# Fusion et nettoyage
# ------------------------------------------------------------

data <- rbind(d2015_c, d2016_c, d2017_c, d2018_c, d2019_c)

numeric_cols <- c(
  "Happiness","GDP","Family",
  "Health","Freedom","Generosity","Corruption"
)

data[numeric_cols] <- lapply(
  data[numeric_cols],
  function(x) as.numeric(as.character(x))
)

data$Region <- as.character(data$Region)

missing_region <- is.na(data$Region) | data$Region == ""

if (any(missing_region)) {

  continent_guess <- countrycode(
    data$Country[missing_region],
    origin = "country.name",
    destination = "continent",
    custom_match = c(Kosovo = "Europe")
  )

  data$Region[missing_region] <- continent_guess
}

data$Region[is.na(data$Region) | data$Region == ""] <- "Other"

data$Region  <- as.factor(data$Region)
data$Country <- as.factor(data$Country)
data$Year    <- as.factor(data$Year)

data$HappinessLevel <- cut(
  data$Happiness,
  breaks = c(0, 4, 6, 10),
  labels = c("Low","Medium","High"),
  include.lowest = TRUE
)

data <- na.omit(data)

cat("Dimensions finales :", dim(data), "\n")

write.csv(data, "world_happiness_cleaned.csv", row.names = FALSE)

# ============================================================
# ANALYSE UNIVARIÉE
# ============================================================

cat("\n==== ANALYSE UNIVARIÉE ====\n")

print(summary(data[, numeric_cols]))

cat("\nTests de normalité :\n")

for (v in numeric_cols) {

  sw <- shapiro.test(data[[v]])

  cat(sprintf(
    "  %-12s : W = %.4f | p = %.4e\n",
    v, sw$statistic, sw$p.value
  ))
}

cat("\nFréquences des régions :\n")
print(sort(table(data$Region), decreasing = TRUE))

cat("\nFréquences HappinessLevel :\n")
print(table(data$HappinessLevel))

print(round(
  prop.table(table(data$HappinessLevel)) * 100,
  1
))

p1 <- ggplot(data, aes(x = Happiness)) +
  geom_histogram(
    bins = 20,
    fill = "#2e86ab",
    color = "white"
  ) +
  geom_vline(
    xintercept = mean(data$Happiness),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = mean(data$Happiness) + 0.25,
    y = 60,
    label = paste0(
      "Moy. = ",
      round(mean(data$Happiness), 2)
    ),
    color = "red",
    size = 3.5
  ) +
  labs(
    title = "Distribution du Score de Bonheur",
    x = "Score de bonheur",
    y = "Nombre"
  ) +
  theme_minimal(base_size = 12)

print(p1)

ggsave(
  "fig1_happiness_histogram.png",
  p1,
  width = 8,
  height = 5,
  dpi = 300
)

p1b <- ggplot(data, aes(x = Year, y = Happiness, fill = Year)) +
  geom_boxplot(
    alpha = 0.8,
    outlier.color = "red"
  ) +
  labs(
    title = "Bonheur par année",
    x = "Année",
    y = "Score de bonheur"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(p1b)

ggsave(
  "fig1b_happiness_by_year.png",
  p1b,
  width = 8,
  height = 5,
  dpi = 300
)

# ============================================================
# ANALYSE BIVARIÉE
# ============================================================

cat("\n==== ANALYSE BIVARIÉE ====\n")

num_data <- data.frame(
  lapply(data[, numeric_cols], as.numeric)
)

cor_matrix <- cor(num_data, use = "complete.obs")

cat("\nMatrice de corrélation :\n")
print(round(cor_matrix, 3))

png(
  "fig2_corrplot.png",
  width = 800,
  height = 700,
  res = 120
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.cex = 0.9,
  col = colorRampPalette(
    c("#c0392b","white","#2e86ab")
  )(200)
)

dev.off()

cat("\nTests de corrélation :\n")

for (v in setdiff(numeric_cols, "Happiness")) {

  ct <- cor.test(data$Happiness, data[[v]])

  cat(sprintf(
    "  Happiness ~ %-12s : r = %.3f | p = %.2e\n",
    v, ct$estimate, ct$p.value
  ))
}

p3 <- ggplot(data, aes(
  x = GDP,
  y = Happiness,
  color = Region
)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    linewidth = 1.2
  ) +
  labs(
    title = "PIB vs Bonheur",
    x = "PIB par habitant",
    y = "Score de bonheur"
  ) +
  theme_minimal(base_size = 11)

print(p3)

ggsave(
  "fig3_gdp_vs_happiness.png",
  p3,
  width = 9,
  height = 6,
  dpi = 300
)

p3b <- ggplot(data, aes(
  x = Freedom,
  y = Happiness,
  color = Region
)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    linewidth = 1.2
  ) +
  labs(
    title = "Liberté vs Bonheur",
    x = "Freedom",
    y = "Score de bonheur"
  ) +
  theme_minimal(base_size = 11)

print(p3b)

ggsave(
  "fig3b_freedom_vs_happiness.png",
  p3b,
  width = 9,
  height = 6,
  dpi = 300
)

p5 <- ggplot(
  data,
  aes(
    x = reorder(Region, Happiness, median),
    y = Happiness,
    fill = Region
  )
) +
  geom_boxplot(alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Bonheur par région",
    x = NULL,
    y = "Score de bonheur"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

print(p5)

ggsave(
  "fig4_boxplot_region.png",
  p5,
  width = 9,
  height = 7,
  dpi = 300
)

anova_model <- aov(Happiness ~ Region, data = data)

cat("\nANOVA :\n")
print(summary(anova_model))

cat("\nTest de Tukey :\n")

tukey <- TukeyHSD(anova_model)

tukey_df <- as.data.frame(tukey$Region)

significant_pairs <- tukey_df[
  tukey_df$`p adj` < 0.05,
]

cat(
  "Nombre de paires significatives :",
  nrow(significant_pairs),
  "\n"
)

tab <- table(data$Region, data$HappinessLevel)

cat("\nTest du Chi-deux :\n")

chi2_test <- chisq.test(tab)

print(chi2_test)

cat("\nProportions par région :\n")

print(round(
  prop.table(tab, margin = 1) * 100,
  1
))

# ============================================================
# ANALYSE MULTIVARIÉE
# ============================================================

cat("\n==== ANALYSE MULTIVARIÉE ====\n")

model_basic <- lm(
  Happiness ~ GDP + Family + Health +
    Freedom + Generosity + Corruption,
  data = data
)

cat("\nModèle de base :\n")
print(summary(model_basic))

model_year <- lm(
  Happiness ~ GDP + Family + Health +
    Freedom + Generosity + Corruption + Year,
  data = data
)

cat("\nModèle avec Year :\n")
print(summary(model_year))

cat("\nComparaison des modèles :\n")
print(anova(model_basic, model_year))

cat("\nVIF :\n")
print(vif(model_year))

cat("\nErreurs robustes HC1 :\n")

print(
  coeftest(
    model_year,
    vcov = vcovHC(model_year, type = "HC1")
  )
)

par(mfrow = c(2, 2))
plot(model_year)
par(mfrow = c(1, 1))

cat("\nShapiro sur les résidus :\n")
print(shapiro.test(residuals(model_year)))

cat("\nBreusch-Pagan :\n")
print(bptest(model_year))

cat("\nncvTest :\n")
print(ncvTest(model_year))

cooks <- cooks.distance(model_year)
hat_values <- hatvalues(model_year)

n_influential <- sum(cooks > (4 / nrow(data)))
n_leverage <- sum(hat_values > 2 * mean(hat_values))

cat(
  sprintf(
    "\nPoints influents : %d\n",
    n_influential
  )
)

cat(
  sprintf(
    "Points à levier élevé : %d\n",
    n_leverage
  )
)

influencePlot(
  model_year,
  main = "Graphique d'influence"
)

# ============================================================
# ACP
# ============================================================

cat("\n==== ACP ====\n")

pca_data <- data.frame(
  lapply(
    data[, c(
      "GDP","Family","Health",
      "Freedom","Generosity","Corruption"
    )],
    as.numeric
  )
)

pca_result <- PCA(
  pca_data,
  scale.unit = TRUE,
  graph = FALSE
)

cat("\nValeurs propres :\n")
print(pca_result$eig)

cat("\nLoadings :\n")
print(round(pca_result$var$coord, 3))

fviz_eig(
  pca_result,
  addlabels = TRUE,
  ylim = c(0, 55)
)

p6 <- fviz_pca_var(
  pca_result,
  repel = TRUE,
  col.var = "contrib",
  gradient.cols = c(
    "#f0a500",
    "#2e86ab",
    "#1a3a5c"
  )
)

print(p6)

ggsave(
  "fig6_pca_variables.png",
  p6,
  width = 8,
  height = 6,
  dpi = 300
)

big_regions <- names(
  which(table(data$Region) >= 5)
)

data_for_pca <- data[
  data$Region %in% big_regions,
]

pca_data2 <- data.frame(
  lapply(
    data_for_pca[, c(
      "GDP","Family","Health",
      "Freedom","Generosity","Corruption"
    )],
    as.numeric
  )
)

pca_result2 <- PCA(
  pca_data2,
  scale.unit = TRUE,
  graph = FALSE
)

p7 <- fviz_pca_biplot(
  pca_result2,
  habillage = data_for_pca$Region,
  addEllipses = TRUE,
  ellipse.type = "norm",
  ellipse.level = 0.90,
  repel = TRUE
)

print(p7)

ggsave(
  "fig7_pca_biplot.png",
  p7,
  width = 10,
  height = 7,
  dpi = 300
)

# ============================================================
# K-MEANS
# ============================================================

cat("\n==== K-MEANS ====\n")

set.seed(123)

km <- kmeans(
  scale(pca_data),
  centers = 3,
  nstart = 25
)

data$Cluster <- as.factor(km$cluster)

cat("\nTailles des clusters :\n")
print(table(data$Cluster))

cat("\nMoyennes par cluster :\n")

cluster_means <- aggregate(
  pca_data,
  by = list(Cluster = data$Cluster),
  mean
)

print(data.frame(
  Cluster = cluster_means$Cluster,
  round(cluster_means[, -1], 3)
))

cat("\nBonheur moyen par cluster :\n")

print(
  aggregate(
    Happiness ~ Cluster,
    data = data,
    FUN = mean
  )
)

p9 <- fviz_cluster(
  km,
  data = scale(pca_data),
  geom = "point",
  ellipse.type = "convex",
  palette = c(
    "#2e86ab",
    "#f0a500",
    "#2d6a4f"
  )
)

print(p9)

ggsave(
  "fig8_clusters.png",
  p9,
  width = 9,
  height = 6,
  dpi = 300
)

# ============================================================
# Sauvegarde finale
# ============================================================

write.csv(
  data,
  "world_happiness_final.csv",
  row.names = FALSE
)

cat("\n========================================\n")
cat("Analyse terminée avec succès\n")
cat("Fichiers et figures sauvegardés.\n")
