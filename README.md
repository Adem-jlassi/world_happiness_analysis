World Happiness Analysis

Project Overview

Statistical analysis of the World Happiness Report dataset using R to identify the economic, social, and health-related factors that influence happiness across countries.

The project combines statistical analysis, regression modeling, dimensionality reduction, and clustering techniques to study global happiness trends between 2015 and 2019.



Dataset

World Happiness Report (2015–2019)

Raw Data

* 2015.csv
* 2016.csv
* 2017.csv
* 2018.csv
* 2019.csv

Processed Data

* world_happiness_cleaned.csv
* world_happiness_final.csv

The final dataset contains approximately 781 observations covering around 170 countries over a five-year period.



Methodology

The analysis includes:

* Data Cleaning and Preparation
* Exploratory Data Analysis (EDA)
* Correlation Analysis
* ANOVA
* Chi-Square Test
* Multiple Linear Regression
* Principal Component Analysis (PCA)
* K-Means Clustering


Technologies Used

* R
* ggplot2
* dplyr
* tidyr
* FactoMineR
* factoextra
* lmtest
* sandwich
* car



Repository Structure

world-happiness-analysis/
│
├── README.md
│
├── code/
│   └── world_happiness_analysis.R
│
├── dataset/
│   ├── raw/
│   ├── world_happiness_cleaned.csv
│   └── world_happiness_final.csv
│
├── figures/
│   ├── fig1_happiness_histogram.png
│   ├── fig1b_happiness_by_year.png
│   ├── fig2_corrplot.png
│   ├── fig3_gdp_vs_happiness.png
│   ├── fig3b_freedom_vs_happiness.png
│   ├── fig4_boxplot_region.png
│   ├── fig6_pca_variables.png
│   ├── fig7_pca_biplot.png
│   └── fig8_clusters.png
│
└── report/
    └── world_happiness_report.pdf



Key Results

* GDP per capita, health, and social support showed the strongest positive relationships with happiness.
* Regression analysis explained a large proportion of happiness score variability.
* PCA revealed major latent dimensions related to development and institutional quality.
* K-Means clustering identified distinct groups of countries with similar happiness profiles.
* Significant regional differences in happiness levels were confirmed through statistical testing.



Learning Outcomes

* Statistical Analysis in R
* Data Cleaning and Preparation
* Regression Modeling
* Principal Component Analysis (PCA)
* Cluster Analysis
* Data Visualization
* Hypothesis Testing



Author

Adem Jlassi

BDAD – Big Data & Data Analytics

ISAMM Tunis
