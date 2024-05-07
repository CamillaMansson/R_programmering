# Regressionsmodellering

# Dataset: Blocket
# Syfte: Skapa en modell med "Pris på bilen" som Y.

# Ladda in nödvändiga bibliotek
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(caret)
library(glmnet)
library(tidyverse)
library(car)
library(leaps)
library(corrplot)
library(Metrics)

# Läs in data från Excel-fil
file_path <- "C:/Users/camil/Desktop/R programmering/Kunskapskontroll/Blocket.xlsx"
car_data <- read_excel(file_path)

# Kontroll av datan (Missing values och felaktigheter i datan justerades i Excel)
dim(car_data)
head(car_data)
str(car_data)
summary(car_data)
View(car_data)

# Variablerna konverteras till lämpliga typer för att kunna hanteras
car_data$Pris <- as.integer(car_data$Pris)
car_data$Hästkrafter <- as.integer(car_data$Hästkrafter)
car_data$Miltal <- as.integer(car_data$Miltal)
car_data$Färg <- as.factor(car_data$Färg)
car_data$Märke <- as.factor(car_data$Märke)
car_data$Modell <- as.factor(car_data$Modell)

# Kategori för modellår skapas
car_data$Modellår_Kategori <- cut(car_data$Modellår, 
                                  breaks = c(2015, 2021, 2024), 
                                  labels = c("Medel", "Ny"))

car_data$Modellår <- NULL

str(car_data)

# Visualiseringar skapas för att utforska datan -------------------------------------------------------------------

# Histogram för alla numeriska variabler
num_vars <- c("Pris", "Miltal", "Hästkrafter")
num_plots <- lapply(num_vars, function(var) {
  ggplot(car_data, aes(x = !!sym(var))) + 
    geom_histogram(fill = "skyblue", color = "black") +
    labs(title = paste("Histogram avseende", var))
})

# Färgschema för att få rätt färg på färg-kategorierna
color_scheme <- c("Blå" = "#1f77b4", "Vit" = "#f0f0f0", "Röd" = "#d62728", 
                  "Grön" = "#2ca02c", "Gul" = "#ffff00", "Orange" = "#ffa500",
                  "Svart" = "#000000", "Silver" = "#d3d3d3", "Grå" = "#7f7f7f", 
                  "Brun" = "#8c564b")

# Stapeldiagram för att se fördelningen av antal bilar per färg och märke
color_count <- car_data %>%
  count(Färg) %>%
  mutate(Färg = factor(Färg, levels = rev(levels(Färg)))) %>%
  ggplot(aes(x = Färg, y = n, fill = Färg)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_scheme) +
  labs(title = "Antalet bilar fördelat per färg", x = "Färg", y = "Antal bilar", fill = "Color") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


brand_count <- car_data %>%
  count(Märke) %>%
  mutate(Märke = factor(Märke, levels = rev(levels(Märke)))) %>%
  ggplot(aes(x = Märke, y = n, fill = Märke)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_scheme) +
  labs(title = "Antalet bilar fördelat per märke", x = "Märke", y = "Antal bilar", fill = "Brand") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Scatter plots för att visa sambandet mellan Pris och hästkrafter respektive miltal
hp_price_plot <- ggplot(car_data, aes(x = Hästkrafter, y = Pris)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Sambandet mellan hästkrafter och pris")


mileage_price_plot <- ggplot(car_data, aes(x = Miltal, y = Pris)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "Sambandet mellan miltal och pris")


# Boxplots för att se fördelningen av pris för varje märke samt modellår
boxplot_price_by_brand <- ggplot(car_data, aes(x = Märke, y = Pris, fill = Märke)) +
  geom_boxplot() +
  labs(title = "Fördelningen av pris för varje märke") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Märke")

year_plot <- ggplot(car_data, aes(x = Modellår_Kategori, y = Pris, fill = Modellår_Kategori)) +
  geom_boxplot() +
  labs(title = "Fördelningen av pris för respektive modellårskategori")


# Violinplot för fördelningen av Pris för varje färg
color_plot <- ggplot(car_data, aes(x = Färg, y = Pris, fill = Färg)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = color_scheme) +
  labs(title = "Fördelningen av pris för respektive färg")


# Grid.arrange för de olika visualiseringarna
grid.arrange(
  color_count, brand_count,
  ncol = 1
)

grid.arrange(
  num_plots[[1]], num_plots[[2]], num_plots[[3]],
  ncol = 1
)

grid.arrange(
  hp_price_plot, mileage_price_plot,
  ncol = 1
)

grid.arrange(
  boxplot_price_by_brand, color_plot, year_plot,
  ncol = 1
)

# För att se eventuell korrelation mellan de numeriska variablerna
print(correlation_matrix)


# Databearbetning -----------------------------------------------------------------------------

# Färger som har mindre än 30 observationer läggs ihop i "Övriga färger"
color_counts <- car_data %>%
  count(Färg)

sällsynta_färger <- color_counts %>%
  filter(n < 30) %>%
  pull(Färg)

car_data$Färg <- ifelse(car_data$Färg %in% sällsynta_färger, "Övriga färger", as.character(car_data$Färg))

car_data$Färg <- factor(car_data$Färg)


# Märke som har mindre än 30 observationer läggs ihop i "Övriga märken"
brand_counts <- car_data %>%
  count(Märke)

sällsynta_märken <- brand_counts %>%
  filter(n < 30) %>%
  pull(Märke)

car_data$Märke <- ifelse(car_data$Märke %in% sällsynta_märken, "Övriga märken", as.character(car_data$Märke))

car_data$Märke <- factor(car_data$Märke)

# Kontroll av antalet observationer för att överväga om de ska tas bort
antal_ovriga_farger <- sum(car_data$Färg == "Övriga färger")
antal_ovriga_marken <- sum(car_data$Märke == "Övriga märken")

antal_ovriga_farger
antal_ovriga_marken

# Ta bort variabeln "Modell" från hela datan då det är få observationer per modell
car_data <- subset(car_data, select = -Modell)

# Log för att Pris ska bli mer normalfördelat samt histogram för att se skillnaden
car_data$Pris_log <- log(car_data$Pris)
par(mfrow = c(2, 1))
hist(car_data$Pris)
hist(car_data$Pris_log)


# Datan delas upp i träning, validering och test ------------------------------------------------------------------
spec <- c(train = .6, validate = .2, test = .2)

set.seed(123)
g <- sample(cut(
  seq(nrow(car_data)), 
  nrow(car_data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res <- split(car_data, g)

# Kontroll av att datan splittats korrekt
sapply(res, nrow)/nrow(df)
addmargins(prop.table(table(g)))

car_data_train <- res$train
car_data_val <- res$validate
car_data_test <- res$test


# Modeller --------------------------------------------------------------------------------------------------------


# 1. Första modellen - egenvalda variabler
lm_1 <- lm(Pris ~ Modellår_Kategori + Miltal + Hästkrafter, data = car_data_train)
summary(lm_1)
par(mfrow = c(2, 2))
plot(lm_1)
vif(lm_1)

# För att behandla heteroskedascitet. Transformera den beroende variabeln y.
lm_1_log <- lm(Pris_log ~ Modellår_Kategori + Miltal + Hästkrafter, data = car_data_train)
summary(lm_1_log)
par(mfrow = c(2, 2))
plot(lm_1_log)
vif(lm_1_log)

# Beräkna cooks avstånd för att ta bort outliers
cooks_dist <- cooks.distance(lm_1_log)
outlier_index <- which(cooks_dist > 0.07)
car_data_train_out <- car_data_train[-outlier_index, ]

lm_1_log_out <- lm(Pris_log ~ Modellår_Kategori + Miltal + Hästkrafter, data = car_data_train_out)
summary(lm_1_log_out)
par(mfrow = c(2, 2))
plot(lm_1_log_out)
vif(lm_1_log_out)

# Testar att ta bort Modellår för att se skillnaden
lm_1_log_out_updated <- lm(Pris_log ~ Miltal + Hästkrafter, data = car_data_train)
summary(lm_1_log_out_updated)
par(mfrow = c(2, 2))
plot(lm_1_log_out_updated)
vif(lm_1_log_out_updated)

lm_1_log_out_updated2 <- lm(Pris_log ~ Miltal + Hästkrafter, data = car_data_train_out)
summary(lm_1_log_out_updated2)
par(mfrow = c(2, 2))
plot(lm_1_log_out_updated2)
vif(lm_1_log_out_updated2)

# Resultatet blev sämre, därför används den förra modellen men byter till enklare namn.
logic <- lm_1_log_out
summary(logic)



# 2. Andra modellen. Starta från fullständig modell
# Linjär regressionsmodell med alla variabler utom Modell(för få observationer per modell) och en interaktionsterm
lm_2_full <- lm(Pris_log ~ . - Pris + Hästkrafter:Miltal, data = car_data_train)
summary(lm_2_full)
par(mfrow = c(2, 2))
plot(lm_2_full)
vif(lm_2_full)

# Interaktionstermens Miltal:Hästkrafter indikerar enligt VIF-analysen på hög multikollinaritet - tar bort den.
lm_2_full_updated <- lm(Pris_log ~ . - Pris , data = car_data_train)
summary(lm_2_full_updated)
par(mfrow = c(2, 2))
plot(lm_2_full_updated)
vif(lm_2_full_updated)

# Beräkna cooks avstånd för att ta bort outliers
cooks_dist <- cooks.distance(lm_2_full_updated)
outlier_index2 <- which(cooks_dist > 0.02)
car_data_train_out2 <- car_data_train[-outlier_index2, ]

lm_2_full_updated_out <- lm(Pris_log ~ . - Pris , data = car_data_train_out2)
summary(lm_2_full_updated_out)
par(mfrow = c(2, 2))
plot(lm_2_full_updated_out)
vif(lm_2_full_updated_out)

# Behåller bara signifikanta variabler
lm_2_full_updated_out2 <- lm(Pris_log ~ . - Färg - Pris , data = car_data_train_out2)
summary(lm_2_full_updated_out2)
par(mfrow = c(2, 2))
plot(lm_2_full_updated_out2)
vif(lm_2_full_updated_out2)

full <- lm_2_full_updated_out2



# 3. Tredje modellen. Best Subset regression.
lm_3 <- regsubsets(Pris_log ~ . - Pris , data = car_data_train, nvmax = 13)
lm_3_summary <- summary(lm_3)
lm_3_summary


names(lm_3_summary)
lm_3_summary$adjr2
par(mfrow = c(1, 1))
plot(lm_3_summary$adjr2)


coef(lm_3, 3)
plot(lm_3, scale = "adjr2")

lm_3_best <- lm(Pris_log ~ Märke + Miltal + Hästkrafter, data = car_data_train)      
summary(lm_3_best)
par(mfrow = c(2, 2))
plot(lm_3_best)
vif(lm_3_best)

# Beräkna cooks avstånd för att ta bort outliers
cooks_dist <- cooks.distance(lm_3_best)
outlier_index3 <- which(cooks_dist > 0.2)
car_data_train_out3 <- car_data_train[-outlier_index3, ]

best_subset <- lm(Pris_log ~ Märke + Miltal + Hästkrafter, data = car_data_train_out3)      
summary(best_subset)
par(mfrow = c(2, 2))
plot(best_subset)
vif(best_subset)



# Utvärdering --------------------------------------------------------------

# Förutsäg värden för valideringsdata med varje modell
val_pred_logic <- predict(logic, newdata = car_data_val)
val_pred_full <- predict(full, newdata = car_data_val)
val_pred_best_subset <- predict(best_subset, newdata = car_data_val)

# Beräkna RMSE för varje modell
RMSE_val_pred_logic <- RMSE(car_data_val$Pris_log, val_pred_logic)
RMSE_val_pred_full <- RMSE(car_data_val$Pris_log, val_pred_full)
RMSE_val_pred_best_subset <- RMSE(car_data_val$Pris_log, val_pred_best_subset)

# Skapa resultatdataramen
results <- data.frame(
  Model = c("Modell 1", "Modell 2", "Modell 3"),
  RMSE_val_data = c(RMSE_val_pred_logic, RMSE_val_pred_full, RMSE_val_pred_best_subset),
  Adj_R_squared = c(summary(logic)$adj.r.squared, 
                    summary(full)$adj.r.squared,      
                    summary(best_subset)$adj.r.squared),    
  BIC = c(BIC(logic), BIC(full), BIC(best_subset)) 
)

results

# Transformera tillbaka Y = Pris så det blir tolkningsbart
val_pred_logic_exp <- exp(val_pred_logic)
val_pred_full_exp <- exp(val_pred_full)
val_pred_best_subset_exp <- exp(val_pred_best_subset)

# En ny data.frame för att visa resultaten med Pris i rätt format
results_exp <- data.frame(
  Model = c("Modell 1", "Modell 2", "Modell 3"),
  RMSE_val_data = c(rmse(exp(car_data_val$Pris_log), val_pred_logic_exp),
                    rmse(exp(car_data_val$Pris_log), val_pred_full_exp),
                    rmse(exp(car_data_val$Pris_log), val_pred_best_subset_exp)),
  Adj_R_squared = c(summary(logic)$adj.r.squared,
                    summary(full)$adj.r.squared,
                    summary(best_subset)$adj.r.squared),
  BIC = c(BIC(logic), BIC(full), BIC(best_subset))
)

results_exp #Bästa modellen enligt detta resultat var "Full".


# Utvärdering på testdata -----------------------------------------------------------------------------------------

# Prediktera pris på testdata med vald modell
test_pred_full_exp <- exp(test_pred_full)
RMSE_test_data <- rmse(exp(car_data_test$Pris_log), test_pred_full_exp)
print(RMSE_test_data)
                 
# De faktiska priserna
true_prices <- car_data_test$Pris

# Visualisering av prediktioner jämfört med faktiska priser
options(scipen = 999) #tillagd för att notationen på x-axeln skulle bli rätt
plot(test_pred_full_exp, true_prices,
     xlab = "Predikterat pris", ylab = "Faktiskt pris",
     main = "Det predikterade priset vs. det faktiska priset")

# Lägger till en linje för perfekt matchning
abline(a = 0, b = 1, col = "red")


# Konfidensintervall och prediktionsintervall beräknas och omvandlas för bättre förståelse
confidence_intervals <- predict(full, newdata = car_data_test, interval = "confidence", level = 0.95)
prediction_intervals <- predict(full, newdata = car_data_test, interval = "prediction", level = 0.95)

exp(confidence_intervals)
exp(prediction_intervals)


# Tabell för att lagra data 
interval_table <- data.frame(True_Price = car_data_test$Pris,
                             Fit = exp(confidence_intervals[, "fit"]),
                             Lower = exp(confidence_intervals[, "lwr"]),
                             Upper = exp(confidence_intervals[, "upr"]))
                  

# Tabell där varje observation syns med det faktiska priset samt KI
print(interval_table)
