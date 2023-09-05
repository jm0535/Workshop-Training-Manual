# Load necessary packages (if not already loaded)
library(rstatix)
library(ggplot2)
library(knitr)

# Load your dataset (assuming it's loaded into a variable named 'forest_data')
forest_data <- readr::read_csv("./docs/data/Filtered.csv")

# Perform a Pearson correlation test
correlation_result <- forest_data %>% 
rstatix::cor_test(
  vars = "Tree_Density_per_ha",
  vars2 = "Aboveground_Tree_Carbon_ton_per_ha",
  alternative = "two.sided",
  method = "pearson",
  conf.level = 0.95,
  use = "pairwise.complete.obs"
)

# Alternatively, you can perform a Spearman correlation test for non-linear relationships:
#correlation_result <- forest_data %>% 
# rstatix::cor_test(
#   vars = "Tree_Density_per_ha",
#   vars2 = "Aboveground_Tree_Carbon_ton_per_ha",
#   alternative = "two.sided",
#   method = "spearman",
#   conf.level = 0.95,
#   use = "pairwise.complete.obs"
# )

# Print the correlation result
print(correlation_result %>% knitr::kable())

# load R package
library(tidyverse)

# summarise and check data
summary(forest_data[1:4])
tibble::glimpse(forest_data)

# default plot function
plot(
  Aboveground_Tree_Carbon_ton_per_ha_per_year ~ 
  Tree_Density_per_ha, data = forest_data,
  ylab = "AGTC (tonnes/ha/year)",
  xlab = "Tree density (ha)",
  type = "p",
  cex = 2
)

abline(lm(forest_data$Aboveground_Tree_Carbon_ton_per_ha_per_year ~ 
      forest_data$Tree_Density_per_ha), lty = 2, col = "red", lwd = 4)

# using ggplot2 functions
ggplot2::ggplot(
  data = forest_data,
  aes(
    x = forest_data$Management_regime,
    y = forest_data$Aboveground_Tree_Carbon_ton_per_ha_per_year
    ,
    fill = forest_data$Management_regime
  )
) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(y = "AGTC (tonnes/ha/year)",
                x = "",
                fill = "Management Regime") +
  ggplot2::theme_bw()

# load R package
library(tidyverse)

# summarise and check data
summary(data[1:4])
tibble::glimpse(data)

# default plot function
plot(
  Aboveground_Tree_Carbon_ton_per_ha_per_year ~
    Tree_Density_per_ha,
  data = data,
  ylab = "AGTC (tonnes/ha/year)",
  xlab = "Tree density (ha)",
  type = "p",
  cex = 2
)
abline(
  lm(
    data$Aboveground_Tree_Carbon_ton_per_ha_per_year ~
      data$Tree_Density_per_ha
  ),
  lty = 2,
  col = "red",
  lwd = 4
)

# using ggplot2 functions
ggplot2::ggplot(data = data,
                aes(x = Tree_Density_per_ha,
                    y = Aboveground_Tree_Carbon_ton_per_ha_per_year)) +
  ggplot2::geom_point() +
  ggplot2::labs(y = "AGTC (tonnes/ha/year)",
                x = "Tree density (ha)") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::theme_bw()

data("CO2")

# Create a histogram of CO2 uptake
hist(CO2$uptake, main = "Histogram of CO2 Uptake", xlab = "CO2 Uptake")
plot(density(CO2$uptake), main = "Density Plot of CO2 Uptake", xlab = "CO2 Uptake")


# measures of central tendancy and variability
_
