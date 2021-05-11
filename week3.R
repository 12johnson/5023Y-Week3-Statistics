library(tidyverse)
library(skimr)

wood_density <- read_csv("Data/wood_density.csv")

### loading the data

skim(wood_density)
is.na(wood_density)
str(wood_density)

### checking the data
### complete rate 100%, no N/A, data type=col_double

wood_density %>% ggplot(aes(x=Density,
                            y=Hardness))+
  geom_point()+
  geom_smooth(method="lm")

### plots a regression line with a 95% confidence interval

density_model <- lm(Hardness~Density, data=wood_density)
density_model

### creates a model, with Hardness as the dependent and Density as independent variable
### Hardness = -1160.50 + 57.51(Density)













