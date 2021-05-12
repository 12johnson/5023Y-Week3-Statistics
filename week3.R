library(tidyverse)
library(skimr)
library(patchwork)

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

### -1160.50 + (57.51*24.7) =259.997
### manual calculation of the lowest Hardness

### coef(density_model)[1] + coef(density_model)[2]*24.7
### This code calculates the hardness using the coefficients from the model
### the result is 259.9152, this is lower as the coefficients have not been rounded

fitted(density_model)

### this code shows the predicted Hardness for the Density's of wood in the data frame
### we can now work out the residuals from the actual Hardness minus the predicted Hardness
### example: 484-259.9152 = 224.0848

wood_density_augmented <- wood_density %>% 
  mutate(predictions=fitted(density_model)) %>% 
  mutate(residuals=Hardness-predictions)

### this code adds two columns
### the first being the predicted Hardness
### the second being the residual (difference between prediction and the actual Hardness)

p1 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line()+
  ggtitle("Full Data")

p2 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=predictions))+
  geom_line()+
  ggtitle("Linear trend")

p3 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=residuals))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining pattern")

p1+p2+p3

### adding all 3 plots together to visually see it.

