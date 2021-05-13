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

broom::glance(density_model)
 
### Constructs a concise one-row summary of the model. 
### This typically contains values such as R^2, adjusted R^2,
### your F values, degrees of freedom and P

broom::tidy(density_model, conf.int=TRUE)

### Constructs a small tibble with most of the models summary data in it. 
### Very similar to our summary() output.

broom::augment(density_model, wood_density, interval="confidence")

### Takes computations from our model fit and adds them back onto our original dataframe.
### .fitted = predictions of the model
### .resid = residuals
### .upper is the 95% confidence interval upper value for our fit line
### .lower is the 95% confidence interval lower value for our fit line

plot1 <- broom::augment(density_model, wood_density, 
                        interval="confidence") %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line(aes(x=Density, y=.fitted))+
  geom_line(aes(x=Density, y=.upper), linetype="dashed")+
  geom_line(aes(x=Density, y=.lower), linetype="dashed")+
  geom_point() +
  ggtitle("Manually fitting linear model \n and confidence intervals")

plot2 <- wood_density %>% ggplot(aes(x=Density, y=Hardness))+
  geom_smooth(method=lm)+
  geom_point()+
  ggtitle("Geom smooth method to plotting \n a linear model")

plot1+plot2

### plot1 shows the code to work out the confidence interval manually
### plot2 shows the code to make the plot with geom_smooth()
### they produce the same plot with great accuracy

### write up 
### Wood density is an excellent predictor of timber hardness. 
### On average for every pound per cubic foot increase in the density
### of wood, we see a 57.5 point increase in the Janka “hardness scale” 
### (F1,34= 637, P <0.001, R^2 = 0.94).


### week 4 workshop below 

darwin <- read_csv("Data/darwin.csv")

### read in the data

darwin <- darwin %>% 
  pivot_longer(cols=c("Self":"Cross"), 
               names_to="type", 
               values_to="height") %>% 
  mutate(type=factor(type, 
                     levels=c("Self",
                              "Cross"))) %>% 
  mutate(pair=factor(pair))

### tidy data
### changing data type to a more appropriate one

darwin_model <- lm(formula = height ~ type + pair, data = darwin)
darwin_model

### code for a linear model that includes type and pair
### selfed plant from pair 1 is an inbred plant from parent 1 
### crossed plant has one parent as parent 1 and 
### was outbred to have one other parent.
### This is now the equivalent of a paired t-test. 
### As the estimate of height difference now excludes
### any variance accounted for by the pairs.

summary(darwin_model)

### Well it looks as though when you include variance
### explained by pairing, the type Cross/Inbred is still significant.
### However none of the pair groups appear to
### significantly alter the mean height of plants.

### On average within each pair the crossed plant is taller
### than the selfed plant.
### There is not a difference in the average height
### of plants between different pairs.

