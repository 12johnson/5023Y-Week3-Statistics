library(tidyverse)
library(skimr)
library(patchwork)
library(emmeans)

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

summary(aov(darwin_model))

### instead of the looking at the estimates in turn we can  
### summarise these differences by an anova test.
### This seems to be back up our observation that pair
### does not significantly affect the mean height,
### while type does. As the Pr value is 0.0497 for type,
### and 0.8597 for pair.

### At this point we can reject a hypothesis that
### the means of the crossed and self plants are equal
### within each pair.
### However we cannot reject a hypothesis that
### the mean heights between the different pairs of plants are equal.

estimates <- emmeans(darwin_model, specs="type")

### here it will take the average of the values
### across all the pairs to calculate means for type

estimates %>% 
  as_tibble %>%
  ggplot(aes(x=type, 
             y=emmean, 
             colour=type))+
  geom_pointrange(aes(ymin=lower.CL, 
                      ymax=upper.CL))+
  geom_pointrange(aes(ymin=emmean-SE, 
                      ymax=emmean+SE), 
                  size=1.2)

### emmeans outputs a grid by default, but can easily be changed
### the code creates a plot that shows the mean height
### and the 66% (thick line) and 95% (thin line) confidence intervals

tidymodel1 <- broom::tidy(darwin_model) %>% 
  mutate(lwr=((estimate-(std.error*2))),
         upr=(estimate+(std.error*2)))

### this code forces the summary statistics into a dataframe
### format as well as using the mutate function to add the 
### upper and lower confidence intervals

tidymodel1 %>% 
  ggplot(aes(x=estimate, 
             y=term))+
  geom_pointrange(aes(xmin=lwr, 
                      xmax=upr))+
  geom_vline(xintercept=0, 
             linetype="dashed")

### this code creates a plot showing the means and 
### the confidence intervals the plot shows that no pair is
### significantly different at the P<0.05 as
### they all cross the zero line

tidymodel2 <- broom::tidy(darwin_model, conf.int=T) 
tidymodel2[2,]

### this code shows the confidence interval
### the second line of code specifies that we only wish to see row 2

tidymodel2 %>% 
  ggplot(aes(x=estimate, 
             y=term))+
  geom_pointrange(aes(xmin=conf.low, 
                      xmax=conf.high))+
  geom_vline(xintercept=0, 
             linetype="dashed")

### this plot has more accurate confidence intervals because
### CI are approximately 2*S.E if the assumption of
### a normal z distribution is true.
### Sample sizes <30 become increasingly further away from
### the z distribution and follow the t distribution instead.
### When R calculates CI probabilities it uses the exact t distribution
### for the sample size.
### We can manually calculate the appropriate t for ourselves
### (though we don’t have to). So that we better understand how
### sample size can affect our ability to make accurate predictions.

t.crit <- qt(0.975, df=14)

upr <- 2.62+t.crit*1.22
lwr <- 2.62-t.crit*1.22

### we can also use the tcrit function to set a threshold
### of 0.975 (equivalent to 0.95 for a two-sided test)

### WRITE UP - WEEK 4

### A paired t-test showed that the cross-pollinated maize
### were significantly taller than the self-pollinated plants
### by an average of 2.6 inches (95% CI: 0.004-5.23).

### Why no F-tests, df or P values? Well in theory this gives
### us the same qualititative information as we know that our
### 95% CI range is >0 so we can say that the difference in
### height is significant at P<0.05. And we are providing direct 
### biological information about the estimated height difference.

### BUT most people will expect you to report F values, 
### df, P values etc. so you should probably include them as well.

### A pairwise linear model showed that the cross-pollinated
### maize were significantly taller than the self-pollinated plants
### (F1,14= 4.61, P = 0.05) by an average of 2.6 inches
### (95% CI: 0.004-5.23)

### EFFECT SIZE: Finally although we know we have a significant
### height difference it is only by a very small margin (0.004 inches).
### In this way we can actually say we have 95% confidence
### that the height difference is at least 0.004 inches.
### If we wanted to we could estimate a range of confidence intervals
### e.g. we have 66% confidence that the height difference is at
###  least 1.4 inches {confint(model, level=0.66)}
