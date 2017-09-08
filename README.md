# BMIplot
A simple tool to make a BMI plot of height vs. weight in ggplot2 in R

## Examples
 
### Set up demo data set
```demo_data=read.csv("http://people.ucsc.edu/~cdobkin/NHIS%202007%20data.csv")%>%
   mutate(Weight=weight/2.20462, #convert pounds to kg
          Height=height*2.54,    #convert inches to cm
          Gender=recode(SEX,`1`="Male",`2`="Female"))%>%
   filter(Height<200,Weight<200) #remove ridiculous entries```
### Specify lots
`makeBMIplot(sample_n(demo_data,100),weight="Weight",height="Height",colFactor="Gender",aspect_ratio = 3)`

### Specify colour factor
`makeBMIplot(sample_n(demo_data,100),colFactor="Gender")`

### Don't specify colour factor
`makeBMIplot(sample_n(demo_data,100),alpha=0.3)`

### Add jitter and alpha to counteract overplotting
`makeBMIplot(demo_data,weight="Weight",height="Height",colFactor="Gender",aspect_ratio = 3,jitter=TRUE,alpha=0.4)` 
