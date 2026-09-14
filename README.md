# RO_Census_2021
The repository of the R project, made for the paper "Determining rank-size relations including outliers for territorial units of Romania"

The structure of this project is based on 5 different R files, because a complete analysis for a single county comprises at least 500 lines of code. The files are organized as follows:
- ***RO_Census_emp_laws_popsize.R*** -> it contains the descriptive statistics of the data and the complete analysis on initial OLS regressions, for each county; based on this file, it can be extracted the values of outliers, using the specific metrics, and also it can be computed the classical assumptions tests for homoscedasticity, autocorrelation of the residuals and the errors normality.
- ***RO_Census_outlierswo_popsize.R*** -> it contains the complete analysis on the re-estimated models, after the outliers removal, for each county; based on this file, it can be computed the new values of the classical assumptions tests for homoscedasticity, autocorrelation of the residuals and the errors normality.
- ***RO_Census_compare_popsize.R*** -> it contains the analysis on all regression metrics (R-Squared, Adjusted R-Squared, F-Statistic, Akaike Information Criterion), where the best-fit initial models are compared with best-fit new models after the outliers removal.
- ***RO_Census_testRobust_popsize.R*** -> it contains the complete analysis on initial OLS regressions and on robust regressions, for each county; based on this file, it was computer new regression metrics, such as Bayesian Information Criterion, Root Mean Square Error and Mean Absolute Error for all the models (OLS and robust); after estimating each pair of models, it was calculated the Root Mean Square Error and Mean Absolute Error by each segment (Tail, Body, Head); also, in this file, it was done the complete comparison between the OLS and the robust regressions.
- ***RO_Census_function.R*** -> it contains custom functions used in all the R files; it includes the function to determine the parameters values for Zipf-Mandelbrot distribution in OLS using a Monte-Carlo simulation, the modified version of this function for the robust regression and a function to eliminate the outliers from the desired sample.

The rules for running the scripts are as follows:

1. The entire ***RO_Census_emp_laws_popsize.R*** file must be executed, along with the final export commands for xlsx.
2. Then, the ***RO_Census_outlierswo_popsize.R*** file should be executed, along with the final export commands for xlsx.
3. Finally, the ***RO_Census_compare_popsize.R*** file needs to be executed, to import the final xlsx files, so thus the comparison between initial OLS models and the new one after outliers removal to be done.
4. The ***RO_Census_testRobust_popsize.R*** file can be executed the last, because it does not have any dependencies on the other files.

Since each R file takes at least 2–3 hours to run on average, I have included the resulting exports in the "Export" folder of this repository, to make it easier to verify the correctness of the scripts. In any case, if the scripts are run fully in the described order, the newly generated files will be exported directly to the project folder rather than the "Export" folder.

The list of used packages and their version in R is as follows:
- car 3.1.5
- DMwR 0.4.1
- e1071 1.7.17
- ggpmisc 1.0.0
- gridExtra 2.3.1
- gtsummary 2.5.1
- jmv 2.8.0
- lmtest 0.9.40
- MASS 7.3.65
- olsrr 0.7.0
- openxlsx 4.2.8.1
- paletteer 1.7.0
- readxl 1.5.0
- rstatix 1.0.0
- scales 1.4.0
- skedastic 2.0.3
- tidytext 0.4.3
- tidyverse 2.0.0
- treemapify 2.6.1
- tseries 0.10.61
