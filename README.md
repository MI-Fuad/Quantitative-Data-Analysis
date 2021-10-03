# Quantitative-Data-Analysis

## DESCRIPTION OF THE ASSESSMENT

The dataset for this assessment is called Aids2ann and contains annualized data on patients diagnosed with AIDS in Australia before 1 July 2001.
The data contains 6014 observations on the following 9 variables:

| Variables     | Description                                                                             | 
| ------------- |:---------------------------------------------------------------------------------------:| 
| state         | Grouped State of Origin                                                                 | 
| sex           | Sex of patient                                                                          |   
| diag          | Julian date of diagnosis (the number of days since 1970-01-01)                          | 
| death         | Julian date of death or end of observation                                              |
| status        | "A" (Alive) or "D" (dead) at the end of observation                                     | 
| T.categ       | Reported Transmission category                                                          | 
| age           | Age(years) at diagnosis                                                                 | 
| year          | The year of observation (normal calendar)                                               |  
| outcome       | ‘1’ if the patient died in the year of observation specified in ‘year’, ‘0’ if survived | 

## Tasks carried out:
### Exploratory data analysis
- Possible issues with dataset
- Summary statistics of categorical and quantitative variables
- Outlier detection
### Pairwise associations between variables 
- Chi-squared test for independence
### Logistic Regression
- Multiple logistic regresission
- Likelihood Ratio Test (LRT)
- Analysis of covariate
