# HCQO Indicators

## Information about this project

This project is meant to replace the excel computation of HCQO indicators. The project is divided into four files/folders:
 - calculate_display.Rmd:
    - This is the main file. It is used for calculation of the indicators and output. It calls IndicatorClasses and value_mapping.csv:
- IndicatorClasses: 
    - Contains definitions of Indicator classes. Also contains OOP definitions and functions that are used to compute and format the dataframes.
 - Classes:
    - a subfolder that contains a seperate file for each indicator. In these files, we define how the 'Indicator' object is initialized. In particular, we define what kind of population structure is to be used, age groups and scalar with which rates are multiplied by. Furthermore, we provide an overriding definition of the methods if special handling is required. 

- value_mapping.csv:
    - A key-value file where for each indicator, we provide what name its indicator 'value' should have. For example, some PS indicator values are named 'CRUDE_DIS_SURG'. We only change the names at the end, before that each value is called 'std_rate'. 
## Getting started
To compute indicators using this project, you must first download the files in this repository. You can download manually by using UI and clicking on the 'Download' icon in the root of the repository. You can also download it directly using Git.

Save the files in under one folder in a similar structure as you see in this repository. You also need to have a data folder where you have csv's of the indicators that you want to calculate.

After downloading the package, open Rstudio on your computer which can be found at "\\main.oecd.org\em_apps\R". You must execute the Rstudio.cmd file. If it is your first time using Rstudio, you will have to download several packages. A pop-up should show up which packages you need to download.


You must also change the path in Calculate_display.Rmd to the root of the project in your local computer. Once you have everything set, you can run the code in Rstudio. The results will be placed in the results folder which will be created to the working directory if it doesn't already exist.


## How to create a new indicator (also helpful for understanding the code)
1. Create a new parent class in IndicatorClasses. Check IndicatorClasses for reference
2. Define how many types there are and define type vectors. Indicator has a different type when any of the following are different
	- population strucuture
	- Age groups
	- Method of standardization
	- Output (for some indicators, we return only the standard rate, etc.)
3. Create a child class that inherits the parent in the /classes folder. It should be two characters.

4. Implement the CalculateStandardization method. By default, standardized rate and CI's are calculated for indicators. You will need to implement a function if that is not the desired output.
5. Implement GetOutputDataframe. Add a new if/else clause if the handling of this indicator is different from the already existing ones.
6. Implement InferIndicatorType. You should add as many cases as there are types.

## Bugs/problems
- For some indicators, the standard rate is calculated correctly, but not the standard errors. Hence, the resulting confidence intervals are wrong. This needs to be investigated as for other indicators, the results are inline with the 'RESULTS' excel sheets. Currently, in my code I calculate standard errors in two ways. One method for all standardized rates and another for nonstandardized. The question is that is there a different way that the standard errors are calculated for some indicators.  **FIXED** Error in R code, now fixed.
 - Indicator PROPPCOU. Possibly an error in calculation as results do not match. **FIXED** Error in Excel calculation not in R
- Indicator ADMECAN; we only see a subset of results although they are correct. This is most likely due to the those observations getting dropped in preprocessing
 phase. May be due to abscence of denominator data, missing data etc,.

## Ideas for future development

- Adding minute suffix to the name of the csv when saving.
- Efficiency gains. In particular, almost all calculations can be Paralellized
- Adding consistency checks, validation and showing results. 

In essence, for the code to work, the data file has to have:
- Columns (caps sensitive): COUNTRY,PERIODS,INDICATOR,GENDER,AGE_GROUP,VALUE,NUMBER
- We assume that each indicator is either 'as is' or 'calculation required'. If it is calculation required, meaning that the rates are to be computed and not to be taken from the data, 
the VALUE column needs to have NUM and DEN for each TIME INDICATOR group. If it is not the case, some changes have to be made to the code. 
For example, in AC indicator, some DEN's are empty for some indicators as they are inferred from another indicator group. Naturally, this needs to be implemented. 

All in all, the OOP approach should provide a lot of flexibility for heterogeneity. A seperate class can theoretically be created to tackle each case. 

Feel free to add comments or ask for clarifications about what the code does. 
