------------------------------------------------------------------------------
# PROJECT INFORMATION
Current version: 05/05/2024

This project is meant to replace the excel computation of HCQO indicators as part of the OECD Data Modernisation process. The file is intended to be used as a data validation and visualisation tool of the data that is sent, the results file is not required for submission.

This README file is divided into the following 3 sections:
1 - RUNNING THE CODES
    Basic How To guide on executing the codes to produce a results output. For detailed explanations, please consult the HCQO technical guidelines which contains a dedicated section on how to set up R and run the codes.

2 - UNDERSTANDING THE CODES
    Contains details on the structure of the main file, the file contents, what each defined function does.

3 - CHANGING THE CODES
    How to change certain aspects of the code, current bugs, ideas for further development.


------------------------------------------------------------------------------
# RUNNING THE CODES
## Running the main script
1. Preparation and Setup
- Install R and RStudio: Download and install R from the Comprehensive R Archive Network (CRAN) and RStudio from RStudio's website. These tools provide the necessary environment to execute R scripts.
- Project Files Setup: Ensure all project files, particularly the calc_display_2024.Rmd and supporting files in the classes, functions, and data directories, are downloaded from the OECD's HCQO ONE Communities platform and saved in a designated workspace on your machine.

2. Install Required Packages
- Open calc_display_2024.Rmd in RStudio and begin by installing any missing R packages. The script will typically prompt you to install necessary packages. If it doesn’t, manually install them by executing install.packages(c("dplyr", "tidyr", "ggplot2", "shiny", "rmarkdown")) in the console.

3. Running the Script
- Load the Data: Place your data files in the data folder. These files should adhere to the flat file structure as outlined in the guidelines, with proper naming conventions and data types.
- Execute the Script: Run the calc_display_2024.Rmd by clicking the "Knit" button in RStudio, which processes the entire document and outputs a report. You can also run individual code chunks sequentially by clicking the "Run" button at the top of each chunk to facilitate debugging and closer inspection of intermediate results.


------------------------------------------------------------------------------
# UNDERSTANDING THE CODES
## File contents
- calc_display.Rmd: 
    This is the main file. It is used for calculation of the indicators and output. It calls IndicatorClasses and value_mapping.csv:
- functions: 
    A folder containing all the functions used throughout the main script to make it more concise and separable.
- data: 
    A folder in which input data is meant to be saved
- classes:  
    a subfolder that contains a seperate file for each indicator. In these files, we define how the 'Indicator' object is initialized. In particular, we define what kind of population structure is to be used, age groups and scalar with which rates are multiplied by. Furthermore, we provide an overriding definition of the methods if special handling is required. 
- value_mapping.csv:
    - A key-value file where for each indicator, we provide what name its indicator 'value' should have. For example, some PS indicator values are named 'CRUDE_DIS_SURG'. We only change the names at the end, before that each value is called 'std_rate'. 

## Code Sections
The main script is divided into 4 sections:

(1) - Setup
This section loads the R packages necessary to run the script, sets the working directory of the user in order to be able to access the files, and calls predefined functions (can be viewed in the HCQO_validation/function folder) that will be used in the script. It displays the input data as data.tables as is to verify that the expected files are loaded.

(2) - Validation Checks
This section applies validation checks to the data to verify it. The current validation checks performed are the following:
- Validation Check 1: verifies data format, including column names and values
- Validation Check 2: verifies whether any numerators have no matching denominators
- Validation Check 3: verifies whether there are any rates where the corresponding NUM > DEN
Validation checks that have passed will be printed below the chunk. If validation check 1 did not pass, it will print out which columns contain incorrect names or values. If validation checks 2-3 do not pass, a table will be printed out with only the rows that made the check fail.
Dependencies: validate_rates, validate_format, validate_missing_denom.

(3) - Calculate Rates
This section calls the calculate_indicator function, which infers the indicator, calculates rates and confidence intervals for each indicator type, and merges the result into a single dataframe.
Dependencies: calculate_rates, indicator_classes and all the indicators in classes folder.

Please note, chunks 2 and 3 are independent from each other, meaning they can be run even if the other does not work.

(4) - Dashboard
This section generates a dashboard of the inputted data.
Dependencies: dashboard_setup.

(5) - Generate Output
This section generates a csv file inside HCQO_validation/results which will be named according to the date and time of creation of the file.


## Functions
The following functions are defined within the functions folder and used in the main script:
- calculate_indicator.R
- dashboard_setup.R
- indicator_classes.R
- load_csv.R
- validate_format.R
- validate_missing_denom.R
- validate_rates.R


------------------------------------------------------------------------------
# CHANGING THE CODES
## Essential aspects to consider when implementing changess
FILE FORMAT
- Currently the R script only accepts .csv inputs.

COLUMN NAMES
- Mandatory columns (caps sensitive): "REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "UNIT", "OBS_VALUE"
- Optional columns: "AGE", "INCOME"

NB: the order of the columns in the input dataset does not matter.

INDICATORS
- We assume that each indicator is either 'as is' or 'calculation required'. If it is calculation required, meaning that the rates are to be computed and not to be taken from the data.
- If any of the functions are changed, it is important to rerun chunk 1 of the main code to see the effect of the changes.

## How to add new possibilities for data input format
If you want to add options for the R script to take in formats other than csv, you need to add to the load_csv function to include these other formats. In order for the whole script to work, the first chunk of code must output properly formatted data frames with the correct columns.


## How to create a new indicator that belongs to a group
The OOP approach should provide a lot of flexibility for heterogeneity. A seperate class can theoretically be created to tackle each case. 

To add a new indicator:
- make sure to update the corresponding indicator group this indicator belongs to in its respective R code (e.g. AA).
- the indicator name has to be added to the dashboard_setup function, in part #1, in its respective vector group


## How to create a new indicator group
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



## How to add a new input column
If the column name is not included in the MANDATORY or OPTIONAL column names, a new column name should be added. 

## Further work
Bugs/problems
- PE and MP currently not being processed correctly

Ideas for future development
- Efficiency gains. In particular, almost all calculations can be Paralellized
- Adding other validation checks
- Outsourcing all indicator names to a different file
- extend load_csv to include further delimiter/format possibilities
- address warnings
