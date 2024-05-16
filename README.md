## Version History

### Note on Internal vs External codes
Suffix internal/external means that the codes developed are intended for internal (OECD) use or external (country) use.
Internal codes contain more complex codes designed to load larger datasets (several 100k) and compare country data in the dashboard. 
External codes are intended to be kept more simple, so that countries easily understand them.

### 04-19
- all functions outsourced to separate folder to make the main script shorter and cleaner
- validation checks separated from calculate rates chunk
- visualisation includes both bar and line graphs, as well as confidence intervals
- dropdown menu to select indicators/sex/reference area

### 04-25
- new R code to handle change in column names (now REF_AREA, TIME_PERIOD, AGE, SEX, OBS_VALUE, MEASURE, UNIT)
- new load_csv.R function to handle csvs where the data is inputted in a single column
- additional validate_format.R function to check that new column names are respected (Validation Check 1)


### 05-06
- new R code to handle modified data to comply with SDMX standards (concerns TIME, AGE values)
- detailed comments in each function to explain parts of the code
- README file rewrote and restructured
- data validation check 1 checks that SDMX is complied with
- in data folder: example data csv which complies with the format and should run without errors.
- General: R codes now run with all indicators

### 16-06-internal
- new output at the end of chunk 1 to previsualise the data as a tibble
- new data visualisation for cross country comparison, which allows several REF_AREAs to be selected
- validation checks run very slowly on combined datasets, rewrote validation checks with new syntax that makes them run in seconds instead of minutes


## Ideas for improvement
- automatic assignment of indicators contained in input data when displaying shiny interface
- internal: anomalies detection in validation check
- new validation check for missing data (e.g. a data point is missing)

