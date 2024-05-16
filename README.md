## Version History

### Note on Internal vs External codes
Suffix internal/external means that the codes developed are intended for internal (OECD) use or external (country) use.
Internal codes contain more complex codes designed to run faster and compare country data. 
External codes are intended to be kept more simple, so that countries easily understand them.


### 05-06
- new SDMX format in the R codes
- data validation check 1 checks that SDMX is complied with
- in data folder: example data csv which complies with the format and should run without errors.
- General: R codes now run with all indicators.

### 16-06-internal
- new data visualisation for cross country comparison
- validation checks run very slowly on combined datasets, rewrote validation checks with new syntax that makes them run in seconds instead of minutes
