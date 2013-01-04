automatch
=========

Script R to visualy confront and correct the discrepancies between to nearly identical tables (particularly useful for large ones: dozens of columns, tens of thousands of lines). 
It is typically usefull in case of double digitazion. It is in almost daily use in our team since February 2012.

To use it simply source the automatch.R file and then call the automatch function:

source("automatch.R")
automatch()

You will be asked what are the files containing each database (should be in .csv). 
Then it will ask you what are the reference fields: the fiels that should be used to match one table to the other.
First a structure check is performed (columns names and number, matching lines).

Finally you will have the opportunity to examin all the discrepancies between the tables, one by one, in a graphical window.
For each discrepancy the key fields are indicated as well as the problematic ones allowing you to easily correct the problematic fields.
If nothing is corrected, you will be offer the possibility to stop the match and save the work done. 
