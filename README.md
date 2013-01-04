automatch
=========

Script R to visualy confront and correct the discrepancies between to nearly identical tables. 
It is typically usefull in case of double digitazion of large tables: dozens of columns, tens of thousands of lines). 
It is in almost daily use by my team since February 2012.

Use (example)
---------------
To use it simply source the automatch.R file and then call the automatch function:
```R
source("automatch.R")
automatch()
```
Warning messages concerning RMySQL and gWidgetstcltk can be safely ignored (see Additional Features).  

You will be asked what are the files containing each database (should be in .csv). 

_For example you can compare the d1.csv and the d2.csv given as examples here._

Then it will ask you what are the reference fields: the fiels that should be used to match one table to the other.

_In the example you should use "unicode" and "Codigo.Corral", simply pick Enter._

First a structure check is performed (columns names and number, matching lines).

_For the example databases, you'll see that three lines are only in the first table and 8 only in the second. Here you should first correct these errors but for now let's continue. Hit y and Enter._

Finally you will have the opportunity to examin all the discrepancies between the tables, one by one, in a graphical window.
For each discrepancy the key fields are indicated as well as the problematic ones allowing you to easily correct the problematic fields.

_Our first error is a difference between the two tables for the field "Sillar.Noble", once both lines at the correct value click on the quit button to got to the next error._

If nothing is corrected, you will be offer the possibility to stop the match and save the work done. 


Additional Features
-------------------
If the RMySQL package is installed, you will be offered the possibility to compare tables from a MySQL server and save there the result.
This functionality is working but not actively further developped as is it allows to go a much longer way on a MySQL table than the simple "EXCEPT" or "NOT IN" commands.

In the precence of the R graphical package gWidgetstcltk a graphical interface will allow you to select the files with the tables you want to compare.

Linguistic note
----------------
The dialogs in the script are by default in spanish at it mainly used by our spanish speaking team. English is supported though, simply by changing "sp" to "en" at the first line of the script.

Contributing
------------
You are welcome to contribute to the code in the form of pull requests via GitHub. Corrections of the linguistic are particularly welcome. New languages are also welcome and should be very straight forward to add (see beginning of automatch.R). 

Licensing
---------
You are welcome to use, modify and redistribute this code at your will as long as you make a reference in the code to this GitHub Repo. 

