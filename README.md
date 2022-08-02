# dcf - Stata code to create CSPro dictionary 

**dcf** TBD

## **Installation**

Type

`net install dcf , from("https://raw.githubusercontent.com/petbrueck/dcf/main/src") replace`  

in your Stata command window. Once installed, type `help dcf` to retrieve more information about the package.


## **Overview**
TBD


## **Example**
Show that if large dataset you can simply keep 1 obs.

```
*OPEN DATASET OF YOUR MAIN SURVEY DATA (INTERVIEW_LEVEL)
use "main_intlevel_dataset.dta",clear

**SET UP DICTIONARY
dcf setup, dictionary("Example Dictionary") questionnaire("Survey Form X") iditems(idvar1 idvar2 idvar3) 

**ADD A MAIN INTERVIEW LEVEL RECORD
dcf addrecord using  "EXAMPLE_DICTIONARY.dcf", record("Interview Level") iditems(idvar1 idvar2 idvar3)

*OPEN DATASET OF ROSTER, E.G. PERSON ROSTER
*ASSUMPTION: iditems VARIABLES ARE IN THERE AND IN CORRECT ORDER
use "person_roster.dta",clear

dcf addrecord using  "EXAMPLE_DICTIONARY.dcf", record("Person Roster") iditems(idvar1 idvar2 idvar3) 
```



## Disclaimer
No responsibility or liability for the correct functionality of the commands is taken!

Please report any bug, issues or provide feature requests!

Last revision based on CSPro Version 7.7.2