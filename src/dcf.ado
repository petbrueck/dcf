*! version 0.0.1 August 2022
*! Author: Peter Brueckmann, p.brueckmann@mailbox.org

*TODO: 2) IF ONLY ONE VALUE LABEL (e.g. Age "Less than one year"), ADD THE REMAINING VALUES
*TODO: RecordTypeLen STILL NEEDS TO BE UPDATED WHENVEVER U ADD A RECORD
*TODO: IF VALUESET MORE THAN 500 UNIQUE LABELS, RETURN ERROR OR DON'T DO
*TODO: ADD TO ITEM. Decimal=6 (?) CAN ACTUALL BE EASILY SOURCED FROM STRINGCOUNT
*TODO: FLAG ALL INPUTS AND IMPROVE INPUTS (e.g. maxrecord only allow 1 digit)
*TODO: GO THROUGH ALL MISC. TODOS IN FILE
*TODO: PRECISION ISSUE OF DOUBLE. WILL GIVE ISSUES?
*TODO: SECURITYOPTIONS REALLY NEEDED? OR DO RANDOM NUMBER?
*TODO: COMMANDS AFTER SETUP: CHECK IF FILE EXISTS!
*TODO: DO BOOKMARKS & BETTER DOCUMENTATION

capture prog drop dcf
program define dcf
	
**# 1. IDENTIFY SUBCOMMAND
********************************************************************************
gettoken proc 0 : 0, parse(" ,")

	if length("`proc'")==0 {
		di as error "no subcommand specified. See help on {help dcf##|_new:dcf}"
		exit 198
	}
	if "`proc'"=="setup" {
	dcf_setup `0'
	}
   	else if "`proc'"=="addrecord" {
	dcf_addrecord `0'
	}
	else {
	di as error "Unrecognized command. Check subcommand. See help on {help dcf##|_new:dcf}"
	ex 199
	}
end




**# 2. UTILITY PROGRAMS
********************************************************************************

 **# INVESTIGATE A VARIABLE AND RETURN LENGTH & DATA TYPE & FORMAT
capture program drop investigate_item

program investigate_item, rclass

syntax varlist

*GET THE LENGTH OF VARIABLE: 
*IDENTIFY TYPE
mata:  st_local("item_type",st_vartype("`varlist'") )  
*FOR STRING
if regexm("`item_type'","^str")==1 {
	*IDENTIFY VALUE  BASED ON STRING TYPE
	loc item_length=substr("`item_type'",4,.)
	*IF 0 (NO VALUE ENTERED) CHANGE TO 1 
	if `item_length'==0 loc item_length=1
	*IF ABOVE 255, SET TO 255 
	if `item_length'>255 | "`item_type'"=="strL" loc item_length=255
}
*FOR NUMERIC. 
else if regexm("`item_type'","^str")==0 {

	*IF VALUE LABEL EXIST, TAKE THOSE, AS POTENTIALLY UNDERLYING DATA DOES NOT CONTAIN THE MAX LENGTH. WOULD CAUSE ISSUES IN READING DICT. 
    mata:  st_local("value_label",st_varvaluelabel("`varlist'"))
	if "`value_label'"!="" {
			//GET THE VALUE LABELS 
			mata: st_vlload(st_varvaluelabel("`varlist'"), values=., text=.)
			//IDENTIFY LENGTH OF MAX IN VALUES OF VALUE LABEL
			mata: st_local("item_length",strofreal(strlen(strofreal(max(values),"%16.0g"))))
	}
	*OTHERWISE, TAKE SIMPLY FROM DATA: 
	*TODO: ADD A NOTE HERE TO USER?
	else if "`value_label'"=="" mata: st_local("item_length", strofreal(strlen(strofreal(max(st_data(.,"`varlist'")),"%16.0g"))))

	*IF DOUBLE DO SPECIAL
	*TODO: ACTUALLY NECESSARY OR BY DEFAULT 13? 
	if "`item_type'"=="double" & "`value_label'"=="" {
		noi dis as result "Attention. Variable `varlist' of type 'double'. Might lead to conversion and/or precision issues"
		*GET MAX AND COUNT LENGTH WITHOUT DECIMAL
		mata: st_local("item_length", strofreal(max(st_data(.,"`varlist'")),"%16.0g"))
		loc item_length=length(subinstr("`item_length'",".","",.))
	}
	
	*IF LARGER THAN 15, SET TO 15
	if `item_length'>15 noi dis as result "Attention. Numeric variable `varlist' has length>15. Set to 15 to comply with CSPro"
	if `item_length'>15 loc item_length=15

}

*RETURN THE ITEM LENGTH
return local item_length= `item_length'
*AS WELL AS DATATYPE 
return local csprodata_type=cond(regexm("`item_type'","^str")==1,"Alpha","Numeric")
*AND THE UNDERLYING VARIABLE TYPE
return local variable_type="`item_type'"
end 


 **# GET THE FULL LENGTH OF ALL ITEMS SPECIFIED (USUALLY THE IDITEMS)
capture program drop record_start

program record_start, rclass

syntax varlist

*GET THE LENGTH OF VARIABLE: 
loc record_start=1
foreach var of var `varlist' {
	investigate_item `var'
	loc record_start=`record_start'+`r(item_length)'
}

*RETURN THE ITEM LENGTH
return local record_start= `record_start'
end 



 **# WRITE THE "[ITEM]" PART OF DICTIONARY.
capture program drop write_item

program write_item

syntax , itemlabel(string) itemname(string) start(integer)

**FIRST MAIN CONTENT
	 #d ;
	file write dictionary  
	 _newline
	 _newline
	"[Item]"  _newline
	"Label=`itemlabel'" _newline
	"Name=`itemname'" _newline
	"Start=`start'" _newline
	"Len=`r(item_length)'" ;
	#d cr

** IF ALPHA ADD TYPE 
if "`r(csprodata_type)'"=="Alpha" file write dictionary _newline "DataType=Alpha"
**IF DOUBLE ADD DECIMAL
if "`r(variable_type)'"=="double" file write dictionary _newline "DecimalChar=Yes"

end 



 **# WRITE THE "[ValueSet]" PART OF DICTIONARY. 
capture program drop write_valueset

program write_valueset

syntax , variable(string) vslabel(string) vsname(string) 

	   	**FIRST WRITE THE HEADER
		#d ;
		file write dictionary  
		 _newline
		 _newline
		"[ValueSet]" _newline
		"Label=`vslabel'" _newline
		"Name=`vsname'" ;
		#d cr
		
		*NOW WRITE THE ACTUAL VALUES 
		//GET THE VALUE LABELS 
		mata: st_vlload(st_varvaluelabel("`variable'"), values=., text=.)
		//STORE THE VALUE LABELS IN A MATRIX
		mata:VL_MATRIX= strofreal(values,"%16.0g"),text
		//TEMPORARILY CREATE A NEW DATASET THAT STORES THE VALUE LABELS
		preserve
		 clear
		 **GET IT INTO DATA FRAME
		 getmata(code label)=VL_MATRIX
		 
		 *CREATE VARIABLE THAT STORES THE CSPROP VALUE PATTERN
		 g cspro_value="Value="+code+";"+label
		 *GO THROUGH EACH ROW 
		 *TODO: POSSIBLE TO DO IN ONELINER W/OUT LOOP?
		 forvalues row=1/`c(N)' {
			file write dictionary _newline (cspro_value[`row'])
		 }
		restore

end 


********************************************************************************
**# 3.  THE MAIN PROGRAMS
********************************************************************************


 **# 3.1  dcf_setup
********************************************************************************

capture program drop dcf_setup

program dcf_setup
*TODO: FLAG IF LABELS (E:G: QUESTIONNAIRE OR VARIABLE) ARE MORE THAN 255. CURRENTLY SILENTLY SHORTENS

syntax ,  dictionary(string) ///
		  questionnaire(string) ///
		  iditems(varlist) ///
		  [FOLDER(string)]

**# PROCESS THE INPUT 
**FOLDER
if length("`folder'")>0 {
mata : st_numscalar("OK", direxists("`folder'"))
	if scalar(OK)==0 {
		noi dis as error _n "Attention. Folder: ""`folder'"" not found."
		noi dis as error  "Please correctly specify {help dcf_setup##dcf_setup:folder(string)}"
		ex 601
	}
}
if "`folder'"=="" loc folder "`c(pwd)'"

**QUESTIONNAIRE
*CORRECT NAME
 loc questionnaire_name=upper(ustrregexra("`questionnaire'","\s|\t|\n","_"))
*CORRECT LABEL
 loc questionnaire_label=substr("`questionnaire'",1,255)

**NAMES OF INPUT
****************
**TODO: CSPRO REQUIREMENT The first character of a name must be a letter; the last character cannot be an underscore
 **# DICTIONARY NAME 
 loc dictionary_label=upper(ustrregexra("`dictionary'","\s|\t|\n","_")) 
 
**# MAIN SETTINGS/LOCALS
** THE STARTING POSITION OF ITEMS
loc item_pos=1
 
**# WRITE THE DICTIONARY FILE IN TEMPFILE
tempfile wf
file open dictionary using `wf' , write replace 		

 **# MAIN DICTIONARY 'OPTIONS'/SETTINGS	
 *FIRST IDENTIFY THE LENGHTH OF IDITEMS
record_start `iditems'

#d ;
file write dictionary   
"[Dictionary]"  _newline
"Version=CSPro 7.7"  _newline
"Label=`dictionary'"  _newline
"Name=`dictionary_label'"  _newline
"RecordTypeStart= `r(record_start)'"  _newline
"RecordTypeLen=1"  _newline
"Positions=Relative"  _newline
"ZeroFill=Yes"  _newline
"SecurityOptions=3F804FBE8B125F1D4F2888E49611F0CC40D87D7DF9AF3DC8E94E672801895741AF8C05AF419305EC3E456524EA2B37F7" _newline
"DecimalChar=Yes" ;
#d cr

 **# MAIN LEVEL: 'QUESTIONNAIRE' LEVEL
#d ;
file write dictionary  
 _newline
 _newline
"[Level]"  _newline
"Label=`questionnaire_label'"  _newline
"Name=`questionnaire_name'";
#d cr

 **# IDITEMS
file write dictionary _newline _newline "[IdItems]"
 
**ADD ALL ID-ITEMS. 
foreach iditem of loc iditems {
	*VARIABLE LABEL (SHORTEN TO 255 CHAR MAX)
	loc item_lbl: variable label `iditem'
	loc item_lbl=substr("`item_lbl'",1,255)

	*VARIABLE NAME. IN UPPER CASE TO MIMIC CSPRO 
	loc item_name=upper("`iditem'")
	
	*VALUE SET NAME 
	loc item_vs_name="`item_name'"+"_VS1"
	loc item_vs_name=substr("`item_vs_name'",1,255)
	
	
	*GET LENGTH OF THAT VARIABLE AND DATATYPE
	investigate_item `iditem'
    
	*WRITE ITEM
    write_item,itemlabel("`item_lbl'") itemname("`item_name'") start(`item_pos')
	
	*AFTER EACH VARIABLE, IDENTIFY THE NEW START POSITION
	loc item_pos=`item_pos'+`r(item_length)'

	*WRITE THE VALUE SET - ONLY IF THERE ARE VALUE LABELS 
	*IDENTIFY VALUE LABEL ("" IF IT DOESNT EXIST)
    mata:  st_local("value_label",st_varvaluelabel("`iditem'"))
	   if "`value_label'"!="" write_valueset, variable("`iditem'") vslabel("`item_lbl'") vsname("`item_vs_name'")
}
   
 **# CLOSE THE FILE
file close dictionary 
**COPY TEMPFILE TO FINAL FILE 
copy `wf' "`folder'/`dictionary_label'.dcf",replace
end 





**ADD RECORD 
 **# 3.2  dcf_addrecord
********************************************************************************
capture program drop dcf_addrecord

program dcf_addrecord
*TODO: FLAG IF LABELS (E:G: QUESTIONNAIRE OR VARIABLE) ARE MORE THAN 255. CURRENTLY SILENTLY SHORTENS

syntax using/,   record(string) iditems(varlist) [required(string)] [maxrecord(numlist)]

**# PROCESS THE INPUT 

**NAMES OF INPUT
****************

 *REQUIRED 
 if !inlist("`required'","Yes","No") &  length("`required'")>0 {
	noi dis as error "Option 'required' needs to be either 'Yes' or 'No'. Case sensitive"
	ex 198
 }
 if length("`required'")==0 loc required="Yes"

 **# RECORD SHEET 
 **TODO: CSPRO REQUIREMENT The first character of a name must be a letter; the last character cannot be an underscore
 loc record_name=upper(ustrregexra("`record'","\s|\t|\n","_"))
 
 **MISC. CHECKS 
  *CHECK IF IDITEMS UNIQUELY IDENTIFY OBS. IF NOT AND 'required' NOT SPECIFIED: Give warning
 capt isid `iditems'
 if _rc==459 & "`required'"=="Yes" {
	noi display as error  "Attention. iditems specified do not uniquely identify the observations."
	noi dis as error "Consider specifiy 'required('No')'"
 }

 
**# IDENTIFY LAST START & LENGTH OF ITEM & RECORDVALUE IN EXISTING DICTIONARY FILE
 *FIRST IDENTIFY THE LENGHTH OF IDITEMS
record_start `iditems'

**# MAIN SETTINGS/LOCALS
** THE STARTING POSITION OF ITEMS: BASED ON LENGT OF IDITEMS + 1
loc item_pos=`r(record_start)'+1

*ADD 1 TO RECORD LENGTH  FROM READ_DICT
loc dict_newrecord=`r(dict_lrecord)'+1

**# OPEN THE FILE OF USER
file open dictionary using  `"`using'"', write append 		

**# APPEND RECORD SETTINGS 
 #d ;
file write dictionary _newline _newline 
"[Record]" _newline
"Label=`record'" _newline
"Name=`record_name'" _newline
"RecordTypeValue='`dict_newrecord''" _newline
"Required=`required'";
#d cr

** IF MAX VALUE SPECIFIED 
if "`maxrecord'"!="" file write dictionary _newline "MaxRecords=`maxrecord'"




**# IDENTIFY ALL VARIABLES TO ADD
 qui ds `iditems',not
 loc first=word("`r(varlist)'",1)
 loc last=word("`r(varlist)'",wordcount("`r(varlist)'"))
 *PRESENT TO USER
 noi dis as result "Variables '`first'' - '`last'' will be added to record"


**ADD ALL ID-ITEMS. 
foreach iditem of var `r(varlist)' {
	*VARIABLE LABEL (SHORTEN TO 255 CHAR MAX)
	loc item_lbl: variable label `iditem'
	loc item_lbl=substr("`item_lbl'",1,255)

	*CHANGE TO VARIABLE NAME IF NO LABEL
    if "`item_lbl'"=="" loc item_lbl="`iditem'"

	*VARIABLE NAME. IN UPPER CASE TO MIMIC CSPRO 
	loc item_name=upper("`iditem'")
	
	*VALUE SET NAME 
	loc item_vs_name="`item_name'"+"_VS1"
	loc item_vs_name=substr("`item_vs_name'",1,255)
	
	*GET LENGTH OF THAT VARIABLE AND DATATYPE
	investigate_item `iditem'
    
	*WRITE ITEM
    write_item,itemlabel("`item_lbl'") itemname("`item_name'") start(`item_pos')
	
	*AFTER EACH VARIABLE, IDENTIFY THE NEW START POSITION
	loc item_pos=`item_pos'+`r(item_length)'

	*WRITE THE VALUE SET - ONLY IF THERE ARE VALUE LABELS 
	*IDENTIFY VALUE LABEL ("" IF IT DOESNT EXIST)
    mata:  st_local("value_label",st_varvaluelabel("`iditem'"))
	   if "`value_label'"!="" write_valueset, variable("`iditem'") vslabel("`item_lbl'") vsname("`item_vs_name'")
}

 **# CLOSE THE FILE
file close dictionary 

end 
