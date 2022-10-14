*! version 0.0.2 October 2022
*! Author: Peter Brueckmann, p.brueckmann@mailbox.org

*TODO: recordlength ALWAYS 1? OR DONT SPECIFY AT ALL? MAYBE CAN BE DROPPED
*TODO: MAKE recordvalue FOR addrecord NOT NECESSARY. GET "read_dict" BACK IN
*TODO: IF INDEED THE ORDER OF IDITEMS MATTERS (E.G DISTRICT FIRST THEN INTKEY) THEN ASSERT?
*TODO: Recordtypelenght potentially only=1. Then it at least doesnt give an error. weirds
*TODO: CHECK ISSUE FLAGGED XXXX__123. I THINK BETTER TO TAKE OUT THE VALUE LABEL CHECK IN THAT COMMAND? BUT THEN NAURU GIVES MISMATCHES AGAIN. INVESTIGATE WHERE THEY COME FRM
*TODO: MAKE DROPPING OBS IF ALL MISSING OPTIONAL?
*TODO: ADJUST COMMAND SO IT DOES RecordTypeLen ONESELF? See https://www.statalist.org/forums/forum/general-stata-discussion/general/1458650-overwrite-line-in-textfile-with-file-command
*TODO: IF VALUESET MORE THAN 500 UNIQUE LABELS, RETURN ERROR OR DON'T DO
*TODO: FLAG ALL INPUTS AND IMPROVE INPUTS 
*TODO: GO THROUGH ALL MISC. TODOS IN FILE
*TODO: SECURITYOPTIONS REALLY NEEDED? OR DO RANDOM NUMBER?
*TODO: COMMANDS AFTER SETUP: CHECK IF FILE EXISTS!
*TODO: DO BOOKMARKS & BETTER DOCUMENTATION
*TODO: double: CONFIRM LENGTH OF ITEM OF TYPE double. INDEED ONLY 4D BEHIND '.'? AND BEUATIFY COMMAND & CHECK IF NEGATIVE SIGN IS COUNTED AS WELL BY CSPRO CONV.
*TODO: INVESTIGATE POSSIBILITY OF ADDING SPECIFIC VALUE SETS
*TODO: Add "Excel to CSPro" STREAM TO GENERATE THE CSPRO DATA? 

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

syntax varlist, ///
[quiet] //TO RUN QUIETLY. IF CALLED AS UTILITY WRAPPER FROM record_items_sumup 

*GET THE LENGTH OF VARIABLE: 
*IDENTIFY TYPE
mata:  st_local("item_type",st_vartype("`varlist'") )  
*IDENTIFY IF ALL MISSING. IF SO: item_length==0 & MIGHT BE DROPPED BASED ON USER CHOICE.
qui count if missing(`varlist')
if `r(N)'==`c(N)' {
	 if "`quiet'"=="" noi dis as error  "'`varlist'' contains only missings. Will not be added to dictionary and dropped from dataset."
	loc item_length=0
}
else if `r(N)'!=`c(N)' {


*FOR STRING
if regexm("`item_type'","^str")==1 {
	*FOR NOW, IDENTIFY VALUE  BASED ON STRING TYPE. HOWEVER, IF DATA  MANIUPLATED AND TYPE NOT ADJUSTED,
	*CAN LEAD TO DIFFERENT RESULT. SO TEMP COMPRESS THE VARIABLE AND GET NEW ITEM TYPE
	preserve 
	qui compress `varlist'
	mata:  st_local("item_type",st_vartype("`varlist'") )  
	restore 
	*NOW GET THE VALUE AGAIN
	loc item_length=substr("`item_type'",4,.)
	*IF 0 (NO VALUE ENTERED) CHANGE TO 1 
	if `item_length'==0 loc item_length=1
	*IF ABOVE 255, SET TO 255 
	if `item_length'>255 | "`item_type'"=="strL" loc item_length=255
}
*FOR NUMERIC. 
else if regexm("`item_type'","^str")==0 {
	
	*BY DEFAULT WE SIMPLY TAKE IT FROM UNDERLYING DATA (GET MIN AND MAX AS CSPRO READS CHARACTERS)
	 mata: st_local("max_value", strofreal(strlen(strofreal(max(st_data(.,"`varlist'")),"%16.0g"))))
	 mata: st_local("min_value", strofreal(strlen(strofreal(min(st_data(.,"`varlist'")),"%16.0g"))))
	 loc item_length=cond(`max_value'>=`min_value', `max_value',`min_value')

	*TODO: CLEAN THIS UP AND REMOVE AS OUTDATED. SHOULDN'T BE USED AS UNDERLYING DATA IS USED FOR EXCEL!!
	*IF VALUE LABEL EXIST: NEED TO  GET MAXIMUM FROM VALUE LABEL LIST. BECAUSE IF UNDERLYING DATA DOES NOT CONTAIN THE MAX VALUE (BUT LABEL EXISTS FOR IT) IT WOULD CAUSE ISSUES
	*SO COMPARE IT 
	/*
    mata:  st_local("value_label",st_varvaluelabel("`varlist'"))
	if "`value_label'"!="" {
			//GET THE VALUE LABELS 
			mata: st_vlload(st_varvaluelabel("`varlist'"), values=., text=.)
			//IDENTIFY LENGTH OF MIN/MAX IN VALUES OF VALUE LABEL. WHATEVER IS "LONGER" IN STRING TERMS WILL BE ITEM LENGT
			mata: st_local("max_value",strofreal(strlen(strofreal(max(values),"%16.0g"))))
			mata: st_local("min_value",strofreal(strlen(strofreal(min(values),"%16.0g"))))
			loc label_max=cond(`max_value'>=`min_value', `max_value',`min_value')
			*NOW UPDATE item_length IF IT IS LARGER THAN UNDERLYING DATA
			loc item_length=cond(`label_max'>=`item_length', `label_max',`item_length')
	}
*/
	*IF DOUBLE/FLOAT DO SPECIAL
	if inlist("`item_type'","double","float") & "`value_label'"=="" {
		noi dis as result "Attention. Variable `varlist' of type '`item_type''. Might lead to conversion and/or precision issues"
		*BRUTE FORCE TO STRING. COUNT THE ALPHA CHARACTERS BASED ON FORMULA: 
		*ALL CHARS OF INTEGER PART + DELIMITER + FRACTIONAL PART UP TO 6!
		*UNFORTUNATELY DO DIRTY (FOR NOW).
		tempvar helpvar helpvar2 helpvar3
		qui tostring `varlist', g(`helpvar') force
		*KEEP ONLY 6 DIGITS AFTER THE DOT
		qui replace `helpvar'=substr(`helpvar',1,strpos(`helpvar',".")+6)
		qui egen `helpvar2'=max(length(`helpvar'))
		loc item_length=`helpvar2'[1]
		*CHECK IF ANY INTEGER. IF NONE, ADD 1 TO LENGTH AS CSPRO READS LEADING 0
		drop `helpvar' `helpvar2'
		qui g `helpvar3'=int(`varlist')
		qui sum `helpvar3'
		if `r(min)'==0 & `r(max)'==0 loc item_length=`item_length'+1
	}

	*IF LARGER THAN 15, SET TO 15
	if `item_length'>15 noi dis as result "Attention. Numeric variable `varlist' has length>15. Set to 15 to comply with CSPro"
	if `item_length'>15 loc item_length=15

		}
	}


*RETURN THE ITEM LENGTH
return local item_length= `item_length'
*AS WELL AS DATATYPE 
return local csprodata_type=cond(regexm("`item_type'","^str")==1,"Alpha","Numeric")
*AND THE UNDERLYING VARIABLE TYPE
return local variable_type="`item_type'"

end 


 **# GET THE FULL LENGTH OF ALL ITEMS SPECIFIED (USUALLY THE IDITEMS). SIMPLE SUMS UP THE LENGTH
capture program drop record_items_sumup

program record_items_sumup, rclass

syntax varlist,[quiet]

*GET THE LENGTH OF VARIABLE: 
loc record_items_sumup=1
foreach var of var `varlist' {
	if "`quiet'"=="" investigate_item `var'
	if "`quiet'"!="" investigate_item `var',quiet
	loc record_items_sumup=`record_items_sumup'+`r(item_length)'
}

*RETURN THE ITEM LENGTH
return local record_items_sumup= `record_items_sumup'
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
**IF DOUBLE ADD DECIMAL + DECIMAL=6. SEEMS TO BE DEFAULT BY CONVERSION
if "`r(variable_type)'"=="double" file write dictionary _newline "DecimalChar=Yes"
*if "`r(variable_type)'"=="double" file write dictionary _newline "Decimal=6"


end 



 **# WRITE THE "[ValueSet]" PART OF DICTIONARY. 
capture program drop write_valueset

program write_valueset

syntax , variable(string) vslabel(string) vsname(string) itemlength(numlist)
qui {
	   	**## FIRST WRITE THE HEADER
		#d ;
		file write dictionary  
		 _newline
		 _newline
		"[ValueSet]" _newline
		"Label=`vslabel'" _newline
		"Name=`vsname'" ;
		#d cr

		**## NOW WRITE THE VALUE SET

		**### FIRST GET THE ACTUAL VALUES IN UNDERLYING DATA 
			qui levelsof `variable',loc(levels)
			*TEMP: COUNT DISTINCT VALUES. AS STATA <15 DOESNT RETURN r(r) AND DONT WANT TO USE distinct.ado USER PROGRAMM FOR NOW
			loc distinct_count=0
			foreach levl of loc levels {
				loc distinct_count=`distinct_count'+1
			}


			*GET TEMPORARY HELPFILE FOR MERGIN IT BELOW 
			preserve
			clear 
			set obs `distinct_count'
			g code=""
			loc helpcount=1
			foreach lev of loc levels {
				qui replace code="`lev'" in `helpcount'
				loc ++helpcount
			}

			tempfile all_values
			save `all_values'
			restore


		**### NOW GET THE VALUE LABELS 
		mata: st_vlload(st_varvaluelabel("`variable'"), values=., text=.)
		//STORE THE VALUE LABELS IN A MATRIX
		mata:VL_MATRIX= strofreal(values,"%16.0g"),text
		//TEMPORARILY CREATE A NEW DATASET THAT STORES THE VALUE LABELS
		preserve
		 clear
		 **GET IT INTO DATA FRAME
		 getmata(code label)=VL_MATRIX
		 **#### COMPARE VALUE LABELS AGAINST UNDERLYING DATA
		 *CHECK IF THE VALUES ACTUALLY EXIST IN UNDERLIYING DATA:SIMPYL MERGE ALL UNIQUE VALUES IN
		 qui merge  1:1 code using `all_values'
		 *IF ONLY MASTER (merge==1): THIS VALUE EXISTED ONLY IN VALUE LABEL. 
		 *WILL BE DROPPED IN CASE THE ITEM LENGTH IS SMALLER THAN THE LENGTH OF THAT VALUE 
		 g length_str=length(code)
		 **TODO: HIGHLIGHT TO USER WHICH VALUE LABEL NOT USED? 
		 drop if _merge==1 & length_str>`itemlength'
		 *IF ONLY USING: DIDN'T HAVE VALUE LABEL. (E.g. Less than one year for age Questions)
		 *ADD VALUE LABEL = CODE
		 replace label=code if _merge==2
		 drop _merge length_str
		 *END OF CHECK
		 ****
		 
		 *CREATE VARIABLE THAT STORES THE CSPROP VALUE PATTERN
		 g cspro_value="Value="+code+";"+label

		 *GET IT IN PROPER ORDER, THAT IS SORT BY INTEGER NOT BY CHARACTER
		 destring code, replace
		 sort code
		
		 *GO THROUGH EACH ROW AND WRITE
		 *TODO: POSSIBLE TO DO IN ONELINER W/OUT LOOP?´
		 sort code
		 forvalues row=1/`c(N)' {
			file write dictionary _newline (cspro_value[`row'])
		 }
		restore
}
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
		  recordlength(numlist) ///
		  [FOLDER(string)]
qui{
**# PROCESS THE INPUT 
if `recordlength'<=0 {
	noi dis as error "recordlength(numlist) must not be negative. Please check."
	ex 198
}
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
 

**IDITEMS ARE TRULY IDs? 
isid `iditems'

**# MAIN SETTINGS/LOCALS
** THE STARTING POSITION OF ITEMS. STARTS AT 2 AS RECORDTYPESTART is 1
loc item_pos=2
 
**# WRITE THE DICTIONARY FILE IN TEMPFILE
tempfile wf
file open dictionary using `wf' , write replace 		
 **# MAIN DICTIONARY 'OPTIONS'/SETTINGS	

 *FIRST IDENTIFY THE LENGHTH OF IDITEMS
*record_items_sumup `iditems'  

#d ;
file write dictionary   
"[Dictionary]"  _newline
"Version=CSPro 7.7"  _newline
"Label=`dictionary'"  _newline
"Name=`dictionary_label'"  _newline
"RecordTypeStart=1"  _newline
"RecordTypeLen=`recordlength'"  _newline
"Positions=Relative"  _newline
"ZeroFill=Yes"  _newline
"DecimalChar=Yes" _newline
"SecurityOptions=3F804FBE8B125F1D4F2888E49611F0CC40D87D7DF9AF3DC8E94E672801895741AF8C05AF419305EC3E456524EA2B37F7" ;
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
	*CHECK IF ANY MISSING. SHOULDNT BE THE CASE 
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
    if `r(item_length)'==0 {
		noi dis as error "`iditem' is of length 0. Iditems need to be of length 1+"
		ex 198
	}
	*WRITE ITEM
    write_item,itemlabel("`item_lbl'") itemname("`item_name'") start(`item_pos')

	*AFTER EACH VARIABLE, IDENTIFY THE NEW START POSITION
	loc item_pos=`item_pos'+`r(item_length)'

	*WRITE THE VALUE SET - ONLY IF THERE ARE VALUE LABELS 
	*IDENTIFY VALUE LABEL ("" IF IT DOESNT EXIST)
    mata:  st_local("value_label",st_varvaluelabel("`iditem'"))
	   if "`value_label'"!="" write_valueset, variable("`iditem'") vslabel("`item_lbl'") vsname("`item_vs_name'") itemlength(`r(item_length)')
	   		

}

   
 **# CLOSE THE FILE
file close dictionary 
**COPY TEMPFILE TO FINAL FILE 
copy `wf' "`folder'/`dictionary_label'.dcf",replace

}
end 





**ADD RECORD 
 **# 3.2  dcf_addrecord
********************************************************************************
capture program drop dcf_addrecord

program dcf_addrecord
*TODO: FLAG IF LABELS (E:G: QUESTIONNAIRE OR VARIABLE) ARE MORE THAN 255. CURRENTLY SILENTLY SHORTENS

syntax using/,   record(string) iditems(varlist) recordvalue(integer) [required(string)]

qui {

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

 

 **# OPEN THE FILE OF USER
file open dictionary using  `"`using'"', write append 		

**# MAIN RECORD SETTINGS/LOCALS
**## IDENTIFY MAX RECORDS (NUMBER OF MAX OBSERVATIONS PER iditems TO BE EXPECTED)
tempvar max 
bys `iditems': g `max'=_N
qui sum `max'
loc max_record=`r(max)'
drop `max'


**## IDENTIFY LENGTH OF RECORD (RecordLen)
**WHICH IS THE SUM OF ALL ITEMS TO BE ADDED + THE LENGTH OF IDITEMS (WHICH IN TURN IS THE START OF THE ITEMS IN RECORD. THATS WHY WE RUN IN NEXT LINES record_items_sumup SEPERATELY)!

**### IDENTIFY START OF ITEMS WITHIN RECORD WHICH IS LENGTH OF IDITEM  + 1 (AS Record Type Value=1) NO VALUE ADDED. 
**BASED ON THIS INITIAL VALUE, THE FIRST ITEM WILL USE TIS VALUE, 
*THE OTHERS ARE start+LENGHT OF PREVIOUS ITEMS. 
record_items_sumup `iditems'
loc item_pos=`r(record_items_sumup)' +1

**NOW THE ITEMS TO BE ADDED (ALL BUT iditems)
qui ds `iditems',not
record_items_sumup `r(varlist)',quiet
*Final length: items + iditems 
loc recordlen=`r(record_items_sumup)' + `item_pos'



**## APPEND RECORD SETTINGS 
 #d ;
file write dictionary _newline _newline 
"[Record]" _newline
"Label=`record'" _newline
"Name=`record_name'" _newline
"RecordTypeValue='`recordvalue''" _newline
"RecordLen='`recordlen''" _newline
"MaxRecords=`max_record'"  _newline
"Required=`required'";
#d cr



**# IDENTIFY ALL VARIABLES TO ADD
 qui ds `iditems',not
 loc first=word("`r(varlist)'",1)
 loc last=word("`r(varlist)'",wordcount("`r(varlist)'"))
 *PRESENT TO USER
 noi dis as result "Variables '`first'' - '`last'' will be added to record"


**ADD ALL ITEMS. 
foreach itemvar of var `r(varlist)' {
	*VARIABLE LABEL (SHORTEN TO 255 CHAR MAX)
	loc item_lbl: variable label `itemvar'
	loc item_lbl=substr("`item_lbl'",1,255)

	*CHANGE TO VARIABLE NAME IF NO LABEL
    if "`item_lbl'"=="" loc item_lbl="`itemvar'"

	*VARIABLE NAME. IN UPPER CASE TO MIMIC CSPRO 
	loc item_name=upper("`itemvar'")
	
	*VALUE SET NAME 
	loc item_vs_name="`item_name'"+"_VS1"
	loc item_vs_name=substr("`item_vs_name'",1,255)
	
	*GET LENGTH OF THAT VARIABLE AND DATATYPE
	investigate_item `itemvar' 


	*WRITE ITEM (ONLY IF NOT ALL MISSING)
    if `r(item_length)'!=0 {
       	write_item,itemlabel("`item_lbl'") itemname("`item_name'") start(`item_pos')
	}
	if `r(item_length)'==0{
		drop `itemvar'
		continue
	}
	
	*AFTER EACH VARIABLE, IDENTIFY THE NEW START POSITION
	loc item_pos=`item_pos'+`r(item_length)'

	*WRITE THE VALUE SET - ONLY IF THERE ARE VALUE LABELS & ITEM LENGTH>0 (Latter avoids if .c etc. are labelled)
	*IDENTIFY VALUE LABEL ("" IF IT DOESNT EXIST)
    mata:  st_local("value_label",st_varvaluelabel("`itemvar'"))
	   if "`value_label'"!="" & `r(item_length)'>0 write_valueset, variable("`itemvar'") vslabel("`item_lbl'") vsname("`item_vs_name'") itemlength(`r(item_length)')
}

 **# CLOSE THE FILE
file close dictionary 
}

end 
