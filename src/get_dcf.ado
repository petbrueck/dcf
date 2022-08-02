*! version 0.0.1 August 2022
*! Author: Peter Brueckmann, p.brueckmann@mailbox.org



**# UTILITY PROGRAMS
********************************************************************************

 **# INVESTIGATE A VARIABLE AND RETURN LENGTH & DATA TYPE 
capture program drop investigate_item

program investigate_item, rclass

syntax varlist

*GET THE LENGT OF VARIABLE: 
*IDENTIFY TYPE
mata:  st_local("item_type",st_vartype("`varlist'") )  
*FOR STRING
if regexm("`item_type'","^str")==1  mata: st_local("item_length", strofreal(max(strlen(st_sdata(.,"`varlist'")))))
*FOR NUMERIC
else if regexm("`item_type'","^str")==0 mata: st_local("item_length", strofreal(strlen(strofreal(max(st_data(.,"`varlist'")),"%20.0f"))))

*RETURN THE ITEM LENGTH
return local item_length= `item_length'
*AS WELL AS DATATYPE 
return local data_type=cond(regexm("`item_type'","^str")==1,"Alpha","Numeric")

end 


 **# WRITE THE "[ITEM]" PART OF DICTIONARY.
capture program drop write_item

program write_item

syntax , itemlabel(string) itemname(string) start(integer)
	 #d ;
	file write dictionary  
	 _newline
	 _newline
	"[Item]"  _newline
	"Label=`itemlabel'" _newline
	"Name=`itemname'" _newline
	"Start=`start'" _newline
	"Len= `r(item_length)'" _newline
	"DataType=`r(data_type)'" ;
	#d cr

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
		mata:VL_MATRIX= strofreal(values,"%20.0f"),text
		//TEMPORARILY CREATE A NEW DATASET THAT STORES THE VALUE LABELS
		preserve
		 clear
		 **GET IT INTO DATA FRAME
		 getmata(code label)=VL_MATRIX
		 
		 *CREATE VARIABLE THAT STORES THE CSPROP VALUE PATTERN
		 g cspro_value="Value="+code+";"+label
		 *GO THROUGH EACH ROW 
		 forvalues row=1/`c(N)' {
			file write dictionary _newline (cspro_value[`row'])
		 }
		restore

end 



********************************************************************************
**# THE MAIN PROGRAM
********************************************************************************
capture program drop get_dcf

program get_dcf
*TODO: CHANGE FROM RECORDS TO SHEETS
*TODO: FLAG IF LABELS (E:G: QUESTIONNAIRE OR VARIABLE) ARE MORE THAN 255. CURRENTLY SILENTLY SHORTENS

syntax ,  dictionary(string) questionnaire(string) iditems(varlist) records(string)

**# PROCESS THE INPUT 

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
 **# RECORD SHEET 
 loc records_name=upper(ustrregexra("`records'","\s|\t|\n","_"))
 
 
**# MAIN SETTINGS/LOCALS
** THE STARTING POSITION OF ITEMS
loc item_pos=1
** THE STARTING RECORDTYPE VALUE 
loc record_start_value=1
 
**# WRITE THE DICTIONARY FILE
*TODO: WRITE TO TEMP AND ONLY IF SUCCESSFULLY COPY OVER
file open dictionary using  "`c(pwd)'/dev_dict.dcf", write replace 		

 **# MAIN DICTIONARY 'OPTIONS'/SETTINGS	
*TODO: IDENTIFY RecordTypeStart MEANING
#d ;
file write dictionary   
"[Dictionary]"  _newline
"Version=CSPro 7.6"  _newline
"Label=`dictionary'"  _newline
"Name=`dictionary_label'"  _newline
"RecordTypeStart=45"  _newline
"RecordTypeLen=1 "  _newline
"Positions=Relative"  _newline
"ZeroFill=Yes"  _newline
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
 

 **# RECORD 
 *TODO: CONSIDER TO MOVE THIS OUT OF ONE COMMAND AND HAVE THEM RATHER ADD SHEET BY SHEET? SO CURRENT COMMAND IS JUST "BASE DICTIONARY"
 *TODO: CORRECT RECORD LENGTH :(
 #d ;
file write dictionary _newline _newline 
"[Record]" _newline
"Label=`records'" _newline
"Name=`records_name'" _newline
"RecordTypeValue='`record_start_value''" _newline
"RecordLen=683" ; 
#d cr

 
  
  
 **# CLOSE THE FILE
file close dictionary 

end 
