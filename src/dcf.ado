*! version 0.0.1 August 2022
*! Author: Peter Brueckmann, p.brueckmann@mailbox.org

*TODO: DO BOOKMARKS & BETTER DOCUMENTATION
*TODO: WRITE ITEM, ONLY WRITE LINES THAT ARE NECESSARY (E.G: DATATYPE)
*TODO: IF VALUESET MORE THAN 500 UNIQUE LABELS, RETURN ERROR OR DON'T DO

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

 **# INVESTIGATE A VARIABLE AND RETURN LENGTH & DATA TYPE 
capture program drop investigate_item

program investigate_item, rclass

syntax varlist

*GET THE LENGT OF VARIABLE: 
*IDENTIFY TYPE
mata:  st_local("item_type",st_vartype("`varlist'") )  
*FOR STRING
if regexm("`item_type'","^str")==1  mata: st_local("item_length", strofreal(max(strlen(st_sdata(.,"`varlist'")))))
*FOR NUMERIC. IF LARGER THAN 15, SET TO 15
else if regexm("`item_type'","^str")==0 {
	mata: st_local("item_length", strofreal(strlen(strofreal(max(st_data(.,"`varlist'")),"%20.0g"))))
	if `item_length'>15 loc item_length=15
}

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
	"Len=`r(item_length)'" _newline
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
		mata:VL_MATRIX= strofreal(values,"%20.0g"),text
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


 **# IDENTIFY THE LAST START/LENGT OF ITEM IN EXISTING DICTIONAROY
 **  UTIL FUNCTION FOR dcf_addrecord
capture program drop read_dict

        program read_dict,rclass
                syntax using/
				*READ IN TEMPFILE SO IT CLOSES IF BREAKS
                tempname temp_read
				*START READING LINE 0
                local linenum = 0
				*OPEN AND READ FIRST LINE
                file open `temp_read' using `using', read
                file read `temp_read' line
				*GO THROUGH UNTIL END OF LINE
                while r(eof)==0 {
                        local linenum = `linenum' + 1
						*IDENTIFY THE LAST START POSITION OF ITEM
						if regexm(`"`macval(line)'"',"^Start=")==1  loc lstart_pos=ustrregexra(`"`macval(line)'"',"^Start=","")
						*IDENTIFY THE LAST ITEM LENGTH
						if regexm(`"`macval(line)'"',"^Len=")==1  loc l_length=ustrregexra(`"`macval(line)'"',"^Len=","")
                        file read `temp_read' line
                }
                file close `temp_read'
		noi dis as result "Last Start of Item: `lstart_pos'"
		noi dis as result "Last Length of Item: `l_length'"
		*RETURN THE ITEM LENGTH
		return local dict_litem_start= `lstart_pos'
		*AS WELL AS DATATYPE 
		return local dict_litem_length=`l_length'
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
		  iditems(varlist)

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
 
**# MAIN SETTINGS/LOCALS
** THE STARTING POSITION OF ITEMS
loc item_pos=1
 
**# WRITE THE DICTIONARY FILE
*TODO: WRITE TO TEMP AND ONLY IF SUCCESSFULLY COPY OVER
file open dictionary using  "`c(pwd)'/`dictionary_label'.dcf", write replace 		

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
   
 **# CLOSE THE FILE
file close dictionary 

end 





**ADD RECORD 
 **# 3.2  dcf_addrecord
********************************************************************************
capture program drop dcf_addrecord

program dcf_addrecord
*TODO: FLAG IF LABELS (E:G: QUESTIONNAIRE OR VARIABLE) ARE MORE THAN 255. CURRENTLY SILENTLY SHORTENS

syntax using/,   record(string) iditems(varlist)

**# PROCESS THE INPUT 

**NAMES OF INPUT
****************
**TODO: CSPRO REQUIREMENT The first character of a name must be a letter; the last character cannot be an underscore
 **# RECORD SHEET 
 loc record_name=upper(ustrregexra("`record'","\s|\t|\n","_"))
 
 
 
**# IDENTIFY LAST START & LENGTH OF ITEM & RECORDVALUE IN EXISTING DICTIONARY FILE
read_dict using `"`using'"'

**# MAIN SETTINGS/LOCALS
** THE STARTING POSITION OF ITEMS: BASED ON LAST ITEM IN FILE: START + LENGTH + 1
loc item_pos=`r(dict_litem_start)' +  `r(dict_litem_length)'+1

*ADD 1 TO RECORD LENGTH  FROM READ_DICT
loc dict_newrecord=`r(dict_lrecord)'+1

**# OPEN THE FILE OF USER
*TODO: WRITE TO TEMP AND ONLY IF SUCCESSFULLY COPY OVER
file open dictionary using  `"`using'"', write append 		

**# APPEND RECORD SETTINGS 
*TODO: Validate if RecordLen necessary?
 #d ;
file write dictionary _newline _newline 
"[Record]" _newline
"Label=`record'" _newline
"Name=`record_name'" _newline
"RecordTypeValue='`dict_newrecord''" ;
#d cr



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
