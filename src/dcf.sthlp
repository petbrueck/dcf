{smcl}
{cmd:help get_dcf}
{hline}

{title:Title}

{p 5 15}
{cmd:get_dcf} {hline 2} does XXXXX


{title:Syntax}

{p 8 17 2}
{cmd:get_dcf}
{cmd:,} {opt dictionary(string)} {opt questionnaire(string)}  {opt iditems(varlist)}


{synoptset 21 tabbed}{...}
{synopthdr:Required }
{synoptline}
{synopt:{opt dictionary(string)}} Name of dictionary to be created {p_end}
{synopt:{opt questionnaire(string)}}Name of instrument/questionnaire. Determines the main level in dictionary {p_end}
{synopt:{opt iditems(varlist)}} List of variables that will serve as [Identification Items](https://www.csprousers.org/help/CSPro/identification_items.html) that uniquely identify the observations {p_end}
{synoptline}