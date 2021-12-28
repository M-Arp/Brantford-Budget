rename Q1 Satisfaction
rename Q2 Value
rename Q3A SA_Police
rename Q3B SA_Fire
rename Q3C SA_PW
rename Q3D SA_Housing
rename Q3E SA_Transit
rename Q3F SA_Parks
rename Q3G SA_Paramedics
rename Q3H SA_Social
rename Q3I SA_Garbage
rename Q3J SA_PHU
rename Q3K SA_Facilities
rename Q3L SA_Noble
rename Q3M SA_Children
rename Q3N SA_Tourism
rename Q3O SA_Econ
rename Q3P SA_Storm

rename Q4 Infrastructure
rename Q5 Maintain
rename Q6A Fee_Parks
rename Q6B Fee_Parking
rename Q6C Fee_Street
rename Q6D Fee_Permits
rename Q6F Fee_Transit
rename Q6G Fee_Roads

rename Q7A Opt_Airport
rename Q7B	Opt_Library
rename Q7C	Opt_Facilities
rename Q7D	Opt_Parks
rename Q7E	Opt_Health
rename Q7F	Opt_Social
rename Q7G	Opt_Downtown
rename Q7H	Opt_Econ
rename Q7I	Opt_Horticulture	
rename Q7J Opt_Golf
rename Q7K	Opt_Sports
rename Q7L	Opt_Snow
rename Q7M	Opt_Special

rename Q8 Years
rename Q9 Postal
rename Q10 DOB
rename Q11 EDU
rename Q12 Ethnicity
rename Q13 gender

gen age=2011- CPS11_78
recode age (-7988=.)

recode gender (5=2)

gen wt_gender=.
replace wt_gender=.82 if gender==1
replace wt_gender=1.2 if gender==2

gen Age_Bin4 =.
replace Age_Bin4=1 if age<35
replace Age_Bin4=2 if age>34 & age<50
replace Age_Bin4=3 if age>49 & age<65
replace Age_Bin4=4 if age>64

gen agewt=.
replace agewt=0.14 if Age_Bin4==1
replace agewt=0.47 if Age_Bin4==2
replace agewt=1.07 if Age_Bin4==3
replace agewt=2.44 if Age_Bin4==4
rename agewt wt_age

gen white=.
replace white=1 if ethnicity==1
replace white=0 if ethnicity>1
replace white=. if ethnicity==.

gen Edu_4 =.
replace Edu_4=1 if edu<2
tab edu
drop Edu_4
gen college=.
replace college=1 if edu<4
replace college=0 if edu<4
replace college=1 if edu>3
replace college=. if edu==.

gen years_Bin4=.
replace years_Bin4=1 if years<7
replace years_Bin4=2 if years>6 & years<20
replace years_Bin4=3 if years>19 & years<40
replace years_Bin4=4 if years>39
replace years_Bin4=. if years==.

svyset [pweight=wt_age]

svy: reg satisfaction white college gender Age_Bin4 years_Bin4
svy: reg value white college gender Age_Bin4 years_Bin4

svy: reg sa_police white college gender Age_Bin4 years_Bin4
svy: reg  sa_fire white college Age_Bin4 gender years_Bin4
svy: reg  sa_pw white college Age_Bin4 gender years_Bin4
svy: reg  sa_housing white college Age_Bin4 gender years_Bin4
svy: reg  sa_transit white college Age_Bin4 gender years_Bin4
svy: reg  sa_parks white college Age_Bin4 gender years_Bin4
svy: reg   sa_paramedics white college Age_Bin4 gender years_Bin4
svy: reg  sa_social white college Age_Bin4 gender years_Bin4
svy: reg  sa_garbage white college Age_Bin4 gender years_Bin4
svy: reg   sa_phu white college Age_Bin4 gender years_Bin4
svy: reg   sa_facilities white college Age_Bin4 gender years_Bin4
svy: reg   sa_noble white college Age_Bin4 gender years_Bin4
svy: reg  sa_children white college Age_Bin4 gender years_Bin4
svy: reg   sa_tourism white college Age_Bin4 gender years_Bin4
svy: reg  sa_econ white college Age_Bin4 gender years_Bin4
svy: reg  sa_storm white college Age_Bin4 gender years_Bin4

svy: reg  infrastructure white college Age_Bin4 gender years_Bin4
svy: reg  maintain white college Age_Bin4 gender years_Bin4

svy: reg   fee_parks white college Age_Bin4 gender years_Bin4
svy: reg   fee_parking white college Age_Bin4 gender years_Bin4
svy: reg   fee_street white college Age_Bin4 gender years_Bin4
svy: reg   fee_permits white college Age_Bin4 gender years_Bin4
svy: reg  fee_transit white college Age_Bin4 gender years_Bin4
svy: reg  fee_roads white college Age_Bin4 gender years_Bin4

svy: reg  opt_airport white college Age_Bin4 gender years_Bin4
svy: reg  opt_library white college Age_Bin4 gender years_Bin4
svy: reg  opt_facilities white college Age_Bin4 gender years_Bin4
svy: reg  opt_parks white college Age_Bin4 gender years_Bin4
svy: reg  opt_health white college Age_Bin4 gender years_Bin4
svy: reg  opt_social white college Age_Bin4 gender years_Bin4
svy: reg   opt_downtown white college Age_Bin4 gender years_Bin4
svy: reg   opt_econ white college Age_Bin4 gender years_Bin4
svy: reg  opt_horticulture white college Age_Bin4 gender years_Bin4
svy: reg  opt_golf white college Age_Bin4 gender years_Bin4
svy: reg  opt_sports white college Age_Bin4 gender years_Bin4
svy: reg  opt_snow white college Age_Bin4 gender years_Bin4
svy: reg  opt_special white college Age_Bin4 gender years_Bin4

