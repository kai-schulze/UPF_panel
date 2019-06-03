	****************************************************************************
	*** Kai Schulze
	*** 15/03/2019
	*** Project: Associations between sales of ultra-processed food products and
	*** 	     prevalence of adiposity and diabetes mellitus: a panel analysis 
    *** 		 of 76 countries between 2001-2016
	*** Step: Estimation of Associations between UPF and Adiposity 
	****************************************************************************
	
	*** Questions regarding code: please feel free to contact me at ks727@medschl.cam.ac.uk 
	
	/* TABLE OF CONTENTS  
	
	I.) ALL COUNTRIES - 					LINE 20 ff.
	II.) LOW-TO-MIDDLE INCOME COUNTRIES - 	LINE 349 ff.
	III.) HIGH-INCOME COUNTRIES - 			LINE 678 ff.
	
	*/
	
	*****************************************
	*||     I.)	   ALL    	COUNTRIES 	  *||
	*****************************************
	
	clear all
	version 15
	set more off, perm
	set matsize 800
		
	cd "C:\Users\..."
	use panel
	
	*|| Create Standardized Values 
	
	foreach var of varlist unpf - asdpm {
	egen z`var' = std(`var')
	drop `var'
	}
	renpfix z
	
	*|| Set outcomes and exposures 
	
	global depvars_male "bmim owm obem " 
	global depvars_female "bmif owf obef "
	global depvars_boys "bmib owb obeb " 
	global depvars_girls "bmig owg obeg"
	
	global exposure1 "upfssb"
	global exposure2 "upfssb_av2"
	global exposure3 "upfssb_av3"
	
	*|| Set the models for 2-level analysis 
	*Adult males 
	global covsadu_ma1 ""
	global covsadu_ma2 "unpf ipa_ma gdppc"
	global covsadu_ma3 "unpf ipa_ma gdppc alcohollpc totalkcalpc"
	global covsadu_ma4 "unpf ipa_ma gdppc alcohollpc totalkcalpc prop_urban"

	*Adult females
	global covsadu_fe1 ""
	global covsadu_fe2 "unpf ipa_fe gdppc"
	global covsadu_fe3 "unpf ipa_fe gdppc alcohollpc totalkcalpc"
	global covsadu_fe4 "unpf ipa_fe gdppc alcohollpc totalkcalpc prop_urban"
	
	*Children and adolescents boys
	global covsca_ma1 ""
	global covsca_ma2 "unpf ipa_ma gdppc"
	global covsca_ma3 "unpf ipa_ma gdppc totalkcalpc"
	global covsca_ma4 "unpf ipa_ma gdppc totalkcalpc prop_urban"

	*Children and adolescents girls
	global covsca_fe1 ""
	global covsca_fe2 "unpf ipa_fe gdppc"
	global covsca_fe3 "unpf ipa_fe gdppc totalkcalpc"
	global covsca_fe4 "unpf ipa_fe gdppc totalkcalpc prop_urban"
	
	*|| Set the models for Fixed-Effects (without ipa)
	*Adult males 
	global covsadu_ma1_fe ""
	global covsadu_ma2_fe "unpf gdppc"
	global covsadu_ma3_fe "unpf gdppc alcohollpc totalkcalpc"
	global covsadu_ma4_fe "unpf gdppc alcohollpc totalkcalpc prop_urban"

	*Adult females
	global covsadu_fe1_fe ""
	global covsadu_fe2_fe "unpf gdppc alcohollpc"
	global covsadu_fe3_fe "unpf gdppc alcohollpc totalkcalpc"
	global covsadu_fe4_fe "unpf gdppc alcohollpc totalkcalpc prop_urban"
	
	*Children and adolescents boys
	global covsca_ma1_fe ""
	global covsca_ma2_fe "unpf gdppc"
	global covsca_ma3_fe "unpf gdppc totalkcalpc"
	global covsca_ma4_fe "unpf gdppc totalkcalpc prop_urban"

	*Children and adolescents girls
	global covsca_fe1_fe ""
	global covsca_fe2_fe "unpf gdppc"
	global covsca_fe3_fe "unpf gdppc totalkcalpc"
	global covsca_fe4_fe "unpf gdppc totalkcalpc prop_urban"

	save panel_analysis.dta, replace
	
	*******************************************************************************	
	*|| TWO-LEVEL MIXED MODEL WITH VARYING INTERCEPTS AND SLOPES & Clustered SE ||*
	*******************************************************************************
	use panel_analysis
		
	*|| Define program to save estimation results in matrices
	cap program drop matdef
	program matdef
			mat a = r(table)
			
			mat p = a[4,1]
			mat b = a[1,1]
			mat se = a[2,1]
			mat lb = a[5,1]
			mat ub = a[6,1]
		
			mat pval=(pval \ p)
			mat bval=(bval \ b)
			mat seval=(seval \ se)
			mat lbval=(lbval \ lb)
			mat ubval=(ubval \ ub)
	end
		
	mat pval=[0]
	mat bval=[0]
	mat seval=[0]
	mat lbval=[0]	
	mat ubval=[0]

	*|| Adult Females 
	foreach depvar of global depvars_female {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsadu_fe1 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_fe2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_fe3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_fe4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}
	
	*|| Adult Males
	foreach depvar of global depvars_male {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsadu_ma1 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_ma2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_ma3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_ma4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}

	*|| Children and adolescent girls
	foreach depvar of global depvars_girls {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsca_fe1 || country: `exp', nolog cov(un) vce(cluster country) 
			matdef
		mixed `depvar' `exp' $covsca_fe2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_fe3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_fe4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}
	
	*|| Children and adolescent boys
	foreach depvar of global depvars_boys {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsca_ma1 || country: `exp', nolog cov(un) vce(cluster country) 
			matdef
		mixed `depvar' `exp' $covsca_ma2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_ma3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_ma4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}

	
	**********************************************
	*|| TRANSFORM MATRICES AND OUTPUT TO EXCEL ||*
	**********************************************
	*|| Create matrix with all estimates, transform to variables, save in Excel Workbook. 

	mat bvall = [bval[2..5,1],bval[6..9,1],bval[10..13,1],bval[14..17,1],bval[18..21,1],bval[22..25,1],bval[26..29,1],bval[30..33,1],bval[34..37,1] \ ///
				bval[38..41,1], bval[42..45,1],bval[46..49,1],bval[50..53,1],bval[54..57,1],bval[58..61,1],bval[62..65,1],bval[66..69,1],bval[70..73,1] \ ///
				bval[74..77,1],bval[78..81,1], bval[82..85,1],bval[86..89,1],bval[90..93,1],bval[94..97,1],bval[98..101,1],bval[102..105,1],bval[106..109,1] \ ///
				bval[110..113,1],bval[114..117,1],bval[118..121,1],bval[122..125,1],bval[126..129,1],bval[130..133,1],bval[134..137,1],bval[138..141,1],bval[142..145,1]]
	
	mat pvall = [pval[2..5,1],pval[6..9,1],pval[10..13,1],pval[14..17,1],pval[18..21,1],pval[22..25,1],pval[26..29,1],pval[30..33,1],pval[34..37,1] \ ///
				pval[38..41,1],pval[42..45,1],pval[46..49,1],pval[50..53,1],pval[54..57,1],pval[58..61,1],pval[62..65,1],pval[66..69,1],pval[70..73,1] \ ///
				pval[74..77,1],pval[78..81,1], pval[82..85,1],pval[86..89,1],pval[90..93,1],pval[94..97,1],pval[98..101,1],pval[102..105,1],pval[106..109,1] \ ///
				pval[110..113,1],pval[114..117,1],pval[118..121,1],pval[122..125,1],pval[126..129,1],pval[130..133,1],pval[134..137,1],pval[138..141,1],pval[142..145,1]]
	
	mat li pvall
	mat li bvall
	
	
	*|| Put into Excel (note: bp_mulitverse file needs to be in directory)

	putexcel set bp_multiverse_19, modify
	putexcel H5 = matrix(bvall)
	
	putexcel set bp_multiverse_19, modify
	putexcel X5 = matrix(pvall)
		
	clear all
	
	****************************************************************************	
	*|| FE-PANEL REG.WITH DRISCOLL-KRAAY SE'S FOR CROSS-SECTIONAL DEPENDENCE ||*
	****************************************************************************
	
	use panel_analysis
	
	*|| Define program to save estimation results in matrices
	
	cap program matdef
			mat a = r(table)
			
			mat p = a[4,1]
			mat b = a[1,1]
			mat se = a[2,1]
			mat lb = a[5,1]
			mat ub = a[6,1]
		
			mat pval=(pval \ p)
			mat bval=(bval \ b)
			mat seval=(seval \ se)
			mat lbval=(lbval \ lb)
			mat ubval=(ubval \ ub)
		end
		
	mat pval=[0]
	mat bval=[0]
	mat seval=[0]
	mat lbval=[0]	
	mat ubval=[0]
		
	*|| Adult Females 
	foreach depvar of global depvars_female {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsadu_fe1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_fe2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_fe3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_fe4_fe , fe lag(3)
			matdef
				}
				}
				}
	
	*|| Adult Males
	foreach depvar of global depvars_male {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsadu_ma1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_ma2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_ma3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_ma4_fe , fe lag(3)
			matdef
				}
				}
				}
				
	*|| Children and adolescent females
   foreach depvar of global depvars_girls {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsca_fe1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_fe2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_fe3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_fe4_fe , fe lag(3)
			matdef
				}
				}
				}
	
	*|| Children and adolescent males
	foreach depvar of global depvars_boys {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsca_ma1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_ma2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_ma3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_ma4_fe , fe lag(3)
			matdef
				}
				}
				}

	**********************************************
	*|| TRANSFORM MATRICES AND OUTPUT TO EXCEL ||*
	**********************************************
	*|| Create matrix with all estimates, transform to variables, save in Excel Workbook. 

	mat bvall = [bval[2..5,1],bval[6..9,1],bval[10..13,1],bval[14..17,1],bval[18..21,1],bval[22..25,1],bval[26..29,1],bval[30..33,1],bval[34..37,1] \ ///
				bval[38..41,1], bval[42..45,1],bval[46..49,1],bval[50..53,1],bval[54..57,1],bval[58..61,1],bval[62..65,1],bval[66..69,1],bval[70..73,1] \ ///
				bval[74..77,1],bval[78..81,1], bval[82..85,1],bval[86..89,1],bval[90..93,1],bval[94..97,1],bval[98..101,1],bval[102..105,1],bval[106..109,1] \ ///
				bval[110..113,1],bval[114..117,1],bval[118..121,1],bval[122..125,1],bval[126..129,1],bval[130..133,1],bval[134..137,1],bval[138..141,1],bval[142..145,1]]
	
	mat pvall = [pval[2..5,1],pval[6..9,1],pval[10..13,1],pval[14..17,1],pval[18..21,1],pval[22..25,1],pval[26..29,1],pval[30..33,1],pval[34..37,1] \ ///
				pval[38..41,1],pval[42..45,1],pval[46..49,1],pval[50..53,1],pval[54..57,1],pval[58..61,1],pval[62..65,1],pval[66..69,1],pval[70..73,1] \ ///
				pval[74..77,1],pval[78..81,1], pval[82..85,1],pval[86..89,1],pval[90..93,1],pval[94..97,1],pval[98..101,1],pval[102..105,1],pval[106..109,1] \ ///
				pval[110..113,1],pval[114..117,1],pval[118..121,1],pval[122..125,1],pval[126..129,1],pval[130..133,1],bval[134..137,1],pval[138..141,1],pval[142..145,1]]
	
	mat li pvall
	mat li bvall
	
	
	*|| Put into Excel (note: bp_mulitverse file needs to be in directory)

	putexcel set bp_multiverse_19, modify
	putexcel H21 = matrix(bvall)
	
	putexcel set bp_multiverse_19, modify
	putexcel X21 = matrix(pvall)
	

	***************

	******************************************
	*|| II.)LOW-TO-MIDDLE-INCOME COUNTRIES *||
	******************************************

	clear all
	version 15
	set more off, perm
	set matsize 800
		
	cd "C:\Users\..."
	use panel
	
	*|| Create Standardized Values 
	
	foreach var of varlist unpf - asdpm {
	egen z`var' = std(`var')
	drop `var'
	}
	renpfix z
	
	*|| Set outcomes and exposures 
	
	global depvars_male "bmim owm obem" 
	global depvars_female "bmif owf obef"
	global depvars_boys "bmib owb obeb" 
	global depvars_girls "bmig owg obeg"
	
	global exposure1 "upfssb"
	global exposure2 "upfssb_av2"
	global exposure3 "upfssb_av3"
	
	*|| Set the models for 2-level analysis 
	*Adult males 
	global covsadu_ma1 "if inc2==0"
	global covsadu_ma2 "unpf ipa_ma gdppc if inc2==0"
	global covsadu_ma3 "unpf ipa_ma gdppc alcohollpc totalkcalpc if inc2==0"
	global covsadu_ma4 "unpf ipa_ma gdppc alcohollpc totalkcalpc prop_urban if inc2==0"

	*Adult females
	global covsadu_fe1 "if inc2==0"
	global covsadu_fe2 "unpf ipa_fe gdppc if inc2==0"
	global covsadu_fe3 "unpf ipa_fe gdppc alcohollpc totalkcalpc if inc2==0"
	global covsadu_fe4 "unpf ipa_fe gdppc alcohollpc totalkcalpc prop_urban if inc2==0"
	
	*Children and adolescents boys
	global covsca_ma1 "if inc2==0"
	global covsca_ma2 "unpf ipa_ma gdppc if inc2==0"
	global covsca_ma3 "unpf ipa_ma gdppc totalkcalpc if inc2==0"
	global covsca_ma4 "unpf ipa_ma gdppc totalkcalpc prop_urban if inc2==0"

	*Children and adolescents girls
	global covsca_fe1 "if inc2==0"
	global covsca_fe2 "unpf ipa_fe gdppc if inc2==0"
	global covsca_fe3 "unpf ipa_fe gdppc totalkcalpc if inc2==0"
	global covsca_fe4 "unpf ipa_fe gdppc totalkcalpc prop_urban if inc2==0"
	
	*|| Set the models for Fixed-Effects (without ipa)
	*Adult males 
	global covsadu_ma1_fe "if inc2==0"
	global covsadu_ma2_fe "unpf gdppc if inc2==0"
	global covsadu_ma3_fe "unpf gdppc alcohollpc totalkcalpc if inc2==0"
	global covsadu_ma4_fe "unpf gdppc alcohollpc totalkcalpc prop_urban if inc2==0"

	*Adult females
	global covsadu_fe1_fe "if inc2==0"
	global covsadu_fe2_fe "unpf gdppc alcohollpc if inc2==0"
	global covsadu_fe3_fe "unpf gdppc alcohollpc totalkcalpc if inc2==0"
	global covsadu_fe4_fe "unpf gdppc alcohollpc totalkcalpc prop_urban if inc2==0"
	
	*Children and adolescents boys
	global covsca_ma1_fe "if inc2==0"
	global covsca_ma2_fe "unpf gdppc if inc2==0"
	global covsca_ma3_fe "unpf gdppc totalkcalpc if inc2==0"
	global covsca_ma4_fe "unpf gdppc totalkcalpc prop_urban if inc2==0"

	*Children and adolescents girls
	global covsca_fe1_fe "if inc2==0"
	global covsca_fe2_fe "unpf gdppc if inc2==0"
	global covsca_fe3_fe "unpf gdppc totalkcalpc if inc2==0"
	global covsca_fe4_fe "unpf gdppc totalkcalpc prop_urban if inc2==0"

	save panel_analysis.dta, replace
	
	*******************************************************************************	
	*|| TWO-LEVEL MIXED MODEL WITH VARYING INTERCEPTS AND SLOPES & Clustered SE ||*
	*******************************************************************************
	use panel_analysis
		
	*|| Define program to save estimation results in matrices
	cap program matdef
			mat a = r(table)
			
			mat p = a[4,1]
			mat b = a[1,1]
			mat se = a[2,1]
			mat lb = a[5,1]
			mat ub = a[6,1]
		
			mat pval=(pval \ p)
			mat bval=(bval \ b)
			mat seval=(seval \ se)
			mat lbval=(lbval \ lb)
			mat ubval=(ubval \ ub)
		end
		
	mat pval=[0]
	mat bval=[0]
	mat seval=[0]
	mat lbval=[0]	
	mat ubval=[0]

	*|| Adult Females 
	foreach depvar of global depvars_female {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsadu_fe1 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_fe2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_fe3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_fe4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}
	
	*|| Adult Males
	foreach depvar of global depvars_male {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsadu_ma1 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_ma2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_ma3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_ma4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}

	*|| Children and adolescent girls
	foreach depvar of global depvars_girls {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsca_fe1 || country: `exp', nolog cov(un) vce(cluster country) 
			matdef
		mixed `depvar' `exp' $covsca_fe2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_fe3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_fe4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}
	
	*|| Children and adolescent boys
	foreach depvar of global depvars_boys {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsca_ma1 || country: `exp', nolog cov(un) vce(cluster country) 
			matdef
		mixed `depvar' `exp' $covsca_ma2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_ma3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_ma4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}

	
	**********************************************
	*|| TRANSFORM MATRICES AND OUTPUT TO EXCEL ||*
	**********************************************
	*|| Create matrix with all estimates, transform to variables, save in Excel Workbook. 

	mat bvall = [bval[2..5,1],bval[6..9,1],bval[10..13,1],bval[14..17,1],bval[18..21,1],bval[22..25,1],bval[26..29,1],bval[30..33,1],bval[34..37,1] \ ///
				bval[38..41,1], bval[42..45,1],bval[46..49,1],bval[50..53,1],bval[54..57,1],bval[58..61,1],bval[62..65,1],bval[66..69,1],bval[70..73,1] \ ///
				bval[74..77,1],bval[78..81,1], bval[82..85,1],bval[86..89,1],bval[90..93,1],bval[94..97,1],bval[98..101,1],bval[102..105,1],bval[106..109,1] \ ///
				bval[110..113,1],bval[114..117,1],bval[118..121,1],bval[122..125,1],bval[126..129,1],bval[130..133,1],bval[134..137,1],bval[138..141,1],bval[142..145,1]]
	
	mat pvall = [pval[2..5,1],pval[6..9,1],pval[10..13,1],pval[14..17,1],pval[18..21,1],pval[22..25,1],pval[26..29,1],pval[30..33,1],pval[34..37,1] \ ///
				pval[38..41,1],pval[42..45,1],pval[46..49,1],pval[50..53,1],pval[54..57,1],pval[58..61,1],pval[62..65,1],pval[66..69,1],pval[70..73,1] \ ///
				pval[74..77,1],pval[78..81,1], pval[82..85,1],pval[86..89,1],pval[90..93,1],pval[94..97,1],pval[98..101,1],pval[102..105,1],pval[106..109,1] \ ///
				pval[110..113,1],pval[114..117,1],pval[118..121,1],pval[122..125,1],pval[126..129,1],pval[130..133,1],pval[134..137,1],pval[138..141,1],pval[142..145,1]]
	
	mat li pvall
	mat li bvall
	
	
	*|| Put into Excel (note: bp_mulitverse file needs to be in directory)

	putexcel set bp_multiverse_LMIC, modify
	putexcel H5 = matrix(bvall)
	
	putexcel set bp_multiverse_LMIC, modify
	putexcel X5 = matrix(pvall)
	
	clear all
	
	****************************************************************************	
	*|| FE-PANEL REG.WITH DRISCOLL-KRAAY SE'S FOR CROSS-SECTIONAL DEPENDENCE ||*
	****************************************************************************
	
	use panel_analysis
	
	*|| Define program to save estimation results in matrices
	
	cap program matdef
			mat a = r(table)
			
			mat p = a[4,1]
			mat b = a[1,1]
			mat se = a[2,1]
			mat lb = a[5,1]
			mat ub = a[6,1]
		
			mat pval=(pval \ p)
			mat bval=(bval \ b)
			mat seval=(seval \ se)
			mat lbval=(lbval \ lb)
			mat ubval=(ubval \ ub)
		end
		
	mat pval=[0]
	mat bval=[0]
	mat seval=[0]
	mat lbval=[0]	
	mat ubval=[0]
		
	*|| Adult Females 
	foreach depvar of global depvars_female {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsadu_fe1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_fe2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_fe3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_fe4_fe , fe lag(3)
			matdef
				}
				}
				}
	
	*|| Adult Males
	foreach depvar of global depvars_male {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsadu_ma1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_ma2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_ma3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_ma4_fe , fe lag(3)
			matdef
				}
				}
				}
				
	*|| Children and adolescent females
   foreach depvar of global depvars_girls {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsca_fe1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_fe2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_fe3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_fe4_fe , fe lag(3)
			matdef
				}
				}
				}
	
	*|| Children and adolescent males
	foreach depvar of global depvars_boys {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsca_ma1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_ma2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_ma3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_ma4_fe , fe lag(3)
			matdef
				}
				}
				}

	**********************************************
	*|| TRANSFORM MATRICES AND OUTPUT TO EXCEL ||*
	**********************************************
	*|| Create matrix with all estimates, transform to variables, save in Excel Workbook. 

	mat bvall = [bval[2..5,1],bval[6..9,1],bval[10..13,1],bval[14..17,1],bval[18..21,1],bval[22..25,1],bval[26..29,1],bval[30..33,1],bval[34..37,1] \ ///
				bval[38..41,1], bval[42..45,1],bval[46..49,1],bval[50..53,1],bval[54..57,1],bval[58..61,1],bval[62..65,1],bval[66..69,1],bval[70..73,1] \ ///
				bval[74..77,1],bval[78..81,1], bval[82..85,1],bval[86..89,1],bval[90..93,1],bval[94..97,1],bval[98..101,1],bval[102..105,1],bval[106..109,1] \ ///
				bval[110..113,1],bval[114..117,1],bval[118..121,1],bval[122..125,1],bval[126..129,1],bval[130..133,1],bval[134..137,1],bval[138..141,1],bval[142..145,1]]
	
	mat pvall = [pval[2..5,1],pval[6..9,1],pval[10..13,1],pval[14..17,1],pval[18..21,1],pval[22..25,1],pval[26..29,1],pval[30..33,1],pval[34..37,1] \ ///
				pval[38..41,1],pval[42..45,1],pval[46..49,1],pval[50..53,1],pval[54..57,1],pval[58..61,1],pval[62..65,1],pval[66..69,1],pval[70..73,1] \ ///
				pval[74..77,1],pval[78..81,1], pval[82..85,1],pval[86..89,1],pval[90..93,1],pval[94..97,1],pval[98..101,1],pval[102..105,1],pval[106..109,1] \ ///
				pval[110..113,1],pval[114..117,1],pval[118..121,1],pval[122..125,1],pval[126..129,1],pval[130..133,1],pval[134..137,1],pval[138..141,1],pval[142..145,1]]
	
	mat li pvall
	mat li bvall
	
	
	*|| Put into Excel (note: bp_mulitverse file needs to be in directory)

	putexcel set bp_multiverse_LMIC, modify
	putexcel H21 = matrix(bvall)
	
	putexcel set bp_multiverse_LMIC, modify
	putexcel X21 = matrix(pvall)
	
	clear all
	***************

	*****************************************
	*||   III.) HIGH-INCOME COUNTRIES     *||
	*****************************************
	
	clear all
	version 15
	set more off, perm
	set matsize 800
		
	cd "C:\Users\..."
	use panel
	
	*|| Create Standardized Values 
	
	foreach var of varlist unpf - asdpm {
	egen z`var' = std(`var')
	drop `var'
	}
	renpfix z
	
	*|| Set outcomes and exposures 
	
	global depvars_male "bmim owm obem" 
	global depvars_female "bmif owf obef"
	global depvars_boys "bmib owb obeb" 
	global depvars_girls "bmig owg obeg"
	
	global exposure1 "upfssb"
	global exposure2 "upfssb_av2"
	global exposure3 "upfssb_av3"
	
	*|| Set the models for 2-level analysis 
	*Adult males 
	global covsadu_ma1 "if inc2==1"
	global covsadu_ma2 "unpf ipa_ma gdppc if inc2==1"
	global covsadu_ma3 "unpf ipa_ma gdppc alcohollpc totalkcalpc if inc2==1"
	global covsadu_ma4 "unpf ipa_ma gdppc alcohollpc totalkcalpc prop_urban if inc2==1"

	*Adult females
	global covsadu_fe1 "if inc2==1"
	global covsadu_fe2 "unpf ipa_fe gdppc if inc2==1"
	global covsadu_fe3 "unpf ipa_fe gdppc alcohollpc totalkcalpc if inc2==1"
	global covsadu_fe4 "unpf ipa_fe gdppc alcohollpc totalkcalpc prop_urban if inc2==1"
	
	*Children and adolescents boys
	global covsca_ma1 "if inc2==1"
	global covsca_ma2 "unpf ipa_ma gdppc if inc2==1"
	global covsca_ma3 "unpf ipa_ma gdppc totalkcalpc if inc2==1"
	global covsca_ma4 "unpf ipa_ma gdppc totalkcalpc prop_urban if inc2==1"

	*Children and adolescents girls
	global covsca_fe1 "if inc2==1"
	global covsca_fe2 "unpf ipa_fe gdppc if inc2==1"
	global covsca_fe3 "unpf ipa_fe gdppc totalkcalpc if inc2==1"
	global covsca_fe4 "unpf ipa_fe gdppc totalkcalpc prop_urban if inc2==1"
	
	*|| Set the models for Fixed-Effects (without ipa)
	*Adult males 
	global covsadu_ma1_fe "if inc2==1"
	global covsadu_ma2_fe "unpf gdppc if inc2==1"
	global covsadu_ma3_fe "unpf gdppc alcohollpc totalkcalpc if inc2==1"
	global covsadu_ma4_fe "unpf gdppc alcohollpc totalkcalpc prop_urban if inc2==1"

	*Adult females
	global covsadu_fe1_fe "if inc2==1"
	global covsadu_fe2_fe "unpf gdppc alcohollpc if inc2==1"
	global covsadu_fe3_fe "unpf gdppc alcohollpc totalkcalpc if inc2==1"
	global covsadu_fe4_fe "unpf gdppc alcohollpc totalkcalpc prop_urban if inc2==1"
	
	*Children and adolescents boys
	global covsca_ma1_fe "if inc2==1"
	global covsca_ma2_fe "unpf gdppc if inc2==1"
	global covsca_ma3_fe "unpf gdppc totalkcalpc if inc2==1"
	global covsca_ma4_fe "unpf gdppc totalkcalpc prop_urban if inc2==1"

	*Children and adolescents girls
	global covsca_fe1_fe "if inc2==1"
	global covsca_fe2_fe "unpf gdppc if inc2==1"
	global covsca_fe3_fe "unpf gdppc totalkcalpc if inc2==1"
	global covsca_fe4_fe "unpf gdppc totalkcalpc prop_urban if inc2==1"

	save panel_analysis.dta, replace
	
	*******************************************************************************	
	*|| TWO-LEVEL MIXED MODEL WITH VARYING INTERCEPTS AND SLOPES & Clustered SE ||*
	*******************************************************************************
	use panel_analysis
		
	*|| Define program to save estimation results in matrices
	cap program matdef
			mat a = r(table)
			
			mat p = a[4,1]
			mat b = a[1,1]
			mat se = a[2,1]
			mat lb = a[5,1]
			mat ub = a[6,1]
		
			mat pval=(pval \ p)
			mat bval=(bval \ b)
			mat seval=(seval \ se)
			mat lbval=(lbval \ lb)
			mat ubval=(ubval \ ub)
		end
		
	mat pval=[0]
	mat bval=[0]
	mat seval=[0]
	mat lbval=[0]	
	mat ubval=[0]

	*|| Adult Females 
	foreach depvar of global depvars_female {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsadu_fe1 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_fe2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_fe3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_fe4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}
	
	*|| Adult Males
	foreach depvar of global depvars_male {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsadu_ma1 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_ma2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_ma3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsadu_ma4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}

	*|| Children and adolescent girls
	foreach depvar of global depvars_girls {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsca_fe1 || country: `exp', nolog cov(un) vce(cluster country) 
			matdef
		mixed `depvar' `exp' $covsca_fe2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_fe3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_fe4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}
	
	*|| Children and adolescent boys
	foreach depvar of global depvars_boys {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		mixed `depvar' `exp' $covsca_ma1 || country: `exp', nolog cov(un) vce(cluster country) 
			matdef
		mixed `depvar' `exp' $covsca_ma2 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_ma3 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
		mixed `depvar' `exp' $covsca_ma4 || country: `exp', nolog cov(un) vce(cluster country)
			matdef
				}
				}
				}

	
	**********************************************
	*|| TRANSFORM MATRICES AND OUTPUT TO EXCEL ||*
	**********************************************
	*|| Create matrix with all estimates, transform to variables, save in Excel Workbook. 

	mat bvall = [bval[2..5,1],bval[6..9,1],bval[10..13,1],bval[14..17,1],bval[18..21,1],bval[22..25,1],bval[26..29,1],bval[30..33,1],bval[34..37,1] \ ///
				bval[38..41,1], bval[42..45,1],bval[46..49,1],bval[50..53,1],bval[54..57,1],bval[58..61,1],bval[62..65,1],bval[66..69,1],bval[70..73,1] \ ///
				bval[74..77,1],bval[78..81,1], bval[82..85,1],bval[86..89,1],bval[90..93,1],bval[94..97,1],bval[98..101,1],bval[102..105,1],bval[106..109,1] \ ///
				bval[110..113,1],bval[114..117,1],bval[118..121,1],bval[122..125,1],bval[126..129,1],bval[130..133,1],bval[134..137,1],bval[138..141,1],bval[142..145,1]]
	
	mat pvall = [pval[2..5,1],pval[6..9,1],pval[10..13,1],pval[14..17,1],pval[18..21,1],pval[22..25,1],pval[26..29,1],pval[30..33,1],pval[34..37,1] \ ///
				pval[38..41,1],pval[42..45,1],pval[46..49,1],pval[50..53,1],pval[54..57,1],pval[58..61,1],pval[62..65,1],pval[66..69,1],pval[70..73,1] \ ///
				pval[74..77,1],pval[78..81,1], pval[82..85,1],pval[86..89,1],pval[90..93,1],pval[94..97,1],pval[98..101,1],pval[102..105,1],pval[106..109,1] \ ///
				pval[110..113,1],pval[114..117,1],pval[118..121,1],pval[122..125,1],pval[126..129,1],pval[130..133,1],pval[134..137,1],pval[138..141,1],pval[142..145,1]]
	
	mat li pvall
	mat li bvall
	
	
	*|| Put into Excel (note: bp_mulitverse file needs to be in directory)

	putexcel set bp_multiverse_HIC, modify
	putexcel H5 = matrix(bvall)
	
	putexcel set bp_multiverse_HIC, modify
	putexcel X5 = matrix(pvall)
	
	clear all
	
	****************************************************************************	
	*|| FE-PANEL REG.WITH DRISCOLL-KRAAY SE'S FOR CROSS-SECTIONAL DEPENDENCE ||*
	****************************************************************************
	
	use panel_analysis
	
	*|| Define program to save estimation results in matrices
	
	cap program matdef
			mat a = r(table)
			
			mat p = a[4,1]
			mat b = a[1,1]
			mat se = a[2,1]
			mat lb = a[5,1]
			mat ub = a[6,1]
		
			mat pval=(pval \ p)
			mat bval=(bval \ b)
			mat seval=(seval \ se)
			mat lbval=(lbval \ lb)
			mat ubval=(ubval \ ub)
		end
		
	mat pval=[0]
	mat bval=[0]
	mat seval=[0]
	mat lbval=[0]	
	mat ubval=[0]
		
	*|| Adult Females 
	foreach depvar of global depvars_female {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsadu_fe1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_fe2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_fe3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_fe4_fe , fe lag(3)
			matdef
				}
				}
				}
	
	*|| Adult Males
	foreach depvar of global depvars_male {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsadu_ma1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_ma2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_ma3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsadu_ma4_fe , fe lag(3)
			matdef
				}
				}
				}
				
	*|| Children and adolescent females
   foreach depvar of global depvars_girls {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsca_fe1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_fe2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_fe3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_fe4_fe , fe lag(3)
			matdef
				}
				}
				}
	
	*|| Children and adolescent males
	foreach depvar of global depvars_boys {
	 forvalues i= 1/3 {
	  foreach exp of global exposure`i' {
		xtscc `depvar' `exp' $covsca_ma1_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_ma2_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_ma3_fe , fe lag(3)
			matdef
		xtscc `depvar' `exp' $covsca_ma4_fe , fe lag(3)
			matdef
				}
				}
				}

	**********************************************
	*|| TRANSFORM MATRICES AND OUTPUT TO EXCEL ||*
	**********************************************
	*|| Create matrix with all estimates, transform to variables, save in Excel Workbook. 

	mat bvall = [bval[2..5,1],bval[6..9,1],bval[10..13,1],bval[14..17,1],bval[18..21,1],bval[22..25,1],bval[26..29,1],bval[30..33,1],bval[34..37,1] \ ///
				bval[38..41,1], bval[42..45,1],bval[46..49,1],bval[50..53,1],bval[54..57,1],bval[58..61,1],bval[62..65,1],bval[66..69,1],bval[70..73,1] \ ///
				bval[74..77,1],bval[78..81,1], bval[82..85,1],bval[86..89,1],bval[90..93,1],bval[94..97,1],bval[98..101,1],bval[102..105,1],bval[106..109,1] \ ///
				bval[110..113,1],bval[114..117,1],bval[118..121,1],bval[122..125,1],bval[126..129,1],bval[130..133,1],bval[134..137,1],bval[138..141,1],bval[142..145,1]]
	
	mat pvall = [pval[2..5,1],pval[6..9,1],pval[10..13,1],pval[14..17,1],pval[18..21,1],pval[22..25,1],pval[26..29,1],pval[30..33,1],pval[34..37,1] \ ///
				pval[38..41,1],pval[42..45,1],pval[46..49,1],pval[50..53,1],pval[54..57,1],pval[58..61,1],pval[62..65,1],pval[66..69,1],pval[70..73,1] \ ///
				pval[74..77,1],pval[78..81,1], pval[82..85,1],pval[86..89,1],pval[90..93,1],pval[94..97,1],pval[98..101,1],pval[102..105,1],pval[106..109,1] \ ///
				pval[110..113,1],pval[114..117,1],pval[118..121,1],pval[122..125,1],pval[126..129,1],pval[130..133,1],pval[134..137,1],pval[138..141,1],pval[142..145,1]]
	
	mat li pvall
	mat li bvall
	
	
	*|| Put into Excel (note: bp_mulitverse file needs to be in directory)

	putexcel set bp_multiverse_HIC, modify
	putexcel H21 = matrix(bvall)
	
	putexcel set bp_multiverse_HIC, modify
	putexcel X21 = matrix(pvall)
	
	clear all
	
	**********************
	*|| END OF DO-FILE ||*
	**********************
