	****************************************************************************
	*** Kai Schulze
	*** 01/03/2019
	*** Project: Associations between sales of ultra-processed food products and
	*** 	     prevalence of adiposity and diabetes mellitus: a panel analysis 
    *** 		 of 76 countries between 2001-2016
	*** Step: Descriptive Figures and Stats
	****************************************************************************
		
	****************************************************************************
	*||     					   SUMMARY OF STEPS			         		 ||*
	****************************************************************************
	
	/*
		I. 		COLLECTION OF REFERENCES TO MANIPULATE GRAPHS IN STATA
		II. 	CREATE GROWTH VARIABLES, OUTPUT TABLE 1
		III. 	CREATE BMI GRAPHS: FEMALE/MALE/GIRLS/BOYS FOR ALL, HICS, LMICS
		IV. 	CREATE OVERWEIGHT GRAPHS: FEMALE/MALE/GIRLS/BOYS FOR ALL, HICS, LMICS
		V. 		CREATE OBESITY GRAPHS: FEMALE/MALE/GIRLS/BOYS FOR ALL, HICS, LMICS
		VI. 	CREATE DIABETES GRAPHS: FEMALE/MALE FOR ALL, HICS, LMICS
		VII. 	COMBINE THE GRAPHS TO CREATE FIGURES FOR MAIN TEXT/APPENDIX
		VIII. 	EXPORT DATA FOR WORLD MAP OF UPF AND CHANGES OF UPF
	*/ 	
	
	clear all
	version 15
	set more off
	
	cd ""
	use panel	
	
	*****************************************************************
	*|| I. COLLECTION OF REFERENCES TO MANIPULATE GRAPHS IN STATA *|| 
	*****************************************************************
	set scheme plotplain
	
	/*
	MOST IMPORTANT:
	help graph query
	
	1.) 
	CLINE Options for type of Line:
	https://www.stata.com/manuals13/g-3cline_options.pdf#g-3cline_options
	
	2.) 
	Symbolstyle for style of symbols, in for example scatter
	https://www.stata.com/manuals13/g-4symbolstyle.pdf
	
	3.) 
	Showing all the colors that are available:
	graph query colorstyle
	
	4.) Overall Twoway options:
	https://www.stata.com/manuals13/g-3twoway_options.pdf
	
	5.) TRANSPARENCY IN GRAPHS, from Stata 15 
	https://www.stata.com/new-in-stata/transparency-in-graphs/
	simply add: mcolor(plg2%40) 
	*/
		
	***************************************************
	*|| II. CREATE GROWTH VARIABLES, OUTPUT TABLE 1 *|| 
	***************************************************

	*keep cou cc year upfssb asdpm asdpf bmib bmig bmim bmif owb owg owm owf obeb obeg obem obef
	keep if year==2001 | year==2016

	*|| CREATE GROWTH VARIABLES 

		foreach var of varlist bmim bmif bmib bmig asdpm asdpf owb owg owm owf obeb obeg obem obef upfssb unpf prop_urban alcohollpc totalkcalpc gdppc{
		bys cou: gen v1=100*(`var'[_n]-`var'[_n-1])/`var'[_n-1]
		bys cou: egen v2 = mean(v1)
		gen gr`var' = (v2+100) 
		drop v1-v2
		}

		order cou year upfssb grupfssb asdpm grasdpm bmib grbmib obeb grobeb obef grobef 
		
	*|| GIVE LABELS AND NAMES 
	
		cap la var grunpf "Change of Un- or Minimally Processed Foods (in %)"	
		cap la var grpf "Change of Processed Foods (in %)"
		cap la var grupf "Change of Ultra-processed Foods (in %)"
		cap la var grupfssb "Change of Ultra-Processed Foods (in %)"
		cap la var grurb "Change of Urbanization Rate (in %)"
		
		cap la var grasdpf "Change of Diabetes Mellitus Prevalence in females (in %)" 
		cap la var grasdpm "Change of Diabetes Mellitus Prevalence in males (in %)" 
		
		cap la var grbmib "Change of BMI in boys(in %)" 
		cap la var grbmig "Change of BMI in girls(in %)" 
		cap la var grbmim "Change of BMI in males(in %)" 
		cap la var grbmif "Change of BMI in females(in %)" 

		cap la var growb "Change of Overweight in boys(in %)" 
		cap la var growg "Change of Overweight in girls(in %)" 
		cap la var growm "Change of Overweight in males(in %)" 
		cap la var growf "Change of Overweight in females(in %)" 
		
		cap la var grobeb "Change of Obesity in boys(in %)" 
		cap la var grobeg "Change of Obesity in girls(in %)" 
		cap la var grobem "Change of Obesity in males(in %)" 
		cap la var grobef "Change of Obesity in females(in %)" 
		
		cap la def inc2 0 "Low & middle income countries" 1 "High income countries"
		cap la values inc2 inc2
		
		*keep if year==2016
		
	save descriptives, replace
	
	*|| TABLE 1 --> Outcomes and Covariates 2001/2016 + Wilcoxon-Rank-Sum Test  
	
	clear all
	use descriptives, clear 		
	
	foreach var of varlist obem obef obeb obeg owb owf owg owm asdpf asdpm {
		gen z`var' = 100*`var'
		drop `var'
	}
	renpfix z
	
	preserve
	table1_mc, test by(year) ///
	vars(bmim conts \ bmif conts \ bmig conts \ bmib conts \ obem conts \ obef conts \ ///
		obeb conts \ obeg conts \ owb conts \ owf conts \ owg conts \ owm conts \ asdpf conts \ asdpm conts \ ///
		upfssb conts \ unpf conts \ prop_urban conts \ alcohollpc conts \ ///
		totalkcalpc conts \ gdppc conts) ///
		format(%2.1f) extraspace clear 
		table1_mc_dta2docx using table1.docx, replace
	restore
	
	*bmim bmif bmib bmig asdpm asdpf owb owg owm owf obeb obeg obem obef upfssb unpf prop_urban alcohollpc totalkcalpc gdppc
	
	****************************************************************************
	*|| III. CREATE BMI GRAPHS: FEMALE/MALE/GIRLS/BOYS FOR ALL, HICS, LMICS  *||
	****************************************************************************
	clear all
	use descriptives, clear 
	keep if year==2016

	*|| Adults
		twoway /// 
			(qfit grbmif grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmif grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfbmifemale, replace)
		
		twoway /// 
			(qfit grbmim grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmim grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off) ytitle("") xlabel(#6) xtitle("")  ///
				saving(upfbmimale,replace)
				
	*|| Adults by income
		*| Females 
		twoway /// 
			(qfit grbmif grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmif grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfbmifemale_LMIC, replace)
				
		twoway /// 
			(qfit grbmif grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmif grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfbmifemale_HIC, replace) 
		
		*| Males
		twoway /// 
			(qfit grbmim grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmim grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off) ytitle("") xlabel(#6) xtitle("")  ///
				saving(upfbmimale_LMIC,replace)
		
		twoway /// 
			(qfit grbmim grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmim grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off) ytitle("") xlabel(#6) xtitle("")  ///
				saving(upfbmimale_LMIC,replace)
	
	*|| Children and adolescents 
		twoway /// 
			(qfit grbmig grupfssb , lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmig grupfssb , mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(circle) msize(medsmall) mcolor(mint%40)), ///
			legend(off) ytitle("") xtitle("") xlabel(#8) ///
				saving(upfbmigirls,replace)
			
		twoway /// 
			(qfit grbmib grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmib grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(square) msize(medsmall) mcolor(sand%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("") ///
				saving(upfbmiboys,replace)
	
	*|| Children and adolescents by income 
		*| Girls
		twoway /// 
			(qfit grbmig grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmig grupfssb if inc2==0 , mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(circle) msize(medsmall) mcolor(mint%40)), ///
			legend(off) ytitle("") xtitle("") xlabel(#8) ///
				saving(upfbmigirls_LMIC,replace)
		twoway /// 
			(qfit grbmig grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmig grupfssb if inc2==1 , mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(circle) msize(medsmall) mcolor(mint%40)), ///
			legend(off) ytitle("") xtitle("") xlabel(#8) ///
				saving(upfbmigirls_HIC,replace)
		
		*| Boys
		twoway /// 
			(qfit grbmib grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmib grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(square) msize(medsmall) mcolor(sand%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("") ///
				saving(upfbmiboys_LMIC,replace)
		twoway /// 
			(qfit grbmib grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grbmib grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(square) msize(medsmall) mcolor(sand%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("") ///
				saving(upfbmiboys_HIC,replace)

	**********************************************************************************
	*|| IV. CREATE OVERWEIGHT GRAPHS: FEMALE/MALE/GIRLS/BOYS FOR ALL, HICS, LMICS  *||
	**********************************************************************************
	clear all
	use descriptives, clear 
	keep if year==2016			
	
	*|| Adults
		twoway /// 
			(qfit growf grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growf grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfowefemale, replace)
		
		twoway /// 
			(qfit growm grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growm grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off) ytitle("") xlabel(#6) xtitle("")  ///
				saving(upfowemale,replace)
				
	*|| Adults by income
		*| Females 
		twoway /// 
			(qfit growf grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growf grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfowefemale_LMIC, replace)
				
		twoway /// 
			(qfit growf grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growf grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfowefemale_HIC, replace) 
		
		*| Males
		twoway /// 
			(qfit growm grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growm grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off) ytitle("") xlabel(#6) xtitle("")  ///
				saving(upfowemale_LMIC,replace)
		
		twoway /// 
			(qfit growm grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growm grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off) ytitle("") xlabel(#6) xtitle("")  ///
				saving(upfowemale_HIC,replace)
	
	*|| Children and adolescents 
		twoway /// 
			(qfit growg grupfssb , lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growg grupfssb , mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(circle) msize(medsmall) mcolor(mint%40)), ///
			legend(off) ytitle("") xtitle("") xlabel(#8) ///
				saving(upfowegirls,replace)
			
		twoway /// 
			(qfit growb grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growb grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(square) msize(medsmall) mcolor(sand%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("") ///
				saving(upfoweboys,replace)
	
	*|| Children and adolescents by income 
		*| Girls
		twoway /// 
			(qfit growg grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growg grupfssb if inc2==0 , mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(circle) msize(medsmall) mcolor(mint%40)), ///
			legend(off) ytitle("") xtitle("") xlabel(#8) ///
				saving(upfowegirls_LMIC,replace)
		twoway /// 
			(qfit growg grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growg grupfssb if inc2==1 , mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(circle) msize(medsmall) mcolor(mint%40)), ///
			legend(off) ytitle("") xtitle("") xlabel(#8) ///
				saving(upfowegirls_HIC,replace)
		
		*| Boys
		twoway /// 
			(qfit growb grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growb grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(square) msize(medsmall) mcolor(sand%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("") ///
				saving(upfoweboys_LMIC,replace)
		twoway /// 
			(qfit growb grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter growb grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(square) msize(medsmall) mcolor(sand%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("") ///
				saving(upfoweboys_HIC,replace)			
	
	******************************************************************************
	*|| V. CREATE OBESITY GRAPHS: FEMALE/MALE/GIRLS/BOYS FOR ALL, HICS, LMICS  *||
	******************************************************************************
	clear all
	use descriptives, clear 
	keep if year==2016						
					
	*|| Adults
		twoway /// 
			(qfit grobef grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobef grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfobefemale, replace)
		
		twoway /// 
			(qfit grobem grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter  grobem grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off)	ytitle("") xlabel(#6) xtitle("") ///
				saving(upfobemale,replace)
				
	*|| Adults by income 
		*| Females 
		twoway /// 
			(qfit grobef grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobef grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfobefemale_LMIC, replace)
		twoway /// 
			(qfit grobef grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobef grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfobefemale_HIC, replace)
				
		*| Males 
		twoway /// 
			(qfit grobem grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobem grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off)	ytitle("") xlabel(#6) xtitle("") ///
				saving(upfobemale_LMIC, replace)
		twoway /// 
			(qfit grobem grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobem grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off)	ytitle("") xlabel(#6) xtitle("") ///
				saving(upfobemale_HIC, replace)
	
	*|| Children and adolescents 
	preserve
		drop if cou==64 // Zambia
		twoway /// 
			(qfit grobeg grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobeg grupfssb , mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(circle) msize(medsmall) mcolor(mint%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfobegirls,replace)
			
		twoway /// 
			(qfit grobeb grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobeb grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(square) msize(medsmall) mcolor(sandb%40)), ///
			legend(off) ytitle("") xlabel(#6) xtitle("") ///
				saving(upfobeboys,replace)
					
	*|| Children and adolescents by income
		*| Girls 
		twoway /// 
			(qfit grobeg grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobeg grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(circle) msize(medsmall) mcolor(mint%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfobegirls_LMIC,replace)
		twoway /// 
			(qfit grobeg grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobeg grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(circle) msize(medsmall) mcolor(mint%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfobegirls_HIC,replace)

		*| Boys
		twoway /// 
			(qfit grobeb grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobeb grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(square) msize(medsmall) mcolor(sand%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("") ///
				saving(upfobeboys_LMIC,replace)
		twoway /// 
			(qfit grobeb grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grobeb grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(square) msize(medsmall) mcolor(sand%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("") ///
				saving(upfobeboys_HIC,replace)
	restore
	
	********************************************************************************
	*|| VI. CREATE DIABETES GRAPHS: FEMALE/MALE/GIRLS/BOYS FOR ALL, HICS, LMICS  *||
	********************************************************************************
	clear all
	use descriptives, clear 
	keep if year==2016	
	
	*|| Adults
		twoway /// 
			(qfit grasdpf grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grasdpf grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfdiabfemale, replace)
		
		twoway /// 
			(qfit grasdpm grupfssb, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grasdpm grupfssb, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off)	ytitle("") xlabel(#6) xtitle("") ///
				saving(upfdiabmale,replace)
	
	*|| Female by income
		twoway /// 
			(qfit grasdpf grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grasdpf grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfdiabfemale_LMIC, replace)	
		twoway /// 
			(qfit grasdpf grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grasdpf grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(triangle) msize(medsmall) mcolor(lavender%40)), ///
			legend(off) ytitle("") xlabel(#8) xtitle("")  ///
				saving(upfdiabfemale_HIC, replace)	
				
	*|| Male by income
		twoway /// 
			(qfit grasdpm grupfssb if inc2==0, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grasdpm grupfssb if inc2==0, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off)	ytitle("") xlabel(#6) xtitle("") ///
				saving(upfdiabmale_LMIC, replace)	
		twoway /// 
			(qfit grasdpm grupfssb if inc2==1, lpattern(solid) lwidth(thin) lcolor (gs4)) ///
			(scatter grasdpm grupfssb if inc2==1, mlabel(cc) mlabposition(6) mlabsize(small) ///
			msymbol(diamond) msize(medsmall) mcolor(plb2%40)), ///
			legend(off)	ytitle("") xlabel(#6) xtitle("") ///
				saving(upfdiabmale_HIC, replace)	
	
	*************************************************************************
	*|| VII. COMBINE THE GRAPHS TO CREATE FIGURES FOR MAIN TEXT/APPENDIX  *||
	*************************************************************************
	
	*|| FIGURE 2 -- BMI&OBESITY -- FEMALES, MALES, GIRLS, BOYS
	gr combine "upfbmifemale" "upfbmimale" "upfbmigirls" "upfbmiboys" /// 
		"upfobefemale" "upfobemale" "upfobegirls" "upfobeboys", imargin(0 0 0 0) ///
			title("") rows(2)	
	
	gr export UPFBMIOBE.tif, width(2500) replace
	
	*|| FIGURE 3 -- OBESITY -FEMALES & GIRLS HIC/LMICS; DIABETES FEMALES ALL/LMIC/HIC, MALE ALL
	gr combine "upfobefemale_LMIC" "upfobefemale_HIC" "upfobegirls_LMIC" "upfobegirls_HIC" ///
		"upfdiabfemale" "upfdiabmale" "upfdiabfemale_LMIC" "upfdiabfemale_HIC", ///
		imargin(0 0 0 0) title("") rows(2)
	gr export OBEDIABBYINC.tif, width(2500) replace
	
	*|| SUPPLEMENTAL FIGURE 1 - BMI ALL FIGURES 
	gr combine "upfbmifemale" "upfbmimale" "upfbmigirls" "upfbmiboys" ///
	"upfbmifemale_LMIC" "upfbmifemale_HIC" "upfbmimale_LMIC" "upfbmimale_HIC" ///
	"upfbmigirls_LMIC" "upfbmigirls_HIC" "upfbmiboys_LMIC" "upfbmiboys_HIC", imargin(0 0 0 0) ///
			title("") rows(3) 		
	gr export UPFBMI_ALL.tif, width(2500) replace	
	
	*|| SUPPLEMENTAL FIGURE 1 ALTERNATIVE - BMI BY INCOME FOR ALL POPULATIONS 
	gr combine "upfbmifemale_LMIC" "upfbmifemale_HIC" "upfbmimale_LMIC" "upfbmimale_HIC" ///
	"upfbmigirls_LMIC" "upfbmigirls_HIC" "upfbmiboys_LMIC" "upfbmiboys_HIC", imargin(0 0 0 0) ///
			title("") rows(2) 		
	gr export UPFBMI_BYINC.tif, width(2500) replace	
	
	*|| SUPPLEMENTAL FIGURE 2 - ALL OVERWEIGHT FIGURES 
	gr combine "upfowefemale" "upfowemale" "upfowegirls" "upfoweboys" ///
	"upfowefemale_LMIC" "upfowefemale_HIC" "upfowemale_LMIC" "upfowemale_HIC" ///
	"upfowegirls_LMIC" "upfowegirls_HIC" "upfoweboys_LMIC" "upfoweboys_HIC", imargin(0 0 0 0) ///
			title("") rows(3)
	
	gr export UPFOWE_ALL.tif, width(2500) replace	

	*|| SUPPLEMENTAL FIGURE 3 - ALL OBESITY FIGURES
	gr combine "upfobefemale" "upfobemale" "upfobegirls" "upfobeboys" ///
	"upfobefemale_LMIC" "upfobefemale_HIC" "upfobemale_LMIC" "upfobemale_HIC" ///
	"upfobegirls_LMIC" "upfobegirls_HIC" "upfobeboys_LMIC" "upfobeboys_HIC", imargin(0 0 0 0) ///
			title("") rows(3)	
	gr export UPFOBE_ALL.tif, width(2500) replace	

	*|| SUPPLEMENTAL FIGURE 3 - DIABETES 
	gr combine "upfdiabmale_LMIC" "upfdiabmale_HIC", imargin(0 0 0 0) ///
		title("") rows(1)
		
	gr export UPFDIAB_MALEBYINCOME.tif, width(2500) replace		
	
	*********************************************************************
	//*  VIII. EXPORT DATA FOR WORLD MAP OF UPF AND CHANGES OF UPF   *// 
	*********************************************************************
	clear all
	use descriptives, clear 
	keep country upfssb grupfssb
	gen upfssb2 = upfssb*100



