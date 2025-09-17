*****************************
* 							*
*		Regressions			*
*							*
*****************************
/*
ssc install csdid
ssc install drdid
*/

clear all
set more off

local usuario=c(username)

if "`usuario'"=="sebas"{
	global mainpath= "C:\Users\sebas\Dropbox\Documents\Proyecto_UsosDeTierra"
}
*else if{
*	global mainpath= ""
*}

cd "${mainpath}"

use "CreatedData/dataset_landuse.dta",clear

order codmpio year, first
sort codmpio year
gen p_max_viewed=total_analyzed_max/area_km2


*gen log_p_max_trees = log(p_max_trees)
*tw scatter p_max_trees sd_before2000
*tw scatter log_p_max_trees sd_before2000
*tw scatter log_max_trees sd_before2000

/// ------------------------------------------------------------------------------------------------------- ///
/// 											Labeling data												///
/// ------------------------------------------------------------------------------------------------------- ///
* Descriptive
lab var area_km2 "Total area"
lab var total_analyzed_max "Total analized area"
lab var trees_max "Maximum trees area"
lab var grass_max "Maximum grass area"
lab var crops_max "Maximum crops area"
lab var flooded_vegetation_max "Flooded vegetation area"
lab var muni_mean_precipitation "Municipal mean precipitation"
lab var mean_all "Mean precipitations registered" 
lab var sd_all "SD precipitation registered"
lab var mean_before2000 "Mean precipitations before 2000"
lab var sd_before2000 "SD precipitations before 2000" 
lab var mean_t_period "Mean precipitations 2015-2022"
lab var sd_t_period "SD precipitations 2015-2022"
lab var weighted_mean_all "Weighted mean precipitations registered"
lab var weighted_sd_all "SD weighted precipitation registered"
lab var weighted_mean_before2000 "Weighted mean precipitations before 2000"
lab var weighted_sd_before2000 "SD weighted precipitation before 2000"
lab var weighted_mean_t_period "Weighted mean precipitations 2015-2022"
lab var weighted_sd_t_period "SD weighted precipitations 2015-2022"
lab var p_max_viewed "Maximum protion of area analyzed"
lab var p_max_trees "Portion of trees in municipal area"
lab var p_max_grass "Portion of grass in municipal area"
lab var p_max_crops "Portion of crops in municipal area"
lab var pp_max_trees "Portion of trees in max. area analyzed"
lab var pp_max_grass "Portion of grass in max. area analyzed"
lab var pp_max_crops "Portion of crops in max. area analyzed"
lab var pp_max_flooded_veg "Portion of flooded vegetation in maximum area analyzed"
* Treatment
lab var t_dummy_all_1SD "Treatment by all precipitations registered 1SD"
lab var t_dummy_pre2000_1SD "Treatment by precipitations before 2000 1SD" 
lab var t_dummy_t_period_1SD "Treatment by precipitations 2015-2022 1SD"
lab var t_dummy_all_2SD "Treatment by all precipitations registered 2SD"
lab var t_dummy_pre2000_2SD "Treatment by precipitations before 2000 2SD"
lab var t_dummy_t_period_2SD "Treatment by precipitations 2015-2022 2SD"
lab var t_dummy_all_3SD "Treatment by all precipitations registered 3SD"
lab var t_dummy_pre2000_3SD "Treatment by precipitations before 2000 3SD"
lab var t_dummy_t_period_3SD "Treatment by precipitations 2015-2022 3SD"
lab var t_dummy_all_4SD "Treatment by all precipitations registered 4SD"
lab var t_dummy_pre2000_4SD "Treatment by precipitations before 2000 4SD"
lab var t_dummy_t_period_4SD "Treatment by precipitations 2015-2022 4SD"
* Deviation of the mean precipitation
lab var deviation_mean_all "Deviation of the mean precipitation registered" 
lab var deviation_mean_pre2000 "Deviation of the mean precipitation before 2000"
lab var deviation_mean_t_period "Deviation of the mean precipitation between 2015-22"

/// ------------------------------------------------------------------------------------------------------- ///
///									Table 1: Descriptive Statistics											///
/// -------------------------------------------------------------------------------------------------------	///
/// Area analyzed: 						total_analyzed_max trees_max grass_max crops_max		 			///
/// Percentage area analyzed: 			p_max_viewed p_max_trees p_max_grass p_max_crops 					///
/// Mean depend on period:				mean_all mean_before2000 mean_t_period muni_mean_precipitation		///
/// SD from mean distribution: 			sd_all sd_before2000 sd_t_period									///
/// Deviation of the mean: 				deviation_mean_all deviation_mean_pre2000 deviation_mean_t_period 	///
/// Weighted mean: 						weighted_mean_all weighted_mean_before2000 weighted_mean_t_period 	///
/// SD from weighted mean distribution: weighted_sd_all weighted_sd_before2000 weighted_sd_t_period 		///
/// 																										///
/// ------------------------------------------------------------------------------------------------------- ///

sum total_analyzed_max trees_max grass_max crops_max p_max_viewed p_max_trees p_max_grass p_max_crops mean_all mean_before2000 mean_t_period muni_mean_precipitation sd_all sd_before2000 sd_t_period deviation_mean_all deviation_mean_pre2000 deviation_mean_t_period weighted_mean_all weighted_mean_before2000 weighted_mean_t_period weighted_sd_all weighted_sd_before2000 weighted_sd_t_period

*** ---- Slides ---- ****
/*
eststo DS_1: estpost summarize total_analyzed_max trees_max grass_max crops_max // Area analyzed
esttab . using "Slides/Tables/desstats_1.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.2f))") label replace
eststo DS_2: estpost summarize p_max_viewed p_max_trees p_max_grass p_max_crops // Percentage area analyzed
esttab . using "Slides/Tables/desstats_2.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.0f))") label replace
eststo DS_3: estpost summarize mean_t_period mean_before2000 sd_before2000 deviation_mean_pre2000 weighted_mean_before2000 weighted_sd_before2000
esttab . using "Slides/Tables/desstats_3.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.0f))") label replace

*** ---- Paper ---- ****
eststo DS_1: estpost summarize total_analyzed_max trees_max grass_max crops_max // Area analyzed
esttab . using "Paper/Tables/desstats_1.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.2f))") label replace
eststo DS_2: estpost summarize p_max_viewed p_max_trees p_max_grass p_max_crops // Percentage area analyzed
esttab . using "Paper/Tables/desstats_2.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.0f))") label replace
eststo DS_3: estpost summarize mean_t_period mean_before2000 sd_before2000 deviation_mean_pre2000 weighted_mean_before2000 weighted_sd_before2000
esttab . using "Paper/Tables/desstats_3.tex", cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.0f))") label replace
*/

gen coddepto=(codmpio-mod(codmpio,1000))/1000
drop if muni_mean_precipitation==.

// --------------------------------------------- //
//				Regressions - TWFE				 //
// --------------------------------------------- // 
//						TWFE					 //
// --------------------------------------------- //
//												 //
// 		Portion of trees in municipal area		 //
//												 //
// --------------------------------------------- //
local SDs "1SD 2SD 3SD 4SD" // cluster coddepto
foreach var_reg in p_max_trees p_max_grass p_max_crops /*pp_max_trees pp_max_grass pp_max_crops trees_max grass_max crops_max log_max_trees log_max_grass log_max_crops*/{
	foreach SD of local SDs{
		// Using pre2000 precipitation data available to mean measure
		qui eststo `var_reg'_p00_`SD': reghdfe `var_reg' t_dummy_pre2000_`SD', abs(codmpio year) clu(coddepto)
		qui estadd scalar m_c= _b[_cons]
		qui unique codmpio if t_dummy_pre2000_`SD'==1
		qui estadd local munitreated = r(unique)
		qui unique codmpio
		qui estadd local totmunis = r(unique)
	}
	
	* ---------------- Exporting all regressions ---------------- *
	// Using pre2000 precipitation data available to mean measure
	* -- Slides -- *
	estout `var_reg'_p00_1SD `var_reg'_p00_2SD `var_reg'_p00_3SD `var_reg'_p00_4SD ///
	using "Slides/Tables/TWFE/TWFE_`var_reg'_p00_cludepto.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
	prefoot(\midrule ) ///
	stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
	labels ("Mean Dep.Var. Control" "Treated Munis" "Munis Analyzed"  "N. Obs" "\$R^2\$" )) replace
	* -- Paper --*
	estout `var_reg'_p00_1SD `var_reg'_p00_2SD `var_reg'_p00_3SD `var_reg'_p00_4SD ///
	using "Paper/Tables/TWFE/TWFE_`var_reg'_p00_cludepto.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
	prefoot(\midrule ) stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
	labels ("Mean Dep.Var. Control" "Munis treated" "Munis Analyzed" "N. Obs" "\$R^2\$" )) replace
}

* ---------------- Exporting final tables ---------------- *
* -- Slides -- *
estout p_max_trees_p00_2SD  p_max_grass_p00_2SD  p_max_crops_p00_2SD ///
using "Slides/Tables/TWFE_p00_cludepto_gen.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
prefoot(\midrule ) ///
stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
labels ("Mean Dep.Var. Control" "Treated Munis" "Munis Analyzed"  "N. Obs" "\$R^2\$" )) replace
* -- Paper --*
estout p_max_trees_p00_2SD  p_max_grass_p00_2SD  p_max_crops_p00_2SD  ///
using "Paper/Tables/TWFE_p00_cludepto_gen.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
prefoot(\midrule ) stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
labels ("Mean Dep.Var. Control" "Munis treated" "Munis Analyzed" "N. Obs" "\$R^2\$" )) replace
eststo clear

local SDs "1SD 2SD 3SD 4SD" // robust errors
foreach var_reg in p_max_trees p_max_grass p_max_crops /*pp_max_trees pp_max_grass pp_max_crops trees_max grass_max crops_max log_max_trees log_max_grass log_max_crops*/{
	foreach SD of local SDs{
		// Using pre2000 precipitation data available to mean measure
		qui eststo `var_reg'_p00_`SD': reghdfe `var_reg' t_dummy_pre2000_`SD', abs(codmpio year) vce(robust)
		qui estadd scalar m_c= _b[_cons]
		qui unique codmpio if t_dummy_pre2000_`SD'==1
		qui estadd local munitreated = r(unique)
		qui unique codmpio
		qui estadd local totmunis = r(unique)
	}
	
	* ---------------- Exporting all regressions ---------------- *
	// Using pre2000 precipitation data available to mean measure
	* -- Slides -- *
	estout `var_reg'_p00_1SD `var_reg'_p00_2SD `var_reg'_p00_3SD `var_reg'_p00_4SD ///
	using "Slides/Tables/TWFE/TWFE_`var_reg'_p00_rob.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
	prefoot(\midrule ) stats(m_c munitreated totmunis N r2 , fmt(%6.3f %3.0f %3.0f %6.0f a2 %11.2f) ///
	labels ("Mean Dep.Var. Control" "N. Obs" "\$R^2\$" )) replace
	* -- Paper --*
	estout `var_reg'_p00_1SD `var_reg'_p00_2SD `var_reg'_p00_3SD `var_reg'_p00_4SD ///
	using "Paper/Tables/TWFE/TWFE_`var_reg'_p00_rob.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
	prefoot(\midrule ) stats(m_c munitreated totmunis N r2 , fmt(%6.3f %3.0f %3.0f %6.0f a2 %11.2f) ///
	labels ("Mean Dep.Var. Control" "N. Obs" "\$R^2\$" )) replace
}
* ---------------- Exporting final tables ---------------- *
* -- Slides -- *
estout p_max_trees_p00_2SD  p_max_grass_p00_2SD  p_max_crops_p00_2SD ///
using "Slides/Tables/TWFE_p00_rob_gen.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
prefoot(\midrule ) ///
stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
labels ("Mean Dep.Var. Control" "Treated Munis" "Munis Analyzed"  "N. Obs" "\$R^2\$" )) replace
* -- Paper --*
estout p_max_trees_p00_2SD  p_max_grass_p00_2SD  p_max_crops_p00_2SD  ///
using "Paper/Tables/TWFE_p00_rob_gen.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) d(_cons) ///
prefoot(\midrule ) stats(m_c munitreated totmunis N r2 , fmt(%3.2f %3.0f %3.0f %6.0f a2 %11.2f) ///
labels ("Mean Dep.Var. Control" "Munis treated" "Munis Analyzed" "N. Obs" "\$R^2\$" )) replace
eststo clear

/// --------------------------------------------------- ///
///			Regressions - Callaway Sant' Anna			///
/// ---------------------------------------------------	///
set scheme s1mono
/*
When use the deviation of the historic mean using period between 2015-2022 
*/
local SDs "1SD 2SD 3SD 4SD"
foreach var_reg in p_max_trees p_max_grass p_max_crops /*pp_max_trees pp_max_grass pp_max_crops*/{
	foreach SD of local SDs{
		// Using pre2000 precipitation data available to mean measure
		csdid `var_reg', ivar(codmpio) time(year) gvar(t_group_pre2000_`SD') cluster(coddepto) agg(group) notyet
		estat event, estore(cs_`var_reg'_p00_`SD')
		estat simple, estore(cs_sim_`var_reg'_p00_`SD')
		estat group, estore(cs_gp_`var_reg'_p00_`SD')
		estat calendar, estore(cs_cal_`var_reg'_p00_`SD')
		estat event, window(-4 6)
		csdid_plot
		/*
		event_plot cs_`var_reg'_p00_`SD', default_look  ///
		graph_opt(xtitle("Periods since the event") ytitle("Average effect") ///
		xlabel(-6(1)6)) stub_lag(Tm4#) stub_lead(Tm#) ciplottype(rcap) together
		*/
		graph export "Slides/Figures/CS/ES_`var_reg'_p00_`SD'.pdf", replace
		graph export "Paper/Figures/CS/ES_`var_reg'_p00_`SD'.pdf", replace
	}	
}


// ------------------------------------------------------- //
// 				Export table: main results				   //
// ------------------------------------------------------- //
// only ATT
local SDs "1SD 2SD 3SD 4SD"
foreach SD of local SDs{
	estout cs_sim_p_max_trees_p00_`SD' cs_sim_p_max_grass_p00_`SD' cs_sim_p_max_crops_p00_`SD' /// 
	using "Slides/Tables/CS/CS_estimation_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	estout  cs_sim_p_max_trees_p00_`SD' cs_sim_p_max_grass_p00_`SD' cs_sim_p_max_crops_p00_`SD' ///
	using "Paper/Tables/CS/CS_estimation_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// With lags
local SDs "1SD 2SD 3SD 4SD"
foreach SD of local SDs{
	estout cs_p_max_trees_p00_`SD' cs_p_max_grass_p00_`SD' cs_p_max_crops_p00_`SD' /// 
	using "Slides/Tables/CS/CS_estimationES_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	estout  cs_p_max_trees_p00_`SD' cs_p_max_grass_p00_`SD' cs_p_max_crops_p00_`SD' ///
	using "Paper/Tables/CS/CS_estimationES_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// Group analysis
local SDs "1SD 2SD 3SD 4SD"
foreach SD of local SDs{
	estout cs_gp_p_max_trees_p00_`SD' cs_gp_p_max_grass_p00_`SD' cs_gp_p_max_crops_p00_`SD' /// 
	using "Slides/Tables/CS/CS_estimation_gp_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	estout cs_gp_p_max_trees_p00_`SD' cs_gp_p_max_grass_p00_`SD' cs_gp_p_max_crops_p00_`SD' ///
	using "Paper/Tables/CS/CS_estimation_gp_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// Calendar analysis
local SDs "1SD 2SD 3SD 4SD"
foreach SD of local SDs{
	estout cs_cal_p_max_trees_p00_`SD' cs_cal_p_max_grass_p00_`SD' cs_cal_p_max_crops_p00_`SD' /// 
	using "Slides/Tables/CS/CS_estimation_cal_`SD'.tex",style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
	estout cs_cal_p_max_trees_p00_`SD' cs_cal_p_max_grass_p00_`SD' cs_cal_p_max_crops_p00_`SD' ///
	using "Paper/Tables/CS/CS_estimation_cal_`SD'.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
	label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) replace
}

// ---------------------------------- //
//				Graphics			  //
// ---------------------------------- //
local SDs "1SD 2SD 3SD 4SD"
foreach SD of local SDs{
	preserve
		set scheme plotplain
		duplicates drop codmpio,force
		keep if t_group_pre2000_`SD'>0
		graph bar (count), over(t_group_pre2000_`SD') blabel(bar) ytitle("# municipalities treated")
		graph export "Slides/Figures/treatment_count`SD'.pdf", replace
		graph export "Paper/Figures/treatment_count`SD'.pdf", replace
	restore
}

/* OLD version
local SDs "1SD 2SD 3SD 4SD"
foreach SD of local SDs{
	esttab cs_sim_p_max_trees_p00_`SD' cs_sim_p_max_grass_p00_`SD' cs_sim_p_max_crops_p00_`SD' using "Slides/Tables/CS_estimation_`SD'.tex",coeflabel("ATT") title(" CS estimation") se replace
	esttab cs_sim_p_max_trees_p00_`SD' cs_sim_p_max_grass_p00_`SD' cs_sim_p_max_crops_p00_`SD' using "Paper/Tables/CS_estimation_`SD'.tex", coeflabel("ATT") title(" CS estimation") se replace	
}
*/

foreach var_reg in p_max_trees p_max_grass p_max_crops {
	coefplot ///
	(cs_`var_reg'_p00_1SD, label(1SD) mcolor(dkgreen) ciopts(color(dkgreen%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) /// 
	(cs_`var_reg'_p00_2SD, label(2SD) mcolor(blue) ciopts(color(blue%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) ///
	(cs_`var_reg'_p00_3SD, label(3SD) mcolor(red) ciopts(color(red%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) ///
	(cs_`var_reg'_p00_4SD, label(4SD) mcolor(purple) ciopts(color(violet%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")), ///
	vertical yline(0) keep(Pre_avg Post_avg)
graph export "Slides/Figures/pre_pos_`var_reg'.pdf", replace
graph export "Paper/Figures/pre_pos_`var_reg'.pdf", replace
}
eststo clear

/// --------------------------------------------------- ///
///				Regressions - Chaisemartin				///
/// ---------------------------------------------------	///

/* Install packages
ssc install gtools, replace
ssc install did_multiplegt_dyn, replace
*/

* did_multiplegt_dyn p_max_trees codmpio year t_dummy_pre2000_3SD, effects(7) design(0.5,"console") normalized effects_equal placebo(2)
est clear
foreach var_reg in p_max_trees p_max_grass p_max_crops {
	local SDs "1SD 2SD 3SD 4SD"
	foreach SD of local SDs{
		* Making regressions
		 /// 
		did_multiplegt_dyn `var_reg' codmpio year t_dummy_pre2000_`SD', effects(7) placebo(2) ///
		effects_equal cluster(coddepto) ci_level(95) graph_off
		estadd scalar p_joint = e(p_equality_effects)
		estadd scalar p_placebo = e(p_jointplacebo)
		eststo model_`var_reg'_`SD'
		* Saving plots 
		event_plot e(estimates)#e(variances), default_look  ///
			graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
			xlabel(-2(1)7)) stub_lag(Effect_#) stub_lead(Placebo_#) together ciplottype(rcap)	
		graph export "Slides/Figures/multiplegt_dyn_`var_reg'_`SD'.png", replace
		graph export "Paper/Figures/multiplegt_dyn_`var_reg'_`SD'.png", replace
		
	}
}
/// ------------------------ Complete Table ------------------------------ ///
* --- Slides --- *
estout model_p_max_trees_2SD model_p_max_grass_2SD model_p_max_crops_2SD ///
using "Slides/Tables/multiplegt_dyn.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) ///
prefoot(\midrule ) stats(p_joint p_placebo, fmt(a2 a2)) ///
/*labels ("P-joint" "P-placebo"))*/ replace
* --- Paper --- *
estout model_p_max_trees_2SD model_p_max_grass_2SD model_p_max_crops_2SD ///
using "Paper/Tables/multiplegt_dyn.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) ///
prefoot(\midrule ) stats(p_joint p_placebo, fmt(a2 a2)) ///
/*labels ("P-joint" "P-placebo"))*/ replace

/// ------------------------ Average Effects ------------------------------ ///
* --- Slides --- *
estout model_p_max_trees_2SD model_p_max_grass_2SD model_p_max_crops_2SD ///
using "Slides/Tables/multiplegt_dyn.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) ///
keep(Av_tot_eff) prefoot(\midrule ) stats(p_joint p_placebo, fmt(a2 a2)) ///
/*labels ("P-joint" "P-placebo"))*/ replace
* --- Paper --- *
estout model_p_max_trees_2SD model_p_max_grass_2SD model_p_max_crops_2SD ///
using "Paper/Tables/multiplegt_dyn_gen.tex", style(tex) starl(* 0.10 ** 0.05 *** 0.01) /// 
label cells(b(star fmt(a2)) se(par fmt(a2))) mlabels(none) collabels(none) ///
keep(Av_tot_eff) prefoot(\midrule ) stats(p_joint p_placebo, fmt(a2 a2)) ///
/*labels ("P-joint" "P-placebo"))*/ replace
est clear

did_multiplegt_dyn p_max_trees codmpio year t_dummy_pre2000_3SD, effects(7) effects_equal placebo(2) cluster(coddepto) ci_level(95) graph_off

event_plot e(estimates)#e(variances), default_look  ///
	graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
	xlabel(-2(1)7)) stub_lag(Effect_#) stub_lead(Placebo_#) together ciplottype(rcap)
graph export "Slides/Figures/pre_pos_`var_reg'.png", replace
graph export "Paper/Figures/pre_pos_`var_reg'.png", replace


did_multiplegt_dyn p_max_trees codmpio year t_dummy_pre2000_3SD, placebo(1) cluster(coddepto) ci_level(95) graphoptions(yline(0))
did_multiplegt_dyn p_max_trees codmpio year t_dummy_pre2000_2SD, effects(7) cluster(coddepto) ci_level(95) graphoptions(yline(0))


did_multiplegt_dyn p_max_crops codmpio year t_dummy_pre2000_1SD, placebo(1) cluster(coddepto) ci_level(95)
did_multiplegt_dyn p_max_crops codmpio year t_dummy_pre2000_1SD, effects(7) cluster(coddepto) ci_level(95)

did_multiplegt_dyn p_max_grass codmpio year t_dummy_pre2000_1SD, placebo(1) cluster(coddepto) ci_level(95)
did_multiplegt_dyn p_max_grass codmpio year t_dummy_pre2000_1SD, effects(7) cluster(coddepto) ci_level(95)


eststo clear



