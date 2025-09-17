*****************************
* 							*
*		Regressions			*
*							*
*****************************

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

use "CreatedData/dataset_landuse_V2",clear

order codmpio year, first
sort codmpio year
*gen d_indigena = (PUEBLO!="")
*gen d_2010 = (por_inund_area_muni>=0.1)

* ---------------- Descriptive Statistics ------------------ * 
sum muni_mean_precipitation mean_all sd_all mean_before2000 sd_before2000 mean_t_period sd_t_period weighted_mean_all weighted_sd_all weighted_mean_before2000 weighted_sd_before2000 weighted_mean_t_period weighted_sd_t_period /*p_dec_trees p_dec_grass p_dec_crops*/ p_max_trees p_max_grass p_max_crops /*p_max_flooded*/ deviation_mean_all w_deviation_mean_all deviation_mean_pre2000 w_deviation_mean_pre2000 deviation_mean_t_period w_deviation_mean_t_period max_viewed /*dec_viewed*/ por_inund_area_muni por_area_sem_ciclo_PERMANENTE por_area_sem_ciclo_TRANSITORIO d_indigena d_2010

eststo DS: estpost summarize muni_mean_precipitation mean_all sd_all mean_before2000 sd_before2000 mean_t_period sd_t_period weighted_mean_all weighted_sd_all weighted_mean_before2000 weighted_sd_before2000 weighted_mean_t_period weighted_sd_t_period /*p_dec_trees p_dec_grass p_dec_crops*/ p_max_trees p_max_grass p_max_crops/*p_max_flooded*/ deviation_mean_all w_deviation_mean_all deviation_mean_pre2000 w_deviation_mean_pre2000 deviation_mean_t_period w_deviation_mean_t_period max_viewed /*dec_viewed*/ por_inund_area_muni por_area_sem_ciclo_PERMANENTE por_area_sem_ciclo_TRANSITORIO d_indigena d_2010
esttab . using "Slides/Tables/descriptive_statistics_V2.tex", cells("mean sd count min max") replace
eststo clear // por_inund_area_muni need manual change from R results on code 05 bith dataset 'temp'

* ---------------- Regressions ---------------- *
***** ------------- Table 2a: TWFE ------------- ***** 
* TWFE - max view *
// Using all precipitation data available to mean measure
eststo TWFE_max_tree_all: reghdfe p_max_trees t_dummy_all, abs(codmpio year) vce(robust) nocons
eststo TWFE_max_grass_all: reghdfe p_max_grass t_dummy_all, abs(codmpio year) vce(robust) nocons
eststo TWFE_max_crops_all: reghdfe p_max_crops t_dummy_all, abs(codmpio year) vce(robust) nocons
// Using pre2000 precipitation data available to mean measure
eststo TWFE_max_tree_pre2000: reghdfe p_max_trees t_dummy_pre2000, abs(codmpio year) vce(robust) nocons
eststo TWFE_max_grass_pre2000: reghdfe p_max_grass t_dummy_pre2000, abs(codmpio year) vce(robust) nocons
eststo TWFE_max_crops_pre2000: reghdfe p_max_crops t_dummy_pre2000, abs(codmpio year) vce(robust) nocons
// Using t_period precipitation data available to mean measure
eststo TWFE_max_tree_t_period : reghdfe p_max_trees t_dummy_t_period , abs(codmpio year) vce(robust) nocons
eststo TWFE_max_grass_t_period : reghdfe p_max_grass t_dummy_t_period , abs(codmpio year) vce(robust) nocons
eststo TWFE_max_crops_t_period : reghdfe p_max_crops t_dummy_t_period , abs(codmpio year) vce(robust) nocons

* Exporting table *
esttab TWFE_max_tree_all TWFE_max_grass_all TWFE_max_crops_all  TWFE_max_tree_pre2000 TWFE_max_grass_pre2000  TWFE_max_crops_pre2000  TWFE_max_tree_t_period TWFE_max_grass_t_period  TWFE_max_crops_t_period  /*TWFE_dec_tree TWFE_dec_grass TWFE_dec_crops*/ using "Slides/Tables/TWFE_V2.tex", title(" TWFE estimation") coeflabel("Treatment") se r2 ar2 replace
eststo clear
*************
***** ------------- Table 2b: Callaway Sant' Anna  ------------- ***** 
set scheme s1color
*** No cxontrolls ***

// Using all precipitation data available to mean measure
* Trees
csdid p_max_trees, ivar(codmpio) time(year) gvar(t_group_all) asinr
estat event, estore(cs1_all)
estat simple, estore(cs_sim1_all)
* Grass
csdid p_max_grass, ivar(codmpio) time(year) gvar(t_group_all) asinr
estat event, estore(cs2_all)
estat simple, estore(cs_sim2_all)
* Crops
csdid p_max_crops, ivar(codmpio) time(year) gvar(t_group_all) asinr
estat event, estore(cs3_all)
estat simple, estore(cs_sim3_all)

// Using pre2000 precipitation data available to mean measure
* Trees
csdid p_max_trees, ivar(codmpio) time(year) gvar(t_group_pre2000) asinr
estat event, estore(cs1_pre2000)
estat simple, estore(cs_sim1_pre2000)
* Grass
csdid p_max_grass, ivar(codmpio) time(year) gvar(t_group_pre2000) asinr
estat event, estore(cs2_pre2000)
estat simple, estore(cs_sim2_pre2000)
* Crops
csdid p_max_crops, ivar(codmpio) time(year) gvar(t_group_pre2000) asinr
estat event, estore(cs3_pre2000)
estat simple, estore(cs_sim3_pre2000)

// Using t_period precipitation data available to mean measure
* Trees
csdid p_max_trees, ivar(codmpio) time(year) gvar(t_group_t_period) asinr
estat event, estore(cs1_t_period)
estat simple, estore(cs_sim1_t_period)
* Grass
csdid p_max_grass, ivar(codmpio) time(year) gvar(t_group_t_period) asinr
estat event, estore(cs2_t_period)
estat simple, estore(cs_sim2_t_period)
* Crops
csdid p_max_crops, ivar(codmpio) time(year) gvar(t_group_t_period) asinr
estat event, estore(cs3_t_period)
estat simple, estore(cs_sim3_t_period)


esttab cs_sim1_all cs_sim2_all cs_sim3_all cs_sim1_pre2000 cs_sim2_pre2000 cs_sim3_pre2000 cs_sim1_t_period cs_sim2_t_period cs_sim3_t_period using "Slides/Tables/CS_estimation_V2.tex",coeflabel("ATT") title(" CS estimation") se replace
eststo clear

coefplot (cs1_all, label(Max Trees all) mcolor(dkgreen) ciopts(color(dkgreen%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs1_pre2000, label(Max Trees pre2000) mcolor(green) ciopts(color(green%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs1_t_period, label(Max Trees period) mcolor(midgreen) ciopts(color(midgreen%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) ///
 (cs2_all, label(Max Grass all) mcolor(cranberry) ciopts(color(cranberry%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs2_pre2000, label(Max Grass pre2000) mcolor(purple) ciopts(color(purple%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs2_t_period, label(Max Grass period) mcolor(magenta) ciopts(color(magenta%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) ///
 (cs3_all, label(Max Crops all) mcolor(marron) ciopts(color(marron%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs3_pre2000, label(Max Crops pre2000) mcolor(olive) ciopts(color(olive%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs3_t_period, label(Max Crops period) mcolor(sienna) ciopts(color(sienna%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")), vertical yline(0) keep(Pre_avg Post_avg)
graph export "Slides/Figures/coef_plot_V2.png", replace

* ALL
event_plot cs1_all, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_trees_plot_allV2.png", replace

event_plot cs2_all, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_grass_plot_allV2.png", replace

event_plot cs3_all, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max__crops_allV2.png", replace

* Pre2000
event_plot cs1_pre2000, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_trees_plot_pre2000V2.png", replace

event_plot cs2_pre2000, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_grass_plot_pre2000V2.png", replace

event_plot cs3_pre2000, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max__crops_pre2000V2.png", replace

* Period
event_plot cs1_t_period, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_trees_plot_periodV2.png", replace

event_plot cs2_t_period, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_grass_plot_periodV2.png", replace

event_plot cs3_t_period, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max__crops_periodV2.png", replace

/*
*** Indigena control *** Don't aport anything FE capture it
// Using all precipitation data available to mean measure
* Trees
csdid p_max_trees, ivar(codmpio) time(year) gvar(t_group_all) asinr
estat event, estore(cs1_all)
estat simple, estore(cs_sim1_all)
* Grass
csdid p_max_grass d_indigena, ivar(codmpio) time(year) gvar(t_group_all) asinr
estat event, estore(cs2_all)
estat simple, estore(cs_sim2_all)
* Crops
csdid p_max_crops d_indigena, ivar(codmpio) time(year) gvar(t_group_all) asinr
estat event, estore(cs3_all)
estat simple, estore(cs_sim3_all)

// Using pre2000 precipitation data available to mean measure
* Trees
csdid p_max_trees d_indigena, ivar(codmpio) time(year) gvar(t_group_pre2000) asinr
estat event, estore(cs1_pre2000)
estat simple, estore(cs_sim1_pre2000)
* Grass
csdid p_max_grass d_indigena, ivar(codmpio) time(year) gvar(t_group_pre2000) asinr
estat event, estore(cs2_pre2000)
estat simple, estore(cs_sim2_pre2000)
* Crops
csdid p_max_crops d_indigena, ivar(codmpio) time(year) gvar(t_group_pre2000) asinr
estat event, estore(cs3_pre2000)
estat simple, estore(cs_sim3_pre2000)

// Using t_period precipitation data available to mean measure
* Trees
csdid p_max_trees d_indigena, ivar(codmpio) time(year) gvar(t_group_t_period) asinr
estat event, estore(cs1_t_period)
estat simple, estore(cs_sim1_t_period)
* Grass
csdid p_max_grass d_indigena, ivar(codmpio) time(year) gvar(t_group_t_period) asinr
estat event, estore(cs2_t_period)
estat simple, estore(cs_sim2_t_period)
* Crops
csdid p_max_crops d_indigena, ivar(codmpio) time(year) gvar(t_group_t_period) asinr
estat event, estore(cs3_t_period)
estat simple, estore(cs_sim3_t_period)


esttab cs_sim1_all cs_sim2_all cs_sim3_all cs_sim1_pre2000 cs_sim2_pre2000 cs_sim3_pre2000 cs_sim1_t_period cs_sim2_t_period cs_sim3_t_period using "Slides/Tables/CS_estimation_V2Xindigena.tex",coeflabel("ATT") title(" CS estimation") se replace
eststo clear

coefplot (cs1_all, label(Max Trees all) mcolor(dkgreen) ciopts(color(dkgreen%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs1_pre2000, label(Max Trees pre2000) mcolor(green) ciopts(color(green%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs1_t_period, label(Max Trees period) mcolor(midgreen) ciopts(color(midgreen%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) ///
 (cs2_all, label(Max Grass all) mcolor(cranberry) ciopts(color(cranberry%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs2_pre2000, label(Max Grass pre2000) mcolor(purple) ciopts(color(purple%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs2_t_period, label(Max Grass period) mcolor(magenta) ciopts(color(magenta%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) ///
 (cs3_all, label(Max Crops all) mcolor(marron) ciopts(color(marron%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs3_pre2000, label(Max Crops pre2000) mcolor(olive) ciopts(color(olive%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")) (cs3_t_period, label(Max Crops period) mcolor(sienna) ciopts(color(sienna%50)) rename(Pre_avg="Pre-treatment" Post_avg="Post-treatment")), vertical yline(0) keep(Pre_avg Post_avg)
graph export "Slides/Figures/coef_plot_V2Xindigena.png", replace

* ALL
event_plot cs1_all, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_trees_plot_allV2Xindigena.png", replace

event_plot cs2_all, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_grass_plot_allV2Xindigena.png", replace

event_plot cs3_all, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max__crops_allV2Xindigena.png", replace

* Pre2000
event_plot cs1_pre2000, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_trees_plot_pre2000V2Xindigena.png", replace

event_plot cs2_pre2000, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_grass_plot_pre2000V2Xindigena.png", replace

event_plot cs3_pre2000, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max__crops_pre2000V2Xindigena.png", replace

* Period
event_plot cs1_t_period, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_trees_plot_periodV2Xindigena.png", replace

event_plot cs2_t_period, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max_grass_plot_periodV2Xindigena.png", replace

event_plot cs3_t_period, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_max__crops_periodV2Xindigena.png", replace
*/

*** Chaisemartin ***
/* Install packages
ssc install gtools, replace
ssc install did_multiplegt_dyn, replace
ssc install did_multiplegt, replace
*/
* did_multiplegt_dyn
did_multiplegt_dyn p_max_trees codmpio year t_dummy_all, placebo(1)
did_multiplegt_dyn p_max_trees codmpio year t_dummy_all, /// effects(7)


did_multiplegt_dyn p_max_crops codmpio year t_dummy_all, placebo(1)
did_multiplegt_dyn p_max_crops codmpio year t_dummy_all, effects(7)

did_multiplegt_dyn p_max_grass codmpio year t_dummy_all, placebo(1)
did_multiplegt_dyn p_max_grass codmpio year t_dummy_all, effects(7)

* did_multiplegt
did_multiplegt p_max_trees codmpio year deviation_mean_all
/* *** OLD ***

* TWFE - december view *
*eststo TWFE_dec_tree: reghdfe p_dec_trees t_dummy, abs(codmpio year) vce(robust) nocons
*eststo TWFE_dec_grass: reghdfe p_dec_grass t_dummy, abs(codmpio year) vce(robust) nocons
*eststo TWFE_dec_crops: reghdfe p_dec_crops t_dummy, abs(codmpio year) vce(robust) nocons

* Callaway Sant' Anna *
/*
* Trees
csdid p_dec_trees, ivar(codmpio) time(year) gvar(t_group) 
estat event, estore(cs4)
estat simple, estore(cs_sim4)
* Grass
csdid p_dec_grass, ivar(codmpio) time(year) gvar(t_group) 
estat event, estore(cs5)
estat simple, estore(cs_sim5)
* Crops
csdid p_dec_crops, ivar(codmpio) time(year) gvar(t_group) 
estat event, estore(cs6)
estat simple, estore(cs_sim6)
*/

/*
event_plot cs4, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_dec_trees_plot.png", replace

event_plot cs5, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_dec_grass_plot.png", replace

event_plot cs6, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") xlabel(-6(1)6)) stub_lag(Tp#) stub_lead(Tm#) together
graph export "Slides/Figures/ES_dec_crops_plot.png", replace
*/