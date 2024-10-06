*************************
* Do File for estimating the effect of coups on trade

clear all
set more off

cd "C:\Users\ccard\Dropbox\african_coups"

import delimited C:\Users\ccard\Dropbox\african_coups\data\coups\cam_wide_30.csv

kountry cowcode , geo(cow) from(cown)
keep if GEO=="Africa"
keep year coup1 cowcode country
keep if inrange(year, 1970,2010)
replace coup1="0" if coup1=="NA"
destring coup1, replace
kountry cowcode , from(cown) to(iso3c)
rename _ISO3C_ iso3c


encode iso3c, gen(iso3_n)
xtset iso3_n year

forvalues i=1/10{

gen lcoup`i'=l`i'.coup1
gen fcoup`i'=f`i'.coup1
}



tempfile coups
save `coups'



* Trade Data

use "data\gravity_trade\Gravity_V202102.dta", clear
keep if inrange(year, 1970,2010)


preserve
keep iso3_d
duplicates drop
kountry iso3_d , geo(cow) from(iso3c)
tempfile africa
save `africa'
restore


* All merged
merge m:1 iso3_d using `africa', nogen
keep if GEO=="Africa"




keep year contig tradeflow_comtrade_d tradeflow_imf_d iso3_d tradeflow_comtrade_o tradeflow_imf_o

collapse (sum) tradeflow_comtrade_d tradeflow_imf_d tradeflow_comtrade_o tradeflow_imf_o, by(contig year iso3_d)
sort iso year

replace contig=0 if contig==.
rename iso3_d iso3c

merge m:1 iso3c year using `coups'



foreach i in tradeflow_comtrade_d tradeflow_imf_d tradeflow_comtrade_o tradeflow_imf_o{
gen l_`i'=log(1+`i')
reghdfe l_`i' i.coup1##i.contig, absorb(year iso3c) cluster(iso3c)
} 


stop
reghdfe tradeflow_imf_d i.lcoup1##i.contig i.lcoup2##i.contig i.lcoup3##i.contig i.lcoup4##i.contig i.lcoup5##i.contig i.fcoup1##i.contig i.fcoup2##i.contig i.fcoup3##i.contig i.fcoup4##i.contig i.fcoup5##i.contig, absorb(year iso3c) cluster(iso3c)



didregress (tradeflow_imf_d) (coup1), group(iso3c) time(year)



