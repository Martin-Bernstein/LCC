// clear
// cls
//
// * Load first year (1979)
// use "https://data.nber.org/morg/annual/morg79.dta"
// gen year=
// * 1980–1999
// forvalues y = 80/99 {
//     append using "https://data.nber.org/morg/annual/morg`y'.dta"
// }
//
// * 2000–2024 with leading zero padding
// forvalues y = 0/9 {
//     local yy ="0" + `y'
//     append using "https://data.nber.org/morg/annual/morg`yy'.dta"
// }
// forvalues y = 10/24 {
//     append using "https://data.nber.org/morg/annual/morg`y'.dta"
// }
//
//
//
// save "/Users/seyedmahdihosseinimaasoum/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Lichtinger, Guy - LCC/Data/CPS_raw",replace
//
//
//


******************************************* use 

clear
use "/Users/seyedmahdihosseinimaasoum/Library/CloudStorage/OneDrive-SharedLibraries-HarvardUniversity/Lichtinger, Guy - LCC/Data/CPS_raw"


keep year hhid state ind70 ind80 ind02 ind17  occ70 docc70 occ80 docc80 occ802 occ2011 occ2012 occ18 weight earnwt cmpwgt earnwtp weightp earnhr uearnwk earnwt earnhre earnwke









