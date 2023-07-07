cd "C:\Users\Nicolas Taconet\Documents\GitHub\Mobility_Germany\Mobility_data"


*sqreg Total_emissions_wout_work i.Month i.Weekday i.Income i.Education i.HH_composition i.Age i.Employment i.Gender i.Location i.Migration_background i.Enjoy_Biking i.Enjoy_Car i.Enjoy_PublicTransport i.Car_ownership i.Second_Home i.Car_sharing, quantiles(50 75 90) reps(10)

eststo model1: qreg Total_emissions_wout_work i.Month i.Weekday i.Income i.Education i.HH_composition i.Age i.Employment i.Gender i.Location i.Migration_background i.Enjoy_Biking i.Enjoy_Car i.Enjoy_PublicTransport i.Car_ownership i.Second_Home i.Car_sharing

esttab model1, cells(b(star fmt(3))) qlist(0.5 0.75 0.9) title(Tableau de résultats) replace booktabs


*eststo model1



*esttab scalars (F df_m mss rmse r2 r2_a N) using table1a.tex, replace
*outreg2 addstat(mss_chi2,MSS) using hmhm.xls, bdec(5) replace
