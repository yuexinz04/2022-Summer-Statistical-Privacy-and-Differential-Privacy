library(here)

# load helpers ----
source(here("R/00_custom_functions.R"))

tract_data = read_csv("/Users/leahghazali/Desktop/nhgis_ppdd_20210428_12-2_tract/nhgis_ppdd_20210428_12-2_tract.csv")


dp_total = tract_data[,4]
dp_white = tract_data[,6]
dp_nonwhite = dp_total - dp_white
percent_nonwhite = 100*(dp_nonwhite/dp_total)
sf_total = tract_data[,305]

dp_total[1,]
dp_white[1,]
dp_nonwhite[1,]
percent_nonwhite[1,]
sf_total[1,]

error = dp_total - sf_total


# checked
al_percent_nonwhite = percent_nonwhite[1:1181,]
al_error = error[1:1181,]

al_percent_nonwhite[1]
al_percent_nonwhite[1181]
al_error[1]
al_error[1181]


# checked
de_percent_nonwhite = percent_nonwhite[13700:13917,]
de_error = error[13700:13917,]

de_percent_nonwhite[1]
de_percent_nonwhite[]
de_error[1]
de_error[]


# checked
la_percent_nonwhite = percent_nonwhite[28304:29451,]
la_error = error[28304:29451,]

la_percent_nonwhite[1]
la_percent_nonwhite[]
la_error[1]
la_error[]                # should = -10


# checked
nc_percent_nonwhite = percent_nonwhite[48115:50309,]
nc_error = error[48115:50309,]

nc_percent_nonwhite[1]
nc_percent_nonwhite[]
nc_error[1]
nc_error[]                # should = 1


# checked
pa_percent_nonwhite = percent_nonwhite[55347:58564,]
pa_error = error[55347:58564,]

pa_percent_nonwhite[1]    # should = 4.045
pa_percent_nonwhite[3218] # should = 3.005
pa_error[1]               # should = 16
pa_error[3218]            # should = 10


# checked
sc_percent_nonwhite = percent_nonwhite[58809:59911,]
sc_error = error[58809:59911,]

sc_percent_nonwhite[1]    # should = 14.434
sc_percent_nonwhite[1103] # should = 16
sc_error[1]               # should = 15.541
sc_error[1103]            # should = -7


# checked
ut_percent_nonwhite = percent_nonwhite[66896:67483,]
ut_error = error[66896:67483,]

ut_percent_nonwhite[1]    # should = 10.752
ut_percent_nonwhite[588]  # should = 7.237
ut_error[1]               # should = -14
ut_error[588]             # should = -64


# checked
wa_percent_nonwhite = percent_nonwhite[69575:71032,]
wa_error = error[69575:71032,]

wa_percent_nonwhite[1]    # should = 6.832
wa_percent_nonwhite[1458] # should = 71.367
wa_error[1]               # should = 56
wa_error[1458]            # should = 47


