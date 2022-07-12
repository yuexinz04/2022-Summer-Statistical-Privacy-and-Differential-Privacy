
# 2022-Summer-Statistical-Privacy-and-Differential-Privacy
**Mentor: Ruobin(Robin) Gong; Collaborators: Leah Ghazali, Nami Jain, Yuexin Zhang**

This brach "kenny-et-al-replication" contains all codes and output plots for modifying kenny et al code. 

**Goals:**  
Section: Fit models and make plots --> modify "X" in the gam fit model  
Section: Plotting functions -->compare plots using fitted(m) VS the true difference(error= pop_das12 - pop_orig) as error

**Task List:**
 1. gam (error ~ s(hh)) --> use fitted(m) as error --> produce hh plot only  
 Rsq=0.07213481

 2. gam (error ~ s(white)) --> use fitted(m) as error -> produce non white plot only  
 Rsq=0.05341763

 3. gam(error~t2(turnout, dem, log(dens))) -->use fitted(m) as error --> produce both hh and non white plot
 Rsq=0.09072936

 4. gam (error ~ s(hh)) --> use true diff as error --> produce hh plot only

 5. gam (error ~ s(white)) --> use true diff as error -> produce non white plot only

 6. gam(error ~ t2(turnout, dem, log(dens)) + s(white) + s(hh), data = d) --> use true diff as error  
 Rsq=0.1090283 
 
 
 
