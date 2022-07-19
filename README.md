
# 2022-Summer-Statistical-Privacy-and-Differential-Privacy
**Mentor: Ruobin(Robin) Gong; Collaborators: Leah Ghazali, Nami Jain, Yuexin Zhang**

This brach "kenny-et-al-replication" contains all codes and output plots for modifying kenny et al code. 

## Task 1~6
**Goals:**  
Section: Fit models and make plots --> modify "X" in the gam fit model  
Section: Plotting functions -->compare plots using fitted(m) VS the true difference(error= pop_das12 - pop_orig) as error

**Task List**
 1. gam (error ~ s(hh)) --> use fitted(m) as error --> produce hh plot only  
 Rsq=0.07213481

 2. gam (error ~ s(white)) --> use fitted(m) as error -> produce non white plot only  
 Rsq=0.05341763

 3. gam(error~t2(turnout, dem, log(dens))) -->use fitted(m) as error --> produce both hh and non white plot  
 Rsq=0.09072936

 4. gam (error ~ s(hh)) --> use true diff as error --> produce hh plot only  
 Rsq=0.07213481

 5. gam (error ~ s(white)) --> use true diff as error -> produce non white plot only  
 Rsq=0.05341763

 6. gam(error ~ t2(turnout, dem, log(dens)) + s(white) + s(hh), data = d) --> use true diff as error  
 Rsq=0.1090283 
 
## Add_Band
**Goals:**  
i) Bring _**uncertainty quantification**_ to the gam fit **line** in original kenny et al's graphs  
ii) _**Bootstrapping**_ tract level population error. Plot fitted perturbed error against original x-axis in original kenny et al's graphs

**Task List**
1. Modify ggplot()  --> Plot [Fitted Error Vs Non-White Percent] && [Fitted Error Vs HHI]
> {
geom_smooth(stat = "smooth",method = "gam", formula =y~s(x, bs = "cs"), color = "#222222",size = 0.45, alpha = 0.5,  
**se=TRUE, level=0.95**,fill='lightblue',)+
}

2. **Bootstrapping**  
  
2.1 Randomly perturb true_error(DP-SF)  
> {perturb_error = sample(x=(trueerror_vector), size=length(trueerror_vector), replace= FALSE) } 
   
2.2 Use Perturb_error as Y for computing the gam fit model
> {gam(Perturb_error ~ t2(turnout, dem, log(dens)) + s(white) + s(hh), data = d)}  
  
2.3 Plot "fitted perturb_error" against (non-white; HHI), with the 95% CI (Band)


 
 
