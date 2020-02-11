# ---- dem ----

# Test sensitivity of biomass outcomes to demand and substitution.

# define wrapper function that takes parameters and returns scalar difference of counterfactual and status quo median biomass.
#  drop results where result > 0 or ... whatever works
fun_opt = function(par){
  
  out_0 = par %>% select(a) %>% fun %>% # some filtery and pulling stuff. a is the status quo parameters I guess
  out_1 = par %>% select(b) %>% fun %>% # some filtery and pulling stuff. b is the status quo parameters I guess
  abs(out_0 - out_1)
  
}

fun_opter = function(par){ # wait your parameters are all fucked, see the whale case to get a single scalar input
  
  optimize(f = fun_opt,
           lower = 0,
           upper = 50,
           maximum = FALSE,
           par = par) %>% 
    as.data.frame() %>% 
    select(minimum) %>% 
    pull()
  
}

# minimize function for a test set of parameters

# minimize function for whole set of parameters (sort out ranges for demand, substitution)
#  blow up test set into whole set
#   this seems like a nest and map situation



