#funtion for convert postal to full state names 
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("Alaska","Alabama","Arkansas","Arizona","California","Colorado",
                     "Connecticut","District of Columbia","Delaware","Florida","Georgia",
                     "Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas","Kentucky",
                     "Louisiana","Massachusetts","Maryland","Maine","Michigan","Minnesota",
                     "Missouri","Mississippi","Montana","North Carolina","North Dakota",
                     "Nebraska","New Hampshire","New Jersey","New Mexico","Nevada",
                     "New York","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico",
                     "Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
                     "Utah","Virginia","Vermont","Washington","Wisconsin",
                     "West Virginia","Wyoming"))
  )
  st.x<-data.frame(state=x)
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  return(refac.x)
}


#vector wrapper function 
vectorThis <- function(Vx,Vy){
  return(c(X=Vx,Y=Vy)) 
}

#SIR wrapper
yearSIR <- function( initials, parameters) {
  modelResult <- as_tibble(SIRInducedMortality2(pars = parameters,  #the recovery rate,
                                                init = initials,
                                                time = 0:365)$results ) 
  
  return(modelResult)
  # #calculate total pop at initial (t=0)
  # total_pop <- sum(modelResult[1,2:4])
  # 
  # modelResult <- modelResult %>% 
  #   mutate(total_sickened = Y + Z,
  #          total_dead = total_pop - (X + Y + Z)
  #   )
  # 
  # return(modelResult)
}
