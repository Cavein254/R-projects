#QUESTION 1
# Consider following values for the purpose of this project
iprice=10        #the stock price at t
return = 0.15    #expected annual stock return
volatility = 0.2 #the annualized volatility of the underlying stock
time = 1         #time in Years
steps = 100      #the number of steps involved in calculation
term = rnorm(1) #distribution term 


#SOLUTION
v_steps = c() #a vector to store each successive step
count = 0 #keeps track of number of steps
fprice = 0 #stock price at each time intervals

#A function to calculate the stock price at st + n
stock_price_at_t <- function(iprice = 10, returns = 0.15,
                             volatility =0.2,time =1,steps =100){
    while(steps > 0){
      
    #Calculate st + 1
    fprice = iprice * exp((
             ((return - (0.5*(volatility^2)))*(time/steps)) +
            (volatility * term * sqrt((time/steps)))))
    
    #Joining each st+1 step to a vector v_steps
    v_steps = append(v_steps,fprice,after = length(v_steps))
    
    iprice = fprice
    steps = steps - 1
    count = count + 1
    }
  message = paste("")
  print(message)
}