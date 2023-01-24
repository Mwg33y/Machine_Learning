#Thompson sampling

#A car company makes 10 different ads fora car that they have manufactured. 
#Instead of posting all ten ads, they want to save money on advertising
#and only post one of the ads. They what to find out which ad will have the 
#highest click through rate. The will post the ad with the highest rate to a 
#social media platform for users to see. The hope is to get the highest amount 
#of engagement and to maximize potential number of customers for their product.

#Thompson sampling applies the Bernoulli distribution to Bayes theorem. The Bernoulli
#distribution says that a random variable can only have an outcome of 1 or 0. "p"
#is the probability of an outcome of 1, and "1-p" is the probability f an outcome 
#of 0. Bayes theorem deals with finding the probability of an event given that a
#previous related event took place. The randomness of the outcome event at hand
#has a uniform distribution, meaning a result of 1 or 0 are equally likely. But
#we would like our algorithm to make selection based on knowledge of prior events, in
#this case customer ad clicks: the ad having the highest number of clicks.

getwd()
setwd("C:/Users/Abdul/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_6_Reinforcement_Learning/Section_33_Thompson_Sampling/R")


#Import the advertisement dataset
dataset = read.csv('Ads_CTR_Optimisation.csv')

#Implementing UCB

N = 10000 #number of participants in UCB advertisement study
d = 10 #number of created by car manufacturer for UCB study


number_of_clicks = integer(d) #how many times a specific ad was selected after 
                               #exposure to all participants


number_of_times_ignored = integer(d) #how many times a specific ad was 
                                     #ignored after exposure to all participants


ads_selected = integer(0)#which ad was selected


total_rewards = 0 #how many ad selected in total

#the average reward for each advertisement put in a vector of size 10

N = 10000
d = 10


#how many times was each advertisement viewed according to the dataset
for(n in 1:N){
  
  #The ad with most selections thus far
  max_random = 0
  
  #save the advertisement that has the highest calculated average view number
  #for each participant
  ad = 0
  
  
  #check how many ads participant "n" has clicked        
  for(i in 1:d){
    
    #generate random ad selection using Thompson sampling
    random_beta = rbeta(n = 1, #one random result per ad
                        shape1 = number_of_clicks[i] + 1,
                        shape2 = number_of_times_ignored[i] + 1
                       )
    
    
    if(random_beta > max_random){
      #most clicks for an ad by all participants thus far
      max_random = random_beta
      
      #index which ad achieved the most clicks up to now
      ad = i
    }
  }
  #number of times a specific ad has been select thus far
  ads_selected = append(ads_selected,ad)
  
  
  
  #record a '1' in reward data frame for the ad that was clicked by participant 'n'
  #a '0' is representative of an ignored advertisement
  reward = dataset[n,ad]
  
  #update date advertisement engagement after each participant
  if(reward == 1){
    number_of_clicks[ad] = number_of_clicks[ad] + 1
  }
  else{
    number_of_times_ignored[ad] = number_of_times_ignored[ad] + 1
    }
  
  #total count of ad selection of each ad by all participants
  total_rewards = total_rewards + reward
  
}


#visualize the results of Thompson sampling applied to the dataset

hist(ads_selected,
     col='blue',
     main = 'Histogram of ads selections using Thompson sampling',
     xlabs = 'Ads',
     ylabs = 'Number of times each ad was selected')














