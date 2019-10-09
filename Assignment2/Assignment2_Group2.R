library(dplyr)
library(reshape)

# Load data
judge_withna_df<- read.csv("Judge.csv", header = TRUE)

# Ignoring null values
judge_df <- na.omit(judge_withna_df)

# Splitting into separate dataframes based on the typee of court
common_df <- judge_df[judge_df$Court == "Common",  ] 
domestic_df <- judge_df[judge_df$Court == "Domestic",  ] 
muni_df <- judge_df[judge_df$Court == "Muni",  ] 

# Problem 1 a)  probability of cases being appealed and reversed in the three different courts
mlt <- melt(Judge.df, id =c("Court"), measure = c("Disposed","Appealed","Reversed"))
cstCourt.df <-cast(mlt,Court ~ variable,sum)
cstCourt.df$probAppealed <-cstCourt.df$Appealed/cstCourt.df$Disposed
cstCourt.df$probReversed <-cstCourt.df$Reversed/cstCourt.df$Disposed
print(cstCourt.df)

# Problem 1 Function to find the probability of the following and ranking of the judges
# b) Appealed cases 
# c) Reversed cases
# d) Reversal given an appeal 
# e) Ranking

func_judge_summary <- function(df) {
  return(
    group_by(df, Judge)  %>% 
      summarise(
        Disposed=sum(Disposed), 
        Appealed=sum(Appealed), 
        Reversed=sum(Reversed),
        AppealedProb = sum(Appealed)/sum(Disposed),
        ReversedProb = sum(Reversed)/sum(Disposed),
        ReversalGivenAppealProb = sum(Reversed)/sum(Appealed)
      ) %>%
      arrange(ReversedProb, AppealedProb) %>%
      data.frame()
  )
}

 resultCommonDF <- func_judge_summary(common_df)
 resultCommonDF 
 resultDomesticDF <- func_judge_summary(domestic_df)
 resultDomesticDF 
 resultMunicipalDF <- func_judge_summary(muni_df)
 resultMunicipalDF
 


# Problem 2 a) Given
PrA1 <-   0.05       #Probability(Default)
PrA2 <- 1 - PrA1     #Probability(NotDefault)
PrB_given_A1 <- 1          #Probabiltiy(MissingMonthlyPayment | Default)
PrB_given_A2 <- 0.20       #Probability(MissingMonthlyPayment | NotDefault)

# Probability(Default | MissingMonthlyPayment) using BayesTheorem
# P( A1 | B)  =    	P(A1) P(B | A1) 
#              _________________________
#              P(A1) P(B | A1) + P(A2) P(B | A2)

PrA1_given_B =  (PrA1 * PrB_given_A1) / ((PrA1 * PrB_given_A1) + (PrA2 * PrB_given_A2))
cat(sprintf(" Question (2.a) Probability a Customer will default , given that a customer missed one or more monthly payments 
: %f \n", PrA1_given_B))


# Problem 2 b)
cat(sprintf(" Question (2.b) Since the Probability of a Customer will default , given that a customer missed one or more monthly payments 
is %f which is greater than 0.20 the bank should recall the card from the cutomer \n", PrA1_given_B))




# Problem 3 a) Probability that a weekly sample will result in a shutdown of production if the production 
# process is working properly
1 -pbinom(4,size=25,prob=0.08)

# Problem 3 b)Atleat 7 boxes must fail to meet the standard weight of bananaflavored marshmallows
# so that the management shut down the production of Go Bananas no more than 1%

1-pbinom(6,size=25, prob=0.08)


# Problem 3 c) percentage(5.4%) of 16-ounce boxes to reduce the probability at least five of the sampled boxes 
# that fail to meet the standard to .01 or less 
get_prob_for_givenPbinom <- function(expected_pbinom)
{
  Prob_value <-0.08   
  while (Prob_value >= 0)
  {
    value <- (1-pbinom(4,size =25, prob =Prob_value))
    if (value <=expected_pbinom)
    {
      result <- Prob_value
      break
    }
    Prob_value =(Prob_value -.001)
  }
  result
  
}
get_prob_for_givenPbinom(.01)


# Problem 4 a) Probability that a household views television more than 3 hours a day?
1-pnorm(3 ,8.35 ,2.5)


# 4 b) probability that a household spends 5 – 10 hours watching television more a day
pnorm(10 ,8.35 ,2.5) - pnorm(5 ,8.35 ,2.5)


# 4 c) Hours of television viewing a household must have in order 
# to be in the top 3% of all television viewing households

qnorm(0.97, 8.35, 2.5)







