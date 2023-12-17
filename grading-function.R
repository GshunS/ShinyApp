library(tidyverse)
calculate.grades <- function(group, individual, team = NULL){
    grading.functions <- list(
        
        hihi.A = function(group, individual){
            result <- c()
            for (i in 1:length(individual)){
                ini = group*(exp(0.03*(individual[i]-3.78)^5)*(log(individual[i]/3)+1.296)*exp(1/(individual[i]-7)))
                if (ini > 100) {
                    ini <- 100
                }
                result[i] <- round(ini,0)
            }
            return(result)
        },
        
        hihi.B = function(group, individual){
                                        #Helper function to --- calculate the coefficients
            ScoreModifier <- function(vector) {
                modified <- ifelse(vector >= 1 & vector < 2, 0.9 * vector - 0.9,
                            ifelse(vector >= 2 & vector <= 5, 0.1 * vector + 0.7, 0))
                return(modified)
            }
                                        # Helper function to --- check if all members got scores above 4
            checkallhighscore <- function(individual){
                result = TRUE
                for (x in individual){
                    if(x < 4){
                        result = FALSE
                        break
                    }
                }
                result
            }
                                        # Helper function to --- check any score under 2
            checkanylowscore <- function(individual){
                result = FALSE
                for (x in individual){
                    if(x <= 2){
                        result = TRUE
                        break
                    }
                }
                result
            }
                                        # Check if all members got 4 or above for peer appraisal
            if(checkallhighscore(individual)){
                                        # if yes return group score for individuals
                modefierResult = rep(1,length(individual))
            } else {
                                        # if no, calculate the coefficients
                modefierResult = ScoreModifier(individual)} 
            
                                        # modify the score by multiplying the group score by the coefficients
            CombinedScore = group * modefierResult
                                        # Check if anyone scored under 2 
            if (!checkallhighscore(individual) & checkanylowscore(individual)){
                for (x in 1: length(CombinedScore)){
                                        # To get those members scored over 3 for peer appraisal
                    if (CombinedScore[x] >= group){
                                        # Give 5% bonus to thos above 3 within the group
                        CombinedScore[x] = CombinedScore[x] + 5
                    }
                }
            }
                                        # Check no one exceeds 100%
            CombinedScore = pmin(CombinedScore,100)
                                        # Return the vector
            CombinedScore
        },
        
        hihi.C = function(group, individual, group.weight = 0.7){
                                        # Install (if needed) and load moments package
            if("moments" %in% rownames(installed.packages())) {
                library(moments)
            } else {
                install.packages("moments")
                library(moments)
            }
                                        # Calculate the standard deviation of individual scores
            sd_individual <- sd(individual)
                                        # Adjust the weights based on the standard deviation
            if (sd_individual > 1) {
                group.weight <- 0.6
            } else if (sd_individual < 0.5) {
                group.weight <- 0.8
            }
                                        # If group score is less than 40 or greater than 90, further adjust group weight
            if (group < 40 || group > 90) {
                group.weight <- 0.6
            }
                                        # If individual scores are not all the same, calculate skewness and adjust group weight if needed
            if (!all(individual == individual[1])) {
                skew_individual <- skewness(individual)
                if (abs(skew_individual) > 0.5) {
                    group.weight <- group.weight - 0.1
                }
            }
            
                                        # Calculate the individual weight
            individual.weight <- 1 - group.weight
                                        # Apply the weights
            group.contribution <- group * group.weight
            individual.contribution <- (individual / 5) * 100 * individual.weight
                                        # Calculate the final grades
            final.grades <- group.contribution + individual.contribution
                                        # Ensure that those scoring 1 do not score higher than 50
            final.grades[individual == 1] <- pmin(final.grades[individual == 1], 50)
                                        # In case of extreme variation in individual scores, further adjust grades
            if (max(individual) - min(individual) > 3) {
                final.grades[individual == max(individual)] <- pmin(final.grades[individual == max(individual)] + 5, 100)
                final.grades[individual == min(individual)] <- pmax(final.grades[individual == min(individual)] - 5, 0)
            }
                                        # Detach moments
            detach(package:moments)
                                        # Return the final grades
            return(final.grades)
        },
        
        hihi.D = function(group, individual, min_base = 1.05, slack_base = 1.25, max_add = 30, dp = 1){
            transform_mean <- function(original) {
                if(original < 1 || original > 5) {
                    return("Error.")
                }
                if(original < 2) {
                    new_mean <- (original-2)^2+2
                    return(new_mean - original)
                }
                if(original > 4) {
                    new_mean <- -(original-4)^2+4
                    return(new_mean - original)
                }
                return(0)
            }
            
            active_members <- function(scores) {
                sum(scores >= 2)
            }
            
            scaled_peer <- pmax(pmin(individual+transform_mean(mean(individual)), 5), 1)
            active <- active_members(scaled_peer)
            result <- numeric(length(individual))
            base <- min_base+0.05*(5-active)
            top <- group + max_add
            
            up_score <- scaled_peer[scaled_peer >= 3]
            result[scaled_peer >= 3] <- pmin(100, top-(max_add)*(base^(10*(5-up_score))-1)/(base^(10*(5-3))-1))
            
            neutral_score <- scaled_peer[(scaled_peer < 3) & (scaled_peer >= 2.5)]
            result[(scaled_peer < 3) & (scaled_peer >= 2.5)] <- group
            
            down_score <- scaled_peer[scaled_peer < 2.5]
            result[scaled_peer < 2.5] <- group-group*(slack_base^(10*(2.5-down_score))-1)/(slack_base^(10*(2.5-1))-1)
            
            return(round(result, dp))
        },
        
        hihi.E = function(group, individual){
                                        # set a vector name final_score to record final score for each member
            final_score <- rep(0, length(individual))
            
                                        # avoid scores too high or too low
            if(mean(individual)>=4){
                avg_individual <- mean(individual)
                scaling_factor <- 4 / avg_individual
                individual <- individual * scaling_factor
            }else if (mean(individual)<=2){
                avg_individual <- mean(individual)
                scaling_factor <- 2 / avg_individual
                individual <- individual * scaling_factor
            }
                                        #set a loop to calculate score of each member.
            for (i in 1:length(individual)){
                                        # if this person get lower than 1.75 points, decrease the final score with a sharper gradient wrt individual scores
                if (individual[i] < 1.75){
                    final_score[i] <- group + (individual[i]-2.12)*30
                }
                                        # if this person get lower than 2.5 points and higher or equal to 1.75 points,
                                        # decrease the final score with a less sharp gradient wrt individual scores
                else if (individual[i] >= 1.75 & individual[i] < 2.5){
                    final_score[i] <- group + (individual[i]-2.5)*15
                }
                                        # if this person get higher than 3.5 points and lower or equal to 4.25 points,
                                        # increase the final score with a less sharp gradient wrt individual scores
                else if (individual[i] > 3.5 & individual[i] <= 4.25){
                    final_score[i] <- group + (individual[i]-3.5)*15
                }
                                        # if this person get higher than 4.25 points, increase the final score with a sharper gradient wrt individual scores
                else if(individual[i] > 4.25){
                    final_score[i] <- group + (individual[i]-3.88)*30
                }
                                        # if this person get individual score between 2.5 to 3.5, the final score will equal to group score.
                else{final_score[i] <- group}
            }
                                        # There is situation a person may get more than 100 points, the final score will become 100.
            for (j in 1:length(final_score)){
                if (final_score[j] > 100){
                    final_score[j] <- 100
                }
            }
                                        # There is situation a person may get less than 0 points, the final score will become 0.
            for (j in 1:length(final_score)){
                if (final_score[j] < 0){
                    final_score[j] <- 0
                }
            }
            
                                        # ensure scoring ==1 does not exceed 50%
            for (j in 1:length(final_score)){
                if (individual[j]<=1){
                    final_score[j]=min(final_score[j],50)
                }
            }
            final_score
        },
        
        hihi.F = function(group, individual){
            result = c()
            if (length(individual)<=5){
                group = (((5- length(individual))*15/ 100)+1)*group
                                        # People who have less than 5 people get 20% extra grade for it.
            }

            individual[(individual>5)] = 5
            individual[(individual<1)] = 1
                                        # Any value greater than 5 will be reset to 5 and less than 1 will become
            regex_pattern <- "^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$"
            individual <- ifelse(grepl(regex_pattern, individual), individual, as.numeric(3))
            individual = as.numeric(individual)
                                        #Turns any non integer/float values into 3 (default)
            GoodGroupGrade = group
                                        #GoodGroupGrade is for the people who recieved two or above for their individual score
            
            
            if((length(which(individual>=1 & individual<2)))>=1){
                oneTotwo = sum(10*(2-(individual[which(individual>1 & individual<2)])))
                selfsabotaged = length(individual[which(individual==1)])
                TrashTeamBonus = 1+(((15*selfsabotaged)+ sum(oneTotwo))/100)


                GoodGroupGrade = TrashTeamBonus* group
                GoodGroupGrade[(GoodGroupGrade>100)] = 100
                                        # This only triggers if someone in the group recieved less than 2 marks and gives extra marks to those who got 2 or more
                                        #it gives 15% extra marks for each person who got 1 in the group
                                        #This also gives extra 1% for every accumulated 0.1 away from 2 excluding the people who got 1
            }
            group[(group > 100)] = 100
                                        #From the previous steps if your group grade becomes more than 100%, it resets it to 100%; same was done to GoodGroupGrade earlier

            for(k in 1:length(individual)){
                if (individual[k]<=5.0 && individual[k]>4){
                    result = append(result, (((150-((5-individual[k])*30))*GoodGroupGrade))/100)
                                        # For the values in the individual vector between 4 and 5, it takes 3% away for each 0.1 missing from 5
                                        # The highest mark for this range is 150% of the group and the lowest is 120%
                }
                if (individual[k]<=4.0 && individual[k]>3){
                    result = append(result, (((120-((4-individual[k])*20))*GoodGroupGrade))/100)
                                        # For the values in the individual vector between 3 and 4, it takes 2% away for each 0.1 missing from 4
                                        # The highest mark for this range is 120% of the group and the lowest is 100%
                }
                if (individual[k]<=3.0 && individual[k]>=2){
                    result = append(result, ((GoodGroupGrade* (100-(3-individual[k])*30))/100))
                                        # For the values in the individual vector between 3 and 2, it takes 3% away for each 0.1 missing from 3
                                        # The highest mark for this range is 100% of the group and the lowest is 70%
                }
                
                if (individual[k]<2.0 && individual[k]>=1){
                    result = append(result, (((70-((2-individual[k])*70))*group))/100)
                                        # For the values in the individual vector between 2 and 1, it takes 7% away for each 0.1 missing from 2
                                        # The highest mark for this range is 70% of the group and the lowest is 0%
                }
            }
            result[(result>100)] = 100

                                        #This checks for any value greater than 100% and resets it to 100%
            return(result)
        },
        
        hihi.G = function(group, individual){
            threshold = median(individual)
            n.students <- length(individual)
                                        # score one and that's it for those whom did not contribute 
            calculator = function(i){
                if (i == 1 & (sum(individual == 1) == 1)){
                    return(1)
                }
                ## scores calculation
                                        # calculate for exceeding (maximum bonus caps at 30%)
                if(i - threshold > 3){
                    
                    return(min((3/10 * group + group), 100))
                }
                                        # calculate for exceeding (maximum penalty limits at -30%)
                if(i - threshold < -3){
                    
                    return(max((-3/10 * group + group), 1))
                }
                                        # calculate the final score for individuals otherwise
                out = min((i - threshold)/10 * group + group, 100)
                return(max(out, 1))
            }
            output = vector()
            for(i in 1:n.students){
                output = c(output, calculator(individual[i]))
            }
            output
        },
        
        hihi.H = function(group, individual){
                                        # Validation of input arguments
            if (!is.numeric(group) || group < 0 || group > 100) {
                stop("Invalid value for `group` provided.")
            }
            if (!is.numeric(individual) || min(individual) < 1 || max(individual) > 5) {
                stop("Invalid value for `individual` provided.")
            }

            sigmoid <- plogis   # rename plogis to sigmoid function

                                        # Function to deduct points from underperformers
            f_0 <- function(grades) 14 * (grades - 3)^(3) + 4 * grades - 8

                                        # Function to award points to overperformers
            f_1 <- function(grades) 2.6^(grades - 2.5) + 4.5

                                        # Payoff function to combine the under- and over-performer functions
            payoff_func <- function(grades) {
                return((1 - sigmoid(grades)) * f_0(grades) +
                       sigmoid(grades) * f_1(grades) - 4)
            }

                                        # Initial grade
            grades <- group + payoff_func(individual)

                                        # Bound the grades to [0, 100]
            grades <- pmin(100, grades)
            grades <- pmax(0, grades)

                                        # Make sure individuals with a 1 score cannot get 50%
            grades[individual == 1] <- pmin(25, grades[individual == 1])

                                        # Round the grades to 2 decimal places
            output_grades <- ceiling(grades * 100) / 100

            return(output_grades)
        },
        
        hihi.I = function(group, individual){
            final_grades <- numeric(length(individual))
            for(i in seq_along(individual)) {
                individual_score <- individual[i]
                
                if (individual_score == 5) {
                    final_grades[i] <- group * 1.3
                } else if (individual_score >= 4.1 & individual_score < 5) {
                    final_grades[i] <- group * 1.2
                } else if (individual_score >= 3.1 & individual_score < 4.1) {
                    final_grades[i] <- group * 1.1
                } else if (individual_score >= 2.1 & individual_score < 3.1) {
                    final_grades[i] <- group
                } else if (individual_score >= 1.5 & individual_score < 2.1) {
                    final_grades[i] <- group * 0.9
                } else if (individual_score >= 1.1 & individual_score < 1.5) {
                    final_grades[i] <- group * 0.8
                }
                else {
                    final_grades[i] <- 0
                }
                final_grades[i] <- min(final_grades[i], 100)
            }
            return(final_grades)
        },
        
        hihi.J = function(group, individual){
            marks <- vector()
            
            for (i in 1:length(individual)){
                person <- round(individual[i] + 5*10^(-2), 1)
                
                if (1 <=person & person <= 1.5){
                    mark = group * 0.5
                } else if (1.6 <= person & person <= 2.0){
                    mark = group - 0.35 * group
                } else if (2.1 <= person & person <= 2.5){
                    mark = group - 0.2 *group
                } else if (2.6 <= person & person <= 2.9){
                    mark = group - 0.1 *group
                } else if(person == 3){
                    mark = group
                } else if (group <96 & 3.1 <= person & person <= 5.0){
                    mark = group + 5
                } else {
                    mark = 100
                }
                marks <- c(marks, mark)
            }
            marks
        },
        
        kaka.A = function(group, individual){
            l <- length(individual)
            mean <- mean(individual)
            a <- 0.01
            b <- -4
            c <- 0.008
            final <- numeric(length(individual))
            
            func1 <- function(diff, ind){(-a * exp(b * diff))/ind}
            func2 <- function(diff, ind){(c * diff * ind)}
            
            for (i in 1:l){
                ind <- individual[i]
                if(ind == 1){
                    final[i] <- 0
                    next
                }
                
                diff <- ind - mean
                
                if(ind >= mean -0.2 & ind <= mean + 0.2){
                    final[i] <- group
                    next
                }
                
                if(ind >= 2.8 & ind <=3.2){
                    final[i] <- group
                    next
                }
                
                if(diff < 0){
                    deduct <- func1(diff, ind)
                    final[i] <- max((1 + deduct) * group, 0)
                    next
                }
                
                if(diff > 0){
                    plus <- func2(diff, ind)
                    plus <- min(plus, 0.16)
                    final[i] <- min((group + 100 * plus), 100)
                    next
                }
                
                if(diff == 0){
                    final[i] <- group
                    next
                }
            }
            final
        },
        
        kaka.B = function(group, individual){
            library(formattable)
            if (!is.numeric(group) || group < 0 || group > 100) { 
                stop("'The group grade should be a numeric value between 0 and 100.")
            }
            
            
            if (!is.vector(individual) || any(!is.numeric(individual)) || any(individual < 1) || any(individual > 5)) { 
                return("'The individual grades should be a vector that contains numeric values between 1 and 5.")
            }
            
            plus = mean(individual)
            result = c()
            if(var(individual) == 0){for(i in 1:length(individual)){result = append(result, group)}}
            else{
                for(i in 1:length(individual)){
                    
                    if (individual[i] == "1"){result = append(result, 0)}
                    
                    else if ( "1" < individual[i] & individual[i] < "2"){result = append(result, (group - 4 * (3 - individual[i]) * plus))}
                    
                    else if ( "2" <= individual[i] & individual[i] < "3"){result = append(result, group - 2 * (3 - individual[i]) * plus)}
                    
                    else if ( "3" <= individual[i] & individual[i] <= "4"){result = append(result, group + (individual[i] - 3) * plus)}
                    
                    else if ( "4" < individual[i] & individual[i] <= "5"){result = append(result, group + 1.1*(individual[i] - 3) * plus)}
                    
                    result = replace(result, result > 100, 100)
                }}
            return(as.vector(formattable(result, digits = 2, format = "f")))
        },
        
        kaka.C = function(group, individual){
            final = integer(length(individual))
            gavg = mean(individual)
            for (i in 1:length(individual)) {
                ratio = individual[i] / gavg
                final[i] = group * sqrt(ratio)
                if (individual[i] ==1 ) {
                    final[i] = round(group*0.5)
                }
                
                if (final[i] > 100) {
                    final[i] = 100
                }
                
                final[i] = round(final[i])
            }
            
            return(final)
        },
        
        kaka.D = function(group, individual){
            if (any(individual < 1) || any(individual > 5)) {
                stop("Individual scores should be between 1 and 5.")
            }
            
            if (group < 0 || group > 100) {
                stop("Group grade must be between 0 and 100.")
            }
            
                                        # grade multipliers for each student grade
            grade.multipliers <- c(0, 0, 0.75, 0.9, 1, 1.1)
            
                                        # lower and upper bounds for each grade
            lower.bounds <- floor(individual)
            upper.bounds <- ceiling(individual)
            
                                        # fractional part to use in interpolation
            fractions <- individual - lower.bounds
            
                                        # linear interpolation
            final.grades <- (1 - fractions) * grade.multipliers[lower.bounds + 1] * group +
                fractions * grade.multipliers[upper.bounds + 1] * group
            
            final.grades <- pmin(final.grades, 100)
            return(round(final.grades, 2))
        },
        
        kaka.E = function(group, individual){
            final_grades <- vector("numeric", length(individual)) 
            if (group >100 || group <0 ){
                
                return("Warning:  Grade is out of range")
                
                
                
            }
            
            for (i in seq_along(individual)) {
                if(individual[i]<1 || individual[i]>5){
                    return("Warning:  Grade is out of range")
                    
                    
                }else if (individual[i] == 1 ) {
                    final_grades[i] <- 0
                } else {
                    a <- individual[i]/5 *0.4*100
                                        #print(a)
                    b <- group*0.6
                    final_grades[i] <- a+b
                    
                }
            }
            
            return(final_grades)        },
        
        kaka.F = function(group, individual){
            final_grade <- numeric(length(individual))  # Create a vector to store the results
            
            for (i in seq_along(individual)) {
                if (individual[i] >= 1 & individual[i] <= 1.5) {
                    final_grade[i] = 0
                } else if (individual[i] > 1.5 & individual[i] <= 1.75) {
                    final_grade[i] = 0.85 * group
                } else if (individual[i] > 1.75 & individual[i] <= 2.25) {
                    final_grade[i] = 0.9 * group
                } else if (individual[i] > 2.25 & individual[i] <= 2.75) {
                    final_grade[i] = 0.95 * group
                } else if (individual[i] > 2.75 & individual[i] <= 3.25) {
                    final_grade[i] = 1.00 * group
                } else if (individual[i] > 3.25 & individual[i] <= 3.75) {
                    final_grade[i] = 1.05 * group
                } else if (individual[i] > 3.75 & individual[i] <= 4.25) {
                    final_grade[i] = 1.1 * group
                } else if (individual[i] > 4.25 & individual[i] <= 5) {
                    final_grade[i] = 1.15 * group
                } 
            }
            for (i in 1:length(final_grade)) {
                if (final_grade[i] > 100) {
                    final_grade[i] = 100
                }
            }
            
            return(final_grade)
        },
        
        kaka.G = function(group, individual){
                                        #Data initialization
            group_number <- length(individual)
            half_group <- ceiling(group_number/2)
            overall_grade <- 0
            count <- 0
                                        #When the group number is one, we need to deal with it separately
            if(group_number==1){
                overall_grade[1]=group
                print(overall_grade)
            }
                                        #General condition
            else{
                                        #Count up the "Lazy teammates"
                for (j in 1:group_number) {
                    if(individual[j] < 3){
                        count=count+1
                    }
                }
                                        #Classification discussion
                for (k in 1:group_number){
                    if(individual[k]>=3 && count < half_group){
                                        #overall_grade[k] = (45-group/2)*individual[k]*individual[k]+(7/2*group-310)*individual[k]+(525-5*group)
                        overall_grade[k]=(5/2)*individual[k]*individual[k]+(-15)*individual[k]+(group+45/2)
                        if(overall_grade[k] >= 100){
                            overall_grade[k]=100
                        }
                    }
                    else if(individual[k] < 3){
                                        #overall_grade[k] = (10-group/2)*individual[k]*individual[k]+(5/2*group-40)*individual[k]+(30-2*group)
                        overall_grade[k] = (15-group/2)*individual[k]*individual[k]+(5/2*group-60)*individual[k]+(45-2*group)
                    }
                                        #Plus compensation
                    else if(individual[k] >= 3 && count >= half_group){
                                        #overall_grade[k] = (45-group/2)*individual[k]*individual[k]+(7/2*group-310)*individual[k]+(525-5*group)+10*individual[k]-30
                        overall_grade[k]=(5/2)*individual[k]*individual[k]+(-15)*individual[k]+(group+45/2)+(2.5*individual[k]-7.5)*(count-2)
                        if(overall_grade[k] >= 100){
                            overall_grade[k]=100
                        }
                    }
                }
                overall_grade = round(overall_grade,1)
                overall_grade
            }
        },
        
        kaka.H = function(group, individual){
            avg_individual <- sum(individual) / length(individual)# average individual score of group
            contribution_per <- individual/ avg_individual#the proportion of each member's contribution
            score <- contribution_per*group
            final_score <- round(pmin(rep(100,length(individual)),score),2)#if the score more than 100, stay 100.
            
            return(final_score)
        },
        
        kaka.I = function(group, individual){
                                        # Check for dplyr
            require(dplyr)
            
                                        # Check if group is within the valid range (0 to 100)
            if (group < 0 || group > 100) {
                stop("Group grade should be between 0 and 100.")
            }
            
                                        # Check if individual scores are within the valid range (1 to 5)
            if (any(individual < 1) || any(individual > 5)) {
                stop("Individual scores should be between 1 and 5.")
            }
            {
                final_grades <- dplyr::case_when(
                                           individual == 3 ~ group,
                                           individual <= 1.5 ~ 0,
                                           individual < 2 ~ group * 0.7,
                                           individual < 2.5 ~ group * 0.8,
                                           individual < 3 ~ group * 0.9,
                                           individual < 3.5 ~ group * 1.1,
                                           individual < 4 ~ group * 1.15,
                                           individual < 4.5 ~ group * 1.2,
                                           individual <= 5 ~ group * 1.3
                                       )
            }
            return(pmin(final_grades,100))
        },
        
        kaka.J = function(group, individual){
                                        # Input validation for group score
            if (!is.numeric(group) || group < 0 || group > 100) {
                stop("Group score must be a numeric value between 0 and 100.")
            }
            
                                        # Input validation for individual scores
            if (!is.vector(individual) || any(individual < 1) || any(individual > 5)) {
                stop("Individual scores must be a vector with values between 1 and 5.")
            }
            
                                        # Cap individual scores at 5 and calculate the average individual score for the group
            individual <- pmin(individual, 5)
            average_individual_score <- mean(individual)
            
                                        # Calculate the difference between the group average and individual score for each person
            difference_individual_score <- average_individual_score - individual
            
                                        # Determine whether to subtract or add the difference to the group score
            sign_difference <- ifelse(difference_individual_score > 0, -1, 1)
            
                                        # Convert the absolute difference to percentage and calculate the overall score for each person
            percentage_difference <- abs(difference_individual_score) * 20
            overall_scores <- ifelse(individual == 1, 0, group + sign_difference * percentage_difference)
            overall_scores <- pmin(overall_scores, 100) # Cap overall scores at 100
            overall_scores <- pmax(overall_scores, 0)   # Cap overall scores at 0
            
            return(overall_scores)
        },
        
        kaka.K = function(group, individual){
            if (mean(individual) >= 3) {
                marks <- ifelse(individual <= mean(individual),
                         ifelse(individual >= 3, group,
                                group * (1.209763-1.213589628/ 
                                         (1+(individual/1.75396)^8.529918)^
                                         0.3825534)),
                         group * (1+((individual/mean(individual)-1) * 0.3)))
                return(pmin(round(marks), 100))
            }
            else {marks <- group * (1.209763-1.213589628/ 
                                    (1+(individual/1.75396)^8.529918)^0.3825534)
                                    final_marks = pmin(round(marks), 100) # because it makes more sense
                                    return(final_marks)}
        },
        
        kaka.L = function(group, individual){
                                        # Calculate the mean and standard deviation
            mean_value <- mean(individual)
            std_dev <- sd(individual)
            
                                        #  Define the adjustment factor
            adjustment_factor <- 0.3
            
                                        # Adjust the score based on the standard deviation
            adjusted_score <- mean_value + (std_dev * adjustment_factor)
            
                                        # Output the adjusted score
            adjusted_score
            
                                        # Calculate the workload scores for each team member
            workload_scores <- (individual - adjusted_score)/5
            
                                        # Calculate the reward scores and penalty scores (to ensure they stay within the score range)?
            mapped_workload_scores <- workload_scores*(100-group)
            mapped_workload_scores_punish<-workload_scores*group
            
            final_scores<-c()
            
                                        # Evaluate and apply rewards/penalties to each team member's workload
            for(i in 1:length(individual)){
                if(workload_scores[i]>=0){
                    final_scores[i]=group+mapped_workload_scores[i]
                }
                else{
                    final_scores[i]=group+mapped_workload_scores_punish[i]
                }
            }
            
                                        # Output the final scores
            final_scores<-ceiling(final_scores)
            final_scores
        },
        
        ruru.A = function(group, individual){
            final = c()
                                        # 0-39 part:
            if (group < 40){
                for (ingrade in individual) {
                                        # Score 0 for no-working student, and score from 20 to 60 for the rest
                    ingrade=ifelse(ingrade < 1.5, 0, ingrade * 10 + 10)
                    final = c(final, ingrade)
                }
            }
                                        # 40-100 part:
            else {
                                        # Set percentage of group from 30% to 60%
                per_grade = 0.1 + 0.005 * group
                for (ingrade in individual) {
                                        # Score 0 for no-working student
                    if (ingrade < 1.5) {
                        final = c(final, 0)
                        next
                    } 
                                        # Set 3 as turning point, individual score increase fast before 3 and slow after 3
                    ingrade=ifelse(1.5 <= ingrade & ingrade < 3,
                                   20 * ingrade + 20,
                                   10 * ingrade + 50)
                    final = c(final, per_grade * group + (1 - per_grade) * ingrade)
                }
            }
            round(final, 1)
        },
        
        ruru.B = function(group, individual){
                                        # Check that the input is valid
            if (group < 0 || group > 100) {
                stop("Group grade must be between 0 and 100.")
            }
            if (any(individual < 1,individual > 5)) {
                stop("Individual grades must be between 1 and 5.")
            }
            if(mean(individual)>4){
                individual = individual - 2
            }
            individual_temp  = pnorm(individual + 0.2, mean = 3, sd = 0.8) * 100
            
            individual_final = 0.5 * group + 0.5 * individual_temp
            
            individual_final = round(individual_final)
            
            return(individual_final)
        },
        
        ruru.C = function(group, individual){
                                        # Ensure the group score is between 0 and 100
            group <- pmax(0, pmin(100, group))
            
                                        # Ensure individual scores are between 1 and 5
            individual <- pmax(1, pmin(5, individual))
            
                                        # Special case 1: If individual scores contain 12345 in any order
            if (length(individual) == 5 && all(sort(individual) == 1:5)) {
                individual = pmax(individual - 1, 1)
            }
            
                                        # Special case 2: If all individual scores are the same
            if (length(unique(individual)) == 1) {
                final_grades <- rep(pmax(0,group-10*(6-individual[1])),times=length(individual))
                return(round(final_grades))
            }
            
                                        # Special case 3: Check if there are only four individuals and their scores are 1234
            if (length(individual) == 4 && all(sort(individual) == 1:4)) {
                individual = pmax(individual - 1, 1)
            }
            
                                        # Calculate the individual average score
            individual_average <- sum(individual) / length(individual)
            x1 <- individual / individual_average
            x2 <- group
            final_grades <- 99.4*x1+1.04*x2-34.1*x1**2-0.0027*x2**2+0.137*x1*x2-64.1
            final_grades <- pmax(0, pmin(100, final_grades))
            
            return(round(final_grades,1))
        },
        
        ruru.D = function(group, individual){
            ## The full score of the group on a 100-point scale
            group_max_score <- 100
            
            ## Calculate the percentage weight of the group's results
            group_weight <- 0.7
            individual_weight <- 0.3
            
            ## Calculate individual scores
            individual_max_score <- 5 
            
            ## The full score of an individual's achievement on a 5-point scale
            individual_percentage_scores <- (individual / individual_max_score) * 100
            
            ## If the individual score is 2 points or less, 30% of the group score will be deducted
            adjusted_group_score <- ifelse(individual <= 2, group * (1 - 0.3), group)
            
            ## Calculate each person's final grade
            final_percentage_scores <- (adjusted_group_score * group_weight) + (individual_percentage_scores * individual_weight)
            
            return(final_percentage_scores)
        },
        
        ruru.E = function(group, individual){
            size <- length(individual)
                                        # Calculate the average contribution score for each group member
            avg_individual_score <- mean(individual)
            
                                        # Calculate the weights based on different individual contribution scores
            weights <- ifelse(individual == 1, 0.4, ifelse(individual == 2, 0.65, ifelse(individual == 3, 0.9, 1)))
            
                                        # Adjust the weights for individuals with contribution scores above or equal to the average
            weights[individual >= avg_individual_score] <- 1
            
                                        # Adjust the final grades for each group member based on their individual contribution score
            final_grades <- group * weights
            
                                        # Calculate the number of individuals below average contribution
            num_individuals_below_avg <- sum(individual < avg_individual_score)
            
                                        # Adjust the final grades based on the individual contribution compared to the average
            contribution_adj <- individual / avg_individual_score * num_individuals_below_avg
            final_grades <- final_grades + contribution_adj
            
                                        # Adjust the final grades for individuals with contribution equal to the mean individual contribution
            final_grades[individual == avg_individual_score] <- group
            
                                        # Adjust the final grades based on the group size
            group_adjustment <- 1 + (5 - size) * 0.05 # Smaller group sizes receive higher adjustment
            final_grades <- final_grades * group_adjustment
            
                                        # Make sure final grades are between 0 and 100
            final_grades <- pmin(100, pmax(0, final_grades))
            
            return(final_grades)
        },
        
        ruru.H = function(group, individual){
                                        # Check if group grade is within the valid range
            if (group < 0 || group > 100) {
                stop("Group grade should be between 0 and 100.")
            }
            
                                        # Check if individual scores are within the valid range
            if (any(individual < 1) || any(individual > 5)) {
                stop("Individual scores should be between 1 and 5.")
            }
            
                                        # Calculate the average score of individuals
            average_score <- mean(individual)
            
                                        # Calculate the scaling factor for representation (criteria 6)
            scale_factor1 <- 2.75
            scale_factor2 <- 5
            
                                        # Initialize an empty vector to store final grades
            final_grades <- numeric(length(individual))
            
            for (i in seq_along(individual)) {
                                        # Calculate the final individual score based on criteria 4 and 5
                
                                        # Apply the logarithmic scaling for representation (criteria 6)
                if (individual[i] == average_score) {
                    final_individual_score <- group
                } else if (individual[i] >= average_score) {
                    final_individual_score <- group + (exp(individual[i]-average_score)-1) * scale_factor1
                } else if (individual[i] < average_score) {
                    final_individual_score <- group - (exp(average_score -individual[i])-1)  * scale_factor2
                }
                
                                        # Apply the bonus mechanism for 4-person groups (criteria 8)
                if (length(individual) == 4) {
                    final_individual_score <- final_individual_score * 1.05
                }
                
                                        # Adjust the final individual score to be between 0 and 100
                if (final_individual_score > 100)
                    final_individual_score = 100
                if (final_individual_score < 0)
                    final_individual_score = 0
                
                                        # Store the final individual score in the vector
                final_grades[i] <- final_individual_score
            }
            
                                        # Return the vector of final grades
            return(round(final_grades,3))
        },
        
        ruru.I = function(group, individual){
                                        #part 1: group score
            part1=ifelse(individual==1,
                         0,
                         .5*group
                         )
                                        #part 2: individual score
            mean_grade=mean(individual)
            teamgrade = ifelse(individual <= mean_grade,
                               group / (mean_grade - 1) * (individual - 1),
                               ((100 - group) / (5 - mean_grade)) * individual + (5 * group - 100 * mean_grade) / (5 - mean_grade))
            part2=.5*teamgrade
                                        #add the result of part 1 and part 2
            result=part1+part2
            return(round(result))
        },
        
        ruru.J = function(group, individual){
            if (group < 0 || group > 100) {
                stop("The group parameter must be between 0 and 100.")
            }
            
            if (!all(individual >= 1 & individual <= 5)) {
                stop("The element in the individual parameter must be between 1 and 5")
            }
            
                                        #normal distribution
            custom_function <- function(x) {
                mean_val <- 3  # mean
                sd_val <- 1.5  # sd
                
                y <- dnorm(x, mean = mean_val, sd = sd_val)
                y <- y * 3.76
                
                y <- ifelse(x > 3, (2 - y)^0.5, y)
                return(y)
            }
            
            total_scores_2 <- group * custom_function(individual)
            total_scores_2[total_scores_2 > 100] <- 100

            total_scores_2
        },
        
        ruru.K = function(group, individual){
            X = 0.7 #The default x is 0.7
                                        # Limit individual scores to between 1 and 5
            individual <- pmax(1, pmin(5, individual))
            
                                        # Here are three fitting methods, choose one to run
            
                                        # ①Calculate X values that fit the normal distribution
            individual_mean <- mean(individual)
            individual_sd <- sd(individual)
            normalized_X <- pnorm(individual_mean, mean = 3.5, sd = 1)  # assume that the average of individual scores is around 3.5
            
            
                                        # ②Calculate X values that fit the exponential distribution
                                        #individual_mean <- mean(individual)
                                        #individual_lambda <- 1 / individual_mean  # lambda is the rate parameter in the exponential distribution
                                        #normalized_X <- pexp(individual_mean, rate = individual_lambda)
            
                                        # ③Calculate X values that fit the gamma distribution
                                        #individual_shape <- (mean(individual) / sd(individual))^2  # shape parameter in gamma distribution
                                        #individual_scale <- sd(individual)^2 / mean(individual)  # scale parameter in gamma distribution
                                        #normalized_X <- pgamma(mean(individual), shape = individual_shape, scale = individual_scale)
            
            X_adjusted <- X * normalized_X
            
                                        # Calculate the final score
            total_score <- group * X_adjusted + individual * (1 - X_adjusted) * 100 / 5
            
                                        # When the individual score is 1.0, ensure that the adjusted group score does not exceed 50%
            if (any(individual <= 1)) {
                X_adjusted <- min(X_adjusted,0.5)
            }
            
            return(total_score)
        },
        
        whio.A = function(group, individual){
                                        # 1. Normalize peer review score
            normalize <- function(score) {
                return(approx(c(1, 2, 3, 4, 5), c(0, 75, 100, 110, 120), xout = score)$y)
            }
            
            normalized_score <- normalize(individual)
            
                                        # 2. Calculate team contribution adjustment
            contribution_proportion <- individual / sum(individual)
            contribution_expect <- 1 / length(individual)
            
            contribution_diff <- contribution_proportion - contribution_expect
            
                                        # Set a threshold. If the absolute difference between individual contribution and mean contribution is larger than the threshold, start to adjust
            threshold <- contribution_expect / 2
            
                                        # Scale contribution_difference to be within the range [-0.1, 0.1]
            contribution_adjustment <- ifelse(abs(contribution_diff) > threshold, (contribution_diff - min(contribution_diff)) / (max(contribution_diff) - min(contribution_diff)) * 0.2 - 0.1, 0)
            
                                        # 3. Calculate final score
            final_score <- group * (normalized_score / 100) * (1+contribution_adjustment)
            
                                        # 4. Final check to ensure scores are within the range [0,100]
            final_score <- pmin(pmax(final_score, 0), 100)
            return(round(final_score))
        },
        
        whio.B = function(group, individual){
                                        # Check if the input group grade is within the valid range (0 to 100)
            if (group < 0 || group > 100) {
                stop("Invalid group grade. Grade should be between 0 and 100.")
            }  
            
                                        # Check if the individual appraisal scores are within the valid range (1 to 5)
            if (any(individual < 1) || any(individual > 5)) {
                stop("Invalid individual appraisal scores. Scores should be between 1 and 5.")
            }  
            
                                        # For grade calculations later on
            ind_calc <- individual[(individual > 1) & (individual <= 4)]
            if (length(ind_calc) != 0) {
                ind_max <- max(ind_calc)
                ind_range <- ind_max - 1
            }
            
                                        # Calculate their grades
            final_grades <- c()
            for (i in individual) {
                
                                        # Individual score of 1 automatically get 0%
                if (i == 1){ 
                    final_grades <- append(final_grades, 0)
                }
                
                                        # Individual score > 4 automatically get 105%
                else if (i > 4) {
                    final_grades <- append(final_grades, group * 1.05)
                }
                
                                        # We scale the other grades against their teammates accordingly
                else {
                    i_2 <- i - 1
                    i_percent <- i_2 / ind_range
                                        # 50% minimum mark for each student
                                        # Other 50% comes from scaling their grade
                    ind_grade <- (0.5 + (0.5 * i_percent)) * group 
                    ind_grade <- round(ind_grade, 2)
                    final_grades <- append(final_grades, ind_grade)
                }
            }
            
                                        # Ensure that final grades are capped at 100 
            final_grades <- pmin(final_grades, 100)  
            return(final_grades)
        },
        
        whio.C = function(group, individual){
            scores = c()
            for (i in individual) {
                if (i == 0) {
                    score = 0
                } else if (i > 3) {
                    multiplier = i / 3
                    total = multiplier + (group/100)
                    max1 = multiplier + 1
                    score = total/max1
                } else {
                    multiplier = i / 3
                    score = multiplier * (group/100)
                }
                scores = c(scores, score)
            }
            
            return(round(scores * 100, 2))
        },
        
        whio.D = function(group, individual){
            y = c()
            for(score in individual){
                if(score < 2){
                    final_mark = 0.3*group + 4.9*score
                }else if(score >= 2 & score < 3){ 
                    final_mark = group - group * (0.2) * (3 - score)
                }else{
                    final_mark = group + 0.05*group * (score - 3)
                }
                y = c(y, round(final_mark))
            }
            
            return(pmin(100, y))
        },
        
        whio.E = function(group, individual, average_team_size = 4.5){
            input_score <- individual
            
                                        # Test if all team members' scores are 1
            if (sum(individual == 1) == length(individual)) {
                individual[seq(along = individual)] <- group
                return (individual)
            }
            
                                        # Re-scale workload distribution to groups with size < average_team_size
            if (length(individual) < average_team_size) {
                dummy_one <- rep(1, average_team_size - length(individual))
                individual <- c(individual, dummy_one)
            }
            
            total_log_sum <- sum((log(individual) + 1))
            contribution <- (log(individual) + 1) / total_log_sum
            result <- pmin(group * length(individual) * contribution, 100)
            result[individual == 1] = 0
            
            round(result[seq(along = input_score)])
        },
        
        whio.F = function(group, individual){
                                        # The wrong input.
            if (length(which(individual < 1 | individual > 5)) > 0 | group < 0 | group > 100) {
                stop("Please enter the correct scores.", call. = FALSE)
            }
            
                                        # The correct input.
            else {
                
                                        # The person who got 1 score will not get any mark of the teamwork.
                individual[which(individual==1)] = 0
                
                                        # The person who got more than 1 score(less than 2) will get no more than 75% of the group mark.
                                        # The person who got score 2 will get 75% of the group mark.
                individual[which(individual > 1 & individual <= 2)] = group * (individual[which(individual > 1 & individual <= 2)] - 1) * 0.75
                
                                        # The person who got more than 2 score(less than 3) will get no more than 100% of the group mark. And the person who got score 3 will get 100% of the group mark.
                individual[which(individual > 2 & individual <= 3)] = group * ((individual[which(individual > 2 & individual <= 3)] - 2) * (1 - .75) + .75)
                
                                        # The person who got more than 3 score(less than 4) will get no more than 105% of the group mark. And the person who got score 4 will get 105% of the group mark.
                individual[which(individual > 3 & individual <= 4)] = group * ((individual[which(individual > 3 & individual <= 4)] - 3) * (1.05 - 1) + 1)
                
                                        # The person who got more than 4 score(less than 5) will get no more than 110% of the group mark. And the person who got score 5 will get 110% of the group mark.
                individual[which(individual > 4 & individual <= 5)] = group * ((individual[which(individual > 4 & individual <= 5)] - 4) * (1.1 - 1.05) + 1.05)
                
                                        # The maximum grade of a person in a group is less than or equal to 100.
                ifelse(individual > 100, 100, round(individual,1))
                
            }
            
        }
        
    )
    if (!is.null(team)){
        index = which(grepl(team, names(grading.functions), ignore.case = T))
        out = c()
        for(i in index){
            # grade = names(grading.functions)[i]
            grade = grading.functions[[i]](group, individual)
            out = cbind(out, grade)
        }
        colnames(out) = substr(names(grading.functions)[index], 6, 6)
        
    } else {
        ## Calculating all grades.
        all.grades <- sapply(grading.functions, function(x, group, individual) suppressMessages(x(group, individual)), group, individual)
        
        out = all.grades
    }
    out
}
# team = 'kaka'
# group = 80
# individual = c(1,2,3,4,5)
# calculate.grades(group, individual)
# calculate.grades(group, individual, team)



