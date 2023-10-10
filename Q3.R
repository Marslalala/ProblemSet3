###Problem 3


#  Download the data and import it into R.
us_500 <- read.csv("us-500.csv")

## Part a

#  Firstly, extract email from the data.
email <- us_500$email
#  Then find the number and calculate the proportion.
target_num_a <- length(grep(".net", email))
net_prop <- target_num_a / length(email)
print(net_prop)
#  We can see that the proportion of email addresses are hosted at a domain with TLD ¡°.net¡± is 0.146, which is 14.6%.


## Part b

#  The question is equivalent to find the proportion of email addresses that have at least 3 non alphanumeric
#  characters in them since there will be expected "@" and "." in all addresses.
#  Remove all alphanumerics from the email addresses.
non_alpnum <- gsub("[A-Za-z0-9]", "", email)

#  Calculate the proportion.
non_alpnum_type <- table(non_alpnum)
print(non_alpnum_type)
non_alpnum_prop <- (129 + 124) / sum(non_alpnum_type)
print(non_alpnum_prop)
#  We can see that the proportion of email addresses have at least one non alphanumeric character is 0.506, 
#  which is 50.6%.


## Part c

#  Firstly, extract all phone numbers from the data.
phone_num <- cbind(us_500$phone1, us_500$phone2)

#  Extract the area code from the phone numbers and calculate the frequency.
area_code <- substr(phone_num, 1, 3)
max(table(area_code))
freq <- table(area_code)
#  Find the most common area code.
most_comm_ac <- freq[which.max(freq)]
print(most_comm_ac)


## Part d

#  Firstly, extract all addresses from the data.
address <- us_500$address
#  Then extract all apartment numbers from the address.
add_list <- strsplit(address, "#")
has_apt <- which(sapply(add_list, length) == 2)
c_apt_num <- sapply(add_list[has_apt], function(x) x[2])
apt_num <- as.numeric(c_apt_num)
#  Finally, take log of the apartment numbers and draw the histogram.
log_apt_num <- log(apt_num)
hist(log_apt_num, xlab = "Log of Apt Number", main = "Histogram of log of Apt Number")


## Part e

#  To examine whether the apartment numbers appear to follow Benford¡¯s law, we need the first digit of 
#  all the apartment numbers to follow a particular distribution defined by the law.
#  The distribution: the leading digit d(d ¡Ê {1, ..., 9}) occurs with probability P(d) = log(1 + 1 / d),
#  where log has a base 10. Check if the leading digit of the apartment numbers has this distribution.

#  Start with extracting the first digit from the apartment numbers.
leading_digit <- as.numeric(substr(c_apt_num, 1, 1))

#  Then plot a histogram to compare with the distribution density plot.
hist(leading_digit, probability = TRUE, main = "Histogram of the Data", xlab = "Leading Digit")
#  Generate the density plot of the distribution.
prob <- c(0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046)
leading <- 1:9
barplot(prob, names.arg = leading, xlab = "Leading Digit", ylab = "PMF", main = "Probability Mass Function Plot")
#  From the two plots we can see that the difference between them is not trivial, which means the apartment number
#  may not follow the Benford's Law. However, since the sample is so small, it is hard to say that the data
#  violates the law only based on these graphs. We can check the frequency table of the leading numbers.
print(table(leading_digit))
#  We can see that 1-9 have similar frequencies, which means they are more likely to be uniformly distributed.
#  Thus, I believe the apartment numbers would not pass as real data based on my common sense.


## Part f

#  Firstly, extract all street numbers from the address.
str_list <- strsplit(address, " ")
str_char <- sapply(str_list, function(x) x[1])
str_num <- as.numeric(str_char)
#  Then extracting the last digit from the street numbers.
last_digit <- as.numeric(substring(str_char, nchar(str_char)))

#  Plot a histogram to compare with the distribution density plot.
hist(last_digit, probability = TRUE, main = "Histogram of the Data", xlab = "Last Digit")
#  Similarly, the last digit of street number also seems not following the Benford's Law. Check the frequency table.
print(table(last_digit))
#  However, the last digits of the street numbers do not follow the Benford's Law of First digit semems reasonable.
#  Since we should consider that if it follows a Benford's Law of Last digit(if exists), we cannot just simply 
#  conclude that the street numbers are not real data. Actually it makes sense that the last digits have a uniform
#  distribution just like the histogram shows.