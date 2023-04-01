# Goals:
## 1. Be familiar with different kinds of data types
## 2. Be able to read in data frames and identify the data types they have
## 3. Understand subsetting (vectors and data frames)

# 4 different data types
# 1. numeric
# 2. character
# 3. logical
# 4. factor

# Vectors
## c() function = concatenate. Separate items you want to concatenate together with a comma
animals <- c("mouse", "rat", "dog", "cat")
weight_g <- c(22, 34, 85, 62)

## subsetting / indexing
# Subsetting based on location
animals[3] 

# symbols: - delete an item, : provide a range, 
animals[-3]
animals[1:2]

# Subsetting based on condition
# == equals
# ! not; != not equal to; %in% within
animals == "dog"
# go inside animals and get all of the true values
animals[animals == "dog"]
# Why? is it a bit dumb but will become relevant later. One example is that you can index across different vectors. Get the weights of dog, for instance
weight_g[animals == "dog"]

# More conditions
weight_g > 50 
weight_g[weight_g > 50 ]
weight_g[weight_g != 62 ]

# NA values
## I am manually adding some in for demonstration purposes
animals <- c("mouse", "rat", "dog", NA, "cat")
weight_g <- c(NA, 34, 85, 80, 62)

# is.na function returns logical vector.
is.na(animals)
# We can use that logical vector as a condition 
## E.g., where in the animals is there an NA?
animals[is.na(animals)]
# Way more useful: reverse it by using the ! in front of is.na()
animals_nona <- animals[!is.na(animals)]

# na.rm to remove NA inside a function
sum(weight_g)
# what is a function: takes an input variable and does something to it, gets an output variable
sum(weight_g, na.rm = T)
# argument is more detail about the function and what you want it to do
# use ? to get the help file
?sum

## Read in our data frame
?readRDS
df <- readRDS(file = "data/lgu.rds")
# If you want to look at the data, printing it isn't really useful
df
# Might use the summary function instead
summary(df)

# What are these columns, actually?
# uni_state = state of university
# licensee = the company who buys the plant
# crop_name_common
# crop_name_scientific
# variety_name
# agreement_type = exclusive, non-exclusive, etc.
# effective_date = date of purchase/license
# invention_id = university-specific IDs
# ..

# To figure out what invention id is, try to look at the first 5 rows of invention id
# with data frames, default dimension is column
df[1:5] # assumes you want 1-5 columns, not rows
# ^These two are the same 
df[,1:5]
# to get rows use a comma separator df[rows,columns]
# How to get the first 5 rows
df[1:5,] # default if empty is all 
# 1:5 rows and invention id column
df[1:5,8] # name the column number
df[1:5,'invention_id']# name the column name
df$invention_id[1:5] # or combine with a dollar sign

# Can you use subsetting and the is.na() to get the non-na values from the column?
df$invention_id[!is.na(df$invention_id)]

