###Problem 2


## Part a

#  Firstly, import libraries and load the database.
library(DBI)
sakila <- dbConnect(RSQLite::SQLite(), "sakila_master.db")
dbListTables(sakila)

#  Use the database to answer the question.
dbGetQuery(sakila, "
    SELECT l.name, COUNT(f.film_id) AS frequency
    FROM language AS l
    LEFT JOIN film AS f ON l.language_id = f.language_id
    WHERE NOT l.name == 'English'
    GROUP BY l.name
    ORDER BY frequency DESC
    LIMIT 1
")

#  Since the frequencies of other languages are zero, we know that there are no second most common 
#  language for films.


## Part b

#  Method 1
#  Join category, film_category and film into a single table.
result_tab_b <- dbGetQuery(sakila, "
              SELECT *
              FROM (SELECT *
                    FROM category AS c
                    LEFT JOIN film_category AS fc ON c.category_id = fc.category_id) AS cf
              LEFT JOIN film AS f ON cf.film_id = f.film_id
")
#  Use R built-in functions to answer the question
most_comm <- table(result_tab_b$name)
answer_b <- most_comm[which.max(most_comm)]
print(answer_b)

#  Method 2
dbGetQuery(sakila, "
    SELECT cf.name, COUNT(f.film_id) AS frequency
    FROM (SELECT *
          FROM category AS c
          LEFT JOIN film_category AS fc ON c.category_id = fc.category_id) AS cf
    LEFT JOIN film AS f ON cf.film_id = f.film_id
    GROUP BY cf.name
    ORDER BY frequency DESC
    LIMIT 1
")

#  From both methods we find that the most common genre is Sports, which are 74 movies of this genre.


## Part c

#  Method 1
#  Join country, city, address and customer into a single table.
result_tab_c <- dbGetQuery(sakila, "
              SELECT *
              FROM (SELECT *
                    FROM (SELECT *
                          FROM country AS co
                          LEFT JOIN city AS ci ON co.country_id = ci.country_id) AS c
                    LEFT JOIN address AS ad ON c.city_id = ad.city_id) AS a
              LEFT JOIN customer AS cu ON a.address_id = cu.address_id
")
#  Use R built-in functions to answer the question
counts <- table(result_tab_c$country)
answer_c <- counts[counts == 9]
print(answer_c)

#  Method 2
dbGetQuery(sakila, "
              SELECT a.country, COUNT(cu.customer_id) AS frequency
              FROM (SELECT *
                    FROM (SELECT *
                          FROM country AS co
                          LEFT JOIN city AS ci ON co.country_id = ci.country_id) AS c
                    LEFT JOIN address AS ad ON c.city_id = ad.city_id) AS a
              LEFT JOIN customer AS cu ON a.address_id = cu.address_id
              GROUP BY a.country
              HAVING frequency == 9
")

#  From both methods we find that only UK has exactly 9 customers.