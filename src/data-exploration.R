#
# some data exploration
#
# @author Amir
#

library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};
                               server=DESK-SR;database=dblp_data_clean;
                               trusted_connection=true')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT TOP 10 * from abstract')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT * from abstract 
                           where abstract is null or pid is null')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT TOP 10 pid, DATALENGTH(abstract) 
                           FROM abstract
                           ORDER BY 2 DESC')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT Top 20 abstract.pid, author, abstract
                           FROM abstract INNER JOIN author ON abstract.pid = author.pid')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select top 500 * from author_article')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select TOP 20 author_article.pid, author_article.author_key,
                           author_key.name, author.author
                           from author_article INNER JOIN author 
                           ON author_article.pid = author.pid 
                           INNER JOIN author_key 
                           ON author_key.author_key = author_article.author_key')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT TOP 10 * from author_key')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT Top 100 abstract.pid, 
                           author_article.author_key, abstract
                           FROM author_reference INNER JOIN abstract 
                           ON abstract.pid = author_reference.eid_from
                           INNER JOIN author_article 
                           ON author_article.author_key = author_reference.eid_to')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT Top 10 abstract.pid, abstract.abstract, 
                           has_abstract.has_abstract
                           FROM abstract INNER JOIN has_abstract
                           ON abstract.pid = has_abstract.PID')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT Top 10 abstract.pid, abstract.abstract, 
                           author.author, pyear.year
                           FROM abstract INNER JOIN pyear
                           ON abstract.pid = pyear.PID
                           INNER JOIN author
                           ON abstract.pid = author.PID
                           WHERE pyear.year < 1950')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select TOP 10 abstract.pid, abstract.abstract, 
                           reference.PID1_from
                           from abstract INNER JOIN reference 
                           ON abstract.pid = reference.PID2_to')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select TOP 10 abstract.pid, abstract.abstract,title.title
                           from abstract INNER JOIN title
                           ON abstract.pid = title.PID')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT TOP 10 * FROM venue WHERE venue in
                          (SELECT venue FROM venue GROUP BY venue HAVING COUNT(venue)=1)')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select TOP 40 author.PID, abstract.abstract, author.author, 
                           venue.venue                        
                           from author INNER JOIN venue
                           ON author.pid = venue.PID
                           INNER JOIN abstract
                           ON author.PID = abstract.pid')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT TOP 50 author.PID, author.author
                           FROM author INNER JOIN abstract ON author.PID = abstract.pid
                           GROUP BY author.author, author.PID')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT author.PID as paper, COUNT(author.PID) as NumAuthor
                           FROM author INNER JOIN abstract 
                           ON author.PID = abstract.pid
                           GROUP BY author.PID ORDER BY NumAuthor DESC')
attach(res)
plot(paper, NumAuthor)
boxplot(paper, NumAuthor)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT (COUNT(*)*100.0/(SELECT COUNT(*) FROM has_abstract)) 
                           FROM has_abstract WHERE has_abstract = 0')

res <- sqlQuery(dbhandle, 'SELECT TOP 10 has_abstract.PID, abstract.abstract, has_abstract
                           FROM has_abstract INNER JOIN abstract 
                           ON has_abstract.PID = abstract.pid')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'SELECT pyear.PID as paper, pyear.year as year
                           FROM pyear INNER JOIN abstract 
                           ON pyear.PID = abstract.pid
                           ORDER BY year DESC')
attach(res)
plot(paper, year, type = "p")

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select abstract.pid as paper, count(abstract.pid) as NumReference
                           from abstract INNER JOIN reference 
                           ON abstract.pid = reference.PID2_to
                           GROUP BY abstract.pid
                           ORDER BY NumReference DESC')
attach(res)
plot(paper, NumReference, type = "p")

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select venue.venue as venueName, 
                           count(venue.PID) as NumVenue from venue
                           GROUP BY venue.venue ORDER BY NumVenue DESC')
attach(res)
plot(venueName, NumVenue)

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select eid_from, COUNT(eid_from) as countR
                           from author_reference
                           GROUP BY eid_from
                           ORDER BY COUNT(eid_from) DESC')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select venue, COUNT(venue) as countV
                           from author INNER JOIN venue
                           ON author.PID = venue.PID
                           GROUP BY venue
                           ORDER BY COUNT(venue) DESC')

#---------------------------------------------------------------------------#

NumOfAuthArticle <- function(x,y){
  sqlQuery(dbhandle, paste('select count(author.PID) from 
                            author INNER JOIN pyear on author.pid = pyear.pid
                            where author_key =' , x, 'and pyear.year =' , y)) #pyear.year <=
}

#---------------------------------------------------------------------------#

NumOfAuthRef <- function(x,y){
  sqlQuery(dbhandle, paste('select count(author_reference.eid_from) as countAR
                            from author_reference
                            INNER JOIN pyear ON author_reference.eid_from = pyear.PID
                            where author_reference.eid_to =', x, 'and pyear.year =', y)) #pyear.year <=
}

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select pyear.year, author.author_key, 
                           COUNT(reference.PID1_from) as countAK
                           from reference INNER JOIN author
                           ON reference.PID2_to = author.PID
                           INNER JOIN pyear ON pyear.PID = author.PID
                           GROUP BY pyear.year, reference.PID1_from, author.author_key
                           ORDER BY countAK DESC')

#---------------------------------------------------------------------------#

res <- sqlQuery(dbhandle, 'select pyear.year , venue, COUNT(venue) as countV
                           from pyear INNER JOIN venue
                           ON pyear.PID = venue.PID
                           WHERE pyear.year > 0
                           GROUP BY pyear.year, venue
                           ORDER BY COUNT(venue) DESC')

#---------------------------------------------------------------------------#
