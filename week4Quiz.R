
# 1. Show an appropriate visualization that displays the total number of movies for each decade.

week4Quiz <- function()  {
  # Read FileName
  fileName <- "C:/Anthony/School_CUNY/IS 607/Week4/movies.tab"
  m <- read.table(fileName, sep="\t", header=TRUE, quote="", comment="")
  
  yr1850_1900 <- 0
  yr1900_1910 <- 0
  yr1910_1920 <- 0
  yr1920_1930 <- 0
  yr1930_1940 <- 0
  yr1940_1950 <- 0
  yr1950_1960 <- 0
  yr1960_1970 <- 0
  yr1970_1980 <- 0
  yr1980_1990 <- 0
  yr1990_2000 <- 0
  yr2000_2010 <- 0
  
  for (i in 1:length(m$year)) {
    
    if (m$year[i] >= 1850 & m$year[i]<= 1900)  {
         yr1850_1900 <- yr1850_1900 + 1
    } else  
       if (m$year[i] > 1900 & m$year[i] <= 1910)  {
         yr1900_1910 <- yr1900_1910 + 1
       } else  
         if (m$year[i] > 1910 & m$year[i] <= 1920)  {
           yr1910_1920 <- yr1910_1920 + 1
         } else  
           if (m$year[i] > 1920 & m$year[i] <= 1930)  {
             yr1920_1930 <- yr1920_1930 + 1
           } else  
             if (m$year[i] > 1930 & m$year[i] <= 1940)  {
               yr1930_1940 <- yr1930_1940 + 1
             } else  
               if (m$year[i] > 1940 & m$year[i] <= 1950)  {
                 yr1940_1950 <- yr1940_1950 + 1
               } else  
                 if (m$year[i] > 1950 & m$year[i] <= 1960)  {
                   yr1950_1960 <- yr1950_1960 + 1
                 } else  
                   if (m$year[i] > 1960 & m$year[i] <= 1970)  {
                     yr1960_1970 <- yr1960_1970 + 1
                   } else  
                     if (m$year[i] > 1970 & m$year[i] <= 1980)  {
                       yr1970_1980 <- yr1970_1980 + 1
                     } else  
                       if (m$year[i] > 1980 & m$year[i] <= 1990)  {
                         yr1980_1990 <- yr1980_1990 + 1
                       } else  
                         if (m$year[i] > 1990 & m$year[i] <= 2000)  {
                           yr1990_2000 <- yr1990_2000 + 1
                         } else  
                           if (m$year[i] > 2000 & m$year[i] <= 2010)  {
                             yr2000_2010 <- yr2000_2010 + 1
                           }
  }

   print( yr1850_1900) 
   print( yr1900_1910)
   print( yr1910_1920)
   print( yr1920_1930)
   print( yr1930_1940)
   print( yr1940_1950)
   print( yr1950_1960)
   print( yr1960_1970)
   print( yr1970_1980)
   print( yr1980_1990)
   print( yr1990_2000)
   print( yr2000_2010)
  
  x <- c(1900,
         1910,
         1920,
         1930,
         1940,
         1950,
         1960,
         1970,
         1980,
         1990,
         2000,
         2010)
  
  
  y <- c(yr1850_1900,
    yr1900_1910,
    yr1910_1920,
    yr1920_1930,
    yr1930_1940,
    yr1940_1950,
    yr1950_1960,
    yr1960_1970,
    yr1970_1980,
    yr1980_1990,
    yr1990_2000,
    yr2000_2010)
  

plot(x,y)

}

#2. Show the average IMDB user rating for different genres of movies? 
#   Has this changed over time?
#   The IMDB User rating has changed overtime
week4QuizIMDB <- function()  {
 
  fileName <- "C:/Anthony/School_CUNY/IS 607/Week4/movies.tab"
  m <- read.table(fileName, sep="\t", header=TRUE, quote="", comment="")
  
  Action <- 0
  Animation <- 0
  Comedy <- 0
  Drama <- 0
  Documentary <- 0
  Romance <- 0
  Short <- 0

  
  ActionRate <- 0
  AnimationRate <- 0
  ComedyRate <- 0
  DramaRate <- 0
  DocumentaryRate <- 0
  RomanceRate <- 0
  ShortRate <- 0
  
  
  ActionAvg <- 0
  AnimationAvg <- 0
  ComedyAvg <- 0
  DramaAvg <- 0
  DocumentaryAvg <- 0
  RomanceAvg <- 0
  ShortAvg <- 0
  
  
  
  for (i in 1:length(m$year)) {
    
    if (m$Action[i] == 1)  {
        Action <- Action + 1
        ActionRate <- c(ActionRate, m$rating[i] ) 
      } else if (m$Animation[i] == 1)  {
        Animation <- Animation + 1
        AnimationRate <- c(AnimationRate , m$rating[i] )
      } else if (m$Comedy[i] == 1)  {
        Comedy <- Comedy + 1
        ComedyRate <- c(ComedyRate, m$rating[i])  
      } else if (m$Drama[i] == 1)  {
        Drama <- Drama + 1
        DramaRate <- c(DramaRate , m$rating[i] )
        } else if (m$Documentary[i] == 1)  {
        Documentary <- Documentary + 1        
        DocumentaryRate <- c(DocumentaryRate , m$rating[i] )
        } else if (m$Romance[i] == 1)  {
        Romance <- Romance + 1
        RomanceRate <- c(RomanceRate,m$rating[i])
        } else if (m$Short[i] == 1)  {
        Short <- Short + 1
        ShortRate <- c(ShortRate , m$rating[i] ) 
      }
      
    }
     
  
  ActionAvg <- sum(ActionRate) /  Action
  AnimationAvg <- sum(AnimationRate) / Animation
  ComedyAvg <- sum(ComedyRate) / Comedy
  DramaAvg <-  sum(DramaRate) /  Drama
  DocumentaryAvg <- sum(DocumentaryRate) / Documentary
  RomanceAvg <- sum(RomanceRate) /Romance
  ShortAvg <- sum(ShortRate) / Short
  
  genre <- c(ActionAvg, AnimationAvg, ComedyAvg, DramaAvg, DocumentaryAvg, RomanceAvg, ShortAvg)
  pie(genre, main="Genre", col=rainbow(length(genre)),
      labels=c("Action","Animation","Comedy","Drama","Documentary","Romance","Short")
      )
  
  
}

#3. Is there a relationship between length of movie and movie rating?
# Yes, There are more viewers who watch Movies with Ratings of Medium / High that have a longer 
#   Movie Duration.

week4QuizRating <- function()  {
  
  fileName <- "C:/Anthony/School_CUNY/IS 607/Week4/movies.tab"
  m <- read.table(fileName, sep="\t", header=TRUE, quote="", comment="")
#par(new=T)
  Length <- 0
  Rating <- 0

  RatingLow <- 0
  RatingMedium <- 0
  RatingHigh <- 0
  RatingAllStar <- 0

  LengthLow <- 0
  LengthMedium <- 0
  LengthHigh <- 0
  LengthAllStar <- 0



  for (i in 1:length(m$year)) {
    if (m$rating[i] >= 0 && m$rating[i] <= 3 ) {
      
      RatingLow <- RatingLow + 1
      LengthLow <- LengthLow + 1
      
    } else if (m$rating[i] > 3 && m$rating[i] <= 6 ) {
      RatingMedium <- RatingMedium + 1 
      LengthMedium <- LengthMedium + 1
      
    } else if (m$rating[i] > 6 && m$rating[i] <= 9 ) {
      RatingHigh <- RatingHigh + 1
      LengthHigh <- LengthHigh + 1
      
      
    } else if (m$rating[i] > 9 && m$rating[i] <= 10 ) {
      RatingAllStar <- RatingAllStar + 1       
      LengthAllStar <- LengthAllStar + 1
    }

    
  }
  
   x <- c(LengthLow, LengthMedium, LengthHigh, LengthAllStar)
   y <- c(RatingLow, RatingMedium, RatingHigh, RatingAllStar)


barplot(x,y,  main="Rating Graph", xlab="Rating",  
        ylab="Total", names.arg=c("Low","Medium","High","AllStar"), 
        border="blue", density=c(10,20,30,40))

}

#4. Is there a relationship between length of movie and genre?
#Medium / High Lenths has better Overall Ratings.

week4QuizLength <- function()  {
  
  fileName <- "C:/Anthony/School_CUNY/IS 607/Week4/movies.tab"
  m <- read.table(fileName, sep="\t", header=TRUE, quote="", comment="")
  #par(new=T)
    
  LengthLow <- 0
  LengthMedium <- 0
  LengthHigh <- 0
  LengthAllStar <- 0
  
  
  ActionRate <- 0
  AnimationRate <- 0
  ComedyRate <- 0
  DramaRate <- 0
  DocumentaryRate <- 0
  RomanceRate <- 0
  ShortRate <- 0
  
  Action <- 0
  Animation <- 0
  Comedy <- 0
  Drama <- 0
  Documentary <- 0
  Romance <- 0
  Short <- 0
  
  
  for (i in 1:length(m$year)) {
    
    if (m$Action[i] == 1)  {
      Action <- Action + 1
      ActionRate <- c(ActionRate, m$rating[i] ) 
    } else if (m$Animation[i] == 1)  {
      Animation <- Animation + 1
      AnimationRate <- c(AnimationRate , m$rating[i] )
    } else if (m$Comedy[i] == 1)  {
      Comedy <- Comedy + 1
      ComedyRate <- c(ComedyRate, m$rating[i])  
    } else if (m$Drama[i] == 1)  {
      Drama <- Drama + 1
      DramaRate <- c(DramaRate , m$rating[i] )
    } else if (m$Documentary[i] == 1)  {
      Documentary <- Documentary + 1        
      DocumentaryRate <- c(DocumentaryRate , m$rating[i] )
    } else if (m$Romance[i] == 1)  {
      Romance <- Romance + 1
      RomanceRate <- c(RomanceRate,m$rating[i])
    } else if (m$Short[i] == 1)  {
      Short <- Short + 1
      ShortRate <- c(ShortRate , m$rating[i] ) 
    }
    
  }
  
  
  
  for (i in 1:length(m$year)) {
    if (m$rating[i] >= 0 && m$rating[i] <= 3 ) {      
      LengthLow <- LengthLow + 1
      
    } else if (m$rating[i] > 3 && m$rating[i] <= 6 ) {
      LengthMedium <- LengthMedium + 1
      
    } else if (m$rating[i] > 6 && m$rating[i] <= 9 ) {
      LengthHigh <- LengthHigh + 1
      
      
    } else if (m$rating[i] > 9 && m$rating[i] <= 10 ) {
      LengthAllStar <- LengthAllStar + 1
    }
    
    
  }
  
  y <- c(ActionRate, AnimationRate, ComedyRate, DramaRate, DocumentaryRate, RomanceRate, ShortRate)
  x <- c(LengthLow, LengthMedium, LengthHigh, LengthAllStar)
  
  
  barplot(x,y,  main="Rating Graph", xlab="Rating",  
          ylab="Total", names.arg=c("Low","Medium","High","AllStar"), 
          border="blue", density=c(10,20,30,40))
  
}

#5. Which other variable best predicts total number of votes that a movie received.
#The best predictor to identifying Votes would be the Votes Column in the Data Frame. The Chart I created shows that 
#People Voted for Drama the Most.

week4QuizVotes <- function()  {
  # Read FileName
  fileName <- "C:/Anthony/School_CUNY/IS 607/Week4/movies.tab"
  m <- read.table(fileName, sep="\t", header=TRUE, quote="", comment="") 
  
  ActionVote <- 0
  AnimationVote <- 0
  ComedyVote <- 0
  DramaVote <- 0
  DocumentaryVote <- 0
  RomanceVote <- 0
  ShortVote <- 0  
  
  for (i in 1:length(m$year)) {
    if (m$Action[i] == 1)  {
      ActionVote <- c(ActionVote, m$votes[i] ) 
    } else if (m$Animation[i] == 1)  {
      AnimationVote <- c(AnimationVote, m$votes[i] ) 
    } else if (m$Comedy[i] == 1)  {
      ComedyVote <- c(ComedyVote, m$votes[i] ) 
    } else if (m$Drama[i] == 1)  {
      DramaVote <- c(DramaVote, m$votes[i] ) 
    } else if (m$Documentary[i] == 1)  {
      DocumentaryVote <- c(DocumentaryVote, m$votes[i] ) 
    } else if (m$Romance[i] == 1)  {
      RomanceVote <- c(RomanceVote, m$votes[i] )     
    } else if (m$Short[i] == 1)  {
      ShortVote <- c(ShortVote, m$votes[i] )    
    }
  
  }
  
  ActionTotal <- sum( ActionVote )
  AnimationTotal <- sum( AnimationVote )
  ComedyTotal <- sum( ComedyVote )
  DramaTotal <- sum( DramaVote )
  DocumentaryTotal <- sum( DocumentaryVote )
  RomanceTotal<- sum( RomanceVote )
  ShortTotal <- sum( ShortVote )  
  
 
  y <- c(ActionTotal, AnimationTotal, ComedyTotal, DramaTotal, DocumentaryTotal, RomanceTotal, ShortTotal)
  plot (y, axes=FALSE)
  axis(1, at=1:7, lab=c("Action","Animation","Comedy","Drama","Documentary","Romance","Short"))
  
}

