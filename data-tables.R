
### Przetwarzanie Danych Ustrukturyzowanych 2024L
### Praca domowa nr. 1
###
### UWAGA:
### nazwy funkcji oraz ich parametrow powinny pozostac niezmienione.
###  
### Wskazane fragmenty kodu przed wyslaniem rozwiazania powinny zostac 
### zakomentowane
###

# -----------------------------------------------------------------------------#

#install.packages('sqldf')
#library('sqldf')
#install.packages('dplyr')
#library('dplyr')
#install.packages('data.table')
#library('data.table')
#install.packages('compare')
#library('compare')
#install.packages('microbenchmark')
#library('microbenchmark')

#Posts <- read.csv('C:\\Users\\przem\\OneDrive\\Pulpit\\PRZEMO\\STUDIES_MINI_PW\\SEM__2__\\R\\Homework_1\\Data_materials\\Posts.csv.gz')
#Comments <- read.csv('C:\\Users\\przem\\OneDrive\\Pulpit\\PRZEMO\\STUDIES_MINI_PW\\SEM__2__\\R\\Homework_1\\Data_materials\\Comments.csv.gz')
#PostLinks <- read.csv('C:\\Users\\przem\\OneDrive\\Pulpit\\PRZEMO\\STUDIES_MINI_PW\\SEM__2__\\R\\Homework_1\\Data_materials\\PostLinks.csv.gz')
#Users <- read.csv('C:\\Users\\przem\\OneDrive\\Pulpit\\PRZEMO\\STUDIES_MINI_PW\\SEM__2__\\R\\Homework_1\\Data_materials\\Users.csv.gz')
#Votes <- read.csv('C:\\Users\\przem\\OneDrive\\Pulpit\\PRZEMO\\STUDIES_MINI_PW\\SEM__2__\\R\\Homework_1\\Data_materials\\Votes.csv.gz')

# -----------------------------------------------------------------------------#



# -----------------------------------------------------------------------------#
#                     Zadanie 1
# -----------------------------------------------------------------------------#

sql_1 <- function(Users){
  sqldf("
          SELECT STRFTIME('%Y', CreationDate) AS Year,
                  STRFTIME('%m', CreationDate) AS Month,
                  COUNT(*) AS TotalAccountsCount,
                  AVG(Reputation) AS AverageReputation
          FROM Users
          GROUP BY Year, Month
        ")
}
# idąc po wierszach kolumny CreationDate wyławiamy z nich rok i tworzy sie kolumna zawierająca tylko rok o nazwie Year
# idąc po wierszach kolumny CreationDate wyławiamy z nich miesiąc i tworzy sie kolumna zawierająca tylko miesiąc o nazwie Month
# agregujemy i zliczamy ile razy się powtórzyło względem kolumny roku i miesiąca
# agregujemy kolumnę Reputation względem roku i miesiąca i wyliczamy średnią dla nich
# działamy na ramce danych Users
# grupujemy względem otrzymanych wcześniej kolumn o nazwie Year i Month łącznie

base_1 <- function(Users){
  Year <- as.data.frame(strftime(Users$CreationDate, "%Y"))         # tworzymy kolumnę Year która zawiera rok wyciągnięty z każdego wiersza kolumny CreationDate
  Month <- as.data.frame(strftime(Users$CreationDate, "%m"))        # tworzymy kolumnę Month która zawiera miesiąc wyciągnięty z każdego wiersza kolumny CreationDate
  x <- aggregate(Users$Reputation, cbind(Month,Year), mean)         # agregujemy kolumnę Reputation względem roku i miesiąca i wyliczamy średnią dla nich
  y <- aggregate(Users$Reputation, cbind(Month,Year), length)       # agregujemy i zliczamy ile razy się powtórzyło względem kolumny roku i miesiąca
  y[, c(1,2)] <- NULL                                               # usuwamy z y kolumny Year i Month aby się nie powtarzały przy późniejszym łączeniu ramek
  result_1 <- cbind.data.frame(x,y)                                 # łączymy obie pomocnicze ramki x i y w jedną aby otrzymać wynik
  result_1[, c(1,2)] <- result_1[, c(2,1)]                          # ostateczną ramkę danych poddajemy ostatnim szlifom i zamieniamy miejscami kolumny między sobą tak aby wszystko było perfekcyjnie identyczne w porównaniu z tabelami w pozostałych pakietach
  result_1[, c(3,4)] <- result_1[, c(4,3)]
  colnames(result_1) <- c("Year","Month","TotalAccountsCount","AverageReputation")     # zmiana nazw kolumn na wymagane w zadaniu
  result_1
}

dplyr_1 <- function(Users){
  z1 <- Users %>%                                                                            # naszą ramkę będziemy obrabiac w obrębie ramki Users
    mutate(CreationDate = as.Date(CreationDate)) %>%                                         # tworzymy kolumnę na podstawie juz istniejącej jako data 
    mutate(Year = format(CreationDate, "%Y"), Month = format(CreationDate, "%m")) %>%        # tworzymy kolumnę Year z latami wyciągniętymi z kolumny CreationDate i Month z miesiącami
    group_by(Year, Month) %>%                                                                # grupujemy nasze kolumny względem roku i miesiąca
    summarise(TotalAccountsCount = n(), AverageReputation = mean(Reputation, na.rm = TRUE))  # podsumowujemy dane w kolumnach i dodajemy je do wcześniej utworzonych
}

data.table_1 <- function(Users){
  first <- as.data.table(Users)                                                                                               # tworzymy tabele danych first z Users
  first <- first[, c("Year","Month") := list(format(as.IDate(CreationDate), "%Y"), format(as.IDate(CreationDate), "%m"))]     # tworzymy kolumny z latami i miesiącami
  first <- first[, .(TotalAccountsCount = .N, AverageReputation = mean(Reputation, na.rm = TRUE)), by = .(Year, Month)]       # tworzymy pozostałe kolumny zlicające ile razu pojawiła się data miesięczna i roczna oraz średnie w tych datach
}

#sql1 <- sql_1(Users)
#base1 <- base_1(Users)
#dplyr1 <- dplyr_1(Users)
#data.table1 <- data.table_1(Users)

#compare::compare(sql1, base1)
#compare::compare(sql1, dplyr1)
#compare::compare(sql1, data.table1)
#microbenchmark::microbenchmark(sql1, base1, dplyr1, data.table1)

?compare
# -----------------------------------------------------------------------------#
#                         Zadanie 2
# -----------------------------------------------------------------------------#

sql_2 <- function(Posts){
  sqldf("
        SELECT Users.DisplayName, Users.Location, Users.Reputation,
                STRFTIME('%Y-%m-%d', Users.CreationDate) AS CreationDate,
                Answers.TotalCommentCount
        FROM (
                SELECT OwnerUserId, SUM(CommentCount) AS TotalCommentCount
                FROM Posts
                WHERE PostTypeId == 2 AND OwnerUserId != ''
                GROUP BY OwnerUserId
              ) AS Answers
        JOIN Users ON Users.Id == Answers.OwnerUserId
        ORDER BY TotalCommentCount DESC
        LIMIT 10
        ")
}
# z póxniej opisanych ramek wyberamy kolumny takie jak w opisie plus wyciągamy pełną datę z kolumny CreationDate
# operujemy na ramce Posts, wybieramy z niej tylko wiersze których PostTypeId równe jest 2 oraz OwnerUserId nie jest pusty
# grupujemy według kolumny OwnerUserId a potem wyjmujemy z tak przerobionej ramki tylko kolumny OwnerUserID i agregujemy kolumne 
# CommentCount według funkcji suma, taką ramkę zapisujemy jako Ansewrs
# ramkę Answers łączymy z ramką Users według kolumny Id z Users oraz według kolumny OwnerUserId z Answers
# sortujemy ramkę według malejących wratości w kolumnie TotalCommentCount
# wybieramy pierwszych 10

base_2 <- function(Posts){
  Answers <- Posts[Posts$PostTypeId == 2 & Posts$OwnerUserId != '', ]              # ramka Answers jest ramką Posts specjalnie zmodyfikowaną czyli wybieramy z niej tylko wiersze których PostTypeId równe jest 2 oraz OwnerUserId nie jest pusty itd.
  Answers <- Answers[, c('OwnerUserId','CommentCount')]                            # wybieramy z Answers tylko te dwie kolumny
  Answers <- aggregate(Answers$CommentCount, Answers['OwnerUserId'], sum)          # agregujemy kolumnę CommentCount według kolumny OwnerUserId i fukcji sumy
  names(Answers)[2] <- "TotalCommentCount"                                         # zmiana nazwy kolumny na odpowiednią
  result_2 <- Users[, c("DisplayName","Location","Reputation","Id")]               # tworzymy wynik końcowy z Users i wybieramy cztery pokazane kolumny (potem jeszcze będziemy ją obrabiać)
  names(result_2)[names(result_2)=="Id"] <- "OwnerUserId"                          # zmieniamy nazwę kolumny "Id" na bardziej adekwatną której będą wymagać w zadaniu
  time <- strftime(Users$CreationDate, "%Y-%m-%d")                                 # tworzymy ramkę danych time która zawiera daty wyciągnięte z kolumny CreationDate
  result_2 <- cbind.data.frame(result_2, time)                                     # łączymy obie ramki danych (mają tyle samo wierszy więc nie ma z tym problemu) aby potem na nich dalej operować 
  names(result_2)[names(result_2)=="time"] <- "CreationDate"                       # zmiana nazwy kolumny na odpowiednią CreationDate
  result_2 <- merge.data.frame(result_2,Answers,by="OwnerUserId")                  # łączymy obie ramki danych według kolumny OwnerUserId
  result_2["OwnerUserId"] <- NULL                                                  # usuwamy niepotrzebną kolumnę OwnerUserId
  result_2 <- result_2[order(result_2$TotalCommentCount, decreasing=TRUE), ]       # sortujemy malejąco według kolumny TotalCommentCount
  result_2 <- result_2[1:10, ]                                                     # wybieramy pierwszych 10 wierszy po posortowaniu
  row.names(result_2) <- c(1:10)
  result_2
}

dplyr_2 <- function(Posts){
  z2 <- Posts %>%                                                                  # tworzymy z2 z Posts na podstawie której będziemy operować 
    filter(PostTypeId == 2, OwnerUserId != '') %>%                                 # filtrujemy po specyficznych wierszach i je wybieramy
    group_by(OwnerUserId) %>%                                                      # grupujemy według kolumny OwnerUserId
    summarise(TotalCommentCount = sum(CommentCount, na.rm=TRUE)) %>%               # podsumowujemy dane w kolumnie CommentCount i wyliczmy sumę tworząc nową kolumnę
    inner_join(Users, by = c("OwnerUserId" = "Id")) %>%                            # łączymy z2 z Users na podstawie kolumny OwnerUserId z z2 i Id z Users
    mutate(CreationDate = as.Date(CreationDate, "%Y-%m-%d")) %>%                   # tworzymy kolumne z datami wyciągniętymi z CreationDate
    select(DisplayName, Location, Reputation, CreationDate, TotalCommentCount) %>% # wybieramy cztery kolumny z których będzie składać się końcowy wynik
    arrange(desc(TotalCommentCount)) %>%                                           # sortujemy malejąco według kolumny TotalCommentCount funkcją arrange
    top_n(10)                                                                      # wybieramy pierwszych 10 wierszy po posortowaniu
}

data.table_2 <- function(Posts){
  posts1 <- as.data.table(Posts)                                                                                        # tworze tabele danych z Posts
  Answers <- posts1[PostTypeId == 2 & OwnerUserId != "", .(TotalCommentCount = sum(CommentCount)), by = OwnerUserId]    # tworzymy tabele z tej poprzedniej, wybieramy tylko odpowiednie wiersze, i tworzymy kolumne Total CommentCount oraz grupujemy po OwnerUserId gdzie ta kolumna zostanie dodana do tworzonej tabeli
  users1 <- as.data.table(Users)                                                                                        # tworzymy tabele danych z Users
  second <- merge.data.table(Answers, users1, by.x = "OwnerUserId", by.y = "Id")                                        # łączymy tabele Answers i utworzoną chwilę wcześniej na podstwaie kolumn OwnerUserId i Id 
  second <- second[, .(DisplayName, Location, Reputation, CreationDate = format(as.IDate(CreationDate), "%Y-%m-%d"), TotalCommentCount)]   # końcowy wynik zawiera wybrane kolumny i utworzoną CreationDate zaiwerającą daty
  second <- second[order(-TotalCommentCount)]                                                                           # sortujemy malejąco według kolumny TotalCommentCount
  second <- second[1:10]                                                                                                # wybieramy pierwszych 10 wierszy po posortowaniu
}

#sql2 <- sql_2(Posts)
#base2 <- base_2(Posts)
#dplyr2 <- dplyr_2(Posts)
#data.table2 <- data.table_2(Posts)

#compare::compare(sql2, base2)
#compare::compare(sql2, dplyr2)
#compare::compare(sql2, data.table2)
#microbenchmark::microbenchmark(sql2, base2, dplyr2, data.table2)


# -----------------------------------------------------------------------------#
#                         Zadanie 3
# -----------------------------------------------------------------------------#

sql_3 <- function(Posts,Users,Votes){
  sqldf("
        SELECT Spam.PostId, UsersPosts.PostTypeId, UsersPosts.Score,
                UsersPosts.OwnerUserId, UsersPosts.DisplayName,
                UsersPosts.Reputation
        FROM (
                SELECT PostId
                FROM Votes
                WHERE VoteTypeId == 12
              ) AS Spam
        JOIN (
                SELECT Posts.Id, Posts.OwnerUserId, Users.DisplayName,
                        Users.Reputation, Posts.PostTypeId, Posts.Score
                FROM Posts 
                JOIN Users ON Posts.OwnerUserId = Users.Id
              ) AS UsersPosts ON Spam.PostId = UsersPosts.Id
        ")
}
# wybieramy tylko i wyłącznie kolumne PostId
# operujemy na Votes
# filtrujemy po wierszach w VoteTypeId
# ramkę zapisujemy jako Spam
# Spam łączymy z UsersPosts (która tworzymy z Posts połączonej z Users na podstawie kolumn i z tak 
# połączonej ramki wybieramy odpowiednie kolumny) poprzez kolumny PostId oraz Id
# z tak połączonej ramki Spam z UsersPosts wybieramy odpwiednie kolumny i otrzymujemy ramkę końcową

base_3 <- function(Posts,Users,Votes){
  Spam <- Votes[Votes$VoteTypeId==12, "PostId", drop = FALSE]                   # tworzymy Spam dokładnie z takim samym opisaem jaki zastosowałem w przypadku pakietu sqldf
  Users_1 <- Users                                                              # tworzę pomocniczą rankę danych
  Users_1 <- Users_1[, c("Id","DisplayName","Reputation")]                      # z tej pomocniczej ramki wybieram parę kolumn które będa przydatne
  Posts_1 <- Posts                                                              # tworzę pomocniczą rankę danych
  Posts_1 <- Posts_1[, c("Id","OwnerUserId","PostTypeId","Score")]              # z tej pomocniczej ramki wybieram parę kolumn które będa przydatne
  UsersPosts <- merge.data.frame(Posts_1,Users_1,by.x="OwnerUserId",by.y="Id")  # UsersPosts otrzymuje łącząc obie ramki pomocnicze na podstawie identycznych danych z kolumn OwnerUserId oraz Id
  result_3 <- merge.data.frame(UsersPosts,Spam,by.x="Id",by.y="PostId")         # ramkę wynikowa przetwarzam najpierw łącząc obie ramki danych poprzez identyczne dane z kolumny Id i PostId
  names(result_3)[names(result_3)=="Id"] <- "PostId"                            # zmieniam nazwę kolumny Id na bardziej adekwatną
  result_3 <- result_3[, c(1,3,4,2,5,6)]                                        # zamieniam kolumny miejscami aby ramka wyglądała identycznie jak w pakiecie sqldf
  result_3[c(1,2), ] <- result_3[c(2,1), ]                                      # zamieniam wiersze miejscami aby ramka wyglądała identycznie jak w pakiecie sqldf
  result_3
}

dplyr_3 <- function(Posts,Users,Votes){
  Spam <- Votes %>%                                                             # tworzę Spam z Votes (zaraz bede to dalej przetwarzał)
    filter(VoteTypeId == 12) %>%                                                # filtruję Spam po odpowiednich wierszach wyznaczonych przez kolumnę VoteTypeId
    select(PostId)                                                              # wybieram jedną jedyna kolumne która znajdzie się w Spam
  UsersPosts <- Posts %>%                                                       # tworzę Spam z UsersPosts (zaraz bede to dalej przetwarzał)
    select(Id, PostTypeId, Score, OwnerUserId) %>%                              # wybieram kolumny które beda mnie dalej interesować
    inner_join(Users, by = c("OwnerUserId" = "Id"))                             # łączę UsersPosts z Users na podstawie danych z kolumn OwnerUserId i Id
  z3 <- Spam %>%                                                                # tworzę ramkę końcową z Spam (zaraz bede to dalej przetwarzał)
    inner_join(UsersPosts, by = c("PostId" = "Id")) %>%                         # łączę ramkę koncową z UsersPosts na podstawie danych z kolumn PostId i Id
    select(PostId, PostTypeId, Score, OwnerUserId, DisplayName, Reputation)     # wybieram odpowiednie kolumny aby rezultat końcowy zgadzał sie z tym z innych pakietów
}

data.table_3 <- function(Posts,Users,Votes){
  votes1 <- as.data.table(Votes)                                                                # tworze tabele danych z Votes
  Spam <- votes1[VoteTypeId == 12, .(PostId)]                                                   # tworze Spam obrabiajac odpowiednio tabele utworzoną wcześniej
  posts1 <- as.data.table(Posts)                                                                # tworze tabele danych z Posts
  users1 <- as.data.table(Users)                                                                # tworze tabele danych z Users
  UsersPosts <- merge.data.table(posts1, users1, by.x = "OwnerUserId", by.y = "Id")             # tworzę UsersPosts łącząc dwie tabele utworzone wcześniej na podstawie danych z kolumn OwnerUserId i Id 
  UsersPosts <- UsersPosts[, .(Id, OwnerUserId, DisplayName, Reputation, PostTypeId, Score)]    # kończe przewarzac UsersPosts wybierając wymagane kolumny
  third <- merge.data.table(Spam, UsersPosts, by.x = "PostId", by.y = "Id")                     # wynik końcowy jest (ale jeszcze nie do końca) połączoną tabelą ze Spam i UsersPosts na podstawie danych z kolumn PostId i Id
  third <- third[, .(PostId, PostTypeId, Score, OwnerUserId, DisplayName, Reputation)]          # wynik końcowy otrzymuję wybierając odpowiednie kolumny
}

#sql3 <- sql_3(Posts,Users,Votes)
#base3 <- base_3(Posts,Users,Votes)
#dplyr3 <- dplyr_3(Posts,Users,Votes)
#data.table3 <- data.table_3(Posts,Users,Votes)

#compare::compare(sql3, base3)
#compare::compare(sql3, dplyr3)
#compare::compare(sql3, data.table3)
#microbenchmark::microbenchmark(sql3, base3, dplyr3, data.table3)


# -----------------------------------------------------------------------------#
#                         Zadanie  4
# -----------------------------------------------------------------------------#

sql_4 <- function(Posts,Users,PostLinks){
  sqldf("
        SELECT Users.Id, Users.DisplayName, Users.UpVotes, Users.DownVotes, Users.Reputation,
        
                COUNT(*) AS DuplicatedQuestionsCount
        FROM (
                SELECT Duplicated.RelatedPostId, Posts.OwnerUserId
                FROM (
                        SELECT PostLinks.RelatedPostId
                        FROM PostLinks
                        WHERE PostLinks.LinkTypeId == 3
                      ) AS Duplicated
                JOIN Posts ON Duplicated.RelatedPostId = Posts.Id
              ) AS DuplicatedPosts
        JOIN Users ON Users.Id == DuplicatedPosts.OwnerUserId
        GROUP BY Users.Id
        HAVING DuplicatedQuestionsCount > 100
        ORDER BY DuplicatedQuestionsCount DESC
        ")
}
# z później utworzonej i przetworzonej ramki danych wybieramy i tworzymy odpowiednie kolumny
# z ramki PostLinks tworzymy ramke Duplicated wybierajac kolumne RelatedPosts
# i filtrujac wedlug odpowiednich wartosci w wierszach kolumny LinkTypeId
# łacze Duplicated z ramka danych Posts wdlug danych z kolumn RelatedPostId i Id i tak utworzona ramke nazywamy DuplicatedPosts
# łącze DuplicatedPosts z Users według identycznych danych z kolumn Id i OwnerUserId z dwóch ramek
# grupuje wedlug kolumny Id
# filtruje po wierszach w których wartość w tej kolumnie jest większa od 100
# sortuje malejąco według wartości w kolumnie DuplicatedQuestionCount

base_4 <- function(Posts,Users,PostLinks){
  Duplicated <- PostLinks[PostLinks$LinkTypeId==3, "RelatedPostId", drop=FALSE]                  # tworze ramke Duplicated
  DuplicatedPosts <- merge.data.frame(Duplicated,Posts,by.x="RelatedPostId",by.y="Id")           # tworze ramke DuplicatedPosts
  DuplicatedPosts <- merge.data.frame(Users,DuplicatedPosts,by.x="Id",by.y="OwnerUserId")        # łączye ją z ramką Users
  grouped <- aggregate(DuplicatedPosts["Id"], DuplicatedPosts["Id"], length)                     # tworze pomocnicza ramke danych z pogrupowana kolumna Id wedlug dlugosci
  names(grouped)[2] <- "DuplicatedQuestionsCount"                                                # zmieniam nazwe kolumny
  result_4 <- merge.data.frame(grouped, DuplicatedPosts, "Id")                                   # ostateczna ramke tworze łącząc te dwie ramki wedlug danych z kolumny Id
  result_4 <- result_4[result_4$DuplicatedQuestionsCount>100, ]                                  # filtruje po wierszach w których wartość w tej kolumnie jest większa od 100
  result_4 <- result_4[order(result_4$DuplicatedQuestionsCount, decreasing=TRUE), ]              # sortuje malejąco według wartości w kolumnie DuplicatedQuestionCount
  result_4 <- result_4[, c(1,5,11,12,3,2)]                                                       # wybieram kolumny które nas interesuja
  result_4 <- unique(result_4)                                                                   # wybieram tylko unikatowe wiersze 
  row.names(result_4) <- c(1:9)
  result_4
}

dplyr_4 <- function(Posts,Users,PostLinks){
  Duplicated <- PostLinks %>%                                                                    # tworzymy ramke danych Duplicated
    filter(LinkTypeId == 3) %>%
    select(RelatedPostId)
  DuplicatedPosts <- Duplicated %>%                                                              # tworzymy ramke danych DuplicatedPosts
    inner_join(Posts, by = c("RelatedPostId" = "Id")) %>%
    select(RelatedPostId, OwnerUserId)
  z4 <- DuplicatedPosts %>%                                                                      # przetwarzam ostateczna ramke danych
    inner_join(Users, by = c("OwnerUserId" = "Id")) %>%                                          # łącze ją z Users według odpowiednich kolumn
    group_by(OwnerUserId, DisplayName, UpVotes, DownVotes, Reputation) %>%                       # grupuje ramke
    summarise(DuplicatedQuestionsCount = n()) %>%                                                # tworze kolumne z sumą 
    filter(DuplicatedQuestionsCount > 100) %>%                                                   # filtruje po odpowiednich wierszach
    arrange(desc(DuplicatedQuestionsCount)) %>%                                                  # sortuje malejąco według wartości w kolumnie DuplicatedQuestionCount
    rename("Id" = "OwnerUserId")                                                                 # zmieniam nazwe kolumny
}

data.table_4 <- function(Posts,Users,PostLinks){
  postlinks1 <- as.data.table(PostLinks)                                                         # tworze pomocnicza tabele danych z ramki PostLinks
  Duplicated <- postlinks1[LinkTypeId == 3, .(RelatedPostId)]                                    # tworze Duplicated na podstawie wytycznych
  posts1 <- as.data.table(Posts)                                                                 # tworze pomocnicza tabele danych z ramki Posts
  DuplicatedPosts <- merge.data.table(Duplicated, posts1, by.x = "RelatedPostId", by.y = "Id")   # tworze DuplicatedPosts łącząc dwie tabele danych na podstawie danych w nich zgromadzonych
  users1 <- as.data.table(Users)                                                                 # tworze pomocnicza tabele danych z ramki Users
  fourth <- merge.data.table(DuplicatedPosts, users1, by.x = "OwnerUserId", by.y = "Id")         # tworze ostateczna tabele danych którą jeszcze bede przetwarzał poprzez połączenie dwóch tabel danych na podstawie odpowiednich kolumn z nich
  fourth <- fourth[, .(DisplayName, UpVotes, DownVotes, Reputation, DuplicatedQuestionsCount = .N), by = OwnerUserId]  # wybieram odpowiednie kolumny z tabeli i tworzymy nowe o odpowiednich właściwościach
  fourth <- fourth[DuplicatedQuestionsCount > 100]                                               # filtruje po odpowiednich wartościach wierszy 
  fourth <- fourth[order(-DuplicatedQuestionsCount)]                                             # sortuje malejąco według wartości w kolumnie DuplicatedQuestionCount
  fourth <- setnames(fourth, "OwnerUserId", "Id")                                                # zmieniam nazwe kolumny Id
  fourth <- unique(fourth)                                                                       # wybieram tylko unikatowe wiersze do ostatecznego wyniku
}

#sql4 <- sql_4(Posts,Users,PostLinks)
#base4 <- base_4(Posts,Users,PostLinks)
#dplyr4 <- dplyr_4(Posts,Users,PostLinks)
#data.table4 <- data.table_4(Posts,Users,PostLinks)

#compare::compare(sql4, base4)
#compare::compare(sql4, dplyr4)
#compare::compare(sql4, data.table4)
#microbenchmark::microbenchmark(sql4, base4, dplyr4, data.table4)


# -----------------------------------------------------------------------------#
#                         Zadanie 5
# -----------------------------------------------------------------------------#

sql_5 <- function(Posts,PostLinks){
  sqldf("
        SELECT QuestionsAnswers.Id,
          QuestionsAnswers.Title,
          QuestionsAnswers.Score,
          MAX(Duplicated.Score) AS MaxScoreDuplicated,
          COUNT(*) AS DulicatesCount,
          CASE
              WHEN QuestionsAnswers.Hour < '06' THEN 'Night'
              WHEN QuestionsAnswers.Hour < '12' THEN 'Morning'
              WHEN QuestionsAnswers.Hour < '18' THEN 'Day'
              ELSE 'Evening'
              END DayTime
        FROM (
                SELECT Id, Title,
                  STRFTIME('%H', CreationDate) AS Hour, Score
                FROM Posts
                WHERE Posts.PostTypeId IN (1, 2)
              ) AS QuestionsAnswers
        JOIN (
                SELECT PL3.RelatedPostId, Posts.Score
                FROM (
                        SELECT RelatedPostId, PostId
                        FROM PostLinks
                        WHERE LinkTypeId == 3
                      ) AS PL3
                JOIN Posts ON PL3.PostId = Posts.Id
              ) AS Duplicated
        ON QuestionsAnswers.Id = Duplicated.RelatedPostId
        GROUP BY QuestionsAnswers.Id
        ORDER By DulicatesCount DESC
        ")
}
# z pozniej przetworzonych ranek danych wybieram i tworze kolumny do wyniku koncowego
# zamieniam godziny na pory dnia wedlug podanego klucza
# tworze ramke danych QuestionAnswers na podstawie ramki Posts filtrujac po odpowiednich wierszach kolumny PostTypeId i wybieram wymagane kolumny i tworze nowa ktora wyjmuje godziny z wierszy kolumny CreationDate
# tworze ramke danych PL3 z ramki PostsLinks filtrujac po odpowiednich wierszach kolumny LinkTypeId i wybierajac odpowiednie klumny
# tworze ramke danych Duplicated z ramki danych PL3 łaczac ja z ramka Posts na podstawie danych z kolumn PostId i Id
# tworze ramke ktora bedzie baza dla koncowego wyniku poprzez zlaczenie ramek QuestionAnswers i Duplicated za pomoca danych z kolumn Id i RelatedPostId
# tak powstala baze grupuje na podstawie wartosci z kolumny Id
# sortuje malejaco moja baze na podstawie danych z kolumny DulicatesCount

base_5 <- function(Posts,PostLinks){
  QuestionsAnswers <- Posts[Posts$PostTypeId==1 | Posts$PostTypeId==2, ]                       # tworze QuestionAnswers poprzez filtrowanie po wierszach ramki danych Posts
  QuestionsAnswers <- QuestionsAnswers[, c("Id","Title","CreationDate","Score")]               # wybieram odpowiednie interesujące mnie kolumny
  QuestionsAnswers["CreationDate"] <- format(as.POSIXct(QuestionsAnswers$CreationDate,format="%Y-%m-%dT%H:%M:%S"),"%H")   # zmieniam kolumne CreationDate i wyciągam z niej godzine
  names(QuestionsAnswers)[names(QuestionsAnswers)=="CreationDate"] <- "Hour"                   # zmieniam nazwe zmienionej wczesniej kolumny
  PL3 <- PostLinks[PostLinks$LinkTypeId==3, c("RelatedPostId","PostId")]                       # tworze PL3 filtrując PostsLinks po odpowiednich wierszach i wybierając odpowiednie kolumny
  Duplicated <- merge.data.frame(PL3, Posts, by.x="PostId", by.y="Id")                         # tworze Duplicated łączac PL3 i Posts za pomocą identycznych danych z kolumn PostId i Id
  result_5 <- merge.data.frame(QuestionsAnswers, Duplicated, by.x="Id", by.y="RelatedPostId")  # tworze baze koncowego wyniku poprzez połączenie QuestionAnswers i Duplicated na podstawie danych z kolumn Id i relatedPostId
  result_5 <- result_5[, c("Id","Title.x","Score.x","Score.y","Hour")]                         # wybieram odpowiednie kolumny z tej bazy
  grouped_1 <- aggregate(result_5["Score.y"], result_5["Id"], max)                             # tworze pomocnicza ramke danych grupujac kolumne Score.y wdedług danych z kolumny Id według funckji max
  result_5 <- merge.data.frame(grouped_1, result_5, by="Id")                                   # baze zmieniam tak ze lacze pomocnicza tabele z ta baza wedlug danych z kolumny Id
  result_5["Score.y.y"] <- NULL                                                                # usuwam kolumne Score.y.y
  names(result_5)[names(result_5)=="Score.y.x"] <- "MaxScoreDuplicated"                        # zmieniam nazwe kolumny
  names(result_5)[names(result_5)=="Score.x"] <- "Score"                                       # zmieniam nazwe kolumny
  names(result_5)[names(result_5)=="Title.x"] <- "Title"                                       # zmieniam nazwe kolumny
  grouped_2 <- aggregate(result_5$Id, result_5["Id"], length)                                  # tworze druga pomocnicza ramke danych grupujac kolumne Id wedlug kolumny tej samej i funkcji length
  result_5 <- merge.data.frame(grouped_2, result_5, by="Id")                                   # znow zmieniam baze 
  names(result_5)[names(result_5)=="x"] <- "DulicatesCount"                                    # zmieniam nazwe kolumny
  result_5 <- unique(result_5)                                                                 # wybieram tylko unikatowe wiersze
  result_5 <- result_5[, c(1,4,5,3,2,6)]                                                       # zmieniam kolejność kolumn
  result_5 <- result_5[order(result_5$DulicatesCount, decreasing = TRUE), ]                    # sortuje przetworzona baze malejaco wedlug wartosci z wierszy kolumny DulicatesCount
  result_5$Hour <- as.numeric(result_5$Hour)                                                   # dalej przetwarzam kolumne Hour tak aby wedlug wartosci godzin jakie w niej otrzymalem zamienic je na pore dnia wedlug podanych wytycznych
  div <- c(-Inf,6,12,18,Inf)
  names <- c("Night","Morning","Day","Evening")
  result_5$Hour <- cut(result_5$Hour, breaks=div, labels=names, right=FALSE)
  names(result_5)[names(result_5)=="Hour"] <- "DayTime"                                        # zmieniam nazwe kolumny
  row.names(result_5) <- c(1:2039)                                                             # zmieniam nazwe wierszy
  result_5                                                                                     # tak przetworzona baza to koncowy wynik
}

dplyr_5 <- function(Posts,PostLinks){
  Hour <- format(as.POSIXct(Posts$CreationDate,format="%Y-%m-%dT%H:%M:%S"),"%H")               # tworze wektor Hour zawierajacy wartosci godzin wyciagniete z kolumny CreationDate  z ramki danych Posts
  QuestionsAnswers <- bind_cols(Posts, Hour = Hour) %>%                                        # tworze ramke QuestionsAnswers (do Posts dodaje nowa kolumne z wartosciami z Hour) wedlug wytycznych z zadania
    filter(PostTypeId == 1 | PostTypeId == 2) %>%
    select(Id, Title, Hour, Score)
  PL3 <- PostLinks %>%                                                                         # tworze ramke PL3 wedlug wytycznych z zadania  
    filter(LinkTypeId == 3) %>%
    select(RelatedPostId, PostId)
  Duplicated <- PL3 %>%                                                                        # tworze ramke Duplicated wedlug wytyczych z zadania
    inner_join(Posts, by = c("PostId" = "Id")) %>%
    select(RelatedPostId, Score)
  QuestionsAnswers$DayTime <- if_else(QuestionsAnswers$Hour<'06',"Night",if_else(QuestionsAnswers$Hour<'12',"Morning",if_else(QuestionsAnswers$Hour<'18','Day','Evening')))    # tworze kolumne w QuestionAnswers ktora przekonwertowuje wartosci z Hour na pore dnia wedlug wytycznych z zadania
  z5 <- QuestionsAnswers %>%                                                                   # tworze baze dla wyniku koncowego
    inner_join(Duplicated, by = c("Id" = "RelatedPostId")) %>%                                 # lacze ja z uplicated z apomoca danych z odpowiednich kolumn 
    select(Id, Title, Score.x, Score.y, DayTime) %>%                                           # wybieram interesujace mnie kolumny    
    group_by(Id) %>%                                                                           # grupuje po wartosciach z kolumny Id
    mutate(MaxScoreDuplicated = max(Score.y, na.rm = TRUE), DulicatesCount = n()) %>%          # tworze nowe kolumney na podstawie wytycznych z tresci zadania
    select(-Score.y) %>%                                                                       # usuwam zbedna kolumne
    rename("Score" = "Score.x") %>%                                                            # zmieniam nazwe kolumny
    select(Id, Title, Score, MaxScoreDuplicated, DulicatesCount, DayTime) %>%                  # wybieram do ostatecznego wyniku tylko potrzebne kolumny
    arrange(desc(DulicatesCount)) %>%                                                          # sortuje malejaco po wartosciach z odpowiedniej kolumny
    distinct()                                                                                 # wybieram tylko unikatowe wiersze
}

data.table_5 <- function(Posts,PostLinks){
  Hour <- format(as.POSIXct(Posts$CreationDate,format="%Y-%m-%dT%H:%M:%S"),"%H")               # tworze wektor Hour zawierajacy wartosci godzin wyciagniete z kolumny CreationDate  z ramki danych Posts
  posts1 <- as.data.table(Posts)                                                               # tworze tabele danych z ramki danych Posts
  QuestionsAnswers <- posts1[, .(Id, PostTypeId, Title, Hour = Hour, Score)]                   # wybieram interesujace mnie kolumny z wczesniej utworzone tabeli i na tej podstawie tworze QuestionAnswers
  QuestionsAnswers <- QuestionsAnswers[PostTypeId == 1 | PostTypeId == 2]                      # filtruje po odpowiednich wierszach
  QuestionsAnswers[, DayTime := ifelse(Hour < '06', 'Night', ifelse(Hour < '12', 'Morning', ifelse(Hour < '18', 'Day', 'Evening')))] # tworze kolumne w QuestionAnswers ktora przekonwertowuje wartosci z Hour na pore dnia wedlug wytycznych z zadania
  QuestionsAnswers[, c("PostTypeId", "Hour") := NULL]                                          # usuwam niepotrzebne kolumny
  postlinks1 <- as.data.table(PostLinks)                                                       # tworze tabele danych z ranki danych PostsLinks
  PL3 <- postlinks1[LinkTypeId == 3, .(RelatedPostId, PostId)]                                 # tworze PL3 na podstawie wytycznych z zadania
  Duplicated <- merge.data.table(PL3, posts1, by.x= "PostId", by.y = "Id")                     # tworze Duplicated na podstawie wytycznych z zadania
  Duplicated <- Duplicated[, .(RelatedPostId, Score)]                                          # wybieram z tego interesujace mnie kolumny
  fifth <- merge.data.table(QuestionsAnswers, Duplicated, by.x = "Id", by.y = "RelatedPostId") # tworze baze pod koncowy wynik laczac dwie tabele danych na podstawie danych z odpowiednich kolumn
  fifth <- fifth[, .(Title, Score.x, MaxScoreDuplicated = max(Score.y, na.rm = TRUE), DulicatesCount = .N, DayTime), by = Id]   # wybieram i tworze wymagane kolumny 
  fifth <- setnames(fifth, "Score.x", "Score")                                                 # zmieniam nazwe kolumny
  fifth <- fifth[order(-DulicatesCount)]                                                       # sortuje malejaco po wartosciach z odpowiedniej kolumny
  fifth <- unique(fifth)                                                                       # wybieram tylko unikatowe wiersze
}

#sql5 <- sql_5(Posts,PostLinks)
#base5 <- base_5(Posts,PostLinks)
#dplyr5 <- dplyr_5(Posts,PostLinks)
#data.table5 <- data.table_5(Posts,PostLinks)

#dplyr::all_equal(base5, sql5)
#dplyr::all_equal(dplyr5, sql5)
#dplyr::all_equal(data.table5, sql5)
#microbenchmark::microbenchmark(sql5, base5, dplyr5, data.table5)

