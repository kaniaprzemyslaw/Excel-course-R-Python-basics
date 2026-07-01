#!/usr/bin/env python
# coding: utf-8

# **Przemysław Kania**
#
# __Rozwiązanie pracy domowej nr 5__


import sqlite3
import pandas as pd
import numpy as np
import os, os.path

Posts = pd.read_csv('./travel_stackexchange_com/Posts.csv.gz', compression='gzip')
Comments = pd.read_csv('./travel_stackexchange_com/Comments.csv.gz', compression='gzip')
PostLinks = pd.read_csv('./travel_stackexchange_com/PostLinks.csv.gz', compression='gzip')
Users = pd.read_csv('./travel_stackexchange_com/Users.csv.gz', compression='gzip')
Votes = pd.read_csv('./travel_stackexchange_com/Votes.csv.gz', compression='gzip')

SCIEZKA_BAZY = './pd5_baza.db'

with sqlite3.connect(SCIEZKA_BAZY) as conn:
    Posts.to_sql("Posts", conn, if_exists='replace')
    Comments.to_sql("Comments", conn, if_exists='replace')
    PostLinks.to_sql("PostLinks", conn, if_exists='replace')
    Users.to_sql("Users", conn, if_exists='replace')
    Votes.to_sql("Votes", conn, if_exists='replace')


zapytanie_1 = """
SELECT STRFTIME('%Y', CreationDate) AS Year, 
       STRFTIME('%m', CreationDate) AS Month, 
       COUNT(*) AS TotalAccountsCount, 
       AVG(Reputation) AS AverageReputation
FROM Users
GROUP BY Year, Month
"""

zapytanie_2 = """
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
"""

zapytanie_3 = """
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
        FROM Posts JOIN Users
        ON Posts.OwnerUserId = Users.Id
     ) AS UsersPosts 
ON Spam.PostId = UsersPosts.Id
"""

zapytanie_4 = """
SELECT Users.Id, Users.DisplayName, Users.UpVotes, Users.DownVotes, Users.Reputation,
       COUNT(*) AS DuplicatedQuestionsCount
FROM (
        SELECT Duplicated.RelatedPostId, Posts.OwnerUserId
        FROM (
                SELECT PostLinks.RelatedPostId
                FROM PostLinks
                WHERE PostLinks.LinkTypeId == 3
             ) AS Duplicated
        JOIN Posts
        ON Duplicated.RelatedPostId = Posts.Id
     ) AS DuplicatedPosts
JOIN Users ON Users.Id == DuplicatedPosts.OwnerUserId
GROUP BY Users.Id
HAVING DuplicatedQuestionsCount > 100
ORDER BY DuplicatedQuestionsCount DESC
"""

zapytanie_5 = """
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
"""

with sqlite3.connect(SCIEZKA_BAZY) as conn:
    sql_1 = pd.read_sql_query(zapytanie_1, conn)
    sql_2 = pd.read_sql_query(zapytanie_2, conn)
    sql_3 = pd.read_sql_query(zapytanie_3, conn)
    sql_4 = pd.read_sql_query(zapytanie_4, conn)
    sql_5 = pd.read_sql_query(zapytanie_5, conn)


# ### Zadanie 1

try:
    Users['CreationDate'] = pd.to_datetime(Users['CreationDate'], format="%Y-%m-%dT%H:%M:%S.%f")
    Users['Year'] = Users['CreationDate'].dt.strftime('%Y')
    Users['Month'] = Users['CreationDate'].dt.strftime('%m')
    pandas_1 = Users.groupby(['Year', 'Month']).agg(TotalAccountsCount=('Year', 'size'), AverageReputation=('Reputation', 'mean')).reset_index()
    print(pandas_1.equals(sql_1))

except Exception as e:
    print("Zad. 1: niepoprawny wynik.")
    print(e)


# ### Zadanie 2

try:
    Users['CreationDate'] = pd.to_datetime(Users['CreationDate'], format="%Y-%m-%dT%H:%M:%S.%f")
    Answers = Posts[(Posts['PostTypeId'] == 2) & (Posts['OwnerUserId'] != '')]
    total_comm = Answers.groupby('OwnerUserId')['CommentCount'].sum().reset_index()
    total_comm.columns = ['OwnerUserId', 'TotalCommentCount']
    merged_data_frames_in_ass_2 = pd.merge(Users, total_comm, left_on='Id', right_on='OwnerUserId')
    pandas_2 = merged_data_frames_in_ass_2.sort_values(by='TotalCommentCount', ascending=False).head(10).reset_index(drop=True)
    pandas_2 = pandas_2[['DisplayName', 'Location', 'Reputation', 'CreationDate', 'TotalCommentCount']]
    pandas_2['CreationDate'] = pandas_2['CreationDate'].dt.strftime('%Y-%m-%d')
    print(pandas_2.equals(sql_2))

except Exception as e:
    print("Zad. 2: niepoprawny wynik.")
    print(e)


# ### Zadanie 3

try:
    Spam = Votes[Votes['VoteTypeId'] == 12]['PostId']
    UsersPosts = pd.merge(Posts, Users, left_on='OwnerUserId', right_on='Id', suffixes=('_post', '_user'))
    UsersPosts = UsersPosts[['Id_post', 'OwnerUserId', 'DisplayName', 'Reputation', 'PostTypeId', 'Score']]
    pandas_3 = pd.merge(Spam, UsersPosts, left_on='PostId', right_on='Id_post')
    pandas_3 = pandas_3[['PostId', 'PostTypeId', 'Score', 'OwnerUserId', 'DisplayName', 'Reputation']]
    print(pandas_3.equals(sql_3))

except Exception as e:
    print("Zad. 3: niepoprawny wynik.")
    print(e)


# ### Zadanie 4

try:
    Duplicated = PostLinks[PostLinks['LinkTypeId'] == 3]['RelatedPostId']
    DuplicatedPosts = pd.merge(Duplicated, Posts, left_on='RelatedPostId', right_on='Id', suffixes=('_links', '_post'))
    DuplicatedPosts = DuplicatedPosts[['RelatedPostId', 'OwnerUserId']]
    DuplicatedQuestionsCounts = DuplicatedPosts.groupby('OwnerUserId').size().reset_index(name='DuplicatedQuestionsCount')
    filtered_users = DuplicatedQuestionsCounts[DuplicatedQuestionsCounts['DuplicatedQuestionsCount'] > 100]
    pandas_4 = pd.merge(filtered_users, Users, left_on='OwnerUserId', right_on='Id', suffixes=('_count', '_user'))
    pandas_4 = pandas_4[['Id', 'DisplayName', 'UpVotes', 'DownVotes', 'Reputation', 'DuplicatedQuestionsCount']]
    pandas_4 = pandas_4.sort_values(by='DuplicatedQuestionsCount', ascending=False).reset_index(drop=True)
    print(pandas_4.equals(sql_4))

except Exception as e:
    print("Zad. 4: niepoprawny wynik.")
    print(e)


# ### Zadanie 5

def hour_day_time(hour):
    if hour < 6:
        return 'Night'
    elif hour < 12:
        return 'Morning'
    elif hour < 18:
        return 'Day'
    else:
        return 'Evening'

try:
    QuestionsAnswers = Posts[(Posts['PostTypeId'] == 1) | (Posts['PostTypeId'] == 2)][['Id', 'Title', 'CreationDate', 'Score']]
    QuestionsAnswers['Hour'] = pd.to_datetime(QuestionsAnswers['CreationDate'], format="%Y-%m-%dT%H:%M:%S.%f").dt.hour
    PL3 = PostLinks[PostLinks['LinkTypeId'] == 3][['RelatedPostId', 'PostId']]
    Duplicated = pd.merge(PL3, Posts, left_on='PostId', right_on='Id', suffixes=('_links', '_post'))[['RelatedPostId', 'Score']]
    merged_data_frames_in_ass_5 = pd.merge(QuestionsAnswers, Duplicated, left_on='Id', right_on='RelatedPostId', suffixes=('_xyz', '_duplicate'))
    grouped_data_frames_in_ass_5 = merged_data_frames_in_ass_5.groupby(['Id', 'Title', 'Score_xyz', 'Hour']).agg(MaxScoreDuplicated=('Score_duplicate', 'max'), DulicatesCount=('RelatedPostId', 'count')).reset_index()
    grouped_data_frames_in_ass_5['DayTime'] = grouped_data_frames_in_ass_5['Hour'].apply(hour_day_time)
    pandas_5 = grouped_data_frames_in_ass_5.sort_values(by=['DulicatesCount', 'Id'], ascending=[False, False]).reset_index(drop=True)
    pandas_5 = pandas_5.drop('Hour', axis=1)
    pandas_5 = pandas_5.rename(columns={'Score_xyz': 'Score'})
    print(pandas_5.equals(sql_5))

except Exception as e:
    print("Zad. 5: niepoprawny wynik.")
    print(e)