#!/usr/bin/python

import json
import os
import pandas as pd
import numpy as np
import re
import boto3
import tweepy
from boto3.dynamodb.conditions import Key, Attr
from datetime import datetime
from textblob import TextBlob
from wordcloud import WordCloud, STOPWORDS
from decimal import Decimal
from Tweet_Metrics import TweetMetrics
import time as tim

now = datetime.now()

sent_df = pd.read_csv("Sentiment_Dict2.csv")
scores_dict1 = {}
for index, row in sent_df.iterrows():
    scores_dict1[row['word']] = int(row['score'])

sentiment_file = "AFINN-111.txt"
afinnfile = open(sentiment_file)
scores_dict2 = {} # initialize an empty dictionary
for line in afinnfile:
    term, score  = line.split("\t")  # The file is tab-delimited. "\t" means "tab character"
    scores_dict2[term] = int(score)

# Twitter Consumer Key:
consumer_key="xxxxxxxxxxxxxxxxxxxxxx"
consumer_secret="xxxxxxxxxxxxxxxxxxxxxx"

# Twitter Access Token:
access_token="xxxxxxxxxxxxxxxxxxxxxx"
access_token_secret="xxxxxxxxxxxxxxxxxxxxxx"

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)

def pull_tweets(user_list, db1='Byzantium', time=now):
    print(str(time))
    users_dict = {}
    dynamodb = boto3.resource('dynamodb', region_name='us-east-1')
    table = dynamodb.Table('Byzantium')
    for user in user_list:
        print(user)
        count = 0
        tweets = api.user_timeline(screen_name=user,
                               count=25, # 200 is the maximum allowed count
                               include_rts = False,
                               # Necessary to keep full_text
                               # otherwise only the first 140 words are extracted
                               tweet_mode = 'extended')

        for t in tweets:
            #if we already have the tweet move on
            if table.query(KeyConditionExpression= Key('username').eq(user) & Key('tweet_id').eq(str(t.id)))['Count'] != 0:
                continue
            #if we don't have the tweet throw it in the db
            else:
                count += 1
                user_id = str(t.user.id)
                retweet_count = t.retweet_count
                favorite_count = t.favorite_count
                follower_count = t.user.followers_count
                tweet_text = t.full_text
                time = t.created_at
                username = user
                location = t.user.location
                tweet_id = str(t.id)

                try:
                    at = re.findall(r'@([^@ ]+)[\s,;]*', tweet_text)
                    hash_tag = re.findall(r'#([^# ]+)[\s,;]*', tweet_text)

                    if len(at) == 0:
                        at = ['no @s']

                    if len(hash_tag) ==0:
                        hash_tag = ['no #s']

                except:
                    at = ['no @s']
                    hash_tag = ['no #s']

                #sentiment score:
                score1 = 0
                score2 = 0
                words = re.findall(r'\w+', tweet_text)
                score3 =  round(Decimal(TextBlob(tweet_text).sentiment[0]),3)
                for word in words:
                    if scores_dict1.get(word):
                        score1 = score1 + scores_dict1[word]
                    if scores_dict2.get(word):
                        score2 = score2 + scores_dict2[word]

                # build tweet data for database upload
                tweet = {"username": username,
                         "tweet_id": str(tweet_id),
                         "user_id": str(user_id),
                         "retweet_count": retweet_count,
                         "favorite_count":favorite_count,
                         "follower_count":follower_count,
                         "text": tweet_text,
                         "hash_tags": hash_tag,
                         "ats": at,
                         "location": location,
                         "time": str(time),
                         "score1": score1,
                         "score2": score2,
                         "score3": score3}

                #upload to database
                dynamodb = boto3.resource('dynamodb', region_name='us-east-1')
                table = dynamodb.Table(db1)
                table.put_item(
                    Item={ "username": tweet['username'],
                           "tweet_id": tweet['tweet_id'],
                           "user_id": tweet['user_id'],
                           "reweet_count": tweet['retweet_count'],
                           "favorite_count": tweet['favorite_count'],
                           "follower_count": tweet['follower_count'],
                           "text": tweet['text'],
                           "hash_tags": tweet['hash_tags'],
                           "ats": tweet['ats'],
                           "location": tweet['location'],
                           "time": tweet['time'],
                           "score1": tweet['score1'],
                           "score2": tweet['score2'],
                           "score3": tweet['score3']})
                           #"all":t})

        users_dict[user] = count

    print(str(users_dict))

    return users_dict, now


if __name__ == '__main__':

    users = ['POTUS', 'JoeBiden',
         'WhiteHouse',
         'VP', 'KamalaHarris',
         'McConnellPress', 'LeaderMcConnell',
         'SenSchumer', 'chuckschumer',
         'TeamPelosi', 'SpeakerPelosi',
         'AOC', 'RepAOC',
         'RepDanCrenshaw', 'DanCrenshawTX ',
         'tedcruz', 'SenTedCruz']

    db = 'Byzantium'

    users_dict, time = pull_tweets(users, db)

    #daily tracking
    with open(str(time)+'.txt', 'w') as f:
        f.write(str(users_dict))
        f.write('\n')
        f.write(str(time))

    #update the data for the dashboard
    os.chdir("/home/ec2-user/Byz/Dashboard")
    test = TweetMetrics('POTUS').df
    for user in users:
        if user != 'POTUS':
            df = TweetMetrics(user).df
            test = test.append(df)
            tim.sleep(10)

    os.chdir("/home/ec2-user/Byz/Dashboard")
    test.to_csv("results.csv")
