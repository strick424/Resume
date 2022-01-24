import json
import os
import pandas as pd
import numpy as np
import re
import boto3
from boto3.dynamodb.conditions import Key, Attr
from datetime import datetime
from textblob import TextBlob
from wordcloud import WordCloud, STOPWORDS
from decimal import Decimal
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.pyplot import text
from matplotlib.pyplot import figure
import nbformat as nbf

class TweetMetrics:
    
    def __init__(self, username):
        self.username = username
        #print(self.username)
        self.db = 'Byzantium'
        #self.df = df
        self.df = self.build_df()
        self.max_retweet = max(self.df['retweet_count'])
        self.max_retweet_text = self.df[self.df['retweet_count'] == self.max_retweet]['text'].all()
        self.max_favorite = max(self.df['favorite_count'])
        self.max_favorite_text = self.df[self.df['favorite_count'] == self.max_favorite]['text'].all()  
        self.follower_count = max(self.df['follower_count'])
        self.avg_daily_tweet = int(self.df.groupby(['month', 'day']).count()['username'].mean())
        self.avg_monthly_tweet = int(self.df.groupby(['month']).count()['username'].mean())
        self.top_tweeted_day = self.df.groupby(['month', 'day']).count()['username'].idxmax()
        self.top_tweeted_day_count = self.df.groupby(['month', 'day']).count()['username'].max()
        self.top_tweeted_month = self.df.groupby(['month']).count()['username'].idxmax()
        self.top_tweeted_month_count = self.df.groupby(['month']).count()['username'].max()
        self.output_metrics()
        #self.compile_report()
        
    def query_db(self):
        dynamodb = boto3.resource('dynamodb', region_name='us-east-1')
        table = dynamodb.Table(self.db)  #CHANGE ME
        scan_kwargs = {'FilterExpression': Attr('username').eq(self.username)}

        done = False
        start_key = None
        tweet_list = []
        while not done:
            if start_key:
                scan_kwargs['ExclusiveStartKey'] = start_key
            response = table.scan(**scan_kwargs)
            tweet_list.append(response['Items'])
            start_key = response.get('LastEvaluatedKey', None)
            done = start_key is None
        tweet_list = sum(tweet_list, [])

        return tweet_list
    
    def build_df(self):
        username = []
        tweet_id = []
        user_id = []
        retweet_count = []
        favorite_count = []
        follower_count = []
        text = []
        t = []
        hash_tag = []
        at = []
        location = []
        time = []
        year = []
        month = []
        month_name = []
        day = []
        score1 = [] 
        score2 = []
        score3 = []
        month_l = [0, 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
        db = self.query_db()
        
        for data in db:
            username.append(data['username'])
            tweet_id.append(data['tweet_id'])
            user_id.append(data['user_id'])
            retweet_count.append(data['reweet_count'])
            if data['favorite_count'] == 0:
                favorite_count.append(1)
            else:
                favorite_count.append(data['favorite_count'])
            follower_count.append(data['follower_count'])
            text.append(str(data['text']))
            t.append(data['text'][0:15])
            hash_tag.append(data['hash_tags'])
            at.append(data['ats'])
            location.append(data['location'])
            time.append(data['time'])
            d = datetime.fromisoformat(data['time'])
            year.append(int(d.year))
            month_name.append(month_l[int(d.month)])
            month.append(d.month)
            day.append(int(d.day))
            score1.append(float(data['score1']))
            score2.append(round(float(data['score2']),2))
            score3.append(round(float(data["score3"]),2))

        byz_df = pd.DataFrame(columns = ["username", 
                                         "tweet_id", 
                                         "user_id", 
                                         "retweet_count", 
                                         "favorite_count", 
                                         "follower_count", 
                                         "text", 
                                         "hash_tags", 
                                         "ats", 
                                         "location", 
                                         "time", 
                                         "year",
                                         "month",
                                         "day",
                                         "score1", 
                                         "score2", 
                                         "score3"])

        byz_df["username"] = username 
        byz_df["tweet_id"] = tweet_id
        byz_df["user_id"] = user_id
        byz_df["retweet_count"] = retweet_count
        byz_df["favorite_count"] = favorite_count
        byz_df["follower_count"] = follower_count
        byz_df["text"] = text
        byz_df["t"] = t
        byz_df["hash_tags"] = hash_tag
        byz_df["ats"] = at
        byz_df["location"] = location
        byz_df["time"] = time
        byz_df["year"] = year
        byz_df['month_name'] = month_name
        byz_df["month"] = month
        byz_df["day"] = day
        byz_df["score1"] = score1 
        byz_df["score2"] = score2
        byz_df["score3"] = score3
        
        return byz_df
    
    def output_metrics(self):
        metric_df = pd.DataFrame(columns = ['Item', 'Count', 'Details'])
        item_list = ['Most Retweeted Tweet', 'Most Favorited Tweet', 'Avg Tweets per Day', 'Avg Tweets per Month']
        count_list = [self.max_retweet, self.max_favorite, self.avg_daily_tweet, self.avg_monthly_tweet]
        month = [0, 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
        details_list = [self.max_retweet_text, 
                        self.max_favorite_text, 
                        'Top Tweeted Day: ' + month[self.top_tweeted_day[0]] + ', ' + str(self.top_tweeted_day[1]) + ' with ' + str(self.top_tweeted_day_count) + ' tweets.',
                        'Top Tweeted Month: ' + month[self.top_tweeted_month] + ' with ' + str(self.top_tweeted_month_count) + ' tweets.']


        metric_df['Item'] = item_list
        metric_df['Count'] = count_list
        metric_df['Details'] = details_list
        metric_df
        metric_df = metric_df.reset_index(drop=True)
        #h = metric_df.to_html()
        #text_file = open(self.username + ".html", "w", encoding="utf-8-sig")
        #text_file.write(h)
        #text_file.close() 
        
        return metric_df