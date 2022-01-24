import dash
import dash_core_components as dcc
import dash_html_components as html
import dash_bootstrap_components as dbc
from dash.dependencies import Input, Output
import pandas as pd
import numpy as np
import re
import os
from datetime import datetime, timedelta
from datetime import date
from Tweet_Metrics import TweetMetrics
from wordcloud import WordCloud, STOPWORDS
import plotly.express as px
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.pyplot import text
from matplotlib.pyplot import figure


df = pd.read_csv('results.csv')
#df = TweetMetrics('POTUS').df

users = ['POTUS', 'JoeBiden', 
         'WhiteHouse',
         'VP', 'KamalaHarris',
         'McConnellPress', 'LeaderMcConnell',
         'SenSchumer', 'chuckschumer',
         'TeamPelosi', 'SpeakerPelosi',
         'AOC', 'RepAOC', 
         'RepDanCrenshaw', 'DanCrenshawTX ',
         'tedcruz', 'SenTedCruz']

def get_db_info(time_now):
    db_write_times = [1900]
    current_time = time_now.strftime("%m/%d/%Y %H:%M:%S")
    next_write_time = (time_now + timedelta(hours=-24)).strftime("%m/%d/%Y %H:%M:%S")
    if current_time[0:10] != next_write_time[0:10]:
        last_write = (time_now + timedelta(hours=-24)).strftime("%m/%d/%Y %H:%M:%S")[0:10] + " " + "19:00"
        next_write = current_time[0:10] + " " + "19:00"
        info = {'Last Write': last_write, 
                'Next Write': next_write}
    else:
        last_write = current_time[0:10] + " " + "19:00"
        next_write = current_time[0:10] + " " + "19:00"
        info = {'Last Write': last_write, 
                'Next Write': next_write}
    return info

def treemap(d, username):
    plot_df = d[d['username']==username]
    plot_df=plot_df.rename(columns = {'score1':'Sentiment Score'})
    plot_df.text = plot_df.text.str.wrap(100)
    plot_df.text = plot_df.text.apply(lambda x: x.replace('\n', '<br>'))

    fig = px.treemap(plot_df, path=["month_name", 'day', 'text'], values='favorite_count',
                      color='Sentiment Score', hover_data=['Sentiment Score'],
                      color_continuous_scale='rdylgn',
                      color_continuous_midpoint=0, template='ggplot2')

    fig.update_traces(hovertemplate="%{label} <br><br>Favorite Count: %{value} </br>Sentiment Score: %{color}") 
    fig.update_layout(title = f"{username} Tweets <br><sup>(Click box to zoom in, box size based on favorite count)</sup>")
    fig.update_layout(margin = dict(t=60, l=25, r=25, b=0))
    return fig
    
def time_series_tweets(d, username):
    date_count_dict = {}
    df1 = d[d['username']==username]
    for index, row in df1.iterrows():
        year = datetime.fromisoformat(row['time']).year
        month = datetime.fromisoformat(row['time']).month
        day = datetime.fromisoformat(row['time']).day
        date = datetime(year, month, day)
        try: 
            date_count_dict[date] += 1
        except:
            date_count_dict[date] = 1

    plot_df = pd.DataFrame(columns = ['Date', 'Tweet Count'])
    plot_df['Date'] = date_count_dict.keys()
    plot_df['Tweet Count'] = date_count_dict.values()
    fig = px.line(plot_df, x="Date", y="Tweet Count", 
                  title=f"{username} Tweet Frequency <br><sup>(Click and drag to zoom in, double-click to zoom out)</sup><br>", 
                  template="ggplot2")
    fig.update_traces(line_color='#201e1e')
    fig.update_layout(margin = dict(t=60, l=25, r=25, b=0))
    fig.update_yaxes(range=[0, max(plot_df['Tweet Count'])+8])
    return fig

def word_counts(d, username, qty):
    user_df = d[d['username']==username]
    stopwords = set(STOPWORDS)
    stopwords.update(["https", "bif","BIF", "co", "s", "amp", "t", "w", "re", "ve", "BBB", 
                      "don", "I", "will", "We", "—","The", "My", "-", "&", "bbb", "It's", 'w/'])
    text = ''.join(t for t in user_df.text)
    text = text.strip()
    str_list = text.split()
    REPLACE_APS = re.compile(r"[\[&\+\’\'\"\]\ \. \-\#\@\–]")
    
    # gives set of unique words
    unique_words = set(str_list)
    
    word_list = []
    word_count = []
    for word in unique_words:
        word = word.strip()
        word = REPLACE_APS.sub("", word)
        if word.lower() not in stopwords and word.lower() not in word_list:
            word_list.append(word.lower())
            word_count.append(str_list.count(word))
    plot_df = pd.DataFrame(columns = ['Word', 'Count'])
    plot_df['Word'] = word_list
    plot_df['Count'] = word_count
    plot_df = plot_df.sort_values(by=['Count'], ascending=False)
    plot_df = plot_df[0:qty]
    fig = px.bar(plot_df, x="Count", y="Word", orientation='h', 
                 title=f"{username} Word Frequency", template="ggplot2")
    fig.update_traces(marker=dict(color='#201e1e'))   #(paper_bgcolor = '#201e1e'
    fig.update_yaxes(dict(autorange="reversed"))
    fig.update_layout(yaxis_title=None, yaxis = dict(tickfont = dict(size=10)))
    return fig

def plot_ats_or_hashtags(d, username, select, qty):
    if select == "@":
        item_listing = d[d['username']==username].ats
    elif select == "#":
        item_listing = d[d['username']==username].hash_tags
    REPLACE_APS = re.compile(r"[\[&\+\’\'\"\]\ \. \-\#\@\:\\]")
    items = ','.join(REPLACE_APS.sub("", str(item).lower().strip()) for item in item_listing)
    items = items.split(",")
    unique_items = set(items)
    items_list = []
    items_count = []
    for item in unique_items:
        if item != "nos" and len(item)!=0 :
            items_list.append(item)
            items_count.append(items.count(item))
    plot_df = pd.DataFrame(columns = ['Items', 'Count'])
    plot_df['Items'] = items_list
    plot_df['Count'] = items_count
    plot_df = plot_df.sort_values(by=['Count'], ascending=False)
    plot_df = plot_df[0:qty]
    fig = px.bar(plot_df, x="Count", y="Items", orientation='h', 
                     title=f"{username} Tag Frequency", template="ggplot2")
    fig.update_traces(marker=dict(color='#201e1e')) 
    fig.update_layout(margin = dict(t=60, l=25, r=25, b=0))
    fig.update_yaxes(dict(autorange="reversed"))
    fig.update_layout(yaxis_title=None, yaxis = dict(tickfont = dict(size=10)))
    return fig

app = dash.Dash(__name__)

colors = {
    'background': '#201e1e',
    'background2': '#eae8d5',
    'background3': '#dfdddd',
    'text': '#a19d7e', 
    'text2':'#201e1e',
    'text3': '#ffffff'
}

db_info = get_db_info(datetime.now())

app = dash.Dash(external_stylesheets = [dbc.themes.BOOTSTRAP])
app.layout = html.Div([
    #First Row
    html.Div(style={'backgroundColor': colors['background'],'margin-top': '1vw', 'margin-bottom': '1vw'}, 
             children = [
            html.H2(children='Exploratory Analysis of Congressional Tweets',style={
                'textAlign': 'center',
                'color': colors['text'],
                'fontSize':56}#, 'font-family':'Lato'}
                    ),
            html.P(f"Last write to db: {db_info['Last Write']} EST ----  Next write to db: {db_info['Next Write']} EST", style={
                'textAlign': 'left',
                'color': colors['text'], 'fontSize':12,}# 'font-family':'Lato'}
                    ),
    ]),
    #Second Row
    html.Div(children = [
        #First Column
        html.Div(style={'backgroundColor': colors['background2'], 'display':'inline-block',
                        'vertical-align': 'top', 'width':'15%',  'margin-left': '1vw'}, 
                children=[
                html.H3(children='Select a Name:', style={
                    'textAlign': 'Center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':24, 'display':'inline-block'}#, 'font-family':'Lato'}
                ),
                dcc.Dropdown(id='username-dropdown',
                             options=[{'label': i, 'value': i} for i in users],
                                        value='POTUS', 
                                        style={'backgroundColor': colors['background3'],
                                        'fontSize':16, 'display': 'block'}#,'font-family':'Lato', }
                ),    
        ]), 
        #Second Column
        html.Div(style={'backgroundColor': colors['background2'], 'display': 'inline-block', 
                        'width':'20%', 'margin-left': '1vw'},
                children=[        
                html.Div([
                    html.H4(children='Number of Followers', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':16}
                      ),
                    html.H4(id='follower_count', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':24}# 'font-family':'Lato'}
                      )
                ])
        ]),
        
        #Third Column
        html.Div(style={'backgroundColor': colors['background2'], 'display': 'inline-block', 
                        'width':'20%', 'margin-left': '5px'},
                     children=[ 
                html.Div([
                    html.H4(children='Average Tweets per Day', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':16}
                      ),
                    html.H4(id='avg_daily_tweet', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':24}# 'font-family':'Lato'}
                      )
                ])
        ]),
        
        #Fourth Column
        html.Div(style={'backgroundColor': colors['background2'], 'display': 'inline-block', 
                        'width':'20%', 'margin-left': '5px'},
                     children=[ 
                html.Div([
                    html.H4(children='Average Tweets per Month', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':16}
                      ),
                    html.H4(id='avg_monthly_tweet', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':24}# 'font-family':'Lato'}
                      )
                ])
        ]),
        
        #Fifth Column
        html.Div(style={'backgroundColor': colors['background2'], 'display': 'inline-block', 
                        'width':'20%', 'margin-left': '5px', 'margin-right': '1vw'},
                     children=[ 
                html.Div([
                    html.H4(children='Highest Tweeted Day', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':16}
                      ),
                    html.H4(id='top_tweeted_day', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':24}# 'font-family':'Lato'}
                      )
                ]),
        ])
    ]),                     
    #Third Row
    html.Div(style={'display': 'inline-block'}, children = [
        #First Column
        html.Div(style={'backgroundColor': colors['background2'], 'display':'inline-block',
                        'vertical-align': 'top',  'width':'15%', 'height':'40vw',
                        'margin-left': '1vw', 'margin-top': '1vw'},
                     children=[        
                html.Div([
                    html.H4(children='Most Recent Tweet', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':16, 'margin-top': '.2vw'}
                      ),
                    html.P(id='recent_tweet_text', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':10}# 'font-family':'Lato'}
                      ),
                    html.P(id='recent_tweet_date', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':12}# 'font-family':'Lato'}
                      )
                ]),
                html.Div([
                    html.H4(children='Most Retweeted Tweet', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':16, 'margin-top': '.2vw'}
                      ),
                    html.P(id='retweet_text', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':10}# 'font-family':'Lato'}
                      ),
                    html.P(id='retweet_count', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':12}# 'font-family':'Lato'}
                      )
                ]),
                html.Div(style={'margin-top': '2vw'}, children = [
                    html.H4(children='Most Favorited Tweet', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':16, 'margin-top': '.2vw'}
                      ),
                    html.P(id='favorite_text', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':10}# 'font-family':'Lato'}
                      ),
                    html.P(id='favorite_count', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'fontSize':12}# 'font-family':'Lato'}
                      )
                ])
        ]),
        #Third Column
        html.Div(style={'display': 'inline-block', 'width':'40%', 'backgroundColor': colors['background3'], 
                        'vertical-align': 'top', 'margin-bottom': '1vw', 'margin-right': '.5vw',
                        'margin-left': '1vw',
                        'margin-top': '1vw'},
                 children=[
                    #First Row
                        html.P(children='Tag Frequency', style={
                            'textAlign': 'Center',
                            'color': colors['text2'], 
                            'fontSize':18, 'margin-bottom': '.00vw',}#, 'font-family':'Lato'}
                        ),
                         html.P(children='Filter by Tag Type:', style={
                            'textAlign': 'left',
                            'color': colors['text2'], 
                            'fontSize':14, 'margin-bottom': '.5vw', 'margin-top': '.00vw', 'margin-left': '2vw',
                            'display': 'inline-block'}#, 'font-family':'Lato'}
                        ),
                         dcc.RadioItems(id='tag_choice',
                            options=[{'label': '@ ', 'value': '@'},
                                     {'label': '# ', 'value': '#'}],
                            value='@', style={'align':'center','margin-left': '.3vw', 'display': 'inline-block'}
                        ),
                        html.P(children='Number of Terms to Display:', style={
                            'textAlign': 'left',
                            'color': colors['text2'], 
                            'fontSize':14, 'margin-bottom': '.5vw',  'margin-top': '.00vw',
                            'margin-left': '3vw',
                            'display': 'inline-block'}#, 'font-family':'Lato'}
                        ),
                        dcc.Dropdown(id='tag_qty_choice',
                             options=[{'label': i, 'value': i} for i in range(5, 26, 5)],
                                        value=5, 
                                        style={'fontSize':14, 
                                               'display': 'inline-block', 
                                               'margin-left': '.3vw', 
                                               'margin-right': '.5vw',
                                               'margin-top': '.05vw'}#,'font-family':'Lato', }
                        ),
                    #Third Row
                    html.Div(children=[
                        dcc.Graph(id = 'get_tag_plot', config={'displayModeBar': False})
                    ]),
        ]),
        #Fourth Column
        html.Div(style={'display': 'inline-block', 'width':'40%', 'backgroundColor': colors['background3'], 
                        'vertical-align': 'top', 'margin-bottom': '1vw', 'margin-right': '1vw',
                        'margin-left': '.5vw',
                        'margin-top': '1vw'},
                 children=[
                    #First Row
                        html.P(children='Word Frequency', style={
                            'textAlign': 'Center',
                            'color': colors['text2'], 
                            'fontSize':18, 'margin-bottom': '.00vw',}#, 'font-family':'Lato'}
                        ),
                         html.P(children='Number of Words to Display:', style={
                            'textAlign': 'left',
                            'color': colors['text2'], 
                            'fontSize':14, 'margin-bottom': '.5vw', 'margin-top': '.00vw', 'margin-left': '2vw',
                            'display': 'inline-block'}#, 'font-family':'Lato'}
                        ),
                        dcc.Dropdown(id='word_qty',
                             options=[{'label': i, 'value': i} for i in range(5, 26, 5)],
                                        value=5, 
                                        style={'fontSize':14, 
                                               'display': 'inline-block', 
                                               'margin-left': '.3vw', 
                                               'margin-right': '.5vw',
                                               'margin-top': '.05vw'}#,'font-family':'Lato', }
                        ),
                    #Third Row
                    html.Div(children=[
                        dcc.Graph(id = 'word_count_plot', config={'displayModeBar': False})
                    ]),
        ]),
    ]),
    #Fifth Row
    html.Div(style={'backgroundColor': colors['background2'], 'display': 'inline-block', 
                    'width':'100%', 'margin-left': '1vw', 'margin-right': '1vw', 
                    'margin-top': '.01vw',}, 
            children = [
            html.Div([
                    html.H4(children='Explore the Tweets', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':20}
                    )
            ])
    ]),
    #Sixth Row
    html.Div(children = [
        html.Div(style={'display': 'inline-block', 'width':'100%', 'margin-right': '2vw'},
                     children=[
                dcc.Graph(id = 'treemap', config={'displayModeBar': False})
            ]),
        ]),
    #Seventh Row
    html.Div(style={'backgroundColor': colors['background2'], 'display': 'inline-block', 
                    'width':'100%', 'margin-left': '1vw', 
                    'margin-right': '1vw', 'margin-top': '.5vw',}, 
            children = [
            html.Div([
                    html.H4(children='Tweet Frequency', style={
                    'textAlign': 'center',
                    'color': colors['text2'], 'backgroundColor': colors['background2'], 
                    'fontSize':20}
                    )
            ])
    ]),
    #Eigth Row
    html.Div(children = [
        html.Div(style={'display': 'inline-block', 'width':'100%', 'margin-right': '2vw'},
                     children=[
                dcc.Graph(id = 'time_series_plot', config={'displayModeBar': False})
            ]),
    ])
])

@app.callback(
    Output('treemap', 'figure'),
    Input('username-dropdown', 'value'))
def update_treepmap(selected_username):
    fig = treemap(df, selected_username)
    return fig

@app.callback(
    Output('avg_daily_tweet', 'children'),
    Input('username-dropdown', 'value'))
def get_daily_tweet_count(selected_username):
    df_user = df[df['username']==selected_username]
    avg_daily_tweet_value = int(df_user.groupby(['month', 'day']).count()['username'].mean())
    return avg_daily_tweet_value

@app.callback(
    Output('avg_monthly_tweet', 'children'),
    Input('username-dropdown', 'value'))
def get_monthly_tweet_count(selected_username):
    df_user = df[df['username']==selected_username]
    avg_monthly_tweet_value = int(df_user.groupby(['month']).count()['username'].mean())
    return avg_monthly_tweet_value

@app.callback(
    Output('follower_count', 'children'),
    Input('username-dropdown', 'value'))
def get_follower_count(selected_username):
    df_user = df[df['username']==selected_username]
    follower_count = max(df_user['follower_count'])
    return format(follower_count, ",d")

@app.callback(
    Output('top_tweeted_day', 'children'),
    Input('username-dropdown', 'value'))
def top_tweeted_day(selected_username):
    df_user = df[df['username']==selected_username]
    month = [0, 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
    top_tweeted_day = df_user.groupby(['month', 'day', 'year']).count()['username'].idxmax()
    top_tweeted_day = str(month[top_tweeted_day[0]] + ' ' + str(top_tweeted_day[1]) + ', ' + str(top_tweeted_day[2]))
    return top_tweeted_day

@app.callback(
    Output('time_series_plot', 'figure'),
    Input('username-dropdown', 'value'))
def get_time_series_plot(selected_username):
    fig = time_series_tweets(df, selected_username)
    fig.update_layout(height=450, margin={'l': 20, 'b': 10, 'r': 10, 't': 35}, title_y=.965)
    return fig

@app.callback(
    Output('recent_tweet_text', 'children'),
    Input('username-dropdown', 'value'))
def get_recent_tweet_text(selected_username):
    df_user = df[df['username']==selected_username]
    recent_tweet_text = df_user.iloc[-1]['text']
    return recent_tweet_text

@app.callback(
    Output('recent_tweet_date', 'children'),
    Input('username-dropdown', 'value'))
def get_recent_tweet_date(selected_username):
    df_user = df[df['username']==selected_username]
    month = [0, 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
    recent_tweet_date = f"{month[df_user.iloc[-1]['month']]} {df_user.iloc[-1]['day']}, {df_user.iloc[-1]['year']}"
    return recent_tweet_date

@app.callback(
    Output('retweet_count', 'children'),
    Input('username-dropdown', 'value'))
def get_reweeted_tweet_count(selected_username):
    df_user = df[df['username']==selected_username]
    retweet_count = f"Retweet Count: {format(max(df_user['retweet_count']), ',d')}"
    return retweet_count

@app.callback(
    Output('retweet_text', 'children'),
    Input('username-dropdown', 'value'))
def get_reweeted_text(selected_username):
    df_user = df[df['username']==selected_username]
    retweet_text = df_user[df_user['retweet_count'] == max(df_user['retweet_count'])]['text'].all()
    return retweet_text
                                                           
@app.callback(
    Output('favorite_count', 'children'),
    Input('username-dropdown', 'value'))
def get_reweeted_tweet_count(selected_username):
    df_user = df[df['username']==selected_username]
    favorite_count = f"Favorite Count: {format(max(df_user['favorite_count']), ',d')}"
    return favorite_count

@app.callback(
    Output('favorite_text', 'children'),
    Input('username-dropdown', 'value'))
def get_reweeted_text(selected_username):
    df_user = df[df['username']==selected_username]
    favorite_text = df_user[df_user['favorite_count'] == max(df_user['favorite_count'])]['text'].all()
    return favorite_text

@app.callback(
    Output('word_count_plot', 'figure'),
    [Input('username-dropdown', 'value'),
     Input('word_qty', 'value')])
def word_count_plot(selected_username, qty):
    fig = word_counts(df, selected_username, qty)
    fig.update_layout(height=450, margin={'l': 20, 'b': 10, 'r': 10, 't': 35}, title_y=.965)
    return fig

@app.callback(
    Output('get_tag_plot', 'figure'),
    [Input('username-dropdown', 'value'),
    Input('tag_choice', 'value'),
    Input('tag_qty_choice', 'value')])
def get_tag_plot(selected_username, select, qty):
    fig = plot_ats_or_hashtags(df, selected_username, select, qty)
    fig.update_layout(height=450, margin={'l': 20, 'b': 10, 'r': 10, 't': 35}, title_y=.965)
    return fig

if __name__ == '__main__':
    #app.run_server(debug = False, port=8050)
    app.run_server(host='0.0.0.0', port=8050, debug = False)
    #app.run_server(host="52.204.49.146", port=8050)