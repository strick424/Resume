import numpy as np
import pandas as pd

class ConstructDictionary:
    
        def __init__(self, d, u):
            self.data = d
            self.username = u
            self.rest_network = self.connections_within()
        
        def build_restaurant_data(self):
            """This function constructs a dictionary containing all pertinant information for each user."""

            rest = {}                                                  #contruct an empty dict
            rest[self.username] = {'location': 'Annapolis, MD',        #our restaurants don't have Twitter acounts
                                   'friends_count': "Tracked Tag",     # so we will "make" an entry for them
                                   'follower_count': len(self.data), 
                                   'description': 'Restaruant', 
                                   'mentions': ['none'],
                                   'connections': ['none']}   

            for user in self.data:         #iterate through all the users in our data
                rest[user['screen_name']] = {'location': user['location'], 
                                             'friends_count': user['friends_count'],
                                             'follower_count': user['followers_count'], 
                                             'description': user['description'], 
                                             'mentions': ['none'],
                                             'connections': [(user['screen_name'], self.username)]}
                try:                       #if the user has mentioned anyone we want to know that info
                    m = []
                    #depending on how many people they have mentioned, we need carefully add the mentions
                    if len(user['status']['entities']['user_mentions'])>1:             
                        for i in np.arange(0,len(user['status']['entities']['user_mentions'])):
                            c = user['status']['entities']['user_mentions'][i]['screen_name']
                            m.append(c)
                    else:
                        c = user['status']['entities']['user_mentions'][0]['screen_name']
                        m.append(c)
                    rest[user['screen_name']]['mentions'] = m

                except:
                    continue
            
            return rest

        def connections_within(self):
            """Construct listing of connections based on mentions for each user."""
            d = self.build_restaurant_data()
            for k1, v1 in d.items():             #iterate through each user from the restaurant
                for k2, v2 in d.items():         #reiterate through each user from the restaurant
                    #if the user is mentioned by someone else, establish a connection
                    if (k1 in d[k2]['mentions']) and (k1!=k2) and ((k1,k2) not in d[k1]['connections']):
                        c = (k1,k2)
                        d[k1]['connections'].append(c)   #add the new connection to the user's lisf of connections
            #self.rest = d
            return d
        
class ConnectionsAcross:
    
    def __init__(self, d1, d2):
        self.d1 = d1
        self.d2 = d2
        self.rest_across = self.connections_across()
    
    def connections_across(self):
        """Construct a listing of connections across two restaurants"""
        
        for k1, v1 in self.d1.items():                                #iterate through each user from restaurant 1
            for k2, v2 in self.d2.items():                            #iterate through each user from restaurant 2
                if (k1 in self.d2[k2]['mentions']) and (k1!=k2) and ((k1,k2) not in self.d1[k1]['connections']):  #if the user from restaurant 1 is mentioned by a user from restaurant 2, establish a connection
                    c = (k1,k2)
                    self.d1[k1]['connections'].append(c)              #update user from restaurant 1's listing of connections
                    if (k2 == self.d1[k1]['mentions']) and (k1!=k2):  #check the recipricol - this might not be needed
                        c = (k2,k1)
                        self.d2[k1]['connections'].append(c)       

        #now consolidate the two datasets into one
        d3 = {} #new dataset to hold the information
        for k1 in self.d1.keys():                         #iterate through the users in d1
            if k1 in self.d2.keys():                      #if that user is found in d2
                conns = self.d2[k1]['connections']        #get the connections from d2
                for c in conns:  #add the listing of connections to d1 if they are not already there.
                    if c not in self.d1[k1]['connections']:
                        self.d1[k1]['connections'].append(c)
            d3[k1] = self.d1[k1]                 #add the updated d1 to our new consolidated dataset

        for k2 in self.d2.keys():                #check the recipricol from above
            if k2 not in self.d1.keys():
                d3[k2] = self.d2[k2]             #update the consolidated dataset as needed

        return d3                                #return the dataset
    
    
class BuildDf:
    
    def __init__(self, r_dict, n):
        self.restaurants = r_dict
        self.n = n
        self.df = self.build_dataframe()
        
    def build_dataframe(self):
        node1_l = []
        node2_l = []
        friend_count_l = []
        follower_count_l = []
        for key, value in self.restaurants.items():
            for connection in self.restaurants[key]['connections']:
                node1_l.append(connection[0])
                node2_l.append(connection[1])
                friend_count_l.append(self.restaurants[key]['friends_count'])
                follower_count_l.append(self.restaurants[key]['follower_count'])

        df = pd.DataFrame(columns=['Node1', 'Node2', 'Friend_Count', 'Follower_Count'])
        df['Node1'] = node1_l
        df['Node2'] = node2_l
        df['Friend_Count'] = friend_count_l
        df['Follower_Count'] = follower_count_l
        df = df[df['Follower_Count']>=self.n].reset_index()
        
        return df
        
        
        
        