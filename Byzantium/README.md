## Overview
A one-stop-shop to enable voters the ability to interact with what their elected leaders are saying on Twitter. Likes and retweets are not enough in the new age of social media, both elected officials and voters are easily lost in the noise that is cast through current platforms. Our platform offers voters an opportunity to make an impact with the leaders they elected, and elected officials have a platform to speak directly to their constituents without user interference.

## Features
While the idea is in it's infant stages and carving many paths, a robust data workflow has been established to retrieve a better understanding of what can be used from Twitter data - highlighting where we want to target our approach. The data workflow, as depicted in "Data_Workflow.pdf" features a full stack setup to enable daily retrieval, packaging, and storage of specified users' tweets on Twitter. The workflow is managed on Amazon Web Services (AWS), and is fully automated from data retrieval to hosting an interactive dashboard as seen [here](http://52.204.49.146:8050/). 

## File Overview

1. *Twitter_Pull_Script.py*: interaction with the Twitter API and DynamoDB, tweets are retrieved, packaged, and uploaded via this script.
2. *Tweet_Metrics.py*: script to build tweet metrics for each user which is passed to the dashboard.
3. *app.py*: script executing the dashboard construction.
