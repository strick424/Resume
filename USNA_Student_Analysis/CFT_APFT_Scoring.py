import pandas as pd
import numpy as np
import math
import os
import sys
import re
import datetime
import time

#\Users\strickla\Desktop\WPy64-3680\python-3.6.8.amd64\python.exe CFT_APFT_Scoring.py MARSOT_Fall_Scores_Raw.csv Fall_2019_Final.csv

def get_sec(t):
    if pd.isnull(t):
        return ("NA")
    else:
        x = time.strptime(t,'%M:%S')
        return datetime.timedelta(hours=x.tm_hour,minutes=x.tm_min,seconds=x.tm_sec).total_seconds()

def score_data(input_file, output_file):

    CFT_score_male = pd.read_csv("CFT_Score_Male_17_21.csv")
    CFT_score_female = pd.read_csv("CFT_Score_Female_17_21.csv")
    APFT_score_male = pd.read_csv("APFT_Score_Male_17_21.csv")
    APFT_score_female = pd.read_csv("APFT_Score_Female_17_21.csv")

    raw_data = pd.read_csv(input_file, encoding='latin-1')

    CFT_score_male["MTC_Time_sec"] = CFT_score_male["MTC_Time"].map(get_sec)
    CFT_score_male["MUF_Time_sec"] = CFT_score_male["MUF_Time"].map(get_sec)
    CFT_score_female["MTC_Time_sec"] = CFT_score_female["MTC_Time"].map(get_sec)
    CFT_score_female["MUF_Time_sec"] = CFT_score_female["MUF_Time"].map(get_sec)
    APFT_score_male["Run_Time_sec"] = APFT_score_male["Run_Time"].map(get_sec)
    APFT_score_female["Run_Time_sec"] = APFT_score_female["Run_Time"].map(get_sec)

    super_max_dict = build_supermax_dict(CFT_score_male, CFT_score_female, APFT_score_male, APFT_score_female)

    df = get_scores(raw_data, CFT_score_male, CFT_score_female, APFT_score_male, APFT_score_female, super_max_dict)

    df.to_csv(str(output_file))

    return df

def get_avg_inc_score(df, event):
    x = df[event]
    l = len(x)
    v=[]
    for i in range(1,l):
        if pd.isnull(x[i]):
            break
        first_value = x[i-1]
        second_value = x[i]
        v.append(first_value-second_value)

    return sum(v)/len(v)

def build_supermax_dict(cft_df_m, cft_df_f, apft_df_m, apft_df_f):
    supermax_dict = {"Male": {}, "Female": {}}

    #Males:
    supermax_dict["M"] = {"APFT": {"Push_Ups": get_avg_inc_score(apft_df_m, "PushUps_Score") ,
                                   "Sit_Ups": get_avg_inc_score(apft_df_m, "SitUps_Score"),
                                   "Run_Time": get_avg_inc_score(apft_df_m, "Run_Time_Score")}, #every six seconds
                            "CFT": {"ACL": get_avg_inc_score(cft_df_m, "ACL_Score"),
                                    "MTC": get_avg_inc_score(cft_df_m, "MTC_Score"), #every one second
                                    "MUF": get_avg_inc_score(cft_df_m, "MUF_Score")}} #every one second


    #Females
    supermax_dict["F"] = {"APFT": {"Push_Ups": get_avg_inc_score(apft_df_f, "PushUps_Score") ,
                                     "Sit_Ups": get_avg_inc_score(apft_df_f, "SitUps_Score"),
                                     "Run_Time": get_avg_inc_score(apft_df_f, "Run_Time_Score")}, #every six seconds
                            "CFT": {"ACL": get_avg_inc_score(cft_df_f, "ACL_Score"),
                                    "MTC": get_avg_inc_score(cft_df_f, "MTC_Score"), #every one second
                                    "MUF": get_avg_inc_score(cft_df_f, "MUF_Score")}} #every one second

    return supermax_dict

def get_supermax_score(count, supermax_dict, event, gender, category, max_count):
    if category == "Run_Time":
        score = 100 + round((max_count - count)/6,0)*supermax_dict[gender][event][category]

    elif category == "MUF" or category == "MTC":
        score = 100 + (max_count-count)*supermax_dict[gender][event][category]

    else:
        score = 100 + (count - max_count)*supermax_dict[gender][event][category]

    return score

def get_cft_score(gender, ac, mtc, muf, cft_df_m, cft_df_f, super_max_dict):

    score1 = 0
    score2 = 0
    score3 = 0

        #get the correct df
    if gender == "M":
        score_df = cft_df_m
    elif gender == "F":
        score_df = cft_df_f

    ac_max = score_df['ACL_Count'].max()
    mtc_max = score_df['MTC_Time_sec'].max()
    muf_max = score_df['MUF_Time_sec'].max()

    ac_min = score_df['ACL_Count'].min()
    mtc_min = score_df['MTC_Time_sec'].min()
    muf_min = score_df['MUF_Time_sec'].min()

    #score ac
    if ac < ac_min:
        score1 = 0
    elif ac > ac_max:
        score1 = get_supermax_score(ac, super_max_dict, "CFT", gender, "ACL", ac_max)
    else:
        score1 = list(score_df[score_df['ACL_Count'] == ac]['ACL_Score'])[0]

    #score mtc
    if mtc < mtc_min or mtc > mtc_max:
        score2 = get_supermax_score(mtc, super_max_dict, "CFT", gender, "MTC", mtc_min)
    else:
        mtc_low = 0
        for index, row in score_df.iterrows():
            if mtc > mtc_low and mtc <= row['MTC_Time_sec']:
                score2 = row['MTC_Score']
                break
            else:
                mtc_low = row['MTC_Time_sec']

    #score muf
    if muf < muf_min or muf > muf_max:
        score3 = get_supermax_score(muf, super_max_dict, "CFT", gender, "MUF", muf_min)
    else:
        muf_low = 0
        for index, row in score_df.iterrows():
            if muf > muf_low and muf <= row['MUF_Time_sec']:
                score3 = row['MUF_Score']
                break
            else:
                muf_low = row['MUF_Time_sec']

    score = score1 + score2 + score3

    return score

def get_apft_score(gender, pu, su, rt, apft_df_m, apft_df_f, super_max_dict):

    score1 = 0
    score2 = 0
    score3 = 0

    #get the correct df
    if gender == "M":
        score_df = apft_df_m
    elif gender == "F":
        score_df = apft_df_f

    pu_max = score_df['PushUps_Count'].max()
    su_max = score_df['SitUps_Count'].max()
    rt_max = score_df['Run_Time_sec'].max()

    pu_min = score_df['PushUps_Count'].min()
    su_min = score_df['SitUps_Count'].min()
    rt_min = score_df['Run_Time_sec'].min()

    #score push ups
    if pu < pu_min:
        score1 = 0
    elif pu > pu_max:
        score1 = get_supermax_score(pu, super_max_dict, "APFT", gender, "Push_Ups", pu_max)
    else:
        score1 = list(score_df[score_df['PushUps_Count'] == pu]['PushUps_Score'])[0]

    #score sit ups
    if su < su_min:
        score2 = 0
    elif su > su_max:
        score2 = get_supermax_score(su, super_max_dict, "APFT", gender, "Sit_Ups", su_max)
    else:
        score2 = list(score_df[score_df['SitUps_Count'] == su]['SitUps_Score'])[0]

    #score run time
    if rt < rt_min or rt > rt_max:
        score3 = get_supermax_score(rt, super_max_dict, "APFT", gender, "Run_Time", rt_min)
    else:
        rt_low = 0
        for index, row in score_df.iterrows():
            if rt > rt_low and rt <= row['Run_Time_sec']:
                score3 = row['Run_Time_Score']
                break
            else:
                rt_low = row['Run_Time_sec']

    score = score1 + score2 + score3

    return score

def get_scores(df, cft_df_m, cft_df_f, apft_df_m, apft_df_f, super_max_dict):

    apft = []
    cft = []

    for index, row in df.iterrows():
        apft_score = get_apft_score(row['Gender'],
                                    row['Push.Ups'], row['Sit.Ups'], row['Two_Mile_Time'],
                                    apft_df_m, apft_df_f, super_max_dict)

        cft_score = get_cft_score(row['Gender'],
                                   row['Ammo.Lift'], row['MTC_Time'], row['MUF_Time'],
                                   cft_df_m, cft_df_f, super_max_dict)

        apft.append(apft_score)
        cft.append(cft_score)

    df['APFT_Score'] = apft
    df['CFT_Score'] = cft

    return df

#if __name__ == '__main__':

    #if len(sys.argv) != 3:
    #    sys.stderr.write("\t This script requires 2 command line arguments \n")
    #    sys.stderr.write("\t arg1: name of input file \n")
    #    sys.stderr.write("\t arg1: name of output file \n")

#    score_data(os.getcwd(), str(sys.argv[1]), sys.argv[2])
