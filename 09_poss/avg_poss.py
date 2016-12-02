#!/usr/bin/env python

############################################################
# avg_poss - possession statistics
# from 2014-2015 HSBC World Rugby Sevens Series matches
# written by jliberman@utexas.edu
############################################################

import pandas as pd
import numpy as np

# convert min:sec to sec
def to_seconds(mmss):
    seconds= 0
    for sect in mmss.split(':'):
        seconds = seconds * 60 + int(sect)
    return seconds

# read possession data to csv
fp = pd.read_csv('https://raw.githubusercontent.com/jliberma/Data_processing/master/data/full_poss.csv')
top = pd.read_csv('https://raw.githubusercontent.com/jliberma/Data_processing/master/data/top.csv',header=0,index_col=0)

top[['avg']] = top[['avg']].applymap(to_seconds)
avg = round(top[['avg']].mean(),0)
avg_min = round(top[['avg']].min(),0)
avg_max = round(top[['avg']].max(),0)
print('Top teams: Mean: %s Min: %s Max: %s' %
		(avg, avg_min, avg_max))

# convert min:sec columns to sec
fp[['T1t','T2t']] = fp[['T1t','T2t']].applymap(to_seconds)

# combine both teams to single data series
p = fp.T1p.append(fp.T2p)
t = fp.T1t.append(fp.T2t)

# add mean scoring/time of possession lines to the plot
mt = np.mean(t)
mp = np.mean(p)
tmax = np.max(p)
print('All matches: mean time: %s, mean points: %s, max_points: %s' % (round(mt,0), round(mp,0), round(tmax,0)))

# calculate point and possession differentials for all matches
pd = fp.T1p-fp.T2p
td = fp.T1t-fp.T2t

# Calculate win frequency for teams with more possession
total = (pd * td > 0).sum()
pct = round(float(total.sum())/len(fp.index),2)
print('Matches won by possession leaders (all teams): %s%% (%s/%s)' %
    (int(100*pct), total, len(fp.index)))

# plot differentials for matches with >= 1 non-core team
noncore = ['BELGIUM','BRAZIL','RUSSIA','HONG KONG',
'ZIMBABWE','AMERICAN SAMOA','PAPUA NEW GUINEA']
nc = fp[fp['T1'].isin(noncore) | fp['T2'].isin(noncore)]
ncp = nc.T1p-nc.T2p
nct = nc.T1t-nc.T2p

# plot differentials for matches with 0 non-core teams
ct = fp[-fp['T1'].isin(noncore) & -fp['T2'].isin(noncore)]
ctp = ct.T1p-ct.T2p
ctt = ct.T1t-ct.T2t

# core winning percentage for possession leader
core_total = (ctt * ctp > 0).sum()
c_pct = round(float(core_total.sum())/len(ct.index),2)
print('Matches won by possession leaders (core teams): %s%% (%s/%s)' %
    (int(100*c_pct), core_total, len(ct.index)))

# non-core winning percentage for possession leader
non_core_total = (nct * ncp > 0).sum()
nc_pct = round(float(non_core_total.sum())/len(nc.index),2)
print('Matches won by possession leaders (non-core): %s%% (%s/%s)' %
    (int(100*nc_pct), non_core_total, len(nc.index)))
