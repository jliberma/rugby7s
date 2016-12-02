#!/usr/bin/env python

############################################################
# ex_poss - plot relationship between possession and scoring
# from 2014-2015 HSBC World Rugby Sevens Series matches
# written by jliberman@utexas.edu
############################################################

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

# make Tufte-style dot-dash graph -- dashes on the axes mark the data points
# etframes: https://github.com/ahupp/etframes/blob/master/etframes.py
import etframes
plt.style.use('ggplot')

# convert min:sec to sec
def to_seconds(mmss):
    seconds= 0
    for sect in mmss.split(':'):
        seconds = seconds * 60 + int(sect)
    return seconds

# jitter the scatterplot for readability
# from: http://stackoverflow.com/questions/8671808/matplotlib-avoiding-overlapping-datapoints-in-a-scatter-dot-beeswarm-plot
def rand_jitter(arr):
    np.random.seed(1234)
    stdev = .01*(max(arr)-min(arr))
    return arr + np.random.randn(len(arr)) * stdev

def jitter(x, y, s=20, c='b', marker='o', cmap=None, norm=None, vmin=None, vmax=None,
		alpha=None, linewidths=None, verts=None, hold=None, **kwargs):
    return plt.scatter(rand_jitter(x), rand_jitter(y), 
        s=s, c=c, marker=marker, cmap=cmap, norm=norm, 
        vmin=vmin, vmax=vmax, alpha=alpha, linewidths=linewidths, 
        verts=verts, hold=hold, **kwargs)

# read possession data to csv
fp = pd.read_csv('https://raw.githubusercontent.com/jliberma/Data_processing/master/data/full_poss.csv')
top = pd.read_csv('https://raw.githubusercontent.com/jliberma/Data_processing/master/data/top.csv',header=0,index_col=0)
#fp = pd.read_csv('full_poss.csv')
#top = pd.read_csv('top.csv',header=0,index_col=0)

top[['avg']] = top[['avg']].applymap(to_seconds)
avg = round(top[['avg']].mean(),0)
avg_min = round(top[['avg']].min(),0)
avg_max = round(top[['avg']].max(),0)

ax = top.plot(kind='bar')
plt.plot(plt.gca().get_xlim(),[avg,avg], linestyle="--")
#plt.legend((top,avg),('Non-core','Core'),loc='upper right',fontsize=8)
plt.ylim(0,420)
plt.ylabel("Seconds")
plt.xlabel("")
plt.title("Average time of possession")
plt.savefig("7s_poss_avg.png")
plt.clf()

# convert min:sec columns to sec
fp[['T1t','T2t']] = fp[['T1t','T2t']].applymap(to_seconds)

# combine both teams to single data series
p = fp.T1p.append(fp.T2p)
t = fp.T1t.append(fp.T2t)

# plot relationship between time of possession and scoring
allteams=jitter(t,p,c=".5")
etframes.add_dot_dash_plot(plt.gca(), ys=p, xs=t)
plt.ylim(-5,65)
plt.xlim(0,400)

# add mean scoring/time of possession lines to the plot
mt = np.mean(t)
mp = np.mean(p)
tmax = np.max(p)
plt.plot([mt,mt],plt.gca().get_ylim(), linestyle="--")
plt.plot(plt.gca().get_xlim(),[mp,mp], linestyle="--")

# label the graph
plt.ylabel("Points")
plt.xlabel("Possession (seconds)")
plt.title("Scoring and time of possession in rugby 7s")

# save the graph
plt.savefig("7s_poss_scoring.png")
plt.clf()

# calculate point and possession differentials for all matches
pd = fp.T1p-fp.T2p
td = fp.T1t-fp.T2t

# Calculate win frequency for teams with more possession
total = (pd * td > 0).sum()
pct = round(float(total.sum())/len(fp.index),2)

# plot differentials for matches with >= 1 non-core team
noncore = ['BELGIUM','BRAZIL','RUSSIA','HONG KONG',
'ZIMBABWE','AMERICAN SAMOA','PAPUA NEW GUINEA']
nc = fp[fp['T1'].isin(noncore) | fp['T2'].isin(noncore)]
ncp = nc.T1p-nc.T2p
nct = nc.T1t-nc.T2p
jitter(nct,ncp,c=".5")
etframes.add_dot_dash_plot(plt.gca(), ys=ncp, xs=nct)

# plot differentials for matches with 0 non-core teams
ct = fp[-fp['T1'].isin(noncore) & -fp['T2'].isin(noncore)]
ctp = ct.T1p-ct.T2p
ctt = ct.T1t-ct.T2t
coreteams=jitter(ctt,ctp,c=".5")
etframes.add_dot_dash_plot(plt.gca(), ys=ctp, xs=ctt)

# annotate graph with Pearson correlation coefficient
pc = stats.pearsonr(p,t)
plt.annotate('r = %s' % round(pc[0],4), xy=(0,0), xytext=(100,58))

# add best fit line for all matches
slope,intercept = np.polyfit(td,pd,1)
ablineValues = slope * td + intercept
plt.plot(td, ablineValues, 'b', c=".5")

# label graph
plt.ylim(-5,65)
plt.xlim(-300,310)
plt.ylabel("Point differential")
plt.xlabel("Possession differential (seconds)")
plt.title("Winning and time of possession in rugby 7s\n(All teams)")
plt.savefig("7s_poss_diff_scoring_all.png")
plt.clf()

# plot core and non-core matches as separate series
ncj = jitter(nct,ncp,c=".75")
cj = jitter(ctt,ctp,c=".5")
plt.legend((ncj,cj),('Non-core','Core'),scatterpoints=1,
		loc='upper right',fontsize=8)
etframes.add_dot_dash_plot(plt.gca(), ys=ncp, xs=nct)
etframes.add_dot_dash_plot(plt.gca(), ys=ctp, xs=ctt)

# core winning percentage for possession leader
core_total = (ctt * ctp > 0).sum()
c_pct = round(float(core_total.sum())/len(ct.index),2)

# non-core winning percentage for possession leader
non_core_total = (nct * ncp > 0).sum()
nc_pct = round(float(non_core_total.sum())/len(nc.index),2)

# add best fit lines for both core and non-core
plt.plot(td, ablineValues, 'b', c=".75")
slope,intercept = np.polyfit(ctt,ctp,1)
core_ablineValues = slope * ctt + intercept
plt.plot(ctt, core_ablineValues, 'b', c="r")

# plot the Pearson correlation coefficient for core matches
pc = stats.pearsonr(ctp,ctt)
plt.annotate('r = %s' % round(pc[0],4), xy=(0,0), xytext=(100,58))

# label the graph
plt.ylim(-5,65)
plt.xlim(-300,310)
plt.ylabel("Point differential")
plt.xlabel("Possession differential (seconds)")
plt.title("Winning and time of possession in rugby 7s\n(Core teams)")
plt.savefig("7s_poss_diff_scoring_core.png")
