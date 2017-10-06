#!/usr/bin/env python
#
# Authors:     KL
# Maintainers: KL
# Copyright:   2016, HRDAG, GPL v2 or later
# ============================================
# policing/simulation/run-predpol/


import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as dist
import random
import array
import pylab
import pickle
import random
import pandas as pd
import datetime
import scipy
import copy
import sys
import predpol as pp
import sys
assert sys.version_info.major == 3
import argparse

#tv_rds = r.readRDS("../output/crimes_by_bin_drugs.rds")

## ------------ define functions ----------------------##
#function to take stuff in from makefile
def getargs():
    parser = argparse.ArgumentParser()
    parser.add_argument("--drug_crimes_with_bins", required=True)
    parser.add_argument("--global_start", required=True)
    parser.add_argument("--global_end", required=True)
    parser.add_argument("--predictions", required=True)
    parser.add_argument("--observed", required=True)
    parser.add_argument("--predpol_window", required=True)
    parser.add_argument("--begin_predpol", required=True)
    parser.add_argument("--add_crimes_logical", required=True)
    parser.add_argument("--percent_increase", required=True)

    return parser.parse_args()


#define function to set up data for run
def prepare_data_for_predpol(data, start_time, end_time):

	data = data[(data.DateTime >= start_time) & (data.DateTime < end_time)]
	pp_dict = dict((int(i), []) for i in set(data.bin))

	for i in data.index:
		pp_dict[int(data.bin[i])].append(data['DateTime'].loc[i])

	keys_to_drop = []
	pp_dict = {n: sorted(v) for n,v in pp_dict.items()}
	for n in pp_dict.keys():
		if len(pp_dict[n]) < 1:
			keys_to_drop.append(n)

	for key in keys_to_drop:
		pp_dict.pop(key, None)

	return(pp_dict)

if __name__ == '__main__':
    args = getargs()


## ------------ load data -----------------------------------##
#load data
data = pd.read_csv(args.drug_crimes_with_bins)
data.columns = ['rownum', 'bin', 'OCCURRED', 'LAG']
data = data[pd.notnull(data['bin'])]
data['DateTime'] = pd.to_datetime(data.OCCURRED, format = '%m/%d/%y')


## ---------- set parameters of run-------------------------##
#define dates to be usued
global_start = pd.to_datetime(args.global_start) #2011/01/01
global_end = pd.to_datetime(args.global_end) #2011/12/31
begin_predpol = int(args.begin_predpol)#90

#define length of sliding window to be used in predpol
predpol_window = int(args.predpol_window)

#define whether we'll be adding c rimes
if args.add_crimes_logical == 'True':
	add_crimes_logical = True
else: 
	add_crimes_logical = False

#if we're adding crimes, how much?
percent_increase = float(args.percent_increase) #.5


#define where to output simulation results
output_location_predictions = args.predictions #'output/predpol_drug_predictions'
output_location_observed = args.observed #'output/predpol_drug_observed'

if add_crimes_logical:
	output_location_predictions += '_add_' + str(int(percent_increase*100)) + 'percent.csv'
else:
	output_location_predictions += '.csv'

if add_crimes_logical:
	output_location_observed += '_add_' + str(int(percent_increase*100)) + 'percent.csv'
else:
	output_location_observed += '.csv'


# define how many bins are needed for dataframe
max_bin = max(data.bin)
print("max bins is: " + str(max_bin))

#print some stuff
print(len(data))
data = data[(data.DateTime >= global_start) & (data.DateTime <= global_end)]
print(len(data))


## ----------- initialize objects to hold outputs -----------------##
num_predictions = (global_end - global_start).days - predpol_window

#make things for output
results_rates = pd.DataFrame()
results_rates['bin'] = range(1, int(max_bin+1))
results_rates = results_rates.set_index(['bin'])

#num_crimes_per_bin
results_num_crimes = pd.DataFrame()
results_num_crimes['bin'] = range(1, int(max_bin+1))
results_num_crimes = results_num_crimes.set_index(['bin'])


## ------------ run predpol -------------------------------------##
#run predpol iteratively
for i in range(num_predictions):
	print(i)
	start_date = global_start + pd.DateOffset(i)
	end_date = global_start + pd.DateOffset(i + predpol_window)
	pp_dict = prepare_data_for_predpol(data, start_date, end_date)
	r,o, om, thet = pp.runEM(pp_dict, predpol_window, end_date)

	# save rates
	str_date = str(end_date).split(' ')[0]
	results_rates[str_date] = 0
	keys = list(pp_dict.keys())
	#print(len(keys))
	#print(len(r))
	results_rates.loc[keys, str_date] = r


	#add p% crimes if that's what we're doing
	if add_crimes_logical and i >= begin_predpol:
		crime_today = pd.value_counts(data[data.DateTime==end_date].bin)
		crime_today_predicted = crime_today.loc[o]
		crime_today_predicted[pd.isnull(crime_today_predicted)] = 0


		add_crimes = np.random.binomial(list(crime_today_predicted + 1) , percent_increase)
		new_crimes = pd.DataFrame()
		new_crimes['bin'] = np.repeat(list(crime_today_predicted.index), add_crimes)
		new_crimes['DateTime'] = end_date
		new_crimes['OCCURRED'] = end_date
		new_crimes['LAG'] = 0
		new_crimes.index = range(1 + max(data.index), 1 + max(data.index) + sum(add_crimes))
		data = data.append(new_crimes)
		print(len(data))

		#also need to record the total number of crimes per day
	crime_today = pd.value_counts(data[data.DateTime==end_date].bin)
	results_num_crimes[str_date] = 0
	results_num_crimes.loc[crime_today.index, str_date] = list(crime_today)

	#remove old data to speed things up!
	data = data[(data.DateTime >= start_date)]





#output results
results_rates.to_csv(output_location_predictions)
results_num_crimes.to_csv(output_location_observed)