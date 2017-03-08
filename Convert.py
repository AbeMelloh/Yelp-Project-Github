# -*- coding: utf-8 -*-
"""
Created on Mon Nov 21 14:44:44 2016

@author: abeme
"""

'''
Convert Yelp Academic Dataset from JSON to CSV
Requires Pandas (https://pypi.python.org/pypi/pandas)
By Paul Butler, No Rights Reserved
'''

import json
import pandas as pd
from glob import glob

def convert(x):
    ''' Convert a json string to a flat python dictionary
    which can be passed into Pandas. '''
    ob = json.loads(x)
    for k, v in ob.items():
        if isinstance(v, list):
            ob[k] = ','.join(str(v) for v in ob)
        elif isinstance(v, dict):
            for kk, vv in v.items():
                ob['%s_%s' % (k, kk)] = vv
            del ob[k]
    return ob

for json_filename in glob('crunchbase_2013_snapshot_20131212.json'):
    csv_filename = '%s.csv' % json_filename[:-5]
    print 'Converting %s to %s' % (json_filename, csv_filename)
    df = pd.DataFrame([convert(line) for line in file(json_filename)])
    df.to_csv(csv_filename, encoding='utf-8', index=False)
    

