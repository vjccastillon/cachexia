#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul  1 17:28:28 2024

@author: castilv
"""
import pickle
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

file_path = '/Users/castilv/Documents/Cachexia/cac_data/Venise_lab_data_030725.pickle'
#load
def load_pickle(file_path):
    with open(file_path, 'rb') as file:
        data = pickle.load(file)
        print("Data loaded successfully!")
        return data
    
data = load_pickle(file_path)

#check
if data:
    first_mrn = next(iter(data))  
    print("First MRN:", first_mrn)
    print("Data under first MRN:", data[first_mrn])
    
ALL_INCLUDED_TESTS = ['ALK', 'ALT', 'AST', 'Albumin', 'Alphafetoprotein', 'BUN', 'Baso', 
                      'Beta-2 Microglobulin', 'Bilirubin, Total', 'Blast', 'CEA', 'CO2', 
                      'Calcium', 'Cancer Antigen 125', 'Cancer Antigen 15-3', 'Cancer Antigen 19-9', 
                      'Chloride', 'Conjugated Bili (mg)', 'Creatinine', 'Eos', 'Glucose', 'HCT', 
                      'HGB', 'Immature Granulocyte', 'Luc', 'Lymph', 'MCH', 'MCHC', 'MCV', 
                      'Megakaryocyte Fragment', 'Meta', 'Mono', 'Myelo', 'Neut', 'Nucleated RBC', 
                      'PSA', 'Platelets', 'Potassium', 'Promyelo', 'Protein, Total', 'RBC', 
                      'RDW', 'Sodium', 'TSH', 'Variant Lymph', 'WBC']

def flatten_data(data):
    records = []
    for mrn, dates in data.items():
        for date, tests in dates.items():
            if len(tests) == len(ALL_INCLUDED_TESTS):
                record = {'MRN': mrn, 'Date': date}
                record.update(dict(zip(ALL_INCLUDED_TESTS, tests)))
                records.append(record)
            else:
                print(f"Data mismatch at MRN {mrn} on {date}: Expected {len(ALL_INCLUDED_TESTS)} tests, found {len(tests)}")
    return pd.DataFrame(records)

lab = flatten_data(data)


for column in lab.columns:
    if column not in ['MRN', 'Date']:  
        lab[column] = pd.to_numeric(lab[column], errors='coerce')

lab['Date'] = pd.to_datetime(lab['Date'], errors='coerce')

test_counts = lab.drop(columns=['MRN', 'Date']).notna().sum().sort_values(ascending=False)

top_30_tests = test_counts.head(30)

plt.figure(figsize=(10, 8))
sns.barplot(x=top_30_tests.values, y=top_30_tests.index, color='palevioletred')
plt.title('Serological Tests')
plt.xlabel('Number of Measurements')
plt.ylabel('Lab Tests')
plt.show()
labs2use = test_counts.sort_values(ascending=False).head(30).index.tolist()
labs = lab[['MRN', 'Date'] + labs2use]



msk_clin = pd.read_csv('/Users/castilv/Documents/Cachexia/cac_data/metadata_clin_1104.csv')# get tumor diag for mrns already analyzed
msk_clin['Tumor Diagnosis Date'] = pd.to_datetime(msk_clin['Tumor Diagnosis Date'])
labs = pd.merge(labs, msk_clin[['MRN', 'Tumor Diagnosis Date', 'GENDER','ANCESTRY_LABEL','CANCER_TYPE_DETAILED']], on='MRN', how='left')
labs['Days Since Diagnosis'] = (labs['Date'] - labs['Tumor Diagnosis Date']).dt.days
#labs = labs[labs['Days Since Diagnosis'] >= 0]

labs = labs.dropna(subset=['Tumor Diagnosis Date'])


spans_episodes= pd.read_csv('/Users/castilv/Documents/Cachexia/cac_data/spans_episodes_0224.csv') 
spans_mrn = spans_episodes.drop_duplicates(subset='MRN', keep='first')
spans_ser = pd.merge( spans_episodes,labs, on='MRN', how='left')
spans_labtests = spans_ser[(spans_ser['Days Since Diagnosis'] >= spans_ser['start_day']) & 
                            (spans_ser['Days Since Diagnosis'] <= spans_ser['end_day'])]


spans_labtests.to_csv('/Users/castilv/Documents/Cachexia/cac_data/spans_labtests_0307.csv', index=False)
