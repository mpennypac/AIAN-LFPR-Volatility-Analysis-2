import pandas as pd
import numpy as np
import re, struct

"""state_abb = ["AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
             "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
             "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"]

data = pd.DataFrame()
#aian_lfpr = pd.read_csv('AIAN Labor Force Participation Rate.csv')
#data['AIAN'] = aian_lfpr[aian_lfpr.columns[1]]

for state in state_abb:
    df = pd.read_csv("{0} LFPR.csv".format(state))
    df = df.drop('DATE',axis=1)
    df = df.rename(columns={df.columns[0]:'{0}'.format(state)})
    data[state] = df

data.to_csv('state-level-LFPR.csv',index=False)"""

#months = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months = ['dec','nov','oct','sep','aug','jul','jun','may','apr','mar','feb','jan']
years = ['00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16',
         '17', '18', '19', '20', '21', '22']

"""
## testing block
filename = '{0}{1}pub.csv'.format('dec', '22')
data = pd.read_csv(filename)
data = data[data['ptdtrace'] == 3].reset_index(drop=True)
avg_age = np.mean(data['prtage'])
age_survey_ct = len(data['prtage'])
#age_missing_ct = len(data[data['prtage'] == ''])
print(data.columns)
print(avg_age, age_survey_ct)#, age_missing_ct)
"""

#lines = data.read()
"""numlines = 0
for line in data:
    lines = line
    print(lines)
    print(type(lines))
    print(len(lines))
    numlines += 1
print(numlines)
data.close()"""
"""
#compiled_data = pd.DataFrame()
compiled_data = pd.read_csv('aian-and-nat-compiled-data.csv')
#compiled_data = compiled_data.drop([compiled_data.columns[0], compiled_data.columns[1]],axis=1)
#compiled_data.to_csv('compiled-data.csv',index=False)
count = len(compiled_data) + 1
#for yr in years:
yr = '00'
for mon in months:
    compiled_data.loc[count, 'Time'] = '{0}{1}'.format(mon, yr)

    if (yr == '12' and mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']) or (yr in ['22', '21', '20', '19', '18', '17', '16', '15', '14', '13']):
        race_var = 'ptdtrace'
        aian_val = 3
        age_var = 'prtage'

    elif (yr == '12' and mon in ['jan', 'feb', 'mar', 'apr']) or (yr in ['11', '10', '09', '08', '07', '06']) or (yr == '05' and mon in ['aug', 'sep', 'oct', 'nov', 'dec']):
        race_var = 'ptdtrace'
        aian_val = 3
        age_var = 'peage'
    elif (yr == '04' and mon in ['jan', 'feb', 'mar', 'apr']) or (yr == '03'):
        race_var = 'ptdtrace'
        aian_val = 3
        age_var = 'prtage'
    elif (yr == '05' and mon in ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul']) or (yr == '04' and mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']):
        race_var = 'prdtrace'
        aian_val = 3
        age_var = 'peage'
    elif (yr in ['02', '01', '00']):
        race_var = 'perace'
        aian_val = 3
        age_var = 'prtage'

"""
    ## new earnings variable determination section, this is after I did all of the above so I didn't want to mess with/figure out how to fit this into
    ## that system
    #if yr in ['22','21','02','01','00']:
    #    ## hourly earnings question we're looking for
    #    ## WHAT IS YOUR HOURLY RATE OF PAY ON (this is the line to paste when searching)
    #    ## THIS JOB, EXCLUDING OVERTIME PAY, TIPS OR COMMISSION?
    #    earn_var = 'pternh1c'
    #elif yr in ['20','19','18','17','16','15','14','13','12','11','10','09','08','07','06','05','04','03']:
    #    earn_var = 'puernh1c'
"""
    ## the earnings data for both AIAN and national populations are a bit of a wash in the CPS, so opted for what industry
    ## the respondents work in. It's a lot more computationally intensive (since it's 14x2=28 new variables I have to collect)
    ## but I'm thinking this makes a bit more sense for determining the causes of the difference in cyclicality of LFPRs anyway
    #if yr in ['22', '21', '20', '19', '18', '17', '16', '15', '14', '13', '12', '11', '10', '09', '08', '07', '06', '05', '04', '03']:
    ## turns out to be the same in every year, so we can get rid of that if statement!
    ## however, I must note that starting in 2002 (and going back to 2000 since it follows the same data layout)
    ## there was an issue - I couldn't find the list of industry values despite being able to find the same
    ## variable listed for the same purpose, so once I get to 2002 I'll take a look at the data it adds and see if it makes sense
    ## line to paste:
    ## MAJOR INDUSTRY RECODE - JOB 1
    industry_var = 'prmjind1'
    agr_val = 1
    mine_val = 2
    const_val = 3
    manu_val = 4
    retail_val = 5
    transp_val = 6
    info_val = 7
    fin_val = 8
    business_val = 9
    educ_health_val = 10
    hosp_val = 11
    other_val = 12
    public_admin_val = 13
    mil_val = 14


    gend_var = 'pesex'
    male_val = 1
    female_val = 2

    ## data compilation format for years 2000-2019 (weird ascii file type)
    if yr in ['19', '18', '17', '16', '15', '14', '13', '12', '11', '10', '09', '08', '07', '06', '05', '04', '03', '02', '01', '00']:
        ## data dictionary file for years 2017-2019
        #data_dict = open('January_2017_Record_Layout.txt').read()
        ## data dictionary file for years 2015-2016
        #data_dict = open('January_2015_Record_Layout.txt').read()
        ## data dictionary file for year 2014
        #data_dict = open('January_2014_Record_Layout.txt').read()
        ## data dictionary file for year 2013
        #data_dict = open('January_2013_Record_Layout.txt').read()
        ## data dictionary files for year 2012
        #if mon in ['jan', 'feb', 'mar', 'apr']:
        #    data_dict = open('jan10dd.txt').read()
        #elif mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']:
        #    data_dict = open('may12dd.txt').read()
        ## data dictionary file for years 2010-apr2012
        #data_dict = open('jan10dd.txt').read()
        ## data dictionary file for year 2009
        #data_dict = open('jan09dd.txt').read()
        ## data dictionary file for years 2007-2008
        #data_dict = open('jan07dd.txt').read()
        ## data dictionary files for year 2006
        #data_dict = open('augnov05dd.txt').read()
        ## data dictionary files for year 2005
        #if mon in ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul']:
        #    data_dict = open('may04dd.txt').read()
        #elif mon in ['aug', 'sep', 'oct', 'nov', 'dec']:
        #    data_dict = open('augnov05dd.txt').read()
        ## data dictionary files for year 2004
        #if mon in ['jan', 'feb', 'mar', 'apr']:
        #    data_dict = open('jan03dd.txt').read()
        #elif mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']:
        #    data_dict = open('may04dd.txt').read()
        ## data dictionary files for year 2003
        #data_dict = open('jan03dd.txt').read()
        ## data dictionary files for years 1998-2002
        data_dict = open('jan98dd.asc.txt').read()
        age_var = age_var.upper()
        race_var = race_var.upper()
        #earn_var = earn_var.upper()
        industry_var = industry_var.upper()
        gend_var = gend_var.upper()
        var_names = [race_var, age_var, industry_var, gend_var]
        #print()
        #print(mon)
        #print(data_dict)
        if (yr in ['19','18','17','16','15','14','13']) or (yr == '12' and mon in ['may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']):
            p = f'\n({"|".join(var_names)})\s+(\d+)\s+.*?\t+.*?(\d\d+).*?(\d\d+)'
            d = {s[0]: [int(s[2])-1, int(s[3]), f'{s[1]}s']
                 for s in re.findall(p, data_dict)}
            #print(re.findall(p, data_dict))
            #print(p)
            #print(d)
        elif (yr in ['11','10','09','08','07','06','05','04','03']) or (yr == '12' and mon in ['jan', 'feb', 'mar', 'apr']):
            p = f'\n({"|".join(var_names)})\s+(\d+)\s+.*?\t*?(\d\d+).*?(\d\d+)'
            d = {s[0]: [int(s[2]) - 1, int(s[3]), f'{s[1]}s']
                 for s in re.findall(p, data_dict)}
            #PESEX = f'\n({"PESEX"})\s+(\d+)\s+.*?\t+.*?(\d\d*).*?(\d\d+)'
            #print(re.findall(p, data_dict))
            #print(p)
            #print(d)
        elif (yr in ['02','01','00']):
            #p = f'\n\S\s({"|".join(var_names)})\s+(\d\d*)\s+(\d\d*)\n'
            p = f'\n\S\s({"|".join(var_names)})\s+(\d\d*)\s+(\d\d*)\n'
            #print(re.findall(p, data_dict))
            d = {s[0]: [int(s[2]) - 1, int(s[2])+int(s[1])-1, f'{s[1]}s']
                 for s in re.findall(p, data_dict)}
            #print(p)
            #print(d)

        start, end, width = zip(*d.values())
        skip = ([f'{s - e}x' for s, e in zip(start, [0] + list(end[:-1]))])
        unpack_fmt = ''.join([j for i in zip(skip, width) for j in i])
        #print(unpack_fmt)
        unpacker = struct.Struct(unpack_fmt).unpack_from

        ## files for years 2007-2019
        #raw_data = open('{0}{1}pub.dat'.format(mon, yr),'rb').readlines()
        ## files for year 2006
        #if mon in ['mar','apr','may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']:
        #    raw_data = open('{0}{1}pub.dat'.format(mon, yr),'rb').readlines()
        #elif mon in ['jan','feb']:
        #    raw_data = open('{0}{1}pub.cps'.format(mon, yr), 'rb').readlines()
        ## files for years 2000-2005
        if (yr in ['02', '03', '04', '05']) or (yr == '01' and mon != 'mar') or (yr == '00' and mon in ['aug', 'dec', 'feb', 'jun', 'nov', 'oct', 'sep']):
            raw_data = open('{0}{1}pub.cps'.format(mon,yr), 'rb').readlines()
        elif (yr == '01' and mon == 'mar') or (yr == '00' and mon in ['jan', 'apr', 'jul', 'mar', 'may']):
            raw_data = open('{0}{1}pub.dat'.format(mon, yr), 'rb').readlines()
        #print(raw_data)
        race = d[race_var]
        #print(int(raw_data[0][race[0]:race[1]]))
        aian_data = [[*map(int, unpacker(row))] for row in raw_data
                if int(row[race[0]:race[1]]) == aian_val]
        #print(data[:5])

        aian_df = pd.DataFrame(aian_data, columns=d.keys())
        # get average age
        avg_age = np.mean(aian_df[age_var])
        compiled_data.loc[count, 'AIAN Average Age'] = avg_age
        # get average earnings
        #avg_earnings = np.mean(aian_df[earn_var])
        #compiled_data.loc[count, 'AIAN Average Hourly Earnings'] = avg_earnings
        agr_prop = len(aian_df[aian_df[industry_var] == agr_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Agricultural Proportion'] = agr_prop

        mine_prop = len(aian_df[aian_df[industry_var] == mine_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Mining Proportion'] = mine_prop

        const_prop = len(aian_df[aian_df[industry_var] == const_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Construction Proportion'] = const_prop

        manu_prop = len(aian_df[aian_df[industry_var] == manu_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Manufacturing Proportion'] = manu_prop

        retail_prop = len(aian_df[aian_df[industry_var] == retail_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Retail Proportion'] = retail_prop

        transp_prop = len(aian_df[aian_df[industry_var] == transp_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Transportation Proportion'] = transp_prop

        info_prop = len(aian_df[aian_df[industry_var] == info_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Information Proportion'] = info_prop

        fin_prop = len(aian_df[aian_df[industry_var] == fin_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Financial Services Proportion'] = fin_prop

        business_prop = len(aian_df[aian_df[industry_var] == business_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Business Proportion'] = business_prop

        educ_health_prop = len(aian_df[aian_df[industry_var] == educ_health_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Education and Health Proportion'] = educ_health_prop

        hosp_prop = len(aian_df[aian_df[industry_var] == hosp_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Hospitality Proportion'] = hosp_prop

        other_prop = len(aian_df[aian_df[industry_var] == other_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Other Proportion'] = other_prop

        public_admin_prop = len(aian_df[aian_df[industry_var] == public_admin_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Public Administration Proportion'] = public_admin_prop

        mil_prop = len(aian_df[aian_df[industry_var] == mil_val]) / len(aian_df)
        compiled_data.loc[count, 'AIAN Military Proportion'] = mil_prop

        # get gender distribution
        num_males = len(aian_df[aian_df[gend_var] == male_val])
        num_females = len(aian_df[aian_df[gend_var] == female_val])
        compiled_data.loc[count, 'AIAN Num Male Respondents'] = num_males
        compiled_data.loc[count, 'AIAN Num Female Respondents'] = num_females
        male_prop = num_males / (num_males + num_females)
        female_prop = num_females / (num_males + num_females)
        compiled_data.loc[count, 'AIAN Male Proportion'] = male_prop
        compiled_data.loc[count, 'AIAN Female Proportion'] = female_prop


        compiled_data.loc[count, 'AIAN Survey Size'] = len(aian_df)

        ## this is my system to get everyone in the dataframe, since doing it by race varies each year (i.e. in how many
        ## racial/ethnic categories they allow respondents to fill in)
        gender = d[gend_var]
        all_data = [[*map(int, unpacker(row))] for row in raw_data
                     if int(row[gender[0]:gender[1]]) in [male_val,female_val]]
        all_df = pd.DataFrame(all_data, columns=d.keys())
        # get average age
        avg_age = np.mean(all_df[age_var])
        compiled_data.loc[count, 'National Average Age'] = avg_age
        # get average earnings
        #avg_earnings = np.mean(all_df[earn_var])
        #compiled_data.loc[count, 'National Average Hourly Earnings'] = avg_earnings
        agr_prop = len(all_df[all_df[industry_var] == agr_val]) / len(all_df)
        compiled_data.loc[count, 'National Agricultural Proportion'] = agr_prop

        mine_prop = len(all_df[all_df[industry_var] == mine_val]) / len(all_df)
        compiled_data.loc[count, 'National Mining Proportion'] = mine_prop

        const_prop = len(all_df[all_df[industry_var] == const_val]) / len(all_df)
        compiled_data.loc[count, 'National Construction Proportion'] = const_prop

        manu_prop = len(all_df[all_df[industry_var] == manu_val]) / len(all_df)
        compiled_data.loc[count, 'National Manufacturing Proportion'] = manu_prop

        retail_prop = len(all_df[all_df[industry_var] == retail_val]) / len(all_df)
        compiled_data.loc[count, 'National Retail Proportion'] = retail_prop

        transp_prop = len(all_df[all_df[industry_var] == transp_val]) / len(all_df)
        compiled_data.loc[count, 'National Transportation Proportion'] = transp_prop

        info_prop = len(all_df[all_df[industry_var] == info_val]) / len(all_df)
        compiled_data.loc[count, 'National Information Proportion'] = info_prop

        fin_prop = len(all_df[all_df[industry_var] == fin_val]) / len(all_df)
        compiled_data.loc[count, 'National Financial Services Proportion'] = fin_prop

        business_prop = len(all_df[all_df[industry_var] == business_val]) / len(all_df)
        compiled_data.loc[count, 'National Business Proportion'] = business_prop

        educ_health_prop = len(all_df[all_df[industry_var] == educ_health_val]) / len(all_df)
        compiled_data.loc[count, 'National Education and Health Proportion'] = educ_health_prop

        hosp_prop = len(all_df[all_df[industry_var] == hosp_val]) / len(all_df)
        compiled_data.loc[count, 'National Hospitality Proportion'] = hosp_prop

        other_prop = len(all_df[all_df[industry_var] == other_val]) / len(all_df)
        compiled_data.loc[count, 'National Other Proportion'] = other_prop

        public_admin_prop = len(all_df[all_df[industry_var] == public_admin_val]) / len(all_df)
        compiled_data.loc[count, 'National Public Administration Proportion'] = public_admin_prop

        mil_prop = len(all_df[all_df[industry_var] == mil_val]) / len(all_df)
        compiled_data.loc[count, 'National Military Proportion'] = mil_prop
        # get gender distribution
        num_males = len(all_df[all_df[gend_var] == male_val])
        num_females = len(all_df[all_df[gend_var] == female_val])
        compiled_data.loc[count, 'National Num Male Respondents'] = num_males
        compiled_data.loc[count, 'National Num Female Respondents'] = num_females
        male_prop = num_males / (num_males + num_females)
        female_prop = num_females / (num_males + num_females)
        compiled_data.loc[count, 'National Male Proportion'] = male_prop
        compiled_data.loc[count, 'National Female Proportion'] = female_prop

        compiled_data.loc[count, 'National Survey Size'] = len(all_df)



    ## data compilation format for years 2020-2022
    if yr in ['22', '21', '20']:
        filename = '{0}{1}pub.csv'.format(mon, yr)
        data = pd.read_csv(filename)
        data = data[data[race_var] == aian_val].reset_index(drop=True)
        # get average age
        avg_age = np.mean(data[age_var])
        compiled_data.loc[count, 'AIAN Average Age'] = avg_age
        # get average earnings
        #avg_earnings = np.mean(data[data[earn_var] not in []])
        #compiled_data.loc[count, 'Average Weekly Earnings'] = avg_earnings
        #compiled_data.loc[count, 'AIAN Average Hourly Earnings'] = avg_earnings
        agr_prop = len(data[data[industry_var] == agr_val]) / len(data)
        compiled_data.loc[count, 'AIAN Agricultural Proportion'] = agr_prop

        mine_prop = len(data[data[industry_var] == mine_val]) / len(data)
        compiled_data.loc[count, 'AIAN Mining Proportion'] = mine_prop

        const_prop = len(data[data[industry_var] == const_val]) / len(data)
        compiled_data.loc[count, 'AIAN Construction Proportion'] = const_prop

        manu_prop = len(data[data[industry_var] == manu_val]) / len(data)
        compiled_data.loc[count, 'AIAN Manufacturing Proportion'] = manu_prop

        retail_prop = len(data[data[industry_var] == retail_val]) / len(data)
        compiled_data.loc[count, 'AIAN Retail Proportion'] = retail_prop

        transp_prop = len(data[data[industry_var] == transp_val]) / len(data)
        compiled_data.loc[count, 'AIAN Transportation Proportion'] = transp_prop

        info_prop = len(data[data[industry_var] == info_val]) / len(data)
        compiled_data.loc[count, 'AIAN Information Proportion'] = info_prop

        fin_prop = len(data[data[industry_var] == fin_val]) / len(data)
        compiled_data.loc[count, 'AIAN Financial Services Proportion'] = fin_prop

        business_prop = len(data[data[industry_var] == business_val]) / len(data)
        compiled_data.loc[count, 'AIAN Business Proportion'] = business_prop

        educ_health_prop = len(data[data[industry_var] == educ_health_val]) / len(data)
        compiled_data.loc[count, 'AIAN Education and Health Proportion'] = educ_health_prop

        hosp_prop = len(data[data[industry_var] == hosp_val]) / len(data)
        compiled_data.loc[count, 'AIAN Hospitality Proportion'] = hosp_prop

        other_prop = len(data[data[industry_var] == other_val]) / len(data)
        compiled_data.loc[count, 'AIAN Other Proportion'] = other_prop

        public_admin_prop = len(data[data[industry_var] == public_admin_val]) / len(data)
        compiled_data.loc[count, 'AIAN Public Administration Proportion'] = public_admin_prop

        mil_prop = len(data[data[industry_var] == mil_val]) / len(data)
        compiled_data.loc[count, 'AIAN Military Proportion'] = mil_prop
        # get gender distribution
        num_males = len(data[data[gend_var] == male_val])
        num_females = len(data[data[gend_var] == female_val])
        compiled_data.loc[count, 'AIAN Num Male Respondents'] = num_males
        compiled_data.loc[count, 'AIAN Num Female Respondents'] = num_females
        male_prop = num_males / (num_males + num_females)
        female_prop = num_females / (num_males + num_females)
        compiled_data.loc[count, 'AIAN Male Proportion'] = male_prop
        compiled_data.loc[count, 'AIAN Female Proportion'] = female_prop

        compiled_data.loc[count, 'AIAN Survey Size'] = len(data)

        ## for these files it's easier to get everyone, just re-read in the file and delete the race-selecting step
        data = pd.read_csv(filename)
        # get average age
        avg_age = np.mean(data[age_var])
        compiled_data.loc[count, 'National Average Age'] = avg_age
        # get average earnings
        #avg_earnings = np.mean(data[earn_var])
        # compiled_data.loc[count, 'Average Weekly Earnings'] = avg_earnings
        #compiled_data.loc[count, 'National Average Hourly Earnings'] = avg_earnings
        agr_prop = len(data[data[industry_var] == agr_val]) / len(data)
        compiled_data.loc[count, 'National Agricultural Proportion'] = agr_prop

        mine_prop = len(data[data[industry_var] == mine_val]) / len(data)
        compiled_data.loc[count, 'National Mining Proportion'] = mine_prop

        const_prop = len(data[data[industry_var] == const_val]) / len(data)
        compiled_data.loc[count, 'National Construction Proportion'] = const_prop

        manu_prop = len(data[data[industry_var] == manu_val]) / len(data)
        compiled_data.loc[count, 'National Manufacturing Proportion'] = manu_prop

        retail_prop = len(data[data[industry_var] == retail_val]) / len(data)
        compiled_data.loc[count, 'National Retail Proportion'] = retail_prop

        transp_prop = len(data[data[industry_var] == transp_val]) / len(data)
        compiled_data.loc[count, 'National Transportation Proportion'] = transp_prop

        info_prop = len(data[data[industry_var] == info_val]) / len(data)
        compiled_data.loc[count, 'National Information Proportion'] = info_prop

        fin_prop = len(data[data[industry_var] == fin_val]) / len(data)
        compiled_data.loc[count, 'National Financial Services Proportion'] = fin_prop

        business_prop = len(data[data[industry_var] == business_val]) / len(data)
        compiled_data.loc[count, 'National Business Proportion'] = business_prop

        educ_health_prop = len(data[data[industry_var] == educ_health_val]) / len(data)
        compiled_data.loc[count, 'National Education and Health Proportion'] = educ_health_prop

        hosp_prop = len(data[data[industry_var] == hosp_val]) / len(data)
        compiled_data.loc[count, 'National Hospitality Proportion'] = hosp_prop

        other_prop = len(data[data[industry_var] == other_val]) / len(data)
        compiled_data.loc[count, 'National Other Proportion'] = other_prop

        public_admin_prop = len(data[data[industry_var] == public_admin_val]) / len(data)
        compiled_data.loc[count, 'National Public Administration Proportion'] = public_admin_prop

        mil_prop = len(data[data[industry_var] == mil_val]) / len(data)
        compiled_data.loc[count, 'National Military Proportion'] = mil_prop
        # get gender distribution
        num_males = len(data[data[gend_var] == male_val])
        num_females = len(data[data[gend_var] == female_val])
        compiled_data.loc[count, 'National Num Male Respondents'] = num_males
        compiled_data.loc[count, 'National Num Female Respondents'] = num_females
        male_prop = num_males / (num_males + num_females)
        female_prop = num_females / (num_males + num_females)
        compiled_data.loc[count, 'National Male Proportion'] = male_prop
        compiled_data.loc[count, 'National Female Proportion'] = female_prop

        compiled_data.loc[count, 'National Survey Size'] = len(data)

    count += 1

    print(compiled_data)

#print(compiled_data)
compiled_data.to_csv('aian-and-nat-compiled-data.csv',index=False)
"""

"""
data = pd.read_csv('aian-and-nat-compiled-data.csv')
print(data)
data_reversed = data.iloc[::-1].replace(0, np.nan).reset_index(drop=True)
print(data_reversed)

data_reversed.to_csv('aian-and-nat-compiled-data.csv',index=False)
"""