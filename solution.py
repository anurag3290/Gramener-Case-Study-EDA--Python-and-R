# to understand the driving factors (or driver variables) behind loan default, i.e. the variables which are strong indicators of default.
import pandas as pd
import seaborn as sns
df = pd.read_csv("~/Documents/UPGRAD/Python DS/Projects/EDA Consumer Finance Company/loan.csv")
df.info()
# RangeIndex: 39717 entries, 0 to 39716
# Columns: 111 entries, id to total_il_high_credit_limit
df.describe()
# there are a lot of columns which have same values throught the rows and NaN values
# Remove columns that have similar values
keep = [c for c in list(df) if len(df[c].unique()) > 1]
df = df[keep]
## We can also remove the columns of the dataframe that either have 0 values or have blank values
## We can remove those columns which have majorily blank values and the existing value is of no use
## we can also remove the title, desc and url
## We can remove outstanding Pricipal as it is only for current payment behaviour(not needed)
## Similarly we can remove Recoveries as it is only for Charged Off status and cant be compared
df = df.drop(['tax_liens', 'chargeoff_within_12_mths','collections_12_mths_ex_med','next_pymnt_d','title','desc','url','out_prncp','out_prncp_inv','recoveries'], axis=1)
## COlumn containing multiple data values - Separating date (already standardized) into months and date for analysis
df[['earliest_cr_line.month','earliest_cr_line.date']] = df['earliest_cr_line'].str.split('-',expand=True)
df[['issue_d.month','issue_d.date']] = df['issue_d'].str.split('-',expand=True)
## Identifying NA values
df.isnull().sum()
# Removing columns having more NA values 
df = df.drop(['mths_since_last_delinq', 'mths_since_last_record'], axis=1)
df = df.dropna(subset=['emp_length','emp_title','pub_rec_bankruptcies','last_pymnt_d','revol_util'])
df['revol_util'] = df['revol_util'].str.replace("%", "")
df['int_rate'] = df['int_rate'].str.replace("%", "")

## Starting with Univariate analysis, we can first take some insight from 'purpose'
## For deep insight we need to create a temporary dataframe called - 'purpose'
purpose = pd.crosstab(index=df["purpose"], columns=df["loan_status"])
purpose['purpose'] = purpose.index
purpose['pcnt_charged_off'] = (purpose['Charged Off']/(purpose['Charged Off']+purpose['Current']+purpose['Fully Paid']))*100
## purpose dataframe clearly shows that maximum number of loan (for all three status) is taken for 'debt_consolidation'
# "small business" tends to be the hisghest percentage in loan defaulters

# Similary we are analysisng how the grade effect on defaulter
## For deep insight we need to create a temporary dataframe called - 'grades'
grades = pd.crosstab(index=df["grade"], columns=df["loan_status"])
grades['grade'] = grades.index
grades['pcnt_charged_off'] = (grades['Charged Off']/(grades['Charged Off']+grades['Current']+grades['Fully Paid']))*100
## grades dataframe clearly shows that maximum number of loan (for all three status) is taken for Grade= 'B'
# Grade='G' tends to be the hisghest percentage in loan defaulters

## Analysing States
states = pd.crosstab(index=df["addr_state"], columns=df["loan_status"])
states['addr_state'] = states.index
states['pcnt_charged_off'] = (states['Charged Off']/(states['Charged Off']+states['Current']+states['Fully Paid']))*100
## states dataframe clearly shows that maximum number of loan (for all three status) is taken for State= 'CA'
# State='NV' tends to be the hisghest percentage in loan defaulters

## Analysing Verification
verification = pd.crosstab(index=df["verification_status"], columns=df["loan_status"])
verification['verification_status'] = verification.index
verification['pcnt_charged_off'] = (verification['Charged Off']/(verification['Charged Off']+verification['Current']+verification['Fully Paid']))*100
## state dataframe clearly shows that maximum number of loan defaulters are Not Verified
# income has been verified by LC tends to be the hisghest percentage in loan defaulters

## Analysing the Term for which loan is approved for
term = pd.crosstab(index=df["term"], columns=df["loan_status"])
term['term'] = term.index
term['pcnt_charged_off'] = (term['Charged Off']/(term['Charged Off']+term['Current']+term['Fully Paid']))*100
## term dataframe clearly shows that maximum number of loan defaulters have term of 36 months
## One interesting thing to note is that there is no Current Loan for 36 month term

## Analysing for dti_range
## defining a rank column based on the range of dti as dti_range
criteria = [df['dti'].between(0, 5), df['dti'].between(6, 10), df['dti'].between(11, 15), df['dti'].between(16, 20), df['dti'].between(21, 25), df['dti'].between(26, 30)]
values = ['0..5','6..10','11..15','16..20','21..25','26..30']
df['dti_range'] = np.select(criteria, values, 0)
dti_range = pd.crosstab(index=df["dti_range"], columns=df["loan_status"])
dti_range['dti_range'] = dti_range.index
dti_range['pcnt_charged_off'] = (dti_range['Charged Off']/(dti_range['Charged Off']+dti_range['Current']+dti_range['Fully Paid']))*100
## dti_ranges dataframe clearly shows that maximum number of loan defaulters have dti between 11-15
# 21-25 tends to be the hisghest percentage in loan defaulters

## Analysing for annual income
## defining a rank column based on the range of income as income_range
criteria = [df['annual_inc'].between(0, 10000), df['annual_inc'].between(10001, 20000), df['annual_inc'].between(20001, 30000), df['annual_inc'].between(30001, 40000), df['annual_inc'].between(40001, 50000), df['annual_inc'].between(50001, 60000), df['annual_inc'].between(60001, 70000), df['annual_inc'].between(70001, 80000), df['annual_inc'].between(80001, 90000), df['annual_inc'].between(90001, 100000), df['annual_inc'].between(100001, 500000), df['annual_inc'].between(500001, 1000000), df['annual_inc'].between(1000001, 2000000)]
values = ['0-10K','10-20K','20-30K','30-40K','30-40K','40-50K','50-60K','60-70K','70-80K','80-90K','90-100K','100-500K','500-1000K']
df['inc_range'] = np.select(criteria, values, 0)
inc_range = pd.crosstab(index=df["inc_range"], columns=df["loan_status"])
inc_range['inc_range'] = inc_range.index
inc_range['pcnt_charged_off'] = (inc_range['Charged Off']/(inc_range['Charged Off']+inc_range['Current']+inc_range['Fully Paid']))*100
## inc_ranges dataframe clearly shows that maximum number of loan defaulters have income raanges between 30-40K
## the plot shows that those who has income 0-10K tends to be the hisghest percentage in loan defaulters

## Skewness 
df.skew()
## Removing all the temporary variable
del(inc_range,purpose,term,verification,states,dti_range,grades,criteria,values,keep)

## Convert int_rate to float
df['int_rate'] = df['int_rate'].convert_objects(convert_numeric=True)
df['revol_util'] = df['revol_util'].convert_objects(convert_numeric=True)
temp = df.pivot_table(values='int_rate',index=['purpose','loan_status'], aggfunc='mean')
sns.heatmap(temp,cmap='YlGnBu')
## Three interesting facts i.e.
## 1. the average interest rate for the loan defaulters is higher than the average interest rate for Fully paid
## 2. The average interest rate for the current is highest. The reason being the recovery. For recovery purpose the interest rate have been continously increasing
## 3. In case of housing loans, the gap between avg int rate for defauters and fully paid is vry high. And contrasting to others, the avg rate for current is lower than the defaulters

## Analysing Of Rate of Interest with Various variables
sns.distplot(df.loc[df['loan_status'] == 'Charged Off', 'int_rate'])
## For Charged Off loan interest rate is higher for 11-13
## Analysis of Interst Rate with Grade

## Analysis of Interst Rate with Grade
sns.barplot(x='grade',y='int_rate',hue='loan_status',data=df)
## Grade G tends to be the higher Fully Paid borrowers for high interest of rate

## Sum of Rate Of Interest vs Home Ownership
sns.barplot(x='home_ownership',y='int_rate',hue='loan_status',data=df)
## No variation for Fully Paid the loan for HIgh Interest in terms of Home Ownership

## Correlation between loan_amnt and funded_amnt
## For Charged Off
df.loc[df['loan_status'] == 'Charged Off', 'loan_amnt'].corr(df.loc[df['loan_status'] == 'Charged Off', 'funded_amnt'])
## For Fully Piad
df.loc[df['loan_status'] == 'Fully Paid', 'loan_amnt'].corr(df.loc[df['loan_status'] == 'Fully Paid', 'funded_amnt'])
## For Current
df.loc[df['loan_status'] == 'Current', 'loan_amnt'].corr(df.loc[df['loan_status'] == 'Current', 'funded_amnt'])

## Correlation between loan_amnt and funded_amnt_inv
## For Charged Off
df.loc[df['loan_status'] == 'Charged Off', 'loan_amnt'].corr(df.loc[df['loan_status'] == 'Charged Off', 'funded_amnt_inv'])
## For Fully Piad
df.loc[df['loan_status'] == 'Fully Paid', 'loan_amnt'].corr(df.loc[df['loan_status'] == 'Fully Paid', 'funded_amnt_inv'])
## For Current
df.loc[df['loan_status'] == 'Current', 'loan_amnt'].corr(df.loc[df['loan_status'] == 'Current', 'funded_amnt_inv'])

## Correlation between funded_amnt and funded_amnt_inv
## For Charged Off
df.loc[df['loan_status'] == 'Charged Off', 'funded_amnt'].corr(df.loc[df['loan_status'] == 'Charged Off', 'funded_amnt_inv'])
## For Fully Piad
df.loc[df['loan_status'] == 'Fully Paid', 'funded_amnt'].corr(df.loc[df['loan_status'] == 'Fully Paid', 'funded_amnt_inv'])
## For Current
df.loc[df['loan_status'] == 'Current', 'funded_amnt'].corr(df.loc[df['loan_status'] == 'Current', 'funded_amnt_inv'])

## Seeing the above correlation among three, It can deduce that all three are highly correlated measure
## Except funded_amnt_inv shows slightly less correlation for Charged Off than for Fully Paid and Current
## It can be deduce that correlation funded_amnt_inv (agreed by investors) with loan amount applied should be greater than or equal to 95%

## Correlation between Interest Rate and Revolving Utilization rate
## Utilization rate is an imporatnt factor and contribute almost 30% in calculating credit score
## To Maximize credit score, we want Revolving Utilization Rate as low as possible
## For Charged Off
df.loc[df['loan_status'] == 'Charged Off', 'int_rate'].corr(df.loc[df['loan_status'] == 'Charged Off', 'revol_util'])
## For Fully Piad
df.loc[df['loan_status'] == 'Fully Paid', 'int_rate'].corr(df.loc[df['loan_status'] == 'Fully Paid', 'revol_util'])
## For Current
df.loc[df['loan_status'] == 'Current', 'int_rate'].corr(df.loc[df['loan_status'] == 'Current', 'revol_util'])
sns.barplot(x='int_rate',y='revol_util',hue='loan_status',data=df)
## Seeing the above correlation and graph folowing points can deduce:
## a. There ia a quite difference in the correlation between for charged off and For Fully Paid
## b. At interest rate 0-10% Utilization Rate for current loans decreases which is contrasting to other loan status 
## c. We conclude that 5-10% interest rate is an interest rate to which LC can consider for applications having Low Revolving Utilization Rate
