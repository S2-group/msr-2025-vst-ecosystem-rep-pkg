# library & dataset
import seaborn as sns
import pandas as pd 
import matplotlib.pyplot as plt
import scipy
import numpy as np
from datetime import date
import datetime
import matplotlib.ticker as mtick
import statsmodels.api as sm
import ast
import re

# dataframe for repos data
df_repos = pd.read_csv('CSVs Used//final_data_curated - final_data_curated_categorized.csv')
df_programming_languages = pd.read_csv('CSVs Used//only_programming_languages_used_across_categorized_repositories.csv')
df_contributors = pd.read_csv('CSVs Used//contributors_details.csv')
df_users = pd.read_csv('CSVs Used//users_details.csv')
df_users_twins = pd.read_csv('CSVs Used//users_details_twins.csv')
df_issues_content = pd.read_csv('CSVs Used//issues_content_final.csv')
df_prs_content = pd.read_csv('CSVs Used//prs_content_final.csv')
df_library_initial = pd.read_csv('CSVs Used//libraries_used.csv')
df_frameworks = pd.read_csv('CSVs Used//frameworks_used.csv')
df_scp = pd.read_csv('CSVs Used//scp_tangled.csv')
df_qg_status = pd.read_csv('CSVs Used//quality_gate_status.csv')

# date when the data was mined 
datetime_str = '2023-09-27'
datetime_object = datetime.datetime.strptime(datetime_str, '%Y-%m-%d')

##################################
########## REPO TOPICS ###########
##################################

# All tags from all the years
df_intermediary_topic = pd.DataFrame(data = df_repos['topics'])
df_intermediary_topic['topics'] = df_intermediary_topic['topics'].apply(ast.literal_eval)
df_topic = df_intermediary_topic.explode('topics')
df_topic2 = df_topic['topics'].value_counts()[:20].reset_index()
df_topic2.columns = ['Tag', 'Count']

topic = df_topic2.plot(x='Tag',y='Count',kind='bar',legend=False,figsize=(7,4))

topic.set(xlabel='Topics used')
topic.set(ylabel='Repositories count')

plt.xticks(rotation=45, ha='right')
plt.savefig("Figures//topics.png",bbox_inches='tight')
plt.clf()

# All tags from the last 5 years - the gap contains leap years so there are 1827 days
till_5_years = pd.to_datetime(date.today() - datetime.timedelta(days=1827))

df_intermediary_topic_5_years = pd.DataFrame(data = df_repos[['topics', 'created_at']])
df_intermediary_topic_5_years['created_at'] =  pd.to_datetime(df_intermediary_topic_5_years['created_at'].str.slice(0,10),format='%Y-%m-%d')
df_intermediary_topic_5_years = df_intermediary_topic_5_years.sort_values(['created_at'])

df_intermediary_topic_5_years = df_intermediary_topic_5_years[df_intermediary_topic_5_years['created_at']>=till_5_years]
df_intermediary_topic_5_years['topics'] = df_intermediary_topic_5_years['topics'].apply(ast.literal_eval)

df_topic_5_years = df_intermediary_topic_5_years.explode('topics')
df_topic2_5_years = df_topic_5_years['topics'].value_counts()[:20].reset_index()
df_topic2_5_years.columns = ['Tag', 'Count']

topic_5_years = df_topic2_5_years.plot(x='Tag',y='Count',kind='bar',legend=False,figsize=(7,4))

topic_5_years.set(xlabel='Topics used in the last five years')
topic_5_years.set(ylabel='Repositories count')

plt.xticks(rotation=45, ha='right')
plt.savefig("Figures//topics_last_5_years.png",bbox_inches='tight')
plt.clf()

# See the evolution of JUCE
plt.figure(figsize=(7,4))

topic = 'juce'
df_intermediary_topic_JUCE = pd.DataFrame(data = df_repos[['name','topics','created_at']])
df_intermediary_topic_JUCE['contains_juce_tag'] = df_intermediary_topic_JUCE['topics'].map(lambda x: topic in x)

df_topic_JUCE = df_intermediary_topic_JUCE.loc[df_intermediary_topic_JUCE['contains_juce_tag'] == True]
df_topic_JUCE['created_at'] =  pd.to_datetime(df_topic_JUCE['created_at'].str.slice(0,10),format='%Y-%m-%d')
juce_year = df_topic_JUCE['created_at'].dt.year
df_topic_JUCE['count_per_year'] = df_topic_JUCE.groupby(juce_year)['created_at'].transform('size')
juce_count = df_topic_JUCE['count_per_year']

plt.bar(juce_year,juce_count)
plt.xlabel('Year')
plt.ylabel('Respositories count')
plt.xticks(juce_year)
plt.savefig("Figures//juce_framework_over_the_years.png")
plt.clf()

#################################
########## CREATED AT ###########
#################################
plt.figure(figsize=(7,4))

# Count repos per year
df_created_at_count = pd.DataFrame(data = df_repos[['name','created_at']])
df_created_at_count['created_at'] = pd.to_datetime(df_created_at_count['created_at'].str.slice(0,10),format='%Y-%m-%d')
year = df_created_at_count['created_at'].dt.year
df_created_at_count['count_per_year'] = df_created_at_count.groupby(year)['created_at'].transform('size')
created_at_count = df_created_at_count['count_per_year']

plt.bar(year,created_at_count)
plt.xlabel('Year')
plt.ylabel('Respositories count')
plt.xticks(year,rotation=45, ha='right')
plt.savefig("Figures//created_at.png",bbox_inches='tight')
plt.clf()

###################################
########## SIZE OF REPO ###########
###################################
plt.figure(figsize=(7,4))

size = sns.violinplot(data=df_repos, y=df_repos[df_repos["size"]<100000]["size"], palette='Blues_d')
sns.boxplot(y=df_repos[df_repos["size"]<100000]["size"], data=df_repos, palette='Blues', width=0.3,boxprops={'zorder': 2})

size.set(ylim = (0,99900))
size.set(xlabel='Repositories', ylabel='Size (Kb)')

plt.savefig("Figures//size.png",bbox_inches='tight')
plt.clf()

###########################################
########## PROGRAMMING LANGUAGE ###########
###########################################
plt.figure(figsize=(4,11))

df_language_count = pd.DataFrame(data = df_programming_languages['language'])
df_language_count['count_per_language'] = df_language_count.groupby(df_programming_languages['language'])['language'].transform('size')

plt.barh(df_programming_languages['language'],df_language_count['count_per_language'],height = 0.9)
plt.xlabel('Respositories count')
plt.ylabel('Programming language',labelpad = 10)
plt.savefig("Figures//programming_language.png",bbox_inches='tight')
plt.clf()

#############################################
########## PROVIDED FUNCTIONALITY ###########
#############################################
plt.figure(figsize=(10,150))

df_provided_functionality = pd.DataFrame(data = df_repos['functionality'])
df_provided_functionality['count_per_functionality'] = df_provided_functionality.groupby(df_repos['functionality'])['functionality'].transform('size')
xlabels = df_provided_functionality['functionality']
xlabels_new = [re.sub("(.{90})", "\\1\n", label, 0, re.DOTALL) for label in xlabels]
plt.barh(xlabels_new,df_provided_functionality['count_per_functionality'])
plt.xlabel('Respositories count')
plt.ylabel('Provided functionality in-depth')
plt.savefig("Figures//provided_functionality.png",bbox_inches='tight')
plt.clf()

# Only the first 10 results
df_provided_functionality['functionality'].value_counts().head(10).plot(kind='barh',legend=False,figsize=(4,10))
plt.xlabel('Respositories count')
plt.ylabel('Provided functionality in-depth')
plt.savefig("Figures//provided_functionality_first_results.png",bbox_inches='tight')
plt.clf()

#core functionality
plt.figure(figsize=(7,4))
df_core_provided_functionality = pd.DataFrame(data = df_repos['core_functionality'].str.strip())
df_core_provided_functionality['count_per_functionality'] = df_core_provided_functionality.groupby(df_repos['core_functionality'])['core_functionality'].transform('size')
plt.bar(df_core_provided_functionality['core_functionality'],df_core_provided_functionality['count_per_functionality'])
plt.xlabel('Provided core functionality')
plt.ylabel('Respositories count')
plt.savefig("Figures//core_provided_functionality.png",bbox_inches='tight')
plt.clf()

#######################################
########## MEGA CONTRIBUTOR ###########
#######################################
plt.figure(figsize=(10,100))

df_mega_contributor = pd.DataFrame(data = df_contributors['login'])
df_mega_contributor['count_per_contributor'] = df_mega_contributor.groupby(df_mega_contributor['login'])['login'].transform('size')

plt.barh(df_mega_contributor['login'],df_mega_contributor['count_per_contributor'])
plt.xlabel('Respositories count')
plt.ylabel('Owners and contributors GitHub usernames')
plt.savefig("Figures//mega_contributor.png",bbox_inches='tight')
plt.clf()

# Only the first 10 results
df_mega_contributor['login'].value_counts().head(10).plot(kind='bar',legend=False,figsize=(7,4))
plt.xlabel('Owners and contributors GitHub usernames')
plt.ylabel('Respositories count')
plt.xticks(rotation=45, ha='right')
plt.savefig("Figures//mega_contributor_first_results.png",bbox_inches='tight')
plt.clf()

df_mega_contributor = df_contributors.filter(['login','url'])
df_mega_contributor.drop_duplicates(subset='login', keep='first', inplace=True)
df_mega_contributor.to_csv("Output CSVs//contributors_across_repositories.csv")


##################################
########## STARS COUNT ###########
##################################
plt.figure(figsize=(7,4))

stars = sns.violinplot(data=df_repos, y=df_repos[df_repos["stargazers_count"]<200]["stargazers_count"], palette='Blues_d')
sns.boxplot(y=df_repos[df_repos["stargazers_count"]<200]["stargazers_count"], data=df_repos, palette='Blues', width=0.3,boxprops={'zorder': 2})

stars.set(ylim = (0,220))
stars.set(xlabel='Repositories', ylabel='Stars')

plt.tight_layout()
plt.savefig("Figures//stars_count.png",bbox_inches='tight')
plt.clf()

#####################################
########## WATCHERS COUNT ###########
#####################################
plt.figure(figsize=(7,4))

watchers = sns.violinplot(data=df_repos, y=df_repos[df_repos["subscribers_count"]<30]["subscribers_count"], palette='Blues_d')
sns.boxplot(y=df_repos[df_repos["subscribers_count"]<30]["subscribers_count"], data=df_repos, palette='Blues', width=0.3,boxprops={'zorder': 2})

watchers.set(ylim = (0,32))
watchers.set(xlabel='Repositories', ylabel='Watchers')

plt.tight_layout()
plt.savefig("Figures//watchers_count.png",bbox_inches='tight')
plt.clf()

##################################
########## FORKS COUNT ###########
##################################
plt.figure(figsize=(7,4))

forks = sns.violinplot(data=df_repos, y=df_repos[df_repos["forks_count"]<100]["forks_count"], palette='Blues_d')
sns.boxplot(y=df_repos[df_repos["forks_count"]<100]["forks_count"], data=df_repos, palette='Blues', width=0.3,boxprops={'zorder': 2})

forks.set(ylim = (0,110))
forks.set(xlabel='Repositories', ylabel='Forks')

plt.tight_layout()
plt.savefig("Figures//forks_count.png",bbox_inches='tight')
plt.clf()

#####################################
########## DEFAULT BRANCH ###########
#####################################
plt.figure(figsize=(7,4))

df_branch_count = pd.DataFrame(data = df_repos[['name','default_branch']])
branch = df_repos['default_branch']
df_branch_count['count_per_branch'] = df_branch_count.groupby(branch)['default_branch'].transform('size')
branch_count = df_branch_count['count_per_branch']

plt.bar(branch,branch_count)
plt.xlabel('Default branch')
plt.ylabel('Respositories count')
plt.savefig("Figures//default_branch.png",bbox_inches='tight')
plt.clf()

#############################
########## ISSUES ###########
#############################
issues_fig, ax_issues = plt.subplots(ncols=3,figsize=(7,4),sharey=True)

df_issues = pd.DataFrame(data = df_repos[['open_issues_only','closed_issues_only']])
df_issues['total_issues'] = df_issues['open_issues_only'] + df_issues['closed_issues_only']

issues_opened = sns.violinplot(data=df_issues, y=df_issues[df_issues["open_issues_only"]<15]["open_issues_only"], palette='Reds_d', ax=ax_issues[0])
sns.boxplot(y=df_issues[df_issues["open_issues_only"]<15]["open_issues_only"], data=df_issues, palette='Reds', width=0.3,boxprops={'zorder': 2},ax=ax_issues[0])
issues_closed = sns.violinplot(data=df_issues,  y=df_issues[df_issues["closed_issues_only"]<15]["closed_issues_only"], palette='Blues_d', ax=ax_issues[1])
sns.boxplot(y=df_issues[df_issues["closed_issues_only"]<15]["closed_issues_only"], data=df_issues,  palette='Blues', width=0.3,boxprops={'zorder': 2},ax=ax_issues[1])
issues_merged = sns.violinplot(data=df_issues, y=df_issues[df_issues["total_issues"]<15]["total_issues"], palette='Greens_d', ax=ax_issues[2])
sns.boxplot(y=df_issues[df_issues["total_issues"]<15]["total_issues"], data=df_issues, palette='Greens', width=0.3,boxprops={'zorder': 2},ax=ax_issues[2])

issues_opened.set(ylim = (0,17))
issues_opened.set(title='Opened issues')
issues_opened.set(xlabel='Repositories', ylabel='Issues')

issues_closed.set(ylim = (0,17))
issues_closed.set(title='Closed issues')
issues_closed.set(xlabel='Repositories', ylabel='Issues')

issues_merged.set(ylim = (0,17))
issues_merged.set(title='Total issues')
issues_merged.set(xlabel='Repositories', ylabel='Issues')

plt.tight_layout()
plt.savefig("Figures//issues.png",bbox_inches='tight')
plt.clf()

####################################
########## PULL REQUESTS ###########
####################################
prs_fig, ax_prs = plt.subplots(ncols=3,figsize=(7,4),sharey=True)

df_prs = pd.DataFrame(data = df_repos[['open_pull_requests','closed_pull_requests']])
df_prs['total_pull_requests'] = df_prs['open_pull_requests'] + df_prs['closed_pull_requests']

prs_opened = sns.violinplot(data=df_prs, y=df_prs[df_prs["open_pull_requests"]<15]["open_pull_requests"], palette='Reds_d', ax=ax_prs[0])
sns.boxplot(y=df_prs[df_prs["open_pull_requests"]<15]["open_pull_requests"], data=df_prs, palette='Reds', width=0.3,boxprops={'zorder': 2},ax=ax_prs[0])
prs_closed = sns.violinplot(data=df_prs, y=df_prs[df_prs["closed_pull_requests"]<15]["closed_pull_requests"], palette='Blues_d', ax=ax_prs[1])
sns.boxplot(y=df_prs[df_prs["closed_pull_requests"]<15]["closed_pull_requests"], data=df_prs,  palette='Blues', width=0.3,boxprops={'zorder': 2},ax=ax_prs[1])
prs_merged = sns.violinplot(data=df_prs, y=df_prs[df_prs["total_pull_requests"]<15]["total_pull_requests"], palette='Greens_d', ax=ax_prs[2])
sns.boxplot(y=df_prs[df_prs["total_pull_requests"]<15]["total_pull_requests"], data=df_prs, palette='Greens', width=0.3,boxprops={'zorder': 2},ax=ax_prs[2])

prs_opened.set(ylim = (0,13))
prs_opened.set(title='Opened pull requests')
prs_opened.set(xlabel='Repositories', ylabel='Pull requests')

prs_closed.set(ylim = (0,13))
prs_closed.set(title='Closed pull requests')
prs_closed.set(xlabel='Repositories', ylabel='Pull requests')

prs_merged.set(ylim = (0,13))
prs_merged.set(title='Total pull requests')
prs_merged.set(xlabel='Repositories', ylabel='Pull requests')

plt.tight_layout()
plt.savefig("Figures//prs.png",bbox_inches='tight')
plt.clf()

####################################
########## COMMITS COUNT ###########
####################################
plt.figure(figsize=(7,4))

commits = sns.violinplot(data=df_repos, y=df_repos[df_repos["commits_number"]<250]["commits_number"], palette='Blues_d')
sns.boxplot(y=df_repos[df_repos["commits_number"]<250]["commits_number"], data=df_repos, palette='Blues', width=0.3,boxprops={'zorder': 2})

commits.set(ylim = (0,270))
commits.set(xlabel='Repositories')
commits.set(ylabel='Commits')

plt.tight_layout()
plt.savefig("Figures//commits.png",bbox_inches='tight')
plt.clf()

#########################################
########## CONTRIBUTORS COUNT ###########
#########################################
plt.figure(figsize=(7,4))

contributors = sns.violinplot(data=df_repos, y=df_repos[df_repos["contributors_number"]<10]["contributors_number"], palette='Blues_d')
sns.boxplot(y=df_repos[df_repos["contributors_number"]<10]["contributors_number"], data=df_repos, palette='Blues', width=0.3,boxprops={'zorder': 2})

contributors.set(ylim = (0,11))
contributors.set(xlabel='Repositories')
contributors.set(ylabel='Contributors')

plt.tight_layout()
plt.savefig("Figures//contributors.png",bbox_inches='tight')
plt.clf()

###########################################
########## ORGANIZATION VS USER ###########
###########################################
plt.figure(figsize=(7,4))

df_organization = pd.DataFrame(data = df_repos['owner.type'])
df_organization['count_per_organization'] = df_organization.groupby(df_organization['owner.type'])['owner.type'].transform('size')

plt.bar(df_organization['owner.type'],df_organization['count_per_organization'])
plt.xlabel('Owner type')
plt.ylabel('Respositories count')
plt.savefig("Figures//organization_vs_user.png",bbox_inches='tight')
plt.clf()

# Repositories created per organization
df_organizations = df_repos.loc[df_repos['owner.type'] == 'Organization']
df_organizations['organization_name'] = df_organizations['full_name'].str.split('/',expand=True)[0]

df_organizations_repositories = df_organizations['organization_name'].value_counts().reset_index()
df_organizations_repositories.columns = ['Organization', 'Repositories developed']
df_organizations_repositories.to_csv('Output CSVs//total_number_of_repositories_developed_by_organizations.csv')

repositories_per_organization = df_organizations_repositories.plot.barh(x='Organization',y='Repositories developed',legend=False,figsize=(4,11))

repositories_per_organization.set(xlabel='Repositories developed count')
repositories_per_organization.set(ylabel='Organization\'s name')

plt.savefig("Figures//repositories_per_organization.png",bbox_inches='tight')
plt.clf()

################################
########## LIBRARIES ###########
################################
plt.figure(figsize=(7,4))
df_hf_clf = pd.DataFrame(data = df_repos['hf_or_clf'].dropna())
df_hf_clf['count_per_hl_clf'] = df_hf_clf.groupby(df_hf_clf['hf_or_clf'])['hf_or_clf'].transform('size')

plt.bar(df_hf_clf['hf_or_clf'],df_hf_clf['count_per_hl_clf'])
plt.xlabel('C and C++ library type')
plt.ylabel('Respositories count')
plt.savefig("Figures//hf_clf.png",bbox_inches='tight')
plt.clf()

# Libraries
plt.figure(figsize=(10,40))
df_library = pd.DataFrame(data = df_library_initial['libraries'].dropna())
df_library['count_per_library'] = df_library.groupby(df_library['libraries'])['libraries'].transform('size')

plt.barh(df_library['libraries'],df_library['count_per_library'])
plt.xlabel('Respositories count')
plt.ylabel('Library')
plt.savefig("Figures//library.png",bbox_inches='tight')
plt.clf()


# Only the first 10 results
df_library['libraries'].value_counts().head(10).plot(kind='barh',legend=False,figsize=(4,10))
plt.xlabel('Respositories count')
plt.ylabel('Library')
plt.savefig("Figures//libraries_first_results.png",bbox_inches='tight')
plt.clf()


################################
########## FRAMEWORK ###########
################################
plt.figure(figsize=(4,10))

df_frameworks_repos = pd.DataFrame(data = df_frameworks['frameworks'])
df_frameworks_repos['count_per_framework'] = df_frameworks_repos.groupby(df_frameworks_repos['frameworks'])['frameworks'].transform('size')

plt.barh(df_frameworks_repos['frameworks'],df_frameworks_repos['count_per_framework'])
plt.xlabel('Respositories count')
plt.ylabel('Framework')
plt.savefig("Figures//frameworks.png",bbox_inches='tight')
plt.clf()


###################################
########## LICENSE TYPE ###########
###################################
plt.figure(figsize=(7,4))

df_license_count = pd.DataFrame(data = df_repos[['name','license.key']])
license = df_repos['license.key']
df_license_count['count_per_license'] = df_license_count.groupby(license)['license.key'].transform('size')
license_count = df_license_count['count_per_license']

plt.bar(license,license_count)
plt.xlabel('License type')
plt.ylabel('Respositories count')
plt.xticks(rotation=45, ha='right')
plt.savefig("Figures//licenses.png",bbox_inches='tight')
plt.clf()

# All licenses from the last 5 years - the gap contains leap years so there are 1827 days
plt.figure(figsize=(7,4))

till_5_years = pd.to_datetime(date.today() - datetime.timedelta(days=1827))

df_intermediary_license_5_years = pd.DataFrame(data = df_repos[['license.key', 'created_at']])
df_intermediary_license_5_years['created_at'] =  pd.to_datetime(df_intermediary_license_5_years['created_at'].str.slice(0,10),format='%Y-%m-%d')
df_intermediary_license_5_years = df_intermediary_license_5_years.sort_values(['created_at'])

df_intermediary_license_5_years = df_intermediary_license_5_years[df_intermediary_license_5_years['created_at']>=till_5_years]

df_license_5_years = df_intermediary_license_5_years['license.key'].value_counts().reset_index()
df_license_5_years.columns = ['License', 'Count']

#license_5_years = df_license_5_years.plot(x='License',y='Count',kind='bar',legend=False,figsize=(15,10))

plt.bar(df_license_5_years['License'],df_license_5_years['Count'])
plt.xlabel('License type')
plt.ylabel('Respositories count')
plt.xticks(rotation=45, ha='right')
plt.savefig("Figures//licenses_last_5_years.png",bbox_inches='tight')
plt.clf()

###############################
########## REPO AGE ###########
###############################
plt.figure(figsize=(7,4))

df_age = pd.DataFrame(data = df_repos[['name','created_at','updated_at']])
df_age['created_at'] = pd.to_datetime(df_age['created_at'].str.slice(0,10),format='%Y-%m-%d')
df_age['updated_at'] = pd.to_datetime(df_age['updated_at'].str.slice(0,10),format='%Y-%m-%d')
df_age['days_age'] =  (df_age['updated_at'] - df_age['created_at']).dt.days
df_age.to_csv('Output CSVs//repositories_age.csv')

age = sns.violinplot(data=df_age, y=df_age['days_age'], palette='Blues_d')
sns.boxplot(y=df_age['days_age'], data=df_age, palette='Blues', width=0.3,boxprops={'zorder': 2})

age.set(ylim = (0,5200))
age.set(xlabel='Repositories')
age.set(ylabel='Repositories\' age expressed in days')

plt.savefig("Figures//age.png",bbox_inches='tight')
plt.clf()

#####################################
########## REPO FRESHNESS ###########
#####################################
plt.figure(figsize=(7,4))

df_freshness = pd.DataFrame(data = df_repos[['name','pushed_at']])
df_freshness['pushed_at'] = pd.to_datetime(df_freshness['pushed_at'].str.slice(0,10),format='%Y-%m-%d')
df_freshness['today_at'] = datetime_object

df_freshness['freshness_days'] =  (df_freshness['today_at'] - df_freshness['pushed_at']).dt.days
df_freshness.to_csv('Output CSVs//repositories_freshness.csv')

freshness = sns.violinplot(data=df_freshness, y=df_freshness['freshness_days'], palette='Blues_d')
sns.boxplot(y=df_freshness['freshness_days'], data=df_freshness, palette='Blues', width=0.3,boxprops={'zorder': 2})

freshness.set(ylim = (0,3700))
freshness.set(xlabel='Repositories')
freshness.set(ylabel='Repositories\' freshness in days')

plt.savefig("Figures//freshness.png",bbox_inches='tight')
plt.clf()

#################################
########## USERS AGE ############
#################################
plt.figure(figsize=(7,4))

df_users_age = pd.DataFrame(data = df_users[['login','created_at']])
df_users_age['created_at'] = pd.to_datetime(df_users_age['created_at'].str.slice(0,10),format='%Y-%m-%d')
df_users_age['today_at'] = datetime_object

df_users_age['age_in_days'] =  (df_users_age['today_at'] - df_users_age['created_at']).dt.days
#user_age = df_users_age.plot(x='login',y='age_in_days',kind='bar',legend=False,figsize=(100,10))

user_age = sns.violinplot(data=df_users_age, y=df_users_age['age_in_days'], palette='Blues_d')
sns.boxplot(y=df_users_age['age_in_days'], data=df_users_age, palette='Blues', width=0.3,boxprops={'zorder': 2})
user_age.set(ylim = (0,3700))
user_age.set(xlabel='Owners and contributors on GitHub')
user_age.set(ylabel='Their GitHub profiles age in days')

plt.savefig("Figures//users_age.png",bbox_inches='tight')
plt.clf()

df_users_age.to_csv("Output CSVs/users_details.csv")

##### FOR TWINS #####
plt.figure(figsize=(7,4))

df_users_age = pd.DataFrame(data = df_users_twins[['login','created_at']])
df_users_age['created_at'] = pd.to_datetime(df_users_age['created_at'].str.slice(0,10),format='%Y-%m-%d')
df_users_age['today_at'] = datetime_object

df_users_age['age_in_days'] =  (df_users_age['today_at'] - df_users_age['created_at']).dt.days
#user_age = df_users_age.plot(x='login',y='age_in_days',kind='bar',legend=False,figsize=(100,10))

user_age = sns.violinplot(data=df_users_age, y=df_users_age['age_in_days'], palette='Blues_d')
sns.boxplot(y=df_users_age['age_in_days'], data=df_users_age, palette='Blues', width=0.3,boxprops={'zorder': 2})
user_age.set(ylim = (0,3700))
user_age.set(xlabel='Owners and contributors on GitHub')
user_age.set(ylabel='Their GitHub profiles age in days')

plt.savefig("Figures//users_age_twins.png",bbox_inches='tight')
plt.clf()

df_users_age.to_csv("Output CSVs/users_details_twins.csv")

#############################################
########## TIME TO CLOSE AN ISSUE ###########
#############################################
plt.figure(figsize=(7,4))

mask = (df_issues_content['created_at'].str.slice(0,10) < '2023-09-27') & (df_issues_content['closed_at'].str.slice(0,10) < '2023-09-27')
df_time_issue = df_issues_content.loc[mask]

df_time_issue['created_at'] = pd.to_datetime(df_time_issue['created_at'].str.slice(0,10),format='%Y-%m-%d')
df_time_issue['closed_at'] = pd.to_datetime(df_time_issue['closed_at'].str.slice(0,10),format='%Y-%m-%d')
df_time_issue['days_age'] =  (df_time_issue['closed_at'] - df_time_issue['created_at']).dt.days
df_time_issue.to_csv('Output CSVs//Issues_content_until_27.09.2023.csv')

issue = sns.violinplot(data=df_time_issue, y=df_time_issue['days_age'], palette='Blues_d')
sns.boxplot(y=df_time_issue['days_age'], data=df_time_issue, palette='Blues', width=0.3,boxprops={'zorder': 2})
issue.set(ylim = (0,2700))
issue.set(xlabel='Repositories')
issue.set(ylabel='Time in days to close an issue')

plt.savefig("Figures//issue_close_time.png",bbox_inches='tight')
plt.clf()

df_issues_content['repository_url'] = df_issues_content['repository_url'].astype(str)
mask_surge = (df_issues_content['created_at'].str.slice(0,10) < '2023-09-27') & (df_issues_content['closed_at'].str.slice(0,10) < '2023-09-27') & (df_issues_content['repository_url'].astype(str)=='https://api.github.com/repos/surge-synthesizer/surge')
df_time_issue_surge = df_issues_content.loc[mask_surge]

df_time_issue_surge['created_at'] = pd.to_datetime(df_time_issue_surge['created_at'].str.slice(0,10),format='%Y-%m-%d')
df_time_issue_surge['closed_at'] = pd.to_datetime(df_time_issue_surge['closed_at'].str.slice(0,10),format='%Y-%m-%d')
df_time_issue_surge['days_age'] =  (df_time_issue_surge['closed_at'] - df_time_issue_surge['created_at']).dt.days
df_time_issue_surge.to_csv('Output CSVs//Issues_content_until_27.09.2023_Surge.csv')

#########################################
########## TIME TO CLOSE A PR ###########
#########################################
plt.figure(figsize=(7,4))

mask = (df_prs_content['created_at'].str.slice(0,10) < '2023-09-27') & (df_prs_content['closed_at'].str.slice(0,10) < '2023-09-27')
df_time_pr = df_prs_content.loc[mask]
df_time_pr['created_at'] = pd.to_datetime(df_time_pr['created_at'].str.slice(0,10),format='%Y-%m-%d')
df_time_pr['closed_at'] = pd.to_datetime(df_time_pr['closed_at'].str.slice(0,10),format='%Y-%m-%d')
df_time_pr['days_age'] =  (df_time_pr['closed_at'] - df_time_pr['created_at']).dt.days
df_time_pr.to_csv("Output CSVs//PR_content_until_27.09.2023.csv")

# Time to close a pull request
pr = sns.violinplot(data=df_time_pr, y=df_time_pr['days_age'], palette='Blues_d')
sns.boxplot(y=df_time_pr['days_age'], data=df_time_pr, palette='Blues', width=0.3,boxprops={'zorder': 2})
pr.set(ylim = (0,2700))
pr.set(xlabel='Repositories')
pr.set(ylabel='Time in days to close a pull request')
plt.savefig("Figures//pr_close_time.png",bbox_inches='tight')
plt.clf()

######################################################
########## ISSUES THAT ARE CLOSED VIA A PR ###########
######################################################
plt.figure(figsize=(7,4))

mask = (df_issues_content['created_at'].str.slice(0,10) < '2023-09-27') & (df_issues_content['closed_at'].str.slice(0,10) < '2023-09-27') & (df_issues_content['state'] == 'closed')
df_tied_issues = df_issues_content.loc[mask]
df_tied_issues['closed_via_pr'] = df_tied_issues['pull_request.url'].fillna('Not closed via a PR')
df_tied_issues['closed_via_pr'] = df_tied_issues['closed_via_pr'].mask(df_tied_issues['closed_via_pr'].str.startswith('https'),'Closed via a PR')

issue_closed_type = df_tied_issues['closed_via_pr']
df_tied_issues['count_per_issue'] = df_tied_issues.groupby(issue_closed_type)['closed_via_pr'].transform('size')
issue_closed_type_count = df_tied_issues['count_per_issue']

plt.bar(issue_closed_type,issue_closed_type_count)
plt.xlabel('Closed or not closed via a pull request')
plt.ylabel('Issues')
plt.savefig("Figures//issue_closed_via_pr.png",bbox_inches='tight')
plt.clf()

##################################
########## SCP TANGLED ###########
##################################
plt.figure(figsize=(4,11))

df_scp_tangled = pd.DataFrame(data = df_scp['SQ_SCP_tangled'])
scp = df_scp_tangled['SQ_SCP_tangled']
df_scp_tangled['count_per_scp'] = df_scp_tangled.groupby(scp)['SQ_SCP_tangled'].transform('size')
scp_count = df_scp_tangled['count_per_scp']

plt.barh(scp,scp_count)
plt.xlabel('Respositories count')
plt.ylabel('Standard code practice tangled')
plt.savefig("Figures//scp_tangled.png",bbox_inches='tight')
plt.clf()

df_scp_tangled.drop_duplicates(subset="SQ_SCP_tangled", keep='first', inplace=True)
df_scp_tangled.to_csv("Output CSVs//scp_tangled_count.csv")

##############################
########## TESTING ###########
##############################
plt.figure(figsize=(7,4))

df_testing = pd.DataFrame(data = df_repos[['testing_mentioned_in_README_file','testing']])
df_testing['count_per_README'] = df_testing.groupby(df_testing['testing_mentioned_in_README_file'])['testing_mentioned_in_README_file'].transform('size')
df_testing['count_per_TESTING'] = df_testing.groupby(df_testing['testing'])['testing'].transform('size')


plt.bar(df_testing['testing_mentioned_in_README_file'],df_testing['count_per_README'])
plt.xlabel('Manual testing mentioned in README file')
plt.ylabel('Respositories count')
plt.savefig("Figures//testing_readme.png",bbox_inches='tight')
plt.clf()

plt.figure(figsize=(7,4))
plt.bar(df_testing['testing'],df_testing['count_per_TESTING'])
plt.xlabel('Software testing method')
plt.ylabel('Respositories count')
plt.savefig("Figures//testing.png",bbox_inches='tight')
plt.clf()

###################################
########## QUALITY GATE ###########
###################################
plt.figure(figsize=(7,4))

df_qg_status['SQ_quality_gate_count'] = df_qg_status.groupby(df_qg_status['SQ_quality_gate_status'])['SQ_quality_gate_status'].transform('size')

plt.bar(df_qg_status['SQ_quality_gate_status'],df_qg_status['SQ_quality_gate_count'])
plt.xlabel('Quality gate status')
plt.ylabel('Respositories count')
plt.savefig("Figures//qg.png",bbox_inches='tight')
plt.clf()

###############################
########## COMMENTS ###########
###############################
plt.figure(figsize=(7,4))

df_comments = pd.DataFrame(data = df_repos['comments'])
df_comments['comments_percentages'] = df_repos['comments'].str.slice(0,2)

comments = sns.violinplot(data=df_comments, y=df_comments['comments_percentages'].astype(float), palette='Blues_d')
sns.boxplot(y=df_comments['comments_percentages'].astype(float), data=df_comments, palette='Blues', width=0.3,boxprops={'zorder': 2})

comments.set(xlabel='Repositories')
comments.set(ylabel='Comments lines density (percentages)')
comments.set(ylim = (0,77))
comments.yaxis.set_major_formatter(mtick.PercentFormatter())

comments.yaxis.set_tick_params(labelbottom=True)
plt.savefig("Figures//comments.png",bbox_inches='tight')
plt.clf()

