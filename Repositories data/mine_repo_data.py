import requests             # Used for Git REST API requests
import json                 # Used to load data into JSON format
from pprint import pprint   # pretty-print
import pandas as pd         # Used to create dataframes
import glob                 # Used for merging the JSON files
import numpy as np          # For arrays 
import time                 # For .sleep(x) method, execute a GET GIT API request every x seconds
import re                   # To get the total number of commits, contributors for a repo, it's acting like 'sed' for linux
import glob, os, shutil     # For creating twin repos folders
import pathlib

TOKEN = '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'
starttime = time.time()


def mine_git_repos_demographic_basic_data():
    topics = ['vst','vst2','vst3','vsts','vst-plugin','vsti','vstfx']
    for topic in topics:
        for x in range(10):
            urls = requests.get('https://api.github.com/search/repositories?q=topic:'+topic+'&page='+str(x)+'&per_page=100', headers={'Authorization': 'Bearer '+TOKEN}) # (your url)
            data = urls.json()
            with open('repo_demographic_mined_data//data-topic_'+topic+'_pageNumber_'+str(x)+'.json', 'w') as f:
                json.dump(data, f)

def mine_git_repos_programming_languages():
    # Loading categorized csv file as dataframe
    df = pd.read_csv('repo_final_mined_data//curated_csv//final_data_curated - final_data_curated_categorized.csv')
    repos_name = list(df['full_name'])
    for repo in repos_name:
        time.sleep(10)
        repo_name = repo.replace('/','-')
        urls = requests.get('https://api.github.com/repos/'+repo+'/languages', headers={'Authorization': 'Bearer '+TOKEN})
        data = urls.json()
        with open('repo_final_mined_data//languages//'+repo_name+'_languages_list.json', 'w') as f:
            json.dump(data, f)
        pprint('programming languages list gathered for repo: ' + repo)
    # For repo programming languages
    merged_contents_programming_languages = []
    for f in glob.glob('repo_final_mined_data/languages/*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_programming_languages.append(a_dict)
    with open('repo_final_mined_data//mined_repo_programming_languages.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_programming_languages, file_out)
    
    # only for the programming language
    df_programming_languages = pd.DataFrame()
    programming_languages = []
    with open("repo_final_mined_data//mined_repo_programming_languages.json") as jsonFile:
        data = json.load(jsonFile)
        for x in data:
            programming_languages.append(x.keys())
    df_programming_languages['language'] = programming_languages
    df_only_programming_languages = df_programming_languages.explode('language')
    df_only_programming_languages.to_csv('repo_final_mined_data//curated_csv//only_programming_languages_used_across_categorized_repositories.csv')
    

def mine_git_repos_contributors():
    # Loading categorized csv file as dataframe
    df = pd.read_csv('repo_final_mined_data//curated_csv//final_data_curated - final_data_curated_categorized.csv')
    repos_name = list(df['full_name'])
    for repo in repos_name:
        time.sleep(10)
        repo_name = repo.replace('/','-')
        urls = requests.get('https://api.github.com/repos/'+repo+'/contributors', headers={'Authorization': 'Bearer '+TOKEN})
        data = urls.json()
        with open('repo_final_mined_data//contributors//'+repo_name+'_contributors_list.json', 'w') as f:
            json.dump(data, f)
        pprint('contributors list gathered for repo: ' + repo)

    merged_contents_contributors = []
    for f in glob.glob('repo_final_mined_data/contributors/*.json'):
        with open(f, "r") as file_in:
            merged_contents_contributors += json.load(file_in)
    with open("repo_final_mined_data//mined_repo_contributors.json", "w") as file_out:
        json.dump(merged_contents_contributors, file_out)
    
    with open('repo_final_mined_data//mined_repo_contributors.json') as file_in:
         data = json.load(file_in)
    df_contributors = pd.json_normalize(data)
    df_contributors.to_csv('repo_final_mined_data//curated_csv//contributors_details.csv')

def mine_users_age():
    # Loading categorized csv file as dataframe
    df = pd.read_csv('Quantitative analysis//Output CSVs//contributors_across_repositories.csv')
    users_name = list(df['login'])
    for user in users_name:
        time.sleep(10)
        urls = requests.get('https://api.github.com/users/'+ user, headers={'Authorization': 'Bearer '+TOKEN})
        data = urls.json()
        with open('repo_final_mined_data//users//'+user+'_details.json', 'w') as f:
            json.dump(data, f)
        pprint('details gathered for user: ' + user)

    merged_contents_users = []
    for f in glob.glob('repo_final_mined_data/users/*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_users.append(a_dict)
    with open('repo_final_mined_data//mined_repo_users_details.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_users, file_out)
    
    with open('repo_final_mined_data//mined_repo_users_details.json') as file_in:
         data = json.load(file_in)
    users_details = pd.json_normalize(data)
    users_details.to_csv('repo_final_mined_data//curated_csv//users_details.csv')

def mine_git_issues_content():
    # Loading categorized csv file as dataframe
    df = pd.read_csv('repo_final_mined_data//curated_csv//repos_that_have_issues_and_prs.csv')
    repos_name = list(df['full_name'])
    for repo in repos_name:
        for x in range(12):
            time.sleep(10)
            repo_name = repo.replace('/','-')
            urls = requests.get('https://api.github.com/search/issues?q=repo:'+repo+'&is:issue&is:open&page='+str(x)+'&per_page=100', headers={'Authorization': 'Bearer '+TOKEN})
            data = urls.json()
            with open('repo_final_mined_data//issues//'+repo_name+'_issues_pageNumber_'+str(x)+'.json', 'w') as f:
                json.dump(data, f)
            pprint('issues gathered for repo: ' + repo + 'page' + str(x))
    merged_issues_content = []
    for f in glob.glob('repo_final_mined_data//issues//*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_issues_content.append(a_dict)
    with open('repo_final_mined_data//mined_repo_issues_content.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_issues_content, file_out)
    with open('repo_final_mined_data//mined_repo_issues_content.json') as file_in:
        data = json.load(file_in)
    df_issues = pd.json_normalize(data)
    df_issues_content = pd.json_normalize(df_issues['items'].explode())
    df_issues_content.to_csv('repo_final_mined_data//curated_csv//issues_content_final.csv')

def mine_git_prs_content():
    # Loading categorized csv file as dataframe
    df = pd.read_csv('repo_final_mined_data//curated_csv//repos_that_have_issues_and_prs.csv')
    repos_name = list(df['full_name'])
    for repo in repos_name:
        for x in range(12):
            time.sleep(10)
            repo_name = repo.replace('/','-')
            urls = requests.get('https://api.github.com/search/issues?q=repo:'+repo+'&is:pr&is:open&page='+str(x)+'&per_page=100', headers={'Authorization': 'Bearer '+TOKEN})
            data = urls.json()
            with open('repo_final_mined_data//prs//'+repo_name+'_issues_pageNumber_'+str(x)+'.json', 'w') as f:
                json.dump(data, f)
            pprint('prs gathered for repo: ' + repo)
    merged_prs_content = []
    for f in glob.glob('repo_final_mined_data//prs//*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_prs_content.append(a_dict)
    with open('repo_final_mined_data//mined_repo_prs_content.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_prs_content, file_out)
    with open('repo_final_mined_data//mined_repo_prs_content.json') as file_in:
        data = json.load(file_in)
    df_prs = pd.json_normalize(data)
    df_prs_content = pd.json_normalize(df_prs['items'].explode())
    df_prs_content.to_csv('repo_final_mined_data//curated_csv//prs_content_final.csv')

def merge_json_files():
    # For repo basic info
    merged_contents_basic_info = []
    for f in glob.glob('repo_demographic_mined_data/*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_basic_info.append(a_dict)
    with open('repo_final_mined_data//mined_repo_data_basic.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_basic_info, file_out)
    
    # For repo in depth details
    merged_contents_in_depth_details = []
    for f in glob.glob('repo_demographic_mined_data/more_data/in_depth_details/*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_in_depth_details.append(a_dict)
    with open('repo_final_mined_data//mined_repo_in_depth_details.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_in_depth_details, file_out)

    # For repo issues count
    merged_contents_issues_count_opened = []
    for f in glob.glob('repo_demographic_mined_data/more_data/issue_count/*_open_*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_issues_count_opened.append(a_dict)
    with open('repo_final_mined_data//mined_repo_issues_count_opened.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_issues_count_opened, file_out)
    merged_contents_issues_count_closed = []
    for f in glob.glob('repo_demographic_mined_data/more_data/issue_count/*_closed_*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_issues_count_closed.append(a_dict)
    with open('repo_final_mined_data//mined_repo_issues_count_closed.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_issues_count_closed, file_out)

    # For repo prs count
    merged_contents_prs_count_opened = []
    for f in glob.glob('repo_demographic_mined_data/more_data/pr_count/*_open_*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_prs_count_opened.append(a_dict)
    with open('repo_final_mined_data//mined_repo_prs_count_opened.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_prs_count_opened, file_out)
    merged_contents_prs_count_closed = []
    for f in glob.glob('repo_demographic_mined_data/more_data/pr_count/*_closed_*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_prs_count_closed.append(a_dict)
    with open('repo_final_mined_data//mined_repo_prs_count_closed.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_prs_count_closed, file_out)
    
    # For repo commits count
    merged_contents_commits_count = []
    for f in glob.glob('repo_demographic_mined_data/more_data/commit_count/*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_commits_count.append(a_dict)
    with open('repo_final_mined_data//mined_repo_commits_count.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_commits_count, file_out)

    # For repo contributors count
    merged_contents_contributors_count = []
    for f in glob.glob('repo_demographic_mined_data/more_data/contributor_count/*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_contributors_count.append(a_dict)
    with open('repo_final_mined_data//mined_repo_contributors_count.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_contributors_count, file_out)

    
def mine_git_twin_repositories():
    # Loading categorized csv file as dataframe
    #df = pd.read_csv('repo_final_mined_data//curated_csv//original.csv')
    #repos_name = list(df['full_name'])
    for repo in repos_name:
        language = df.query("full_name=='"+repo+"'")["language"].iloc[0]
        created_year = df.query("full_name=='"+repo+"'")["created_at"].str.slice(0,4).iloc[0]
        pushed_year = df.query("full_name=='"+repo+"'")["pushed_at"].str.slice(0,4).iloc[0]
        time.sleep(10)ßß
        repo_name = repo.replace('/','-')
        urls = requests.get('https://api.github.com/search/repositories?q=created:'+created_year+'+pushed:'+pushed_year+'+language:'+language+'&is:public&per_page=100', headers={'Authorization': 'Bearer '+TOKEN})
        data = urls.json()
        print(urls.url)
        os.mkdir('repo_final_mined_data//twin_repositories//'+repo_name)
        with open('repo_final_mined_data//twin_repositories//'+repo_name+'//'+repo_name+'_twins_json.json', 'w') as f:
            json.dump(data, f)
        with open('repo_final_mined_data//twin_repositories//'+repo_name+'//'+repo_name+'_twins_json.json') as file_in:
            data = json.load(file_in)
            df_twin_repositories = pd.json_normalize(data)
            df_twin_repositories_repos = df_twin_repositories.explode('items')
            df_twin_repositories_repos = pd.json_normalize(df_twin_repositories_repos['items'])
            df_twin_repositories_repos.to_csv('repo_final_mined_data//twin_repositories//'+repo_name+'//'+repo_name+'_twins_csv.csv')
        pprint('twin gathered for repo: ' + repo) 
        
    
    # Merge twin.csv files for all repos
    data_dir = 'repo_final_mined_data//twin_repositories//'
    out_dir = 'repo_final_mined_data//raw_csv//'
    list_files = []
    for filename in pathlib.Path(data_dir).glob('**/twin.csv'):
        list_files.append(filename)
    df = pd.concat(map(pd.read_csv, list_files), ignore_index=True)
    df.to_csv(pathlib.Path(out_dir) / 'combined_twins.csv', index=False)
     
    # For twins, mine even more details
    df_git_repo = pd.read_csv('repo_final_mined_data//raw_csv//combined_twins.csv')
    twin_repos_full_name_list = df_git_repo['full_name'].to_list()
    for repo in twin_repos_full_name_list:
        time.sleep(10)
        repo_name = repo.replace('/','-')
        # More details than mine_git_repos_demographic_basic_data() per repo - needed for extracting the watchers count (subscribers_count) of a repo 
        try:
            details_url = f'https://api.github.com/repos/{repo}'
            pprint('Details gathered for repo: ' + repo)
            urls_details = requests.get(details_url, headers={'Authorization': 'Bearer '+TOKEN})
            data = urls_details.json()
            with open('repo_demographic_mined_data//more_data//in_depth_details//twins//details_for_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
        except ValueError:  # includes simplejson.decoder.JSONDecodeError
            print('Decoding JSON has failed')
        # Commits total number per repo, code from: https://gist.github.com/0penBrain/7be59a48aba778c955d992aa69e524c5
        commit_url = f'https://api.github.com/repos/{repo}/commits?per_page=1'
        pprint('commits count gathered for repo: ' + repo)
        urls_commits = requests.get(commit_url, headers={'Authorization': 'Bearer '+TOKEN})
        data = urls_commits.json()[0]
        data['repository'] = repo
        data['number'] = re.search('\d+$', urls_commits.links['last']['url']).group()
        with open('repo_demographic_mined_data//more_data//commit_count//twins//commits_number_for_repo_'+repo_name+'.json', 'w') as f:
            json.dump(data, f)
        
        # Contributors total number per repo, code adapted from above
        try:
            contributor_url = f'https://api.github.com/repos/{repo}/contributors?per_page=1'
            pprint('contributors count gathered for repo: ' + repo)
            urls_contributors = requests.get(contributor_url, headers={'Authorization': 'Bearer '+TOKEN})
            data = urls_contributors.json()[0]
            data['repository'] = repo
            data['number'] = re.search('\d+$', urls_contributors.links['last']['url']).group()
            with open('repo_demographic_mined_data//more_data//contributor_count//twins//commits_number_for_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
        except KeyError:
            data={'repository': repo, 'number': 0}
            with open('repo_demographic_mined_data//more_data//contributor_count//twins//commits_number_for_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
            pass
        except IndexError:
            data={'repository': repo, 'number': 0}
            with open('repo_demographic_mined_data//more_data//contributor_count//twins//commits_number_for_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
            pass        

        # Opened and closed issues count per repo
        for issue_status in ('open','closed'):
            time.sleep(10)
            issue_url = f'https://api.github.com/search/issues?q=repo:{repo}%20is:issue%20is:{issue_status}&per_page=1'
            pprint(issue_status + ' issues count gathered for repo: ' + repo)
            urls_issues = requests.get(issue_url, headers={'Authorization': 'Bearer '+TOKEN})
            data = urls_issues.json()
            with open('repo_demographic_mined_data//more_data//issue_count//twins//issue_type_'+issue_status+'_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
    
        # Opened and closed pull requests count per repo
        for pr_status in ('open','closed'): 
            time.sleep(10)
            prs_url = f'https://api.github.com/search/issues?q=repo:{repo}%20is:pr%20is:{pr_status}&per_page=1'
            pprint(pr_status + ' pull requests count gathered for repo: ' + repo)
            urls_prs = requests.get(prs_url, headers={'Authorization': 'Bearer '+TOKEN})
            data = urls_prs.json()
            with open('repo_demographic_mined_data//more_data//pr_count//twins//pr_type_'+pr_status+'_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
     
    # For repo in depth details
    merged_contents_in_depth_details = []
    for f in glob.glob('repo_demographic_mined_data/more_data/in_depth_details/twins/*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_in_depth_details.append(a_dict)
    with open('repo_final_mined_data//mined_repo_in_depth_details_twins.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_in_depth_details, file_out)

    # For repo issues count
    merged_contents_issues_count_opened = []
    for f in glob.glob('repo_demographic_mined_data/more_data/issue_count/twins/*_open_*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_issues_count_opened.append(a_dict)
    with open('repo_final_mined_data//mined_repo_issues_count_opened_twins.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_issues_count_opened, file_out)
    merged_contents_issues_count_closed = []
    for f in glob.glob('repo_demographic_mined_data/more_data/issue_count/twins/*_closed_*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_issues_count_closed.append(a_dict)
    with open('repo_final_mined_data//mined_repo_issues_count_closed_twins.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_issues_count_closed, file_out)

    # For repo prs count
    merged_contents_prs_count_opened = []
    for f in glob.glob('repo_demographic_mined_data/more_data/pr_count/twins/*_open_*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_prs_count_opened.append(a_dict)
    with open('repo_final_mined_data//mined_repo_prs_count_opened_twins.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_prs_count_opened, file_out)
    merged_contents_prs_count_closed = []
    for f in glob.glob('repo_demographic_mined_data/more_data/pr_count/twins/*_closed_*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_prs_count_closed.append(a_dict)
    with open('repo_final_mined_data//mined_repo_prs_count_closed_twins.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_prs_count_closed, file_out)
    
    # For repo commits count
    merged_contents_commits_count = []
    for f in glob.glob('repo_demographic_mined_data/more_data/commit_count/twins/*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_commits_count.append(a_dict)
    with open('repo_final_mined_data//mined_repo_commits_count_twins.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_commits_count, file_out)

    # For repo contributors count
    merged_contents_contributors_count = []
    for f in glob.glob('repo_demographic_mined_data/more_data/contributor_count/twins/*.json'):
        with open(f, 'r', encoding='utf-8') as file_in:
            for line in file_in:
                a_dict = json.loads(line)
                merged_contents_contributors_count.append(a_dict)
    with open('repo_final_mined_data//mined_repo_contributors_count_twins.json', 'w', encoding='utf-8') as file_out:
        json.dump(merged_contents_contributors_count, file_out)
    
    #########################################
            # SAVE RAW DATAFRAMES #
    #########################################

    # Save as raw dataframe as csv for repos in depth info
    with open('repo_final_mined_data//mined_repo_in_depth_details_twins.json') as file_in:
        data = json.load(file_in)
    df_in_depth_details = pd.json_normalize(data)
    df_in_depth_details.to_csv('repo_final_mined_data//raw_csv//mined_repo_in_depth_details_raw_twins.csv')
    
    # Save as raw dataframes as csvs for issues count
    with open('repo_final_mined_data//mined_repo_issues_count_opened_twins.json') as file_in:
        data = json.load(file_in)
    df_issues_open = pd.json_normalize(data)
    df_issues_open_repo_url = df_issues_open.explode('items')
    df_issues_open_repo_url = pd.json_normalize(df_issues_open_repo_url['items'])
    df_issues_open['repository_url'] = df_issues_open_repo_url['repository_url']
    df_issues_open.to_csv('repo_final_mined_data//raw_csv//mined_repo_issues_count_opened_raw_twins.csv')
    with open('repo_final_mined_data//mined_repo_issues_count_closed_twins.json') as file_in:
        data = json.load(file_in)
    df_issues_close = pd.json_normalize(data)
    df_issues_close_repo_url = df_issues_close.explode('items')
    df_issues_close_repo_url = pd.json_normalize(df_issues_close_repo_url['items'])
    df_issues_close['repository_url'] = df_issues_close_repo_url['repository_url']
    df_issues_close.to_csv('repo_final_mined_data//raw_csv//mined_repo_issues_count_closed_raw_twins.csv')

    # Save as raw dataframes as csvs for prs count
    with open('repo_final_mined_data//mined_repo_prs_count_opened_twins.json') as file_in:
        data = json.load(file_in)
    df_prs_open = pd.json_normalize(data)
    df_prs_open_repo_url = df_prs_open.explode('items')
    df_prs_open_repo_url = pd.json_normalize(df_prs_open_repo_url['items'])
    df_prs_open['repository_url'] = df_prs_open_repo_url['repository_url']
    df_prs_open.to_csv('repo_final_mined_data//raw_csv//mined_repo_prs_count_opened_raw_twins.csv')
    with open('repo_final_mined_data//mined_repo_prs_count_closed_twins.json') as file_in:
        data = json.load(file_in)
    df_prs_close = pd.json_normalize(data)
    df_prs_close_repo_url = df_prs_close.explode('items')
    df_prs_close_repo_url = pd.json_normalize(df_prs_close_repo_url['items'])
    df_prs_close['repository_url'] = df_prs_close_repo_url['repository_url']
    df_prs_close.to_csv('repo_final_mined_data//raw_csv//mined_repo_prs_count_closed_raw_twins.csv')

    # Save as raw dataframes as csvs for commits count
    with open('repo_final_mined_data//mined_repo_commits_count_twins.json') as file_in:
        data = json.load(file_in)
    df_commits = pd.json_normalize(data)
    df_commits.to_csv('repo_final_mined_data//raw_csv//mined_repo_commits_count_raw_twins.csv')
    
    # Save as raw dataframes as csvs for contributors count
    with open('repo_final_mined_data//mined_repo_contributors_count_twins.json') as file_in:
        data = json.load(file_in)
    df_contributors = pd.json_normalize(data)
    df_contributors.to_csv('repo_final_mined_data//raw_csv//mined_repo_contributors_count_raw_twins.csv')

    #########################################
        # SAVE COMBINED RAW DATAFRAMES #
    #########################################
    df_intermediate = pd.read_csv('repo_final_mined_data//raw_csv//combined_twins.csv', sep=',')

    # Create dataframe for mining more data about git repos
    df_git_repo = df_intermediate.filter(['full_name'])
    df_git_repo.drop_duplicates(subset='full_name',keep='first')

    # Create a dataframe only with issues (opened+closed) number, exported as csv
    # total_count_x = number of issues opened, total_count_y = number of issues closed
    df_issues = df_issues_open.merge(df_issues_close, on=['repository_url'], how='inner', suffixes= ('_issues_opened','_issues_closed'))
    df_issues['open_issues_only']=df_issues['total_count_issues_opened']
    df_issues['closed_issues_only']=df_issues['total_count_issues_closed']
    df_issues.to_csv('repo_final_mined_data//raw_uncurated_csv//issues_count_raw_twins.csv')

    # Create a dataframe only with prs (opened+closed) number, exported as csv
    # total_count_x = number of prs opened, total_count_y = number of prs closed
    df_prs = df_prs_open.merge(df_prs_close, on=['repository_url'], how='inner', suffixes =('_prs_opened','_prs_closed'))
    df_prs['open_pull_requests']=df_prs['total_count_prs_opened']
    df_prs['closed_pull_requests']=df_prs['total_count_prs_closed']
    df_prs.to_csv('repo_final_mined_data//raw_uncurated_csv//prs_count_raw_twins.csv')
 
    # Create a dataframe only with contributors and commits number, exported as csv
    # number_x = number of commits, number_y = number of contributors
    df_cc = df_commits.merge(df_contributors, on=['repository'], how='inner', suffixes = ('_commits','_contributors'))
    df_cc['repository_url'] = 'https://api.github.com/repos/'+df_cc['repository']
    df_cc['commits_number']=df_cc['number_commits']
    df_cc['contributors_number']=df_cc['number_contributors']
    df_cc.to_csv('repo_final_mined_data//raw_uncurated_csv//contributors_and_commits_count_raw_twins.csv')

    

    #########################################
           # SAVE FINAL DATAFRAMES #
    #########################################

    # Create a final dataframe for issues, prs, commits and contributors
    df_in_depth_selected_details = df_in_depth_details.filter(['url','subscribers_count'])
    df_in_depth_selected_details.drop_duplicates(subset='url',keep='first')
    df_in_depth_selected_details.to_csv('repo_final_mined_data//curated_csv//in_depth_selected_details_twins.csv')
    df_issues_final = df_issues.filter(['repository_url','open_issues_only','closed_issues_only'])
    df_issues_final.drop_duplicates(subset='repository_url',keep='first')
    df_issues_final.to_csv('repo_final_mined_data//curated_csv//issues_final_twins.csv')
    df_prs_final = df_prs.filter(['repository_url','open_pull_requests','closed_pull_requests'])
    df_prs_final.drop_duplicates(subset='repository_url',keep='first')
    df_prs_final.to_csv('repo_final_mined_data//curated_csv//prs_final_twins.csv')
    df_cc_final = df_cc.filter(['repository_url','commits_number','contributors_number'])
    df_cc_final.drop_duplicates(subset='repository_url',keep='first')
    df_cc_final.to_csv('repo_final_mined_data//curated_csv//cc_final_twins.csv')

    # Final dataframe with all fields mined + duplicated
    df_final_intermediary_raw = df_issues.merge(df_prs.merge(df_cc, on=['repository_url'], how='inner',suffixes=('_PRS','_CC')), on=['repository_url'], how='inner', suffixes = ('_ISSUES','_PRS_CC'))
    df_final_intermediary_raw['url'] = df_final_intermediary_raw['repository_url']

    df_final_raw = df_intermediate.merge(df_final_intermediary_raw.merge(df_in_depth_details, on=['url'], how='left', suffixes=('_INT_RAW','_DPTH_DET')).fillna(0), on=['url'], how='left', suffixes = ('_INTERMEDIATE', '_INT_RAW_DPTH_DET')).fillna(0)
    df_final_raw.to_csv('repo_final_mined_data//raw_uncurated_csv//final_data_uncurated_twins.csv')

    # Final dataframe with only fields of interest mined + without duplicates
    df_final_intermediary_curated = df_issues_final.merge(df_prs_final.merge(df_cc_final, on=['repository_url'], how='inner'), on=['repository_url'], how='inner')
    df_final_intermediary_curated['url'] = df_final_intermediary_curated['repository_url']

    df_final_curated = df_intermediate.merge(df_final_intermediary_curated.merge(df_in_depth_selected_details, on=['url'], how='left').fillna(0), on=['url'], how='left').fillna(0)
    df_final_curated.drop_duplicates(subset='id',keep='first',inplace=True)
    df_final_curated.filter(['html_url','name','full_name','description','topics','created_at','updated_at','pushed_at',
        'size','language','stargazers_count','subscribers_count','has_issues','has_downloads','has_discussions',
        'forks_count','default_branch','private','open_issues_only','closed_issues_only','open_pull_requests','closed_pull_requests','commits_number','contributors_number','license.key','owner.type']).to_csv('repo_final_mined_data//curated_csv//baseline_repos.csv')


def mine_more_data_and_create_dataframes():
    with open('repo_final_mined_data//mined_repo_data_basic.json') as file_in:
        data = json.load(file_in)
    # Load initial JSON file as a dataframe and normalize it
    df_initial = pd.json_normalize(data)
    # Export as csv file only the 'items' column
    df_intermediate = pd.json_normalize(df_initial['items'].explode())
    df_intermediate.to_csv('repo_final_mined_data//raw_csv//mined_repo_data_raw.csv')

    # Create dataframe for mining more data about git repos
    df_git_repo = df_intermediate.filter(['full_name'])
    df_git_repo.drop_duplicates(subset='full_name',keep='first')
    repos_full_name_list = df_git_repo['full_name'].to_list()


    #########################################
         # START MINING MORE_DATA/REPO #
    #########################################    
    for repo in repos_full_name_list:
        time.sleep(10)
        repo_name = repo.replace('/','-')

        # More details than mine_git_repos_demographic_basic_data() per repo - needed for extracting the watchers count (subscribers_count) of a repo 
        try:
            details_url = f'https://api.github.com/repos/{repo}'
            pprint('Details gathered for repo: ' + repo)
            urls_details = requests.get(details_url, headers={'Authorization': 'Bearer '+TOKEN})
            data = urls_details.json()
            with open('repo_demographic_mined_data//more_data//in_depth_details//details_for_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
        except ValueError:  # includes simplejson.decoder.JSONDecodeError
            print('Decoding JSON has failed')
        # Commits total number per repo, code from: https://gist.github.com/0penBrain/7be59a48aba778c955d992aa69e524c5
        commit_url = f'https://api.github.com/repos/{repo}/commits?per_page=1'
        pprint('commits count gathered for repo: ' + repo)
        urls_commits = requests.get(commit_url, headers={'Authorization': 'Bearer '+TOKEN})
        data = urls_commits.json()[0]
        data['repository'] = repo
        data['number'] = re.search('\d+$', urls_commits.links['last']['url']).group()
        with open('repo_demographic_mined_data//more_data//commit_count//commits_number_for_repo_'+repo_name+'.json', 'w') as f:
            json.dump(data, f)
        
        # Contributors total number per repo, code adapted from above
        try:
            contributor_url = f'https://api.github.com/repos/{repo}/contributors?per_page=1'
            pprint('contributors count gathered for repo: ' + repo)
            urls_contributors = requests.get(contributor_url, headers={'Authorization': 'Bearer '+TOKEN})
            data = urls_contributors.json()[0]
            data['repository'] = repo
            data['number'] = re.search('\d+$', urls_contributors.links['last']['url']).group()
            with open('repo_demographic_mined_data//more_data//contributor_count//commits_number_for_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
        except KeyError:
            data={'repository': repo, 'number': 0}
            with open('repo_demographic_mined_data//more_data//contributor_count//commits_number_for_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
            pass
        except IndexError:
            data={'repository': repo, 'number': 0}
            with open('repo_demographic_mined_data//more_data//contributor_count//commits_number_for_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
            pass        

        # Opened and closed issues count per repo
        for issue_status in ('open','closed'):
            time.sleep(10)
            issue_url = f'https://api.github.com/search/issues?q=repo:{repo}%20is:issue%20is:{issue_status}&per_page=1'
            pprint(issue_status + ' issues count gathered for repo: ' + repo)
            urls_issues = requests.get(issue_url, headers={'Authorization': 'Bearer '+TOKEN})
            data = urls_issues.json()
            with open('repo_demographic_mined_data//more_data//issue_count//issue_type_'+issue_status+'_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)
    
        # Opened and closed pull requests count per repo
        for pr_status in ('open','closed'): 
            time.sleep(10)
            prs_url = f'https://api.github.com/search/issues?q=repo:{repo}%20is:pr%20is:{pr_status}&per_page=1'
            pprint(pr_status + ' pull requests count gathered for repo: ' + repo)
            urls_prs = requests.get(prs_url, headers={'Authorization': 'Bearer '+TOKEN})
            data = urls_prs.json()
            with open('repo_demographic_mined_data//more_data//pr_count//pr_type_'+pr_status+'_repo_'+repo_name+'.json', 'w') as f:
                json.dump(data, f)


    #########################################
            # SAVE RAW DATAFRAMES #
    #########################################

    # Save as raw dataframe as csv for repos in depth info
    with open('repo_final_mined_data//mined_repo_in_depth_details.json') as file_in:
        data = json.load(file_in)
    df_in_depth_details = pd.json_normalize(data)
    df_in_depth_details.to_csv('repo_final_mined_data//raw_csv//mined_repo_in_depth_details_raw.csv')
    
    # Save as raw dataframes as csvs for issues count
    with open('repo_final_mined_data//mined_repo_issues_count_opened.json') as file_in:
        data = json.load(file_in)
    df_issues_open = pd.json_normalize(data)
    df_issues_open_repo_url = df_issues_open.explode('items')
    df_issues_open_repo_url = pd.json_normalize(df_issues_open_repo_url['items'])
    df_issues_open['repository_url'] = df_issues_open_repo_url['repository_url']
    df_issues_open.to_csv('repo_final_mined_data//raw_csv//mined_repo_issues_count_opened_raw.csv')
    with open('repo_final_mined_data//mined_repo_issues_count_closed.json') as file_in:
        data = json.load(file_in)
    df_issues_close = pd.json_normalize(data)
    df_issues_close_repo_url = df_issues_close.explode('items')
    df_issues_close_repo_url = pd.json_normalize(df_issues_close_repo_url['items'])
    df_issues_close['repository_url'] = df_issues_close_repo_url['repository_url']
    df_issues_close.to_csv('repo_final_mined_data//raw_csv//mined_repo_issues_count_closed_raw.csv')

    # Save as raw dataframes as csvs for prs count
    with open('repo_final_mined_data//mined_repo_prs_count_opened.json') as file_in:
        data = json.load(file_in)
    df_prs_open = pd.json_normalize(data)
    df_prs_open_repo_url = df_prs_open.explode('items')
    df_prs_open_repo_url = pd.json_normalize(df_prs_open_repo_url['items'])
    df_prs_open['repository_url'] = df_prs_open_repo_url['repository_url']
    df_prs_open.to_csv('repo_final_mined_data//raw_csv//mined_repo_prs_count_opened_raw.csv')
    with open('repo_final_mined_data//mined_repo_prs_count_closed.json') as file_in:
        data = json.load(file_in)
    df_prs_close = pd.json_normalize(data)
    df_prs_close_repo_url = df_prs_close.explode('items')
    df_prs_close_repo_url = pd.json_normalize(df_prs_close_repo_url['items'])
    df_prs_close['repository_url'] = df_prs_close_repo_url['repository_url']
    df_prs_close.to_csv('repo_final_mined_data//raw_csv//mined_repo_prs_count_closed_raw.csv')

    # Save as raw dataframes as csvs for commits count
    with open('repo_final_mined_data//mined_repo_commits_count.json') as file_in:
        data = json.load(file_in)
    df_commits = pd.json_normalize(data)
    df_commits.to_csv('repo_final_mined_data//raw_csv//mined_repo_commits_count_raw.csv')
    
    # Save as raw dataframes as csvs for contributors count
    with open('repo_final_mined_data//mined_repo_contributors_count.json') as file_in:
        data = json.load(file_in)
    df_contributors = pd.json_normalize(data)
    df_contributors.to_csv('repo_final_mined_data//raw_csv//mined_repo_contributors_count_raw.csv')

    #########################################
        # SAVE COMBINED RAW DATAFRAMES #
    #########################################

    # Create a dataframe only with issues (opened+closed) number, exported as csv
    # total_count_x = number of issues opened, total_count_y = number of issues closed
    df_issues = df_issues_open.merge(df_issues_close, on=['repository_url'], how='inner', suffixes= ('_issues_opened','_issues_closed'))
    df_issues['open_issues_only']=df_issues['total_count_issues_opened']
    df_issues['closed_issues_only']=df_issues['total_count_issues_closed']
    df_issues.to_csv('repo_final_mined_data//raw_uncurated_csv//issues_count_raw.csv')

    # Create a dataframe only with prs (opened+closed) number, exported as csv
    # total_count_x = number of prs opened, total_count_y = number of prs closed
    df_prs = df_prs_open.merge(df_prs_close, on=['repository_url'], how='inner', suffixes =('_prs_opened','_prs_closed'))
    df_prs['open_pull_requests']=df_prs['total_count_prs_opened']
    df_prs['closed_pull_requests']=df_prs['total_count_prs_closed']
    df_prs.to_csv('repo_final_mined_data//raw_uncurated_csv//prs_count_raw.csv')
 
    # Create a dataframe only with contributors and commits number, exported as csv
    # number_x = number of commits, number_y = number of contributors
    df_cc = df_commits.merge(df_contributors, on=['repository'], how='inner', suffixes = ('_commits','_contributors'))
    df_cc['repository_url'] = 'https://api.github.com/repos/'+df_cc['repository']
    df_cc['commits_number']=df_cc['number_commits']
    df_cc['contributors_number']=df_cc['number_contributors']
    df_cc.to_csv('repo_final_mined_data//raw_uncurated_csv//contributors_and_commits_count_raw.csv')

    

    #########################################
           # SAVE FINAL DATAFRAMES #
    #########################################

    # Create a final dataframe for issues, prs, commits and contributors
    df_in_depth_selected_details = df_in_depth_details.filter(['url','subscribers_count'])
    df_in_depth_selected_details.drop_duplicates(subset='url',keep='first')
    df_in_depth_selected_details.to_csv('repo_final_mined_data//curated_csv//in_depth_selected_details.csv')
    df_issues_final = df_issues.filter(['repository_url','open_issues_only','closed_issues_only'])
    df_issues_final.drop_duplicates(subset='repository_url',keep='first')
    df_issues_final.to_csv('repo_final_mined_data//curated_csv//issues_final.csv')
    df_prs_final = df_prs.filter(['repository_url','open_pull_requests','closed_pull_requests'])
    df_prs_final.drop_duplicates(subset='repository_url',keep='first')
    df_prs_final.to_csv('repo_final_mined_data//curated_csv//prs_final.csv')
    df_cc_final = df_cc.filter(['repository_url','commits_number','contributors_number'])
    df_cc_final.drop_duplicates(subset='repository_url',keep='first')
    df_cc_final.to_csv('repo_final_mined_data//curated_csv//cc_final.csv')

    # Final dataframe with all fields mined + duplicated
    df_final_intermediary_raw = df_issues.merge(df_prs.merge(df_cc, on=['repository_url'], how='inner',suffixes=('_PRS','_CC')), on=['repository_url'], how='inner', suffixes = ('_ISSUES','_PRS_CC'))
    df_final_intermediary_raw['url'] = df_final_intermediary_raw['repository_url']

    df_final_raw = df_intermediate.merge(df_final_intermediary_raw.merge(df_in_depth_details, on=['url'], how='left', suffixes=('_INT_RAW','_DPTH_DET')).fillna(0), on=['url'], how='left', suffixes = ('_INTERMEDIATE', '_INT_RAW_DPTH_DET')).fillna(0)
    df_final_raw.to_csv('repo_final_mined_data//raw_uncurated_csv//final_data_uncurated.csv')

    # Final dataframe with only fields of interest mined + without duplicates
    df_final_intermediary_curated = df_issues_final.merge(df_prs_final.merge(df_cc_final, on=['repository_url'], how='inner'), on=['repository_url'], how='inner')
    df_final_intermediary_curated['url'] = df_final_intermediary_curated['repository_url']

    df_final_curated = df_intermediate.merge(df_final_intermediary_curated.merge(df_in_depth_selected_details, on=['url'], how='left').fillna(0), on=['url'], how='left').fillna(0)
    df_final_curated.drop_duplicates(subset='id',keep='first',inplace=True)
    df_final_curated.filter(['html_url','name','full_name','description','topics','created_at','updated_at','pushed_at',
        'size','language','stargazers_count','subscribers_count','has_issues','has_downloads','has_discussions',
        'forks_count','default_branch','private','open_issues_only','closed_issues_only','open_pull_requests','closed_pull_requests','commits_number','contributors_number','license.key','owner.type']).to_csv('repo_final_mined_data//curated_csv//final_data_curated.csv')




#mine_git_repos_demographic_basic_data()
#merge_json_files()
#mine_git_repos_programming_languages()
#mine_users_age()
#mine_git_repos_contributors()
#mine_git_issues_content()
#mine_git_prs_content()
#mine_git_twin_repositories()
#mine_more_data_and_create_dataframes()
