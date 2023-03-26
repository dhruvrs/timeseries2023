# import pandas as pd
# import requests
# from bs4 import BeautifulSoup
# import matplotlib.pyplot as plt

# # URL pattern
# url_pattern = 'https://www.boxofficemojo.com/weekly/by-year/{}/'

# # List to store URLs for each year from 2000 to 2022
# all_urls = []
# for year in range(2000, 2023):
#     url = url_pattern.format(year)
#     all_urls.append(url) 

# # List to store all the DataFrames
# dfs = []

# # Loop through each URL and read the first table using pd.read_html()
# # Append the resulting DataFrame to the dfs list
# for url in all_urls:
#     df = pd.read_html(url, header=0)[0]
#     dfs.append(df)

# # Concatenate all the DataFrames in the dfs list using pd.concat()
# # Set ignore_index=True to reset the index of the concatenated DataFrame
# final_data = pd.concat(dfs, ignore_index=True)
# columns_to_drop = ['Genre', 'Budget', 'Running Time', 'Dates']
# final_data = final_data.drop(columns_to_drop, axis=1)
# print(final_data.head())

# overall_gross = final_data['Overall Gross'].apply(lambda x: str(x).strip())
# final_data['Overall Gross'] = pd.to_numeric(overall_gross, errors='coerce')
# print(final_data.head())
# create a sample DataFrame
import pandas as pd
import numpy as np

df = pd.DataFrame({'A': [10, 15, 20, 25, 30]})

# calculate the percentage change of column A
df['A_pct_change'] = df['A'].pct_change()

# print the DataFrame
print(df)
