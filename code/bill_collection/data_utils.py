import requests
import os
import json


# function to get all the bills from a state based on a query
def get_bills(api_key, state, q, page=1, per_page=20):
    base_api_link = "https://v3.openstates.org"
    """Returns json of bills from a state based on a query 
    and pagr number"""
    # url
    url = f"{base_api_link}/bills?jurisdiction={state}&sort=updated_desc&q={q}&page={page}&per_page={per_page}&apikey={api_key}"

    # get the bills from the first page
    page = requests.get(url)

    # check if API returned a 200 status code
    if page.status_code != 200:
        page.raise_for_status()
        # break

    # convert the response to json
    bills_json = page.json()

    # return the bills
    return bills_json


# save bills to a json file
def save_bill_json(json_data, state, q, page, dir_path):
    file_name = f"bills_{state}_{q}_{page}.json"
    file_path = os.path.join(dir_path, file_name)
    with open(file_path, "w") as f:
        json.dump(json_data, f)
