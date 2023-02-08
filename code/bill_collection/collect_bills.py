import os
import argparse
from Config import Config
from data_utils import get_bills, save_bill_json
import time
from pathlib import Path
from tqdm.auto import tqdm


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--state", type=str, required=True)
    parser.add_argument("--query", type=str, required=True)
    parser.add_argument("--page", type=int, required=False, default=1)
    parser.add_argument("--per_page", type=int, required=False, default=20)
    args = parser.parse_args()

    start_time = time.time()

    state = args.state
    q = args.query
    page = args.page
    per_page = args.per_page

    # create the directory to save the json files
    state_dir = os.path.join(
        Config.processed_data_dir,
        Config.open_states_subdir,
        Config.bill_jsons_subdir,
        state,
        q,
    )
    print(state_dir)
    Path(state_dir).mkdir(parents=True, exist_ok=True)

    # get the api key
    api_key = Config.API_KEY

    # get the bills from the start page (default is 1)
    bills = get_bills(api_key, state, q, page=page, per_page=per_page)
    max_page = bills["pagination"]["max_page"]
    save_bill_json(
        json_data=bills["results"], state=state, q=q, page=page, dir_path=state_dir
    )

    time.sleep(5)
    # loop over all the pages
    for p_ in tqdm(range(page + 1, max_page + 1)):
        # get the bills from the page
        bills = get_bills(api_key, state, q, page=p_, per_page=per_page)
        # append the bills from the page to the bills dataframe
        save_bill_json(
            json_data=bills["results"], state=state, q=q, page=p_, dir_path=state_dir
        )
        time.sleep(5)

    print("--- %s seconds ---" % (time.time() - start_time))
