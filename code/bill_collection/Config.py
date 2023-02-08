import os
import dotenv

dotenv.load_dotenv()


class Config:
    processed_data_dir = "../../data/processed_data"
    open_states_subdir = "open_states"
    bill_jsons_subdir = "bill_jsons"

    API_KEY = os.getenv("OPENSTATES_API_KEY")
