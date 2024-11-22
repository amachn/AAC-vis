from csv import QUOTE_NONNUMERIC
from dotenv import load_dotenv
from os import getenv
from requests import get
from time import sleep
import pandas as pd
import re

# TODO: logging to console each request result
class Geocoder:
    key: str | None
    raw_df: pd.DataFrame
    out_df: pd.DataFrame

    def __init__(self) -> None:
        load_dotenv()
        self.key = getenv("GEOCODE_API_KEY")

        self.raw_df = pd.read_csv("dat/raw_addrs.csv")
        self.out_df = pd.read_csv("dat/geocoded_addrs.csv")

    def _log_run(self, *args) -> ...:
        pass # TODO: log date/time when geocoder was last run with count

    def _geocode(self, address: str) -> tuple[int, int] | int:
        req = get(f"https://geocode.maps.co/search?q={address}&api_key={self.key}")

        if req.status_code == 429:
            return -1

        if len(req) != 0: # successful query for coordinates
            data = req.json()[0]
            return (data['lat'], data['lon'])
        else: # if len is 0, then the request was bad and yielded no coordinates
            return (-1, -1)

    def run(self, count: int) -> None:
        start_idx = int(self.out_df.tail(1)["idx"].values[0]) + 1 
        new_rows = [] 

        for idx in range(start_idx, start_idx + count):
            row = self.raw_df.query("idx == @idx") # find row with matching idx
            base_addr = row["address"].values[0]

            if base_addr != "Outside Jurisdiction" and re.match("\\d", base_addr):
                # regexes to format addr for GET request
                addr = re.sub("[()]", "", base_addr)
                addr = re.sub("\\Win\\W", " ", addr)
                addr = re.sub("\\W", "+", addr)
                coords = self._geocode(addr)
            else:
                coords = (-1, -1)

            if coords == -1: # we've hit the request limit, end geocoding
                print(f"Request limit exceeded! Prematurely terminating script @ idx {idx}")
                break

            vals: list[int | str] = row.values[0].tolist()
            vals.extend(coords)            

            new_rows.append(vals)

            sleep(1.2) # avoid hitting API ratelimit of 1req/s

        df_extension = pd.DataFrame(new_rows, columns=self.out_df.columns)
        df_extension.to_csv("dat/geocoded_addrs.csv", mode='a',
                            index=False, header=False,
                            quoting=QUOTE_NONNUMERIC)


# TO RUN THIS SCRIPT:
# 1. must have a valid API key from geocode.maps.co
#   - API key must also be placed in a .env file located at ./
# 2. buildDataset.R must be run to generate .csv files       

if __name__ == "__main__":
    geocoder = Geocoder()

    count = None
    wait = None
    while True:
        if not isinstance(count, int):
            count = input("How many addresses should be geocoded [default = 1000/day, max = 5000/day]? ")

            try:
                if int(count) < 1 or int(count) > 5000:
                    raise ValueError("Number not in range.")
            except ValueError:
                print("Please enter a number between 1 and 5000.")
                continue
            else:
                count = int(count)

        if not isinstance(wait, int) and not isinstance(wait, float):
            wait = input("How long should the script wait between requests [default=1s]? ")

            try:
                wait = float(wait)
            except ValueError:
                print("Please enter a number only.")
                continue

        break

        

    geocoder.run(count)
