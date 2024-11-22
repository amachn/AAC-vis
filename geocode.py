from csv import QUOTE_NONNUMERIC
from dotenv import load_dotenv
from os import getenv
from requests import get
from time import sleep
import pandas as pd
import re


class Geocoder:
    key: str | None
    raw_df: pd.DataFrame
    out_df: pd.DataFrame

    def __init__(self) -> None:
        load_dotenv()
        self.key = getenv("GEOCODE_API_KEY")

        self.raw_df = pd.read_csv("dat/raw_addrs.csv")
        self.out_df = pd.read_csv("dat/geocoded_addrs.csv")

    def _geocode(self, address: str) -> tuple[int, int] | None:
        req = get(f"https://geocode.maps.co/search?q={address}&api_key={self.key}")

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

    while True:
        count = input("How many addresses should be geocoded (max: 5000/day)? ")

        try:
            if int(count) < 1 or int(count) > 5000:
                raise ValueError("Number not in range.")
        except ValueError:
            print("Please enter a number between 1 and 5000.")
        else:
            count = int(count)
            break

    geocoder.run(count)
