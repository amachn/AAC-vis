from csv import QUOTE_NONNUMERIC
from datetime import datetime
from dotenv import load_dotenv
from os import getenv
from requests import get
from time import sleep
import logging
import numpy as np
import pandas as pd
import re


class Geocoder:
    key: str | None
    count: int | None
    wait: float | None
    raw_df: pd.DataFrame
    out_df: pd.DataFrame

    def __init__(self) -> None:
        load_dotenv()
        self.key = getenv("GEOCODE_API_KEY")

        self.count = None
        self.wait = None

        self.raw_df = pd.read_csv("dat/raw_addrs.csv")
        self.out_df = pd.read_csv("dat/geocoded_addrs.csv")

    @staticmethod
    def _get_var( 
        prompt: str, 
        default: float, 
        vRange: range | np.ndarray,
        exc_msg: str
    ) -> float:
        var = None

        while not isinstance(var, float) or var not in vRange:
            var = input(prompt)

            try:
                if var in ['', ' ']:
                    var = default
                else:
                    var = float(var)
            except ValueError:
                var = None
                print(exc_msg)
            else:
                print(exc_msg) if var not in vRange else None

        return var

    def _run_startup_tasks(self) -> None:
        self.count = int(
            self._get_var(
                "How many addresses should be geocoded [default = 1000/day, max = 5000/day]? ", 1000,
                range(1, 5001), "Please enter a number between 1 and 5000."
            )
        )

        self.wait = self._get_var(
            "How long should the script wait between requests [default=1.2s, max=30s]? ", 1.2,
            np.linspace(1, 30, num=2901), "Please enter a number between 1 and 30."
        )


    def _check_max(self) -> bool:
        current_date = datetime.today().strftime("%m-%d-%y")

        with open("./run-list.log") as logfile:
            logfile = logfile.readlines()
        
        queries = 0
        for line in logfile:
            if current_date in line:
                queries += int(re.search("\\d{1,}", line).group(0))

        if queries >= 5000:
            return False
        else:
            return True

    def _log_run(self, count: int) -> None:
        logging.basicConfig(
            filename="./run-list.log", filemode='a',
            format="%(asctime)s | %(name)s | %(levelname)s | %(message)s",
            datefmt="%m-%d-%Y", level=logging.INFO
        )
        logging.info(f"Running {count} queries.")   

    def _geocode(self, address: str) -> tuple[float, float] | int:
        req = get(f"https://geocode.maps.co/search?q={address}&api_key={self.key}")

        if req.status_code == 401:
            return -2

        if req.status_code == 429:
            return -1

        if len(req.json()) != 0: # successful query for coordinates
            data = req.json()[0]
            return (
                round(float(data['lat']), 6), 
                round(float(data['lon']), 6)
            )
        else: # if len is 0, then the request was bad and yielded no coordinates
            return (-1, -1)

    def run(self) -> None:
        if not self._check_max():
            print("Daily query limit reached!")
            return

        self._run_startup_tasks()
        self._log_run(self.count)

        start_idx = int(self.out_df.tail(1)["idx"].values[0]) + 1 
        end_idx = start_idx + self.count
        new_rows = [] 

        for idx in range(start_idx, end_idx):
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

            print(f"{(idx)}/{end_idx-1} ({idx-start_idx+1}) | {row["AID"].values[0]} - {coords}")

            if coords == -1: # we've hit the request limit, end geocoding
                print(f"Request limit exceeded! Prematurely terminating script @ idx {idx}")
                break
            elif coords == -2: # missing API key in /.env
                print("No API key found!")
                break

            vals: list[int | str] = row.values[0].tolist()
            vals.extend(coords)            

            new_rows.append(vals)

            sleep(self.wait) # avoid hitting API ratelimit of 1req/s

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
    geocoder.run()
