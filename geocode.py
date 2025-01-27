"""
Forward Geocoding Script

This script is utilized to convert addresses into mappable lat/lon
coordinates. Only .csv files are supported for input and must contain
at least an 'address' column. The output will be saved to a new .csv
file.

This file is best run as a standalone script, but the following can be
imported and utilized otherwise:

    * Endpoint - dataclass for storing API endpoint information
    * Geocoder - handles API communication between geo-endpoints
"""

from dataclasses import dataclass
from os import getenv, system, name
from pathlib import Path
# TODO: reimpl. all print stmts. w/ logging

from dotenv import load_dotenv
from requests import get


@dataclass
class Endpoint:
    """
    Dataclass utilized for storing information pertinent to specific endpoints

    Attributes
    ----------
    name : str
        name of the endpoint
    url : str
        API endpoint URL
    key_query : str
        query parameter for the API key
    key_var : str
        environment variable name for the API key
    key : str, optional
        API key for the endpoint
    queries : int
        number of queries allowed per day (default = 1000)
    delay : float
        delay between queries in seconds (default = 1.5)
    """

    name: str
    url: str
    key_query: str
    key_var: str
    key: str | None = None
    queries: int = 1000
    delay: float = 1.5


class Geocoder:
    """
    Class which handles API communication for forward geocoding across
    multiple potential endpoints

    Attributes
    ----------

    Methods
    -------

    """


class _App:
    """
    Main application class for running the geocoding process and taking
    input that modifies the process

    Attributes
    ----------
    endpoints : dict[str, Endpoint]
        dictionary containing all available endpoints
    selected : str
        currently selected endpoint
    in_fn : str
        input file name
    out_fn : str
        output file name
    valid_key : bool
        flag for whether the API key for the current selected endpoint is valid
    """

    endpoints: dict[str, Endpoint]
    selected: str
    in_fn: str
    out_fn: str

    valid_key: bool

    def __init__(self) -> None:
        self.endpoints = self.generate_endpoints()
        self.selected = "maps.co"
        self.in_fn = "dat/raw_addrs.csv"
        self.out_fn = "dat/geocoded_addrs.csv"

    # - internals -

    @staticmethod
    def generate_endpoints() -> dict[str, Endpoint]:
        """
        Generates the available endpoint configs

        Returns
        -------
        dict[str, Endpoint]
            dictionary with all endpoint configs
        """
        return {
            "mapbox": Endpoint(
                "mapbox",
                "https://api.mapbox.com/search/geocode/v6/forward",
                "access_token",
                "MAPBOX_KEY"
            ),
            "maps.co": Endpoint(
                "maps.co",
                "https://geocode.maps.co/search",
                "api_key",
                "MAPS_KEY"
            )
        }

    @staticmethod
    def reset_console() -> None:
        """
        Clears the console and outputs top line text
        """
        system("cls" if name == "nt" else "clear")
        print("geocoder\n--------")

    def check_key(self, endpoint: Endpoint) -> bool:
        """
        Validates the API key for the selected endpoint

        Parameters
        ----------
        endpoint : Endpoint
            endpoint to check the key for

        Returns
        -------
        bool
            flag for whether the key is valid

        """
        def query_key(endpoint: Endpoint) -> bool:
            if endpoint.key is not None:
                req = get(
                    f"{endpoint.url}?{endpoint.key_query}={endpoint.key}&q=Austin+TX", timeout=5
                )
                if req.status_code == 200:
                    return True

            return False

        load_dotenv()
        endpoint.key = getenv(endpoint.key_var)
        self.valid_key = query_key(endpoint)
        return self.valid_key

    def check_file(self, fn: str) -> bool:
        """
        Validates the existence of the passed file name and ensures
        the correct file type

        Parameters
        ----------
        fn : str
            file name to check

        Returns
        -------
        bool
            flag for whether the file is valid
        """
        file = Path(fn)

        if file.is_file() and file.suffix == ".csv":
            return True

        return False


    # - console fetches -

    def fetch_options(self) -> str:
        """
        Writes the available options to the console and fetches the user's selection

        Returns
        -------
        str
            user's selection
        """
        endpoint = self.endpoints[self.selected]
        valid_key = self.check_key(endpoint)

        print("\noptions:")
        print("\t1) run the geocoder")
        print(f"\t2) set the API to use ~ current: {endpoint.name}")
        print(f"\t3) set the amount of queries to send ~ current: {endpoint.queries}")
        print(f"\t4) set the delay between each query ~ current: {endpoint.delay}")
        print(f"\t5) refresh API key variable ~ current: {'valid' if valid_key else 'invalid'}")
        print(f"\t6) set the input file name ~ current: {self.in_fn}")
        print(f"\t7) set the output file name ~ current: {self.out_fn}")
        print("\t8) reset to default settings (includes both APIs)")
        print("\t9) set debug level ~ current: ")
        print("\t0) exit")

        return input("select one: ")

    def fetch_endpoint(self) -> str:
        """
        Writes the available endpoints to the console and fetches the user's selection

        Returns
        -------
        str
            user's endpoint selection
        """
        while True:
            print("\nAPI options:")
            for i, k in enumerate(self.endpoints):
                print(f"\t{i + 1}) {k}{' (current)' if k == self.selected else ''}")
            print("\t0) exit")

            selected = input("select one: ")

            try:
                selected = int(selected)
            except ValueError:
                print("\ninput must be a numeric value, please try again!")
                continue

            if selected == 0:
                break

            if selected in range(1, len(self.endpoints) + 1):
                self.selected = list(self.endpoints.keys())[selected - 1]
                break

            print("\ninvalid option, please try again!")

        return self.selected

    def fetch_queries(self) -> int:
        """
        Fetches the amount of queries to send to the selected endpoint

        Returns
        -------
        int
            selected amount of queries to send
        """
        while True:
            try:
                queries = int(input("\nenter amount of queries to send (max: 10000): "))
            except ValueError:
                print("\ninput must be a numeric value, please try again!")
                continue

            if queries in range(1, 10001):
                self.endpoints[self.selected].queries = queries
                break

            print("\ninvalid amount, please try again!")

        return queries

    def fetch_delay(self) -> float:
        """
        Fetches the delay between queries to send to the selected endpoint

        Returns
        -------
        float
            selected delay between queries
        """
        while True:
            try:
                delay = float(input("\nenter delay between queries (min, max: 0.5s, 15s): "))
            except ValueError:
                print("\ninput must be a numeric value, please try again!")
                continue

            if delay >= 0.5 and delay <= 15:
                self.endpoints[self.selected].delay = delay
                break

            print("\ninvalid delay, please try again!")

        return delay

    # - run process -

    def _start_tasks(self) -> None:
        self.reset_console()

    def _run_checks(self) -> ...:
        if not self.check_file(self.in_fn):
            pass

        if not self.check_file(self.out_fn):
            pass

        if not self.valid_key:
            self.valid_key = self.check_key(self.endpoints[self.selected])
            pass



        # TODO: validate input file is not empty/has entries to geocode
        # TODO: validate selected amount of queries falls within API limit
        # TODO: validate API selected is not at 429 limit
        pass

    def _inner_match(self, option: int) -> str:
        ret = None

        match option:
            case 0:
                ret = 0
            case 1:
                self._run_checks()
                # TODO: implement geocoding init. process
                ret = "\ncompleted geocoder run" # TODO: add completion statistics
            case 2:
                val = self.fetch_endpoint()
                ret = f"\nselected endpoint: {val}"
            case 3:
                val = self.fetch_queries()
                ret = f"\nendpoint queries set to: {val}"
            case 4:
                val = self.fetch_delay()
                ret = f"\nendpoint delay set to: {val}"
            case 5:
                val = self.check_key(self.endpoints[self.selected])
                ret = f"\nAPI key refreshed: {'valid' if val else 'invalid'}"
            case 6:
                self.in_fn = input("\nenter input file name: ")
                ret = f"\ninput file set to: {self.in_fn}"
            case 7:
                self.out_fn = input("\nenter output file name: ")
                ret = f"\noutput file set to: {self.out_fn}"
            case 8:
                self.endpoints = self.generate_endpoints()
                self.in_fn = "dat/raw_addrs.csv"
                self.out_fn = "dat/geocoded_addrs.csv"
                ret = "\nendpoint configs regenerated and files reset."
            case 9:
                pass # TODO: update debug level here
            case _:
                ret = "\ninvalid option, please try again!"

        return ret + "\n"

    def _loop(self) -> None:
        out = ""

        while True:
            self.reset_console()

            print(out, end="")

            try:
                option = int(self.fetch_options())
            except ValueError:
                out = "\ninput must be a numeric value, please try again!\n"
                continue

            out = self._inner_match(option)

            if out == 0:
                break

    def _close_tasks(self) -> None:
        pass # TODO: logging, cleanup, etc.

    def run(self) -> bool:
        """
        Top-level run method, handles all internal modification and
        subsequently running the geocoder

        Returns
        -------
        bool
            flag for whether the geocoder was successfully run
        """
        try:
            self._start_tasks()
            self._loop()
            self._close_tasks()
        except Exception as e:
            # TODO: log error here
            return False

        return True

# run requirements:
# 1. valid API key for utilized geocoding service
#     - store in .env file located at ./
#     - use MAPS_KEY for maps.co, or MAPBOX_KEY for mapbox
# 2. buildDataset.R has to have been run to generate the required .csv files

if __name__ == "__main__":
    _App().run()
