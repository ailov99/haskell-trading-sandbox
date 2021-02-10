# haskell-trading-sandbox
This is a hobby project for playing with Haskell.
Aim is to build a simplistic sandbox for examining the stock market and prototyping faux trading algorithms.

Prerequisites:
- This project currently uses Finnhub to query for market data. Please ensure a system environment variable "FinnhubApiKey" exists with a valid API key as its value

To build and run the project, use the build.py command script with either one of the following options:
* build      - Builds project
* run        - Builds and then runs project
* clean      - Cleans project via stack
* deep-clean - Cleans project via git
* doc        - Cleans previously generated Haddock docs and re-generates
* test       - Runs test suite