# reggae
Reggae is a regex engine built in haskell.
It uses no dependencies (apart from the Haskell standard
library) and applies a simple Recursive Descent parsing
strategy.

## Installation
Install via
```bash
cabal install
```

## Usage
```bash
reggae <regex> <filename>
```
For example, filter IP-addresses out of a webserver
log file
```bash
reggae '([0-9]{1,3}\.){3}[0-9]{1,3}' access.log
```
