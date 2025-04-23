# sizzle
> A simple Haskell library for exploration and learning.

[![Hackage](https://img.shields.io/hackage/v/sizzle.svg)](https://hackage.haskell.org/package/sizzle) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)

## Table of Contents
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
  - [Library](#library)
  - [Web Server Example](#web-server-example)
- [Modules](#modules)
- [Contributing](#contributing)
- [License](#license)

## Features
- Safe file IO helpers (`EZIO`)
- Infinite sequences and series (`Series`)
- Combinatorics, vector operations, and clustering (`Math`)
- Simple HTTP client wrappers (`Network`)
- A quickstart web server demo using Scotty (`app/Main.hs`)

## Installation

### Requirements
- GHC ≥ 8.0
- [Stack](https://docs.haskellstack.org/) or [Cabal](https://www.haskell.org/cabal/)

### Using Stack
```bash
stack update
stack install sizzle
```
Or add to your project’s `package.yaml`:
```yaml
dependencies:
  - sizzle
```

### Using Cabal
```bash
cabal update
cabal install sizzle
```
Or in your `.cabal` file:
```cabal
build-depends: sizzle >=0.2 && <0.3
```

## Usage

### Library

#### Hello Sizzle
```haskell
import Sizzle (helloSizzle)

main :: IO ()
main = helloSizzle
```

#### File IO Helpers (EZIO)
```haskell
import EZIO (readLines, writeLines)

main :: IO ()
main = do
  eLines <- readLines "input.txt"
  case eLines of
    Left err -> putStrLn $ "Error: " ++ err
    Right ls -> do
      print (length ls)
      _ <- writeLines "output.txt" ls
      return ()
```

#### Infinite Series (Series)
```haskell
import Series (fibonacci, primes, evens)

main = do
  print $ take 10 fibonacci     -- [0,1,1,2,3,5,8,13,21,34]
  print $ take 10 primes        -- [2,3,5,7,11,13,17,19,23,29]
  print $ take  5 evens         -- [0,2,4,6,8]
```

#### Math Utilities (Math)
```haskell
import Math (kMeans, initializeSimple)

let points = [(1.0,2.0),(3.0,4.0),(5.0,6.0)]
    initCts = initializeSimple 2 points
    clusters = kMeans initializeSimple 2 points 0.01
in print clusters
```

#### HTTP Client (Network)
```haskell
import Network (httpGet)

main :: IO ()
main = do
  response <- httpGet "https://httpbin.org/get"
  case response of
    Left err  -> putStrLn $ "HTTP Error: " ++ err
    Right body -> putStrLn body
```

### Web Server Example

The `app/Main.hs` file provides a simple Scotty-based demo server on port 3000.

#### Running the server
```bash
stack run sizzle-exe
# or
cabal run sizzle-exe
```

#### Available endpoints
- `GET /`
  Returns a welcome message.
- `GET /random`
  JSON array of 20 random integers (1–100).
- `GET /jsonExample`
  JSON-encoded example string.
- `GET /header`
  Echoes the `User-Agent` header or errors if missing.
- `GET /static/:file`
  Serves the file located at `static/:file`.

Example:
```bash
curl http://localhost:3000/random
```

## Modules

- **EZIO**: Safe file IO (`readLines`, `writeLines`, `tryIO`).
- **Series**: Infinite sequences (`fibonacci`, `primes`, etc.).
- **Math**: Vectors, combinatorics (`powerset`, `permute`), k-means.
- **Network**: HTTP client wrappers (`httpGet`, `httpPost`, `wget`).
- **Sizzle**: Primary module re-exporting all submodules plus `helloSizzle`.

## Contributing

Contributions, issues, and feature requests are welcome!

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/foo`)
3. Commit your changes (`git commit -am 'Add foo'`)
4. Push to the branch (`git push origin feature/foo`)
5. Open a pull request

Please follow the existing style and add tests where appropriate.

## License

This project is licensed under the BSD3 License. See the [LICENSE](LICENSE) file for details.
