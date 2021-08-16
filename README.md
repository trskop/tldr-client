# TLDR Client

Client for [tldr-pages](https://tldr.sh/) that supports [Tldr-pages Client
Specification v1.5
](https://github.com/tldr-pages/tldr/blob/v1.5/CLIENT-SPECIFICATION.md)


## Usage

```
tldr [--config=FILE] [-p PLATFORM|--platform=PLATFORM]
    [-L LANGUAGE|--language=LANGUAGE] [--source=SOURCE [...]]
    {-l|--list|COMMAND [...]}
tldr {-u|--update} [--config=FILE] [--source=SOURCE [...]]
tldr [--config=FILE] {--config-typecheck|--config-print-type}
tldr {-v|--version}
tldr {-h|--help}
```


## Configuration

Simple default configuration can look like:

```dhall
let Config = ./dhall/Config/package.dhall in Config::{=}
```

More complicated example that can be turned into a library can be found in
[`config.dhall`](./config.dhall) file.


## Features and Limitations

[] Dhall configuration file.

[] Support for multiple sources of pages including custom sources and local
    pages.

[ ] Use index file/DB instead of guessing paths. This should allow us to do
    more advanced search.
