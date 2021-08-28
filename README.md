# TLDR Client

Client for [tldr-pages](https://tldr.sh/) that supports [Tldr-pages Client
Specification v1.5
](https://github.com/tldr-pages/tldr/blob/v1.5/CLIENT-SPECIFICATION.md)


## Usage

```
tldr [--config=EXPR] [{--platform=PLATFORM|-p PLATFORM} [...]]
    [{--language=LANGUAGE|-L LANGUAGE} [...]]
    [{--source=SOURCE|-s SOURCE} [...]] COMMAND [SUBCOMMAND [...]]

tldr {--list|-l} [--config=EXPR] [{--platform=PLATFORM|-p PLATFORM} [...]]
    [{--language=LANGUAGE|-L LANGUAGE} [...]]
    [{--source=SOURCE|-s SOURCE} [...]]

tldr {--update|-u} [--config=EXPR] [{--platform=PLATFORM|-p PLATFORM} [...]]
    [{--language=LANGUAGE|-L LANGUAGE} [...]]
    [{--source=SOURCE|-s SOURCE} [...]]

tldr --clear-cache [--config=EXPR] [{--platform=PLATFORM|-p PLATFORM} [...]]
    [{--language=LANGUAGE|-L LANGUAGE} [...]]
    [{--source=SOURCE|-s SOURCE} [...]]

tldr {--config-typecheck|--config-print-type} [--config=EXPR]

tldr {--version|-v}

tldr {--help|-h}
```


## Configuration

Simple default configuration can look like:

```dhall
let Config = ./dhall/Config/package.dhall in Config::{=}
```

More complicated example that can be turned into a library can be found in
[`dhall/config.dhall`](./dhall/config.dhall) file.


## Features and Limitations

- [x] Dhall configuration file.

- [x] Support for multiple sources of pages including custom sources and local
  pages.

- [x] Use index file/DB instead of guessing paths. This should allow us to do
  more advanced search in the future.

- [ ] Support tarballs (`.tar`, `.tar.gz`, `.tar.bz2`, `.tar.xz`, etc.).

- [ ] Support removing subdirectories from a source. For example if the archive
  has a root directory where `pages${locale}` directories are stored.

- [ ] Command line completion.
