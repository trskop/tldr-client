For latest version of this document see [`ChangeLog.md on GitHub`
](https://github.com/trskop/tldr-client/blob/main/ChangeLog.md).


### HEAD (unreleased changes)

* Build against newer dependencies:

  - LTS 18.6 → 21.25
  - GHC 8.10.4 → 9.4.8 (covered by LTS 21.25)
  - dhall 1.40.1 → 1.41.2 (covered by LTS 21.25)
  - tldr 0.9.1 → 0.9.2
  - Dropped country-codes dependency in favour of local implementation.

* Unit tests for various parts of the client.

* Support [tldr-pages client specification v2.2
  ](https://github.com/tldr-pages/tldr/blob/v2.2/CLIENT-SPECIFICATION.md).

* Handling of locale(s) is now a lot more in line with the tldr-pages
  client specification and handles things like `C.utf8` locale
  correctly.

* Standard output and error output handles are passed explicitly for
  easier testing and debugging.

* Lots of refactoring and code reorganisation in general.


### 0.2.0.0

* Command-line completion.

* Improved help message and documentation.

* Split code base into executable and a library, previously there was no
  library.

* Handle language (translation) preferences in a way that is much closer to
  [tldr-pages client specification v1.5
  ](https://github.com/tldr-pages/tldr/blob/v1.5/CLIENT-SPECIFICATION.md).

* Introduces Command Wrapper subcommand.


### 0.1.0.0

Initial release; not uploaded to Hackage.
