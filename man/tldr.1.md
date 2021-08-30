% TLDR-CLIENT(1) TLDR Pages Client | TLDR Pages Client
% Peter Trsko
% 30th August 2021

# NAME

`tldr` - Client for [tldr pages](https://tldr.sh/), a collection of simplified
and community-driven man pages.


# USAGE

tldr
\[\--config=*EXPR*]
\[{\--language=*LANGUAGE*|-L *LANGUAGE*} \[...]]
\[{\--platform=*PLATFORM*|-p *PLATFORM*} \[...]]
\[{\--source=*SOURCE*|-s *SOURCE*} \[...]]
*COMMAND* \[*SUBCOMMAND* \[...]\]

tldr {\--list|-l}
\[\--config=*EXPR*]
\[{\--language=*LANGUAGE*|-L *LANGUAGE*} \[...]]
\[{\--platform=*PLATFORM*|-p *PLATFORM*} \[...]]
\[{\--source=*SOURCE*|-s *SOURCE*} \[...]]

tldr {\--update|-u}
\[\--config=*EXPR*]
\[{\--language=*LANGUAGE*|-L *LANGUAGE*} \[...]]
\[{\--platform=*PLATFORM*|-p *PLATFORM*} \[...]]
\[{\--source=*SOURCE*|-s *SOURCE*} \[...]]

tldr \--clear-cache
\[\--config=*EXPR*]
\[{\--language=*LANGUAGE*|-L *LANGUAGE*} \[...]]
\[{\--platform=*PLATFORM*|-p *PLATFORM*} \[...]]
\[{\--source=*SOURCE*|-s *SOURCE*} \[...]]

tldr {\--config-print-type|\--config-typecheck}
\[\--config=*EXPR*]

tldr {\--version|-v}

tldr {\--help|-h}


# DESCRIPTION

[Tldr pages](https://tldr.sh/) is a collection of simplified and
community-driven man pages, for commonly used command-line tools.

There are many clients for [tldr pages](https://tldr.sh/), this specific one is
highly configurable (see *CONFIGURATION* section) and supports custom pages.

This client supports [tldr-pages client specification version 1.5
](https://github.com/tldr-pages/tldr/blob/v1.5/CLIENT-SPECIFICATION.md).

See *EXAMPLES* section, below, to see few basic usage examples.


# EXAMPLES

*   Show tldr page for a specific command:

    ```bash
    tldr COMMAND
    ```

    More specific example:

    ```bash
    tldr tar
    ```

*   Show tldr page for a specific *COMMAND* and its *SUBCOMMAND* (both variants
    have the same effect):

    ```bash
    tldr COMMAND SUBCOMMAND
    tldr COMMAND-SUBCOMMAND
    ```

    More specific example:

    ```
    tldr git commit
    tldr git-commit
    ```

*   Update offline cache:

    ```bash
    tldr --update
    tldr -u
    ```

*   List available tldr pages:

    ```bash
    tldr --list
    tldr -l
    ```


# OPTIONS

COMMAND \[...]
:   Show pages for *COMMAND*. Multiple *COMMAND*s can be specified, in which
    case they are treated as one command with dashes in between.

    For example, both of the commands bellow have the same effect:

    ```bash
    tldr git commit
    tldr git-commit
    ```

\--list, -l
:   Lists all the pages for the current platform to the standard output; if
    `--platform=all` is specified then all pages in all platforms are listed.

    What is listed can be further restricted by using `--language=`*LANGUAGE*,
    `--source=`*SOURCE*, and aforementioned `--platform=`*PLATFORM* option.

\--update, -u
:   Updates the offline cache of pages; if `--sources=`*SOURCE* is specified
    only cache for those page *SOURCE*s is updated.

    Individual *SOURCE*s are specified in the configuration, see
    *CONFIGURATION* section.

\--clear-cache
:   Clear offline cache; by default the whole cache is purged, but if
    `--source=`*SOURCE*, `--platform=`*PLATFORM*, or `--language=`*LANGUAGE*
    are specified then they limit what parts of the cache are removed.

    Individual *SOURCE*s are specified in the configuration, see
    *CONFIGURATION* section.

\--config=*EXPR*
:   Set configuration to *EXPR*, where *EXPR* is a [Dhall](https://dhall-lang.org/)
    expression; if application fails to parse or typecheck the *EXPR* it
    terminates with exit code 1.

    This option overrides user's configuration file (see *FILES* section) and
    `TLDR_CONFIG=`*EXPR* environment variable (see *ENVIRONMENT VARIABLES*
    section). In other words, the `--config=`*EXPR* has the highest priority of
    all the ways how configuration can be passed to the application.

\--language=*LANGUAGE*, -l *LANGUAGE*
:   Search/list pages written in *LANGUAGE*. Overrides default language
    detection mechanism, see *ENVIRONMENT VARIABLES* section for details on how
    language is detected.

    The *LANGUAGE* value can be for example `en` or `zh_TW`. In general it is
    in the following format:

    ```
    <language>[_<country>]
    ```

    Where:

    *   `<language>` — a two-letter ISO 639-2 language code. Complete list can
        be found on [Wikipedia — List of ISO 639-2 codes
        ](https://en.wikipedia.org/wiki/List_of_ISO_639-2_codes).

    *   `<country>` (optional) — a two-letter ISO 3166-1 country code. Complete
        list can be found on [Wikipedia — ISO 3166-1 alpha-2
        ](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2).

    When `--language=`*LANGUAGE* is not specified then `LANG` and `LANGUAGE`
    environment variables are used to derive language(s) to use. If that
    mechanism fails to yield a result then the application defaults to `en`,
    which is considered to be "the default language".

\--platform=*PLATFORM*, -p *PLATFORM*
:   Search or list pages for specified *PLATFORM*. If not option is omitted
    then the platform the application is running on is used as a default.

    List of recognised *PLATFORM*s is flexible. What it means is that if a
    *SOURCE* defines a platform that is not currently supported by official
    [tldr pages](https://tldr.sh/) then this application will handle it.

    There are few special *PLATFORM*s that will always be supported:

    *   `all` — not actually a platform, it just means that there will be no
        platform restriction applied when performing an operation.
    *   `common` — universal architecture, i.e. pages for this platform are
        considered valid for any *PLATFORM* that has been specified.

    In addition to the above [tldr pages](https://tldr.sh/) currently recognise
    following platforms:

    *   `android`
    *   `linux`
    *   `osx` (also can be specified as `--platform=macos`, but pages will
        always use `osx` as the platform name)
    *   `sunos`
    *   `windows`

\--source=*SOURCE*, -s *SOURCE*
:   Show, list, or update cache only for specified *SOURCE*s; by default all
    sources are used; this option can be used multiple times to specify
    multiple *SOURCE*s.

    Individual *SOURCE*s are specified in the configuration, see
    *CONFIGURATION* section.

\--config-print-type
:   Print Dhall type of configuration accepted by the application.

    Configuration is a Dhall *EXPR*ession passed via `--config=`*EXPR*, or
    `TLDR_CONFIG=`*EXPR* environment variable (see *ENVIRONMENT VARIABLES*
    section), or if none of those is passed then it will type check user's
    configuration file (see *FILES* section for details).

\--config-typecheck
:   Type check the configuration and exit; exit code 0 is used on success and
    exit code 1 on failure to typecheck.

    Configuration is a Dhall *EXPR*ession passed via `--config=`*EXPR*, or
    `TLDR_CONFIG=`*EXPR* environment variable (see *ENVIRONMENT VARIABLES*
    section), or if none of those is passed then it will type check user's
    configuration file (see *FILES* section for details).

\--version, -v
:   Print version information to standard output and terminate with exit code
    0.

\--help, -h
:   Print help information to standard output and terminate with exit code 0.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/tldr/config.dhall`
:   User configuration file containing [Dhall](https://dhall-lang.org/)
    expression.

    Can be overridden by `TLDR_CONFIG=`*EXPR* environment variable or
    `--config=`*EXPR* option, see *CONFIGURATION* section for more details.


# ENVIRONMENT VARIABLES

`TLDR_CONFIG=`*EXPR*
:   Dhall *EXPR*ession to be used as a configuration. For more information
    about the *EXPR*ession see *CONFIGURATION* section.

    This environment variable has higher priority than user's configuration
    file (see *FILES* section) and lower than `--config=`*EXPR* command line
    option (see *OPTIONS* section). See also *CONFIGURATION* for full
    configuration resolution algorithm.

`LANG`, `LANGUAGE`
:   These environment variables are used to figure out the default language and
    alternative languages to use. The algorithm how this is done is documented
    in [tldr-pages client specification version 1.5
    ](https://github.com/tldr-pages/tldr/blob/v1.5/CLIENT-SPECIFICATION.md).

`NO_COLOR`
:   This environment variable is an informal standard which is available
    online at [no-color.org](https://no-color.org). The standard states:

    > Accepting the futility of trying to reverse this trend, an informal
    > standard is hereby proposed:
    >
    > All command-line software which outputs text with ANSI color added
    > should check for the presence of a `NO_COLOR` environment variable
    > that, when present (regardless of its value), prevents the addition of
    > ANSI color.

    To see how this affects output of this command just try (notice that the
    environment variable is defined and has empty value, but any value will
    do):

    ```bash
    NO_COLOR= tldr --help
    ```

    On shells like `fish` you may need to use `env`:

    ```bash
    env NO_COLOR= tldr --help
    ```

    Following can be used to temporarily disable `NO_COLOR`:

    ```bash
    env -u 'NO_COLOR' tldr --help
    ```


# CONFIGURATION

Application takes configuration Dhall expression from only one source by
following following resolution algorithm:

1. If `--config=`*EXPR* is specified then Dhall *EXPR*ession is used as
   configuration. See *OPTIONS* section for more details.

   Try the next step if the command line option is not specified.

2. If `TLDR_CONFIG=`*EXPR* is defined then Dhall *EXPR*ession is used as
   configuration. See *ENVIRONMENT VARIABLES* section for more details.

   Try the next step if the environment variable is not defined.

3. If `${XDG_CONFIG_HOME:-$HOME/.config}/tldr/config.dhall` (user's
   configuration file) exists then it is used as configuration. See *FILES*
   section for more details.

   Try the next step if the user's configuration file doesn't exist.

4. Use default configuration that defines source for only official tldr pages.

TODO


# SEE ALSO

* [tldr pages](https://tldr.sh/)
* [Dhall configuration language](https://dhall-lang.org/)
* [tldr-pages client specification version 1.5
  ](https://github.com/tldr-pages/tldr/blob/v1.5/CLIENT-SPECIFICATION.md)
* [no-color.org](https://no-color.org)

# BUGS

<https://github.com/trskop/tldr-client/issues>
