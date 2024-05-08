# command-wrapper-tldr

> Client for [tldr pages](https://tldr.sh/), a collection of simplified
> and community-driven man pages.

- Show tldr page for a specific command:

`TOOLSET_COMMAND tldr {{command}}`

- Show tldr page for a specific command and its subcommand:

`TOOLSET_COMMAND tldr {{command}} {{subcommand}}`

- Alternative way to show a tldr page for a specific command and its subcommand (same effect as the above example):

`TOOLSET_COMMAND tldr {{command}}-{{subcommand}}`

- [u]pdate offline cache:

`TOOLSET_COMMAND tldr {{-u|--update}}`

- [l]ist available tldr pages:

`TOOLSET_COMMAND tldr {{-l|--list}}`

- Render a CommonMark file or a content passed to standard input as tldr page:

`TOOLSET_COMMAND tldr {{--render-stdin|--render-file=path/to/a/page.md}}`
