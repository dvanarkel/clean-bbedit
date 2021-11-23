# Eastwood linter and language server

Eastwood is a linter and language server for the functional programming language [Clean][].

Neovim and VS Code are the currently supported IDEs. Other IDEs will need to be configured manually.

## The project configuration file (Eastwood.yml)

Eastwood requires a project configuration file `Eastwood.yml` in the current working directory.
This is a YAML file with the following fields:

- `compiler`: compiler executable (usually `cocl` or `cocl-itasks`)
- `libraries`: libraries to include, located in the Clean distribution's library directory
- `paths`: additional relative or absolute paths in which included modules are located

An example Eastwood.yml file:

```
compiler: cocl-itasks
libraries:
  - StdEnv
  - Platform
paths:
  - .
```

It is required to specify a compiler.

It is required to specify a YAML list for the libraries key (use "libraries: []" for an empty list).

It is required to specify a YAML list for the paths key (use "paths: []" for an empty list). If the directory in which
`Eastwood.yml` is placed also contains source code, add `.` to paths.

## Usage in Neovim

The configuration below assumes you have neovim 0.5 or greater with lsp-config installed. Simply add the following to
your vimrc:

```lua
lua << EOF
  local lspconfig = require'lspconfig'
  local util = require'lspconfig/util'
  local configs = require'lspconfig/configs'
  if not lspconfig.eastwood then
    configs.eastwood = {
      default_config = {
        cmd = {'eastwood-cls'};
        filetypes = {'clean'};
        root_dir = util.root_pattern 'Eastwood.yml',
        settings = {};
      };
    }
  end
  lspconfig.eastwood.setup{}
EOF
```

## Usage in VS Code

Use the [Eastwood extension for VS Code](https://gitlab.com/top-software/eastwood-vs-code).

## Binaries

Nightly builds are available here:

- [Linux, x64](https://ftp.cs.ru.nl/Clean/builds/linux-x64/clean-eastwood-linux-x64-latest.tgz)
- [Linux, x86](https://ftp.cs.ru.nl/Clean/builds/linux-x86/clean-eastwood-linux-x86-latest.tgz)
- [Linux, ARM64](https://ftp.cs.ru.nl/Clean/builds/linux-arm64/clean-eastwood-linux-arm64-latest.tgz)
- [MacOS, x64](https://ftp.cs.ru.nl/Clean/builds/macos-x64/clean-eastwood-macos-x64-latest.tgz)
- [Windows, x64](https://ftp.cs.ru.nl/Clean/builds/windows-x64/clean-eastwood-windows-x64-latest.tgz)
- [Windows, x86](https://ftp.cs.ru.nl/Clean/builds/windows-x86/clean-eastwood-windows-x86-latest.tgz)

## Building
The sources can be compiled as follows:

```bash
make -C src/linter eastwood-lint
make -C src/languageServer eastwood-cls
```

Remember to initialize the git submodules located within the project before compilation through:
```bash
git submodule init
git submodule update
```
## Running tests

The tests can be run as follows:

```bash
make -C test runTests
```

## Documentation

Further documentation can be found [here](https://gitlab.com/top-software/eastwood/-/tree/main/docs).

## Copyright &amp; License

Eastwood is copyright &copy; 2021 TOP Software Technology B.V.

This software is licensed under the GPL 3.0 or later license. For details, see the
[LICENSE](/LICENSE) file.

[Clean]: http://clean.cs.ru.nl/
