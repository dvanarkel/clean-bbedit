# clean-bbedit
Clean support in BBEdit

## syntax colouring

Initially use the non-coded language support in BBEdit.

From BBEdit, select menu 'BBEdit | Folders | Language Modules' in order to open the target location in the Finder.
Drop in the clean-language.plist file and restart BBEdit

## language server protocol

From BBEdit, select menu 'BBEdit | Folders | Language Servers' in order to open the target location in the Finder.
Drop in the eastwood-cls file.
In BBEdit Preferences, choose Languages pane under Language-specific settings, add Clean language, and in Server tab
'Enable language server', Command 'eastwood-cls', Language ID 'clean'.

## eastwood notes

This is a modified copy of eastwood-cls as included in recent Clean distributions.
Eastwood requires an Eastwood.yaml file in your project folder describing project related info.

For example from the eastwood project itself:

```yaml
compiler: cocl
libraries:
  - StdEnv
  - Platform
  - Gast
paths:
  - src
  - src/languageServer
  - src/linter
  - src/lib
  - src/lib/yaml/src
  - src/lib/clean-lsp/src
  - test
  - test/suite-config-empty-paths
  - test/suite-config-missing-paths
  - test/suite-config-missing-stdenv
  - test/suite-config-no-paths-key
  - test/suite-config-non-existing-paths
  - test/suite-default
  - test/suite-without-config
```

This needs to be somewhere between root and your source file directory in order to be found.
Note that BBEdit currently appears to initialise the language server only once when starting up, so if multiple clean files are open a random one is used to initialise the language server. 

## Sources

Eastwood is currently hosted at: https://gitlab.com/top-software/eastwood

The modified branch is hosted here under the 'eastwood' branch.
