# aqua syntax highlighting

Syntax highlighting for `aqua` programming language

## Installation

1. Install [the extension](https://marketplace.visualstudio.com/items?itemName=FluenceLabs.aqua-syntax-highlight)
2. Configure colors for aqua-specific tokens (see below)

Add the following lines to the [settings.json](https://code.visualstudio.com/docs/getstarted/settings) file. Feel free to choose colors according to you favorite theme. In the example below services will be highlighted as green and all the keywords which affect the topology will be highlighted as red

```json
    "editor.tokenColorCustomizations": {
        "textMateRules": [
            {
                "scope": "keyword.topology.aqua",
                "settings": {
                    "foreground": "#FF0000",
                }
            },
            {
                "scope": "support.service.aqua",
                "settings": {
                    "foreground": "#00FF00",
                }
            }
        ]
    }
```



## What is aqua?

Aqua is a new-gen language for distributed systems.

Aqua programs are executed on many peers, sequentially or in parallel, forming a single-use coordination network.

Aqua's runtime is heterogeneous: it includes browsers, servers, devices, all involved in solving a single task. Therefore, Aqua scripts are compiled into several targets at once, with AIR and Typescript as a default.

## Features

Enables syntax highlighting for `aqua` programming language

## Developing

see [vsc-extension-quickstart.md](vsc-extension-quickstart.md)

NOTE: if you're going to change pattern names, check out the naming rules in [TextMate Grammar doc](https://macromates.com/manual/en/language_grammars). You have to use predefined pattern naming scheme, or the syntax won't be highlighted.

## License

[Apache 2.0](LICENSE)