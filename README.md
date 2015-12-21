# dictcc.el

An interface to look up translations on [dict.cc](http://dict.cc) without
leaving emacs.

## Installation

You can get it from [melpa](http://melpa.org):

```
M-x package-install dictcc
```

## Commands

The only command this package provides is `dictcc`, which prompts you for a
query and lets you pick a translation through a helm interface.

![Showcasing dictcc](http://i.imgur.com/2GwXnCw.png)

## Customization

- `dictcc-source-lang`: First language to look up words in (Default `"en"`)
- `dictcc-destination-lang`: Second language to look up words in (Default `"de"`)

Despite the names source and destination, [dictcc](http://dict.cc) actually just looks up queries in both languages at once.
This means that it does not matter if you specify the attributes as `"de"` and `"fr"` or `"fr"` and `"de"`.
