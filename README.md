# dictcc.el

An interface to look up translations on [dict.cc](http://dict.cc) without
leaving emacs.

## Installation

You can get it from [melpa](http://melpa.org):

```
M-x package-install dictcc
```

## Commands

The main command in this package is `dictcc`, which prompts you for a query and
lets you pick a translation through a completion interface (either ivy or
helm). There is another convenience command `dictcc-at-point` that searches for
the word at point or the content of an active region.

![Showcasing dictcc](http://i.imgur.com/2GwXnCw.png)

## Customization

- `dictcc-source-lang`: First language to look up words in (Default `"en"`)
- `dictcc-destination-lang`: Second language to look up words in (Default `"de"`)

  Despite the names source and destination, [dictcc](http://dict.cc) actually
  just looks up queries in both languages at once.  This means that it does not
  matter if you specify the attributes as `"de"` and `"fr"` or `"fr"` and
  `"de"`.  There is, however, a difference between these terms when it comes to
  which language to insert into the buffer.

- `dictcc-completion-backend`: Which completion backend to use (Default
  `'ivy`). If you have `helm` installed but not `ivy`, it will be set to `'helm`
  automatically.  If you want to use neither `helm`, nor `ivy`, setting the
  variable to `completing-read` will make use of Emacs's default completion
  system.
