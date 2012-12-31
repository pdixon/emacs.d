# File Structure
Everything lives under `.emacs.d`. `init.el` is the entry point. This
file set up the paths where everything else is found.

Paths:
`lisp`
:   elisp code I've written.

`mytemplates`
:   Autoinsert file templates.

`snippets`
:   Text snippets for yasnippets.

`tweaks`
:   This is where all my setup and configuration lives.

`user`
:   Machine specific customisations.

`vendor`
:   Third party packages. Where possible I'm moving to using submodules
    to track these from upstream.


# Packages
Where ever possible I'm using package.el to manage these. I have
[melpa](), and [marmalade]() set up as package sources as well as the
default gnu elpa.

Anything that isn't packaged I include as a sub-module, or as a last
resort I'll check the file in directly. In either of these cases the
package well be living under `vendor`

[melpa]:
[marmalade]:

# Applications

## Org mode
Version: As bundled with emacs 24.
[org-mode](http://orgmode.org/index.html#sec-3)

I used to use Remember with org-mode, since org-capture was released
I've been using that.

## Deft

## Gnus

## Magit

## Dired

# Auto Typing
I like to make the computer do as much of my work as I possibly can.
Hence I've spend a bit of time on the various auto typing features.

## yasnippet
[yasnippet](http://code.google.com/p/yasnippet/)

## autoinsert
As bundled with emacs.

# Key-bindings

Based on [Xahlee's Dvorak Ergo
Keybindings](http://xahlee.org/emacs/ergonomic_emacs_keybinding.html).

I've made some changes.

Actually I don't use this anymore. Enough other programs (i.e all
standard text fields in Cocoa) use Emacs style key bindings that it's
worth learning the stand bindings.

# File modes

## lua mode
What it sounds like.
from [here](http://luaforge.net/projects/lua-mode/).

## lilypond

## markdown-mode
Version: 1.7
Get it from [here](http://jblevins.org/projects/markdown-mode/).

## AuCTex
Version 11.85 from [here](http://www.gnu.org/software/auctex/)

Installing on Mac OSX in a portable way is... interesting.
I ended up using the following:

    ./configure --prefix=/Users/pdixon/.emacs.d/vendor/ \\
    --with-lispdir=/Users/pdixon/.emacs.d/vendor/ \\
    --with-emacs="/Applications/Emacs.app/Contents/MacOS/Emacs" \\
    --without-texmf-dir

## Haskell
Version 2.4 from [here](http://www.iro.umontreal.ca/~monnier/elisp/#haskell-mode)


# External Programs
## Aspell
This is essential for flyspell. When I was Carbon Emacs this was
bundled in the package. Since I've changed to Emacs 23 (now 24)
nightly builds I don't get this convenience. After spending hours
trying various ports and binary builds I finally hit upon the easy
solution. Build it from source. It has not non-standard dependencies,
so 5 minutes later it was all done.

## Mac OSX `PATH`
On Mac OSX Emacs.app doesn't inherit the `PATH` from the shell.
Instead it looks in the Defaults system, specifically
`.MacOSX\environment.plist`. Create this using:

    defaults write $HOME/.MacOSX/environment PATH "$PATH"
