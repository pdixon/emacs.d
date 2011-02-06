# File Structure
Everything lives under `.emacs.d`. `init.el` is the entry point. This
file set up the paths where everything else is found.

Paths:

`tweaks`
:   This is where all my setup and configuration lives.

`mysnippets`
:   Text snippets for yasnippets.

`mytemplates`
:   Autoinsert file templates.

`vendor`
:   Third party packages. Where possible I'm moving to using submodules
    to track these from upstream.


# Vendor Libraries
All third party packages live under `vendor`.

## Color Theme
Easy management of color schemes.

### [Zenburn](http://www.brockman.se/software/zenburn/)
The most important part of Vim.

I've applied this
[patch](http://sysphere.org/~anrxc/local/scr/sources/color-theme-zenburn-orgmode.patch)
to add support for org-mode. I've also added colors for rst mode.

## Org mode
Version: git
Downloaded From:
[org-mode](http://orgmode.org/index.html#sec-3)

I used to use Remember with org-mode, since org-capture was released
I've been using that.

## Auto Typing
I like to make the computer do as much of my work as I possibly can.
Hence I've spend a bit of time on the various auto typing features.

## yasnippet
Version: 0.6.1c
[yasnippet](http://code.google.com/p/yasnippet/)

## autopair
Version: 0.3
[autopair](http://code.google.com/p/autopair/)

## autocomplete
Version: git
[autocomplete](http://cx4a.org/software/auto-complete/)

## autoinsert
As bundled with emacs.

## File modes

### lua mode
What it sounds like.
from [here](http://luaforge.net/projects/lua-mode/).

### lilypond

### rst

### markdown-mode
Version: 1.7
Get it from [here](http://jblevins.org/projects/markdown-mode/).

### AuCTex
Version 11.85 from [here](http://www.gnu.org/software/auctex/)

Installing on Mac OSX in a portable way is... interesting.
I ended up using the following:

    ./configure --prefix=/Users/pdixon/.emacs.d/vendor/ \\
    --with-lispdir=/Users/pdixon/.emacs.d/vendor/ \\
    --with-emacs="/Applications/Emacs.app/Contents/MacOS/Emacs" \\
    --without-texmf-dir

### Haskell
Version 2.4 from [here](http://www.iro.umontreal.ca/~monnier/elisp/#haskell-mode)

# Key-bindings

Based on [Xahlee's Dvorak Ergo
Keybindings](http://xahlee.org/emacs/ergonomic_emacs_keybinding.html).

I've made some changes.

Actually I don't use this anymore. Enough other programs (i.e all
standard text fields in Cocoa) use Emacs style key bindings that it's
worth learning the stand bindings.

# External Programs
## Aspell
This is essential for flyspell. When I was Carbon Emacs this was
bundled in the package. Since I've changed to Emacs 23 nightly builds
I don't get this convenience. After spending hours trying various
ports and binary builds I finally hit upon the easy solution. Build it
from source. It has not non-standard dependencies, so 5 minutes later
it was all done.

## Mac OSX `PATH`
On Mac OSX Emacs.app doesn't inherit the `PATH` from the shell.
Instead it looks in the Defaults system, specifically
`.MacOSX\environment.plist`. Create this using:

    defaults write $HOME/.MacOSX/environment PATH "$PATH"
    
