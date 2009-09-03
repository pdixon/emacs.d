# File Structure
Everything lives under `.emacs.d`. `init.el` is the entry point. This
file set up the paths where everything else is found.

Paths:

`tweaks`
:  This is where all my setup and configuration lives.

`snippets`
:  Text snippets for yasnippets.

`vendor`
:  Third party packages.


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
Version: 6.28e
Downloaded From:
[org-mode](http://orgmode.org/index.html#sec-3)

### Remember
Version: 2.0
[remember](https://gna.org/p/remember-el)

### org-mac-protocol

### org-blog

## yasnippet
Version: 0.5.10
[yasnippet](http://code.google.com/p/yasnippet/)

## company mode
Version: 0.4.3
[company-mode](http://nschum.de/src/emacs/company-mode/)

## File modes

### lua mode
What it sounds like.
from [here](http://luaforge.net/projects/lua-mode/).

### lilypond

### rst

### taskpaper

### markdown-mode
Get it from [here](http://jblevins.org/projects/markdown-mode/).

### AuCTex
Version 11.85 from [here](http://www.gnu.org/software/auctex/)

Installing on Mac OSX in a portable way is... interesting.
I ended up using the following:

    ./configure --prefix=/Users/pdixon/.emacs.d/vendor/ \\
    --with-lispdir=/Users/pdixon/.emacs.d/vendor/ \\
    --with-emacs="/Applications/Emacs.app/Contents/MacOS/Emacs" \\
    --without-texmf-dir

# Key-bindings

Based on [Xahlee's Dvorak Ergo
Keybindings](http://xahlee.org/emacs/ergonomic_emacs_keybinding.html).

I've made some changes.

# External Programs
## Aspell
This is essential for flyspell. When I was Carbon Emacs this was
bundled in the package. Since I've changed to Emacs 23 nightly builds
I don't get this convenience. After spending hours trying various
ports and binary builds I finally hit upon the easy solution. Build it
from source. It has not non-standard dependencies, so 5 minutes later
it was all done.
