;;; org-exp.el --- ASCII, HTML, XOXO and iCalendar export for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.26d
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

(require 'org)
(require 'org-agenda)
(eval-and-compile
  (require 'cl))

(declare-function org-export-latex-preprocess "org-latex" (parameters))
(declare-function org-export-ascii-preprocess "org-ascii" (parameters))
(declare-function org-export-html-preprocess "org-html" (parameters))
(declare-function org-export-docbook-preprocess "org-docbook" (parameters))
(declare-function org-agenda-skip "org-agenda" ())
(declare-function org-infojs-options-inbuffer-template "org-jsinfo" ())
(declare-function org-export-htmlize-region-for-paste "org-html" (beg end))

(defgroup org-export nil
  "Options for exporting org-listings."
  :tag "Org Export"
  :group 'org)

(defgroup org-export-general nil
  "General options for exporting Org-mode files."
  :tag "Org Export General"
  :group 'org-export)

;; FIXME
(defvar org-export-publishing-directory nil)

(defcustom org-export-run-in-background nil
  "Non-nil means export and publishing commands will run in background.
This works by starting up a separate Emacs process visiting the same file
and doing the export from there.
Not all export commands are affected by this - only the ones which
actually write to a file, and that do not depend on the buffer state.

If this option is nil, you can still get background export by calling
`org-export' with a double prefix arg: `C-u C-u C-c C-e'.

If this option is t, the double prefix can be used to exceptionally
force an export command into the current process."
  :group 'org-export-general
  :type 'boolean)


(defcustom org-export-select-tags '("export")
  "Tags that select a tree for export.
If any such tag is found in a buffer, all trees that do not carry one
of these tags will be deleted before export.
Inside trees that are selected like this, you can still deselect a
subtree by tagging it with one of the `org-export-exclude-tags'."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag")))

(defcustom org-export-exclude-tags '("noexport")
  "Tags that exclude a tree from export.
All trees carrying any of these tags will be excluded from export.
This is without condition, so even subtrees inside that carry one of the
`org-export-select-tags' will be removed."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag")))

;; FIXME: rename, this is a general variable
(defcustom org-export-html-expand t
  "Non-nil means, for HTML export, treat @<...> as HTML tag.
When nil, these tags will be exported as plain text and therefore
not be interpreted by a browser.

This option can also be set with the +OPTIONS line, e.g. \"@:nil\"."
  :group 'org-export-html
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-special-strings t
  "Non-nil means, interpret \"\-\", \"--\" and \"---\" for export.
When this option is turned on, these strings will be exported as:

  Org   HTML       LaTeX
 -----+----------+--------
  \\-    &shy;      \\-
  --    &ndash;    --
  ---   &mdash;    ---
  ...   &hellip;   \ldots

This option can also be set with the +OPTIONS line, e.g. \"-:nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-html-link-up ""
  "Where should the \"UP\" link of exported HTML pages lead?"
  :group 'org-export-html
  :group 'org-export-general
  :type '(string :tag "File or URL"))

(defcustom org-export-html-link-home ""
  "Where should the \"HOME\" link of exported HTML pages lead?"
  :group 'org-export-html
  :group 'org-export-general
  :type '(string :tag "File or URL"))

(defcustom org-export-language-setup
  '(("en" "Author"     "Date"  "Table of Contents" "Footnotes")
    ("ca"  "Autor"      "Data" "&Iacute;ndex" "Peus de p&agrave;gina")
    ("cs" "Autor"      "Datum" "Obsah" "Pozn\xe1mky pod carou")
    ("da" "Ophavsmand" "Dato"  "Indhold" "Fodnoter")
    ("de" "Autor"      "Datum" "Inhaltsverzeichnis" "Fu&szlig;noten")
    ("eo"  "A&#365;toro"      "Dato" "Enhavo" "Piednotoj")
    ("es" "Autor"      "Fecha" "&Iacute;ndice" "Pies de p&aacute;gina")
    ("fi" "Tekij&auml;"     "P&auml;iv&auml;m&auml;&auml;r&auml;"   "Sis&auml;llysluettelo"  "Alaviitteet")
    ("fr" "Auteur"     "Date"  "Table des mati&egrave;res" "Notes de bas de page")
    ("hu" "Szerz&otilde;" "D&aacute;tum" "Tartalomjegyz&eacute;k" "L&aacute;bjegyzet")
    ("is" "H&ouml;fundur" "Dagsetning" "Efnisyfirlit" "Aftanm&aacute;lsgreinar")
    ("it" "Autore"     "Data"  "Indice" "Note a pi&egrave; di pagina")
    ("nl" "Auteur"     "Datum" "Inhoudsopgave" "Voetnoten")
    ("no" "Forfatter"  "Dato"  "Innhold" "Fotnoter")
    ("nb" "Forfatter"  "Dato"  "Innhold" "Fotnoter")  ;; nb = Norsk (bokm.l)
    ("nn" "Forfattar"  "Dato"  "Innhald" "Fotnotar")  ;; nn = Norsk (nynorsk)
    ("pl" "Autor"      "Data" "Spis tre&sacute;ci"  "Przypis")
    ("sv" "F&ouml;rfattare" "Datum" "Inneh&aring;ll" "Fotnoter"))
  "Terms used in export text, translated to different languages.
Use the variable `org-export-default-language' to set the language,
or use the +OPTION lines for a per-file setting."
  :group 'org-export-general
  :type '(repeat
	  (list
	   (string :tag "HTML language tag")
	   (string :tag "Author")
	   (string :tag "Date")
	   (string :tag "Table of Contents")
	   (string :tag "Footnotes"))))

(defcustom org-export-default-language "en"
  "The default language of HTML export, as a string.
This should have an association in `org-export-language-setup'."
  :group 'org-export-general
  :type 'string)

(defvar org-export-page-description ""
  "The page description, for the XHTML meta tag.
This is best set with the #+DESCRIPTION line in a file, it does not make
sense to set this globally.")

(defvar org-export-page-keywords ""
  "The page description, for the XHTML meta tag.
This is best set with the #+KEYWORDS line in a file, it does not make
sense to set this globally.")

(defcustom org-export-skip-text-before-1st-heading nil
  "Non-nil means, skip all text before the first headline when exporting.
When nil, that text is exported as well."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-headline-levels 3
  "The last level which is still exported as a headline.
Inferior levels will produce itemize lists when exported.
Note that a numeric prefix argument to an exporter function overrides
this setting.

This option can also be set with the +OPTIONS line, e.g. \"H:2\"."
  :group 'org-export-general
  :type 'integer)

(defcustom org-export-with-section-numbers t
  "Non-nil means, add section numbers to headlines when exporting.

This option can also be set with the +OPTIONS line, e.g. \"num:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-section-number-format '((("1" ".")) . "")
  "Format of section numbers for export.
The variable has two components.
1. A list of lists, each indicating a counter type and a separator.
   The counter type can be any of \"1\", \"A\", \"a\", \"I\", or \"a\".
   It causes causes numeric, alphabetic, or roman counters, respectively.
   The separator is only used if another counter for a subsection is being
   added.
   If there are more numbered section levels than entries in this lists,
   then the last entry will be reused.
2. A terminator string that will be added after the entire
   section number."
  :group 'org-export-general
  :type '(cons
	  (repeat
	   (list
	    (string :tag "Counter Type")
	    (string :tag "Separator   ")))
	  (string :tag "Terminator")))

(defcustom org-export-with-toc t
  "Non-nil means, create a table of contents in exported files.
The TOC contains headlines with levels up to`org-export-headline-levels'.
When an integer, include levels up to N in the toc, this may then be
different from `org-export-headline-levels', but it will not be allowed
to be larger than the number of headline levels.
When nil, no table of contents is made.

Headlines which contain any TODO items will be marked with \"(*)\" in
ASCII export, and with red color in HTML output, if the option
`org-export-mark-todo-in-toc' is set.

In HTML output, the TOC will be clickable.

This option can also be set with the +OPTIONS line, e.g. \"toc:nil\"
or \"toc:3\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "No Table of Contents" nil)
	  (const :tag "Full Table of Contents" t)
	  (integer :tag "TOC to level")))

(defcustom org-export-mark-todo-in-toc nil
  "Non-nil means, mark TOC lines that contain any open TODO items."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-todo-keywords t
  "Non-nil means, include TODO keywords in export.
When nil, remove all these keywords from the export."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-priority nil
  "Non-nil means, include priority cookies in export.
When nil, remove priority cookies for export."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-preserve-breaks nil
  "Non-nil means, preserve all line breaks when exporting.
Normally, in HTML output paragraphs will be reformatted.  In ASCII
export, line breaks will always be preserved, regardless of this variable.

This option can also be set with the +OPTIONS line, e.g. \"\\n:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-archived-trees 'headline
  "Whether subtrees with the ARCHIVE tag should be exported.
This can have three different values
nil       Do not export, pretend this tree is not present
t         Do export the entire tree
headline  Only export the headline, but skip the tree below it."
  :group 'org-export-general
  :group 'org-archive
  :type '(choice
	  (const :tag "not at all" nil)
	  (const :tag "headline only" 'headline)
	  (const :tag "entirely" t)))

(defcustom org-export-author-info t
  "Non-nil means, insert author name and email into the exported file.

This option can also be set with the +OPTIONS line,
e.g. \"author-info:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-creator-info t
  "Non-nil means, the postamble should contain a creator sentence.
This sentence is \"HTML generated by org-mode XX in emacs XXX\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-time-stamp-file t
  "Non-nil means, insert a time stamp into the exported file.
The time stamp shows when the file was created.

This option can also be set with the +OPTIONS line,
e.g. \"timestamp:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-timestamps t
  "If nil, do not export time stamps and associated keywords."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-remove-timestamps-from-toc t
  "If nil, remove timestamps from the table of contents entries."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-tags 'not-in-toc
  "If nil, do not export tags, just remove them from headlines.
If this is the symbol `not-in-toc', tags will be removed from table of
contents entries, but still be shown in the headlines of the document.

This option can also be set with the +OPTIONS line, e.g. \"tags:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Off" nil)
	  (const :tag "Not in TOC" not-in-toc)
	  (const :tag "On" t)))

(defcustom org-export-with-drawers nil
  "Non-nil means, export with drawers like the property drawer.
When t, all drawers are exported.  This may also be a list of
drawer names to export."
  :group 'org-export-general
  :type '(choice
	  (const :tag "All drawers" t)
	  (const :tag "None" nil)
	  (repeat :tag "Selected drawers"
		  (string :tag "Drawer name"))))

(defvar org-export-preprocess-hook nil
  "Hook for preprocessing an export buffer.
Pretty much the first thing when exporting is running this hook.")

(defvar org-export-preprocess-after-include-files-hook nil
  "Hook for preprocessing an export buffer.
This is run after the contents of included files have been inserted.")

(defvar org-export-preprocess-after-tree-selection-hook nil
  "Hook for preprocessing an export buffer.
This is run after selection of trees to be exported has happened.
This selection includes tags-based selection, as well as removal
of commented and archived trees.")

(defvar org-export-preprocess-before-backend-specifics-hook nil
  "Hook run before backend-specific functions are called during preprocessing.")

(defvar org-export-preprocess-final-hook nil
  "Hook for preprocessing an export buffer.
This is run as the last thing in the preprocessing buffer, just before
returning the buffer string to the backend.")

(defgroup org-export-translation nil
  "Options for translating special ascii sequences for the export backends."
  :tag "Org Export Translation"
  :group 'org-export)

(defcustom org-export-with-emphasize t
  "Non-nil means, interpret *word*, /word/, and _word_ as emphasized text.
If the export target supports emphasizing text, the word will be
typeset in bold, italic, or underlined, respectively.  Works only for
single words, but you can say: I *really* *mean* *this*.
Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"*:nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-with-footnotes t
  "If nil, export [1] as a footnote marker.
Lines starting with [1] will be formatted as footnotes.

This option can also be set with the +OPTIONS line, e.g. \"f:nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-with-sub-superscripts t
  "Non-nil means, interpret \"_\" and \"^\" for export.
When this option is turned on, you can use TeX-like syntax for sub- and
superscripts.  Several characters after \"_\" or \"^\" will be
considered as a single item - so grouping with {} is normally not
needed.  For example, the following things will be parsed as single
sub- or superscripts.

 10^24   or   10^tau     several digits will be considered 1 item.
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
			 terminated by almost any nonword/nondigit char.
 x_{i^2} or   x^(2-i)    braces or parenthesis do grouping.

Still, ambiguity is possible - so when in doubt use {} to enclose the
sub/superscript.  If you set this variable to the symbol `{}',
the braces are *required* in order to trigger interpretations as
sub/superscript.  This can be helpful in documents that need \"_\"
frequently in plain text.

Not all export backends support this, but HTML does.

This option can also be set with the +OPTIONS line, e.g. \"^:nil\"."
  :group 'org-export-translation
  :type '(choice
	  (const :tag "Always interpret" t)
	  (const :tag "Only with braces" {})
	  (const :tag "Never interpret" nil)))

(defcustom org-export-with-TeX-macros t
  "Non-nil means, interpret simple TeX-like macros when exporting.
For example, HTML export converts \\alpha to &alpha; and \\AA to &Aring;.
Not only real TeX macros will work here, but the standard HTML entities
for math can be used as macro names as well.  For a list of supported
names in HTML export, see the constant `org-html-entities'.
Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"TeX:nil\"."
  :group 'org-export-translation
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-with-LaTeX-fragments nil
  "Non-nil means, convert LaTeX fragments to images when exporting to HTML.
When set, the exporter will find LaTeX environments if the \\begin line is
the first non-white thing on a line.  It will also find the math delimiters
like $a=b$ and \\( a=b \\) for inline math,  $$a=b$$ and \\[ a=b \\] for
display math.

This option can also be set with the +OPTIONS line, e.g. \"LaTeX:t\"."
  :group 'org-export-translation
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-with-fixed-width t
  "Non-nil means, lines starting with \":\" will be in fixed width font.
This can be used to have pre-formatted text, fragments of code etc.  For
example:
  : ;; Some Lisp examples
  : (while (defc cnt)
  :   (ding))
will be looking just like this in also HTML.  See also the QUOTE keyword.
Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"::nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-match-sexp-depth 3
  "Number of stacked braces for sub/superscript matching.
This has to be set before loading org.el to be effective."
  :group 'org-export-translation
  :type 'integer)

(defgroup org-export-tables nil
  "Options for exporting tables in Org-mode."
  :tag "Org Export Tables"
  :group 'org-export)

(defcustom org-export-with-tables t
  "If non-nil, lines starting with \"|\" define a table.
For example:

  | Name        | Address  | Birthday  |
  |-------------+----------+-----------|
  | Arthur Dent | England  | 29.2.2100 |

Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"|:nil\"."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-highlight-first-table-line t
  "Non-nil means, highlight the first table line.
In HTML export, this means use <th> instead of <td>.
In tables created with table.el, this applies to the first table line.
In Org-mode tables, all lines before the first horizontal separator
line will be formatted with <th> tags."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-table-remove-special-lines t
  "Remove special lines and marking characters in calculating tables.
This removes the special marking character column from tables that are set
up for spreadsheet calculations.  It also removes the entire lines
marked with `!', `_', or `^'.  The lines with `$' are kept, because
the values of constants may be useful to have."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-prefer-native-exporter-for-tables nil
  "Non-nil means, always export tables created with table.el natively.
Natively means, use the HTML code generator in table.el.
When nil, Org-mode's own HTML generator is used when possible (i.e. if
the table does not use row- or column-spanning).  This has the
advantage, that the automatic HTML conversions for math symbols and
sub/superscripts can be applied.  Org-mode's HTML generator is also
much faster."
  :group 'org-export-tables
  :type 'boolean)


(defgroup org-export-xml nil
  "Options specific for XML export of Org-mode files."
  :tag "Org Export XML"
  :group 'org-export)

;;;; Exporting

;;; Variables, constants, and parameter plists

(defconst org-level-max 20)

(defvar org-current-export-file nil) ; dynamically scoped parameter
(defvar org-current-export-dir nil) ; dynamically scoped parameter
(defvar org-export-opt-plist nil
  "Contains the current option plist.")
(defvar org-last-level nil) ; dynamically scoped variable
(defvar org-min-level nil) ; dynamically scoped variable
(defvar org-levels-open nil) ; dynamically scoped parameter

(defconst org-export-plist-vars
  '((:link-up		      nil	  org-export-html-link-up)
    (:link-home		      nil	  org-export-html-link-home)
    (:language		      nil	  org-export-default-language)
    (:keywords		      nil	  org-export-page-keywords)
    (:description             nil	  org-export-page-description)
    (:customtime	      nil	  org-display-custom-times)
    (:headline-levels	      "H"	  org-export-headline-levels)
    (:section-numbers	      "num"	  org-export-with-section-numbers)
    (:section-number-format   nil	  org-export-section-number-format)
    (:table-of-contents	      "toc"	  org-export-with-toc)
    (:preserve-breaks	      "\\n"	  org-export-preserve-breaks)
    (:archived-trees	      nil	  org-export-with-archived-trees)
    (:emphasize		      "*"	  org-export-with-emphasize)
    (:sub-superscript	      "^"	  org-export-with-sub-superscripts)
    (:special-strings	      "-"	  org-export-with-special-strings)
    (:footnotes		      "f"	  org-export-with-footnotes)
    (:drawers		      "d"	  org-export-with-drawers)
    (:tags		      "tags"	  org-export-with-tags)
    (:todo-keywords	      "todo"	  org-export-with-todo-keywords)
    (:priority		      "pri"	  org-export-with-priority)
    (:TeX-macros	      "TeX"	  org-export-with-TeX-macros)
    (:LaTeX-fragments	      "LaTeX"	  org-export-with-LaTeX-fragments)
    (:skip-before-1st-heading "skip"	  org-export-skip-text-before-1st-heading)
    (:fixed-width	      ":"	  org-export-with-fixed-width)
    (:timestamps	      "<"	  org-export-with-timestamps)
    (:author-info	      "author"	  org-export-author-info)
    (:creator-info	      "creator"	  org-export-creator-info)
    (:time-stamp-file	      "timestamp" org-export-time-stamp-file)
    (:tables		      "|"	  org-export-with-tables)
    (:table-auto-headline     nil	  org-export-highlight-first-table-line)
    (:style-include-default   nil	  org-export-html-style-include-default)
    (:style-include-scripts   nil	  org-export-html-style-include-scripts)
    (:style		      nil	  org-export-html-style)
    (:style-extra	      nil	  org-export-html-style-extra)
    (:agenda-style	      nil	  org-agenda-export-html-style)
    (:convert-org-links	      nil	  org-export-html-link-org-files-as-html)
    (:inline-images	      nil	  org-export-html-inline-images)
    (:html-extension	      nil	  org-export-html-extension)
    (:html-table-tag	      nil	  org-export-html-table-tag)
    (:expand-quoted-html      "@"	  org-export-html-expand)
    (:timestamp		      nil	  org-export-html-with-timestamp)
    (:publishing-directory    nil	  org-export-publishing-directory)
    (:preamble		      nil	  org-export-html-preamble)
    (:postamble		      nil	  org-export-html-postamble)
    (:auto-preamble	      nil	  org-export-html-auto-preamble)
    (:auto-postamble	      nil	  org-export-html-auto-postamble)
    (:author		      nil	  user-full-name)
    (:email		      nil	  user-mail-address)
    (:select-tags	      nil	  org-export-select-tags)
    (:exclude-tags	      nil	  org-export-exclude-tags))
  "List of properties that represent export/publishing variables.
Each element is a list of 3 items:
1. The property that is used internally, and also for org-publish-project-alist
2. The string that can be used in the OPTION lines to set this option,
   or nil if this option cannot be changed in this way
3. The customization variable that sets the default for this option."
)

(defun org-default-export-plist ()
  "Return the property list with default settings for the export variables."
  (let ((l org-export-plist-vars) rtn e s v)
    (while (setq e (pop l))
      (setq s (nth 2 e)
	    v (if (boundp s) (symbol-value s) nil)
	    rtn (cons (car e) (cons v rtn))))
    rtn))

(defvar org-export-inbuffer-options-extra nil
  "List of additional in-buffer options that should be detected.
Just before export, the buffer is scanned for options like #+TITLE, #+EMAIL,
etc.  Extensions can add to this list to get their options detected, and they
can then add a function to `org-export-options-filters' to process these
options.
Each element in this list must be a list, with the in-buffer keyword as car,
and a property (a symbol) as the next element.  All occurrences of the
keyword will be found, the values concatenated with a space character
in between, and the result stored in the export options property list.")

(defvar org-export-options-filters nil
  "Functions to be called to finalize the export/publishing options.
All these options are stored in a property list, and each of the functions
in this hook gets a chance to modify this property list.  Each function
must accept the property list as an argument, and must return the (possibly
modified) list.")

;; FIXME: should we fold case here?
(defun org-infile-export-plist ()
  "Return the property list with file-local settings for export."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((re (org-make-options-regexp
		 (append
		  '("TITLE" "AUTHOR" "DATE" "EMAIL" "TEXT" "OPTIONS" "LANGUAGE"
		    "LINK_UP" "LINK_HOME" "SETUPFILE" "STYLE" "LATEX_HEADER"
		    "EXPORT_SELECT_TAGS" "EXPORT_EXCLUDE_TAGS"
		    "KEYWORDS" "DESCRIPTION")
		  (mapcar 'car org-export-inbuffer-options-extra))))
	    p key val text options a pr style
	    latex-header
	    ext-setup-or-nil setup-contents (start 0))
	(while (or (and ext-setup-or-nil
			(string-match re ext-setup-or-nil start)
			(setq start (match-end 0)))
		   (and (setq ext-setup-or-nil nil start 0)
			(re-search-forward re nil t)))
	  (setq key (upcase (org-match-string-no-properties 1 ext-setup-or-nil))
		val (org-match-string-no-properties 2 ext-setup-or-nil))
	  (cond
	   ((setq a (assoc key org-export-inbuffer-options-extra))
	    (setq pr (nth 1 a))
	    (setq p (plist-put p pr (concat (plist-get p pr) " " val))))
	   ((string-equal key "TITLE") (setq p (plist-put p :title val)))
	   ((string-equal key "AUTHOR")(setq p (plist-put p :author val)))
	   ((string-equal key "EMAIL") (setq p (plist-put p :email val)))
	   ((string-equal key "DATE") (setq p (plist-put p :date val)))
	   ((string-equal key "KEYWORDS") (setq p (plist-put p :keywords val)))
	   ((string-equal key "DESCRIPTION")
	    (setq p (plist-put p :description val)))
	   ((string-equal key "LANGUAGE") (setq p (plist-put p :language val)))
	   ((string-equal key "STYLE")
	    (setq style (concat style "\n" val)))
	   ((string-equal key "LATEX_HEADER")
	    (setq latex-header (concat latex-header "\n" val)))
	   ((string-equal key "TEXT")
	    (setq text (if text (concat text "\n" val) val)))
	   ((string-equal key "OPTIONS")
	    (setq options (concat val " " options)))
	   ((string-equal key "LINK_UP")
	    (setq p (plist-put p :link-up val)))
	   ((string-equal key "LINK_HOME")
	    (setq p (plist-put p :link-home val)))
	   ((string-equal key "EXPORT_SELECT_TAGS")
	    (setq p (plist-put p :select-tags (org-split-string val))))
	   ((string-equal key "EXPORT_EXCLUDE_TAGS")
	    (setq p (plist-put p :exclude-tags (org-split-string val))))
	   ((equal key "SETUPFILE")
	    (setq setup-contents (org-file-contents
				  (expand-file-name
				   (org-remove-double-quotes
				    (org-trim val)))
				  'noerror))
	    (if (not ext-setup-or-nil)
		(setq ext-setup-or-nil setup-contents start 0)
	      (setq ext-setup-or-nil
		    (concat (substring ext-setup-or-nil 0 start)
			    "\n" setup-contents "\n"
			    (substring ext-setup-or-nil start)))))))
	(setq p (plist-put p :text text))
	(when style (setq p (plist-put p :style-extra style)))
	(when latex-header
	  (setq p (plist-put p :latex-header-extra (substring latex-header 1))))
	(when options
	  (setq p (org-export-add-options-to-plist p options)))
	;; Add macro definitions
	(goto-char (point-min))
	(while (re-search-forward
		"^#\\+macro:[ \t]+\\([-a-zA-Z0-9_]+\\)[ \t]+\\(.*?[ \t]*$\\)"
		nil t)
	  (setq p (plist-put p (intern (concat ":macro-"
					       (downcase (match-string 1))))
			     (match-string 2))))
	p))))

(defun org-export-add-options-to-plist (p options)
  "Parse an OPTIONS line and set values in the property list P."
  (let (o)
    (when options
      (let ((op org-export-plist-vars))
	(while (setq o (pop op))
	  (if (and (nth 1 o)
		   (string-match (concat (regexp-quote (nth 1 o))
					 ":\\([^ \t\n\r;,.]*\\)")
				 options))
	      (setq p (plist-put p (car o)
				 (car (read-from-string
				       (match-string 1 options))))))))))
  p)

(defun org-export-add-subtree-options (p pos)
  "Add options in subtree at position POS to property list P."
  (save-excursion
    (goto-char pos)
    (when (org-at-heading-p)
      (let (a)
	;; This is actually read in `org-export-get-title-from-subtree'
	;; (when (setq a (org-entry-get pos "EXPORT_TITLE"))
	;;   (setq p (plist-put p :title a)))
	(when (setq a (org-entry-get pos "EXPORT_TEXT"))
	  (setq p (plist-put p :text a)))
	(when (setq a (org-entry-get pos "EXPORT_AUTHOR"))
	  (setq p (plist-put p :author a)))
	(when (setq a (org-entry-get pos "EXPORT_DATE"))
	  (setq p (plist-put p :date a)))
	(when (setq a (org-entry-get pos "EXPORT_OPTIONS"))
	  (setq p (org-export-add-options-to-plist p a)))))
    p))

(defun org-export-directory (type plist)
  (let* ((val (plist-get plist :publishing-directory))
	 (dir (if (listp val)
		  (or (cdr (assoc type val)) ".")
		val)))
    dir))

(defun org-export-process-option-filters (plist)
  (let ((functions org-export-options-filters) f)
    (while (setq f (pop functions))
      (setq plist (funcall f plist))))
  plist)

;;;###autoload
(defun org-export (&optional arg)
  "Export dispatcher for Org-mode.
When `org-export-run-in-background' is non-nil, try to run the command
in the background.  This will be done only for commands that write
to a file.  For details see the docstring of `org-export-run-in-background'.

The prefix argument ARG will be passed to the exporter.  However, if
ARG is a double universal prefix `C-u C-u', that means to inverse the
value of `org-export-run-in-background'."
  (interactive "P")
  (let* ((bg (org-xor (equal arg '(16)) org-export-run-in-background))
	 (help "[t]   insert the export option template
\[v]   limit export to visible part of outline tree

\[a] export as ASCII

\[h] export as HTML    [H] to temporary buffer   [R] export region
\[b] export as HTML and open in browser

\[l] export as LaTeX   [L] to temporary buffer
\[p] export as LaTeX and process to PDF
\[d] export as LaTeX, process to PDF, and open the resulting PDF document

\[D] export as DocBook
\[V] export as DocBook, process to PDF, and open the resulting PDF document

\[x] export as XOXO

\[i] export current file as iCalendar file
\[I] export all agenda files as iCalendar files
\[c] export agenda files into combined iCalendar file

\[F] publish current file          [P] publish current project
\[X] publish a project...          [A] publish all projects")
	 (cmds
	  '((?t org-insert-export-options-template nil)
	    (?v org-export-visible nil)
	    (?a org-export-as-ascii t)
	    (?h org-export-as-html t)
	    (?b org-export-as-html-and-open t)
	    (?H org-export-as-html-to-buffer nil)
	    (?R org-export-region-as-html nil)
	    (?x org-export-as-xoxo t)
	    (?D org-export-as-docbook t)
	    (?V org-export-as-docbook-pdf-and-open t)
	    (?l org-export-as-latex t)
	    (?p org-export-as-pdf t)
	    (?d org-export-as-pdf-and-open t)
	    (?L org-export-as-latex-to-buffer nil)
	    (?i org-export-icalendar-this-file t)
	    (?I org-export-icalendar-all-agenda-files t)
	    (?c org-export-icalendar-combine-agenda-files t)
	    (?F org-publish-current-file t)
	    (?P org-publish-current-project t)
	    (?X org-publish t)
	    (?A org-publish-all t)))
	 r1 r2 ass)
    (save-excursion
      (save-window-excursion
	(delete-other-windows)
	(with-output-to-temp-buffer "*Org Export/Publishing Help*"
	  (princ help))
	(org-fit-window-to-buffer (get-buffer-window
				   "*Org Export/Publishing Help*"))
	(message "Select command: ")
	(setq r1 (read-char-exclusive))))
    (setq r2 (if (< r1 27) (+ r1 96) r1))
    (unless (setq ass (assq r2 cmds))
      (error "No command associated with key %c" r1))
    (if (and bg (nth 2 ass)
	     (not (buffer-base-buffer))
	     (not (org-region-active-p)))
	;; execute in background
	(let ((p (start-process
		  (concat "Exporting " (file-name-nondirectory (buffer-file-name)))
		  "*Org Processes*"
		  (expand-file-name invocation-name invocation-directory)
		  "-batch"
		  "-l" user-init-file
		  "--eval" "(require 'org-exp)"
		  "--eval" "(setq org-wait .2)"
		  (buffer-file-name)
		  "-f" (symbol-name (nth 1 ass)))))
	  (set-process-sentinel p 'org-export-process-sentinel)
	  (message "Background process \"%s\": started" p))
      ;; background processing not requested, or not possible
      (call-interactively (nth 1 ass)))))

(defun org-export-process-sentinel (process status)
  (if (string-match "\n+\\'" status)
      (setq status (substring status 0 -1)))
  (message "Background process \"%s\": %s" process status))

(defconst org-html-entities
  '(("nbsp")
    ("iexcl")
    ("cent")
    ("pound")
    ("curren")
    ("yen")
    ("brvbar")
    ("vert" . "&#124;")
    ("sect")
    ("uml")
    ("copy")
    ("ordf")
    ("laquo")
    ("not")
    ("shy")
    ("reg")
    ("macr")
    ("deg")
    ("plusmn")
    ("sup2")
    ("sup3")
    ("acute")
    ("micro")
    ("para")
    ("middot")
    ("odot"."o")
    ("star"."*")
    ("cedil")
    ("sup1")
    ("ordm")
    ("raquo")
    ("frac14")
    ("frac12")
    ("frac34")
    ("iquest")
    ("Agrave")
    ("Aacute")
    ("Acirc")
    ("Atilde")
    ("Auml")
    ("Aring") ("AA"."&Aring;")
    ("AElig")
    ("Ccedil")
    ("Egrave")
    ("Eacute")
    ("Ecirc")
    ("Euml")
    ("Igrave")
    ("Iacute")
    ("Icirc")
    ("Iuml")
    ("ETH")
    ("Ntilde")
    ("Ograve")
    ("Oacute")
    ("Ocirc")
    ("Otilde")
    ("Ouml")
    ("times")
    ("Oslash")
    ("Ugrave")
    ("Uacute")
    ("Ucirc")
    ("Uuml")
    ("Yacute")
    ("THORN")
    ("szlig")
    ("agrave")
    ("aacute")
    ("acirc")
    ("atilde")
    ("auml")
    ("aring")
    ("aelig")
    ("ccedil")
    ("egrave")
    ("eacute")
    ("ecirc")
    ("euml")
    ("igrave")
    ("iacute")
    ("icirc")
    ("iuml")
    ("eth")
    ("ntilde")
    ("ograve")
    ("oacute")
    ("ocirc")
    ("otilde")
    ("ouml")
    ("divide")
    ("oslash")
    ("ugrave")
    ("uacute")
    ("ucirc")
    ("uuml")
    ("yacute")
    ("thorn")
    ("yuml")
    ("fnof")
    ("Alpha")
    ("Beta")
    ("Gamma")
    ("Delta")
    ("Epsilon")
    ("Zeta")
    ("Eta")
    ("Theta")
    ("Iota")
    ("Kappa")
    ("Lambda")
    ("Mu")
    ("Nu")
    ("Xi")
    ("Omicron")
    ("Pi")
    ("Rho")
    ("Sigma")
    ("Tau")
    ("Upsilon")
    ("Phi")
    ("Chi")
    ("Psi")
    ("Omega")
    ("alpha")
    ("beta")
    ("gamma")
    ("delta")
    ("epsilon")
    ("varepsilon"."&epsilon;")
    ("zeta")
    ("eta")
    ("theta")
    ("iota")
    ("kappa")
    ("lambda")
    ("mu")
    ("nu")
    ("xi")
    ("omicron")
    ("pi")
    ("rho")
    ("sigmaf") ("varsigma"."&sigmaf;")
    ("sigma")
    ("tau")
    ("upsilon")
    ("phi")
    ("chi")
    ("psi")
    ("omega")
    ("thetasym") ("vartheta"."&thetasym;")
    ("upsih")
    ("piv")
    ("bull") ("bullet"."&bull;")
    ("hellip") ("dots"."&hellip;")
    ("prime")
    ("Prime")
    ("oline")
    ("frasl")
    ("weierp")
    ("image")
    ("real")
    ("trade")
    ("alefsym")
    ("larr") ("leftarrow"."&larr;") ("gets"."&larr;")
    ("uarr") ("uparrow"."&uarr;")
    ("rarr") ("to"."&rarr;") ("rightarrow"."&rarr;")
    ("darr")("downarrow"."&darr;")
    ("harr") ("leftrightarrow"."&harr;")
    ("crarr") ("hookleftarrow"."&crarr;") ; has round hook, not quite CR
    ("lArr") ("Leftarrow"."&lArr;")
    ("uArr") ("Uparrow"."&uArr;")
    ("rArr") ("Rightarrow"."&rArr;")
    ("dArr") ("Downarrow"."&dArr;")
    ("hArr") ("Leftrightarrow"."&hArr;")
    ("forall")
    ("part") ("partial"."&part;")
    ("exist") ("exists"."&exist;")
    ("empty") ("emptyset"."&empty;")
    ("nabla")
    ("isin") ("in"."&isin;")
    ("notin")
    ("ni")
    ("prod")
    ("sum")
    ("minus")
    ("lowast") ("ast"."&lowast;")
    ("radic")
    ("prop") ("proptp"."&prop;")
    ("infin") ("infty"."&infin;")
    ("ang") ("angle"."&ang;")
    ("and") ("wedge"."&and;")
    ("or") ("vee"."&or;")
    ("cap")
    ("cup")
    ("int")
    ("there4")
    ("sim")
    ("cong") ("simeq"."&cong;")
    ("asymp")("approx"."&asymp;")
    ("ne") ("neq"."&ne;")
    ("equiv")
    ("le")
    ("ge")
    ("sub") ("subset"."&sub;")
    ("sup") ("supset"."&sup;")
    ("nsub")
    ("sube")
    ("supe")
    ("oplus")
    ("otimes")
    ("perp")
    ("sdot") ("cdot"."&sdot;")
    ("lceil")
    ("rceil")
    ("lfloor")
    ("rfloor")
    ("lang")
    ("rang")
    ("loz") ("Diamond"."&loz;")
    ("spades") ("spadesuit"."&spades;")
    ("clubs") ("clubsuit"."&clubs;")
    ("hearts") ("diamondsuit"."&hearts;")
    ("diams") ("diamondsuit"."&diams;")
    ("smile"."&#9786;") ("blacksmile"."&#9787;") ("sad"."&#9785;")
    ("quot")
    ("amp")
    ("lt")
    ("gt")
    ("OElig")
    ("oelig")
    ("Scaron")
    ("scaron")
    ("Yuml")
    ("circ")
    ("tilde")
    ("ensp")
    ("emsp")
    ("thinsp")
    ("zwnj")
    ("zwj")
    ("lrm")
    ("rlm")
    ("ndash")
    ("mdash")
    ("lsquo")
    ("rsquo")
    ("sbquo")
    ("ldquo")
    ("rdquo")
    ("bdquo")
    ("dagger")
    ("Dagger")
    ("permil")
    ("lsaquo")
    ("rsaquo")
    ("euro")

    ("arccos"."arccos")
    ("arcsin"."arcsin")
    ("arctan"."arctan")
    ("arg"."arg")
    ("cos"."cos")
    ("cosh"."cosh")
    ("cot"."cot")
    ("coth"."coth")
    ("csc"."csc")
    ("deg"."deg")
    ("det"."det")
    ("dim"."dim")
    ("exp"."exp")
    ("gcd"."gcd")
    ("hom"."hom")
    ("inf"."inf")
    ("ker"."ker")
    ("lg"."lg")
    ("lim"."lim")
    ("liminf"."liminf")
    ("limsup"."limsup")
    ("ln"."ln")
    ("log"."log")
    ("max"."max")
    ("min"."min")
    ("Pr"."Pr")
    ("sec"."sec")
    ("sin"."sin")
    ("sinh"."sinh")
    ("sup"."sup")
    ("tan"."tan")
    ("tanh"."tanh")
    )
  "Entities for TeX->HTML translation.
Entries can be like (\"ent\"), in which case \"\\ent\" will be translated to
\"&ent;\".  An entry can also be a dotted pair like (\"ent\".\"&other;\").
In that case, \"\\ent\" will be translated to \"&other;\".
The list contains HTML entities for Latin-1, Greek and other symbols.
It is supplemented by a number of commonly used TeX macros with appropriate
translations.  There is currently no way for users to extend this.")

;;; General functions for all backends

(defvar org-export-target-aliases nil
  "Alist of targets with invisible aliases.")
(defvar org-export-preferred-target-alist nil
  "Alist of section id's with preferred aliases.")
(defvar org-export-code-refs nil
  "Alist of code references and line numbers")

(defun org-export-preprocess-string (string &rest parameters)
  "Cleanup STRING so that that the true exported has a more consistent source.
This function takes STRING, which should be a buffer-string of an org-file
to export.  It then creates a temporary buffer where it does its job.
The result is then again returned as a string, and the exporter works
on this string to produce the exported version."
  (interactive)
  (let* ((htmlp (plist-get parameters :for-html))
	 (asciip (plist-get parameters :for-ascii))
	 (latexp (plist-get parameters :for-LaTeX))
	 (docbookp (plist-get parameters :for-docbook))
	 (backend (cond (htmlp 'html)
			(latexp 'latex)
			(asciip 'ascii)
			(docbookp 'docbook)))
	 (archived-trees (plist-get parameters :archived-trees))
	 (inhibit-read-only t)
	 (drawers org-drawers)
	 (outline-regexp "\\*+ ")
	 target-alist rtn)

    (setq org-export-target-aliases nil)
    (setq org-export-preferred-target-alist nil)
    (setq org-export-code-refs nil)

    (with-current-buffer (get-buffer-create " org-mode-tmp")
      (erase-buffer)
      (insert string)
      (setq case-fold-search t)

      ;; Remove license-to-kill stuff
      ;; The caller marks some stuff for killing, stuff that has been
      ;; used to create the page title, for example.
      (org-export-kill-licensed-text)

      (let ((org-inhibit-startup t)) (org-mode))
      (setq case-fold-search t)

      ;; Call the hook
      (run-hooks 'org-export-preprocess-hook)

      ;; Process the macros
      (org-export-preprocess-apply-macros)
      (run-hooks 'org-export-preprocess-after-macros-hook)

      (untabify (point-min) (point-max))

      ;; Handle include files, and call a hook
      (org-export-handle-include-files)
      (run-hooks 'org-export-preprocess-after-include-files-hook)

      ;; Get rid of archived trees
      (org-export-remove-archived-trees archived-trees)

      ;; Remove comment environment and comment subtrees
      (org-export-remove-comment-blocks-and-subtrees)

      ;; Get rid of excluded trees, and call a hook
      (org-export-handle-export-tags (plist-get parameters :select-tags)
				     (plist-get parameters :exclude-tags))
      (run-hooks 'org-export-preprocess-after-tree-selection-hook)

      ;; Handle source code snippets
      (org-export-replace-src-segments-and-examples backend)

      ;; Protect short examples marked by a leading colon
      (org-export-protect-colon-examples)

      ;; Normalize footnotes
      (when (plist-get parameters :footnotes)
	(org-footnote-normalize nil t))

      ;; Find all headings and compute the targets for them
      (setq target-alist (org-export-define-heading-targets target-alist))

      ;; Get rid of drawers
      (org-export-remove-or-extract-drawers drawers
					    (plist-get parameters :drawers))

      ;; Get the correct stuff before the first headline
      (when (plist-get parameters :skip-before-1st-heading)
	(goto-char (point-min))
	(when (re-search-forward "^\\(#.*\n\\)?\\*+[ \t]" nil t)
	  (delete-region (point-min) (match-beginning 0))
	  (goto-char (point-min))
	  (insert "\n")))
      (when (plist-get parameters :add-text)
	(goto-char (point-min))
	(insert (plist-get parameters :add-text) "\n"))

      ;; Remove todo-keywords before exporting, if the user has requested so
      (org-export-remove-headline-metadata parameters)

      ;; Find targets in comments and move them out of comments,
      ;; but mark them as targets that should be invisible
      (setq target-alist (org-export-handle-invisible-targets target-alist))

      ;; Select and protect backend specific stuff, throw away stuff
      ;; that is specific for other backends
      (org-export-select-backend-specific-text backend)

      ;; Protect quoted subtrees
      (org-export-protect-quoted-subtrees)

      ;; Remove clock lines
      (org-export-remove-clock-lines)

      ;; Protect verbatim elements
      (org-export-protect-verbatim)

      ;; Blockquotes, verse, and center
      (org-export-mark-blockquote-verse-center)

      ;; Remove timestamps, if the user has requested so
      (unless (plist-get parameters :timestamps)
	(org-export-remove-timestamps))

      ;; Attach captions to the correct object
      (setq target-alist (org-export-attach-captions-and-attributes
			  backend target-alist))

      ;; Find matches for radio targets and turn them into internal links
      (org-export-mark-radio-links)

      ;; Find all links that contain a newline and put them into a single line
      (org-export-concatenate-multiline-links)

      ;; Normalize links: Convert angle and plain links into bracket links
      ;; and expand link abbreviations
      (org-export-normalize-links)

      ;; Find all internal links.  If they have a fuzzy match (i.e. not
      ;; a *dedicated* target match, let the link  point to the
      ;; corresponding section.
      (org-export-target-internal-links target-alist)

      ;; Find multiline emphasis and put them into single line
      (when (plist-get parameters :emph-multiline)
	(org-export-concatenate-multiline-emphasis))

      ;; Remove special table lines
      (when org-export-table-remove-special-lines
	(org-export-remove-special-table-lines))

      ;; Another hook
      (run-hooks 'org-export-preprocess-before-backend-specifics-hook)

      ;; LaTeX-specific preprocessing
      (when latexp
	(require 'org-latex nil)
	(org-export-latex-preprocess parameters))

      ;; ASCII-specific preprocessing
      (when asciip
	(org-export-ascii-preprocess parameters))

      ;; HTML-specific preprocessing
      (when htmlp
	(org-export-html-preprocess parameters))
      
      ;; DocBook-specific preprocessing
      (when docbookp
	(require 'org-docbook nil)
	(org-export-docbook-preprocess parameters))

      ;; Remove or replace comments
      (org-export-handle-comments (plist-get parameters :comments))

      ;; Run the final hook
      (run-hooks 'org-export-preprocess-final-hook)

      (setq rtn (buffer-string)))
    (kill-buffer " org-mode-tmp")
    rtn))

(defun org-export-kill-licensed-text ()
  "Remove all text that is marked with a :org-license-to-kill property."
  (let (p)
    (while (setq p (text-property-any (point-min) (point-max)
				      :org-license-to-kill t))
      (delete-region
       p (or (next-single-property-change p :org-license-to-kill)
	     (point-max))))))

(defun org-export-define-heading-targets (target-alist)
  "Find all headings and define the targets for them.
The new targets are added to TARGET-ALIST, which is also returned."
  (goto-char (point-min))
  (org-init-section-numbers)
  (let ((re (concat "^" org-outline-regexp
		    "\\| [ \t]*:\\(ID\\|CUSTOM_ID\\):[ \t]*\\([^ \t\r\n]+\\)"))
	level target last-section-target a id)
    (while (re-search-forward re nil t)
      (if (match-end 2)
	  (progn
	    (setq id (org-match-string-no-properties 2))
	    (push (cons id target) target-alist)
	    (setq a (or (assoc last-section-target org-export-target-aliases)
			(progn
			  (push (list last-section-target)
				org-export-target-aliases)
			  (car org-export-target-aliases))))
	    (push (caar target-alist) (cdr a))
	    (when (equal (match-string 1) "CUSTOM_ID")
	      (if (not (assoc last-section-target
			      org-export-preferred-target-alist))
		  (push (cons last-section-target id)
			org-export-preferred-target-alist))))
	(setq level (org-reduced-level
		     (save-excursion (goto-char (point-at-bol))
				     (org-outline-level))))
	(setq target (org-solidify-link-text
		      (format "sec-%s" (org-section-number level))))
	(setq last-section-target target)
	(push (cons target target) target-alist)
	(add-text-properties
	 (point-at-bol) (point-at-eol)
	 (list 'target target)))))
  target-alist)

(defun org-export-handle-invisible-targets (target-alist)
  "Find targets in comments and move them out of comments.
Mark them as invisible targets."
  (let (target tmp a)
    (goto-char (point-min))
    (while (re-search-forward "^#.*?\\(<<<?\\([^>\r\n]+\\)>>>?\\).*" nil t)
      ;; Check if the line before or after is a headline with a target
      (if (setq target (or (get-text-property (point-at-bol 0) 'target)
			   (get-text-property (point-at-bol 2) 'target)))
	  (progn
	    ;; use the existing target in a neighboring line
	    (setq tmp (match-string 2))
	    (replace-match "")
	    (and (looking-at "\n") (delete-char 1))
	    (push (cons (setq tmp (org-solidify-link-text tmp)) target)
		  target-alist)
	    (setq a (or (assoc target org-export-target-aliases)
			(progn
			  (push (list target) org-export-target-aliases)
			  (car org-export-target-aliases))))
	    (push tmp (cdr a)))
	;; Make an invisible target
	(replace-match "\\1(INVISIBLE)"))))
  target-alist)

(defun org-export-target-internal-links (target-alist)
  "Find all internal links and assign targets to them.
If a link has a fuzzy match (i.e. not a *dedicated* target match),
let the link  point to the corresponding section.
This function also handles the id links, if they have a match in
the current file."
  (goto-char (point-min))
  (while (re-search-forward org-bracket-link-regexp nil t)
    (org-if-unprotected
     (let* ((md (match-data))
	    (desc (match-end 2))
	    (link (org-link-unescape (match-string 1)))
	    (slink (org-solidify-link-text link))
	    found props pos cref
	    (target
	     (cond
	      ((= (string-to-char link) ?#)
	       ;; user wants exactly this link
	       link)
	      ((cdr (assoc slink target-alist))
	       (or (cdr (assoc (assoc slink target-alist)
			       org-export-preferred-target-alist))
		   (cdr (assoc slink target-alist))))
	      ((and (string-match "^id:" link)
		    (cdr (assoc (substring link 3) target-alist))))
	      ((string-match "^(\\(.*\\))$" link)
	       (setq cref (match-string 1 link))
	       (concat "coderef:" cref))
	      ((string-match org-link-types-re link) nil)
	      ((or (file-name-absolute-p link)
		   (string-match "^\\." link))
	       nil)
	      (t
	       (save-excursion
		 (setq found (condition-case nil (org-link-search link)
			       (error nil)))
		 (when (and found
			    (or (org-on-heading-p)
				(not (eq found 'dedicated))))
		   (or (get-text-property (point) 'target)
		       (get-text-property
			(max (point-min)
			     (1- (or (previous-single-property-change
				      (point) 'target) 0)))
			'target))))))))
       (when target
	 (set-match-data md)
	 (goto-char (match-beginning 1))
	 (setq props (text-properties-at (point)))
	 (delete-region (match-beginning 1) (match-end 1))
	 (setq pos (point))
	 (insert target)
	 (unless desc (insert "][" link))
	 (add-text-properties pos (point) props))))))

(defun org-export-remove-or-extract-drawers (all-drawers exp-drawers)
  "Remove drawers, or extract the content.
ALL-DRAWERS is a list of all drawer names valid in the current buffer.
EXP-DRAWERS can be t to keep all drawer contents, or a list of drawers
whose content to keep."
  (unless (eq t exp-drawers)
    (goto-char (point-min))
    (let ((re (concat "^[ \t]*:\\("
		      (mapconcat
		       'identity
		       (org-delete-all exp-drawers
				       (copy-sequence all-drawers))
		       "\\|")
		      "\\):[ \t]*$"))
	  beg eol)
      (while (re-search-forward re nil t)
	(org-if-unprotected
	 (setq beg (match-beginning 0)
	       eol (match-end 0))
	 (if (re-search-forward "^\\([ \t]*:END:[ \t]*\n?\\)\\|^\\*+[ \t]"
				nil t)
	     (if (match-end 1)
		 ;; terminated in this entry
		 (progn
		   (delete-region beg (match-end 1))
		   (goto-char beg))
	       (goto-char eol))))))))

(defun org-export-handle-export-tags (select-tags exclude-tags)
  "Modify the buffer, honoring SELECT-TAGS and EXCLUDE-TAGS.
Both arguments are lists of tags.
If any of SELECT-TAGS is found, all trees not marked by a SELECT-TAG
will be removed.
After that, all subtrees that are marked by EXCLUDE-TAGS will be
removed as well."
  (remove-text-properties (point-min) (point-max) '(:org-delete t))
  (let* ((re-sel (concat ":\\(" (mapconcat 'regexp-quote
					   select-tags "\\|")
			 "\\):"))
	 (re-excl (concat ":\\(" (mapconcat 'regexp-quote
					   exclude-tags "\\|")
			"\\):"))
	 beg end cont)
    (goto-char (point-min))
    (when (and select-tags
	       (re-search-forward
		(concat "^\\*+[ \t].*" re-sel "[^ \t\n]*[ \t]*$") nil t))
      ;; At least one tree is marked for export, this means
      ;; all the unmarked stuff needs to go.
      ;; Dig out the trees that should be exported
      (goto-char (point-min))
      (outline-next-heading)
      (setq beg (point))
      (put-text-property beg (point-max) :org-delete t)
      (while (re-search-forward re-sel nil t)
	(when (org-on-heading-p)
	  (org-back-to-heading)
	  (remove-text-properties
	   (max (1- (point)) (point-min))
	   (setq cont (save-excursion (org-end-of-subtree t t)))
	   '(:org-delete t))
	  (while (and (org-up-heading-safe)
		      (get-text-property (point) :org-delete))
	    (remove-text-properties (max (1- (point)) (point-min))
				    (point-at-eol) '(:org-delete t)))
	  (goto-char cont))))
    ;; Remove the trees explicitly marked for noexport
    (when exclude-tags
      (goto-char (point-min))
      (while (re-search-forward re-excl nil t)
	(when (org-at-heading-p)
	  (org-back-to-heading t)
	  (setq beg (point))
	  (org-end-of-subtree t)
	  (delete-region beg (point)))))
    ;; Remove everything that is now still marked for deletion
    (goto-char (point-min))
    (while (setq beg (text-property-any (point-min) (point-max) :org-delete t))
      (setq end (or (next-single-property-change beg :org-delete)
		    (point-max)))
      (delete-region beg end))))

(defun org-export-remove-archived-trees (export-archived-trees)
  "Remove archived trees.
When EXPORT-ARCHIVED-TREES is `headline;, only the headline will be exported.
When it is t, the entire archived tree will be exported.
When it is nil the entire tree including the headline will be removed
from the buffer."
  (let ((re-archive (concat ":" org-archive-tag ":"))
	a b)
    (when (not (eq export-archived-trees t))
      (goto-char (point-min))
      (while (re-search-forward re-archive nil t)
	(if (not (org-on-heading-p t))
	    (org-end-of-subtree t)
	  (beginning-of-line 1)
	  (setq a (if export-archived-trees
		      (1+ (point-at-eol)) (point))
		b (org-end-of-subtree t))
	  (if (> b a) (delete-region a b)))))))

(defun org-export-remove-headline-metadata (opts)
  "Remove meta data from the headline, according to user options."
  (let ((re org-complex-heading-regexp)
	(todo (plist-get opts :todo-keywords))
	(tags (plist-get opts :tags))
	(pri  (plist-get opts :priority))
	(elts '(1 2 3 4 5))
	rpl)
    (setq elts (delq nil (list 1 (if todo 2) (if pri 3) 4 (if tags 5))))
    (when (or (not todo) (not tags) (not pri))
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(org-if-unprotected
	 (setq rpl (mapconcat (lambda (i) (if (match-end i) (match-string i) ""))
			      elts " "))
	 (replace-match rpl t t))))))

(defun org-export-remove-timestamps ()
  "Remove timestamps and keywords for export."
  (goto-char (point-min))
  (while (re-search-forward org-maybe-keyword-time-regexp nil t)
    (backward-char 1)
    (org-if-unprotected
     (unless (save-match-data (org-at-table-p))
       (replace-match "")
       (beginning-of-line 1)
       (if (looking-at "[- \t]*\\(=>[- \t0-9:]*\\)?[ \t]*\n")
	   (replace-match ""))))))

(defun org-export-remove-clock-lines ()
  "Remove clock lines for export."
  (goto-char (point-min))
  (let ((re (concat "^[ \t]*" org-clock-string ".*\n?")))
    (while (re-search-forward re nil t)
      (org-if-unprotected
       (replace-match "")))))

(defun org-export-protect-quoted-subtrees ()
  "Mark quoted subtrees with the protection property."
  (let ((re-quote (concat "^\\*+[ \t]+" org-quote-string "\\>")))
    (goto-char (point-min))
    (while (re-search-forward re-quote nil t)
      (goto-char (match-beginning 0))
      (end-of-line 1)
      (add-text-properties (point) (org-end-of-subtree t)
			   '(org-protected t)))))

(defun org-export-protect-verbatim ()
  "Mark verbatim snippets with the protection property."
  (goto-char (point-min))
  (while (re-search-forward org-verbatim-re nil t)
    (add-text-properties (match-beginning 4) (match-end 4)
			 '(org-protected t))
    (goto-char (1+ (match-end 4)))))

(defun org-export-protect-colon-examples ()
  "Protect lines starting with a colon."
  (goto-char (point-min))
  (let ((re "^[ \t]*:\\([ \t]\\|$\\)") beg)
    (while (re-search-forward re nil t)
      (beginning-of-line 1)
      (setq beg (point))
      (while (looking-at re)
	(end-of-line 1)
	(or (eobp) (forward-char 1)))
      (add-text-properties beg (if (bolp) (1- (point)) (point))
			   '(org-protected t)))))

(defun org-export-select-backend-specific-text (backend)
  (let ((formatters
	 '((docbook "DOCBOOK" "BEGIN_DOCBOOK" "END_DOCBOOK")
	   (html "HTML" "BEGIN_HTML" "END_HTML")
	   (ascii "ASCII" "BEGIN_ASCII" "END_ASCII")
	   (latex "LaTeX" "BEGIN_LaTeX" "END_LaTeX")))
	(case-fold-search t)
	fmt)

    (while formatters
      (setq fmt (pop formatters))
      (when (eq (car fmt) backend)
	;; This is selected code, put it into the file for real
	(goto-char (point-min))
	(while (re-search-forward (concat "^#\\+" (cadr fmt)
					  ":[ \t]*\\(.*\\)") nil t)
	  (replace-match "\\1" t)
	  (add-text-properties
	   (point-at-bol) (min (1+ (point-at-eol)) (point-max))
	   '(org-protected t))))
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "^#\\+"
		      (caddr fmt) "\\>.*\\(\\(\n.*\\)*?\n\\)#\\+"
		      (cadddr fmt) "\\>.*\n?") nil t)
	(if (eq (car fmt) backend)
	    ;; yes, keep this
	    (add-text-properties (match-beginning 1) (1+ (match-end 1))
				 '(org-protected t))
	  ;; No, this is for a different backend, kill it
	  (delete-region (match-beginning 0) (match-end 0)))))))

(defun org-export-mark-blockquote-verse-center ()
  "Mark block quote and verse environments with special cookies.
These special cookies will later be interpreted by the backend."
  ;; Blockquotes
  (goto-char (point-min))
  (while (re-search-forward "^#\\+\\(begin\\|end\\)_\\(block\\)?quote\\>.*"
			    nil t)
    (replace-match (if (equal (downcase (match-string 1)) "end")
		       "ORG-BLOCKQUOTE-END" "ORG-BLOCKQUOTE-START")
		   t t))
  ;; Verse
  (goto-char (point-min))
  (while (re-search-forward "^#\\+\\(begin\\|end\\)_verse\\>.*" nil t)
    (replace-match (if (equal (downcase (match-string 1)) "end")
		       "ORG-VERSE-END" "ORG-VERSE-START")
		   t t))
  ;; Center
  (goto-char (point-min))
  (while (re-search-forward "^#\\+\\(begin\\|end\\)_center\\>.*" nil t)
    (replace-match (if (equal (downcase (match-string 1)) "end")
		       "ORG-CENTER-END" "ORG-CENTER-START")
		   t t)))

(defun org-export-attach-captions-and-attributes (backend target-alist)
  "Move #+CAPTION, #+ATTR_BACKEND, and #+LABEL text into text properties.
If the next thing following is a table, add the text properties to the first
table line.  If it is a link, add it to the line containing the link."
  (goto-char (point-min))
  (remove-text-properties (point-min) (point-max)
			  '(org-caption nil org-attributes nil))
  (let ((case-fold-search t)
	(re (concat "^#\\+caption:[ \t]+\\(.*\\)"
		    "\\|"
		    "^#\\+attr_" (symbol-name backend) ":[ \t]+\\(.*\\)"
		    "\\|"
		    "^#\\+label:[ \t]+\\(.*\\)"
		    "\\|"
		    "^[ \t]*|[^-]"
		    "\\|"
		    "^[ \t]*\\[\\[.*\\]\\][ \t]*$"))
	cap attr label)
    (while (re-search-forward re nil t)
      (cond
       ((match-end 1)
	(setq cap (concat cap (if cap " " "") (org-trim (match-string 1)))))
       ((match-end 2)
	(setq attr (concat attr (if attr " " "") (org-trim (match-string 2)))))
       ((match-end 3)
	(setq label (org-trim (match-string 3))))
       (t
	(add-text-properties (point-at-bol) (point-at-eol)
			     (list 'org-caption cap
				   'org-attributes attr
				   'org-label label))
	(if label (push (cons label label) target-alist))
	(setq cap nil attr nil label nil)))))
  target-alist)

(defun org-export-remove-comment-blocks-and-subtrees ()
  "Remove the comment environment, and also commented subtrees."
  (let ((re-commented (concat "^\\*+[ \t]+" org-comment-string "\\>"))
	(case-fold-search nil))
    ;; Remove comment environment
    (goto-char (point-min))
    (while (re-search-forward
	    "^#\\+BEGIN_COMMENT[ \t]*\n[^\000]*?^#\\+END_COMMENT\\>.*" nil t)
      (replace-match "" t t))
    ;; Remove subtrees that are commented
    (goto-char (point-min))
    (while (re-search-forward re-commented nil t)
      (goto-char (match-beginning 0))
      (delete-region (point) (org-end-of-subtree t)))))

(defun org-export-handle-comments (commentsp)
  "Remove comments, or convert to backend-specific format.
COMMENTSP can be a format string for publishing comments.
When it is nil, all comments will be removed."
  (let ((re "^#\\(.*\n?\\)")
	pos)
    (goto-char (point-min))
    (while (or (looking-at re)
	       (re-search-forward re nil t))
      (setq pos (match-beginning 0))
      (if commentsp
	  (progn (add-text-properties
		  (match-beginning 0) (match-end 0) '(org-protected t))
		 (replace-match (format commentsp (match-string 1)) t t))
	(goto-char (1+ pos))
	(org-if-unprotected
	 (replace-match "")
	 (goto-char (max (point-min) (1- pos))))))))

(defun org-export-mark-radio-links ()
  "Find all matches for radio targets and turn them into internal links."
  (let ((re-radio (and org-target-link-regexp
		       (concat "\\([^<]\\)\\(" org-target-link-regexp "\\)"))))
    (goto-char (point-min))
    (when re-radio
      (while (re-search-forward re-radio nil t)
	(org-if-unprotected
	 (replace-match "\\1[[\\2]]"))))))

(defun org-export-remove-special-table-lines ()
  "Remove tables lines that are used for internal purposes."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*|" nil t)
    (beginning-of-line 1)
    (if (or (looking-at "[ \t]*| *[!_^] *|")
	    (and (looking-at ".*?| *<[0-9]+> *|")
		 (not (looking-at ".*?| *[^ <|]"))))
	(delete-region (max (point-min) (1- (point-at-bol)))
		       (point-at-eol))
      (end-of-line 1))))

(defun org-export-normalize-links ()
  "Convert all links to bracket links, and expand link abbreviations."
  (let ((re-plain-link (concat "\\([^[<]\\)" org-plain-link-re))
	(re-angle-link (concat "\\([^[]\\)" org-angle-link-re)))
    (goto-char (point-min))
    (while (re-search-forward re-plain-link nil t)
      (goto-char (1- (match-end 0)))
      (org-if-unprotected-at (1+ (match-beginning 0))
       (let* ((s (concat (match-string 1) "[[" (match-string 2)
			 ":" (match-string 3) "]]")))
	 ;; added 'org-link face to links
	 (put-text-property 0 (length s) 'face 'org-link s)
	 (replace-match s t t))))
    (goto-char (point-min))
    (while (re-search-forward re-angle-link nil t)
      (goto-char (1- (match-end 0)))
      (org-if-unprotected
       (let* ((s (concat (match-string 1) "[[" (match-string 2)
			 ":" (match-string 3) "]]")))
	 (put-text-property 0 (length s) 'face 'org-link s)
	 (replace-match s t t))))
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-regexp nil t)
      (goto-char (1- (match-end 0)))
      (org-if-unprotected
       (let* ((xx (save-match-data
		    (org-translate-link
		     (org-link-expand-abbrev (match-string 1)))))
	      (s (concat
		  "[[" (org-add-props (copy-sequence xx)
			   nil 'org-protected t)
		  "]"
		  (if (match-end 3)
		      (match-string 2)
		    (concat "[" (org-add-props
				    (copy-sequence xx)
				    '(org-protected t))
			    "]"))
		  "]")))
	 (put-text-property 0 (length s) 'face 'org-link s)
	 (replace-match s t t))))))

(defun org-export-concatenate-multiline-links ()
  "Find multi-line links and put it all into a single line.
This is to make sure that the line-processing export backends
can work correctly."
  (goto-char (point-min))
  (while (re-search-forward "\\(\\(\\[\\|\\]\\)\\[[^]]*?\\)[ \t]*\n[ \t]*\\([^]]*\\]\\(\\[\\|\\]\\)\\)" nil t)
    (org-if-unprotected
     (replace-match "\\1 \\3")
     (goto-char (match-beginning 0)))))

(defun org-export-concatenate-multiline-emphasis ()
  "Find multi-line emphasis and put it all into a single line.
This is to make sure that the line-processing export backends
can work correctly."
  (goto-char (point-min))
  (while (re-search-forward org-emph-re nil t)
    (if (and (not (= (char-after (match-beginning 3))
		     (char-after (match-beginning 4))))
	     (save-excursion (goto-char (match-beginning 0))
			     (save-match-data (not (org-at-table-p)))))
	(org-if-unprotected
	 (subst-char-in-region (match-beginning 0) (match-end 0)
			       ?\n ?\  t)
	 (goto-char (1- (match-end 0))))
      (goto-char (1+ (match-beginning 0))))))

(defun org-export-grab-title-from-buffer ()
  "Get a title for the current document, from looking at the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let ((end (if (looking-at org-outline-regexp)
		     (point)
		   (save-excursion (outline-next-heading) (point)))))
	(when (re-search-forward "^[ \t]*[^|# \t\r\n].*\n" end t)
	  ;; Mark the line so that it will not be exported as normal text.
	  (org-unmodified
	   (add-text-properties (match-beginning 0) (match-end 0)
				(list :org-license-to-kill t)))
	  ;; Return the title string
	  (org-trim (match-string 0)))))))

(defun org-export-get-title-from-subtree ()
  "Return subtree title and exclude it from export."
  (let (title (rbeg (region-beginning)) (rend (region-end)))
    (save-excursion
      (goto-char rbeg)
      (when (and (org-at-heading-p)
		 (>= (org-end-of-subtree t t) rend))
	;; This is a subtree, we take the title from the first heading
	(goto-char rbeg)
	(looking-at org-todo-line-regexp)
	(setq title (match-string 3))
	(org-unmodified
	 (add-text-properties (point) (1+ (point-at-eol))
			      (list :org-license-to-kill t)))
	(setq title (or (org-entry-get nil "EXPORT_TITLE") title))))
    title))

(defun org-solidify-link-text (s &optional alist)
  "Take link text and make a safe target out of it."
  (save-match-data
    (let* ((rtn
	    (mapconcat
	     'identity
	     (org-split-string s "[ \t\r\n]+") "=="))
	   (a (assoc rtn alist)))
      (or (cdr a) rtn))))

(defun org-get-min-level (lines &optional offset)
  "Get the minimum level in LINES."
  (let ((re "^\\(\\*+\\) ") l)
    (catch 'exit
      (while (setq l (pop lines))
	(if (string-match re l)
	    (throw 'exit (org-tr-level (- (length (match-string 1 l))
					  (or offset 0))))))
      1)))

;; Variable holding the vector with section numbers
(defvar org-section-numbers (make-vector org-level-max 0))

(defun org-init-section-numbers ()
  "Initialize the vector for the section numbers."
  (let* ((level  -1)
	 (numbers (nreverse (org-split-string "" "\\.")))
	 (depth (1- (length org-section-numbers)))
	 (i depth) number-string)
    (while (>= i 0)
      (if (> i level)
	  (aset org-section-numbers i 0)
	(setq number-string (or (car numbers) "0"))
	(if (string-match "\\`[A-Z]\\'" number-string)
	    (aset org-section-numbers i
		  (- (string-to-char number-string) ?A -1))
	  (aset org-section-numbers i (string-to-number number-string)))
	(pop numbers))
      (setq i (1- i)))))

(defun org-section-number (&optional level)
  "Return a string with the current section number.
When LEVEL is non-nil, increase section numbers on that level."
  (let* ((depth (1- (length org-section-numbers)))
	 (string "")
	 (fmts (car org-export-section-number-format))
	 (term (cdr org-export-section-number-format))
	 (sep "")
	 ctype fmt idx n)
    (when level
      (when (> level -1)
	(aset org-section-numbers
	      level (1+ (aref org-section-numbers level))))
      (setq idx (1+ level))
      (while (<= idx depth)
	(if (not (= idx 1))
	    (aset org-section-numbers idx 0))
	(setq idx (1+ idx))))
    (setq idx 0)
    (while (<= idx depth)
      (when (> (aref org-section-numbers idx) 0)
	(setq fmt (or (pop fmts) fmt)
	      ctype (car fmt)
	      n (aref org-section-numbers idx)
	      string (if (> n 0)
			 (concat string sep (org-number-to-counter n ctype))
		       (concat string ".0"))
	      sep (nth 1 fmt)))
      (setq idx (1+ idx)))
    (save-match-data
      (if (string-match "\\`\\([@0]\\.\\)+" string)
	  (setq string (replace-match "" t nil string)))
      (if (string-match "\\(\\.0\\)+\\'" string)
	  (setq string (replace-match "" t nil string))))
    (concat string term)))

(defun org-number-to-counter (n type)
  "Concert number N to a string counter, according to TYPE.
TYPE must be a string, any of:
 1  number
 A  A,B,....
 a  a,b,....
 I  upper case roman numeral
 i  lower case roman numeral"
  (cond
   ((equal type "1") (number-to-string n))
   ((equal type "A") (char-to-string (+ ?A n -1)))
   ((equal type "a") (char-to-string (+ ?a n -1)))
   ((equal type "I") (org-number-to-roman n))
   ((equal type "i") (downcase (org-number-to-roman n)))
   (t (error "Invalid counter type `%s'" type))))

(defun org-number-to-roman (n)
  "Convert integer N into a roman numeral."
  (let ((roman '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
		 ( 100 . "C") ( 90 . "XC") ( 50 . "L") ( 40 . "XL")
		 (  10 . "X") (  9 . "IX") (  5 . "V") (  4 . "IV")
		 (   1 . "I")))
	(res ""))
    (if (<= n 0)
	(number-to-string n)
      (while roman
	(if (>= n (caar roman))
	    (setq n (- n (caar roman))
		  res (concat res (cdar roman)))
	  (pop roman)))
      res)))

;;; Macros

(defun org-export-preprocess-apply-macros ()
  "Replace macro references."
  (goto-char (point-min))
  (let (sy val key)
    (while (re-search-forward "{{{\\([a-zA-Z][-a-zA-Z0-9_]*\\)}}}" nil t)
      (setq key (downcase (match-string 1)))
      (when (setq val (or (plist-get org-export-opt-plist
				     (intern (concat ":macro-" key)))
			  (plist-get org-export-opt-plist
				     (intern (concat ":" key)))))
	(and (stringp val)
	     (replace-match val t t))))))

(defun org-export-apply-macros-in-string (s)
  "Apply the macros in string S."
  (when s
    (with-temp-buffer
      (insert s)
      (org-export-preprocess-apply-macros)
      (buffer-string))))

;;; Include files

(defun org-export-handle-include-files ()
  "Include the contents of include files, with proper formatting."
  (let ((case-fold-search t)
	params file markup lang start end prefix prefix1 switches)
    (goto-char (point-min))
    (while (re-search-forward "^#\\+INCLUDE:?[ \t]+\\(.*\\)" nil t)
      (setq params (read (concat "(" (match-string 1) ")"))
	    prefix (org-get-and-remove-property 'params :prefix)
	    prefix1 (org-get-and-remove-property 'params :prefix1)
	    file (org-symname-or-string (pop params))
	    markup (org-symname-or-string (pop params))
	    lang (and (member markup '("src" "SRC"))
		      (org-symname-or-string (pop params)))
	    switches (mapconcat '(lambda (x) (format "%s" x)) params " "))
      (delete-region (match-beginning 0) (match-end 0))
      (if (or (not file)
	      (not (file-exists-p file))
	      (not (file-readable-p file)))
	  (insert (format "CANNOT INCLUDE FILE %s" file))
	(when markup
	  (if (equal (downcase markup) "src")
	      (setq start (format "#+begin_src %s %s\n"
				  (or lang "fundamental")
				  (or switches ""))
		    end "#+end_src")
	    (setq start (format "#+begin_%s %s\n" markup switches)
		  end  (format "#+end_%s" markup))))
	(insert (or start ""))
	(insert (org-get-file-contents (expand-file-name file) prefix prefix1))
	(or (bolp) (newline))
	(insert (or end ""))))))

(defun org-get-file-contents (file &optional prefix prefix1)
  "Get the contents of FILE and return them as a string.
If PREFIX is a string, prepend it to each line.  If PREFIX1
is a string, prepend it to the first line instead of PREFIX."
  (with-temp-buffer
    (insert-file-contents file)
    (when (or prefix prefix1)
      (goto-char (point-min))
      (while (not (eobp))
	(insert (or prefix1 prefix))
	(setq prefix1 nil)
	(beginning-of-line 2)))
    (buffer-string)))

(defun org-get-and-remove-property (listvar prop)
  "Check if the value of LISTVAR contains PROP as a property.
If yes, return the value of that property (i.e. the element following
in the list) and remove property and value from the list in LISTVAR."
  (let ((list (symbol-value listvar)) m v)
    (when (setq m (member prop list))
      (setq v (nth 1 m))
      (if (equal (car list) prop)
	  (set listvar (cddr list))
	(setcdr (nthcdr (- (length list) (length m) 1) list)
		(cddr m))
	(set listvar list)))
    v))

(defun org-symname-or-string (s)
  (if (symbolp s)
      (if s (symbol-name s) s)
    s))

;;; Fontification and line numbers for code examples

(defvar org-export-last-code-line-counter-value 0)

(defun org-export-replace-src-segments-and-examples (backend)
  "Replace source code segments with special code for export."
  (setq org-export-last-code-line-counter-value 0)
  (let ((case-fold-search t)
	lang code trans opts)
    (goto-char (point-min))
    (while (re-search-forward
	    "\\(^#\\+BEGIN_SRC:?[ \t]+\\([^ \t\n]+\\)\\(.*\\)\n\\([^\000]+?\n\\)#\\+END_SRC.*\\)\\|\\(^#\\+BEGIN_EXAMPLE:?\\(?:[ \t]+\\(.*\\)\\)?\n\\([^\000]+?\n\\)#\\+END_EXAMPLE.*\\)"
	    nil t)
      (if (match-end 1)
	  ;; src segments
	  (setq lang (match-string 2)
		opts (match-string 3)
		code (match-string 4))
	(setq lang nil
	      opts (match-string 6)
	      code (match-string 7)))

      (setq trans (org-export-format-source-code-or-example
		   backend lang code opts))
      (replace-match trans t t))))

(defvar htmlp)  ;; dynamically scoped
(defvar latexp)  ;; dynamically scoped

(defun org-export-format-source-code-or-example (backend
						 lang code &optional opts)
  "Format CODE from language LANG and return it formatted for export.
If LANG is nil, do not add any fontification.
OPTS contains formatting optons, like `-n' for triggering numbering lines,
and `+n' for continuing previous numering.
Code formatting according to language currently only works for HTML.
Numbering lines works for all three major backends (html, latex, and ascii)."
  (save-match-data
    (let (num cont rtn rpllbl keepp textareap cols rows fmt)
      (setq opts (or opts "")
	    num (string-match "[-+]n\\>" opts)
	    cont (string-match "\\+n\\>" opts)
	    rpllbl (string-match "-r\\>" opts)
	    keepp (string-match "-k\\>" opts)
	    textareap (string-match "-t\\>" opts)
	    cols (if (string-match "-w[ \t]+\\([0-9]+\\)" opts)
		     (string-to-number (match-string 1 opts))
		   80)
	    rows (if (string-match "-h[ \t]+\\([0-9]+\\)" opts)
		     (string-to-number (match-string 1 opts))
		   (org-count-lines code))
	    fmt (if (string-match "-l[ \t]+\"\\([^\"\n]+\\)\"" opts)
		    (match-string 1 opts)))
      (when (and textareap (eq backend 'html))
	;; we cannot use numbering or highlighting.
	(setq num nil cont nil lang nil))
      (if keepp (setq rpllbl 'keep))
      (setq rtn code)
      (when (equal lang "org")
	(setq rtn (with-temp-buffer
		    (insert rtn)
		    ;; Free up the protected lines
		    (goto-char (point-min))
		    (while (re-search-forward "^," nil t)
		      (replace-match "")
		      (end-of-line 1))
		    (buffer-string))))
      ;; Now backend-specific coding
      (cond
       ((eq backend 'docbook)
	(setq rtn (org-export-number-lines rtn 'docbook 0 0 num cont rpllbl fmt))
	(concat "\n#+BEGIN_DOCBOOK\n"
		(org-add-props (concat "<programlisting><![CDATA["
				       rtn
				       "]]>\n</programlisting>\n")
		    '(org-protected t))
		"#+END_DOCBOOK\n"))
       ((eq backend 'html)
	;; We are exporting to HTML
	(when lang
	  (require 'htmlize nil t)
	  (when (not (fboundp 'htmlize-region-for-paste))
	    ;; we do not have htmlize.el, or an old version of it
	    (setq lang nil)
	    (message
	     "htmlize.el 1.34 or later is needed for source code formatting")))

	(if lang
	    (let* ((mode (and lang (intern (concat lang "-mode"))))
		   (org-inhibit-startup t)
		   (org-startup-folded nil))
	      (setq rtn
		    (with-temp-buffer
		      (insert rtn)
		      (if (functionp mode)
			  (funcall mode)
			(fundamental-mode))
		      (font-lock-fontify-buffer)
		      (set-buffer-modified-p nil)
		      (org-export-htmlize-region-for-paste
		       (point-min) (point-max))))
	      (if (string-match "<pre\\([^>]*\\)>\n?" rtn)
		  (setq rtn (replace-match
			     (format "<pre class=\"src src-%s\">\n" lang)
			     t t rtn))))
	  (if textareap
	      (setq rtn (concat
			 (format "<p>\n<textarea cols=\"%d\" rows=\"%d\" overflow-x:scroll >\n"
				 cols rows)
			 rtn "</textarea>\n</p>\n"))
	    (with-temp-buffer
	      (insert rtn)
	      (goto-char (point-min))
	      (while (re-search-forward "[<>&]" nil t)
		(replace-match (cdr (assq (char-before)
					  '((?&."&amp;")(?<."&lt;")(?>."&gt;"))))
			       t t))
	      (setq rtn (buffer-string)))
	    (setq rtn (concat "<pre class=\"example\">\n" rtn "</pre>\n"))))
	(unless textareap
	  (setq rtn (org-export-number-lines rtn 'html 1 1 num
					     cont rpllbl fmt)))
	(concat "\n#+BEGIN_HTML\n" (org-add-props rtn '(org-protected t)) "\n#+END_HTML\n\n"))
       ((eq backend 'latex)
	(setq rtn (org-export-number-lines rtn 'latex 0 0 num cont rpllbl fmt))
	(concat "\n#+BEGIN_LaTeX\n"
		(org-add-props (concat "\\begin{verbatim}\n" rtn "\n\\end{verbatim}\n")
		    '(org-protected t))
		"#+END_LaTeX\n\n"))
       ((eq backend 'ascii)
	;; This is not HTML or LaTeX, so just make it an example.
	(setq rtn (org-export-number-lines rtn 'ascii 0 0 num cont rpllbl fmt))
	(concat "#+BEGIN_ASCII\n"
		(org-add-props
		    (concat
		     (mapconcat
		      (lambda (l) (concat "  " l))
		      (org-split-string rtn "\n")
		      "\n")
		     "\n")
		    '(org-protected t))
		"#+END_ASCII\n"))))))

(defun org-export-number-lines (text backend
				     &optional skip1 skip2 number cont
				     replace-labels label-format)
  (if (and (not number) (not (eq replace-labels 'keep)))
      (setq replace-labels nil)) ;; must use names if no numbers
  (setq skip1 (or skip1 0) skip2 (or skip2 0))
  (if (not cont) (setq org-export-last-code-line-counter-value 0))
  (with-temp-buffer
    (insert text)
    (goto-char (point-max))
    (skip-chars-backward " \t\n\r")
    (delete-region (point) (point-max))
    (beginning-of-line (- 1 skip2))
    (let* ((last (org-current-line))
	   (n org-export-last-code-line-counter-value)
	   (nmax (+ n (- last skip1)))
	   (fmt (format "%%%dd:  " (length (number-to-string nmax))))
	   (fm
	    (cond
	     ((eq backend 'html) (format "<span class=\"linenr\">%s</span>"
					 fmt))
	     ((eq backend 'ascii) fmt)
	     ((eq backend 'latex) fmt)
	     ((eq backend 'docbook) fmt)
	     (t "")))
	   (label-format (or label-format org-coderef-label-format))
	   (label-pre (if (string-match "%s" label-format)
			  (substring label-format 0 (match-beginning 0))
			label-format))
	   (label-post (if (string-match "%s" label-format)
			   (substring label-format (match-end 0))
			 ""))
	   (lbl-re
	    (concat 
	     ".*?\\S-.*?\\([ \t]*\\("
	     (regexp-quote label-pre)
	     "\\([-a-zA-Z0-9_]+\\)"
	     (regexp-quote label-post)
	     "\\)\\)"))
	   ref)

      (goto-line (1+ skip1))
      (while (and (re-search-forward "^" nil t) (not (eobp)) (< n nmax))
	(if number
	    (insert (format fm (incf n)))
	  (forward-char 1))
	(when (and (not (eq replace-labels 'keep))
		   (looking-at lbl-re))
	  (setq ref (match-string 3))
	  (if replace-labels
	      (progn
		(delete-region (match-beginning 1) (match-end 1))
		(push (cons ref n) org-export-code-refs))
	    (goto-char (match-beginning 2))
	    (delete-region (match-beginning 2) (match-end 2))
	    (insert "(" ref ")")
	    (push (cons ref (concat "(" ref ")")) org-export-code-refs))
	  (when (eq backend 'html)
	    (save-excursion
	      (beginning-of-line 1)
	      (insert (format "<span id=\"coderef-%s\" class=\"coderef-off\">"
			      ref))
	      (end-of-line 1)
	      (insert "</span>")))))
      (setq org-export-last-code-line-counter-value n)
      (goto-char (point-max))
      (newline)
      (buffer-string))))

(defun org-search-todo-below (line lines level)
  "Search the subtree below LINE for any TODO entries."
  (let ((rest (cdr (memq line lines)))
	(re org-todo-line-regexp)
	line lv todo)
    (catch 'exit
      (while (setq line (pop rest))
	(if (string-match re line)
	    (progn
	      (setq lv (- (match-end 1) (match-beginning 1))
		    todo (and (match-beginning 2)
			      (not (member (match-string 2 line)
					  org-done-keywords))))
					; TODO, not DONE
	      (if (<= lv level) (throw 'exit nil))
	      (if todo (throw 'exit t))))))))

;;;###autoload
(defun org-export-visible (type arg)
  "Create a copy of the visible part of the current buffer, and export it.
The copy is created in a temporary buffer and removed after use.
TYPE is the final key (as a string) that also select the export command in
the `C-c C-e' export dispatcher.
As a special case, if the you type SPC at the prompt, the temporary
org-mode file will not be removed but presented to you so that you can
continue to use it.  The prefix arg ARG is passed through to the exporting
command."
  (interactive
   (list (progn
	   (message "Export visible: [a]SCII  [h]tml  [b]rowse HTML [H/R]uffer with HTML  [D]ocBook  [x]OXO  [ ]keep buffer")
	   (read-char-exclusive))
	 current-prefix-arg))
  (if (not (member type '(?a ?\C-a ?b ?\C-b ?h ?D ?x ?\ )))
      (error "Invalid export key"))
  (let* ((binding (cdr (assoc type
			      '((?a . org-export-as-ascii)
				(?\C-a . org-export-as-ascii)
				(?b . org-export-as-html-and-open)
				(?\C-b . org-export-as-html-and-open)
				(?h . org-export-as-html)
				(?H . org-export-as-html-to-buffer)
				(?R . org-export-region-as-html)
				(?D . org-export-as-docbook)
				(?x . org-export-as-xoxo)))))
	 (keepp (equal type ?\ ))
	 (file buffer-file-name)
	 (buffer (get-buffer-create "*Org Export Visible*"))
	 s e)
    ;; Need to hack the drawers here.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-drawer-regexp nil t)
	(goto-char (match-beginning 1))
	(or (org-invisible-p) (org-flag-drawer nil))))
    (with-current-buffer buffer (erase-buffer))
    (save-excursion
      (setq s (goto-char (point-min)))
      (while (not (= (point) (point-max)))
	(goto-char (org-find-invisible))
	(append-to-buffer buffer s (point))
	(setq s (goto-char (org-find-visible))))
      (org-cycle-hide-drawers 'all)
      (goto-char (point-min))
      (unless keepp
	;; Copy all comment lines to the end, to make sure #+ settings are
	;; still available for the second export step.  Kind of a hack, but
	;; does do the trick.
	(if (looking-at "#[^\r\n]*")
	    (append-to-buffer buffer (match-beginning 0) (1+ (match-end 0))))
	(while (re-search-forward "[\n\r]#[^\n\r]*" nil t)
	  (append-to-buffer buffer (1+ (match-beginning 0))
			    (min (point-max) (1+ (match-end 0))))))
      (set-buffer buffer)
      (let ((buffer-file-name file)
	    (org-inhibit-startup t))
	(org-mode)
	(show-all)
	(unless keepp (funcall binding arg))))
    (if (not keepp)
	(kill-buffer buffer)
      (switch-to-buffer-other-window buffer)
      (goto-char (point-min)))))

(defun org-find-visible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(get-char-property s 'invisible)))
    s))
(defun org-find-invisible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(not (get-char-property s 'invisible))))
    s))

(defvar org-archive-location)  ;; gets loaded with the org-archive require.
(defun org-get-current-options ()
  "Return a string with current options as keyword options.
Does include HTML export options as well as TODO and CATEGORY stuff."
  (require 'org-archive)
  (format
   "#+TITLE:     %s
#+AUTHOR:    %s
#+EMAIL:     %s
#+DATE:      %s
#+DESCRIPTION: 
#+KEYWORDS: 
#+LANGUAGE:  %s
#+OPTIONS:   H:%d num:%s toc:%s \\n:%s @:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
#+OPTIONS:   TeX:%s LaTeX:%s skip:%s d:%s todo:%s pri:%s tags:%s
%s
#+EXPORT_SELECT_TAGS: %s
#+EXPORT_EXCLUDE_TAGS: %s
#+LINK_UP:   %s
#+LINK_HOME: %s
#+CATEGORY:  %s
#+SEQ_TODO:  %s
#+TYP_TODO:  %s
#+PRIORITIES: %c %c %c
#+DRAWERS:   %s
#+STARTUP:   %s %s %s %s %s
#+TAGS:      %s
#+FILETAGS:  %s
#+ARCHIVE:   %s
#+LINK:      %s
"
   (buffer-name) (user-full-name) user-mail-address
   (format-time-string (substring (car org-time-stamp-formats) 1 -1))
   org-export-default-language
   org-export-headline-levels
   org-export-with-section-numbers
   org-export-with-toc
   org-export-preserve-breaks
   org-export-html-expand
   org-export-with-fixed-width
   org-export-with-tables
   org-export-with-sub-superscripts
   org-export-with-special-strings
   org-export-with-footnotes
   org-export-with-emphasize
   org-export-with-timestamps
   org-export-with-TeX-macros
   org-export-with-LaTeX-fragments
   org-export-skip-text-before-1st-heading
   org-export-with-drawers
   org-export-with-todo-keywords
   org-export-with-priority
   org-export-with-tags
   (if (featurep 'org-jsinfo) (org-infojs-options-inbuffer-template) "")
   (mapconcat 'identity org-export-select-tags " ")
   (mapconcat 'identity org-export-exclude-tags " ")
   org-export-html-link-up
   org-export-html-link-home
   (or (ignore-errors
	 (file-name-sans-extension
	  (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))))
       "NOFILENAME")
   "TODO FEEDBACK VERIFY DONE"
   "Me Jason Marie DONE"
   org-highest-priority org-lowest-priority org-default-priority
   (mapconcat 'identity org-drawers " ")
   (cdr (assoc org-startup-folded
	       '((nil . "showall") (t . "overview") (content . "content"))))
   (if org-odd-levels-only "odd" "oddeven")
   (if org-hide-leading-stars "hidestars" "showstars")
   (if org-startup-align-all-tables "align" "noalign")
   (cond ((eq org-log-done t) "logdone")
	 ((equal org-log-done 'note) "lognotedone")
	 ((not org-log-done) "nologdone"))
   (or (mapconcat (lambda (x)
		    (cond
		     ((equal '(:startgroup) x) "{")
		     ((equal '(:endgroup) x) "}")
		     ((cdr x) (format "%s(%c)" (car x) (cdr x)))
		     (t (car x))))
		  (or org-tag-alist (org-get-buffer-tags)) " ") "")
   (mapconcat 'identity org-file-tags " ")
   org-archive-location
   "org file:~/org/%s.org"
   ))

;;;###autoload
(defun org-insert-export-options-template ()
  "Insert into the buffer a template with information for exporting."
  (interactive)
  (if (not (bolp)) (newline))
  (let ((s (org-get-current-options)))
    (and (string-match "#\\+CATEGORY" s)
	 (setq s (substring s 0 (match-beginning 0))))
    (insert s)))

(defvar org-table-colgroup-info nil)

(defun org-table-clean-before-export (lines &optional maybe-quoted)
  "Check if the table has a marking column.
If yes remove the column and the special lines."
  (setq org-table-colgroup-info nil)
  (if (memq nil
	    (mapcar
	     (lambda (x) (or (string-match "^[ \t]*|-" x)
			     (string-match
			      (if maybe-quoted
				  "^[ \t]*| *\\\\?\\([\#!$*_^ /]\\) *|"
				"^[ \t]*| *\\([\#!$*_^ /]\\) *|")
			      x)))
	     lines))
      (progn
	(setq org-table-clean-did-remove-column nil)
	(delq nil
	      (mapcar
	       (lambda (x)
		 (cond
		  ((string-match  "^[ \t]*| */ *|" x)
		   (setq org-table-colgroup-info
			 (mapcar (lambda (x)
				   (cond ((member x '("<" "&lt;")) :start)
					 ((member x '(">" "&gt;")) :end)
					 ((member x '("<>" "&lt;&gt;")) :startend)
					 (t nil)))
				 (org-split-string x "[ \t]*|[ \t]*")))
		   nil)
		  (t x)))
	       lines)))
    (setq org-table-clean-did-remove-column t)
    (delq nil
	  (mapcar
	   (lambda (x)
	     (cond
	      ((string-match  "^[ \t]*| */ *|" x)
	       (setq org-table-colgroup-info
		     (mapcar (lambda (x)
			       (cond ((member x '("<" "&lt;")) :start)
				     ((member x '(">" "&gt;")) :end)
				     ((member x '("<>" "&lt;&gt;")) :startend)
				     (t nil)))
			     (cdr (org-split-string x "[ \t]*|[ \t]*"))))
	       nil)
	      ((string-match "^[ \t]*| *[!_^/] *|" x)
	       nil) ; ignore this line
	      ((or (string-match "^\\([ \t]*\\)|-+\\+" x)
		   (string-match "^\\([ \t]*\\)|[^|]*|" x))
	       ;; remove the first column
	       (replace-match "\\1|" t nil x))))
	   lines))))

(defun org-export-cleanup-toc-line (s)
  "Remove tags and timestamps from lines going into the toc."
  (when (memq org-export-with-tags '(not-in-toc nil))
    (if (string-match (org-re " +:[[:alnum:]_@:]+: *$") s)
	(setq s (replace-match "" t t s))))
  (when org-export-remove-timestamps-from-toc
    (while (string-match org-maybe-keyword-time-regexp s)
      (setq s (replace-match "" t t s))))
  (while (string-match org-bracket-link-regexp s)
    (setq s (replace-match (match-string (if (match-end 3) 3 1) s)
			   t t s)))
  s)

(defun org-create-multibrace-regexp (left right n)
  "Create a regular expression which will match a balanced sexp.
Opening delimiter is LEFT, and closing delimiter is RIGHT, both given
as single character strings.
The regexp returned will match the entire expression including the
delimiters.  It will also define a single group which contains the
match except for the outermost delimiters.  The maximum depth of
stacked delimiters is N.  Escaping delimiters is not possible."
  (let* ((nothing (concat "[^" left right "]*?"))
	 (or "\\|")
	 (re nothing)
	 (next (concat "\\(?:" nothing left nothing right "\\)+" nothing)))
    (while (> n 1)
      (setq n (1- n)
	    re (concat re or next)
	    next (concat "\\(?:" nothing left next right "\\)+" nothing)))
    (concat left "\\(" re "\\)" right)))

(defvar org-match-substring-regexp
  (concat
   "\\([^\\]\\)\\([_^]\\)\\("
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(\\(?:\\*\\|[-+]?[^-+*!@#$%^_ \t\r\n,:\"?<>~;./{}=()]+\\)\\)\\)")
  "The regular expression matching a sub- or superscript.")

(defvar org-match-substring-with-braces-regexp
  (concat
   "\\([^\\]\\)\\([_^]\\)\\("
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
   "\\)")
  "The regular expression matching a sub- or superscript, forcing braces.")


(defun org-get-text-property-any (pos prop &optional object)
  (or (get-text-property pos prop object)
      (and (setq pos (next-single-property-change pos prop object))
	   (get-text-property pos prop object))))

(defun org-export-get-coderef-format (path desc)
  (save-match-data
    (if (and desc (string-match
		   (regexp-quote (concat "(" path ")"))
		   desc))
	(replace-match "%s" t t desc)
      "%s")))

(provide 'org-exp)

;; arch-tag: 65985fe9-095c-49c7-a7b6-cb4ee15c0a95

;;; org-exp.el ends here
