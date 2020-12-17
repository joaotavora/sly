Upcoming version
----------------

SLY 1.0.42 (December 2020)
------------------------------

### Much improved company completion

If you haven't yet, just `M-x package-install RET company-mode`, to
enable `company`.  It should start working in every SLY buffer.

Moreover, a new "lazy" way to SLY collect completions from the Lisp
backend means that user input is not blocked if this takes a long
time.  The result is a much more responsive on-the-fly completion
experience.

If you don't like company, you can still use the normal on-request TAB
completion, which also features an improved UI.

### Fixed Helm conflicts

Helm users should now get access to the full capabilities of SLY's
completion function without needing to install any extra packages.
Just need to `M-x sly-symbol-completion-mode` to turn off SLY's
default completion UI.  (https://github.com/joaotavora/sly/issues/303)

### Redesigned completion backend

A redesigned completion backend replaces the legacy contribs
`sly-fuzzy` and `sly-c-p-c`. The new "flex" backend considers package
designators in the search string so you can complete other package's
symbols even when outside a package.

Here's an example:

```
CL-USER> (quiloa)         ->  (ql:quickload)
CL-USER> (scan)           ->  (ppcre:scan)
CL-USER> (setf locadirs)  ->  (setf ql:*local-project-directories*)
CL-USER> (mvbind)         ->  (multiple-value-bind)
```

Flex completion is on by default, but that can be changed via
`sly-complete-symbol-function`. The
[documentation](http://joaotavora.github.io/sly/#Completion) has more
information.

Package-local nicknames are also considered (provided your
implementation supports them).

### Completely rewritten manual

Manual has been reviewed from top to bottom. It should be much easier
to navigate and nicer read in general, thouch it *still needs quite
some proofreading*

[A new chapter for existing SLIME users](http://joaotavora.github.io/sly/#A-SLY-tour-for-SLIME-users)
figures prominently in the top-level.

Presently no major omissions (*except maybe for multiple inspectors*).
Reorganized nodes into a new structure not so focused on core vs
contribs. Deleted stale sections. REPL section heavily rewritten to
explain output types and backreferences.

### New command M-x sly-import-symbol-at-point bound to C-c i by default

This is a counterpart to the existing M-x sly-export-symbol-at-point,
bound to C-c e. It strips the symbol under point of its package
designator, and adds it under the :import-from subclause of the
current package's defpackage form. An improvement suggested and
implemented by Alexander Artemenko (github #147).

### Improved SLY Stickers UI

The UI for SLY Stickers, particularly for `M-x sly-stickers-replay`
has been cleanup up. Upon entering the replay mode, SLY may ask user
if she wants to delete the previous recordings, a commonly forgotten
but useful pre-step (an idea by Javier Olaechea, github #91). Also,
window management is less random.

### Stale buffers are killed on connection close

Inspector, debugger, and other buffers pertaining to a connection are
automatically killed when the connection is closed.  The types of
buffers that are exempt from this is configurable in a new
`sly-keep-buffers-on-connection-close` defcustom, which lists
REPL buffers by default.

### Autodoc function args available in minibuffer prompts

In minibuffer prompts like the one of `M-x sly-inspect`, autodoc
information is available and displayed temporarily in the mode-line.

### `C-c C-o` and `C-c M-o` bound in the REPL

These clear recent output and the whole REPL, respectively.

### Easy to parse copied calls to REPL

The "Copy call to REPL" funcionality in the Trace Dialog and SLY-DB
buffers now understands that symbols and numbers can be printed
directly in the reconstructed function call, which eases reading.

### The .swankrc and .swank.lisp are not loaded by default

A problem encountered by Zach Beane.

### Window management with M-. has been fixed

When finding the definition of a generic function with M-., an *xref*
buffer pops up in a (possibly reused) window, showing code
locations. When selecting one of those locations, make sure to honor
the original intent of M-. of switching buffers in the original
window. Quit the *xref* window should fully restore the previous
window configuration. Thanks to Michael Fiano for insisting on what is
indubitably the correct behavior (github #123).

### More predictably choose REPL windows

Calling `sly-mrepl` interactively, either via `M-x` or
 `sly-selector-map` switches to buffer in the current
 window. Accessing the REPL indirectly (by returning objects into it
 form other modes) attempts to pop the buffer in another window.

A problem encoutered by Zach Beane. 

### Travis CI infrastructure has been revamped

The Travis CI infrastructure now uses `roswell` and `emacs-travis`
instead of `cl-travis` and `apt`, respectively. Thus no longer needs
Travis's `sudo` and uses directory caching, resulting in much faster
builds.

### New variables `*SLYNK:ECHO-NUMBER-ALIST*` and `*SLYNK:PRESENT-NUMBER-ALIST*`

Customize the formats that integer numbers get presented back to SLY,
additionally to the binary, octal and hex defaults. Good when working
with Unix Epoch times, for example. See manual "Other configurables"
for documentation and example.

### New customization variable `sly-command-switch-to-existing-lisp`

This makes a prefixless `M-x sly` switch to an active connection
instead of starting a new one.  Customize this to `never` to never
prompted, or to `always` to always move to the first live connection.

### Loading contribs is a more robust process

If the Slynk-side of a contrib fails to load for whatever
reason, a CONTINUE restart is provided. On the SLY side, the user
can choose to disable that contrib temporarily.

### A lot of cleanup to SLY sources

Courtesy of Zach Shaftel <zshaftel@gmail.com>, sly-cl-indent.el no
longer clobbers or conflicts with Emacs's own cl-indent.el.  The cl.el
library is no longer required, not even at compile-time or in tests.
A lot of cleanup and performance improvements.

### On par with SLIME 2.26

Where applicable, SLY tracks bug-fixes and improvements contributed to
SLIME:

- More secure handling of ~/.sly-secret files

- Compatibility with the latest SBCL and older SBCL.

- ECL backend now supports threads

- Function `create-server` now accepts optional `interface` argument.

- In SBCL, slynk can be bound to IPv6 interface and can work on IPv6-only machines.

- Clasp/ABCL improvements.

SLY 1.0.0-beta-2 (March 2016)
-----------------------------

### Improved `sly-stickers` contrib:

There is now updated documentation for this contrib. See
http://joaotavora.github.io/sly/#SLY-Stickers

Added a menu to help discover the functionality. 

Use `M-x sly-stickers-toggle-break-on-stickers` to turn on stepping
for forms. The debugger pops up with useful restarts and interactive
buttons.

The "replay" interface, `M-x sly-stickers-replay` has been cleaned up.

### Three new independent SLY contribs

In https://github.com/joaotavora/sly-named-readtables there is an
external contrib that adds support for Tobias C Rittweiler's
`EDITOR.HINTS-NAMED.READTABLES`. Available on MELPA.

In https://github.com/joaotavora/sly-macrostep there is a SLY
contrib for expanding CL macros right inside the source
file. Available on MELPA.

In https://github.com/joaotavora/sly-quicklisp there is a SLY
contrib for expanding CL macros right inside the source
file. Available on MELPA.

These contribs also showcase how third-party contribs with both Elisp
and CL parts can be written independently of SLY. See
https://github.com/joaotavora/sly-hello-world for how to write such
contribs.

### Apropos

Argument list information is present in apropos output like
this. A suggestion of Javier Olaechea (github #53)

```
...
SLYNK:TO-LINE
  Function: Print OBJECT to a single line. Return the string.
  Arglist: (OBJECT &OPTIONAL WIDTH)
...  
```
  
`M-- sly-apropos` prompts for just the package. A suggestion of Javier
Olaechea (github #53).

`C-u sly-apropos` allows searching all packages (github #53)

### Macroexpansion

Discovery of the "sexp near point" has been much improved and won't
error cryptically when there is no macroexpansion possible.

### REPL enhancements

`paredit-mode` works in the REPL, as does Emacs 24.4's
`electric-pair-mode` or other parenthesis-matching tools, just like in
any other Lisp buffer.

New variable `sly-mrepl-output-filter-functions` for REPL
output. These work like `comint-preoutput-filter-functions`. Functions
like `ansi-color-apply` are good candidates (github #38).

When using multiple REPL, frame variables from SLY-DB triggered in
secondary REPLs, when returned with M-RET, appear in the correct REPL.

mREPL notes are synched displayed correctly at the "process mark", not
"output mark". This avoids some `; note` in wrong
places (github #45).

Popping up SLY-DB buffers from different threads no longer
misteriously updates a REPL's environment for `*`, `**`, `***`, etc...

Tearing down a reverse-isearch with `C-g` no longer errors (github
\#39).

### Manual

The "Tips and tricks" section was rewritten.

Keymap documentation was corrected and enhanced by Javier Olaechea
(github #36).

### Other

The thread-list buffer can now be properly quit. Reported by Javier
Olaechea (github #51).

Let user ignore protocol mismatches per connection This issue popped
up in SLIME, where it is still unsolved. See discussion in
https://github.com/slime/slime/issues/250

`view-mode` bindings no longer creep into SLY's popup
buffers. By Paul M. Rodriguez.

`sly-inspect-fetch-all` now actually does something (github #49).

`sly-expand-1` and friends error less often and less cryptically when
no nearby sexp's can be found. The region about to be macroexpanded is
flashed for visual feedback.

### On par with SLIME 2.14 and 2.15

Where applicable, SLY tracks bugfixes and improvements contributed to
SLIME:

- Rationals are displayed in the echo area as floats too

- The sly-c-p-c contrib now takes a better guess at symbol case
  (issue https://github.com/slime/slime/issues/233)

- SBCL backend now able to jump to ir1-translators, declaims and alien types

- Various updates supporting SBCL 1.2.12

- ABCL backend fixed inspection of frame-locals in the debugger

- Following a suggestion by Attile Lendvai to SLIME, mouse-1 is now
activates SLY's "part buttons".

- SBCL's M-. can locate forms within PROGN/MACROLET/etc. Needs SBCL 1.2.15

### On par with upcoming SLIME 2.16

- SBCL and ABCL support completion of package-local nicknames

- Robustify the indentation cache request handler
  (issue https://github.com/slime/slime/issues/280)


SLY 1.0.0-beta (March 2015)
----------------------------------------

### mREPL enhancements

In the REPL, a reader macro allows easy backreferences to previously
returned values. `#v3` will access the first value of the fourth
values-form ever returned and `#v3:2` will access the third of those
values. Backreferences are automatically highlighted.

New customization option `sly-mrepl-eli-like-history-navigation` makes
history navigation via `C-r`, `M-p` and `M-n` keep the current input
and add the history input at point.

### New commands `sly-next-connection` and `sly-prev-connection`

Replace deprecated `sly-cycle-connections` and provide more feedback.

### On par with SLIME 2.13

Where applicable, SLY tracks bugfixes and improvements contributed to
SLIME:

- Experimental CLASP support (see `slynk/backend/clasp.lisp`).

- Fix a source-path-parsing bug. A problem raised and solved by Gabor
  Mélis in http://permalink.gmane.org/gmane.lisp.slime.devel/11543.

- Hot updating via MELPA on won't cause SLYNK/SLY version mismatch

  SLIME issue [125](https://github.com/slime/slime/issues/125)

- Allegro's "modern mode" is supported again.

- Port conflicts can be resolved interactively

  SLIME issue [204](https://github.com/slime/slime/issues/204)

- New `SLYNK-FUZZY:*FUZZY-DUPLICATE-SYMBOL-FILTER*` option.

  SLIME issue [215](https://github.com/slime/slime/issues/215)

- `sly-recompile-xrefs` has been fixed.

- More minor issues.

SLY 1.0.0-alpha-3 (December 2014)
---------------------------------

### sly-stickers: live source-code annotation tool

`sly-stickers` is a live code annotation tool, a replacement for the
"print" statements that are often included for debugging the value of
a particular variable or expression. See a much more complete
description in http://joaotavora.github.io/sly/#SLY-Stickers.

Thanks to Artem Malyshev for early testing and ideas.

### Documentation rewrite

The documentation rewrite is underway (github issue #9), mentioning
only a residual amount of SLIME-specific functionality. Browse to
http://joaotavora.github.io/sly for the online version in HTML.

### SLY is available in MELPA

The README.md file has an updated install recipe, basically `M-x
package-install RET sly RET`. Thanks to Kan-Ru Chen for the idea.

### mREPL enhancements

The "comma" shortcut for "sayoonara", "restart lisp", "in-package"
commands has been restored and uses ido-style completion by
default. Suggested by Javier Olaechea (github #18).

`C-c C-z` switches to the nearest REPL, much like `sly-mrepl-sync`,
but without changing the REPL's state. Suggested by Javier Olaechea
(github #13).

`sly-mrepl-sync` is now bound to `C-c ~` in any SLY buffer. A `C-u`
prefix will also yank the last expression into the current REPL.

New customization variable `sly-mrepl-prevent-duplicate-history`
offers finer control over saved history.

When a connection disconnects, a status line is inserted in the REPL
stating the reason for disconnection (network failure, user abort,
etc...)

### Other enhancements

SLY asks the user to confirm the Lisp to kill with `M-x sly-quit` or
disconnect with `M-x sly-disconnect`. It also doesn't ask any
irrelevant questions about switching to a default connection when it
doesn't exist (github #5).

### Notable bugfixes

* Closed github #26: `sly-mrepl` history saved when killing
  Emacs. Reported by Javier Olaechea.

* Closed github #24: Common-lisp-style indentation now works in
  `sly-mrepl`. Reported by Kan-Ru Chen.

* Closed github #22: Compilation with `C-u` debug info now
  working. Reported by Javier Olaechea.

* Closed github #21: Enable incremental output with dedicated output
  streams. Reported by Paul M. Rodriguez.

* Closed github #5: `sly-quit` no longer asks irrelevant questions

### Swank is now called Slynk

Slynk is SLY's implementation of Swank, the supporting Lisp server
where Emacs connects to. A project-wide rename was performed,
including this NEWS.md file.

A `sly-retro` contrib, enabled by default ensures that:

* SLY can still talk to non-lisp Swank backends
* SLY can serve requests for the Swank protocol
* Most user customization in `~/.swankrc` is still valid in SLY.

For details on the architecture of these changes, see the "Swank is
now called Slynk" in the CONTRIBUTING.md.

Thanks to Zach Beane for the great name.
    
### On par with SLIME 2.10.1

Where applicable, SLY tracks bugfixes and improvements contributed to
SLIME:

- `sly-autodoc` has been rewritten by Helmut Eller. Multiline is
  enabled by default.
- Experimental support for MKCL has been added


SLY 1.0.0-alpha (September 2014)
--------------------------------

Since this is the first pre-release of SLY since the fork, this entry
focuses directly on the differences to SLIME.

### Completely redesigned REPL

The `sly-mrepl` contrib is a extensively redesigned
Read-Eval-Print-Loop (REPL) for SLY.

Multiple independent REPLs can be created with the `sly-mrepl-new`
command.

`sly-mrepl` is fully based on Emacs's `comint.el` and as such has a
more familiar interface for history navigation. `C-r` and `C-M-r`,
when used at the prompt, should provide a bash/zsh-like experience.

The prompt gives a visual indication of long-running evaluations that
haven't returned yet.

The prompt gives a visual indication of the number of debugger levels
currently caused by the last evaluation.

Every return part can be inspected and re-returned as the last value.

`C-c ~` on any Lisp file switches to the REPL and synchronized its
`*PACKAGE*` and `*DEFAULT-PATHNAME-DEFAULTS*` with the file's.

Output redirection is automatically setup. The first REPL created is
the target for all threads' output onto the standard output
streams. REPLs created afterward only see their own output. To turn
this off configure the Slynk-side variable
`SLYNK-MREPL:*GLOBALLY-REDIRECT-IO*`.

A dedicated stream connection for output is automatically set
up. Configure the `SLYNK-MREPL:*USE-DEDICATED-OUTPUT-STREAM*` if it
doesn't suit you.

There is saner scrolling behavior as provided by the `comint.el`
substrate. The variables `comint-scroll-show-maximum-output`,
`comint-scroll-to-bottom-on-input` and
`comint-scroll-to-bottom-on-output` (which see) are set to `nil` by
default, but the user might reconfigure them to her liking in the
`sly-mrepl-hook`.

There are Sylvesters. See `sly-mrepl-pop-sylvester`.

### Regexp-capable M-x sly-apropos

If SLY detects that [`cl-ppcre`](http://weitz.de/cl-ppcre/) is
available in the Lisp side it will try to use it for "apropos"
searches, otherwise the user is hinted at this possibility. As regexp
searches are slower, this is only triggered if the pattern is a valid
regular-expression.

This is the default implementation of the new
`SLYNK-BACKEND:MAKE-APROPOS-MATCHER` interface that particular
implementations may wish to override.

The search pattern, whether regexp-enabled or not, is now also applied
to the package qualifier.

### Contribs enabled by default

By default, SLY enables the `sly-fancy` meta-contrib. This contains
`sly-mrepl`, `sly-autodoc`, `sly-fancy-inspector`, `sly-fancy-trace`,
`sly-fuzzy`, `sly-scratch`, `sly-package-fu`, `sly-fontifying-fu`,
`sly-trace-dialog`, `sly-indentation` and `sly-tramp`.

### SLY uses ASDF and loads contribs on demand.

If the user sets `sly-contribs` to `sly-mrepl` she can be sure that no
Lisp code related to other contribs appears in your run-time. Even if
ASDF is unavailable, an improved version of the `slynk-loader.lisp`
program will behave in a similar non-intrusive manner.

This change also enables developers to write completely independent
third-party extensions like
[in this example](http://github.com/joaotavora/sly-hello-world).

See the CONTRIBUTING.md file for more details on architecture changes.

### More consistent interface

The SLY-DB, Inspector, XREF and Apropos buffers have been
redesigned to use a common class of push button with consistent
interfaces. These context-aware commands are known as "part actions"

For instance, the `i`,`p` and `M-RET` commands (`sly-button-inspect`,
`sly-button-pretty-print` and `sly-mrepl-copy-to-repl`) are available
for every interactive part, regardless of the SLY buffer in
question. A command such as `v` (`sly-button-show-source`) is only
available where it makes sense for the represented object.

The same interfaces are also available in the "mREPL" and "Trace
Dialog" buffers.

`sly-mode` is now activated in every buffer related to SLY is now,
meaning global keybindings like `C-c T` and `C-c I` work everywhere.

### Multiple inspectors

Interactive commands for inspecting Lisp objects can be prefixed with
`C-u` to prompt the user for an inspector name. Separate inspector
streams are kept. An idea by Olof-Joachim Frahm
(http://macrolet.net/).

### Copy function call to REPL

An experimental feature: from the Trace Dialog or SLY-DB buffers, a
new button action called "Copy call to REPL" is offered and bound to 

If SLY can calculate the arguments and the function symbol of the
function call represented in the backtrace of trace entry, it will
return them to the REPL, along with an uncommitted input line that
refers back to them and calls the function.

### Other miscellaneous enhancements over SLIME

Faces have been revised and are based on Emacs's standard
faces. Hopefully, SLY will remain readable even when the user
switches themes.

Popping up windows and buffers has been much improved. Windows are
reused if the buffer names match or are similar, meaning that no
longer will the SLY-DB "jump around" in multi-window configurations when
selecting a restart that signals another error.

Interactive expression evaluation will use a separate buffer when the
results is too big to fit in the echo area.

SLY's mode-line string is placed at the right side of the mode-line.

SLY intrudes less than SLIME in the Emacs name-space, and uses more
standard idioms. Macros like `with-struct` and `sly-define-keys` have
been removed.

Buffer names have been consolidated: every buffer name related to SLY
obeys the same structure, stating the type, connection name and any
pertinent additional info.

Reading from the minibuffer has been improved. SLY uses `ido`
completion by default, but it can customized via
`sly-complete-symbol-function`.

Messages and warnings prefix themselves accordingly with "[sly]".

Fixed source locations when recompiling from an xref buffer.  This is
outstanding https://github.com/slime/slime/pull/175 pull-request in
SLIME. Thanks Bart Botta.

Fixed bugs in `contrib/sly-package-fu.el`. This is the outstanding
https://github.com/slime/slime/pull/145 pull-request in SLIME. Thanks
Leo Liu.

### On par with SLIME 2.9

Where applicable, SLY tracks bugfixes and improvements contributed to
SLIME.

### Anti-NEWS (Things removed from SLIME)

SLY 1.0-Alpha supports Emacs 24.3 only. SLY 1.0 is expected to only
support Emacs 24.4.

There is very limited backward compatibility SLIME and only in the
SLIME->SLY direction, meaning a contrib-less SLIME may connect to a
Slynk server started by SLY, but any other combination will probably
fail beyond very basic functionality.

The portable profiling commands have been removed from the SLY menu
`metering.lisp` and the profiling interfaces in `slynk-backend.lisp`
have been kept. SLY 1.0.0 will hopefully have a better integrated
portable profiler.

The `slime-presentations` has been removed. The consistent button
interface is better.

The `slime-c-p-c` contrib has been removed, as it contained a lot of
non-standard window-managing code. Its functionality has been merged
into `sly-fuzzy` and setting `sly-complete-symbol-function` to
`sly-c-p-c-complete-symbol` should give you the previous behavior.
