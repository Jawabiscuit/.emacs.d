rem
rem Aliases
rem https://stackoverflow.com/questions/20530996/aliases-in-windows-command-prompt
rem executing this file using autorun registry key does not play nicely with vcvarsall.bat
rem

DOSKEY ls=dir /b
DOSKEY em=start "" /min emacs $*
DOSKEY eml=start "" /min emacs -l .emacs $*

