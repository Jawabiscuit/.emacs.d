# My emacs settings

**Setup**

1. git clone this repo into home directory

	On **Windows** 10, it's easiest to set HOME to something like "C:\Users\%USERNAME%" if it isn't already set
	
	On **Linux**, this is just ~/
	
	`git clone https://github.com/Jawabiscuit/.emacs.d.git`

2. Initialize the repo

	This step is necessary for automatically detecting and downloading package dependencies upon starting emacs for the first time on a machine.
	
	`cd .emacs.d`
	`git submodule update init --remote --recursive`
	`cd ..`

3. Print status
	
	Output should include status of any submodules

	`git submodule status --recursive`

4. Run emacs!

    *Windows* .bat:
     
    ```
    @echo off

    call activate python27
    
    set HOME=C:\Users\%USERNAME%
    set PATH="C:\Program Files\MiKTeX 2.9\miktex\bin\x64";%PATH%
    "C:\Program Files (x86)\emacs\bin\runemacs.exe"
    ```
    
    I have MiKTeX for LaTeX export and highlighting of org files. It's not strictly necessary.
    
    *Linux*:
    
    `em` or `em -l .emacs` for project specific settings

5. Updating *site-lisp*

	all
	
	`git submodule update --recursive --remote`

	individual
	
	`git submodule update --remote site-lisp/use-package`

