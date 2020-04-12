# My emacs settings

**Configuration**

Take a look at [configuration.org](configuration.org) and [init.el](init.el)

**Setup**

1. git clone this repo into home directory

	On **Windows** 10, it's nice to set HOME to something like "C:\Users\%USERNAME%" if it isn't already set
	
	On **Linux**, this is just ~/
	
    Navigate to the home directory and...
    
	`git clone --recursive https://github.com/Jawabiscuit/.emacs.d.git`

3. Print status
	
	Output should include status of all submodules

    `git status`
	`git submodule status --recursive`

4. Run emacs!

    *Windows* .bat:
    
    - Activate python (I typically use Anaconda, just have Python in the environment)
    - Set a HOME directory on Windows
    - MiKTeX for LaTeX export and highlighting of org files. It's not strictly necessary
    - Instead of `runemacs` I setup aliases on windows `em` and `eml`
    
    ```
    @echo off

    call activate python27
    
    set HOME=C:\Users\%USERNAME%
    set PATH="C:\Program Files\MiKTeX 2.9\miktex\bin\x64";%PATH%
    "C:\Program Files (x86)\emacs\bin\runemacs.exe"
    ```
        
    *Linux*:
    
    `em` or `em -l .emacs` for project specific settings

5. Updating *site-lisp*

	all
	
	`git submodule update --recursive --remote`

	individual
	
	`git submodule update --remote site-lisp/use-package`

