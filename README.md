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

5. Updating

	all
	
	`git submodule update --recursive --remote`

	individual
	
	`git submodule update --remote site-lisp/use-package`

