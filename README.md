# Project emacs settings

## Example Usage - Clone Into Project as a Submodule

1. `cd` into project git repo
 
	`cd project`

2. Create a submodule out of this repo

	`git submodule add https://github.com/Jawabiscuit/.emacs.d.git`
	
	***Errors?***
	
	[Easy way to pull latest of all git submodules](https://stackoverflow.com/a/1032653)
	
	[How do I replace a git submodule with another repo?](https://stackoverflow.com/q/14404704)

3. Initialize it

	`cd .emacs.d`
	`git submodule update --init --recursive`
	`cd ..`

4. Print status - should see `.emacs.d` + any submodules

	`git submodule status --recursive`

5. On windows create a .bat to run emacs with the new settings

	`@echo off`
	`"C:\Program Files (x86)\emacs\bin\runemacs.exe" -l p:\project\.emacs.d\init.el`
