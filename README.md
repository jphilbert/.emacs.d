.emacs.d
========

My Emacs settings

Install
-------
Download and extract the binary zip, emacs-XX.X-bin-i386.zip, to C:\.  Note that the archive file typically contains the root emacs-XX.X, however I typically rename this to simply EMACS yielding a cleaner install (any additional versions I keep the number and rename the directory when I toggle between).  In either case, the binary directory (i.e. C:\EMACS\BIN ) is required to be added to the system path. 

System Variables
----------------
Apart from the system path, EMACS looks for two additional variables.  First is HOME, which should be set to the user’s home directory (C:\USER\USERNAME on windows 7).  This is an alias for Linux’s  ‘~’ directory and also where the .emacs.d directory (customization) should be located.  Although EMACS nows supports site-wide configuration (via site-lisp in the EMACS program root directory), it is not recommended since multiple locations would need to be maintain.

The second variable is ALTERNATE_EDITOR, which should be set to runemacs.exe.   This defines an alternate editor when emacs client fails to find a server.

Associating EMACS
-----------------
To associate EMACS as an editor of a particular file use emacsclientw.exe -n "%1".   To create an ‘Open with Emacs’ 
A registry file has been included to facilitate this.  It does the following:
  - Create the registry key: HKEY_CLASSES_ROOT\*\shell\openwemacs\command
  - Set the default value to:  C:\EMACS\BIN\emacsclientw.exe -n "%1"


Aspell
------
Installing Aspell is done in two trivial steps:  First the application (aspell-X.XX-X-X-setup.exe) must be downloaded and installed, then the word list (aspell-en-X.XX-X-X.exe) .  It is also strongly suggested that it’s binary directory (C:\Program Files\ASPELL\BIN\) be added to the system path.
