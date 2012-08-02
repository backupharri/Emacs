Emacs
=====

In order to syn all the code for emacs in win, mac, linux

If you want to be able to use all the github function,
You have to add .ssh folder in the [Github Folder]/Emacs.
Otherwise, the git push will fail.


In Windows, to put
(load-file "[Github Folder]/Emacs/.emacs");
in the ~/.emacs file.

In Unix-like OS, to create symbol link:
ln -s [Github Folder]/Emacs/.emacs ~/.emacs
ln -s [Github Folder]/Emacs/.emacs.d/ ~/.emacs.d/
test clone
