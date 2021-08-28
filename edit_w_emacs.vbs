' -----------------------------------------------------------------------------
' VB script for editing in Emacs from Windows Explorer
'	specifically designed for opening the file in a frame and invisibly
'
' USE: "wscript.exe \"%~d0%~p0edit_w_emacs.vbs\" \"%%1\""
' i.e. wscript.exe "<PATH>edit_w_emacs.vbs" "%1"
' -----------------------------------------------------------------------------
 
file = Replace(WScript.Arguments(0), "\", "/")
' Path to emacs executables
emacs_path = "C:\EMACS\bin\"
' Path to server file (default is .emacs\server\server)
server_path = "..\backups\server"

quote = """"

' Executable 
exec = quote & emacs_path & "emacsclientw.exe" & quote

' Alternate Editor Option
opAltEditor = " -a=" & quote & emacs_path & "runemacs.exe" & quote

' Evaluate open file in another frame
opEval = " -e " & quote & "(find-file-other-frame \" & quote & file & "\" & quote & ")" & quote

' Use specific server path option
opServer = " -f " & quote & server_path & quote

exec = exec & " -n" & opEval & opAltEditor & opServer
CreateObject("WScript.Shell").Run exec, 0, False
