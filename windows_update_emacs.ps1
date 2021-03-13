git pull
Copy-Item -Path config.org -Destination C:\Users\Justin\Documents\emacs26\home\.config.org
Copy-Item -Path .emacs -Destination C:\Users\Justin\Documents\emacs26\home\.emacs
robocopy snippets C:\Users\Justin\Documents\emacs26\home\.emacs.d\snippets /MIR
