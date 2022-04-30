git pull
Copy-Item -Path config.org -Destination C:\Users\Justin\Documents\emacs\.config.org
Copy-Item -Path .emacs -Destination C:\Users\Justin\Documents\emacs\.emacs
robocopy snippets C:\Users\Justin\Documents\emacs\.emacs.d\snippets /MIR
