git pull
Copy-Item -Path config.org -Destination C:\Users\Justin\Documents\emacs27\.config.org
Copy-Item -Path .emacs -Destination C:\Users\Justin\Documents\emacs27\.emacs
robocopy snippets C:\Users\Justin\Documents\emacs27\.emacs.d\snippets /MIR
