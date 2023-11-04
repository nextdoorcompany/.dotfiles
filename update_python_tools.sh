~/venv/pipx3.12/bin/python -m pip install -U pip pipx
pipx upgrade-all
~/venv/batteries3.12/bin/pip-compile --generate-hashes --strip-extras --upgrade ~/venv/batteries3.12/requirements.in
~/venv/batteries3.12/bin/pip-sync ~/venv/batteries3.12/requirements.txt
