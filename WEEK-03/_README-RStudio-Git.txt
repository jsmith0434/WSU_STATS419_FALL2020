# RStudio and Github ... Windows 10 
# Rmarkdown with long functions ... 
# Learn how to integrate Knit ... Rnotebooks, and output Latex PDF within RStudio
# _git_ ... where code lives ... C:\
# _R-data_ ... where data lives ... R:\


# RGui ... RStudio ... 64-bit ... 
# Git Bash ...

# source ... custom external R files ... functions-file.R ... functions-imdb.R ... functions-matrix.R

# github ... primary email:  monte.shaffer@gmail.com with username:  MonteShaffer

# ssh key ... 
ssh-keygen -t rsa -b 4096 -C "monte.shaffer@gmail.com"		
# (/c/Users/Alexander Nevsky/.ssh/id_rsa)

ssh-agent -s

ssh -vT git@github.com
ssh -T git@github.com

# https://github.com/MonteShaffer/WSU_STATS419_FALL2020.git


git clone https://github.com/MonteShaffer/WSU_STATS419_FALL2020.git

cd WS # ... hit tab to autocomplete

# https://git-scm.com/docs/git-add
# git add .
# git add --all
git add -A

git status

git commit -m "Adding personality-raw dataset"

git push -u origin master

git remote set-url origin git@github.com:MonteShaffer/WSU_STATS419_FALL2020.git

git push -u origin master 

mkdir directornynakl # make directory
touch fjidljasdkl    # create empty file
	
## make changes to the folder ...
 git add .
 git status
 git commit -m "basic common functions"
 git push -u origin master 

# Rnotebook - sandbox to play
# Rmarkdown - writeup for submission (final product)

# Latex ... environment ...
# Miktex
	
	
