# vu_datenanalyse_students
Student files for the course "VU Datenanalyse" at UIBK


## Setting up the workspace

Students who have not done work in R before will need to set up their computer to work with this course.

1. [Install R](https://cran.r-project.org)
2. (Windows only) [Install RTools](https://cran.r-project.org)
3. Install [RStudio](https://posit.co/downloads/), an open-source working environment for R

## Installing Git
(Optional, but recommended)

Having a git installation will allow you to work with the course directly through RStudio, including keeping up-to-date on any changes to the course. It will simplify your work greatly.

### Windows
1. Download and install [git](https://git-scm.com/downloads)
2. Go to the Windows menu, type cmd and hit enter. At the prompt, type `where git`, it will give you a path, like `C:/Program Files (x86)/Git/bin/git.exe`. Copy this path to the clipboard
3. Open RStudio, go to the Tools menu, choose Global Options. Choose git/svn on the sidebar. Check the box labelled "Enable version control interface for RStudio projects". In the `Git executable` box, paste the path from step 2.
4. Restart Rstudio.

### Mac
1. Open `Terminal`, located in the Utilities folder in Applications; or hit command-space, type `Terminal` and press return.
2. At the prompt, type `which git`. This will return a path, probably `/usr/bin/git/`. Copy this to the clipboard.
3. Open RStudio, go to the Rstudio menu, choose Preferences. Choose git/svn on the sidebar. Check the box labelled "Enable version control interface for RStudio projects". In the `Git executable` box, paste the path from step 2.
4. Restart Rstudio.

### Linux
1. Open a terminal window.
2. At the prompt, type `which git`. This will return a path, something like `/usr/bin/git/`. Copy this to the clipboard.
3. Open RStudio, go to the Tools menu, choose Global Options. Choose git/svn on the sidebar. Check the box labelled "Enable version control interface for RStudio projects". In the `Git executable` box, paste the path from step 2.
4. Restart Rstudio.

## Getting the course materials

1. In RStudio, open the `File` menu and choose `New Project`.
2. Select `Version Control`, then `Git`.
3. For `Repository URL`, enter https://github.com/flee-group/vu_datenanalyse_students.git
4. Project directory name will be filled automatically
5. For "Create project as a subdirectory as", hit browse, then choose where on your computer to save the course files. Rstudo will create a new folder called `vu_datenanalyse` wherever you choose.
6. Any time you want to resume working on the course, go to the folder you selected in step 5, open the `vu_datenanalyse_students` folder, and open the `vu_datenanalyse_students.Rproj` file. This will open Rstudio and set it up to work with the course files.