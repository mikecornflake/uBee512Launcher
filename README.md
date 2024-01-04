# UBEE512 Launcher
For now:
+ Allows user to choose a Microbee configuration using System Macros
+ Allows user to select DSK files, and mount these in A: B: or C:
+ Launches a new instance of the **UBEE512** emulator using the chosen configuration

Other
+ Only allows bootable DSKs to be loaded in A:
+ Has a "File Preview": for now, either shows contents of text files, or files inside a DSK
+ Contents of DSK listed using either original **CPMtools** or modified **CPMTools** (allows for Microbee specific formats)
+ Parses ubee512rc to build up System Macros, uses these in Main Form

# TODO
## Short term
+ Report which System Macros are usable (ROMs, DSKs, SRAM in correct folders)
+ Add awareness of **RunCPM** folder structure
+ Add support for **UBEEDISK** tools (to be honest, this means learning them first)

## Medium term
+ Continue to add support for working with DSK files prior to running in a CP/M system
  + Create blank DSK
  + DSK To/From folder (inc To/From **RunCPM** folder)
+ Add direct support for the zip file structure utilised by **@ChickenMan** on both MSPP forum and MicrobeeTechnology forum
  + Display embedded ReadMe in the Preview Pane
  + Implement a "Mount ZIP" that extracts the DSK, then actually mounts that DSK in the CP/M system
+ Add support for Microbee Peripherals (Beetalker etc)
+ Add support for the assorted Harddrive formats recognised by Microbee

## Long Term 
(_dreamer! you're nothing but a dreamer_)
+ Investigate automating a running instance of **RunCPM** (serial? STDIN?)
+ Investigate automating a running instance of **UBEE512** (allow dynamic changing of disks)
+ I strongly suspect there are issues in **CPMTools**.  Investigate using this framework, then report to appropriate developers. (Hmm, looks like original CP/M tools are up to 2.23, but the modified CP/M tools are from 2.1 codebase)

# Development Notes
+ Developed under windows using Lazarus 2.2.6 / fpc 3.2.2 / 64bit
+ Testing under Ubuntu using Lazarus 3.0 / fpc 3.2.2 / 64bit (err, UI issues - really shouldn't use Anchor to pin components to the form)
+ Uses sections directly copied from an OO framework I developed for other projects. This includes the contents of the Support folder. That OO framework adds uncessary complications, so the intention for this project is to keep the design to a simplified application.    
+ This uses the LCL for UI.  Means the project can be compiled for linux or other OS's.  Primary development is being done under Windows.
LCL and FPC are both quickly evolving projects.  I've an ongoing project rationalising my support units.  Several of the routines in the Support folder were developed many years ago, filling voids in the then LCL/FPC.  If anyone stumbles across routines I can deprecate in favour of official LCL/FPC, let me know.

## Distribution
+ **UBEE512**, **UBEEDISK**, Modified **CPMTools**, original **CPMTools** & **RunCPM** are NOT distributed with this application.  You'll have to download these separetely (see Acknowledgements)
+ The app will look for these on your environment path.  If they're not on the path, you'll need to open "File" - "Settings" and set the appropriate paths manually.

## Screenshots
+ We have "minimum viable product", just not platform independent yet
+ ![Image: Main UI](Images/Development_Screenshot_2.png)
+ ![Image: System Macros](Images/Development_Screenshot_1.png?raw=true)

# Acknowledgements
+ **UBEE512**, **UBEEDISK** & modified **CPMTools** developed by user **@uBee** contactable on the MSPP forum (https://www.microbee-mspp.org.au/forum/).  Kudo's, uBee developed a very flexible emulator that is able to emulate all Microbee flavours and is configurable via the rc file and the command line. 
+ Original **UBEE512**, **UBEEDISK** & modified **CPMtools** (2.1) can be obtained from the MSPP repository: https://www.microbee-mspp.org.au/repository/
+ Original **CPMTools** (now up to 2.23) can be obtained from: http://www.moria.de/~michael/cpmtools/
+ Where the above tools access DSK files, they do so using **libdsk**: https://www.seasip.info/Unix/LibDsk/.  **LibDsk** doesn't have support for the Microbee Disk Formats, and this is why the modified **CPMTools** exists.  It's actually **libdsk** that's been modified and **CPMtools** rebuilt with the updated library.
+ **UBEE512** has been forked (there was limited support for MacOS), and ongoing development & friendly support is happening in this discord: https://discord.gg/2rBya9Hh
+ **RunCPM** can be obtained from: https://github.com/MockbaTheBorg/RunCPM
+ Prebuilt **Lazarus/fpc** can be obtained from: https://sourceforge.net/projects/lazarus/files/
+ But really - you know you want to compile **Lazarus** trunk: https://github.com/User4martin/lazarus
+ And finally - a massive shout out to everyone on the Lazarus forums and mailing lists.  Very helpful and knowledgable, the lot of them: https://forum.lazarus.freepascal.org/index.php

# License
This is intended to be free code available to everyone for use everywhere.  Frankly, this is a simple UI wrapping the more complicated emulators and CPMTools developed and released by others.  However, I'm aware there are issues with releasing code under that sort of vagueness, so I've got to have a quick around for an appropriate open source license.  Haven't done that yet...

Mike Thompson
2 Jan 2024
