# UBEE512 Launcher

## Why?
+ ubee512 is an excellent emulator, with a myriad of options available through the setting file (rc) and command line parameters.  But you have to be familiar with these.
+ ubee512Launcher instead allows inexperienced users (me) to easily view & select the available emulated systems defined in the setting file (and there's a lot of them)
+ Yeah, it's also for users (again, me) who are more comfortable with UI than command line.

## Release v0.2 (beta)
+ First beta release, lots to do yet
+ [Link to precompiled binaries](bin)
+ To use:
  + If ubee512 is on your environment path, simply download relevant binary and run.
  + if ubee512 isn't on your environment path, either
    + run ubee512Launcher, open "File" - "Settings", and configure the loctions for "ubee512 executable" and "ubee512 rc (setting file)"
    + or drop the binary in the same folder as the ubee512 binary, and run from there
  + Then:
    + Use the "Type", "Model" and "Title" dropdowns, select the emulated Microbee you wish to run
    + Use the Folder tree view to find some dsk files.  Select a bootable DSK, and click "Add DSK to A"
    + Click "Launch"

## Implemented functionality:
+ Allows user to choose a Microbee configuration using System Macros defined in ubee512rc
+ Allows user to select DSK files, and mount these in A: B: or C:
+ Launches a new instance of the **UBEE512** emulator using the chosen configuration

## Notes on implementation
+ Only allows bootable DSKs to be loaded in A:
+ There's no other validation happening right now:
  + You can select a System macro that defines a disk for A:, and then define your own disk for A:.  I have no idea what the result will be.
  + You can select System Macros that won't run because you don't have ROMs, SRAMs, PAKs or DSKs loaded into the correct ubee512 subfolder
+ Has a "File Preview": for now, either shows contents of text files, or files inside a DSK (you need to load the patched **CPMTools** using "File" - "Settings" for the DSK listing to work)
+ Contents of DSK listed using either original **CPMtools** or patched **CPMTools** (allows for Microbee specific formats)
+ Parses ubee512rc to build up System Macros, uses these in Main Form & Macro Explorer
+ I know there looks like a lot of macros defined in ubee512.  Err, I'm currently filtering out about half - the ones I haven't researched

# TODO
## Short term
+ Report which System Macros are usable (ROMs, DSKs, SRAM in correct folders)
+ Add awareness of **RunCPM** folder structure

## Medium term
+ Continue to add support for working with DSK files prior to running in a CP/M system
  + Create blank DSK
  + DSK To/From folder (inc To/From **RunCPM** folder)
+ Add direct support for the zip file structure utilised by **@ChickenMan** on both MSPP forum and MicrobeeTechnology forum
  + Display embedded ReadMe in the Preview Pane
  + Implement a "Mount ZIP" that extracts the DSK, then actually mounts that DSK in the CP/M system
+ Add support for Microbee Peripherals (Beetalker etc)
+ Add support for the assorted Harddrive formats recognised by uBee512
+ Add support for **UBEEDISK** tools (to be honest, this means learning them first)
+ I keep thinking about adding ability for users to define their own system macros (by first copying an existing).

## Long Term 
(_dreamer! you're nothing but a dreamer_)
+ Investigate automating a running instance of **UBEE512** (allow dynamic changing of disks)
+ I strongly suspect there are issues in **CPMTools**.  Investigate using this framework, then report to appropriate developers. (Hmm, looks like original CP/M tools are up to 2.23, but the patched CP/M tools are from 2.1 codebase)

# Development Notes
+ Developed under windows using Lazarus 2.2.6 / fpc 3.2.2 / 64bit
+ Tested under Ubuntu using Lazarus 3.0 / fpc 3.2.2 / 64bit
+ Uses sections directly copied from an OO framework I developed for other projects. That OO framework adds uncessary complications, as the intention for this project is to keep the design to a simplified application, only the support units were added.   + This uses the LCL for UI.  Means the project can be compiled for linux or other OS's.  Primary development is being done under Windows.
+ LCL and FPC are both quickly evolving projects.  I've an ongoing project rationalising my support units.  Several of the routines in the Support folder were developed many years ago, filling voids in the then LCL/FPC.  If anyone stumbles across routines I can deprecate in favour of official LCL/FPC, let me know.

## Distribution
+ **UBEE512**, **UBEEDISK**, patched **CPMTools**, original **CPMTools** & **RunCPM** are NOT distributed with this application.  You'll have to download these separetely (see Acknowledgements)
+ The app will look for these on your environment path.  If they're not on the path, you'll need to open "File" - "Settings" and set the appropriate paths manually.

## Screenshots
+ We have "minimum viable product", just not platform independent yet
+ ![Image: Main UI](Images/Development_Screenshot_2.png)
+ ![Image: System Macros](Images/Development_Screenshot_1.png)

# Acknowledgements
+ User **uBee** on the MSPP forum (https://www.microbee-mspp.org.au/forum/) is to be congratulated.  **uBee** has developed a very flexible Microbee emulator that is able to emulate all Microbee flavours and is configurable via the rc file and the command line. 
+ **UBEE512**, **UBEEDISK** & patch for **CPMTools** developed by user **@uBee**.
+ **UBEE512**, **UBEEDISK** & patch for **CPMtools** (2.1) can be obtained from the MSPP repository: https://www.microbee-mspp.org.au/repository/
+ Original **CPMTools** (2.23) can be obtained from: http://www.moria.de/~michael/cpmtools/.  This doesn't support most Microbee disk formats.
+ Where the above tools access DSK files, they do so using **libdsk** either directly (**CPMTools**) or optionally (**UBEE512**) : https://www.seasip.info/Unix/LibDsk/.  
+ **UBEE512** has been forked (there was limited support for MacOS), and ongoing development & friendly support is happening in this discord: https://discord.gg/2rBya9Hh
+ **RunCPM** can be obtained from: https://github.com/MockbaTheBorg/RunCPM
+ Prebuilt **Lazarus/fpc** can be obtained from: https://sourceforge.net/projects/lazarus/files/
+ But really - you know you want to compile **Lazarus** trunk: https://github.com/User4martin/lazarus
+ And finally - a massive shout out to everyone on the Lazarus forums and mailing lists.  Very helpful and knowledgable, the lot of them: https://forum.lazarus.freepascal.org/index.php

# License
This is intended to be free code available to everyone for use everywhere.  Frankly, this is a simple UI wrapping the more complicated emulators and CPMTools developed and released by others.  However, I'm aware there are issues with releasing code under that sort of vagueness, so I've got to have a quick look around for an appropriate open source license.  Haven't done that yet...

Mike Thompson
mike.cornflake@gmail.com
5 Jan 2024
