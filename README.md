# MichaelBee-Tools
# UBEE512 Launcher
For now:
+ Allows user to choose Microbee Configuration using System Macros
+ Allows user to select DSK files, and mount these in A: B: or C:
+ If the DSK is bootable, can be used in A:
+ Has a "File Preview": for now, either shows contents of text files, or files inside a DSK
+ Contents of DSK listed using either original CPMtools or modified CPMTools (allows for Microbee specific formats)

# Short term TODO
+ Parse ubee512rc to build up System Macros
+ Report which System Macros are usable (ROMs, DSKs, SRAM in correct folders)
+ Add Awareness of RunCPM folder structure

# Medium term TODO
+ continue to add support for working with DSK files prior to loading in a CP/M system
+ Create blank DSK
+ DSK To/From folder (inc To/From RunCPM folder)

# Long Term TODO (_dreamer! you're nothing but a dreamer_)
+ Investigate automating a running instance of RunCPM (serial? STDIN?)
+ Investigate automating a running instance of UBEE512 (allow dynamic changing of disks)
