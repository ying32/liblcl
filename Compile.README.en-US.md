* [中文](Compile.README.md)   
* English     

----

### liblcl 

liblcl Based on Lazarus 2.2 version FPC 3.2.2, the specific installation method of each platform refers to the official website installation instructions.   

The liblcl project source code is located in the "liblcl/src" directory. `liblcl for Windows, Linux, MacOS`.   

Compile steps:    

* 1. Download and install [Lazarus](https://www.lazarus-ide.org/index.php?page=downloads) of the corresponding platform.
* 2. (Pre-installed) [Third Party Control](src/3rd-party/README.en-US.md) required for installation.
* 3. double-click liblcl.lpi.  
* 4. Menu -> Project -> Project Options -> Compiler Options -> Build modes Switch the relevant ring mode. The current valid mode is as follows:  
   * Win32  
   * Win64  
   * Linux32 **(Untested)**
   * Linux64  
   * LinuxARM **(Untested)** 
   * LinuxARM64 **(Untested)**
   * MacOS64(cocoa)
   * MacOS M1 **(Untested)**
  
* 5. menu -> Run-> Build or Shift + F9 (Must use Build)   

The compiled binary can be viewed in the following directory:        

**Note: This directory is my local compilation directory, please modify the corresponding output location if you compile it yourself.**
Output liblcl location modification: `Menu -> Project -> Project Options -> Compiler Options -> Build modes Switch the relevant ring mode -> Target file name`  

> Windows: `"..\liblcl"`     
> Linux: `"../liblcl"`  
> MacOS: `"$(HOME)liblcl"`
