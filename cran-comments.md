## R CMD check results
The package was built on MacOs 10.15.7 Catalina. There were no ERRORs, WARNINGs nor NOTEs. 

## Environnements
I tested the installation with devtools::check_win_release() without any ERRORs, WARNINGs. 
There was 1 NOTE:
* checking CRAN incoming feasibility ... NOTE

I tested rhub::check() on  Debian Linux, R-devel, clang, without ERRORs, WARNINGs nor NOTEs

I tested rhub::check() on Oracle Solaris 10, x86, 32 bit, R-release with one ERRORs:
* Package required but not available: 'kableExtra'

With devtools::check_rhub(), the following NOTEs came up with Windows Server 2022, R-devel, 64 bit:
* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

There was no ERRORs, WARNINGs nor NOTEs with Ubuntu Linux 20.04.1 LTS, R-release, GCC and Fedora Linux, R-devel, clang, gfortran.

## Resubmission

This is a resubmission. In this version I have:

* added \value to .Rd files regarding exported methods and explained the functions results in the documentation for 
- anaBiv-data.frame-character-method.Rd
- anaBiv-listVar-character-method.Rd
- ft_ana_biv.Rd
- ft_desc_tab.Rd
- ft_parse.Rd
- parseClassFun-parseClass-method.Rd
* Wrote about the structure of the output (class) and also what the output means. 
