This  directory contains XPM images to Ada converter. To build them with
GNAT Ada compiler use the following command line: 

1. Under Windows, assuming that C:/GtkAda is the installation  directory
   of GtkAda: 

gnatmake -Ic:/gtkada/include/gtkada -I.. -I<simple-components-directory>\parser-examples\xpm xpm2gtkada.adb -largs -mwindows

2. Under Linux:

gnatmake -I.. -I<simple-components-directory>/parser-examples/xpm xpm2gtkada.adb `gtkada-config`

Alternatively use the GPR-file provided.
