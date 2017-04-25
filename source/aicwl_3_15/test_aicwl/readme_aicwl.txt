This directory contains tests for Ada Industrial Control Widget  Library
packages described in the documentation. See 

   http://www.dmitry-kazakov.de/ada/aicwl.htm
   
To build them with GNAT Ada compiler use the following command line: 

1. Under Windows: 

gnatmake -Ic:/gtkada/include/gtkada -I.. -I../2.14.0 -I../cairoada -gnat05 test_aicwl.adb -largs -lcairo -lgdk-win32-2.0 -lgio-2.0 -lglib-2.0 -lgobject-2.0 -lgdk_pixbuf-2.0 -lpango-1.0 -lgtkada -mwindows
gnatmake -Ic:/gtkada/include/gtkada -I.. -I../2.14.0 -I../cairoada -gnat05 test_oscilloscope.adb -largs -lcairo -lgdk-win32-2.0 -lgio-2.0 -lglib-2.0 -lgobject-2.0 -lgdk_pixbuf-2.0 -lpango-1.0 -lgtkada -mwindows

2. Under Linux:

gnatmake -I.. -I../2.18.0 -I../cairoada test_aicwl.adb -gnat05 `gtkada-config`
gnatmake -I.. -I../2.18.0 -I../cairoada test_oscilloscope.adb -gnat05 `gtkada-config`

Change  the  directory to 2.14.0, 2.14.2, 2.18.0 according to the GtkAda
version you have. 
