This  directory  contains  various  tests.  To  build them with GNAT Ada
compiler use the following command line: 

1. Under Windows, assuming that C:/GtkAda is the installation  directory
   of GtkAda: 

gnatmake -Ic:/gtkada/include/gtkada -I.. -I../2.18.0 <test-file>.adb -largs -lgio-2.0 -mwindows

Change the directory 2.14.2 to 2.14.0 if you don't have one of these.

2. Under Linux:

gnatmake -I.. -I../2.18.0 <test-file>.adb `gtkada-config`

Change  the  directory to 2.14.0, 2.14.2, 2.18.0 according to the GtkAda
version you have.  Add -lgio-2.0 and/or -ldl if these are not referenced
by gtkada-config. 

To build test_gtk_source_view you need GtkSourceView:

gnatmake -I.. -I../2.18.0 test_gtk_source_view.adb `gtkada-config` -lgtksourceview-2.0

Note that GNAT GPL 2011 is used on more recent systems that have Gnome3,
you must have gtk2 installed. Sometimes you would have to do:

export FONTCONFIG_PATH=/etc/fonts
LD_LIBRARY_PATH=/usr/local/lib:/usr/gnat/lib/gps:$LD_LIBRARY_PATH <program>

to be able to run applications built with GtkAda.

To use with GNAT GPS a gpr file is provided. You might need to modify it
to reflect your system settings.

Note that the following tests require the strings  edit  library  (which
can be found at http://www.dmitry-kazakov.de/ada/strings_edit.htm): 

   test_gtk_directory_browser.adb
