--                                                                    --
--  package Gtk.Main.Router         Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2006       --
--                                                                    --
--                                Last revision :  19:57 08 Aug 2015  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
--  This package  provides  a  synchronization  mechanism  to  use  GTK+
--  framework  with  Ada  tasks.  GTK+  is  known  to be task unsafe. In
--  particular  all  calls  need to be made from the same task (thread).
--  Further  GTK+ has callback-based architecture. For this reason it is
--  impossible to use Ada's entry points  for  synchronization,  because
--  callback subprograms cannot act as entries.
--
--  The  package  allows  a  task  which  needs  any  GTK+  action,   to
--  synchronize on a protected object. It waits for  GTK+  be  ready  to
--  serve them. An idle loop processing calls then the requested  action
--  on the context of the main GTK thread.  Upon action  completion  the
--  task continues  its  work.  For  the  task  it  appears  much  as  a
--  rendezvous with the GTK+ main thread.
--
--  The package provides three ways to request a procedure to be called
--  on the context of GTK+ main thread.
--
--  (o)  Extensible  tagged  type. The task that needs to call some GTK+
--       subroutines, derives from the type Request_Data. and places the
--       parameters there. It overrides Service with the calls needed to
--       be done. Then it  calls  to  Request  one  or  multiple  times.
--       Exceptions  raised in Service are propagated to the task called
--       Request. It  is  safe  to  use  this  Request  from  any  task,
--       including  one of the main GTK+ loop. In the later case Request
--       will not block.
--  (o)  Plain callback routine. The type Gtk_Callback is a pointer to a
--       parameterless   procedure.   The  procedure  Request  with  the
--       parameter  of  this  type  can be used to request a call to the
--       procedure on the context of the main GTK+  loop.  Similarly  to
--       the  variant  with a tagged type, it is safe to call Request on
--       any context.
--  (o)  Generic  package  Generic_Callback_Request  can be instantiated
--       with an appropriate type of callback  routine  parameters.  The
--       procedure  Request  defined  in  the  package acts like a plain
--       callback  routine  described above and also accepts a parameter
--       of the user-defined type.
--
-- The package also provides the procedure Say, which can be used to pop
-- a message box up. It can be called from any task.
--
-- Note the package is based on timer events.
--
with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Gtk.Dialog;             use Gtk.Dialog;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Label;              use Gtk.Label;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Window;             use Gtk.Window;

with Ada.Finalization;

package Gtk.Main.Router is

   Busy_Error : exception;
   Quit_Error : exception;

   type Gtk_Callback is access procedure;
--
-- Request_Data -- GTK+ request data
--
   type Request_Data is abstract
      new Ada.Finalization.Limited_Controlled with null record;
   type Request_Data_Ptr is access all Request_Data'Class;
--
-- Request -- To service on the GTK+ context
--
--    Data - The object containing the request data
--
-- This call is blocking. The caller will wait until the request will be
-- serviced through a call to Service on the GTK+ context.
--
-- Exceptions :
--
--    Program_Error - Init wasn't called
--    Quit_Error    - Main loop quitted
--    others        - Propagated out of Service
--
   procedure Request (Data : in out Request_Data'Class);
--
-- Service -- Servicing the request
--
--    Data - The object containing the request data
--
-- This  abstract procedure which any derived type should implement. All
-- GTK+  calls  shall be placed here. Note that Service is called on the
-- context of the main GTK+ thread.
--
   procedure Service (Data : in out Request_Data) is abstract;
--
-- Init -- GTK+ initialization
--
--    Window   - Main window for which the main loop is running
--    Period   - Of polling for requests to service
--    GPS_Port - The port to connect the GPS server
--
-- This  procedure should be called once from the main GTK+ thread after
-- Gtk.Main.Init. Usually  it  is  done  before  entering  the  loop  of
-- processing events and before  starting  tasks  which  might  use  the
-- package functionality. The parameter  GPS_Port  is  the  TCP/IP  port
-- number  used  by the GPS server. When available at the local host the
-- GPS server will browse the source file locations of the  stack  trace
-- routes  upon  the  corresponding menu selection when right clicked in
-- the trace window.
--
   procedure Init
             (  Window   : not null access Gtk_Window_Record'Class;
                Period   : Duration := 0.2;
                GPS_Port : Natural  := 50_000
             );
--
-- Request -- Callback on the GTK+ context
--
--    Service - The procedure to call
--
-- This  call  is  blocking. The caller will wait until Callback will be
-- called on the GTK+ context.
--
-- Exceptions :
--
--    Program_Error - Init wasn't called
--    others        - Propagated out of Service
--
   procedure Request (Service : Gtk_Callback);
--
-- Generic_Callback_Request
--
--    User_Data - The type of callback routine parameter
--
   generic
      type User_Data (<>) is limited private;
   package Generic_Callback_Request is
   --
   -- Callback_Procedure -- Pointer to callback procedures
   --
      type Callback_Procedure is
         access procedure (Data : not null access User_Data);
   --
   -- Request -- Callback on the GTK+ context
   --
   --    Service - The procedure to call
   --    Data    - The parameters of the call
   --
   -- This call is blocking. The caller will wait until Callback will be
   -- called on the GTK+ context.
   --
   -- Exceptions :
   --
   --    Program_Error - Init wasn't called
   --    Quit_Error    - Main loop quitted
   --    others        - Propagated out of Service
   --
      procedure Request
                (  Callback : Callback_Procedure;
                   Data     : not null access User_Data
                );
   private
      type Callback_Data (Parameters : not null access User_Data) is
         new Request_Data with
      record
         Callback : Callback_Procedure;
      end record;
      procedure Service (Data : in out Callback_Data);

   end Generic_Callback_Request;
--
-- Generic_Message
--
--    User_Data - The type of callback routine parameter
--
   generic
      type User_Data is private;
   package Generic_Message is
   --
   -- Handler_Procedure -- Pointer to handler procedures
   --
      type Handler_Procedure is
         access procedure (Data : in out User_Data);
   --
   -- Send -- Callback on the GTK+ context
   --
   --    Handler - The procedure to call
   --    Data    - The parameters of the call
   --    Timeout - The operation timeout
   --
   -- The  caller  can  be released before Handler is called on the GTK+
   -- context.
   --
   -- Exceptions :
   --
   --    Busy_Error    - Timed out
   --    Program_Error - Init wasn't called
   --    Quit_Error    - Main loop quitted
   --
      procedure Send
                (  Handler : Handler_Procedure;
                   Data    : User_Data;
                   Timeout : Duration := 0.5
                );
   private
      type Message_Data is new Request_Data with record
         Handler : Handler_Procedure;
         Data    : User_Data;
      end record;
      type Message_Data_Ptr is access all Message_Data;
      procedure Service (Data : in out Message_Data);

   end Generic_Message;
--
-- Say -- Message box
--
--    Message       - To show
--    Title         - Of the message box
--    Mode          - Of the dialog
--    Justification - Of the message text
--    Parent        - Of the box
--
-- This procedure showns a message box. It is safe to call it  from  any
-- task.
--
-- Exceptions :
--
--    Program_Error - Init wasn't called
--    Quit_Error    - Main loop quitted
--
   procedure Say
             (  Message       : UTF8_String;
                Title         : UTF8_String := "";
                Mode          : UTF8_String := Stock_Dialog_Info;
                Justification : Gtk_Justification := Justify_Left;
                Parent        : access Gtk_Widget_Record'Class := null
             );
--
-- Trace -- Messages trace box
--
--    Message - To show
--    Break   - Force break flag
--
-- This  procedure  adds  the  message  to  a  dialog box with a program
-- tracing  history.  When  the  "break"  check  button of the dialog is
-- active  or  else  when  the parameter Break is set to true, then each
-- call  to  Trace will block the caller until the user let it continue.
-- Pressing the "next" button continues until the next message. Pressing
-- the "record" button continues until the "break" button is not checked
-- by the user again.
--
-- Exceptions :
--
--    Program_Error - Init wasn't called
--    Quit_Error    - Main loop quitted
--
   procedure Trace
             (  Message : UTF8_String;
                Break   : Boolean := Standard.False
             );
--
-- Trace -- Messages trace box
--
--    Error - Exception
--    Break - Force break flag
--
-- This procedure is a shortcut for:
--
--    Trace (Exception_Information (Error), Break);
--
-- Exceptions :
--
--    Program_Error - Init wasn't called
--    Quit_Error    - Main loop quitted
--
   procedure Trace
             (  Error : Exception_Occurrence;
                Break : Boolean := Standard.True
             );

end Gtk.Main.Router;
