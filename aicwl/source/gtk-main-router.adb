--                                                                    --
--  package Gtk.Main.Router         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2006       --
--                                                                    --
--                                Last revision :  10:25 19 Feb 2017  --
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
-- __________________________________________________________________ --

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Tags;
with Ada.Task_Identification;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Gdk.Color.IHLS;

with Glib.Main;
with Glib.Messages;
with Glib.Values;

with GNAT.Sockets;
with GNAT.Traceback.Symbolic;

with Gtk.Box;
with Gtk.Button;
with Gtk.Check_Button;
with Gtk.Clipboard;
with Gtk.Dialog;
with Gtk.Handlers;
with Gtk.Image;
with Gtk.Image_Menu_Item;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Missed;
with Gtk.Scrolled_Window;
with Gtk.Style_Context;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Tag;
with Gtk.Text_View;
with Gtk.Widget;

with Pango.Font;

with System.Storage_Elements;

package body Gtk.Main.Router is

   --
   -- Gateway_State -- The  state  of  the  protected  object   controlling
   --                  interaction between the main loop task  and  an  Ada
   -- task  requesting  servicing.  The following diagram illustrates state
   -- transitions upon a request.
   --
   --        main loop                   state       task
   --           :                          |           |
   --           :                         Idle         |__
   --        sleeping                      |              | Request_Service
   --           :                         Busy <--------- |
   --  timer -->|                          |    (data)  __|
   --           |__                        |           :
   --              | Initiate_Service      |           :
   --              | Service (data)        |        waiting
   --              | Complete_Service --> Ready        :
   --            __|                       |           :__
   --           |                          |              | Serviced
   --           :                         Idle <--------- |
   --        sleeping                      |           .__|
   --           :                          |           |
   --
   type Gateway_State is (Idle, Failed, Ready, Busy, Quitted);
   subtype Completed is Gateway_State range Idle .. Ready;

   Max_Recursion : constant                        := 100;
   GPS_Prompt    : constant String                 := "GPS>> ";
   GPS_Port      : Natural                         := 50_000;
   Connected     : Boolean                         := Standard.False;
   Recursion     : Natural                         := 0;
   Window_X      : Gint                            := 0;
   Window_Y      : Gint                            := 0;
   Parent        : Gtk.Window.Gtk_Window           := null;
   Main          : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Null_Task_Id;
   Trace_Dialog  : Gtk.Dialog.Gtk_Dialog;
   Channel       : GNAT.Sockets.Socket_Type;
   Command       : Ada.Strings.Unbounded.Unbounded_String;

   function Where (Text : String) return String is
   begin
      return " in Gtk.Main.Router." & Text;
   end Where;

   function Get_Parent (Window : Gtk.Window.Gtk_Window := null)
                        return Gtk.Window.Gtk_Window
   is
      use type Gtk.Window.Gtk_Window;
   begin
      if Window = null then
         return Parent;
      else
         return Window;
      end if;
   end Get_Parent;

   package Window_Callback is
     new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);
   procedure On_Quit (Window : access Gtk.Window.Gtk_Window_Record'Class);

   protected Gateway is
      procedure Abort_Service (Error : Ada.Exceptions.Exception_Occurrence);
      procedure Complete_Service;
      function Get_State return Gateway_State;
      function Get_Request_Info return String;
      entry Initiate_Service (Data : out Request_Data_Ptr);
      entry Request_Service
        (Data        : in out Request_Data'Class;
         Synchronous : Boolean);
      procedure Quit;
   private
      entry Serviced
        (Data        : in out Request_Data'Class;
         Synchronous : Boolean);
      Fault : Ada.Exceptions.Exception_Occurrence;
      State : Gateway_State := Idle;
      Call  : Boolean; -- True if active request is synchronous
      Data  : Request_Data_Ptr;
   end Gateway;
   --
   -- Callback -- Called from the main loop on timer events
   --
   function Callback return Boolean is
      Data : Request_Data_Ptr;
   begin
      loop
         Gateway.Initiate_Service (Data);
         exit when Data = null;
         begin
            if Recursion < Max_Recursion then
               Recursion := Recursion + 1;
               begin
                  Service (Data.all);
                  Recursion := Recursion - 1;
               exception
                  when others =>
                     Recursion := Recursion - 1;
                     raise;
               end;
            end if;
            Gateway.Complete_Service;
         exception
            when Error : others =>
               Gateway.Abort_Service (Error);
         end;
      end loop;
      return Standard.True;
   end Callback;

   protected body Gateway is

      procedure Abort_Service (Error : Ada.Exceptions.Exception_Occurrence) is
      begin
         if Call then
            State := Failed;
            Ada.Exceptions.Save_Occurrence (Fault, Error);
         else
            State := Idle;
         end if;
      end Abort_Service;

      procedure Complete_Service is
      begin
         if Call then
            State := Ready;
         else
            State := Idle;
         end if;
      end Complete_Service;

      function Get_State return Gateway_State is
      begin
         return State;
      end Get_State;

      function Get_Request_Info return String is
         function Info return String is
         begin
            return
              Ada.Tags.Expanded_Name (Data.all'Tag)
              & " at"
              & System.Storage_Elements.Integer_Address'Image
                  (System.Storage_Elements.To_Integer (Data.all'Address));
         end Info;
      begin
         case State is
            when Idle =>
               return "idle";
            when Ready =>
               if Data = null then
                  return "ready ";
               else
                  return "ready with " & Info;
               end if;
            when Busy =>
               if Data = null then
                  return "busy";
               else
                  return "busy on " & Info;
               end if;
            when Failed =>
               if Data = null then
                  return
                    "failed [" & Ada.Exceptions.Exception_Message (Fault) & "]";
               else
                  return
                    "failed [" & Ada.Exceptions.Exception_Message (Fault) &
                    "] on " & Info;
               end if;
            when Quitted =>
               return "quitted";
         end case;
      end Get_Request_Info;

      entry Initiate_Service
        (Data : out Request_Data_Ptr)
        when not (State in Completed and then
                  Request_Service'Count > 0) is
      begin
         if State = Busy then
            Data := Gateway.Data;
         else
            Data := null;
         end if;
      end Initiate_Service;

      procedure Quit is
      begin
         State := Quitted;
      end Quit;

      entry Request_Service
        (Data        : in out Request_Data'Class;
         Synchronous : Boolean) when State = Idle or else State = Quitted is
      begin
         if State = Quitted then
            raise Quit_Error;
         end if;
         Gateway.Data := Data'Unchecked_Access;
         Call  := Synchronous;
         State := Busy;
         if Synchronous then
            requeue Serviced;
         end if;
      end Request_Service;

      entry Serviced
        (Data        : in out Request_Data'Class;
         Synchronous : Boolean)
        when (State = Ready  or else
              State = Failed or else
              State = Quitted) is
      begin
         if State = Quitted then
            raise Quit_Error;
         elsif State = Failed then
            State := Idle;
            Ada.Exceptions.Reraise_Occurrence (Fault);
         else
            State := Idle;
         end if;
      end Serviced;

   end Gateway;

   package body Generic_Message is

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Message_Data,
           Message_Data_Ptr);

      package Sources is new Glib.Main.Generic_Sources (Message_Data_Ptr);

      function Service (Message : Message_Data_Ptr)
                        return Boolean is
         Ptr : Message_Data_Ptr := Message;
      begin
         Message.all.Handler (Message.all.Data);
         Free (Ptr);
         return Standard.False;
      exception
         when others =>
            Free (Ptr);
            return Standard.False;
      end Service;

      procedure Send
        (Handler : Handler_Procedure;
         Data    : User_Data;
         Timeout : Duration := 0.5)
      is
         Message : Message_Data_Ptr := new Message_Data;

         use type Ada.Task_Identification.Task_Id;
      begin
         Message.all.Handler := Handler;
         Message.all.Data    := Data;
         if Main = Ada.Task_Identification.Current_Task then
            declare
               ID : Glib.Main.G_Source_Id;
            begin
               ID := Sources.Idle_Add (Service'Access, Message);
            end;
         elsif Main = Ada.Task_Identification.Null_Task_Id then
            raise Program_Error;
         else
            select
               Gateway.Request_Service (Message.all, Standard.False);
            or delay Timeout;
               raise Busy_Error with
                 "Current state " & Gateway.Get_Request_Info;
            end select;
         end if;
      exception
         when others =>
            Free (Message);
            raise;
      end Send;

      overriding procedure Service (Data : in out Message_Data) is
         Message : Message_Data_Ptr := Data'Unchecked_Access;
      begin
         Message.all.Handler (Message.all.Data);
         Free (Message);
      exception
         when Error : others =>
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               Ada.Exceptions.Exception_Information (Error)
               & Where ("Generic_Message.Service"));
            Free (Message);
      end Service;

   end Generic_Message;
   --
   -- Connect -- To the GPS
   --
   -- The  procedure shows a dialog box while connecting to the GPS server.
   -- The  dialog box shows connection error message when connection fails.
   -- Waiting for connection can be canceled by closing the box.
   --
   procedure Connect is
      Dialog  : Gtk.Dialog.Gtk_Dialog;
      Message : Ada.Strings.Unbounded.Unbounded_String;
      Label   : Gtk.Label.Gtk_Label;

      use type Gtk.Dialog.Gtk_Dialog_Flags;
   begin
      Gtk.Dialog.Gtk_New
        (Dialog,
         "GPS at "
         &  GNAT.Sockets.Host_Name
         &  " port"
         &  Integer'Image (GPS_Port),
         Get_Parent,
         Gtk.Dialog.Destroy_With_Parent + Gtk.Dialog.Modal);
      Gtk.Label.Gtk_New (Label, "...connecting...");
      Gtk.Label.Set_Line_Wrap (Label, Standard.True);
      Gtk.Box.Pack_Start
        (Gtk.Dialog.Get_Content_Area (Dialog),
         Label,
         Standard.True,
         Standard.True);
      declare
         --
         -- An independent timer for a connection  task  to  communicate
         -- with  the  dialog  box.  It  must be independent because the
         -- default timer may be blocked at this point.
         --
         package Timers is new Glib.Main.Generic_Sources (Integer);

         --
         -- Connect_To_GPS -- The task connecting to GPS
         --
         task Connect_To_GPS;
         task body Connect_To_GPS is
            Address : GNAT.Sockets.Sock_Addr_Type;
         begin
            Connected := Standard.False;
            GNAT.Sockets.Create_Socket (Channel);
            Address.Addr :=
              GNAT.Sockets.Addresses
                (GNAT.Sockets.Get_Host_By_Name ("localhost"));
            Address.Port := GNAT.Sockets.Port_Type (GPS_Port);
            begin
               GNAT.Sockets.Connect_Socket (Channel, Address);
               Connected := Standard.True;
            exception
               when Error : GNAT.Sockets.Socket_Error =>
                  Ada.Strings.Unbounded.Append
                    (Message,
                     "Check if GPS is started with the server "
                     &  "option. For example"
                     &  Character'Val (10)
                     &  "  > gps --server="
                     & Ada.Strings.Fixed.Trim (Integer'Image (GPS_Port),
                                               Ada.Strings.Both)
                     &  Character'Val (10)
                     &  Ada.Exceptions.Exception_Message (Error));
               when Error : others =>
                  Ada.Strings.Unbounded.Append
                    (Message, Ada.Exceptions.Exception_Information (Error));
            end;
         end Connect_To_GPS;
         --
         -- Connect_Callback -- Timer callback
         --
         -- When the task is terminated, the callback either  shows  the
         -- error message in the dialog box, or else closes it.
         --
         function Connect_Callback (Data : Integer) return Boolean
         is
            use type Gtk.Label.Gtk_Label;
         begin
            if Connect_To_GPS'Terminated then
               if Ada.Strings.Unbounded.Length (Message) = 0 then
                  Gtk.Dialog.Response (Dialog, Gtk.Dialog.Gtk_Response_OK);
               elsif Label /= null then
                  Label := null;
                  Gtk.Label.Set_Text
                    (Label, Ada.Strings.Unbounded.To_String (Message));
               end if;
            end if;
            return Standard.True;
         end Connect_Callback;

         ID    : Glib.Main.G_Source_Id;
         Close : Gtk.Button.Gtk_Button;
      begin
         Close :=
           Gtk.Missed.Add_Button_From_Stock
             (Dialog   => Dialog,
              Response => Gtk.Dialog.Gtk_Response_Close,
              Icon     => Gtk.Stock.Stock_Close,
              Label    => "_Close",
              Tip      => "Stop connecting");
         Close.all.Set_Can_Default (Standard.True);
         Dialog.all.Show_All;
         ID :=
           Timers.Timeout_Add
             (Interval => 50,
              Func     => Connect_Callback'Access,
              Data     => 0);
         case Gtk.Dialog.Run (Dialog) is
            when Gtk.Dialog.Gtk_Response_OK =>   -- Closed on success from the task
               Glib.Main.Remove (ID);
            when others =>             -- Closed on error or by user
               Glib.Main.Remove (ID);  -- Stop polling
               GNAT.Sockets.Close_Socket (Channel); -- Will aboort task if active
               Connected := Standard.False;
         end case;
      end;
      Gtk.Dialog.Destroy (Dialog);
   end Connect;
   --
   -- GPS_Read -- Reading GPS connection socket until prompt it received
   --
   procedure GPS_Read is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);
      Byte   : Character;
      From   : GNAT.Sockets.Sock_Addr_Type;
      Last   : Ada.Streams.Stream_Element_Offset;
      Index  : Positive := GPS_Prompt'First;
   begin
      loop
         GNAT.Sockets.Receive_Socket (Channel, Buffer, Last, From);
         Byte := Character'Val (Ada.Streams.Stream_Element'Pos (Buffer (1)));
         if Byte = GPS_Prompt (Index) then
            exit when Index = GPS_Prompt'Last;
            Index := Index + 1;
         elsif Index > GPS_Prompt'First then
            Index := GPS_Prompt'First;
            if Byte = GPS_Prompt (Index) then
               Index := GPS_Prompt'First + 1;
            end if;
         end if;
      end loop;
   end GPS_Read;
   --
   -- GPS_Send -- Writing into GPS socket
   --
   --    Command - The GPS command to send
   --
   procedure GPS_Send (Command : String) is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Command'Length);
      for Buffer'Address use Command (Command'First)'Address;
      pragma Import (Ada, Buffer);
      Last   : Ada.Streams.Stream_Element_Offset;
   begin
      GNAT.Sockets.Send_Socket (Channel, Buffer, Last);
      pragma Unreferenced (Last);
   end GPS_Send;

   procedure Init
     (Window   : not null access Gtk.Window.Gtk_Window_Record'Class;
      Period   : Duration := 0.2;
      GPS_Port : Natural  := 50_000)
   is
      use type Ada.Task_Identification.Task_Id;
   begin
      Gtk.Main.Router.GPS_Port := GPS_Port;
      if Main = Ada.Task_Identification.Null_Task_Id then
         Main := Ada.Task_Identification.Current_Task;
         declare
            ID : Glib.Main.G_Source_Id;
         begin
            ID :=
              Glib.Main.Timeout_Add
                (Guint (Float (Period) * 1000.0),
                 Callback'Access);
         end;
         Parent := Window.all'Unchecked_Access;
         Window_Callback.Connect (Window, "destroy", On_Quit'Access);
      end if;
   end Init;

   procedure Request (Data : in out Request_Data'Class)
   is
      use type Ada.Task_Identification.Task_Id;
   begin
      if Main = Ada.Task_Identification.Current_Task then
         if Gateway.Get_State = Quitted then
            raise Quit_Error;
         end if;
         if Recursion < Max_Recursion then
            Recursion := Recursion + 1;
            begin
               Service (Data);
               Recursion := Recursion - 1;
            exception
               when others =>
                  Recursion := Recursion - 1;
                  raise;
            end;
         end if;
      elsif Main = Ada.Task_Identification.Null_Task_Id then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "No main task active" & Where ("Request"));
         raise Program_Error;
      else
         Gateway.Request_Service (Data, Standard.True);
      end if;
   end Request;

   package body Generic_Callback_Request is

      procedure Request
        (Callback : Callback_Procedure;
         Data     : not null access User_Data)
      is
         Marshaler : Callback_Data (Data);
      begin
         Marshaler.Callback := Callback;
         Request (Marshaler);
      end Request;

      overriding procedure Service (Data : in out Callback_Data) is
      begin
         Data.Callback (Data.Parameters);
      end Service;

   end Generic_Callback_Request;

   type Callback_Request_Data is new Request_Data with record
      Callback : Gtk_Callback;
   end record;
   overriding procedure Service (Data : in out Callback_Request_Data);

   overriding procedure Service (Data : in out Callback_Request_Data) is
   begin
      Data.Callback.all;
   end Service;

   procedure Request (Service : Gtk_Callback) is
      Marshaler : Callback_Request_Data;
   begin
      Marshaler.Callback := Service;
      Request (Marshaler);
      pragma Unreferenced (Marshaler);
   end Request;

   type UTF8_String_Ptr is access all UTF8_String;
   type Say_Data is record
      Message       : UTF8_String_Ptr;
      Title         : UTF8_String_Ptr;
      Mode          : UTF8_String_Ptr;
      Justification : Gtk.Enums.Gtk_Justification;
      Parent        : Gtk_Widget;
   end record;
   package Say_Callback is new Generic_Callback_Request (Say_Data);

   procedure Say (Data : not null access Say_Data) is
      Main : Gtk.Window.Gtk_Window := Parent;
   begin
      if Data.all.Parent /= null then
         declare
            Top : Gtk_Widget;
         begin
            Top := Data.all.Parent.all.Get_Toplevel;
            if
              Top /= null and then
              Top.all in Gtk.Window.Gtk_Window_Record'Class
            then
               Main :=
                 Gtk.Window.Gtk_Window_Record'Class (Top.all)'Unchecked_Access;
            end if;
         end;
      end if;
      Gtk.Missed.Message_Dialog
        (Message       => Data.all.Message.all,
         Mode          => Data.all.Mode.all,
         Title         => Data.all.Title.all,
         Justification => Data.all.Justification,
         Parent        => Main);
   end Say;

   procedure Say
     (Message       : UTF8_String;
      Title         : UTF8_String                    := "";
      Mode          : UTF8_String                    := Gtk.Stock.Stock_Dialog_Info;
      Justification : Gtk.Enums.Gtk_Justification    := Gtk.Enums.Justify_Left;
      Parent        : access Gtk_Widget_Record'Class := null)
   is
      Message_Text : aliased UTF8_String := Message;
      Title_Text   : aliased UTF8_String := Title;
      Mode_Text    : aliased UTF8_String := Mode;
      Data         : aliased Say_Data;
   begin
      Data.Message := Message_Text'Unchecked_Access;
      Data.Title   := Title_Text'Unchecked_Access;
      Data.Mode    := Mode_Text'Unchecked_Access;
      Data.Justification := Justification;
      if Parent /= null then
         Data.Parent := Parent.all'Unchecked_Access;
      end if;
      Say_Callback.Request (Say'Access, Data'Access);
   end Say;

   type Trace_Request (Length : Natural; Break : Boolean) is record
      Text : String (1 .. Length);
   end record;

   package Trace_Callback is
     new Generic_Callback_Request (Trace_Request);

   package Dialog_Handlers is
     new Gtk.Handlers.Callback (Gtk.Dialog.Gtk_Dialog_Record);

   package View_Handlers is
     new Gtk.Handlers.User_Callback (Gtk.Text_View.Gtk_Text_View_Record,
                                     Gtk.Dialog.Gtk_Dialog);

   package View_Event_Handlers is
     new Gtk.Handlers.User_Return_Callback
       (Gtk.Text_View.Gtk_Text_View_Record,
        Boolean,
        Gtk.Dialog.Gtk_Dialog);

   package Gtk_Image_Menu_Item_Handlers is
     new Gtk.Handlers.Callback (Gtk.Image_Menu_Item.Gtk_Image_Menu_Item_Record);

   Messages_List : Gtk.Text_Buffer.Gtk_Text_Buffer;
   Step_Button   : Gtk.Button.Gtk_Button;
   Run_Button    : Gtk.Button.Gtk_Button;
   View          : Gtk.Text_View.Gtk_Text_View;
   Break_Button  : Gtk.Check_Button.Gtk_Check_Button;

   type Tag_Index is mod 5;
   Tags        : array (Tag_Index) of Gtk.Text_Tag.Gtk_Text_Tag;
   Current_Tag : Tag_Index := 0;
   --
   -- On_Button_Press -- Event handler
   --
   function On_Button_Press
     (Dialog : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Event  : Gdk_Event;
      Parent : Gtk.Dialog.Gtk_Dialog) return Boolean is
   begin
      if Get_Event_Type (Event) = Button_Press then
         Window_X := Gint (Event.all.Button.X);
         Window_Y := Gint (Event.all.Button.Y);
      end if;
      return Standard.False;
   end On_Button_Press;
   --
   -- On_Go_To_Location -- Event handler
   --
   procedure On_Go_To_Location
     (Item : access Gtk.Image_Menu_Item.Gtk_Image_Menu_Item_Record'Class) is
   begin
      if not Connected then
         Connect;
      end if;
      if Connected then
         GPS_Send (Ada.Strings.Unbounded.To_String (Command));
         GPS_Read;
      end if;
   end On_Go_To_Location;
   --
   -- On_Paste_Trace -- Event handler
   --
   procedure On_Paste_Trace
     (Item : access Gtk.Image_Menu_Item.Gtk_Image_Menu_Item_Record'Class)
   is
      Text    : constant String :=
                  Gtk.Clipboard.Wait_For_Text (Gtk.Clipboard.Get);
      Pointer : Integer := Text'First;
      procedure Skip is
      begin
         while Pointer < Text'Last loop
            exit when Text (Pointer) /= ' ';
            Pointer := Pointer + 1;
         end loop;
      end Skip;
      function Get_Item return System.Address
      is
         Value : System.Storage_Elements.Integer_Address := 0;

         use type System.Storage_Elements.Integer_Address;
      begin
         if Pointer + 1 > Text'Last then
            raise Constraint_Error;
         end if;
         if Text (Pointer .. Pointer + 1) /= "0x" then
            raise Constraint_Error;
         end if;
         Pointer := Pointer + 2;
         while Pointer < Text'Last loop
            case Text (Pointer) is
               when '0' .. '9' =>
                  Value :=
                    (Value * 16
                     +  Character'Pos (Text (Pointer))
                     -  Character'Pos ('0'));
               when 'a' .. 'f' =>
                  Value :=
                    (Value * 16
                     +  Character'Pos (Text (Pointer))
                     -  Character'Pos ('a')
                     +  10);
               when 'A' .. 'F' =>
                  Value :=
                    (Value * 16
                     +  Character'Pos (Text (Pointer))
                     -  Character'Pos ('A')
                     +  10);
               when others =>
                  exit;
            end case;
            Pointer := Pointer + 1;
         end loop;
         if Text (Pointer - 1) = 'x' then
            raise Constraint_Error;
         end if;
         return System.Storage_Elements.To_Address (Value);
      end Get_Item;
      Count   : Natural := 0;
   begin
      declare
         Location : System.Address;
      begin
         while Pointer <= Text'Last loop
            Skip;
            Location := Get_Item;
            Count    := Count + 1;
         end loop;
      exception
         when others =>
            null;
      end;
      if Count > 0 then
         declare
            TB : GNAT.Traceback.Tracebacks_Array (1 .. Count);
         begin
            Pointer := Text'First;
            for Index in TB'Range loop
               Skip;
               TB (Index) := Get_Item;
            end loop;
            Trace
              ("Symbolic stack traceback from the clipboard:"
               & Ada.Characters.Latin_1.LF
               & GNAT.Traceback.Symbolic.Symbolic_Traceback (TB)
               & Ada.Characters.Latin_1.LF);
         end;
      end if;
   end On_Paste_Trace;
   --
   -- On_Populate_Popup -- Event handler
   --
   procedure On_Populate_Popup
     (Dialog : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      Params : Glib.Values.GValues;
      Parent : Gtk.Dialog.Gtk_Dialog)
   is
      Buffer : constant Gtk.Text_Buffer.Gtk_Text_Buffer :=
                 Gtk.Text_View.Get_Buffer (Dialog);
      Widget : Gtk_Widget;
      Menu   : Gtk.Menu.Gtk_Menu;
      Item   : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
      Icon   : Gtk.Image.Gtk_Image;
      Start  : Gtk.Text_Iter.Gtk_Text_Iter;
      Stop   : Gtk.Text_Iter.Gtk_Text_Iter;
      Can_Go : Boolean := Standard.False;

      use type Ada.Task_Identification.Task_Id;
   begin
      Widget := Convert (Glib.Values.Get_Address (Glib.Values.Nth (Params, 1)));
      if Widget /= null and then Widget.all in Gtk.Menu.Gtk_Menu_Record'Class
      then
         Menu := Gtk.Menu.Gtk_Menu_Record'Class (Widget.all)'Unchecked_Access;
         Menu.all.Modify_Font
           (Gtk.Style_Context.Get_Font
              (Gtk.Style_Context.Get_Style_Context (Parent),
               Gtk.Enums.Gtk_State_Flag_Normal));
         if Gtk.Clipboard.Get.Wait_Is_Text_Available then -- Paste traceback item
            Gtk.Image_Menu_Item.Gtk_New (Item, "Paste stack traceback");
            Gtk.Image.Gtk_New
              (Icon, Gtk.Stock.Stock_Find, Gtk.Enums.Icon_Size_Menu);
            Item.all.Set_Image (Icon);
            Item.all.Show_All;
            Gtk_Image_Menu_Item_Handlers.Connect
              (Item,
               "activate",
               On_Paste_Trace'Access);
            Menu.all.Add (Item);
         end if;
         if Main /= Ada.Task_Identification.Null_Task_Id then -- Go to the location item
            declare
               use type Gdk.Gdk_Window;
               Buffer_X : Gint;
               Buffer_Y : Gint;
               Moved    : Boolean;
            begin
               Dialog.all.Window_To_Buffer_Coords
                 (Gtk.Enums.Text_Window_Widget,
                  Window_X,
                  Window_Y,
                  Buffer_X,
                  Buffer_Y);
               Dialog.all.Get_Iter_At_Location (Start, Buffer_X, Buffer_Y);
               Gtk.Text_Iter.Copy (Start, Stop);
               Moved := Standard.True;
               while Moved and then not Gtk.Text_Iter.Starts_Line (Start) loop
                  Gtk.Text_Iter.Backward_Cursor_Position (Start, Moved);
               end loop;
               Moved := Standard.True;
               while Moved and then not Gtk.Text_Iter.Ends_Line (Stop) loop
                  Gtk.Text_Iter.Forward_Cursor_Position (Stop, Moved);
               end loop;
            end;
            declare
               Line    : String  := Buffer.Get_Text (Start, Stop);
               Pointer : Integer := Line'Last + 1;
            begin
               if
                 Line'Length < 3 or else
                 Line (Line'First) /= '0' or else
                 Line (Line'First + 1) /= 'x'
               then
                  goto Break;
               end if;
               loop
                  if Pointer = Line'First then
                     goto Break;
                  end if;
                  Pointer := Pointer - 1;
                  exit when Line (Pointer) not in '0' .. '9';
               end loop;
               if
                 Pointer - 4 < Line'First or else
                 Line (Pointer - 4) /= '.' or else
                 Ada.Characters.Handling.To_Lower (Line (Pointer - 3)) /= 'a' or else
                 Ada.Characters.Handling.To_Lower (Line (Pointer - 2)) /= 'd' or else
                 Line (Pointer) /= ':'
               then
                  goto Break;
               end if;
               Line (Pointer) := ' ';
               case Ada.Characters.Handling.To_Lower (Line (Pointer - 1)) is
                  when 's' | 'b' =>
                     Pointer := Pointer - 4;
                  when others =>
                     goto Break;
               end case;
               loop
                  if Pointer - 4 < Line'First then
                     goto Break;
                  end if;
                  exit when Line (Pointer - 4 .. Pointer - 1) = " at ";
                  Pointer := Pointer - 1;
               end loop;
               Can_Go := Standard.True;
               Command :=
                 Ada.Strings.Unbounded.To_Unbounded_String ("Editor.edit ");
               Ada.Strings.Unbounded.Append
                 (Command, Line (Pointer .. Line'Last));
               Ada.Strings.Unbounded.Append (Command, Character'Val (10));
            end;
            <<Break>>
            Gtk.Image_Menu_Item.Gtk_New (Item, "Go to the source location");
            Item.all.Set_Sensitive (Can_Go);
            Gtk.Image.Gtk_New
              (Icon, Gtk.Stock.Stock_Jump_To, Gtk.Enums.Icon_Size_Menu);
            Item.all.Set_Image (Icon);
            Item.all.Show_All;
            Gtk_Image_Menu_Item_Handlers.Connect
              (Item,
               "activate",
               On_Go_To_Location'Access);
            Menu.all.Add (Item);
         end if;
      end if;
   end On_Populate_Popup;

   procedure On_Quit (Window : access Gtk.Window.Gtk_Window_Record'Class) is
   begin
      Parent := null;
      Gateway.Quit;
   end On_Quit;

   procedure On_Response
     (Dialog   : access Gtk.Dialog.Gtk_Dialog_Record'Class;
      Response : Glib.Values.GValues) is
   begin
      case
        Gtk.Dialog.Gtk_Response_Type
          (Glib.Values.Get_Int (Glib.Values.Nth (Response, 1)))
      is
         when Gtk.Dialog.Gtk_Response_None =>
            Trace_Dialog := null;
         when Gtk.Dialog.Gtk_Response_Cancel =>
            Gtk.Check_Button.Set_Active (Break_Button, Standard.False);
         when Gtk.Dialog.Gtk_Response_OK =>
            Gtk.Check_Button.Set_Active (Break_Button, Standard.True);
         when others =>
            Gtk.Dialog.Destroy (Trace_Dialog);
            Trace_Dialog := null;
      end case;
   end On_Response;

   procedure Trace (Data : not null access Trace_Request) is
      Start : Gtk.Text_Iter.Gtk_Text_Iter;
      Stop  : Gtk.Text_Iter.Gtk_Text_Iter;

      use type Gtk.Text_Tag.Gtk_Text_Tag;
      use type Gtk.Dialog.Gtk_Dialog;
   begin
      if Trace_Dialog = null then
         declare
            Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
            Button : Gtk.Button.Gtk_Button;
            Font   : Pango.Font.Pango_Font_Description :=
                       Pango.Font.From_String ("monospace 10");
         begin
            Gtk.Dialog.Gtk_New
              (Trace_Dialog,
               "Trace",
               Get_Parent,
               Gtk.Dialog.Destroy_With_Parent);

            Tags := (others => null);
            Gtk.Text_Buffer.Gtk_New (Messages_List);
            Gtk.Text_View.Gtk_New (View, Messages_List);
            Messages_List.all.Unref;
            View.all.Modify_Font (Font);
            Pango.Font.Free (Font);
            -- Get menu
            View_Handlers.Connect
              (View,
               "populate-popup",
               On_Populate_Popup'Access,
               Trace_Dialog);
            View_Event_Handlers.Connect
              (View,
               "button_press_event",
               View_Event_Handlers.To_Marshaller
                 (On_Button_Press'Access),
               Trace_Dialog);
            -- Scrolled window
            Gtk.Scrolled_Window.Gtk_New (Scroll);
            Scroll.all.Set_Policy (Gtk.Enums.Policy_Automatic,
                                   Gtk.Enums.Policy_Automatic);
            Scroll.all.Add (View);
            Gtk.Dialog.Get_Content_Area (Trace_Dialog).all.Pack_Start (Scroll);
            -- Break button
            Gtk.Check_Button.Gtk_New (Break_Button, "Break");
            Break_Button.all.Set_Active (Standard.True);
            Trace_Dialog.all.Get_Action_Area.all.Pack_Start
              (Break_Button,
               Standard.False,
               Standard.False);
            -- Continue button
            Step_Button :=
              Gtk.Missed.Add_Button_From_Stock
                (Dialog   => Trace_Dialog,
                 Response => Gtk.Dialog.Gtk_Response_OK,
                 Icon     => Gtk.Stock.Stock_Media_Next,
                 Label    => "_Next",
                 Tip      => "Run to the next message");
            Step_Button.all.Set_Can_Default (Standard.True);
            Run_Button :=
              Gtk.Missed.Add_Button_From_Stock
                (Dialog   => Trace_Dialog,
                 Response => Gtk.Dialog.Gtk_Response_Cancel,
                 Icon     => Gtk.Stock.Stock_Media_Record,
                 Label    => "_Continue",
                 Tip      => "Continue without breaking");
            Button :=
              Gtk.Missed.Add_Button_From_Stock
                (Dialog   => Trace_Dialog,
                 Response => Gtk.Dialog.Gtk_Response_Close,
                 Icon     => Gtk.Stock.Stock_Quit,
                 Label    => "_Close",
                 Tip      => "Close trace window and continue");
            Dialog_Handlers.Connect
              (Trace_Dialog,
               "response",
               On_Response'Access);
            Trace_Dialog.all.Set_Default_Size (200, 400);
            Trace_Dialog.all.Show_All;
            Trace_Dialog.all.Set_Transient_For (Get_Parent);
         end;
      end if;
      declare
         --           Max_Buffer_Size : constant := 10_000;
         Offset : constant Gint := Messages_List.all.Get_Char_Count;
      begin
         --           while Offset > Max_Buffer_Size loop
         --              Messages_List.Get_Iter_At_Line (Start, 0);
         --              Messages_List.Get_End_Iter (Stop);
         --              Messages_List.Get_Iter_At_Line (Stop, 1);
         --              Messages_List.Delete (Start, Stop);
         --              Offset := Messages_List.Get_Char_Count;
         --           end loop;
         Messages_List.all.Get_End_Iter (Start);
         --         Messages_List.Insert (Start, Data.Text);
         Gtk.Missed.Insert_Alt (Messages_List, Start, Data.all.Text);
         Messages_List.all.Get_Iter_At_Offset (Start, Offset);
      end;
      if
        View.all.Scroll_To_Iter (Start, 0.25, Standard.False, 0.0, 0.0)
      then
         null;
      end if;
      -- Colorize new text
      if Tags (Current_Tag) = null then
         Tags (Current_Tag) := Gtk.Text_Buffer.Create_Tag (Messages_List);

         declare
            use type Gdk.Color.IHLS.Gdk_Luminance;
            use type Gdk.Color.IHLS.Gdk_Saturation;
         begin
            Gdk.Color.Set_Property
              (Tags (Current_Tag),
               Gtk.Text_Tag.Background_Gdk_Property,
               Gdk.Color.IHLS.To_RGB
                 (Gdk.Color.IHLS.Val ((Hue        => 0,
                                       Luminance  => 240 * 256,
                                       Saturation => 29  * 256),
                  Natural (Current_Tag),
                  Tags'Length)));
         end;
      end if;
      declare
         Offset : constant Gint := Messages_List.all.Get_Char_Count;
      begin
         Messages_List.all.Get_Iter_At_Offset (Stop, Offset);
      end;
      Messages_List.all.Apply_Tag (Tags (Current_Tag), Start, Stop);
      Current_Tag := Current_Tag + 1;
      if not (Data.all.Break or else Break_Button.all.Get_Active) then
         while Events_Pending loop -- Pump messages queue
            exit when not Main_Iteration_Do (Standard.False);
         end loop;
         return;
      end if;
      -- Setting buttons
      Step_Button.all.Set_Sensitive (Standard.True);
      Run_Button.all.Set_Sensitive (Standard.True);
      Trace_Dialog.all.Set_Default (Step_Button);
      Trace_Dialog.all.Set_Modal (Standard.True);
      case Trace_Dialog.all.Run is
         when Gtk.Dialog.Gtk_Response_Cancel | Gtk.Dialog.Gtk_Response_OK =>
            Trace_Dialog.all.Set_Modal (Standard.False);
            Step_Button.all.Set_Sensitive (Standard.False);
            Run_Button.all.Set_Sensitive (Standard.False);
         when others =>
            null;
      end case;
   end Trace;

   procedure Trace
     (Message : UTF8_String;
      Break   : Boolean := Standard.False)
   is
      Data : aliased Trace_Request (Message'Length + 1, Break);
   begin
      Data.Text := Message & Ada.Characters.Latin_1.LF;
      Trace_Callback.Request (Trace'Access, Data'Access);
   end Trace;

   procedure Trace
     (Error : Ada.Exceptions.Exception_Occurrence;
      Break : Boolean := Standard.True) is
   begin
      Trace (Ada.Exceptions.Exception_Information (Error), Break);
   end Trace;

end Gtk.Main.Router;
