--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Directory_Browser                  Luebeck            --
--  Test for                                       Autumn, 2007       --
--                                                                    --
--                                Last revision :  07:53 21 Jul 2016  --
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
--  This  test  uses  Ada.Directories,  so it requires Ada 2005. It also
--  requires Strings_Edit for wildcard matching.
--
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with Gdk.Event;                 use Gdk.Event;
with GIO.Drive;                 use GIO.Drive;
with GIO.Mount;                 use GIO.Mount;
with GIO.Volume;                use GIO.Volume;
with GIO.Volume_Monitor;        use GIO.Volume_Monitor;
with GLib;                      use GLib;
with Glib.Object;               use Glib.Object;
with GLib.Values;               use GLib.Values;
with GNAT.Exception_Actions;    use GNAT.Exception_Actions;
with Gtk.Abstract_Browser;      use Gtk.Abstract_Browser;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Directory_Browser;     use Gtk.Directory_Browser;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Editable;              use Gtk.Editable;
with Gtk.Entry_Buffer;          use Gtk.Entry_Buffer;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Image_Button;          use Gtk.Image_Button;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Main.Router;           use Gtk.Main.Router;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Progress_Bar;          use Gtk.Progress_Bar;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Window;                use Gtk.Window;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Tree_Model;            use Gtk.Tree_Model;

with Ada.Unchecked_Conversion;
with GNAT.Traceback.Symbolic;
with Gtk.Button;

with Gtk.Wildcard_Directory_Browser;
use  Gtk.Wildcard_Directory_Browser;
--
-- Remove the following line if you do not use the GNAT Ada compiler.
--
with Gtk.Main.Router.GNAT_Stack;

procedure Test_Gtk_Directory_Browser is
   --
   -- All data are global, for the sake of  simplicity.  Otherwise,  the
   -- test were impossible to keep in  just  one  body  due  to  Ada  95
   -- restriction on controlled types.
   --
   Window   : Gtk_Window;
   Pattern  : Gtk_Entry;
   Bar      : Gtk_Progress_Bar;
   Browser  : Gtk_Wildcard_Directory_Browser;
   Selection_Mode : array (Gtk_Selection_Mode) of Gtk_Radio_Button;

   type Files_Record is new Gtk_Directory_Record with null record;
   type Files is access all Files_Record'Class;
   overriding
      procedure Progress
                (  Store     : not null access Files_Record;
                   Directory : Item_Path;
                   State     : GDouble
               );

   Store : Files;

   type Local_Callback is access procedure
        (  Widget : access Gtk_Widget_Record'Class
        );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Callback,
             Gtk.Button.Cb_Gtk_Button_Void
          );
   type Change_Callback is access procedure (Cell : Gtk_Editable);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Change_Callback,
             Cb_Gtk_Editable_Void
          );

   procedure Refreshed (Widget : access Gtk_Widget_Record'Class) is
   begin
      Changed
      (  Browser.Get_Cache,
         Browser.Get_Tree_View.Get_Current_Directory
      );
   exception
      when Error : others =>
         Put_Line
         (  "Fault in Refreshed: "
         &  Exception_Information (Error)
         );
   end Refreshed;

   procedure Deleted (Widget : access Gtk_Widget_Record'Class) is
   begin
      if Get_Selection_Size (Get_Files_View (Browser)) = 0 then
         -- Deleting entire directory
         declare
            Name : constant String :=
                      String
                      (  Get_Current_Directory (Get_Tree_View (Browser))
                      );
            Response : aliased Gtk_Response_Type;
         begin
            Message_Dialog
            (  Message  => "Do you want to delete directory " & Name,
               Parent   => Widget,
               Mode     => Stock_Dialog_Question,
               Response => Response'Access
            );
            if Response = Gtk_Response_Yes then
               Delete (Get_Cache (Browser), Name);
            end if;
         exception
            when Error : others =>
               Say
               (  "Cannot delete "
               &  Name
               &  ". "
               &  Exception_Message (Error)
               );
         end;
      else
         -- Deleting files
         declare
            Selected : constant Selection :=
                          Get_Selection (Get_Files_View (Browser));
            List     : Unbounded_String;
            Response : aliased Gtk_Response_Type;
         begin
            for Index in Selected'Range loop
               if Index /= 1 then
                  Append (List, ", ");
               end if;
               Append
               (  List,
                  String
                  (  Get_Name
                     (  Get_Files_View (Browser),
                        Selected (Index)
               )  )  );
            end loop;
            Message_Dialog
            (  Message  =>
                  (  "Do you want to delete "
                  &  To_String (List)
                  &  " from "
                  &  String (Get_Directory (Get_Files_View (Browser)))
                  ),
               Parent   => Widget,
               Mode     => Stock_Dialog_Question,
               Response => Response'Access
            );
            if Response = Gtk_Response_Yes then
               for Index in reverse Selected'Range loop
                  Delete
                  (  Get_Cache (Browser),
                     String
                     (  Get_Path
                        (  Get_Files_View (Browser),
                           Selected (Index)
                  )  )  );
               end loop;
            end if;
         exception
            when Error : others =>
               Say
               (  "Cannot delete some of the files. "
               &  Exception_Message (Error)
               );
         end;
      end if;
   exception
      when Error : others =>
         Put_Line
         (  "Fault in Deleted: "
         &  Exception_Information (Error)
         );
   end Deleted;

   procedure Filter_Changed (Cell : Gtk_Editable) is
   begin
      Browser.Set_Pattern (Pattern.Get_Text);
   end Filter_Changed;

   procedure Selection_Mode_Set
             (  Widget : access Gtk_Widget_Record'Class
             )  is
   begin
      for Index in Selection_Mode'Range loop
         if Get_Active (Selection_Mode (Index)) then
            Set_Selection_Mode (Get_Files_View (Browser), Index);
            return;
         end if;
      end loop;
   end Selection_Mode_Set;

   function Mode_Label (Mode : Gtk_Selection_Mode) return String is
   begin
      case Mode is
         when Selection_None     => return "None";
         when Selection_Single   => return "Single";
         when Selection_Browse   => return "Browse";
         when Selection_Multiple => return "Multiple";
      end case;
   end Mode_Label;

   function Get_Drives return Gtk_Frame is
      use type Drive_List.Glist;
      use type Volume_List.Glist;
      Frame   : Gtk_Frame;
      View    : Gtk_Tree_View;
      List    : Gtk_Tree_Store;
      Row     : Gtk_Tree_Iter;
      Monitor : constant GVolume_Monitor  := Get;
      Drives  : Drive_List.Glist := Get_Connected_Drives (Monitor);
   begin
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_In);
      Gtk_New (List, (GType_Icon, GType_String));
      declare
         Item  : Drive_List.Glist := Drive_List.First (Drives);
         Icon  : GObject;
         Drive : GDrive;
         Value : GValue;
      begin
         Init (Value, GType_Icon);
         while Item /= Drive_List.Null_List loop
            Drive := Drive_List.Get_Data (Item);
            List.Append (Row, Null_Iter);
            Icon := Drive.Get_Icon;
            if Icon /= null then
               Set (Value, Icon);
               Icon.Unref;
            end if;
            Set_Value (List, Row, 0, Value);
            Gtk.Missed.Set (List, Row, 1, Get_Name (Drive));
            declare
               Parent  : constant Gtk_Tree_Iter := Row;
               Volume  : GVolume;
               Volumes : Volume_List.Glist := Get_Volumes (Drive);
               Item    : constant Volume_List.Glist :=
                         Volume_List.First (Volumes);
            begin
               while Item /= Volume_List.Null_List loop
                  Volume := Volume_List.Get_Data (Item);
                  List.Append (Row, Parent);
                  Icon := Volume.Get_Icon;
                  if Icon /= null then
                     Set (Value, Icon);
                     Icon.Unref;
                  end if;
                  List.Set_Value (Row, 0, Value);
                  List.Set (Row, 1, Get_Name (Volume));
                  Volume.Unref;
               end loop;
               Volume_List.Free (Volumes);
            end;
            Drive.Unref;
            Item := Drive_List.Next (Item);
         end loop;
         Drive_List.Free (Drives);
         Unset (Value);
      end;
      Gtk_New (View);
      declare
         Column        : Gtk_Tree_View_Column;
         Column_No     : GInt;
         Name_Renderer : Gtk_Cell_Renderer_Text;
         Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;
      begin
         Gtk_New (Column);
         Column.Set_Title ("Drive/volume");
         Gtk_New (Icon_Renderer);
         Column.Pack_Start (Icon_Renderer, False);
         Column.Add_Attribute (Icon_Renderer, "gicon", 0);

         Gtk_New (Name_Renderer);
         Column.Pack_Start (Name_Renderer, True);
         Column.Add_Attribute (Name_Renderer, "text", 1);
         Column_No := Append_Column (View, Column);
         Column.Set_Resizable (True);
      end;
      View.Set_Model (To_Interface (List.all'Unchecked_Access));
      Frame.Add (View);
      Unref (Monitor);
      return Frame;
   exception
      when Error : others =>
         Put_Line
         (  "Fault in Get_Drives: "
         &  Exception_Information (Error)
         );
         return Frame;
   end Get_Drives;

   function Get_Volumes return Gtk_Frame is
      use type Volume_List.Glist;
      Frame   : Gtk_Frame;
      View    : Gtk_Tree_View;
      List    : Gtk_Tree_Store;
      Row     : Gtk_Tree_Iter;
      Monitor : constant GVolume_Monitor := Get;
      Volumes : Volume_List.Glist := Get_Volumes (Monitor);
   begin
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Gtk_New
      (  List,
         (GType_Icon, GType_String, GType_Icon, GType_String)
      );
      declare
         Item   : Volume_List.Glist := Volume_List.First (Volumes);
         Icon   : GObject;
         Volume : GVolume;
         Drive  : GDrive;
         Value  : GValue;
      begin
         Init (Value, GType_Icon);
         while Item /= Volume_List.Null_List loop
            Volume := Volume_List.Get_Data (Item);
            List.Append (Row, Null_Iter);
            Icon := Volume.Get_Icon;
            if Icon /= null then
               Set (Value, Icon);
               Icon.Unref;
            end if;
            Set_Value (List, Row, 0, Value);
            Gtk.Missed.Set (List, Row, 1, Get_Name (Volume));

            Drive := Get_Drive (Volume);
            if Drive /= null then
               Icon := Drive.Get_Icon;
               if Icon /= null then
                  Set (Value, Icon);
                  Icon.Unref;
               end if;
               Set_Value (List, Row, 0, Value);
               Gtk.Missed.Set (List, Row, 1, Get_Name (Drive));
               Drive.Unref;
            end if;

            Volume.Unref;
            Item := Volume_List.Next (Item);
         end loop;
         Volume_List.Free (Volumes);
         Unset (Value);
      end;
      Gtk_New (View);
      declare
         Column        : Gtk_Tree_View_Column;
         Column_No     : GInt;
         Name_Renderer : Gtk_Cell_Renderer_Text;
         Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;
      begin
         Gtk_New (Column);
         Column.Set_Title ("Volume");
         Gtk_New (Icon_Renderer);
         Column.Pack_Start (Icon_Renderer, False);
         Column.Add_Attribute (Icon_Renderer, "gicon", 0);

         Gtk_New (Name_Renderer);
         Column.Pack_Start (Name_Renderer, True);
         Column.Add_Attribute (Name_Renderer, "text", 1);
         Column_No := Append_Column (View, Column);
         Column.Set_Resizable (True);

         Gtk_New (Column);
         Column.Set_Title ("Drive");
         Gtk_New (Icon_Renderer);
         Column.Pack_Start (Icon_Renderer, False);
         Column.Add_Attribute (Icon_Renderer, "gicon", 2);

         Gtk_New (Name_Renderer);
         Column.Pack_Start (Name_Renderer, True);
         Column.Add_Attribute (Name_Renderer, "text", 3);
         Column_No := Append_Column (View, Column);
         Column.Set_Resizable (True);
      end;
      View.Set_Model (To_Interface (List.all'Unchecked_Access));
      Frame.Add (View);
      Unref (Monitor);
      return Frame;
   exception
      when Error : others =>
         Put_Line
         (  "Fault in Get_Volumes: "
         &  Exception_Information (Error)
         );
         return Frame;
   end Get_Volumes;

   function Get_Mounts return Gtk_Frame is
      use type Mount_List.Glist;
      Frame   : Gtk_Frame;
      View    : Gtk_Tree_View;
      List    : Gtk_Tree_Store;
      Row     : Gtk_Tree_Iter;
      Monitor : constant GVolume_Monitor := Get;
      Mounts  : Mount_List.Glist := Get_Mounts (Monitor);
   begin
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Gtk_New
      (  List,
         (  GType_Icon, GType_String,
            GType_Icon, GType_String,
            GType_String,
            GType_String,
            GType_String,
            GType_String,
            GType_Icon, GType_String
      )  );
      declare
         Item  : Mount_List.Glist := Mount_List.First (Mounts);
         Icon  : GObject;
         Mount : GMount;
         Value : GValue;
      begin
         Init (Value, GType_Icon);
         while Item /= Mount_List.Null_List loop
            Mount := Mount_List.Get_Data (Item);
            List.Append (Row, Null_Iter);
            Icon := Mount.Get_Icon;
            if Icon /= null then
               Set (Value, Icon);
               Icon.Unref;
            end if;
            Set_Value (List, Row, 0, Value);
            Gtk.Missed.Set (List, Row, 1, Get_Name (Mount));

            declare
               Drive : constant GDrive := Get_Drive (Mount);
            begin
               if Drive = null then
                  Gtk.Missed.Set (List, Row, 3, "none");
               else
                  Icon := Drive.Get_Icon;
                  if Icon /= null then
                     Set (Value, Icon);
                     Icon.Unref;
                  end if;
                  Set_Value (List, Row, 2, Value);
                  Gtk.Missed.Set (List, Row, 3, Get_Name (Drive));
                  Drive.Unref;
               end if;
            end;

            Gtk.Missed.Set (List, Row, 4, Get_UUID (Mount));
            if Can_Unmount (Mount) then
               Gtk.Missed.Set (List, Row, 5, "gtk-apply");
            else
               Gtk.Missed.Set (List, Row, 5, "gtk-cancel");
            end if;
            if Can_Eject (Mount) then
               Gtk.Missed.Set (List, Row, 6, "gtk-apply");
            else
               Gtk.Missed.Set (List, Row, 6, "gtk-cancel");
            end if;
            Gtk.Missed.Set (List, Row, 7, Get_Root (Mount));

            declare
               Volume : constant GVolume := Get_Volume (Mount);
            begin
               if Volume = null then
                  Gtk.Missed.Set (List, Row, 9, "none");
               else
                  Icon := Volume.Get_Icon;
                  if Icon /= null then
                     Set (Value, Icon);
                     Icon.Unref;
                  end if;
                  Set_Value (List, Row, 8, Value);
                  Gtk.Missed.Set (List, Row, 9, Get_Name (Volume));
                  Volume.Unref;
               end if;
            end;

            Unref (Mount);
            Item := Mount_List.Next (Item);
         end loop;
         Mount_List.Free (Mounts);
         Unset (Value);
      end;
      Gtk_New (View);
      declare
         Column        : Gtk_Tree_View_Column;
         Column_No     : GInt;
         Name_Renderer : Gtk_Cell_Renderer_Text;
         Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;
      begin
         Gtk_New (Column);
         Column.Set_Title ("Mounts");
         Gtk_New (Icon_Renderer);
         Column.Pack_Start (Icon_Renderer, False);
         Column.Add_Attribute (Icon_Renderer, "gicon", 0);

         Gtk_New (Name_Renderer);
         Column.Pack_Start (Name_Renderer, True);
         Column.Add_Attribute (Name_Renderer, "text", 1);
         Column_No := Append_Column (View, Column);
         Column.Set_Resizable (True);

         Gtk_New (Column);
         Column.Set_Title ("Can");
         Gtk_New (Icon_Renderer);
         Column.Pack_Start (Icon_Renderer, False);
         Add_Stock_Attribute (Column, Icon_Renderer, 5);
         Gtk_New (Icon_Renderer);
         Column.Pack_Start (Icon_Renderer, False);
         Add_Stock_Attribute (Column, Icon_Renderer, 6);
         Column_No := Append_Column (View, Column);
         Column.Set_Resizable (False);

         Gtk_New (Column);
         Column.Set_Title ("Root");
         Column.Pack_Start (Name_Renderer, True);
         Column.Add_Attribute (Name_Renderer, "text", 7);
         Column_No := Append_Column (View, Column);
         Column.Set_Resizable (True);

         Gtk_New (Column);
         Column.Set_Title ("Volume");
         Gtk_New (Icon_Renderer);
         Column.Pack_Start (Icon_Renderer, False);
         Column.Add_Attribute (Icon_Renderer, "gicon", 8);
         Gtk_New (Name_Renderer);
         Column.Pack_Start (Name_Renderer, True);
         Column.Add_Attribute (Name_Renderer, "text", 9);
         Column_No := Append_Column (View, Column);
         Column.Set_Resizable (True);

         Gtk_New (Column);
         Column.Set_Title ("UUID");
         Column.Pack_Start (Name_Renderer, True);
         Column.Add_Attribute (Name_Renderer, "text", 4);
         Column_No := Append_Column (View, Column);
         Column.Set_Resizable (True);

         Gtk_New (Column);
         Column.Set_Title ("Drive");
         Gtk_New (Icon_Renderer);
         Column.Pack_Start (Icon_Renderer, False);
         Column.Add_Attribute (Icon_Renderer, "gicon", 2);
         Gtk_New (Name_Renderer);
         Column.Pack_Start (Name_Renderer, True);
         Column.Add_Attribute (Name_Renderer, "text", 3);
         Column_No := View.Append_Column (Column);
         Column.Set_Resizable (True);
      end;
      View.Set_Model (To_Interface (List.all'Unchecked_Access));
      Frame.Add (View);
      Unref (Monitor);
      return Frame;
   exception
      when Error : others =>
         Put_Line
         (  "Fault in Get_Mounts: "
         &  Exception_Information (Error)
         );
         return Frame;
   end Get_Mounts;

   procedure Progress
             (  Store     : not null access Files_Record;
                Directory : Item_Path;
                State     : GDouble
            )  is
      Active : Boolean;
   begin
      if State >= 1.0 then
         Bar.Set_Show_Text (False);
         Bar.Set_Fraction (0.0);
         Bar.Set_Text ("");
      else
         Bar.Set_Show_Text (True);
         Bar.Set_Fraction (State);
         if Directory'Length = 0 then
            Bar.Set_Text ("system volumes");
         else
            Bar.Set_Text (String (Directory));
         end if;
         while Gtk.Main.Events_Pending loop
            Active := Gtk.Main.Main_Iteration;
         end loop;
      end if;
   exception
      when Error : others =>
         Put_Line ("Progress: " & Exception_Information (Error));
   end Progress;

   procedure Program_Error_Tracer
             (  Occurence : Exception_Occurrence
             )  is
      use GNAT.Traceback, GNAT.Traceback.Symbolic;
   begin
      Put_Line
      (  "Traced Program_Error "
      &  Exception_Message (Occurence)
      &  " at"
      );
      Put_Line (Symbolic_Traceback (Occurence));
   end Program_Error_Tracer;

   type Exception_Tracer_Ptr is access
      procedure (Occurence : Exception_Occurrence);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Exception_Tracer_Ptr,
             Exception_Action
          );
begin
   Register_Id_Action
   (  Program_Error'Identity,
      +Program_Error_Tracer'Access
   );
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Window.Set_Title ("Test directory browser");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   --
   -- The following lines are meant for debugging under GNAT. They cause
   -- stack  tracing upon errors in the libraries specified. Remove them
   -- if you are using another compiler.
   --
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("Gtk");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GLib-GObject");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GLib-GIO");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GtkAda+");

   declare
      Box     : Gtk_Box;
      Buttons : Gtk_HBox;
      Button  : Gtk_Image_Button;
      Bang    : Gtk_Vseparator;
      Frame   : Gtk_Frame;
      Label   : Gtk_Label;
      Paned   : Gtk_Paned;
   begin
      Gtk_New_Vpaned (Paned);
      --
      -- Upper panel
      --
      Gtk_New_HBox (Box);
      Box.Pack_Start (Get_Drives);
      Box.Pack_Start (Get_Volumes);
      Box.Pack_Start (Get_Mounts);
      Paned.Pack1 (Box, False, False);
      --
      -- Lower panel
      --
      Gtk_New_VBox (Box);
      -- Buttons
      Gtk_New_HBox (Buttons);

      Gtk_New (Button, Stock_Refresh, Icon_Size_Small_Toolbar);
      Button.On_Clicked (+Refreshed'Access);
      Buttons.Pack_Start (Button, False, False);

      Gtk_New (Button, Stock_Delete, Icon_Size_Small_Toolbar);
      Button.On_Clicked (+Deleted'Access);
      Buttons.Pack_Start (Button, False, False);
      -- Radio buttons
      for Index in Selection_Mode'Range loop
         if Index = Gtk_Selection_Mode'First then
            Gtk_New
            (  Selection_Mode (Index),
               Label => Mode_Label (Index)
            );
         else
            Gtk_New
            (  Selection_Mode (Index),
               Selection_Mode (Gtk_Selection_Mode'First).Get_Group,
               Mode_Label (Index)
            );
         end if;
         Buttons.Pack_Start (Selection_Mode (Index), False, False);
      end loop;
      Selection_Mode (Selection_Multiple).Set_Active (True);
      for Index in Selection_Mode'Range loop
         Selection_Mode (Index).On_Clicked (+Selection_Mode_Set'Access);
      end loop;

      Gtk_New_Vseparator (Bang);
      Pack_Start (Buttons, Bang, False, False);

      Gtk_New (Label, "Filter");
      Pack_Start (Buttons, Label, False, False);

      Gtk_New (Pattern);
      Buttons.Pack_Start (Pattern, False, False);
      On_Changed (+Pattern, +Filter_Changed'Access);

      -- Packing buttons
      Box.Pack_Start (Buttons, False, False);
      Gtk_New (Bar);
      Bar.Set_Show_Text (True);
      Buttons.Pack_Start (Bar, True, True);

      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_In);
      Box.Pack_Start (Frame);
      Paned.Pack2 (Box, True, False);

      Window.Add (Paned);
      Window.Show_All;

      -- Files
      Store := new Files_Record;
      Initialize
      (  Store.all'Unchecked_Access,
         Cache_Expanded,
         Trace_Nothing
      );
--    Set_Trace_File ("C:\Temp\Trace.txt");
      Gtk_New
      (  Widget  => Browser,
         Store   => Store.all'Unchecked_Access
--       ,Tracing => Trace_All and not Trace_To_Both
      );
      Browser.Get_Files_View.Set_Editable (True);
      Browser.Get_Tree_View.Set_Editable (True);
      Frame.Add (Browser);
      Browser.Show_All;
   end;

   Gtk.Main.Main;
exception
   when Error : others =>
      Put_Line ("Fatal error: " & Exception_Information (Error));
end Test_Gtk_Directory_Browser;
