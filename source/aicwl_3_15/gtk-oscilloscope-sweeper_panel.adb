--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope.Sweeper_Panel              Luebeck            --
--  Implementation                                 Summer, 2011       --
--                                                                    --
--                                Last revision :  22:46 07 Apr 2016  --
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

with Ada.Exceptions;            use Ada.Exceptions;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties;           use GLib.Properties;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Enums;                 use Gtk.Enums;
with GtkAda.Types;              use GtkAda.Types;
with Strings_Edit.Integers;     use Strings_Edit.Integers;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;

with Ada.Calendar;
with GLib.Object.Checked_Destroy;

package body Gtk.Oscilloscope.Sweeper_Panel is
   use Gtk.Layered.Waveform.Edit;

   Duration_Small : constant := 3;
   Page_Step      : constant := 1.5;
   Edit_Field     : constant := 12;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Oscilloscope.Amplifier_Panel." & Name;
   end Where;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name  => "column-spacings",
               Nick  => "Column spacings",
               Blurb => "Spacing between columns",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "offset-tip",
               Nick    => "Offset tip",
               Blurb   => "Tooltip of the offset entry field",
               Default => "Delay between current time and the values " &
                          "shown at the right margin. When frozen " &
                          "this field indicates the time at the " &
                          "right margin"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "page-tip",
               Nick    => "Page tip",
               Blurb   => "Tooltip of the page entry field",
               Default => "The duration of the page"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "row-spacings",
               Nick    => "Row spacings",
               Blurb   => "Spacing between rows",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "show-axis",
               Default => "Show axis",
               Nick    => "Show axis",
               Blurb   => "Show axis check box text"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "show-graph-paper",
               Default => "Show graph paper",
               Nick    => "Show graph paper",
               Blurb   => "Show graph paper check box text"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget       : out Gtk_Oscilloscope_Sweeper_Panel;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class;
                Sweeper      : Sweeper_Type := Lower;
                Show_Buttons : Boolean      := True;
                Flat         : Boolean      := False
             )  is
   begin
      Widget := new Gtk_Oscilloscope_Sweeper_Panel_Record;
      Initialize (Widget, Oscilloscope, Sweeper, Show_Buttons, Flat);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Oscilloscope_Sweeper_Panel_Record'Class;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class;
                Sweeper      : Sweeper_Type;
                Show_Buttons : Boolean;
                Flat         : Boolean
             )  is
      Row    : GUInt := 0;
      Column : GUInt := 0;
   begin
      Widget.Sweeper := Sweeper;
      G_New (Widget, Get_Type);
      if Show_Buttons then
         if Flat then
            Gtk_Table_Record (Widget.all).Initialize (30, 1, False);
         else
            Gtk_Table_Record (Widget.all).Initialize (5, 6, False);
         end if;
      else
         if Flat then
            Gtk_Table_Record (Widget.all).Initialize (12, 1, False);
         else
            Gtk_Table_Record (Widget.all).Initialize (2, 6, False);
         end if;
      end if;
      Widget.Set_Col_Spacings (3);
      Widget.Set_Row_Spacings (3);
      Widget.Oscilloscope := Oscilloscope.all'Unchecked_Access;
      Ref (Oscilloscope);
      -- Row 1 ---------------------------------------------------------
         -- Slower button
      Page_Slower_Buttons.Gtk_New (Widget.Page_Slower);
      Attach
      (  Widget,
         Widget.Page_Slower,
         1, 2,
         0, 1,
         XOptions => Shrink,
         YOptions => Shrink
      );
      Connect
      (  Widget.Page_Slower,
         "clicked",
         On_Click_Slower'Access,
         Widget.all'Unchecked_Access
      );
      Gtk_New (Widget.Page_Edit);
      Widget.Page_Edit.Set_Width_Chars (Edit_Field);
      if Find_Property (Widget.Page_Edit, "max-width-chars") /= null
      then
         Set_Property
         (  Widget.Page_Edit,
            Build ("max-width-chars"),
            GInt'(Edit_Field)
         );
      end if;
      Widget.Page_Edit.Set_Text
      (  Image
         (  Value    => GDouble (Oscilloscope.Get_Page_Span (Sweeper)),
            AbsSmall =>-Duration_Small
      )  );
      Attach
      (  Widget,
         Widget.Page_Edit,
         2, 3,
         0, 1,
         YOptions => Shrink
      );
      Connect
      (  Widget.Page_Edit,
         "changed",
         On_Changed_Page'Access,
         Widget.all'Unchecked_Access
      );
      Gtk_New (Widget.Page_Unit, "s");
      Attach
      (  Widget,
         Widget.Page_Unit,
         3, 4,
         0, 1,
         XOptions => Gtk.Enums.Fill,
         YOptions => Shrink
      );
      Page_Faster_Buttons.Gtk_New (Widget.Page_Faster);
      Attach
      (  Widget,
         Widget.Page_Faster,
         4, 5,
         0, 1,
         XOptions => Shrink,
         YOptions => Shrink
      );
      Connect
      (  Widget.Page_Faster,
         "clicked",
         On_Click_Faster'Access,
         Widget.all'Unchecked_Access
      );
         -- Forward
      Forward_Buttons.Gtk_New (Widget.Forward_Button);
      Attach
      (  Widget,
         Widget.Forward_Button,
         5, 6,
         0, 1,
         XOptions => Shrink,
         YOptions => Shrink
      );
      Connect
      (  Widget.Forward_Button,
         "clicked",
         On_Click_Forward'Access,
         Widget.all'Unchecked_Access
      );
      -- Row 2 ---------------------------------------------------------
         -- Page left
      Offset_Page_Left_Buttons.Gtk_New (Widget.Offset_Page_Left);
      if Flat then
         Column := 6;
         Row    := 1;
      end if;
      Attach
      (  Widget,
         Widget.Offset_Page_Left,
         0 + Column, 1 + Column,
         1 - Row,    2 - Row,
         XOptions => Shrink,
         YOptions => Shrink
      );
      Connect
      (  Widget.Offset_Page_Left,
         "clicked",
         On_Click_Page_Left'Access,
         Widget.all'Unchecked_Access
      );
         -- Left
      Offset_Left_Buttons.Gtk_New (Widget.Offset_Left);
      Attach
      (  Widget,
         Widget.Offset_Left,
         1 + Column, 2 + Column,
         1 - Row,    2 - Row,
         XOptions => Shrink,
         YOptions => Shrink
      );
      Connect
      (  Widget.Offset_Left,
         "clicked",
         On_Click_Left'Access,
         Widget.all'Unchecked_Access
      );
         -- Frequency box
      Gtk_New (Widget.Offset_Edit);
      Widget.Offset_Edit.Set_Width_Chars (Edit_Field);
      if Find_Property (Widget.Offset_Edit, "max-width-chars") /= null
      then
         Set_Property
         (  Widget.Offset_Edit,
            Build ("max-width-chars"),
            GInt'(Edit_Field)
         );
      end if;
      Widget.Show_Offset;
      Attach
      (  Widget,
         Widget.Offset_Edit,
         2 + Column, 3 + Column,
         1 - Row,    2 - Row,
         YOptions => Shrink
      );
      Connect
      (  Widget.Offset_Edit,
         "changed",
         On_Changed_Offset'Access,
         Widget.all'Unchecked_Access
      );
      Gtk_New (Widget.Offset_Unit, "s");
      Attach
      (  Widget,
         Widget.Offset_Unit,
         3 + Column, 4 + Column,
         1 - Row,    2 - Row,
         XOptions => Gtk.Enums.Fill,
         YOptions => Shrink
      );
         -- Right
      Offset_Right_Buttons.Gtk_New (Widget.Offset_Right);
      Attach
      (  Widget,
         Widget.Offset_Right,
         4 + Column, 5 + Column,
         1 - Row,    2 - Row,
         XOptions => Shrink,
         YOptions => Shrink
      );
      Connect
      (  Widget.Offset_Right,
         "clicked",
         On_Click_Right'Access,
         Widget.all'Unchecked_Access
      );
         -- Page right
      Offset_Page_Right_Buttons.Gtk_New (Widget.Offset_Page_Right);
      Attach
      (  Widget,
         Widget.Offset_Page_Right,
         5 + Column, 6 + Column,
         1 - Row,    2 - Row,
         XOptions => Shrink,
         YOptions => Shrink
      );
      Connect
      (  Widget.Offset_Page_Right,
         "clicked",
         On_Click_Page_Right'Access,
         Widget.all'Unchecked_Access
      );
      if Show_Buttons then
         -- Row 3 ------------------------------------------------------
         if Flat then
            Column := Column + 6;
            Row    := Row    + 1;
         end if;
         Gtk_New (Widget.Axis_Button, "show axis");
         Widget.Axis_Button.Set_Active
         (  Oscilloscope.Get_Time_Axis (Sweeper)
         );
         Attach
         (  Widget,
            Widget.Axis_Button,
            0 + Column, 6 + Column,
            2 - Row,    3 - Row,
            YOptions => Shrink
         );
         Connect
         (  Widget.Axis_Button,
            "toggled",
            On_Toggled_Axis'Access,
            Widget.all'Unchecked_Access
         );
         -- Row 4 ------------------------------------------------------
         if Flat then
            Column := Column + 6;
            Row    := Row    + 1;
         end if;
         Gtk_New (Widget.Grid_Button, "show graph paper");
         Widget.Grid_Button.Set_Active
         (  Oscilloscope.Get_Time_Grid (Sweeper)
         );
         Attach
         (  Widget,
            Widget.Grid_Button,
            0 + Column, 6 + Column,
            3 - Row,    4 - Row,
            YOptions => Shrink
         );
         Connect
         (  Widget.Grid_Button,
            "toggled",
            On_Toggled_Grid'Access,
            Widget.all'Unchecked_Access
         );
         -- Row 5 ------------------------------------------------------
         if Flat then
            Column := Column + 6;
            Row    := Row    + 1;
         end if;
         Gtk_New (Widget.Time_Stamp, "");
         Widget.Time_Stamp.Set_Justify (Justify_Left);
         Attach
         (  Widget,
            Widget.Time_Stamp,
            0 + Column, 6 + Column,
            4 - Row,    5 - Row,
            YOptions => Shrink
         );
      end if;
      On_Frozen_Changed
      (  Oscilloscope,
         Sweeper_Type'Pos (Sweeper),
         Widget.all'Unchecked_Access
      );

      Connect
      (  Oscilloscope,
         "position-changed",
         On_Position_Changed'Access,
         Widget.all'Unchecked_Access
      );
      Connect
      (  Oscilloscope,
         "x-axis-toggled",
         To_Marshaller (On_Axis_Toggled'Access),
         Widget.all'Unchecked_Access
      );
      Connect
      (  Oscilloscope,
         "x-grid-toggled",
         To_Marshaller (On_Grid_Toggled'Access),
         Widget.all'Unchecked_Access
      );
      Connect
      (  Oscilloscope,
         "freezing-changed",
         To_Marshaller (On_Frozen_Changed'Access),
         Widget.all'Unchecked_Access
      );
      Connect
      (  Oscilloscope,
         "offset-changed",
         To_Marshaller (On_Offset_Changed'Access),
         Widget.all'Unchecked_Access
      );

      Connect
      (  Widget,
         "destroy",
         On_Destroy'Access,
         Widget.all'Unchecked_Access
      );
      Connect
      (  Widget,
         "style-updated",
         On_Style_Updated'Access,
         Widget.all'Unchecked_Access
      );
      On_Style_Updated (Widget, Widget.all'Unchecked_Access);
   end Initialize;

   procedure On_Axis_Toggled
             (  Widget : access Gtk_Widget_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      if Axis = Sweeper_Type'Pos (Panel.Sweeper) then
         Panel.Axis_Button.Set_Active
         (  Panel.Oscilloscope.Get_Time_Axis (Panel.Sweeper)
         );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Axis_Toggled")
         )  );
   end On_Axis_Toggled;

   procedure On_Changed_Offset
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      if Panel.Oscilloscope.Get_Frozen (Panel.Sweeper) then
         declare
            use Ada.Calendar;
            use Strings_Edit;
            Text    : constant String  := Panel.Offset_Edit.Get_Text;
            Pointer : Integer := Text'First;
            procedure Get (Symbol : Character) is
            begin
               if Pointer > Text'Last or else Text (Pointer) /= Symbol
               then
                  raise Data_Error;
               end if;
               Pointer := Pointer + 1;
               Get (Text, Pointer);
            end Get;
            Year_No  : Integer := 0;
            Month_No : Integer := 0;
            Day_No   : Integer := 0;
            Hour     : Integer := 0;
            Minute   : Integer := 0;
            Second   : GDouble := 0.0;
            Offset   : Duration;
            Date     : Ada.Calendar.Time;
         begin
            Get (Text, Pointer);
            Get (Text, Pointer, Hour, First => 0, Last => 24);
            Get (Text, Pointer);
            if Pointer <= Text'Last then
               Get (':');
               Get (Text, Pointer, Minute, First => 0, Last => 59);
               Get (Text, Pointer);
               if Pointer <= Text'Last then
                  Get (':');
                  Get
                  (  Source  => Text,
                     Pointer => Pointer,
                     Value   => Second,
                     First   => 0.0,
                     Last    => 60.0
                  );
                  Get (Text, Pointer);
                  if Pointer <= Text'Last then
                     Get
                     (  Source  => Text,
                        Pointer => Pointer,
                        Value   => Year_No,
                        First   => 1901,
                        Last    => 2399
                     );
                     Get (Text, Pointer);
                     Get ('.');
                     Get
                     (  Source  => Text,
                        Pointer => Pointer,
                        Value   => Month_No,
                        First   => 1,
                        Last    => 12
                     );
                     Get ('.');
                     Get
                     (  Source  => Text,
                        Pointer => Pointer,
                        Value   => Day_No,
                        First   => 1,
                        Last    => 31
                     );
                  end if;
               end if;
            end if;
            Get (Text, Pointer);
            if Pointer < Text'Last then
               return;
            end if;
            Offset :=
               (  Day_Duration (Hour)   * 3_600.0
               +  Day_Duration (Minute) * 60.0
               +  Day_Duration (Second)
               );
            if Year_No = 0 then
               Date := Clock;
               Date :=
                  Time_Of
                  (  Year    => Year  (Date),
                     Month   => Month (Date),
                     Day     => Day   (Date),
                     Seconds => Offset
                  );
            else
               Date :=
                  Time_Of
                  (  Year    => Year_Number  (Year_No),
                     Month   => Month_Number (Month_No),
                     Day     => Day_Number   (Day_No),
                     Seconds => Offset
                  );
            end if;
            Panel.Oscilloscope.Set_Time (Panel.Sweeper, Date);
         end;
      else
         declare
            Offset : GDouble;
         begin
            Offset := Value (Panel.Offset_Edit.Get_Text);
            Panel.Oscilloscope.Set_Time
            (  Panel.Sweeper,
               Time'
               (  To_Time
                  (  GDouble
                     (  Panel.Oscilloscope.Time_Axis
                        (  Panel.Sweeper
                        ) .Sweeper.Get_Upper
                     -  Offset
            )  )  )  );
         end;
      end if;
   exception
      when others =>
         null;
   end On_Changed_Offset;

   procedure On_Changed_Page
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Page_Span
      (  Panel.Sweeper,
         Duration (GDouble'(Value (Panel.Page_Edit.Get_Text)))
      );
   exception
      when others =>
         null;
   end On_Changed_Page;

   procedure On_Click_Faster
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      Panel.Page_Edit.Set_Text
      (  Image
         (  Value =>
               GDouble
               (  Panel.Oscilloscope.Get_Page_Span (Panel.Sweeper)
               /  Page_Step
               ),
            AbsSmall =>-Duration_Small
      )  );
   exception
      when others =>
         null;
   end On_Click_Faster;

   procedure On_Click_Forward
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Time
      (  Panel.Sweeper,
         Time'
         (  To_Time
            (  Panel.Oscilloscope.Time_Axis
               (  Panel.Sweeper
               ) .Sweeper.Get_Upper
      )  )  );
      Panel.Show_Offset;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Forward")
         )  );
   end On_Click_Forward;

   procedure On_Click_Hold
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Frozen (Panel.Sweeper, True);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Hold")
         )  );
   end On_Click_Hold;

   procedure On_Click_Left
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
      use type Ada.Calendar.Time;
   begin
      Panel.Oscilloscope.Set_Time
      (  Panel.Sweeper,
         (  Panel.Oscilloscope.Get_Time (Panel.Sweeper)
         +  Panel.Oscilloscope.Get_Page_Span (Panel.Sweeper)
         *  ((Page_Step - 1.0) * 0.1)
      )  );
      Panel.Show_Offset;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Left")
         )  );
   end On_Click_Left;

   procedure On_Click_Page_Left
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
      use type Ada.Calendar.Time;
   begin
      Panel.Oscilloscope.Set_Time
      (  Panel.Sweeper,
         (  Panel.Oscilloscope.Get_Time (Panel.Sweeper)
         +  Panel.Oscilloscope.Get_Page_Span (Panel.Sweeper)
         *  (Page_Step - 1.0)
      )  );
      Panel.Show_Offset;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Page_Left")
         )  );
   end On_Click_Page_Left;

   procedure On_Click_Page_Right
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
      use type Ada.Calendar.Time;
   begin
      Panel.Oscilloscope.Set_Time
      (  Panel.Sweeper,
         (  Panel.Oscilloscope.Get_Time (Panel.Sweeper)
         -  Panel.Oscilloscope.Get_Page_Span (Panel.Sweeper)
         *  (Page_Step - 1.0)
      )  );
      Panel.Show_Offset;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Page_Right")
         )  );
   end On_Click_Page_Right;

   procedure On_Click_Right
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
      use type Ada.Calendar.Time;
   begin
      Panel.Oscilloscope.Set_Time
      (  Panel.Sweeper,
         (  Panel.Oscilloscope.Get_Time (Panel.Sweeper)
         -  Panel.Oscilloscope.Get_Page_Span (Panel.Sweeper)
         *  ((Page_Step - 1.0) * 0.1)
      )  );
      Panel.Show_Offset;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Right")
         )  );
   end On_Click_Right;

   procedure On_Click_Run
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Frozen (Panel.Sweeper, False);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Run")
         )  );
   end On_Click_Run;

   procedure On_Click_Slower
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      Panel.Page_Edit.Set_Text
      (  Image
         (  Value =>
               GDouble
               (  Panel.Oscilloscope.Get_Page_Span (Panel.Sweeper)
               *  Page_Step
               ),
            AbsSmall =>-Duration_Small
      )  );
   exception
      when others =>
         null;
   end On_Click_Slower;

   procedure On_Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      if Panel.Oscilloscope /= null then
         Panel.Oscilloscope.Unref;
         Panel.Oscilloscope := null;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Destroy")
         )  );
   end On_Destroy;

   procedure On_Frozen_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
      use Hold_Buttons;
      use Run_Buttons;
   begin
      if Axis = Sweeper_Type'Pos (Panel.Sweeper) then
         if Panel.Oscilloscope.Get_Frozen (Panel.Sweeper) then
            if Panel.Hold_Button /= null then
               Panel.Remove (Panel.Hold_Button);
               Panel.Hold_Button := null;
            end if;
            if Panel.Run_Button = null then
               Gtk_New (Panel.Run_Button);
               Panel.Run_Button.Ref;
               Connect
               (  Panel.Run_Button,
                  "clicked",
                  On_Click_Run'Access,
                  Panel
               );
               Attach
               (  Panel,
                  Panel.Run_Button,
                  0, 1,
                  0, 1,
                  XOptions => Shrink,
                  YOptions => Shrink
               );
               Panel.Run_Button.Show_All;
               Panel.Show_Offset;
            end if;
         else
            if Panel.Run_Button /= null then
               Panel.Remove (Panel.Run_Button);
               Panel.Run_Button := null;
            end if;
            if Panel.Hold_Button = null then
               Gtk_New (Panel.Hold_Button);
               Panel.Hold_Button.Ref;
               Connect
               (  Panel.Hold_Button,
                  "clicked",
                  On_Click_Hold'Access,
                  Panel
               );
               Attach
               (  Panel,
                  Panel.Hold_Button,
                  0, 1,
                  0, 1,
                  XOptions => Shrink,
                  YOptions => Shrink
               );
               Panel.Hold_Button.Show_All;
               Panel.Show_Offset;
            end if;
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Frozen_Changed")
         )  );
   end On_Frozen_Changed;

   procedure On_Grid_Toggled
             (  Widget : access Gtk_Widget_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      if Axis = Sweeper_Type'Pos (Panel.Sweeper) then
         Panel.Grid_Button.Set_Active
         (  Panel.Oscilloscope.Get_Time_Grid (Panel.Sweeper)
         );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Grid_Toggled")
         )  );
   end On_Grid_Toggled;

   procedure On_Offset_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      if Axis = Sweeper_Type'Pos (Panel.Sweeper) then
         Panel.Show_Offset;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Offset_Changed")
         )  );
   end On_Offset_Changed;

   procedure On_Position_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Values : GValues;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
      Sweeper : constant GUInt   := Get_UInt   (Nth (Values, 1));
      Stamp   : constant GDouble := Get_Double (Nth (Values, 2));
      Diff    : constant GDouble := Get_Double (Nth (Values, 3));
      function Image_Span (Span : GDouble) return String is
      begin
         if Span >= 1_000.0 then
            return Image (Span, AbsSmall => 0) & "s";
         elsif Span >= 1.0 then
            return Image (Span, AbsSmall =>-3) & "s";
         elsif Span >= 0.001 then
            return Image (Span * 1_000.0, AbsSmall =>-3) & "ms";
         elsif Span >= 0.000_001 then
            return
            (  Image (Span * 1_000_000.0, AbsSmall =>-3)
            &  Character'Val (16#C2#)
            &  Character'Val (16#B5#)
            &  "s"
            );
         else
            return Image (Span * 1_000_000_000.0, AbsSmall=>-3) & "ns";
         end if;
      end Image_Span;
   begin
      if Sweeper = Sweeper_Type'Pos (Panel.Sweeper) then
         if Panel.Time_Stamp /= null then
            Panel.Time_Stamp.Set_Markup
            (  "<i>t</i><sub>2</sub>="
            &  Image (To_Time (Stamp))
            &  "   <i>t</i><sub>2</sub>-<i>t</i><sub>1</sub>="
            &  Image_Span (Diff)
            );
         end if;
      end if;
   exception
      when Constraint_Error =>
         null;
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Position_Changed")
         )  );
   end On_Position_Changed;

   procedure On_Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      if Panel.Axis_Button /= null then
         Panel.Axis_Button.Set_Label (Style_Get (Panel, "show-axis"));
      end if;
      if Panel.Grid_Button /= null then
         Panel.Grid_Button.Set_Label
         (  Style_Get (Panel, "show-graph-paper")
         );
      end if;
      Panel.Page_Edit.Set_Tooltip_Text (Style_Get (Panel, "page-tip"));
      Panel.Offset_Edit.Set_Tooltip_Text
      (  Style_Get (Panel, "offset-tip")
      );
      Set_Col_Spacings (Panel, Style_Get (Panel, "column-spacings"));
      Set_Row_Spacings (Panel, Style_Get (Panel, "row-spacings"));
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Style_Updated")
         )  );
   end On_Style_Updated;

   procedure On_Toggled_Axis
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Time_Axis
      (  Panel.Sweeper,
         Panel.Axis_Button.Get_Active
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggled_Axis")
         )  );
   end On_Toggled_Axis;

   procedure On_Toggled_Grid
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Time_Grid
      (  Panel.Sweeper,
         Panel.Grid_Button.Get_Active
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggled_Grid")
         )  );
   end On_Toggled_Grid;

   procedure Show_Offset
             (  Widget : not null access
                         Gtk_Oscilloscope_Sweeper_Panel_Record
             )  is
   begin
      if Widget.Oscilloscope.Get_Frozen (Widget.Sweeper) then
         Widget.Offset_Edit.Set_Text
         (  Image (Widget.Oscilloscope.Get_Time (Widget.Sweeper))
         );
      else
         Widget.Offset_Edit.Set_Text
         (  Image
            (  Value =>
                  GDouble
                  (  Widget.Oscilloscope.Get_Offset (Widget.Sweeper)
                  ),
               AbsSmall =>
                 -Duration_Small
         )  );
      end if;
   end Show_Offset;

end Gtk.Oscilloscope.Sweeper_Panel;
