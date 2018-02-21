--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     On_Line_Plotter                             Luebeck            --
--  Sample plotting using oscilloscope             Spring, 2012       --
--                                                                    --
--                                Last revision :  15:58 22 Jan 2012  --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;
with Gdk.Event;             use Gdk.Event;
with GLib;                  use GLib;
with GLib.Properties;       use GLib.Properties;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Toggle_Button;     use Gtk.Toggle_Button;
with Gtk.Dialog;            use Gtk.Dialog;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Missed;            use Gtk.Missed;
with Gtk.Label;             use Gtk.Label;
with Gtk.Layered.Waveform;  use Gtk.Layered.Waveform;
with Gtk.Oscilloscope;      use Gtk.Oscilloscope;
with Gtk.Progress_Bar;      use Gtk.Progress_Bar;
with Gtk.Stock;             use Gtk.Stock;
with Gtk.Table;             use Gtk.Table;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;

with Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Conversion;
with Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;
with Gtk.Main.Router;
with Worker;
with gtk.layered.line;
procedure On_Line_Plotter is
   Window          : Gtk_Window;
   Calculator      : Worker.Process;
   Start_Button    : Gtk_Button;
   Oscilloscope    : Gtk_Oscilloscope;
   Curve           : Channel_Number;
   Progress        : Gtk_Progress_Bar;
   Start_Frequency : Gtk_Entry;
   Stop_Frequency  : Gtk_Entry;
   Stiffness_Ratio : Gtk_Entry;
   Steps           : Gtk_Entry;
   Autoscale_Check : Gtk_Check_Button;
   Unicode_Check   : Gtk_Check_Button;
--
-- Delete_Event -- Window closing notification event
--
   function Delete_Event
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk_Event
            )  return Boolean is
   begin
      Calculator.Stop; -- Stop the computation process
      return False;    -- Confirm completion exception
   exception
      when Tasking_Error =>
         return False;
   end Delete_Event;
--
-- Value -- Get floating-point value from an entry widget
--
   function Value
            (  Edit : Gtk_Entry;
               Name : String;
               Min  : GDouble := GDouble'First;
               Max  : GDouble := GDouble'Last
            )  return GDouble is
   begin
      return Result : constant GDouble :=
                               GDouble'Value (Edit.Get_Text) do
         if Result not in Min..Max then
            raise Data_Error with Name & " out of range";
         end if;
      end return;
   exception
      when Constraint_Error =>
         raise Data_Error with "Wrong " & Name;
   end Value;
--
-- Value -- Get integer value from an entry widget
--
   function Value
            (  Edit : Gtk_Entry;
               Name : String;
               Min  : Integer := 1;
               Max  : Integer := Integer'Last
            )  return Integer is
   begin
      return Result : constant Integer :=
                               Integer'Value (Edit.Get_Text) do
         if Result not in Min..Max then
            raise Data_Error with Name & " out of range";
         end if;
      end return;
   exception
      when Constraint_Error =>
         raise Data_Error with "Wrong " & Name;
   end Value;
--
-- Save_Clicked -- Button "save to PDF"
--
   procedure Save_Clicked (Widget : access Gtk_Widget_Record'Class) is
      use Gtk.File_Chooser;
      use Gtk.File_Chooser_Dialog;
      use Gtk.Main.Router;
      Dialog : Gtk_File_Chooser_Dialog;
   begin
      Gtk_New (Dialog, "PDF file to save into", Window, Action_Save);
      Add_Button_From_Stock
      (  Dialog,
         Gtk_Response_OK,
         "_OK",
         Stock_OK
      );
      Add_Button_From_Stock
      (  Dialog,
         Gtk_Response_Cancel,
         "_Cancel",
         Stock_Cancel
      );
      case Dialog.Run is
         when Gtk_Response_OK =>
            Oscilloscope.Capture_PDF (Get_Filename (+Dialog));
         when others =>
            null;
      end case;
      Dialog.Destroy;
   exception
      when Error : others =>
         Say (Exception_Information (Error));
   end Save_Clicked;
--
-- Start_Clicked -- Button "start"
--
   procedure Start_Clicked (Widget : access Gtk_Widget_Record'Class) is
      use Gtk.Main.Router;
      From  : GDouble;
      To    : GDouble;
      Ratio : GDouble;
      Width : GDouble;
      Count : Positive;
   begin
      Start_Button.Set_Sensitive (False);
      From  := Value (Start_Frequency, "start frequency");
      To    := Value (Stop_Frequency,  "stop frequency");
      Count := Value (Steps,           "frequency steps");
      Ratio := Value (Stiffness_Ratio, "stiffness ratio", 0.0, 1.0);
      Width := To - From;

         -- Set page size of the scope
      Oscilloscope.Get_Sweeper (Lower).Configure
      (  Value => From,
         Lower => From,
         Upper => To,
         Step_Increment => Width / 100.0,
         Page_Increment => Width / 10.0,
         Page_Size      => Width
      );
         -- Initiate calculation process
      Calculator.Start
      (  (  Start     => Long_Float (From),
            Stop      => Long_Float (To),
            Steps     => Count,
            Stiffness => Long_Float (Ratio)
         ),
         Oscilloscope,
         Curve,
         Progress
      );
   exception
      when Error : Data_Error =>
          Say (Exception_Message (Error));
      when Error : others =>
          Say (Exception_Information (Error));
   end Start_Clicked;
--
-- Unicode_Toggled -- Check button toggling
--
   procedure Unicode_Toggled
             (  Widget : access Gtk_Widget_Record'Class
             )  is
   begin
      Oscilloscope.Set_Superscript (Unicode_Check.Get_Active);
   end Unicode_Toggled;
--
-- Autoscale_Toggled -- Check button toggling
--
   procedure Autoscale_Toggled
             (  Widget : access Gtk_Widget_Record'Class
             )  is
   begin
      Oscilloscope.Set_Auto_Scaling (Left, Autoscale_Check.Get_Active);
   end Autoscale_Toggled;
--
-- Circumvention of accessibility checks
--
   type Local_Widget_Callback is access procedure
        (  Widget : access Gtk_Widget_Record'Class
        );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Widget_Callback,
             Cb_Gtk_Toggle_Button_Void
          );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Widget_Callback,
             Cb_Gtk_Button_Void
          );
   type Local_Delete_Callback is access function
        (  Widget : access Gtk_Widget_Record'Class;
           Event  : Gdk_Event
        )  return Boolean;
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Delete_Callback,
             Cb_Gtk_Widget_Gdk_Event_Boolean
          );
begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window); -- Initialize routing
   Window.Set_Title ("Sample on-line plotting");
   Window.On_Delete_Event (+Delete_Event'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);

   declare
      Main_Box : Gtk_HBox;
   begin
      Gtk_New_HBox (Main_Box);
      Main_Box.Set_Spacing (3);
      Main_Box.Set_Border_Width (3);
      Add (Window, Main_Box);
      declare -- Box on the left
         Left_Box : Gtk_VBox;
      begin
         Gtk_New_VBox (Left_Box);
         Left_Box.Set_Spacing (3);
         Main_Box.Pack_Start (Left_Box, False, False);
         declare -- Parameters in the left box
            Parameters : Gtk_Table;
            procedure Create
                      (  Edit  : out Gtk_Entry;
                         Row   : GUInt;
                         Label : String;
                         Init  : String
                      )  is
               Annotation : Gtk_Label;
            begin
               Gtk_New (Annotation, Label);
               Annotation.Set_Halign (Align_End);
               Annotation.Set_Valign (Align_Center);
--             Annotation.Set_Alignment (1.0, 0.5);
               Parameters.Attach
               (  Annotation,
                  0, 1, Row, Row + 1,
                  XOptions => Fill,
                  YOptions => Shrink
               );
               Gtk_New (Edit);
               Edit.Set_Width_Chars (10);
               if Find_Property (Edit, "max-width-chars") /= null then
                  Set_Property
                  (  Edit,
                     Build ("max-width-chars"),
                     GInt'(10)
                  );
               end if;
               Edit.Set_Text (Init);
               Parameters.Attach
               (  Edit,
                  1, 2, Row, Row + 1,
                  XOptions => Fill or Expand,
                  YOptions => Shrink
               );
            end Create;
         begin
            Gtk_New (Parameters, 6, 2, False);
            Parameters.Set_Row_Spacings (3);
            Parameters.Set_Col_Spacings (3);
            Left_Box.Pack_Start (Parameters);
            Create (Start_Frequency, 0, "Start frequency", "-44.88");
            Create (Stop_Frequency,  1, "Stop frequency",  "67.32");
            Create (Steps,           2, "Steps",           "300");
            Create (Stiffness_Ratio, 3, "Stiffness ratio", "1");
            declare
               Label : Gtk_Label;
            begin
               Gtk_New (Label, "Autoscale Y");
               Label.Set_Halign (Align_End);
               Label.Set_Valign (Align_Center);
--             Label.Set_Alignment (1.0, 0.5);
               Parameters.Attach
               (  Label,
                  0, 1, 4, 5,
                  XOptions => Fill,
                  YOptions => Shrink
               );
               Gtk_New (Autoscale_Check);
               Autoscale_Check.Set_Active (True);
               Parameters.Attach
               (  Autoscale_Check,
                  1, 2, 4, 5,
                  XOptions => Fill,
                  YOptions => Shrink
               );
               Autoscale_Check.On_Toggled (+Autoscale_Toggled'Access);
            end;
            declare
               Label : Gtk_Label;
            begin
               Gtk_New (Label, "Use Unicode");
               Label.Set_Halign (Align_End);
               Label.Set_Valign (Align_Center);
--             Label.Set_Alignment (1.0, 0.5);
               Parameters.Attach
               (  Label,
                  0, 1, 5, 6,
                  XOptions => Fill,
                  YOptions => Shrink
               );
               Gtk_New (Unicode_Check);
               Unicode_Check.Set_Active (True);
               Parameters.Attach
               (  Unicode_Check,
                  1, 2, 5, 6,
                  XOptions => Fill,
                  YOptions => Shrink
               );
               Unicode_Check.On_Toggled (+Unicode_Toggled'Access);
            end;
         end;
         declare -- Save button
            Box    : Gtk_HBox;
            Button : Gtk_Button;
         begin
            Gtk_New_HBox (Box);
            Box.Set_Spacing (3);
            Left_Box.Pack_Start (Box, False, False);
            Gtk_New (Button, "Save to PDF");
            Box.Pack_Start (Button, False, False);
            Button.On_Clicked (+Save_Clicked'Access);
         end;
         declare -- Start button and progress bar in the left box
            Box : Gtk_HBox;
         begin
            Gtk_New_HBox (Box);
            Box.Set_Spacing (3);
            Left_Box.Pack_Start (Box, False, False);
            Gtk_New (Start_Button, "Start");
            Box.Pack_Start (Start_Button, False, False);
            Start_Button.On_Clicked (+Start_Clicked'Access);
            Gtk_New (Progress);
            Box.Pack_Start (Progress);
         end;
      end;
      declare -- Frame with the oscilloscope on the right
         Frame : Gtk_Frame;
      begin
         Gtk_New (Frame);
         Frame.Set_Shadow_Type (Shadow_In);
         Main_Box.Pack_Start (Frame);
         Gtk_New (Oscilloscope);
         Frame.Add (Oscilloscope);
         Oscilloscope.Set_Manual_Sweep (False);
         --
         -- Configuring the lower axis
         --
         Oscilloscope.Set_Frozen     (Lower, True);  -- No sweeping
         Oscilloscope.Set_Time_Scale (Lower, False); -- No scale (slider)
         Oscilloscope.Set_Time_Grid  (Lower, True);  -- Grid
         Oscilloscope.Set_Time_Axis
         (  Lower,
            True,  -- Visible
            False  -- As plain numbers
         );
         --
         -- Adding the channel
         --
         Curve :=
            Add_Channel
            (  Widget  => Oscilloscope,
               Mode    => Gtk.Layered.Linear, -- Linear interpolation
               Color   => RGB (0.0, 0.0, 0.7),
               Sweeper => Lower
            );
         --
         -- Configuring the left axis for this channel (and its group)
         --
         Oscilloscope.Set_Group (Left, Oscilloscope.Get_Group (Curve));
         Oscilloscope.Set_Values_Axis  (Left, True);
         Oscilloscope.Set_Values_Scale (Left, True);
         Oscilloscope.Set_Values_Grid  (Left, True);
         Oscilloscope.Set_Values_Axis_Width (Left, 80);
      end;
   end;
   Window.Set_Default_Size (800, 400);
   Show_All (Window);
   Gtk.Main.Main;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end On_Line_Plotter;
