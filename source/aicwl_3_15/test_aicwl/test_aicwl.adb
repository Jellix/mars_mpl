--                                                                    --
--  procedure Test_AICWL            Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Autumn, 2010       --
--                                                                    --
--                                Last revision :  09:44 08 Oct 2016  --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Numerics;                 use Ada.Numerics;
with Cairo;                        use Cairo;
with Cairo.Ellipses;               use Cairo.Ellipses;
with Gdk.Color;                    use Gdk.Color;
with Gdk.Event;                    use Gdk.Event;
with Gdk.Rectangle;                use Gdk.Rectangle;
with GLib;                         use GLib;
with GLib.Messages;                use GLib.Messages;
with GLib.Values;                  use GLib.Values;
with Gtk.Adjustment;               use Gtk.Adjustment;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Button;                   use Gtk.Button;
with Gtk.Check_Button;             use Gtk.Check_Button;
with Gtk.Cell_Renderer;            use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;       use Gtk.Cell_Renderer_Text;
with Gtk.Combo_Box;                use Gtk.Combo_Box;
with Gtk.Dialog;                   use Gtk.Dialog;
with Gtk.Editable;                 use Gtk.Editable;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Enums.String_Lists;       use Gtk.Enums.String_Lists;
with Gtk.File_Chooser;             use Gtk.File_Chooser;
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.GEntry;                   use Gtk.GEntry;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Layered;                  use Gtk.Layered;
with Gtk.Layered.Arc;              use Gtk.Layered.Arc;
with Gtk.Layered.Bar;              use Gtk.Layered.Bar;
with Gtk.Layered.Clock_Hand;       use Gtk.Layered.Clock_Hand;
with Gtk.Layered.Cap;              use Gtk.Layered.Cap;
with Gtk.Layered.Digital;          use Gtk.Layered.Digital;
with Gtk.Layered_Editor;           use Gtk.Layered_Editor;
with Gtk.Layered.Elliptic_Bar;     use Gtk.Layered.Elliptic_Bar;
with Gtk.Layered.Elliptic_Scale;   use Gtk.Layered.Elliptic_Scale;
with Gtk.Layered.Graph_Paper;      use Gtk.Layered.Graph_Paper;
with Gtk.Layered.Flat_Scale;       use Gtk.Layered.Flat_Scale;
with Gtk.Layered.Label;            use Gtk.Layered.Label;
with Gtk.Layered.Line;             use Gtk.Layered.Line;
with Gtk.Layered.Needle;           use Gtk.Layered.Needle;
with Gtk.Layered.Refresh_Engine;   use Gtk.Layered.Refresh_Engine;
with Gtk.Layered.Sector_Needle;    use Gtk.Layered.Sector_Needle;
with Gtk.Layered.Stream_IO;        use Gtk.Layered.Stream_IO;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Scale;                    use Gtk.Scale;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Stock;                    use Gtk.Stock;
with Gtk.Paned;                    use Gtk.Paned;
with Gtk.Window;                   use Gtk.Window;
with Gtk.Widget;                   use Gtk.Widget;
with Gtk.Table;                    use Gtk.Table;
with Gtk.Toggle_Button;            use Gtk.Toggle_Button;
with Gtk.Tree_Model;               use Gtk.Tree_Model;
with Gtk.Tree_Selection;           use Gtk.Tree_Selection;
with Gtk.Tree_Store;               use Gtk.Tree_Store;
with Gtk.Tree_View;                use Gtk.Tree_View;
with Gtk.Tree_View_Column;         use Gtk.Tree_View_Column;
with Gtk.Widget.Styles.CSS_Store;  use Gtk.Widget.Styles.CSS_Store;
with Pango.Cairo.Fonts;            use Pango.Cairo.Fonts;
with Pango.Font;                   use Pango.Font;
with Strings_Edit.Streams;         use Strings_Edit.Streams;
with Strings_Edit.Floats;          use Strings_Edit.Floats;
with System;                       use System;
with Test_Generator;               use Test_Generator;
with Test_Clock;                   use Test_Clock;

with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Strings;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Cairo.PDF;
with Gtk.File_Chooser_Dialog;
with Gtk.Handlers;
with Gtk.Gauge.Flat_Horizontal;
with Gtk.Gauge.Flat_Vertical;
with Gtk.Gauge.Elliptic_180;
with Gtk.Gauge.LED_Rectangular;
with Gtk.Gauge.LED_Round;
with Gtk.Gauge.Rectangular_70s;
with Gtk.Gauge.Rectangular_70s_Slanted;
with Gtk.Gauge.Round_90;
with Gtk.Gauge.Round_110;
with Gtk.Gauge.Round_180;
with Gtk.Gauge.Round_254;
with Gtk.Gauge.Round_270;
with Gtk.Gauge.Round_270_60s;
with Gtk.Gauge.Round_270_Inout;
with Gtk.Gauge.Round_270_Outer;
with Gtk.Gauge.Round_270_Reversed;
with Gtk.Layered.Graph_Paper_Annotation;
with Gtk.Layered.Waveform;
with Gtk.Main.Router.GNAT_Stack;
with Gtk.Meter.Angular_90;
with Gtk.Meter.Elliptic_90;
with Gtk.Meter.Round_90;
with Gtk.Meter.Round_94;
with Gtk.Meter.Thermo;
with Gtk.Meter.Thermo_Dual;
with Gtk.Meter.Thermo_Symmetric;
with Gtk.Wall_Clock.Imperial;
with Gtk.Wall_Clock.Modern;
with Gtk.Wall_Clock.Classic;

with Gtk.Layered.Elliptic_Annotation;
use  Gtk.Layered.Elliptic_Annotation;

with Gtk.Layered.Elliptic_Background;
use  Gtk.Layered.Elliptic_Background;

with Gtk.Layered.Flat_Annotation;
use  Gtk.Layered.Flat_Annotation;

with Gtk.Layered.Rectangular_Background;
use  Gtk.Layered.Rectangular_Background;

with Gtk.Layered.Waveform.Amplifier;
with Gtk.Layered.Waveform.Sweeper;

procedure Test_AICWL is
   File_Name : constant String := "test_aicwl.css";
   PDF_Name  : constant String := "test_aicwl.pdf";
   --
   -- All data are global, for the sake of  simplicity.  Otherwise,  the
   -- test were impossible to keep in  just  one  body  due  to  Ada  95
   -- restriction on controlled types.
   --
   type Waveform_Layer_Ptr is
     access all Gtk.Layered.Waveform.Waveform_Layer;

   Window         : Gtk_Window;
   Wave           : Waveform_Layer_Ptr;
   Main_Pane      : Gtk_HPaned;
   Test_Pane      : Gtk_VPaned;
   Test_Widget    : Gtk_Widget;
   Get_CSS_Button : Gtk_Button;
   Get_PDF_Button : Gtk_Button;
   Load_Button    : Gtk_Button;
   Save_Button    : Gtk_Button;
   Editor         : Gtk_Layered_Editor;
   Engine         : Layered_Refresh_Engine;
   Offset_Edit    : Gtk_Entry;

   procedure Test_IO is separate;
   procedure Test_Ring_Buffer is separate;

   type Local_Editable_Callback is access
      procedure (Editable : Gtk_Editable);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Editable_Callback,
             Cb_Gtk_Editable_Void
          );
   type Local_Widget_Callback is access
      procedure (Widget : access Gtk_Button_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Widget_Callback,
             Cb_Gtk_Button_Void
          );
   type Local_Selection_Callback is access
      procedure (Selection : access Gtk_Tree_Selection_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Selection_Callback,
             Cb_Gtk_Tree_Selection_Void
          );
   type Local_Toggle_Callback is access
      procedure (Toggle : access Gtk_Toggle_Button_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Toggle_Callback,
             Cb_Gtk_Toggle_Button_Void
          );
   type Local_LED_Round_Callback is access procedure
        (  Widget : access Gtk_Button_Record'Class;
           LED    : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round
        );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_LED_Round_Callback,
             LED_Round_Handlers.Simple_Handler
          );
   type Local_LED_Rectangular_Callback is access procedure
        (  Widget : access Gtk_Button_Record'Class;
           LED    : Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
        );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_LED_Rectangular_Callback,
             LED_Rectangular_Handlers.Simple_Handler
          );

   procedure On_Get_PDF (Button : access Gtk_Button_Record'Class) is
      use Ada.Text_IO;
      use Cairo.PDF;
      Layered : Gtk_Layered;
      Target  : Cairo_Surface;
   begin
      if Editor /= null then
         Layered := Editor.Get;
         if Layered /= null then
            Target :=
               Create
               (  Filename =>
                     PDF_Name,
                  Width_In_Points =>
                     GDouble (Get_Allocated_Width (Layered)),
                  Height_In_Points =>
                     GDouble (Get_Allocated_Height (Layered))
               );
            Layered.Snapshot (Target);
            Set_Label
            (  Get_PDF_Button,
               PDF_Name & " written. Press to rewrite"
            );
            Surface_Destroy (Target);
         end if;
      end if;
   end On_Get_PDF;

   procedure On_Get_RC (Button : access Gtk_Button_Record'Class) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      if Test_Widget /= null then
         Create (File, Out_File, File_Name);
         Put_CSS_Styles (File, Test_Widget);
         Close (File);
         Set_Label
         (  Get_CSS_Button,
            File_Name & " written. Press to rewrite"
         );
      end if;
   end On_Get_RC;

   procedure On_Load (Button : access Gtk_Button_Record'Class) is
      use Ada.Streams.Stream_IO;
      use Gtk.File_Chooser_Dialog;
      Dialog : Gtk_File_Chooser_Dialog;
      Action : constant Gtk_File_Chooser_Action := Action_Open;
   begin
      Editor.Get.Erase;
      Gtk_New (Dialog, "Select file to load", Window, Action);
      Add_Button_From_Stock
      (  Dialog,
         Gtk_Response_Accept,
         "_OK",
         Stock_OK
      );
      if Run (Gtk_Dialog (Dialog)) = Gtk_Response_Accept then
         declare
            File : File_Type;
         begin
            Open (File, In_File, Get_Filename (+Dialog));
            begin
               Restore (Stream (File).all, Editor.Get);
               Close (File);
            exception
               when Error : others =>
                  Close (File);
                  Gtk.Main.Router.GNAT_Stack.Trace (Error);
            end;
            Queue_Draw (Editor.Get);
         exception
            when Error : others =>
               Gtk.Main.Router.GNAT_Stack.Trace (Error);
         end;
      end if;
      Dialog.Destroy;
   end On_Load;

   procedure On_Save (Button : access Gtk_Button_Record'Class) is
      use Ada.Streams.Stream_IO;
      use Gtk.File_Chooser_Dialog;
      Dialog : Gtk_File_Chooser_Dialog;
      Action : constant Gtk_File_Chooser_Action := Action_Save;
   begin
      Gtk_New (Dialog, "Select file to save", Window, Action);
      Add_Button_From_Stock
      (  Dialog,
         Gtk_Response_Accept,
         "_OK",
         Stock_OK
      );
      if Run (Gtk_Dialog (Dialog)) = Gtk_Response_Accept then
         declare
            File : File_Type;
         begin
            Create (File, Out_File, Get_Filename (+Dialog));
            begin
               Store (Stream (File).all, Editor.Get);
               Close (File);
            exception
               when Error : others =>
                  Close (File);
                  Gtk.Main.Router.GNAT_Stack.Trace (Error);
            end;
         exception
            when Error : others =>
               Gtk.Main.Router.GNAT_Stack.Trace (Error);
         end;
      end if;
      Dialog.Destroy;
   end On_Save;

   procedure On_Selection
             (  Selection : access Gtk_Tree_Selection_Record'Class
             )  is
      type Local_Callback is access function return Gtk_Widget;
      function Call is
         new Ada.Unchecked_Conversion (System.Address, Local_Callback);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Value : GValue;
   begin
      Get_Selected (Selection, Model, Iter);
      if Iter /= Null_Iter and then not Has_Child (Model, Iter) then
         Get_Value (Model, Iter, 1, Value);
         if Test_Widget /= null then
            Remove (Test_Pane, Test_Widget);
            Test_Widget := null;
         end if;
         Test_Widget := Call (Get_Address (Value)).all;
         Add2 (Test_Pane, Test_Widget);
         Show_All (Test_Widget);
         Unset (Value);
      end if;
   exception
      when Error : others =>
         Gtk.Main.Router.GNAT_Stack.Trace (Error);
   end On_Selection;
--
-- Individual tests
--
   function Test_1 return Gtk_Widget is
      Widget : Gtk_Layered;
   begin
      Gtk_New (Widget);

      Add_Elliptic_Background
      (  Under         => Widget,
         Outer         => ((0.25, 0.25), 1.0 / 0.25, 0.25, 0.0),
         Color         => RGB (0.2, 0.5, 0.5),
         Scaled        => True,
         Border_Width  => 8.0,
--Border_Width => 40.0,
--border_color => (false, rgb(0.5,0.0,0.0)),
         Border_Shadow => Shadow_Out
--Border_Shadow => Shadow_None
--Border_Shadow => Shadow_in
--Border_Shadow => Shadow_Etched_Out
--Border_Shadow => Shadow_Etched_In
      );
      Add_Cap
      (  Under         => Widget,
         Center        => (0.25, -0.25),
         Radius        => 0.25,
         Scaled        => True,
         Border_Width  => 25.0,
         Border_Color  => (False, RGB (0.0, 5.0, 0.5)),
         Border_Shadow => Shadow_None
      );
      Add_Cap
      (  Under         => Widget,
         Center        => (100.0, 100.0),
         Radius        => 50.0,
         To            => RGB (0.0, 0.0, 0.0),
         Border_Width  => 25.0,
         Border_Color  => (False, RGB (0.0, 5.0, 0.5)),
         Border_Shadow => Shadow_None
      );

      Add_Label
      (  Under    => Widget,
         Text     => "Rotated",
         Face     => Create_Pango ("Sans"),
         Location => (150.0, 200.0),
         Angle    => 0.0,
         Skew     => Pi / 4.0,
         Mode     => Skewed
      );
      Add_Label
      (  Under    => Widget,
         Text     => "Moved",
         Location => (150.0, 50.0),
         Mode     => Moved_Centered
      );
      Add_Label
      (  Under    => Widget,
         Text     => "Rotated",
         Location => (150.0, 100.0),
         Angle    => Pi / 6.0,
         Mode     => Rotated
      );
      Add_Label
      (  Under    => Widget,
         Text     => "Rotated",
         Location => (150.0, 150.0),
         Angle    => Pi / 6.0,
         Skew     => Pi / 4.0,
         Mode     => Skewed
      );
      Add_Elliptic_Scale
      (  Under => Widget,
         Outer => (  Center          => (100.0, 100.0),
                     Major_Curvature => 0.0,
                     Minor_Radius    => 30.0,
                     Angle           => 0.0
                  ),
         Inner => (  Center          => (100.0, 100.0),
                     Major_Curvature => 0.0,
                     Minor_Radius    => 10.0,
                     Angle           => 0.0
                  ),
         Color  => Parse ("white"),
         Width  => 1.0,
         Step   => Pi / 10.0,
         From   => Pi / 4.0,
         Length => Pi / 2.0
      );
      Add_Elliptic_Scale
      (  Under => Widget,
         Outer => (  Center          => (100.0, 100.0),
                     Major_Curvature => 0.0,
                     Minor_Radius    => 30.0,
                     Angle           => 0.0
                  ),
         Inner => (  Center          => (100.0, 100.0),
                     Major_Curvature => 0.0,
                     Minor_Radius    => 10.0,
                     Angle           => 0.0
                  ),
         Color  => Parse ("green"),
         Width  => 1.0,
         Step   => Pi / 5.0,
         From   => Pi * 5.0 / 4.0,
         Length => Pi / 2.0
      );

      Add_Arc
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 0.0, 90.0,  Pi / 2.0),
         From    => Pi * 3.0 / 4.0,
         Length  => Pi / 4.0,
         Color   => Parse ("white")
      );
      Add_Arc
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 0.0, 90.0, Pi / 4.0),
         From    => Pi / 2.0,
         Length  => Pi / 2.0,
         Color   => Parse ("white")
      );
      Add_Arc
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 0.0, 90.0, Pi),
         From    => Pi + Pi / 4.0,
         Length  => Pi / 2.0,
         Color   => Parse ("white")
      );
      Add_Arc
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 0.0, 90.0, 0.0),
         From    => Pi / 4.0,
         Length  => Pi / 2.0,
         Color   => Parse ("white")
      );
      Add_Arc
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 1.0 / 90.0, 90.0, 0.0),
         Color   => Parse ("red")
      );
      Add_Elliptic_Annotation
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 1.0 / 60.0, 60.0, Pi),
         Texts   => "-A-"/"-B-"/"-C-"/"-D-"/"-E-"/"-F-"/"-G-"/"-H-",
         Step    => Pi / 4.0,
         Color   => Parse ("blue"),
         From    => Pi,
         Length  => 2.0 * Pi,
         Mode    => Rotated
      );
      Add_Elliptic_Annotation
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 1.0 / 90.0, 90.0, Pi / 6.0),
         Texts   => "I II III IV V VI VII VIII IX X XI XII",
         Step    => Pi / 6.0,
         From   => -2.0 * Pi / 6.0
      );
      Add_Arc
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 1.0 / 60.0, 60.0, 0.0),
         Color   => Parse ("gray")
      );
      Add_Arc
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 1.0 / 75.0, 50.0, Pi / 6.0),
         From    => 0.0,--Pi,
         Length  => Pi,-- * 2.0,
         Color   => Parse ("green")
      );
      Add_Arc
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 1.0 / 80.0, 80.0, 0.0),
         From    => Pi * 5.0 / 4.0,
         Length  => Pi * 0.5,
         Color   => Parse ("blue")
      );
      Add_Arc
      (  Under   => Widget,
         Ellipse => ((100.0, 100.0), 1.0 / 85.0, 85.0, 0.0),
         From    => Pi * 5.0 / 4.0,
         Length  => -Pi * 0.5,
         Color   => Parse ("blue")
      );

      Add_Elliptic_Scale
      (  Under   => Widget,
         Inner   => ((100.0, 100.0), 1.0 / 75.0, 50.0, Pi / 6.0),
         Outer   => ((100.0, 100.0), 1.0 / 85.0, 80.0, 0.0),
         Step    => Pi / 50.0,
         Skipped => 5,
         From    => 0.0,
         Length  => Pi
      );
      Add_Elliptic_Scale
      (  Under  => Widget,
         Inner  => ((100.0, 100.0), 1.0 / 75.0, 50.0, Pi / 6.0),
         Outer  => ((100.0, 100.0), 1.0 / 90.0, 90.0, 0.0),
         Step   => Pi / 5.0,
         From   => 0.0,
         Length => Pi
      );
   --
   -- Thick borders
   --
      Add_Elliptic_Background
      (  Under => Widget,
         Outer => ((330.0, 100.0), 1.0 / 80.0, 80.0, 0.0),
         Color => RGB (1.0, 0.0, 0.0),
         Border_Width  => 8.0,
         Border_Shadow => Shadow_In
      );
      Add_Elliptic_Background
      (  Under => Widget,
         Outer => ((500.0, 100.0), 1.0 / 80.0, 80.0, 0.0),
         Color => RGB (0.0, 1.0, 0.0),
         Border_Width  => 8.0,
         Border_Shadow => Shadow_Out
      );
      Add_Elliptic_Background
      (  Under => Widget,
         Outer => ((700.0, 100.0), 1.0 / 80.0, 80.0, 0.0),
         Color => RGB (0.0, 0.0, 1.0),
         Border_Width  => 8.0,
         Border_Shadow => Shadow_Etched_In
      );
      Add_Elliptic_Background
      (  Under => Widget,
         Outer => ((900.0, 100.0), 1.0 / 80.0, 80.0, 0.0),
         Color => RGB (1.0, 0.0, 1.0),
         Border_Width  => 8.0,
         Border_Shadow => Shadow_Etched_Out
      );
   --
   -- Zero borders
   --
      Add_Elliptic_Background
      (  Under => Widget,
         Outer => ((1100.0, 100.0), 1.0 / 80.0, 80.0, 0.0),
         Color => RGB (1.0, 0.0, 0.0),
         Border_Shadow => Shadow_In
      );
      Add_Elliptic_Background
      (  Under => Widget,
         Outer => ((1100.0, 300.0), 1.0 / 80.0, 80.0, 0.0),
         Color => RGB (0.0, 1.0, 0.0),
         Border_Shadow => Shadow_Out
      );
      Add_Elliptic_Background
      (  Under => Widget,
         Outer => ((1100.0, 500.0), 1.0 / 80.0, 80.0, 0.0),
         Color => RGB (0.0, 0.0, 1.0),
         Border_Shadow => Shadow_Etched_In
      );
      Add_Elliptic_Background
      (  Under => Widget,
         Outer => ((1100.0, 700.0), 1.0 / 80.0, 80.0, 0.0),
         Color => RGB (1.0, 0.0, 1.0),
         Border_Shadow => Shadow_Etched_Out
      );
   --
   -- Shapes of backgrounds
   --
      Add_Elliptic_Background
      (  Under  => Widget,
         Outer  => ((100.0, 300.0), 1.0 / 80.0, 80.0, 0.0),
         Inner  => ((100.0, 300.0), 1.0 / 20.0, 20.0, 0.0),
         From   => Pi * 3.0 / 4.0,
         Length => Pi * 6.0 / 4.0,
         Color  => RGB (0.0, 1.0, 0.0),
         Border_Width  => 8.0,
         Border_Shadow => Shadow_Out
      );

      Add_Rectangular_Background
      (  Under         => Widget,
         Height        => 20.0,
         Width         => 30.0,
         Center        => (200.0, 50.0),
         Corner_Radius => 5.0
      );

      Add_Rectangular_Background
      (  Under          => Widget,
         Height         => 20.0,
         Width          => 30.0,
         Center         => (200.0, 200.0),
         Corner_Radius  => 5.0,
         Rotation_Angle => 0.5
      );

      Add_Elliptic_Background
      (  Under  => Widget,
         Outer  => ((300.0, 300.0), 1.0 / 80.0, 80.0, 0.0),
         Inner  => ((300.0, 300.0), 1.0 / 20.0, 20.0, 0.0),
         From   => Pi * 5.0 / 4.0,
         Length => Pi * 6.0 / 4.0,
         Color  => RGB (0.0, 1.0, 1.0),
         Border_Width  => 8.0,
         Border_Shadow => Shadow_Etched_In
      );

      Add_Elliptic_Background
      (  Under  => Widget,
         Outer  => ((550.0, 350.0), 1.0 / 80.0, 80.0, 0.0),
         From   => Pi * 3.0 / 4.0,
         Length => Pi * 6.0 / 4.0,
         Color  => RGB (1.0, 1.0, 0.0),
         Border_Width  => 8.0,
         Border_Shadow => Shadow_Etched_Out
      );

      Add_Elliptic_Background
      (  Under  => Widget,
         Outer  => ((500.0, 300.0), 1.0 / 80.0, 80.0, 0.0),
         From   => Pi * 3.0 / 4.0,
         Length => Pi / 2.0,
         Color  => RGB (1.0, 0.0, 1.0)
      );

      Add_Elliptic_Background
      (  Under  => Widget,
         Outer  => ((700.0, 300.0), 1.0 / 80.0, 80.0, Pi / 2.0),
         Inner  => ((700.0, 300.0), 0.0, 10.0, Pi / 2.0),
         From   => Pi * 3.0 / 4.0,
         Length => Pi / 2.0,
         Color        => RGB (0.0, 0.0, 1.0),
         Border_Width => 8.0,
         Border_Color => (False, RGB (0.6, 0.4, 0.4))
      );

      declare
         Center : constant Cairo_Tuple := (200.0, 550.0);
         Size   : constant GDouble      := 150.0;
      begin
         Add_Elliptic_Background
         (  Under => Widget,
            Outer => (Center, 1.0 / Size, Size, 0.0),
            Color => RGB (0.0, 0.0, 0.0)
         );
         Add_Arc
         (  Under   => Widget,
            Ellipse => (Center, 1.0 / (Size * 0.90), Size * 0.90, 0.0),
            Color   => RGB (0.5, 0.5, 0.5),
            Width   => Size / 200.0
         );
         Add_Elliptic_Scale
         (  Under   => Widget,
            Inner   => (  Center,
                          1.0 / (Size * 0.81),
                          Size * 0.81,
                          Pi * 3.0 / 4.0
                       ),
            Outer   => (  Center,
                          1.0 / (Size * 0.87),
                          Size * 0.87,
                          Pi * 3.0 / 4.0
                       ),
            Color   => RGB (1.0, 1.0, 1.0),
            Width   => Size / 200.0,
            Skipped => 5,
            Step    => Pi / 40.0,
            From    => Pi * 3.0 / 4.0,
            Length  => 6.0 * Pi / 4.0
         );
         Add_Elliptic_Scale
         (  Under   => Widget,
            Inner   => (  Center,
                          1.0 / (Size * 0.76),
                          Size * 0.76,
                          Pi * 3.0 / 4.0
                       ),
            Outer   => (  Center,
                          1.0 / (Size * 0.85),
                          Size * 0.85,
                          Pi * 3.0 / 4.0
                       ),
            Color   => RGB (1.0, 1.0, 1.0),
            Width   => 2.0 * Size / 200.0,
            Skipped => 2,
            Step    => Pi / 8.0,
            From    => Pi * 3.0 / 4.0,
            Length  => 6.0 * Pi / 4.0
         );
         Add_Elliptic_Scale
         (  Under   => Widget,
            Inner   => (  Center,
                          1.0 / (Size * 0.73),
                          Size * 0.73,
                          Pi * 3.0 / 4.0
                       ),
            Outer   => (  Center,
                          1.0 / (Size * 0.90),
                          Size * 0.90,
                          Pi * 3.0 / 4.0
                       ),
            Color   => RGB (1.0, 1.0, 1.0),
            Width   => 4.0 * Size / 200.0,
            Step    => Pi / 4.0,
            From    => Pi * 3.0 / 4.0,
            Length  => 6.0 * Pi / 4.0
         );
         Add_Elliptic_Annotation
         (  Under   => Widget,
            Ellipse => (  Center,
                          1.0 / (Size * 0.66),
                          Size * 0.66,
                          Pi * 3.0 / 4.0
                       ),
            Texts   => "0 20 40 60 80 100 120",
            Face    =>
               Create_Toy
               (  Family => "arial",
                  Slant  => CAIRO_FONT_SLANT_ITALIC,
                  Weight => CAIRO_FONT_WEIGHT_BOLD
               ),
            Step    => Pi / 4.0,
            Height  => Size * 0.12,
            Color   => RGB (1.0, 1.0, 1.0),
            From    => Pi * 3.0 / 4.0,
            Length  => 6.0 * Pi / 4.0,
            Mode    => Moved_Inside
         );
         Add_Clock_Hand
         (  Under         => Widget,
            Center        => Center,
            From          => Pi * 3.0 / 4.0,
            Length        => 6.0 * Pi / 4.0,
            Tip_Length    => Size * 0.8,
            Tip_Width     => Size * 0.07,
            Tip_Cap       => CAIRO_LINE_CAP_SQUARE,
            Rear_Length   => Size * 0.2,
            Rear_Width    => Size * 0.1,
            Rear_Cap      => CAIRO_LINE_CAP_ROUND,
            Bulb_Position => Size * 0.4,
            Bulb_Radius   => Size * 0.2,
            Bulb_Width    => Size * 0.07,
            Color         => Parse ("magenta")
         ) .Set_Value (0.2);
         Add_Needle
         (  Under       => Widget,
            Center      => Center,
            From        => Pi * 3.0 / 4.0,
            Length      => 6.0 * Pi / 4.0,
            Tip_Length  => Size * 0.8,
            Tip_Width   => Size * 0.07,
            Tip_Cap     => CAIRO_LINE_CAP_ROUND,
            Rear_Length => Size * 0.2,
            Rear_Width  => Size * 0.1,
            Rear_Cap    => CAIRO_LINE_CAP_SQUARE
         );
         Add_Needle
         (  Under       => Widget,
            Center      => Center,
            From        => Pi * 3.0 / 4.0,
            Length      => 6.0 * Pi / 4.0,
            Tip_Length  => Size * 0.8,
            Tip_Width   => Size * 0.07,
            Tip_Cap     => CAIRO_LINE_CAP_BUTT,
            Rear_Length => Size * 0.2,
            Rear_Width  => Size * 0.1,
            Rear_Cap    => CAIRO_LINE_CAP_BUTT,
            Color       => Parse ("green")
         ) .Set_Value (0.4);
         Add_Needle
         (  Under       => Widget,
            Center      => Center,
            From        => Pi * 3.0 / 4.0,
            Length      => 6.0 * Pi / 4.0,
            Tip_Length  => Size * 0.8,
            Tip_Width   => Size * 0.07,
            Tip_Cap     => CAIRO_LINE_CAP_SQUARE,
            Rear_Length => Size * 0.2,
            Rear_Width  => Size * 0.1,
            Rear_Cap    => CAIRO_LINE_CAP_ROUND,
            Color       => Parse ("blue")
         ) .Set_Value (0.6);
      end;

      declare
         Center : constant Cairo_Tuple := (550.0, 550.0);
         Size   : constant GDouble     := 350.0;
         Color  : Gdk_Color;
         Height : constant GDouble := Size / 5.0;
         Length : constant GDouble := 1.3 * Pi / 2.0;
         Pin    : constant GDouble := Height * 1.2;
      begin
         Set_Rgb
         (  Color,
            GUInt16'Last / 2,
            GUInt16'Last / 2,
            GUInt16'Last / 2
         );
         Add_Rectangular_Background
         (  Under          => Widget,
            Height         => Height,
            Width          => Size,
            Center         => Center,
            Rotation_Angle => 0.0,
            Corner_Radius  => Size / 40.0,
            Color          => Parse ("Black")
         );
         Add_Elliptic_Scale
         (  Under => Widget,
            Outer =>
               (  Center          => (Center.X, Center.Y + Pin),
                  Major_Curvature => 0.0,
                  Minor_Radius    => Height * 1.2,
                  Angle           => 0.0
               ),
            Inner =>
               (  Center          => (Center.X, Center.Y + Pin),
                  Major_Curvature => 0.0,
                  Minor_Radius    => Height * 1.05,
                  Angle           => 0.0
               ),
            Color  => Parse ("white"),
            Width  => Size / 200.0,
            Step   => Length / 10.0,
            From   => (Pi * 3.0 - Length) / 2.0,
            Length => Length
         );
         Add_Elliptic_Scale
         (  Under => Widget,
            Outer =>
               (  Center          => (Center.X, Center.Y + Pin),
                  Major_Curvature => 0.0,
                  Minor_Radius    => Height * 1.1,
                  Angle           => 0.0
               ),
            Inner =>
               (  Center          => (Center.X, Center.Y + Pin),
                  Major_Curvature => 0.0,
                  Minor_Radius    => Height * 1.05,
                  Angle           => 0.0
               ),
            Color   => Parse ("white"),
            Width   => Size / 300.0,
            Step    => Length / 20.0,
            Skipped => 2,
            From    => (Pi * 3.0 - Length) / 2.0,
            Length  => Length
         );
         Add_Elliptic_Annotation
         (  Under   => Widget,
            Ellipse =>
               (  Center          => (Center.X, Center.Y + Pin * 0.85),
                  Major_Curvature => 0.0,
                  Minor_Radius    => Height * 1.2,
                  Angle           => 0.0
               ),
            Texts   => "0 10 20 30 40 50 60 70 80 90 100",
            Face    =>
               Create_Toy
               (  Family => "arial",
                  Slant  => CAIRO_FONT_SLANT_NORMAL,
                  Weight => CAIRO_FONT_WEIGHT_BOLD
               ),
            Step    => Length / 10.0,
            Height  => Size * 0.05,
            Stretch => 0.5,
            Color   => Color,
            From    => (Pi * 3.0 - Length) / 2.0,
            Length  => Length,
            Mode    => --Moved_Centered
                       Skewed
         );
      end;
   --
   -- Flat scales and annotations
   --
      Add_Flat_Scale
      (  Under   => Widget,
         Step    => 20.0,
         Color   => Parse ("white"),
         Breadth => 30.0,
         Width   => 1.0,
         From    => (300.0, 300.0),
         Length  => 200.0,
         Angle   => -Pi / 6.0
      );
      Add_Line
      (  Under   => Widget,
         Color   => Parse ("white"),
         Width   => 1.0,
         From    => (300.0, 300.0),
         Length  => 200.0,
         Angle   => -Pi / 6.0
      );
      Add_Flat_Annotation
      (  Under       => Widget,
         Texts       => "1 2 3 4 5 Left 8 9",
         Step        => 20.0,
         Color       => Parse ("blue"),
         From        => (300.0, 300.0),
         Length      => 200.0,
         Stretch     => 2.0,
         Justify     => Ada.Strings.Left,
         Scale_Angle => -Pi / 6.0
      );
      Add_Flat_Scale
      (  Under   => Widget,
         Step    => 20.0,
         Color   => Parse ("white"),
         Breadth => 30.0,
         Width   => 1.0,
         From    => (300.0, 400.0),
         Length  => 200.0,
         Angle   => -Pi / 6.0
      );
      Add_Line
      (  Under   => Widget,
         Color   => Parse ("white"),
         Width   => 1.0,
         From    => (300.0, 400.0),
         Length  => 200.0,
         Angle   => -Pi / 6.0
      );
      Add_Flat_Annotation
      (  Under       => Widget,
         Texts       => "1 2 3 4 5 Right 8 9",
         Step        => 20.0,
         Color       => Parse ("blue"),
         From        => (300.0, 400.0),
         Length      => 200.0,
         Stretch     => 2.0,
         Justify     => Ada.Strings.Right,
         Scale_Angle => -Pi / 6.0
      );
      Add_Flat_Scale
      (  Under   => Widget,
         Step    => 20.0,
         Color   => Parse ("white"),
         Breadth => 30.0,
         Width   => 1.0,
         From    => (300.0, 500.0),
         Length  => 200.0,
         Angle   => -Pi / 6.0
      );
      Add_Line
      (  Under   => Widget,
         Color   => Parse ("white"),
         Width   => 1.0,
         From    => (300.0, 500.0),
         Length  => 200.0,
         Angle   => -Pi / 6.0
      );
      Add_Flat_Annotation
      (  Under       => Widget,
         Texts       => "1 2 3 4 5 Center 8 9",
         Step        => 20.0,
         Color       => Parse ("blue"),
         From        => (300.0, 500.0),
         Length      => 200.0,
         Stretch     => 2.0,
         Justify     => Ada.Strings.Center,
         Scale_Angle => -Pi / 6.0
      );
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Widget.all'Unchecked_Access;
   end Test_1;

   function Test_2 return Gtk_Widget is
      use Gtk.Gauge.Round_270;
      Widget     : Gtk_Gauge_Round_270;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 20 40 60 80 100 120",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Add_Label   -- Adding a text to the widget
      (  Under    => Widget.Get_Cache,
         Text     => "mph",
         Location => (0.0, 0.17),
         Height   => 0.1,
         Color    => Parse ("white"),
         Scaled   => True
      );
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_2;

   function Test_3 return Gtk_Widget is
      use Gtk.Gauge.Round_254;
      Widget     : Gtk_Gauge_Round_254;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         Minor_Texts => "0 20 40 60 80 100 120 140 160",
         Major_Texts => "10 30 50 70 90 110 130 150 170",
         Adjustment  => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_3;

   function Test_4 return Gtk_Widget is
      use Gtk.Gauge.Round_270_Outer;
      Widget     : Gtk_Gauge_Round_270_Outer;
      Box        : Gtk_HBox;
      Adjustment : array (1..2) of Gtk_Adjustment;
      Slider     : array (1..2) of Gtk_Scale;
   begin
      Gtk_New (Adjustment (1), 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New (Adjustment (2), 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 20 40 60 80 100 120",
         Adjustment => Adjustment (1)
      );
      Add_Sector_Needle
      (  Under      => Widget.Get_Needle,
         Outer      => ((0.0, 0.0), 1.0 / 0.29, 0.29, 0.0),
         Inner      => ((0.0, 0.0), 1.0 / 0.10, 0.10, 0.0),
         From       => 3.0 * Pi / 4.0,
         Length     => 3.0 * Pi / 2.0,
         Color      => RGB (0.9, 1.0, 0.5),
         Adjustment => Adjustment (2),
         Scaled     => True
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider (1), Adjustment (1));
      Box.Pack_Start (Slider (1), False, False);
      Gtk_New_Vscale (Slider (2), Adjustment (2));
      Box.Pack_Start (Slider (2), False, False);
      Add_Label   -- Adding a text to the widget
      (  Under    => Widget.Get_Cache,
         Text     => "mph",
         Location => (0.0, 0.17),
         Height   => 0.1,
         Color    => Parse ("black"),
         Scaled   => True
      );
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_4;

   function Test_5 return Gtk_Widget is
      use Gtk.Wall_Clock.Classic;
      Widget     : Gtk_Wall_Clock_Classic;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 24.0 * 60.0 * 60.0, 1.0, 3600.0);
      Gtk_New_HBox (Box);
      Gtk_New (Widget, Adjustment);
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_5;

   function Test_6 return Gtk_Widget is
      use Gtk.Wall_Clock.Modern;
      Widget     : Gtk_Wall_Clock_Modern;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 24.0 * 60.0 * 60.0, 1.0, 3600.0);
      Gtk_New_HBox (Box);
      Gtk_New (Widget, Adjustment);
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_6;

   function Test_7 return Gtk_Widget is
      use Gtk.Wall_Clock.Imperial;
      Widget     : Gtk_Wall_Clock_Imperial;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 24.0 * 60.0 * 60.0, 60.0, 3600.0);
      Gtk_New_HBox (Box);
      Gtk_New (Widget, Adjustment);
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_7;

   function Test_8 return Gtk_Widget is
      use Gtk.Gauge.Round_180;
      Widget     : Gtk_Gauge_Round_180;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 10 20 30 40 50 60 70 80 90 100 110 120",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_8;

   function Test_9 return Gtk_Widget is
      use Gtk.Gauge.Rectangular_70s;
      Widget     : Gtk_Gauge_Rectangular_70s;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 10 20 30 40 50 60 70 80 90 100 110 120",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_9;

   function Test_10 return Gtk_Widget is
      use Gtk.Gauge.Round_270_60s;
      Widget     : Gtk_Gauge_Round_270_60s;
      Box        : Gtk_HBox;
      Adjustment : array (1..2) of Gtk_Adjustment;
      Slider     : array (1..2) of Gtk_Scale;
   begin
      Gtk_New (Adjustment (1), 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New (Adjustment (2), 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 10 20 30 40 50 60 70 80 90 100 110 120",
         Adjustment => Adjustment (1)
      );
      Add_Elliptic_Bar
      (  Under      => Widget.Get_Background.Get_Foreground,
         Ellipse    => ((0.0, 0.0), 1.0 / 0.47, 0.47, 0.0),
         From       => 1.0 * Pi / 4.0,
         Length     => -3.0 * Pi / 2.0,
         Width      => 0.015,
         Color      => RGB (1.0, 0.6, 0.3),
         Adjustment => Adjustment (2),
         Scaled     => True,
         Widened    => True
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider (1), Adjustment (1));
      Box.Pack_Start (Slider (1), False, False);
      Gtk_New_Vscale (Slider (2), Adjustment (2));
      Box.Pack_Start (Slider (2), False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_10;

   function Test_11 return Gtk_Widget is
      use Gtk.Gauge.Round_270_Reversed;
      Widget     : Gtk_Gauge_Round_270_Reversed;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 20 40 60 80 100 120 160 140 180",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_11;

   function Test_12 return Gtk_Widget is
      use Gtk.Gauge.Round_110;
      Widget     : Gtk_Gauge_Round_110;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 10 20 30 40 50 60 70 80 90",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_12;

   function Test_13 return Gtk_Widget is
      use Gtk.Gauge.Elliptic_180;
      Widget     : Gtk_Gauge_Elliptic_180;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 20 40 60 80 100 120 140 160",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_13;

   function Test_14 return Gtk_Widget is
      use Gtk.Gauge.Round_270_Inout;
      Widget     : Gtk_Gauge_Round_270_Inout;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         Major_Texts => "0 20 40 60 80 100 120 140",
         Minor_Texts => "10 30 50 70 90 110 130",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_14;

   function Test_15 return Gtk_Widget is
      use Gtk.Gauge.Round_90;
      Widget     : Gtk_Gauge_Round_90;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         Texts => "0 20 40 60 80 100 120 140",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_15;

   function Test_16 return Gtk_Widget is
      Widget : Real_Modern_Clock;
      Box    : Gtk_HBox;
   begin
      Set_Period (Engine, 0.2);
      Gtk_New_HBox (Box);
      Gtk_New (Widget);
      Box.Pack_Start (Widget);
      Add (Engine, Widget);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_16;

   function Test_17 return Gtk_Widget is
      use Gtk.Gauge.Rectangular_70s_Slanted;
      Widget     : Gtk_Gauge_Rectangular_70s_Slanted;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 10 20 30 40 50 60 70 80 90 100 110 120",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_17;

   function Test_18 return Gtk_Widget is
      use Gtk.Gauge.Flat_Horizontal;
      Widget     : Gtk_Gauge_Flat_Horizontal;
      Box        : Gtk_HBox;
      Adjustment : array (1..2) of Gtk_Adjustment;
      Slider     : array (1..2) of Gtk_Scale;
   begin
      Gtk_New (Adjustment (1), 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New (Adjustment (2), 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 10 20 30 40 50 60 70 80 90 100 110 120",
         Adjustment => Adjustment (1)
      );
      Add_Bar
      (  Under      => Widget.Get_Needle,
         From       => (-0.45, 0.1),
         To         => ( 0.45, 0.1),
         Width      => 0.03,
         Color      => RGB (0.0, 0.6, 0.7),
         Adjustment => Adjustment (2),
         Scaled     => True,
         Widened    => True
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider (1), Adjustment (1));
      Box.Pack_Start (Slider (1), False, False);
      Gtk_New_Vscale (Slider (2), Adjustment (2));
      Box.Pack_Start (Slider (2), False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_18;

   function Test_19 return Gtk_Widget is
      use Gtk.Gauge.Flat_Vertical;
      Widget     : Gtk_Gauge_Flat_Vertical;
      Box        : Gtk_HBox;
      Adjustment : array (1..2) of Gtk_Adjustment;
      Slider     : array (1..2) of Gtk_Scale;
   begin
      Gtk_New (Adjustment (1), 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New (Adjustment (2), 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 10 20 30 40 50 60 70 80 90 100 110 120",
         Adjustment => Adjustment (1)
      );
      Add_Bar
      (  Under => Widget.Get_Needle,
         From  => (  Widget.Get_Needle.Get_From.X + 0.25,
                     Widget.Get_Needle.Get_From.Y
                  ),
         To    => (  Widget.Get_Needle.Get_To.X + 0.25,
                     Widget.Get_Needle.Get_To.Y
                  ),
         Width      => 0.08,
         Color      => RGB (0.1, 0.3, 0.5),
         Line_Cap   => CAIRO_LINE_CAP_ROUND,
         Adjustment => Adjustment (2),
         Scaled     => True,
         Widened    => True
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider (1), Adjustment (1));
      Box.Pack_Start (Slider (1), False, False);
      Gtk_New_Vscale (Slider (2), Adjustment (2));
      Box.Pack_Start (Slider (2), False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_19;

   function Test_20 return Gtk_Widget is
      Widget : Gtk_Layered;
   begin
      Gtk_New (Widget);

      Add_Elliptic_Background
      (  Under => Widget,
         Outer => ((100.0, 100.0), 1.0 / 90.0, 60.0, 0.0),
         Color => RGB (0.0, 0.0, 0.0)
      );
      Add_Elliptic_Background
      (  Under  => Widget,
         Outer  => ((300.0, 250.0), 1.0 / 90.0, 60.0, 0.0),
         Color  => RGB (0.0, 0.0, 0.0),
         From   => 3.0 * Pi / 4.0,
         Length => 3.0 * Pi / 2.0
      );
      Add_Elliptic_Background
      (  Under  => Widget,
         Outer  => ((100.0, 350.0), 1.0 / 90.0, 90.0, 0.0),
         Inner  => ((100.0, 350.0), 1.0 / 60.0, 60.0, 0.0),
         Color  => RGB (0.0, 0.0, 0.0),
         From   => 3.0 * Pi / 4.0,
         Length => 3.0 * Pi / 2.0
      );
      Add_Elliptic_Background
      (  Under  => Widget,
         Outer  => ((300.0, 100.0), 1.0 / 90.0, 90.0, 0.0),
         Center => (300.0, 150.0),
         Color  => RGB (0.0, 0.0, 0.0),
         From   => 3.0 * Pi / 4.0,
         Length => Pi
      );
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Widget.all'Unchecked_Access;
   end Test_20;

   function Test_21 return Gtk_Widget is
      Widget  : Gtk_Layered;
      Angle   : GDouble;
      Center  : constant Cairo_Tuple := (400.0, 300.0);
      Ellipse : constant Ellipse_Parameters :=
                   (Center, 1.0 / 190.0, 160.0, Pi / 7.0);
   begin
      Gtk_New (Widget);

      Add_Arc
      (  Under   => Widget,
         Ellipse => Ellipse,
         Color   => RGB (0.0, 0.0, 0.0)
      );
      Add_Line
      (  Under   => Widget,
         From    => Center,
         Length  => 200.0,
         Angle   => Pi / 7.0,
         Color   => RGB (0.0, 0.0, 0.0)
      );
      Add_Line
      (  Under   => Widget,
         From    => Center,
         Length  => 200.0,
         Angle   => Pi + Pi / 7.0,
         Color   => RGB (0.0, 0.0, 0.0)
      );
         -- Moved centered
      Angle := Pi + Pi / 3.0;
      Add_Label
      (  Under    => Widget,
         Text     => "centered",
         Face     => Create_Pango ("arial"),
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Mode     => Moved_Centered
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );
         -- Moved inside
      Angle := Pi / 3.0;
      Add_Label
      (  Under    => Widget,
         Text     => "inside",
         Face     => Create_Pango ("arial"),
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Mode     => Moved_Inside
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );
         -- Moved outside
      Angle := Pi / 2.0 + Pi / 3.0;
      Add_Label
      (  Under    => Widget,
         Text     => "outside",
         Face     => Create_Pango ("arial"),
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Mode     => Moved_Outside
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );
         -- Moved skewed
      Angle := -Pi / 4.0;
      Add_Label
      (  Under    => Widget,
         Text     => "skewed",
         Face     => Create_Pango ("arial"),
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Ellipse.Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Skew     => Ellipse.Angle,
         Mode     => Skewed
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );
         -- Rotated
      Angle := Pi * 1.98;
      Add_Label
      (  Under    => Widget,
         Text     => "rotated",
         Face     => Create_Pango ("arial"),
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Mode     => Rotated
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );

      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Widget.all'Unchecked_Access;
   end Test_21;

   function Test_22 return Gtk_Widget is
      Widget : Gtk_Layered;
   begin
      Gtk_New (Widget);

      Add_Cap
      (  Under   => Widget,
         Center  => (0.0, 0.0),
         Radius  => 0.5,
         Scaled  => True,
         Widened => True
      );
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Widget.all'Unchecked_Access;
   end Test_22;

   function Test_23 return Gtk_Widget is
      use Gtk.Meter.Elliptic_90;
      Widget     : Gtk_Meter_Elliptic_90;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 50 100 150 200 250",
         Adjustment => Adjustment
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_23;

   function Test_24 return Gtk_Widget is
      use Gtk.Meter.Round_90;
      Widget     : Gtk_Meter_Round_90;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 10 20 30 40",
         Adjustment => Adjustment
      );
      Add_Label
      (  Under    => Widget.Get_Cache,
         Face     => Create_Pango ("arial unicode ms"),
         Height   => 0.09,
         Location => (0.0, 0.12),
         Text     => "V",
         Scaled   => True
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_24;

   function Test_25 return Gtk_Widget is
      use Gtk.Meter.Angular_90;
      Widget     : Gtk_Meter_Angular_90;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 20 40 60 80 100",
         Adjustment => Adjustment
      );
      Add_Label
      (  Under    => Widget.Get_Cache,
         Face     => Create_Toy
                     (  Family => "arial",
                        Slant  => CAIRO_FONT_SLANT_NORMAL,
                        Weight => CAIRO_FONT_WEIGHT_BOLD
                     ),
         Height   => 0.09,
         Location => (-0.35, -0.35),
         Text     => "V",
         Scaled   => True
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_25;

   function Test_26 return Gtk_Widget is
      use Gtk.Meter.Round_94;
      Widget     : Gtk_Meter_Round_94;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "0 100 200 300 400 500",
         Adjustment => Adjustment
      );
      Add_Label
      (  Under    => Widget.Get_Cache,
         Face     => Create_Toy
                     (  Family => "arial",
                        Slant  => CAIRO_FONT_SLANT_NORMAL,
                        Weight => CAIRO_FONT_WEIGHT_BOLD
                     ),
         Height   => 0.12,
         Location => (0.0, 0.12),
         Text     => "V",
         Scaled   => True
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_26;

   function Test_35 return Gtk_Widget is
      Widget  : Gtk_Layered;
      Angle   : GDouble;
      Center  : constant Cairo_Tuple := (400.0, 300.0);
      Ellipse : constant Ellipse_Parameters :=
                   (Center, 1.0 / 190.0, 160.0, Pi / 7.0);
   begin
      Gtk_New (Widget);

      Add_Arc
      (  Under   => Widget,
         Ellipse => Ellipse,
         Color   => RGB (0.0, 0.0, 0.0)
      );
      Add_Line
      (  Under   => Widget,
         From    => Center,
         Length  => 200.0,
         Angle   => Pi / 7.0,
         Color   => RGB (0.0, 0.0, 0.0)
      );
      Add_Line
      (  Under   => Widget,
         From    => Center,
         Length  => 200.0,
         Angle   => Pi + Pi / 7.0,
         Color   => RGB (0.0, 0.0, 0.0)
      );
         -- Moved centered
      Angle := Pi + Pi / 3.0;
      Add_Label
      (  Under    => Widget,
         Text     => "centered",
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Mode     => Moved_Centered
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );
         -- Moved inside
      Angle := Pi / 3.0;
      Add_Label
      (  Under    => Widget,
         Text     => "inside",
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Mode     => Moved_Inside
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );
         -- Moved outside
      Angle := Pi / 2.0 + Pi / 3.0;
      Add_Label
      (  Under    => Widget,
         Text     => "outside",
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Mode     => Moved_Outside
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );
         -- Moved skewed
      Angle := -Pi / 4.0;
      Add_Label
      (  Under    => Widget,
         Text     => "skewed",
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Ellipse.Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Skew     => Ellipse.Angle,
         Mode     => Skewed
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );
         -- Rotated
      Angle := Pi * 1.98;
      Add_Label
      (  Under    => Widget,
         Text     => "rotated",
         Stretch  => 0.5,
         Height   => 50.0,
         Angle    => Angle,
         Location => Get_Point (Ellipse, Ellipse * Angle),
         Mode     => Rotated
      );
      Add_Line
      (  Under => Widget,
         From  => Center,
         To    => Get_Point (Ellipse, Ellipse * Angle),
         Color => RGB (0.5, 0.5, 1.0)
      );

      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Widget.all'Unchecked_Access;
   end Test_35;

   procedure Set_Autoscale
             (  Toggle : access Gtk_Toggle_Button_Record'Class
             )  is
      use Gtk.Layered.Waveform.Amplifier;
   begin
      Set_Auto_Scaling
      (  Gtk_Waveform_Amplifier_Record'Class
         (  Wave.Get_Amplifier.all
         ) 'Unchecked_Access,
         Get_Active (Toggle)
      );
   end Set_Autoscale;

   procedure Freeze (Toggle : access Gtk_Toggle_Button_Record'Class) is
      use Gtk.Layered.Waveform.Sweeper;
      Sweeper : constant Gtk_Waveform_Sweeper :=
                         Gtk_Waveform_Sweeper_Record'Class
                         (  Wave.Get_Sweeper.all
                         ) 'Unchecked_Access;
   begin
      Set_Frozen (Sweeper, Toggle.Get_Active);
      Set_Text (Offset_Edit, Image (Float (Sweeper.Get_Offset)));
   end Freeze;

   procedure Set_Linear
             (  Toggle : access Gtk_Toggle_Button_Record'Class
             )  is
      use Gtk.Layered.Waveform;
   begin
      if Get_Active (Toggle) then
         Wave.Set_Interpolation_Mode (Linear);
      else
         Wave.Set_Interpolation_Mode (Left);
      end if;
   end Set_Linear;

   procedure Set_Offset (Edit : Gtk_Editable) is
      use Gtk.Layered.Waveform.Sweeper;
      Sweeper : constant Gtk_Waveform_Sweeper :=
                         Gtk_Waveform_Sweeper_Record'Class
                         (  Wave.Get_Sweeper.all
                         ) 'Unchecked_Access;
      Offset  : Float;
   begin
      Offset := Value (Get_Text (-Edit));
      if Offset in 0.0..100_000.0 then
         Sweeper.Set_Time
         (  Ada.Calendar.Time'
            (  Gtk.Layered.Waveform.To_Time
               (  GDouble
                  (  Sweeper.Get_Upper
                  -  GDouble (Offset)
         )  )  )  );
      end if;
   exception
      when others =>
         null;
   end Set_Offset;

   procedure Set_Opacity (Edit : Gtk_Editable) is
      use Gtk.Layered.Waveform;
      use Gtk.Layered.Waveform.Sweeper;
      Opacity : Float;
   begin
      Opacity := Value (Get_Text (-Edit));
      if Opacity in 0.0..1.0 then
         Wave.Set_Opacity (GDouble (Opacity));
      end if;
   exception
      when others =>
         null;
   end Set_Opacity;

   procedure Set_Refresh (Edit : Gtk_Editable) is
      use Gtk.Layered.Waveform;
      Rate : Float;
   begin
      Rate := Value (Get_Text (-Edit));
      if Rate in 0.001..100_000.0 then
         Set_Period (Engine, Duration (Rate));
      end if;
   exception
      when others =>
         null;
   end Set_Refresh;

   procedure Set_Width (Edit : Gtk_Editable) is
      use Gtk.Layered.Waveform;
      use Gtk.Layered.Waveform.Sweeper;
      Rate : Float;
   begin
      Rate := Value (Get_Text (-Edit));
      if Rate in 0.001..100_000.0 then
         Set_Page_Span
         (  Gtk_Waveform_Sweeper_Record'Class
            (  Wave.Get_Sweeper.all
            ) 'Unchecked_Access,
            Duration (Rate)
         );
      end if;
   exception
      when others =>
         null;
   end Set_Width;

   function Test_27 return Gtk_Widget is
      use Gtk.Layered.Graph_Paper_Annotation;
      use Gtk.Layered.Waveform;
      use Gtk.Layered.Waveform.Amplifier;
      use Gtk.Layered.Waveform.Sweeper;
      Rate        : constant String := "0.01";
      Box         : Gtk_VBox;
      Label       : Gtk_Label;
      Edit        : Gtk_Entry;
      Toggle      : Gtk_Check_Button;
      Layered     : Gtk_Layered;
      Sweeper     : Gtk_Waveform_Sweeper;
      Amplifier   : Gtk_Waveform_Amplifier;
      Generator   : Wave_Generator;
      Table       : Gtk_Table;
      Frame       : Gtk_Frame;
      Graph_Paper : access Graph_Paper_Layer;
   begin
      Gtk_New_Vbox (Box);
      Gtk_New (Layered);
      Box.Pack_Start (Layered);
      Gtk_New (Sweeper);
      Gtk_New (Amplifier);
      Graph_Paper :=
         Add_Graph_Paper
         (  Under         => Layered,
            Box           => (-0.5, -0.5, 0.5, 0.5),
            X_Axis        => Sweeper,
            Y_Axis        => Amplifier,
            X_Tick_Length => 50,
            Y_Tick_Length => 50,
            Scaled        => True
         );
      Wave :=
         Add_Waveform
         (  Under     => Layered,
            Box       => (-0.5, -0.5, 0.5, 0.5),
            Scaled    => True,
            Sweeper   => Sweeper,
            Amplifier => Amplifier
         ) .all'Unchecked_Access;
      Add_Graph_Paper_Annotation
      (  Under      => Layered,
         Paper      => Graph_Paper,
         Location   => (Vertical, Absolute, -0.5, 0.5, -0.5),
         Scaled     => True,
         Height     => 12.0,
         Opacity    => 0.5,
         Text_Angle => -Pi / 6.0,
         Justify_X  => Ada.Strings.Left,
         Justify_Y  => Center
      );
      Add_Graph_Paper_Annotation
      (  Under      => Layered,
         Paper      => Graph_Paper,
         Location   => (Vertical, Absolute, -0.5, 0.5, 0.5),
         Scaled     => True,
         Height     => 12.0,
         Opacity    => 0.0,
         Text_Angle => -Pi / 4.0,
         Justify_X  => Ada.Strings.Right,
         Justify_Y  => Center
      );
      Add_Graph_Paper_Time_Annotation
      (  Under    => Layered,
         Paper    => Graph_Paper,
         Location => (Horizontal, Relative, -0.5, 0.5, 0.0),
         Scaled   => True,
         Height   => 12.0,
         Opacity  => 0.0
      );
      Gtk_New (Table, 5, 4, False);
      Set_Col_Spacings (Table, 3);
      Set_Row_Spacings (Table, 3);
         -- Row 1
      Gtk_New (Label, "Refresh rate");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Set_Alignment (Label, 1.0, 0.5);
      Table.Attach (Label, 0, 1, 0, 1, XOptions => Gtk.Enums.Fill);

      Gtk_New (Edit);
      Table.Attach (Edit, 1, 2, 0, 1);
      Set_Text (Edit, Rate);

      Gtk_New (Label, "s");
      Table.Attach (Label, 2, 3, 0, 1, XOptions => Shrink);
      On_Changed (+Edit, +Set_Refresh'Access);

      Gtk_New (Toggle, "Hold");
      Table.Attach (Toggle, 3, 4, 0, 1);
      Toggle.On_Toggled (+Freeze'Access);
         -- Row 2
      Gtk_New (Label, "Width");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Set_Alignment (Label, 1.0, 0.5);
      Table.Attach (Label, 0, 1, 1, 2, XOptions => Gtk.Enums.Fill);

      Gtk_New (Edit);
      Set_Text (Edit, Image (Float (Sweeper.Get_Page_Span)));
      Table.Attach (Edit, 1, 2, 1, 2);

      Gtk_New (Label, "s");
      Table.Attach (Label, 2, 3, 1, 2, XOptions => Shrink);
      On_Changed (+Edit, +Set_Width'Access);

      Gtk_New (Toggle, "Interpolate");
      Set_Active (Toggle, True);
      Table.Attach (Toggle, 3, 4, 1, 2);
      Toggle.On_Toggled (+Set_Linear'Access);
         -- Row 3
      Gtk_New (Label, "Offset");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Set_Alignment (Label, 1.0, 0.5);
      Table.Attach (Label, 0, 1, 2, 3, XOptions => Gtk.Enums.Fill);

      Gtk_New (Offset_Edit);
      Set_Text (Offset_Edit, Image (Float (Sweeper.Get_Offset)));
      Table.Attach (Offset_Edit, 1, 2, 2, 3);

      Gtk_New (Label, "s");
      Table.Attach (Label, 2, 3, 2, 3, XOptions => Shrink);
      On_Changed (+Offset_Edit, +Set_Offset'Access);

      Gtk_New (Toggle, "Autoscale");
      Set_Active (Toggle, True);
      Table.Attach (Toggle, 3, 4, 2, 3);
      Toggle.On_Toggled (+Set_Autoscale'Access);
         -- Row 4
      Gtk_New (Label, "Opacity");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Set_Alignment (Label, 1.0, 0.5);
      Table.Attach (Label, 0, 1, 3, 4, XOptions => Gtk.Enums.Fill);

      Gtk_New (Edit);
      Set_Text (Edit, Image (Float (Wave.Get_Opacity)));
      Table.Attach (Edit, 1, 2, 3, 4);

      Gtk_New (Label, "s");
      Table.Attach (Label, 2, 3, 3, 4, XOptions => Shrink);
      On_Changed (+Edit, +Set_Opacity'Access);
         -- Row 5
      Gtk_New (Generator, Saw, 2.0);
      Gtk_New (Frame, "Generator");
      Frame.Add (Generator);
      Table.Attach (Frame, 0, 4, 4, 5, YOptions => Shrink);

      Box.Pack_Start (Table, False, False);
      Set_Period (Engine, Duration (Strings_Edit.Floats.Value (Rate)));
      Engine.Add (Layered);
      Wave.Set_Source (Generator.Get_Source.all);
      Put (Editor, Layered);
      return Box.all'Unchecked_Access;
   end Test_27;

   function Test_28 return Gtk_Widget is
      Table   : Gtk_Table;
      X_Axis  : Gtk_Adjustment;
      Y_Axis  : Gtk_Adjustment;
      Slider  : Gtk_Scale;
      Layered : Gtk_Layered;
   begin
      Gtk_New (Table, 2, 2, False);
      Gtk_New (X_Axis, 5.0, 0.0, 10.0, 0.05, 0.3, 1.0);
      Gtk_New_HScale (Slider, X_Axis);
      Table.Attach (Slider, 0, 1, 1, 2, Yoptions => Shrink);
      Gtk_New (Y_Axis, 5.0, 0.0, 10.0, 0.05, 0.3, 1.0);
      Gtk_New_VScale (Slider, Y_Axis);
      Table.Attach (Slider, 1, 2, 0, 1, Xoptions => Shrink);
      Gtk_New (Layered);
      Table.Attach (Layered, 0, 1, 0, 1);
      Add_Graph_Paper
      (  Under  => Layered,
         Box    => (-0.5, -0.5, 0.5, 0.5),
         X_Axis => X_Axis,
         Y_Axis => Y_Axis,
         Scaled => True
      );
      Put (Editor, Layered);
      return Table.all'Unchecked_Access;
   end Test_28;

   function Test_29 return Gtk_Widget is
      use Gtk.Meter.Thermo_Symmetric;
      Widget     : Gtk_Meter_Thermo_Symmetric;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, 0.0, 100.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "20 40 60 80 100 120 140 160 180 200 220",
         Adjustment  => Adjustment,
         Left_Label  => Character'Val (16#C2#) &
                        Character'Val (16#B0#) & 'F',
         Right_Label => Character'Val (16#C2#) &
                        Character'Val (16#B0#) & 'F',
         Sectors     => 10,
         Color       => RGB (0.0, 0.0, 1.0)
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_29;

   function Test_30 return Gtk_Widget is
      use Gtk.Meter.Thermo;
      Widget     : Gtk_Meter_Thermo;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, -20.0, 40.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New
      (  Widget,
         "-20 -10 0 10 20 30 40",
         Sectors    => 6,
         Adjustment => Adjustment
      );
      Add_Digital
      (  Under      => Widget.Get_Background.Get_Foreground,
         Location   => (-0.4, 0.0),
         Mode       => Rotated,
         Angle      => Pi,
         Adjustment => Adjustment,
         Precision  => -1,
         Height     => 0.12,
         Scaled     => True
      );
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_30;

   function Test_31 return Gtk_Widget is
      use Gtk.Meter.Thermo_Dual;
      Widget     : Gtk_Meter_Thermo_Dual;
      Box        : Gtk_HBox;
      Adjustment : Gtk_Adjustment;
      Slider     : Gtk_Scale;
   begin
      Gtk_New (Adjustment, 0.0, -10.0, 40.0, 1.0, 10.0);
--    Gtk_New (Adjustment, 0.0, -20.0, 30.0, 1.0, 10.0);
      Gtk_New_HBox (Box);
      Gtk_New_Celsius (Widget, Adjustment, 6);
      Box.Pack_Start (Widget);
      Gtk_New_Vscale (Slider, Adjustment);
      Box.Pack_Start (Slider, False, False);
      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_31;

   procedure Led_Toggled
             (  Widget : access Gtk_Button_Record'Class;
                Led    : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round
             )  is
   begin
      Led.Set_State (not Led.Get_State);
      Led.Queue_Draw;
   end Led_Toggled;

   function Test_32 return Gtk_Widget is
      use Gtk.Gauge.LED_Round;
      Widget     : Gtk_Gauge_LED_Round;
      Box        : Gtk_Table;
      Button     : Gtk_Button;
   begin
      Gtk_New (Box, 2, 4, False);
      Gtk_New (Widget, RGB (0.0, 1.0, 0.0), RGB (0.5, 0.5, 0.5));
      Box.Attach (Widget, 0, 1, 0, 1);
      Gtk_New (Button, "Toggle");
      Connect (Button, Widget, +Led_Toggled'Access);
      Box.Attach (Button, 0, 1, 1, 2);

      Gtk_New
      (  Widget,
         RGB (0.0, 0.0, 1.0),
         RGB (0.5, 0.5, 0.5),
         Shadow_Etched_Out
      );
      Box.Attach (Widget, 1, 2, 0, 1);
      Gtk_New (Button, "Toggle");
      Connect (Button, Widget, +Led_Toggled'Access);
      Box.Attach (Button, 1, 2, 1, 2);

      Gtk_New
      (  Widget,
         RGB (1.0, 1.0, 0.0),
         RGB (0.5, 0.5, 0.5),
         Shadow_Etched_In
      );
      Box.Attach (Widget, 2, 3, 0, 1);
      Gtk_New (Button, "Toggle");
      Connect (Button, Widget, +Led_Toggled'Access);
      Box.Attach (Button, 2, 3, 1, 2);

      Gtk_New
      (  Widget,
         RGB (1.0, 0.0, 0.0),
         RGB (0.5, 0.5, 0.5),
         Shadow_Out
      );
      Box.Attach (Widget, 3, 4, 0, 1);
      Gtk_New (Button, "Toggle");
      Connect (Button, Widget, +Led_Toggled'Access);
      Box.Attach (Button, 3, 4, 1, 2);

      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_32;

   procedure Led_Toggled
             (  Widget : access Gtk_Button_Record'Class;
                Led    : Gtk.Gauge.LED_Rectangular.
                         Gtk_Gauge_LED_Rectangular
             )  is
   begin
      Led.Set_State (not Led.Get_State);
      Led.Queue_Draw;
   end Led_Toggled;

   function Test_33 return Gtk_Widget is
      use Gtk.Gauge.LED_Rectangular;
      Widget     : Gtk_Gauge_LED_Rectangular;
      Box        : Gtk_Table;
      Button     : Gtk_Button;
   begin
      Gtk_New (Box, 2, 4, False);
      Gtk_New (Widget, RGB (0.0, 1.0, 0.0), RGB (0.5, 0.5, 0.5));
      Box.Attach (Widget, 0, 1, 0, 1);
      Gtk_New (Button, "Toggle");
      Connect (Button, Widget, +Led_Toggled'Access);
      Box.Attach (Button, 0, 1, 1, 2);

      Gtk_New
      (  Widget,
         RGB (0.0, 0.5, 1.0),
         RGB (0.5, 0.5, 0.5),
         Shadow_Etched_Out
      );
      Box.Attach (Widget, 1, 2, 0, 1);
      Gtk_New (Button, "Toggle");
      Connect (Button, Widget, +Led_Toggled'Access);
      Box.Attach (Button, 1, 2, 1, 2);

      Gtk_New
      (  Widget,
         RGB (0.0, 1.0, 1.0),
         RGB (0.5, 0.5, 0.5),
         Shadow_Etched_In
      );
      Box.Attach (Widget, 2, 3, 0, 1);
      Gtk_New (Button, "Toggle");
      Connect (Button, Widget, +Led_Toggled'Access);
      Box.Attach (Button, 2, 3, 1, 2);

      Gtk_New
      (  Widget,
         RGB (1.0, 0.0, 0.0),
         RGB (0.5, 0.5, 0.5),
         Shadow_Out
      );
      Box.Attach (Widget, 3, 4, 0, 1);
      Gtk_New (Button, "Toggle");
      Connect (Button, Widget, +Led_Toggled'Access);
      Box.Attach (Button, 3, 4, 1, 2);

      Put (Editor, Widget);
      Set_Sensitive (Load_Button, True);
      Set_Sensitive (Save_Button, True);
      return Box.all'Unchecked_Access;
   end Test_33;

   function Test_34 return Gtk_Widget is
      use Gtk.Layered.Graph_Paper_Annotation;
      use Gtk.Layered.Waveform;
      use Gtk.Layered.Waveform.Amplifier;
      use Gtk.Layered.Waveform.Sweeper;
      Rate        : constant String := "0.01";
      Box         : Gtk_VBox;
      Label       : Gtk_Label;
      Edit        : Gtk_Entry;
      Toggle      : Gtk_Check_Button;
      Layered     : Gtk_Layered;
      Sweeper     : Gtk_Waveform_Sweeper;
      Amplifier   : Gtk_Waveform_Amplifier;
      Generator   : Wave_Generator;
      Table       : Gtk_Table;
      Frame       : Gtk_Frame;
      Graph_Paper : access Graph_Paper_Layer;
   begin
      Gtk_New_Vbox (Box);
      Gtk_New (Layered);
      Box.Pack_Start (Layered);
      Gtk_New (Sweeper);
      Gtk_New (Amplifier);
      Graph_Paper :=
         Add_Graph_Paper
         (  Under         => Layered,
            Box           => (-0.5, -0.5, 0.5, 0.5),
            X_Axis        => Sweeper,
            Y_Axis        => Amplifier,
            X_Tick_Length => 50,
            Y_Tick_Length => 50,
            Scaled        => True
         );
      Wave :=
         Add_Waveform
         (  Under     => Layered,
            Box       => (-0.5, -0.5, 0.5, 0.5),
            Scaled    => True,
            Sweeper   => Sweeper,
            Amplifier => Amplifier
         ) .all'Unchecked_Access;
      Add_Graph_Paper_Time_Annotation
      (  Under    => Layered,
         Paper    => Graph_Paper,
         Location => (Horizontal, Relative, -0.5, 0.5, 0.0),
--         Face     => Create_Toy ("arial"),
         Scaled   => True,
         Height   => 12.0,
         Opacity  => 0.0
      );
      Gtk_New (Table, 5, 4, False);
      Set_Col_Spacings (Table, 3);
      Set_Row_Spacings (Table, 3);
         -- Row 1
      Gtk_New (Label, "Refresh rate");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Set_Alignment (Label, 1.0, 0.5);
      Table.Attach (Label, 0, 1, 0, 1, XOptions => Gtk.Enums.Fill);

      Gtk_New (Edit);
      Table.Attach (Edit, 1, 2, 0, 1);
      Set_Text (Edit, Rate);

      Gtk_New (Label, "s");
      Table.Attach (Label, 2, 3, 0, 1, XOptions => Shrink);
      On_Changed (+Edit, +Set_Refresh'Access);

      Gtk_New (Toggle, "Hold");
      Table.Attach (Toggle, 3, 4, 0, 1);
      Toggle.On_Toggled (+Freeze'Access);
         -- Row 2
      Gtk_New (Label, "Width");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Set_Alignment (Label, 1.0, 0.5);
      Table.Attach (Label, 0, 1, 1, 2, XOptions => Gtk.Enums.Fill);

      Gtk_New (Edit);
      Set_Text (Edit, Image (Float (Sweeper.Get_Page_Span)));
      Table.Attach (Edit, 1, 2, 1, 2);

      Gtk_New (Label, "s");
      Table.Attach (Label, 2, 3, 1, 2, XOptions => Shrink);
      On_Changed (+Edit, +Set_Width'Access);

      Gtk_New (Toggle, "Interpolate");
      Set_Active (Toggle, True);
      Table.Attach (Toggle, 3, 4, 1, 2);
      Toggle.On_Toggled (+Set_Linear'Access);
         -- Row 3
      Gtk_New (Label, "Offset");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Set_Alignment (Label, 1.0, 0.5);
      Table.Attach (Label, 0, 1, 2, 3, XOptions => Gtk.Enums.Fill);

      Gtk_New (Offset_Edit);
      Set_Text (Offset_Edit, Image (Float (Sweeper.Get_Offset)));
      Table.Attach (Offset_Edit, 1, 2, 2, 3);

      Gtk_New (Label, "s");
      Table.Attach (Label, 2, 3, 2, 3, XOptions => Shrink);
      On_Changed (+Offset_Edit, +Set_Offset'Access);

      Gtk_New (Toggle, "Autoscale");
      Set_Active (Toggle, True);
      Table.Attach (Toggle, 3, 4, 2, 3);
      Toggle.On_Toggled (+Set_Autoscale'Access);
         -- Row 4
      Gtk_New (Label, "Opacity");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Set_Alignment (Label, 1.0, 0.5);
      Table.Attach (Label, 0, 1, 3, 4, XOptions => Gtk.Enums.Fill);

      Gtk_New (Edit);
      Set_Text (Edit, Image (Float (Wave.Get_Opacity)));
      Table.Attach (Edit, 1, 2, 3, 4);

      Gtk_New (Label, "s");
      Table.Attach (Label, 2, 3, 3, 4, XOptions => Shrink);
      On_Changed (+Edit, +Set_Opacity'Access);
         -- Row 5
      Gtk_New (Generator, Saw, 2.0);
      Gtk_New (Frame, "Generator");
      Frame.Add (Generator);
      Table.Attach (Frame, 0, 4, 4, 5, YOptions => Shrink);

      Box.Pack_Start (Table, False, False);
      Set_Period (Engine, Duration (Strings_Edit.Floats.Value (Rate)));
      Engine.Add (Layered);
      Wave.Set_Source (Generator.Get_Source.all);
      Put (Editor, Layered);
      return Box.all'Unchecked_Access;
   end Test_34;

begin
   Gtk.Main.Init;
-- Gtk.Main.Router.GNAT_Stack.Set_Log_Trace;
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("Gtk");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GLib-GObject");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GtkAda+");
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Window.Set_Title ("Test Ada industrial control widget library");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Window.Set_Size_Request (900, 600);

   Gtk_New_HPaned (Main_Pane);
   Window.Add (Main_Pane);

   declare
      Scroll : Gtk_Scrolled_Window;
      Box    : Gtk_VBox;
      View   : Gtk_Tree_View;
      Frame  : Gtk_Frame;
      List   : Gtk_Tree_Store;
      Row    : Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter := Null_Iter;
      Ptr    : GValue;

      procedure Add_Test (Name : String; Test : System.Address) is
      begin
         Append (List, Row, Parent);
         Gtk.Missed.Set (List, Row, 0, Name);
         Set_Address (Ptr, Test);
         Set_Value (List, Row, 1, Ptr);
      end Add_Test;
   begin
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_Etched_In);
      Gtk_New_VPaned (Test_Pane);
      Frame.Add (Test_Pane);
      Main_Pane.Add1 (Frame);

      Init (Ptr, GType_Pointer);
      Gtk_New (View);
      Gtk_New (List, (GType_String, GType_Pointer));
      -- Creating the list of tests
         -- Raw I/O
         Append (List, Parent, Null_Iter);
         Gtk.Missed.Set (List, Parent, 0, "Raw I/O");
            Add_Test ("Gtk_Layered", Test_1'Address);
            Add_Test ("Elliptic backgrounds",         Test_20'Address);
            Add_Test ("Text transformations (toy)",   Test_35'Address);
            Add_Test ("Text transformations (pango)", Test_21'Address);
            Add_Test ("Caps",                         Test_22'Address);
         -- Gauges
         Append (List, Parent, Null_Iter);
         Gtk.Missed.Set (List, Parent, 0, "Gauges");
            Add_Test ("Gtk_Gauge_Round_90",           Test_15'Address);
            Add_Test ("Gtk_Gauge_Round_110",          Test_12'Address);
            Add_Test ("Gtk_Gauge_Round_180",          Test_8'Address);
            Add_Test ("Gtk_Gauge_Round_254",          Test_3'Address);
            Add_Test ("Gtk_Gauge_Round_270",          Test_2'Address);
            Add_Test ("Gtk_Gauge_Round_270_Inout",    Test_14'Address);
            Add_Test ("Gtk_Gauge_Round_270_Outer",    Test_4'Address);
            Add_Test ("Gtk_Gauge_Round_270_Reversed", Test_11'Address);
            Add_Test ("Gtk_Gauge_Round_270_60s",      Test_10'Address);
            Add_Test ("Gtk_Gauge_Elliptic_180",       Test_13'Address);
            Add_Test ("Gtk_Gauge_Rectangular_70s",    Test_9'Address);
            Add_Test ("Gtk_Gauge_Rectangular_70s_Slanted",
                                                      Test_17'Address);
            Add_Test ("Gtk_Gauge_Flat_Horizontal",    Test_18'Address);
            Add_Test ("Gtk_Gauge_Flat_Vertical",      Test_19'Address);
         -- Meters
         Append (List, Parent, Null_Iter);
         Gtk.Missed.Set (List, Parent, 0, "Meters");
            Add_Test ("Gtk_Meter_Angular_90",         Test_25'Address);
            Add_Test ("Gtk_Meter_Elliptic_90",        Test_23'Address);
            Add_Test ("Gtk_Meter_Round_90",           Test_24'Address);
            Add_Test ("Gtk_Meter_Round_94",           Test_26'Address);
            Add_Test ("Gtk_Meter_Thermo",             Test_30'Address);
            Add_Test ("Gtk_Meter_Thermo_Dual",        Test_31'Address);
            Add_Test ("Gtk_Meter_Thermo_Symmetric",   Test_29'Address);
         -- Leds
         Append (List, Parent, Null_Iter);
         Gtk.Missed.Set (List, Parent, 0, "LEDs");
            Add_Test ("Gtk_Gauge_LED_Rectangular", Test_33'Address);
            Add_Test ("Gtk_Gauge_LED_Round",       Test_32'Address);
         -- Clocks
         Append (List, Parent, Null_Iter);
         Gtk.Missed.Set (List, Parent, 0, "Wall clocks");
            Add_Test ("Gtk_Clock_Imperial", Test_7'Address);
            Add_Test ("Gtk_Clock_Modern",   Test_6'Address);
            Add_Test ("Gtk_Clock_Classic",  Test_5'Address);
         -- Waveforms
         Append (List, Parent, Null_Iter);
         Gtk.Missed.Set (List, Parent, 0, "Waveforms");
            Add_Test ("Waveform",               Test_27'Address);
            Add_Test ("Graph paper",            Test_28'Address);
            Add_Test ("Graph paper annotation", Test_34'Address);
         Append (List, Parent, Null_Iter);
         -- Refresh engines
         Gtk.Missed.Set (List, Parent, 0, "Refresh engine");
            Add_Test ("Clocks", Test_16'Address);
         -- Done with the list
      Unset (Ptr);
      Set_Rules_Hint (View, True);
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 0);
         Column_No := View.Append_Column (Column);
         Column.Set_Title ("Test");
         Column.Set_Resizable (True);
         Column.Set_Sort_Column_Id (0);
      end;
      Set_Mode (Get_Selection (View), Selection_Single);
      Set_Model (View, To_Interface (List));
      Unref (List);
      View.Expand_All;
      Get_Selection (View).On_Changed (+On_Selection'Access);
      Gtk_New (Scroll);
      Scroll.Add (View);
      Scroll.Set_Size_Request (300, 300);
      Gtk_New (Get_CSS_Button);
      Get_CSS_Button.Set_Label ("Take CSS file from");
      Get_CSS_Button.On_Clicked (+On_Get_RC'Access);
      Gtk_New (Get_PDF_Button);
      Get_PDF_Button.Set_Label ("Write PDF");
      Get_PDF_Button.On_Clicked (+On_Get_PDF'Access);
      Gtk_New_VBox (Box);
      Box.Pack_Start (Scroll);
      Box.Pack_Start (Get_CSS_Button,  False, False);
      Box.Pack_Start (Get_PDF_Button, False, False);
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_Etched_In);
      Frame.Add (Box);
      Test_Pane.Add1 (Frame);
      declare
         Size : Gtk_Requisition;
      begin
         View.Columns_Autosize;    -- Size columns
         View.Size_Request (Size); -- Query the integral size
         View.Set_Size_Request     -- Set new size
         (  GInt'Min (Size.Width,  600),
            GInt'Min (Size.Height, 400)
         );
      end;
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_Etched_In);
      Main_Pane.Add2 (Frame);
      Gtk_New (Editor);
      Frame.Add (Editor);
         -- Load button
      Gtk_New (Load_Button, "Load");
      Load_Button.Set_Sensitive (False);
      Editor.Get_Buttons_Box.Pack_Start (Load_Button, False, False);
      Load_Button.On_Clicked (+On_Load'Access);
         -- Save button
      Gtk_New (Save_Button, "Save");
      Save_Button.Set_Sensitive (False);
      Editor.Get_Buttons_Box.Pack_Start (Save_Button, False, False);
      Save_Button.On_Clicked (+On_Save'Access);
   end;
   Window.Show_All;
   Test_IO;
   Test_Ring_Buffer;
   Gtk.Main.Main;
end Test_AICWL;
