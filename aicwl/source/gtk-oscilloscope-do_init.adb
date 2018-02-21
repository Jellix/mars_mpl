--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--        Do_Init                                  Summer, 2011       --
--  Separate body                                                     --
--                                Last revision :  07:54 21 Jul 2016  --
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

pragma Warnings (Off, "declaration hides ""Refresh_Period""");
pragma Warnings (Off, "declaration hides ""Widget""");

separate (Gtk.Oscilloscope)
procedure Do_Init
  (Widget                       : not null access Gtk_Oscilloscope_Record'Class;
   Lower_Sweeper, Upper_Sweeper : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
   Refresh_Engine               : access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;
   Refresh_Period               : Duration;
   Background                   : Gdk.Color.Gdk_Color;
   Buffer_Size                  : Positive)
is
   use type Gdk.Event.Gdk_Event_Mask;
begin
   G_New (Widget, Get_Type);
   Gtk.Grid.Initialize (Widget);
   declare
      Widget_Type : constant GType := Get_Type (Widget);
   begin
      for Index in Signal_Names'Range loop
         if Signal_IDs (Index) = Invalid_Signal_Id then
            Signal_IDs (Index) :=
              Lookup
                (Widget_Type,
                 Glib.Signal_Name
                   (String'(Interfaces.C.Strings.Value (Signal_Names (Index)))));
            if Signal_IDs (Index) = Invalid_Signal_Id then
               raise Program_Error
                 with ("Unable to find signal " &
                         Interfaces.C.Strings.Value (Signal_Names (Index)));
            end if;
         end if;
      end loop;
   end;
   Widget.all.Groups := new Group_List (1 .. Group_Number (Widget.all.Size));
   Widget.all.Selection :=
     new Selection_State (Group_Number (Widget.all.Size));
   Widget.all.Buffer_Size := Buffer_Size;
   Widget.all.Layers.all.Oscilloscope := Widget.all'Unchecked_Access;
   Gtk.Layered.Initialize (Widget.all.Layers);
   Widget.all.Layers.all.Set_Hexpand (True);
   Widget.all.Layers.all.Set_Vexpand (True);
   Widget.all.Attach (Widget.all.Layers, 0, 0, 1, 1);
   Widget.all.Values_Axis (Left)  .Justify_X := Ada.Strings.Right;
   Widget.all.Values_Axis (Middle).Justify_X := Ada.Strings.Left;
   Widget.all.Values_Axis (Right) .Justify_X := Ada.Strings.Left;
   if Lower_Sweeper = null then
      Gtk.Layered.Waveform.Sweeper.Gtk_New
        (Widget.all.Time_Axis (Lower).Sweeper);
   else
      Widget.all.Time_Axis (Lower).Sweeper :=
        Lower_Sweeper.all'Unchecked_Access;
   end if;
   Widget.all.Time_Axis (Lower).Sweeper.all.Ref;
   if Upper_Sweeper = null then
      Gtk.Layered.Waveform.Sweeper.Gtk_New
        (Widget.all.Time_Axis (Upper).Sweeper);
   else
      Widget.all.Time_Axis (Upper).Sweeper :=
        Upper_Sweeper.all'Unchecked_Access;
   end if;
   Widget.all.Time_Axis (Upper).Sweeper.all.Ref;
   for Index in Widget.all.Time_Axis'Range loop
      Sweeper_Handlers.Connect
        (Widget.all.Time_Axis (Index).Sweeper,
         "freezing-changed",
         On_Freezing_Changed'Access,
         Widget.all'Unchecked_Access);
   end loop;
   Widget.all.Background :=
     Gtk.Layered.Rectangle.Add_Rectangle
       (Under   => Widget.all.Layers,
        Box     => (-0.5, -0.5, 0.5, 0.5),
        Color   => Background,
        Opacity => 1.0);
   Gtk.List_Store.Gtk_New
     (Widget.all.Channel_Names,
      (0 => GType_String,
       1 => GType_Int,
       2 => GType_Int,
       3 => GType_Boolean,
       4 => GType_Int,
       5 => GType_String,
       6 => GType_Boolean,
       7 => GType_Boolean));
   Gtk.List_Store.Gtk_New (Widget.all.Group_Names, (0 => GType_String));
   --
   -- Mouse tracking and buttons clicks events
   --
   Widget.all.Layers.all.Set_Events
     (Gdk.Event.Exposure_Mask
      or Gdk.Event.Leave_Notify_Mask
      or Gdk.Event.Button_Press_Mask
      or Gdk.Event.Button_Release_Mask
      or Gdk.Event.Pointer_Motion_Mask
      or Gdk.Event.Pointer_Motion_Hint_Mask);
   Oscilloscope_Handlers.Connect
     (Widget.all.Layers,
      "button_press_event",
      Oscilloscope_Handlers.To_Marshaller (On_Button_Press'Access),
      Widget.all'Unchecked_Access);
   Oscilloscope_Handlers.Connect
     (Widget.all.Layers,
      "button_release_event",
      Oscilloscope_Handlers.To_Marshaller (On_Button_Release'Access),
      Widget.all'Unchecked_Access);
   Oscilloscope_Handlers.Connect
     (Widget.all.Layers,
      "button_release_event",
      Oscilloscope_Handlers.To_Marshaller (On_Button_Release'Access),
      Widget.all'Unchecked_Access);
   Oscilloscope_Handlers.Connect
     (Widget.all.Layers,
      "motion_notify_event",
      Oscilloscope_Handlers.To_Marshaller (On_Motion'Access),
      Widget.all'Unchecked_Access);
   Menu_Handlers.Connect
     (Widget,
      "destroy",
      On_Destroy'Access,
      Widget.all'Unchecked_Access);
   Menu_Handlers.Connect
     (Widget,
      "style-updated",
      On_Style_Updated'Access,
      Widget.all'Unchecked_Access);
   if Refresh_Engine = null then
      Widget.all.Refresh_Engine := new Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;
      Widget.all.Refresh_Engine.all.Set_Period (Refresh_Period);
      Widget.all.Refresh_Engine.all.Add (Widget.all.Layers);
   else
      Refresh_Engine.all.Add (Widget.all.Layers);
   end if;
   Widget.all.Set_App_Paintable (True);
   On_Style_Updated (Widget, Widget.all'Unchecked_Access);
end Do_Init;

pragma Warnings (On, "declaration hides ""Refresh_Period""");
pragma Warnings (On, "declaration hides ""Widget""");
