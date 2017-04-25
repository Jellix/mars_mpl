--                                                                    --
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
--____________________________________________________________________--

separate (Gtk.Oscilloscope)
   procedure Do_Init
             (  Widget : not null access Gtk_Oscilloscope_Record'Class;
                Lower_Sweeper, Upper_Sweeper :
                         access Gtk_Waveform_Sweeper_Record'Class;
                Refresh_Engine : access Layered_Refresh_Engine;
                Refresh_Period : Duration;
                Background     : Gdk_Color;
                Buffer_Size    : Positive
             )  is
   use Sweeper_Handlers;
   use Interfaces.C.Strings;
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
               (  Widget_Type,
                  Glib.Signal_Name
                  (  String'(Value (Signal_Names (Index)))
               )  );
            if Signal_IDs (Index) = Invalid_Signal_Id then
               raise Program_Error
                  with
                  (  "Unable to find signal "
                  &  Interfaces.C.Strings.Value
                     (  Signal_Names (Index)
                  )  );
            end if;
         end if;
      end loop;
   end;
   Widget.Groups := new Group_List (1..Group_Number (Widget.Size));
   Widget.Selection :=
      new Selection_State (Group_Number (Widget.Size));
   Widget.Buffer_Size := Buffer_Size;
   Widget.Layers.Oscilloscope := Widget.all'Unchecked_Access;
   Gtk.Layered.Initialize (Widget.Layers);
   Widget.Layers.Set_Hexpand (True);
   Widget.Layers.Set_Vexpand (True);
   Widget.Attach (Widget.Layers, 0, 0, 1, 1);
   Widget.Values_Axis (Left  ).Justify_X := Right;
   Widget.Values_Axis (Middle).Justify_X := Left;
   Widget.Values_Axis (Right ).Justify_X := Left;
   if Lower_Sweeper = null then
      Gtk_New (Widget.Time_Axis (Lower).Sweeper);
   else
      Widget.Time_Axis (Lower).Sweeper :=
         Lower_Sweeper.all'Unchecked_Access;
   end if;
   Widget.Time_Axis (Lower).Sweeper.Ref;
   if Upper_Sweeper = null then
      Gtk_New (Widget.Time_Axis (Upper).Sweeper);
   else
      Widget.Time_Axis (Upper).Sweeper :=
         Upper_Sweeper.all'Unchecked_Access;
   end if;
   Widget.Time_Axis (Upper).Sweeper.Ref;
   for Index in Widget.Time_Axis'Range loop
      Connect
      (  Widget.Time_Axis (Index).Sweeper,
         "freezing-changed",
         On_Freezing_Changed'Access,
         Widget.all'Unchecked_Access
      );
   end loop;
   Widget.Background :=
      Add_Rectangle
      (  Under   => Widget.Layers,
         Box     => (-0.5, -0.5, 0.5, 0.5),
         Color   => Background,
         Opacity => 1.0
      );
   Gtk_New
   (  Widget.Channel_Names,
      (  0 => GType_String,
         1 => GType_Int,
         2 => GType_Int,
         3 => GType_Boolean,
         4 => GType_Int,
         5 => GType_String,
         6 => GType_Boolean,
         7 => GType_Boolean
   )  );
   Gtk_New (Widget.Group_Names, (0 => GType_String));
   --
   -- Mouse tracking and buttons clicks events
   --
   Widget.Layers.Set_Events
   (  Exposure_Mask
   or Leave_Notify_Mask
   or Button_Press_Mask
   or Button_Release_Mask
   or Pointer_Motion_Mask
   or Pointer_Motion_Hint_Mask
   );
   Oscilloscope_Handlers.Connect
   (  Widget.Layers,
      "button_press_event",
      Oscilloscope_Handlers.To_Marshaller (On_Button_Press'Access),
      Widget.all'Unchecked_Access
   );
   Oscilloscope_Handlers.Connect
   (  Widget.Layers,
      "button_release_event",
      Oscilloscope_Handlers.To_Marshaller (On_Button_Release'Access),
      Widget.all'Unchecked_Access
   );
   Oscilloscope_Handlers.Connect
   (  Widget.Layers,
      "button_release_event",
      Oscilloscope_Handlers.To_Marshaller (On_Button_Release'Access),
      Widget.all'Unchecked_Access
   );
   Oscilloscope_Handlers.Connect
   (  Widget.Layers,
      "motion_notify_event",
      Oscilloscope_Handlers.To_Marshaller (On_Motion'Access),
      Widget.all'Unchecked_Access
   );
   Menu_Handlers.Connect
   (  Widget,
      "destroy",
      On_Destroy'Access,
      Widget.all'Unchecked_Access
   );
   Menu_Handlers.Connect
   (  Widget,
      "style-updated",
      On_Style_Updated'Access,
      Widget.all'Unchecked_Access
   );
   if Refresh_Engine = null then
      Widget.Refresh_Engine := new Layered_Refresh_Engine;
      Widget.Refresh_Engine.Set_Period (Refresh_Period);
      Widget.Refresh_Engine.Add (Widget.Layers);
   else
      Refresh_Engine.Add (Widget.Layers);
   end if;
   Widget.Set_App_Paintable (True);
   On_Style_Updated (Widget, Widget.all'Unchecked_Access);
end Do_Init;
