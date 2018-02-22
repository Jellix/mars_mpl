--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Wall_Clock.Modern                 Luebeck            --
--  Interface                                      Winter, 2010       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

with Ada.Calendar;

with Gtk.Adjustment;
with Gtk.Handlers.References;
with Gtk.Layered.Cache;
with Gtk.Layered.Elliptic_Annotation;
with Gtk.Layered.Elliptic_Background;
with Gtk.Layered.Elliptic_Scale;
with Gtk.Layered.Needle;

package Gtk.Wall_Clock.Modern is

   --
   -- Class_Name - Of the widget
   --
   Class_Name : constant String := "GtkWallClockModern";

   --
   -- Gtk_Clock_Modern -- Modern clock
   --
   type Gtk_Wall_Clock_Modern_Record is
     new Gtk.Layered.Gtk_Layered_Record with private;
   type Gtk_Wall_Clock_Modern is
     access all Gtk_Wall_Clock_Modern_Record'Class;

   --
   -- Get_Type -- The type of the widget
   --
   -- Returns :
   --
   --    The GTK type of the widget
   --
   function Get_Type return GType;

   --
   -- Gtk_New -- Widget construction
   --
   --    Widget     - The result
   --    Adjustment - The adjustment object to indicate
   --
   procedure Gtk_New
     (Widget     : out Gtk_Wall_Clock_Modern;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null);

   --
   -- Initialize -- The widget initialization
   --
   --    Widget     - The widget to initialize
   --    Adjustment - The adjustment object to indicate
   --
   -- When  a  widget  type  is  derived from this one, it has to call this
   -- procedure from its version of Initialize.
   --
   procedure Initialize
     (Widget     : not null access Gtk_Wall_Clock_Modern_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);

   --
   -- Get_Annotation -- The clock annotation
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The annotation layer
   --
   function Get_Annotation
     (Widget : not null access Gtk_Wall_Clock_Modern_Record)
      return not null access Gtk.Layered.Elliptic_Annotation.Elliptic_Annotation_Layer;

   --
   -- Get_Background -- The clock's background
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The background layer
   --
   function Get_Background
     (Widget : not null access Gtk_Wall_Clock_Modern_Record)
      return not null access Gtk.Layered.Elliptic_Background.Elliptic_Background_Layer;

   --
   -- Get_Hour_Hand -- The clock needle
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The hour needle layer of the widget
   --
   function Get_Hour_Hand
     (Widget : not null access Gtk_Wall_Clock_Modern_Record)
      return not null access Gtk.Layered.Needle.Needle_Layer;

   --
   -- Get_Minute_Hand -- The clock needle
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The minute needle layer of the widget
   --
   function Get_Minute_Hand
     (Widget : not null access Gtk_Wall_Clock_Modern_Record)
      return not null access Gtk.Layered.Needle.Needle_Layer;

   --
   -- Get_Second_Hand -- The clock needle
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The second needle layer of the widget
   --
   function Get_Second_Hand
     (Widget : not null access Gtk_Wall_Clock_Modern_Record)
      return not null access Gtk.Layered.Needle.Needle_Layer;

   --
   -- Get_Cache -- The gauge caching layer
   --
   --    Widget - The widget
   --
   -- If the widget is extended, static things which do not change with the
   -- widget state should be placed below the caching layer for performance
   -- reasons.
   --
   -- Returns :
   --
   --    The cache layer of the widget
   --
   function Get_Cache
     (Widget : not null access Gtk_Wall_Clock_Modern_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer;

   --
   -- Set_Value -- Change the value indicated by the gauge
   --
   --    Widget - The widget
   --    Value  - The value
   --
   procedure Set_Value
     (Widget : not null access Gtk_Wall_Clock_Modern_Record;
      Value  : Ada.Calendar.Time);

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Wall_Clock_Modern_Record);

private

   type Gtk_Wall_Clock_Modern_Record is
     new Gtk.Layered.Gtk_Layered_Record with
      record
         Adjustment    : Gtk.Adjustment.Gtk_Adjustment;
         Changed       : Gtk.Handlers.References.Handler_Reference;
         Value_Changed : Gtk.Handlers.References.Handler_Reference;
         Background    : access Gtk.Layered.Elliptic_Background.Elliptic_Background_Layer;
         Pin           : access Gtk.Layered.Elliptic_Background.Elliptic_Background_Layer;
         Cache         : access Gtk.Layered.Cache.Cache_Layer;
         Minor_Ticks   : access Gtk.Layered.Elliptic_Scale.Elliptic_Scale_Layer;
         Major_Ticks   : access Gtk.Layered.Elliptic_Scale.Elliptic_Scale_Layer;
         Annotation    : access Gtk.Layered.Elliptic_Annotation.Elliptic_Annotation_Layer;
         Hour_Needle   : access Gtk.Layered.Needle.Needle_Layer;
         Minute_Needle : access Gtk.Layered.Needle.Needle_Layer;
         Second_Needle : access Gtk.Layered.Needle.Needle_Layer;
      end record;

   procedure Set
     (Widget  : not null access Gtk_Wall_Clock_Modern_Record;
      Seconds : Gdouble);

end Gtk.Wall_Clock.Modern;