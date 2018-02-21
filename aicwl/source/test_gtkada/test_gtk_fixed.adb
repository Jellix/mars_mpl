--                                                                    --
--  procedure Test_Gtk_Fixed        Copyright (c)  Dmitry A. Kazakov  --
--  Test for renderer                              Luebeck            --
--                                                 Spring, 2006       --
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

with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random; 
with GLib;                       use GLib;
with GLib.Properties;            use GLib.Properties;
with GLib.Values;                use GLib.Values;
with Gtk.Enums;                  use Gtk.Enums;
with Gdk.Event;                  use Gdk.Event;
with Gtk.List_Store;             use Gtk.List_Store;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtk.Cell_Renderer;          use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Fixed;    use Gtk.Cell_Renderer_Fixed;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;

with Ada.Unchecked_Conversion;
with Gtk.Main;
with Gtk.Missed;
with Test_Gtk_Fixed_Handlers;

procedure Test_Gtk_Fixed is

   Window     : Gtk_Window;
   Table_View : Gtk_Tree_View;
   Scroller   : Gtk_Scrolled_Window;

   type Local_Callback is access procedure 
        (  Cell  : access Gtk_Cell_Renderer_Fixed_Record'Class;
           Store : Gtk_List_Store
        );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Callback,
             Test_Gtk_Fixed_Handlers.Simple_Handler
          );

   procedure Commit
             (  Cell  : access Gtk_Cell_Renderer_Fixed_Record'Class;
                Store : Gtk_List_Store
             )  is
      Row   : constant Gtk_Tree_Iter :=
                       Get_Iter_From_String (Store, Get_Path (Cell));
      Value : GValue;
   begin
      if Row /= Null_Iter then
         Init (Value, GType_Double);
         Set_Double (Value, Get_Property (Cell, Build ("value")));
         Set_Value (Store, Row, 0, Value);
         Unset (Value);
      end if;
   end Commit;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Window.Set_Title ("Test Fixed-Point Cell Renderer");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   
   Gtk_New (Scroller);
   Gtk_New (Table_View);
   
   -- Creating a column of numbers (list store)
   declare
      Table : Gtk_List_Store;
   begin
      Gtk_New (Table, (0 => GType_Double));
      declare
         Row    : Gtk_Tree_Iter := Null_Iter;
         Value  : GValue;
         Source : Generator;
      begin
         Init (Value, GType_Double);
         -- Filling the column with random numbers
         for Item in 1..1000 loop
            Table.Append (Row);
            Set_Double
            (  Value,
               GDouble (100.0 * (Random (Source) - 0.5))
            );
            Set_Value (Table, Row, 0, Value);
         end loop;
         -- Attaching the column store to its view
         Table_View.Set_Model (To_Interface (Table));
         Unset (Value);
      end;
      -- Creating columns in the view
      declare
         Column_No : GInt;
         Column    : Gtk_Tree_View_Column;
         Numeric   : Gtk_Cell_Renderer_Fixed;
         Text      : Gtk_Cell_Renderer_Text;
      begin
         -- The first column will use the fixed-point renderer
         Gtk_New (Column);
         Column.Set_Title ("Value");
         Gtk_New (Numeric, 3);
         Numeric.Set_Mode (Cell_Renderer_Mode_Editable);
         Test_Gtk_Fixed_Handlers.Connect
         (  Numeric,
            "commit",
            +Commit'Access,
            Table
         );
         Column.Pack_Start (Numeric, False);
         -- Map column's renderer to the table's column 0
         Column.Add_Attribute (Numeric, "value", 0);
         Column_No := Table_View.Append_Column (Column);
         Column.Set_Resizable (True);
         Column.Set_Sort_Column_Id (0);

         -- The second column uses the standard text renderer
         Gtk_New (Column);
         Column.Set_Title ("Text");
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         -- Map column's renderer to the table's column 0
         Column.Add_Attribute (Text, "text", 0);
         Column_No := Table_View.Append_Column (Column);
         Column.Set_Resizable (True);
         Column.Set_Sort_Column_Id (0);
      end;
   end;
   Scroller.Set_Policy (Policy_Automatic, Policy_Automatic);
   Scroller.Add (Table_View);      
   Window.Add (Scroller);

   Table_View.Show;
   Scroller.Show;
   Window.Show;
   Gtk.Main.Main;
end Test_Gtk_Fixed;
