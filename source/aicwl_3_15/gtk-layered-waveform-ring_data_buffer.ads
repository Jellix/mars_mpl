--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform.                       Luebeck            --
--        Ring_Data_Buffer                         Winter, 2011       --
--  Interface                                                         --
--                                Last revision :  16:49 28 Feb 2016  --
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

package Gtk.Layered.Waveform.Ring_Data_Buffer is

   --
   -- Ring_Data_Buffer_Record -- The waveform ring data buffer.
   --
   --    Size - Maximal number of points in the buffer
   --
   -- The  buffer  can  be  written  from one task and scanned from several
   -- other tasks without interlocking. When more than two tasks write  the
   -- buffer it must be protected from concurrent access.
   --
   type Gtk_Wavefrom_Ring_Data_Buffer_Record (Size : Positive) is
     new GObject_Record
     and Waveform_Data_Feed with private;
   type Gtk_Wavefrom_Ring_Data_Buffer is
     access all Gtk_Wavefrom_Ring_Data_Buffer_Record'Class;

   --
   -- Gtk_New -- Factory
   --
   --    Source - The result
   --    Size   - Maximal number of points in the buffer
   --
   procedure Gtk_New
     (Source : out Gtk_Wavefrom_Ring_Data_Buffer;
      Size   : Positive);

   --
   -- Initialize -- Construction
   --
   --    Source - The result
   --
   -- This  procedure  must  be called  from  the Initialize of any derived
   -- type.
   --
   procedure Initialize
     (Source : not null access Gtk_Wavefrom_Ring_Data_Buffer_Record'Class);

   --
   -- Source_Scanner -- The buffer scanner object
   --
   -- The   object   implements  the  Waveform_Data_Scanner  interface  for
   -- scanning  Ring_Data_Buffer.  The operation Create of Ring_Data_Buffer
   -- returns an instance of this type.
   --
   type Source_Scanner is new Waveform_Data_Scanner with private;

   overriding procedure Backward
     (Source : in out Source_Scanner;
      T      : in out X_Axis;
      V      : out Y_Axis);

   overriding procedure Backward
     (Source : in out Source_Scanner;
      T      : in out X_Axis;
      V      : out Y_Axis;
      Got_It : out Boolean);

   overriding procedure Connected
     (Source : in out Gtk_Wavefrom_Ring_Data_Buffer_Record;
      Layer  : in out Waveform_Layer'Class);

   overriding function Create
     (Source : not null access Gtk_Wavefrom_Ring_Data_Buffer_Record)
      return Waveform_Data_Scanner'Class;

   overriding procedure Disconnected
     (Source : in out Gtk_Wavefrom_Ring_Data_Buffer_Record;
      Layer  : in out Waveform_Layer'Class);

   overriding procedure Erase
     (Source : in out Gtk_Wavefrom_Ring_Data_Buffer_Record);

   overriding procedure First
     (Source : in out Source_Scanner;
      T      : out X_Axis;
      V      : out Y_Axis);

   overriding procedure First
     (Source : in out Source_Scanner;
      T      : out X_Axis;
      V      : out Y_Axis;
      Got_It : out Boolean);

   overriding procedure Forward
     (Source : in out Source_Scanner;
      T      : in out X_Axis;
      V      : out Y_Axis);

   overriding procedure Forward
     (Source : in out Source_Scanner;
      T      : in out X_Axis;
      V      : out Y_Axis;
      Got_It : out Boolean);

   overriding function Get_Source
     (Scanner : Source_Scanner)
      return not null access Waveform_Data_Source'Class;

   overriding function Is_In
     (Source : Source_Scanner;
      T      : X_Axis) return Boolean;

   overriding procedure Last
     (Source : in out Source_Scanner;
      T      : out X_Axis;
      V      : out Y_Axis);

   overriding procedure Last
     (Source : in out Source_Scanner;
      T      : out X_Axis;
      V      : out Y_Axis;
      Got_It : out Boolean);

   overriding procedure Put
     (Source : in out Gtk_Wavefrom_Ring_Data_Buffer_Record;
      T      : X_Axis;
      V      : Y_Axis);

private

   pragma Warnings (Off, "declaration hides ""Points_Array""");

   Initial_Size : constant := 16;

   type Point is record
      T : X_Axis;
      V : Y_Axis;
      pragma Atomic (T);
      pragma Atomic (V);
   end record;
   type Points_Array is array (Positive range <>) of Point;

   type Waveform_Layer_Ptr is access all Waveform_Layer'Class;
   type Layers_List is
     array (Positive range <>) of Waveform_Layer_Ptr;
   type Layers_List_Ptr is access Layers_List;
   type List is new Ada.Finalization.Controlled
     with record
      Connected : Natural   := 0;
      Ptr       : Layers_List_Ptr := new Layers_List (1 .. Initial_Size);
   end record;

   overriding procedure Finalize (Object : in out List);

   type Reference is mod 2 ** System.Word_Size;
   -- type Reference is new Unsigned_64;

   type Gtk_Wavefrom_Ring_Data_Buffer_Record (Size : Positive) is
     new GObject_Record
     and Waveform_Data_Feed with
      record
         First  : Reference := 1;
         Length : Reference := 0;
         Wrap   : Reference := Reference (Size);
         Layers : List;
         Data   : Points_Array (1 .. Size);
         pragma Atomic (First);
         pragma Atomic (Length);
      end record;

   --
   -- Find - Point in the buffer
   --
   --    Source - To search through
   --    T      - To search for
   --    Above  - Constraint
   --    Index  - The reference
   --    Got_It - Set false when T < Buffer if Above = true
   --                         or T > Buffer if Above = false
   --
   -- When Above is true, the result is greater or equal to T. Otherwise it
   -- is less or equal to T.
   --
   procedure Find
     (Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
      T      : X_Axis;
      Above  : Boolean;
      Index  : out Reference;
      Got_It : out Boolean);

   procedure Get
     (Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
      Index  : Reference;
      Data   : out Point;
      Got_It : out Boolean);

   function Is_In
     (Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
      Index  : Reference) return Boolean;

   procedure Notify
     (Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
      From   : X_Axis;
      To     : X_Axis);

   procedure Scan_Backward
     (Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
      Index  : in out Reference;
      T      : in out X_Axis;
      V      : out Y_Axis;
      Got_It : out Boolean);

   procedure Scan_Forward
     (Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
      Index  : in out Reference;
      T      : in out X_Axis;
      V      : out Y_Axis;
      Got_It : out Boolean);

   type Source_Scanner is new Waveform_Data_Scanner
     with record
      Index  : Reference := 1;
      T      : X_Axis    := X_Axis'Last;
      Source : not null access Gtk_Wavefrom_Ring_Data_Buffer_Record;
   end record;

   pragma Inline (Find);
   pragma Inline (First);
   pragma Inline (Get);
   pragma Inline (Is_In);
   pragma Inline (Last);
   pragma Inline (Scan_Backward);
   pragma Inline (Scan_Forward);

   pragma Warnings (On, "declaration hides ""Points_Array""");

end Gtk.Layered.Waveform.Ring_Data_Buffer;
