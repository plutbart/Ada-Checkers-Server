with Ada.Text_IO;
with Ada.Streams; use type Ada.Streams.Stream_Element;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package parser is

    package UB renames Ada.Strings.Unbounded;

    type HTTPmethod is (get, head, post, put, delete);
    type HTTPversion is (http11,http2);
    subtype RequestPath is UB.Unbounded_String;

    type HeaderRecord is record
      header : UB.Unbounded_String;
      value : UB.Unbounded_String;
    end record;

    type HTTPheaders is array(1 .. 256) of HeaderRecord;

    type RequestRecord is record
      method : HTTPmethod;
      path : RequestPath;
      version : HTTPversion;
      headers : HTTPheaders;
    end record;

    Request : RequestRecord;
    Method : HTTPmethod;
    Target : RequestPath;
    Headers : HTTPheaders;

    procedure parseRequest(Data : in Ada.Streams.Stream_Element_Array; Index : in out Ada.Streams.Stream_Element_Offset; Request : out RequestRecord);

    procedure methodObtain(Data : Ada.Streams.Stream_Element_Array; Index : in out Ada.Streams.Stream_Element_Offset; Method : out HTTPmethod);

    procedure targetObtain(Data : Ada.Streams.Stream_Element_Array; Index : in out Ada.Streams.Stream_Element_Offset; Target : out RequestPath);

    function getMethod(Req:RequestRecord) return String;

    function getPath(Req:RequestRecord) return String;

end parser;
