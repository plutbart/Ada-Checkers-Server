with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters;
with Ada.Streams; use type Ada.Streams.Stream_Element_Count;
with GNAT.Sockets;  use GNAT.Sockets;
with GNAT.Regpat;   use GNAT.Regpat;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with parser;
with game;

package requestHandler is
    package UB renames Ada.Strings.Unbounded;

    Request : parser.RequestRecord;
    CRLF : String := (1 => ASCII.CR, 2 => ASCII.LF);
    Send : String := (1 => ASCII.CR, 2 => ASCII.LF, 3 => ASCII.CR, 4 => ASCII.LF);

    type HTTPversion is (http11,http2);
    type HeaderRecord is record
      header : UB.Unbounded_String;
      value : UB.Unbounded_String;
    end record;

    type HTTPheaders is array(1 .. 256) of HeaderRecord;
    subtype RequestPath is UB.Unbounded_String;
    type HTTPmethod is (get, head, post, put, delete);

    type RequestData is record
      method : UB.Unbounded_String;
      path : UB.Unbounded_String;
      data : UB.Unbounded_String;
    end record;

    type RequestRecord is record
      method : HTTPmethod;
      path : RequestPath;
      version: HTTPversion;
      headers: HTTPheaders;
    end record;

    Offset : Ada.Streams.Stream_Element_Count;
    Data : Ada.Streams.Stream_Element_Array (1 .. 512);
    HTTPrequest: UB.Unbounded_String;
    Index : Ada.Streams.Stream_Element_Offset;

    task SendResponse is
      entry Init(Channel: Stream_Access; Data : String);
    end SendResponse;

    function Handle(Data: Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset; Channel: Stream_Access) return RequestData;

    function getPostData(Data : in Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset) return JSON_Value;

    procedure printRequest(Path : UB.Unbounded_String; Data : in Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset);

    procedure printData(Data : in Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset);

    procedure printResponse(Response : String);

    function isAPIrequest(Path : UB.Unbounded_String) return Boolean;

end requestHandler;
