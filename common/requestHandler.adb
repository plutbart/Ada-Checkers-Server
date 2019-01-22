package body requestHandler is

  function getPostData(Data : in Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset) return JSON_Value is
    Char : Character := ' ';
    Line : UB.Unbounded_String := UB.To_Unbounded_String("");
    JSON_Data : JSON_Value;
  begin
    for I in 1 .. Offset loop
        Char := Character'Val(Data(I));
        if Char = Character'Val(10) then
          Line := UB.To_Unbounded_String("");
        else
          UB.Append(Line,Char);
        end if;
    end loop;

    if UB.To_String(Line)'Length > 0 then
      JSON_Data := Read(UB.To_String(Line));
    end if;

    return JSON_Data;
  end getPostData;

  procedure printRequest(Path : UB.Unbounded_String; Data : in Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset) is
  begin
    if isAPIrequest(Path) then
      printData(Data, Offset);
    end if;
  end printRequest;

  function isAPIrequest(Path : UB.Unbounded_String) return Boolean is
    Matches : Match_Array (0 .. 1);
  begin
    Match(Compile("^\/game\/?$"), UB.To_String(Path), Matches);
    if Matches(0) /= No_Match then
      return true;
    end if;

    Match(Compile("/game/([0-9]+)$"), UB.To_String(Path), Matches);
    if Matches (0) /= No_Match then
      return true;
    end if;

    Match(Compile("/game/([0-9]+)/move"), UB.To_String(Path), Matches);
    if Matches(0) /= No_Match then
      return true;
    end if;
    return false;
  end isAPIrequest;

  procedure printData(Data : in Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset) is
  begin
    Ada.Text_IO.Put_Line("==========<< REQUEST START >>==========");
    for I in 1 .. Offset loop
        Ada.Text_IO.Put(Character'Val(Data(I)));
    end loop;
    Ada.Text_IO.Put_Line("");
    Ada.Text_IO.Put_Line("===========<< REQUEST END >>===========");
  end printData;

  procedure printResponse(Response : String) is
  begin
    Ada.Text_IO.Put_Line("=========<< RESPONSE START >>==========");
    Ada.Text_IO.Put_Line(Response);
    Ada.Text_IO.Put_Line("==========<< RESPONSE END >>===========");
    Ada.Text_IO.Put_Line("");
  end printResponse;

  function Handle(Data: Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset; Channel: Stream_Access) return RequestData is
    Index : Ada.Streams.Stream_Element_Offset;
    RequestDataRecord : RequestData;
    RequestMethod : UB.Unbounded_String := UB.To_Unbounded_String("");
    RequestPath : UB.Unbounded_String := UB.To_Unbounded_String("");
    RequestData : UB.Unbounded_String := UB.To_Unbounded_String("");

    PostJSON : JSON_Value := getPostData(Data, Offset);

    Matches : Match_Array (0 .. 1);
  begin

    Index := Data'First;
    parser.parseRequest(Data,Index,Request);

    RequestMethod := UB.To_Unbounded_String(parser.getMethod(Request));
    RequestPath := UB.To_Unbounded_String(parser.getPath(Request));

    if Is_Empty(PostJSON) then
      RequestData := UB.To_Unbounded_String("");
    else
      RequestData := UB.To_Unbounded_String(Write(getPostData(Data, Offset)));
    end if;

    printRequest(RequestPath, Data, Offset);

    RequestDataRecord.method := RequestMethod;
    RequestDataRecord.path := RequestPath;
    RequestDataRecord.data := RequestData;

    return RequestDataRecord;
  end Handle;

  task body SendResponse is
  begin
    loop
      select
        accept Init(Channel: Stream_Access; Data : String) do
          String'Write(Channel,
               "HTTP/1.1 200 OK" & CRLF & "Access-Control-Allow-Origin: *" & CRLF & "Access-Control-Allow-Methods: GET, POST" & CRLF & "Access-Control-Allow-Headers: Content-Type" &
               Send &
               Send &
               Data & Send
            );
        end init;
      end select;
    end loop;
  end SendResponse;

end requestHandler;
