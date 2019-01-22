package body parser is

    procedure parseRequest(Data : in Ada.Streams.Stream_Element_Array; Index : in out Ada.Streams.Stream_Element_Offset; Request : out RequestRecord) is
    begin
        Headers(1) := (header => UB.To_Unbounded_String("host"), value  => UB.To_Unbounded_String("my_host"));

        methodObtain(Data,Index,Method);
        Index := Ada.Streams.Stream_Element_Offset'Succ(Index);
        targetObtain(Data,Index,Target);

        Request := (method  => Method
                   ,path    => Target
                   ,version => http11
                   ,headers => Headers);
    end parseRequest;

    procedure methodObtain(Data:Ada.Streams.Stream_Element_Array; Index: in out Ada.Streams.Stream_Element_Offset; Method: out HTTPmethod) is
        Line_Buffer : String(1..6) := "      ";
        Pos : Integer range 0 .. 6 := 0;
    begin
        while Data(Index) /= 32 and Pos <= 6 loop
            Pos := Pos + 1;
            Line_Buffer(Pos) := Character'Val(Data(Index));
            Index := Ada.Streams.Stream_Element_Offset'Succ(Index);
        end loop;

        Method := HTTPmethod'Value(Line_Buffer);
    exception
        When CONSTRAINT_ERROR => Method := Get;

    end methodObtain;

    procedure targetObtain(Data : Ada.Streams.Stream_Element_Array; Index : in out Ada.Streams.Stream_Element_Offset; Target : out RequestPath) is
        Max : Integer range 0 .. 256 := 0;
    begin
        Target := UB.To_Unbounded_String("");
        while Data(Index) /= 32 and Max <= 256 loop
            Max := Max + 1;
            UB.append(Target, Character'Val(Data(Index)));
            Index := Ada.Streams.Stream_Element_Offset'Succ(Index);
        end loop;
    end targetObtain;

    function getMethod(Req : RequestRecord) return String is
      type Methods is (GET, POST);
    begin
      case Methods'Value (HTTPmethod'Image(Req.method)) is
         when GET => return "GET";
         when POST => return "POST";
         when others => return "null";
      end case;
    end getMethod;

    function getPath(Req : RequestRecord) return String is
      Path : String := UB.To_String(Req.path);
    begin
      return Path;
    end getPath;

end parser;
