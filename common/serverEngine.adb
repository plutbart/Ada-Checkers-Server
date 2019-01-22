with serverHandler; use serverHandler;

package body serverEngine is
  -- pragma Elaborate_Body (serverEngine);
  pragma Suppress (Elaboration_Check);

    task body serverRunner is
    TimeOut : Duration := 10.0;

    begin
      accept run;
        loop
          case Getopt ("-port=") is
            when '-' =>
              if Full_Switch = "-port" then
                Port := Port_Type'Value(Parameter);
                else
                  Ada.Text_IO.Put_Line("Cannot parse " & Full_Switch);
                  exit;
                end if;
              when others =>
                exit;
           end case;
        end loop;

        Ada.Text_IO.Put_Line("Starting server on port : " & Port_Type'Image(Port));

        Address.Addr := Addresses(Get_Host_By_Name (Host_Name), 1);
        Address.Port := Port;

        Create_Socket(Server);
        Set_Socket_Option(Server, Socket_Level, (Reuse_Address, Enabled => True));
        Set_Socket_Option(Server, Socket_Level, (Receive_Timeout, Timeout => TimeOut));

        Bind_Socket(Server,Address);
        Listen_Socket(Server);
        loop
            Accept_Socket(Server, Socket, Address);
            Channel := Stream(Socket);
            begin
              Receive_Socket(Socket, Data, Offset);
              ServerHandler.ServerHandling.HandleRequest(Data,Offset,Channel);

            exception
              when Ada.IO_Exceptions.End_Error=>
                null;
              when Socket_Error =>
                Errors := Errors + 1;
                exit when Errors = 10;
          end;
          Close_Socket(Socket);
        end loop;
    end serverRunner;
end serverEngine;
