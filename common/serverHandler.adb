package body serverHandler is
  pragma Suppress (Elaboration_Check);

  task body ServerHandling is
    CurrentRequest : RequestHandler.RequestData;
  begin
    loop
      select
        accept Start do
          ServerEngine.ServerRunner.Run;
        end Start;
      or
        accept HandleRequest(Data: Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset; Channel: Stream_Access) do
          CurrentRequest := requestHandler.Handle(Data,Offset,Channel);
          RequestHandler.SendResponse.Init(Channel,
            getResponse(
              UB.To_String(CurrentRequest.method),
              UB.To_String(CurrentRequest.path),
              UB.To_String(CurrentRequest.data)
          ));
        end HandleRequest;
      end select;
    end loop;
  end ServerHandling;

  task body CheckGame is
  begin
    loop
      accept Start;
        removeAbandoned;
    end loop;
  end CheckGame;

  procedure removeAbandoned is
  begin
    CheckingIndicator := true;
    while CheckingIndicator loop
      delay 5.0;
      for J in reverse 1 .. GameServer.CurrentGames.Last_Index loop
        if GameServer.CurrentGames(J).hasBeenActive = false or GameServer.CurrentGames(J).winner /= 0 then
          Ada.Text_IO.Put_Line(">> GAME ::" & Integer'Image(GameServer.CurrentGames(J).ID) & " :: HAS BEEN REMOVED");
          GameServer.CurrentGames.Delete(J);
        end if;
        GameServer.CurrentGames(J).hasBeenActive := false;
      end loop;
    end loop;
  end removeAbandoned;

  function getGameServer return game.GameServer is
  begin
    return GameServer;
  end getGameServer;

  procedure printRequest(Req : requestHandler.RequestData) is
  begin
    Ada.Text_IO.Put_Line( UB.To_String(Req.method));
    Ada.Text_IO.Put_Line( UB.To_String(Req.path));
    Ada.Text_IO.Put_Line( UB.To_String(Req.data));
  end printRequest;

  function getResponse(Method : String; Path : String; Data : String) return String is
    Matches : Match_Array (0 .. 1);
    ID : Match_Array (0 .. 1);
    Response : String := "";
  begin

    if Method = "GET" then
      Match(Compile("^\/game\/?$"), Path, Matches);
      if Matches(0) /= No_Match then
        return game.initNewGameResponse(GameServer);
      end if;

      Match(Compile("/game/([0-9]+)$"), Path, Matches);
      if Matches (0) /= No_Match then
        return game.joinGameResponse(Integer'Value(Path(Matches(1).First .. Matches (1).Last)), GameServer);
      end if;

      Match(Compile("/game/([0-9]+)/state"), Path, Matches);
      if Matches(0) /= No_Match then
        return game.getStateResponse(Integer'Value(Path(Matches(1).First .. Matches (1).Last)), GameServer);
      end if;
    end if;

    if Method = "POST" then
      Match(Compile("/game/([0-9]+)/move"), Path, Matches);
      if Matches(0) /= No_Match then
        return game.getMoveResponse(Integer'Value(Path(Matches(1).First .. Matches (1).Last)), Data, GameServer);
      end if;
    end if;
    return "UNDEFINIED REQUEST";
  end getResponse;

end serverHandler;
