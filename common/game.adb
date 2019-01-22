with serverHandler; use serverHandler;

package body game is

  function init(ID : Integer) return GameInstance is
    game : GameInstance;
  begin
    game.ID := ID;
    game.Turn := 0;
    game.Players(1) := getNewPlayerID;
    game.Players(2) := getNewPlayerID(game.Players(1));
    game.GameBoard := initBoard(game.Players(1),game.players(2));
    game.Privileged := initPrivileged;
    game.hasBeenActive := true;
    return game;
  end init;

  function getNewPlayerID return PlayerID is
    PlayerIdGen : RandPlayerID.Generator;
    RandomPlayerID : Integer := 0;
  begin
    RandPlayerID.Reset(PlayerIdGen);
    RandomPlayerID := Integer(RandPlayerID.Random(PlayerIdGen) mod 89999 + 10000);
    return RandomPlayerID;
  end getNewPlayerID;

  function getNewPlayerID(SecondPlayerID : PlayerID) return PlayerID is
    PlayerIdGen : RandPlayerID.Generator;
    RandomPlayerID : Integer := 0;
  begin
    RandPlayerID.Reset(PlayerIdGen);
    RandomPlayerID := Integer(RandPlayerID.Random(PlayerIdGen) mod 89999 + 10000);
    while RandomPlayerID = SecondPlayerID loop
      RandomPlayerID := Integer(RandPlayerID.Random(PlayerIdGen) mod 89999 + 10000);
    end loop;
    return RandomPlayerID;
  end getNewPlayerID;

  function isGameIdAllowed(G : GameServer; ID : game.GameID) return Boolean is
  begin
    for I in G.CurrentGames.First_Index .. G.CurrentGames.Last_Index loop
      if G.CurrentGames(I).ID = ID then
        return false;
      end if;
    end loop;
    return true;
  end isGameIdAllowed;

  function getNewGameID(G: GameServer) return game.GameID is
    GameIdGen : RandGameID.Generator;
    RandomGameID : Integer := 0;
  begin
    RandGameID.Reset(GameIdGen);
    RandomGameID := Integer(RandGameID.Random(GameIdGen) mod 8999 + 1000);
    while isGameIdAllowed(G,RandomGameID) = false loop
      RandomGameID := Integer(RandGameID.Random(GameIdGen) mod 8999 + 1000);
    end loop;
    return RandomGameID;
  end getNewGameID;

  function "+" (X : in GameInstance; Y : in GameInstance) return Boolean is
  begin
    if X.ID /= Y.ID then
      return false;
    end if;
    if X.Turn /= Y.Turn then
      return false;
    end if;
    if X.Players(1) /= Y.Players(1) then
      return false;
    end if;
    if X.Players(2) /= Y.Players(2) then
      return false;
    end if;
    return true;
  end "+";

  function joinToTheGame(G : in out GameInstance) return PlayerID is
    NewPlayerID : PlayerID := getNewPlayerID(G.Players(1));
  begin
    G.Turn := G.Players(1); --NewPlayerID;
    G.Players(2) := NewPlayerID;
    G.GameBoard := initBoard(G.Players(1),G.Players(2));
    G.hasBeenActive := true;
    -- Ada.Text_IO.Put_Line(Write(getGameState(G)));
    return NewPlayerID;
  end joinToTheGame;

  function initNewGameResponse(GS : in out GameServer) return String is
    ID : GameID := getNewGameID(GS);
    NewGame : GameInstance;
    Data : JSON_Value := Create_Object;
    -- NewGame : GameInstance := init(getNewGameID(GS));
  begin
    NewGame := init(ID);
    NewGame.hasBeenActive := true;
    Data.Set_Field("gameID", Create(NewGame.ID));
    Data.Set_Field("board", Create(boardToJSON(NewGame.GameBoard)));
    Data.Set_Field("player1", Create(NewGame.Players(1)));
    Data.Set_Field("player2", Create(NewGame.Players(2)));
    Data.Set_Field("thisPlayer", Create(NewGame.Players(1)));
    Data.Set_Field("turn", Create(NewGame.Turn));

    GS.CurrentGames.append(NewGame);
    printResponse(Write(Data));

    return Write(Data);
  end initNewGameResponse;

  function getMoveResponse(ID : Integer; RequestData : String; GS : in out GameServer) return String is
    Data : JSON_Value;
    ResponseData : JSON_Value := Create_Object;
    GameID : Integer;
    PlayerID : Integer;
    From : JSON_Array := Empty_Array;
    To : JSON_Array := Empty_Array;
    FromX : Integer;
    FromY : Integer;
    ToX : Integer;
    ToY : Integer;
    NewFrom : JSON_Array := Empty_Array;
    NewTo : JSON_Array := Empty_Array;
    FromPosition : Position;
    toPosition : Position;
  begin
    if RequestData = "" then
      ResponseData.Set_Field("error","Server has not revceived sufficient amount of data");
      printResponse(Write(ResponseData));
      return Write(ResponseData);
    end if;

    Data := Read(RequestData);

    if Data.Has_Field("gameID") and Data.Has_Field("playerID") and Data.Has_Field("from") and Data.Has_Field("to") then

      for I in GS.CurrentGames.First_Index .. GS.CurrentGames.Last_Index loop
        if GS.CurrentGames(I).ID = ID then
          GS.CurrentGames(I).hasBeenActive := true;
          GameID := Data.get("gameID");
          PlayerID := Data.get("playerID");
          From := Data.get("from");
          To := Data.get("to");

          if Length(From) < 2 or Length(To) < 2 then
            ResponseData.Set_Field("error","Position dimension mismatch");
            return Write(ResponseData);
          end if;

          FromX := Get(Get(From,1));
          FromY := Get(Get(From,2));

          ToX := Get(Get(To,1));
          ToY := Get(Get(To,2));

          FromPosition.X := FromX;
          FromPosition.Y := FromY;

          ToPosition.X := ToX;
          ToPosition.Y := ToY;

          if (testMove(PlayerID, FromPosition, ToPosition, GS.CurrentGames(I))) then
            ResponseData.Set_Field("message","OK");
          else
            ResponseData.Set_Field("message","Invalid move");
          end if;

          GS.CurrentGames(I).GameBoard := move(PlayerID, FromPosition, ToPosition, GS.CurrentGames(I));-- move(PlayerID, FromPosition, getDirection(FromPosition, ToPosition, PlayerID,  GS.CurrentGames(I)), GS.CurrentGames(I), GS.CurrentGames(I).GameBoard);
          ResponseData.Set_Field("gameID",Create(GS.CurrentGames(I).ID));
          ResponseData.Set_Field("board",Create(boardToJSON(GS.CurrentGames(I).GameBoard)));
          ResponseData.Set_Field("turn",Create(GS.CurrentGames(I).Turn));

          printResponse(Write(ResponseData));
          return Write(ResponseData);
        end if;
      end loop;

      Append(NewFrom,Create(FromX));
      Append(NewFrom,Create(FromY));

      Append(NewTo,Create(ToX));
      Append(NewTo,Create(ToY));

      ResponseData.Set_Field("gameID",Create(GameID));
      ResponseData.Set_Field("PlayerID",Create(GameID));
      ResponseData.Set_Field("From",Create(NewTo));
      ResponseData.Set_Field("To",Create(NewFrom));
      printResponse(Write(ResponseData));

      return Write(ResponseData);
    else
      ResponseData.Set_Field("error", "Insufficient arguments. Needed fields: 'gameID', 'playerID', 'from' and 'to'");
      printResponse(Write(ResponseData));
      return Write(ResponseData);
    end if;
  end getMoveResponse;

  function getStateResponse(ID : GameID; GS : in out GameServer) return String is
    Data : JSON_Value := Create_Object;
    begin
      for I in GS.CurrentGames.First_Index .. GS.CurrentGames.Last_Index loop
        if GS.CurrentGames(I).ID = ID then
          GS.CurrentGames(I).hasBeenActive := true;
          Data := getGameState(GS.CurrentGames(I));
          return Write(Data);
        end if;
      end loop;
      Data.Set_Field("error", "No game of given GameID");
      printResponse(Write(Data));
      return Write(Data);
  end getStateResponse;

  function joinGameResponse(ID: GameID; GS : in out GameServer) return String is
    Data : JSON_Value := Create_Object;
    NewPlayerID : PlayerID;
  begin
    for I in GS.CurrentGames.First_Index .. GS.CurrentGames.Last_Index loop
      if GS.CurrentGames(I).ID = ID then
        GS.CurrentGames(I).hasBeenActive := true;
        NewPlayerID := joinToTheGame(GS.CurrentGames(I));

        Data.Set_Field("gameID", Create(GS.CurrentGames(I).ID));
        Data.Set_Field("board", Create(boardToJSON(GS.CurrentGames(I).GameBoard)));
        Data.Set_Field("player1", Create(GS.CurrentGames(I).Players(1)));
        Data.Set_Field("player2", Create(GS.CurrentGames(I).Players(2)));
        Data.Set_Field("thisPlayer", Create(GS.CurrentGames(I).Players(2)));
        Data.Set_Field("turn", Create(GS.CurrentGames(I).Turn));

        printResponse(Write(Data));
        return Write(Data);
      end if;
    end loop;
    Data.Set_Field("error", "No game of given GameID");
    printResponse(Write(Data));
    return Write(Data);
  end joinGameResponse;

  procedure printGame(game : GameInstance) is
  begin
    Ada.Text_IO.Put_Line("ID : " & game.ID'Img);
    Ada.Text_IO.Put_Line("Turn : " & game.Turn'Img);
    Ada.Text_IO.Put_Line("Player 1 ID : " & game.Players(1)'Img);
    Ada.Text_IO.Put_Line("Player 2 ID : " & game.Players(2)'Img);
  end printGame;

  function initPrivileged return Board is
  begin
    return (1 => (others=>0),
            2 => (others=>0),
            3 => (others=>0),
            4 => (others=>0),
            5 => (others=>0),
            6 => (others=>0),
            7 => (others=>0),
            8 => (others=>0));
  end initPrivileged;

  function initBoard(Player1 : PlayerID; Player2 : PlayerID) return Board is
  begin
    return (1 => (1 => Player2, 3=> Player2, 5 => Player2, 7 => Player2, others=>0),
  	        2 => (2 => Player2, 4=> Player2, 6 => Player2, 8 => Player2, others=>0),
  	        3 => (1 => Player2, 3=> Player2, 5 => Player2, 7 => Player2, others=>0),
  	        6 => (2 => Player1, 4=> Player1, 6 => Player1, 8 => Player1, others=>0),
  	        7 => (1 => Player1, 3=> Player1, 5 => Player1, 7 => Player1, others=>0),
  	        8 => (2 => Player1, 4=> Player1, 6 => Player1, 8 => Player1, others=>0),
  	        others => (others => 0));
  end initBoard;

  function testMove(ID : PlayerID; fromPosition:Position; toPosition : Position; State : GameInstance) return Boolean is
    direction : Integer := (if (State.Players(1) = ID) then 1 else -1);

    distX : Integer := toPosition.x - fromPosition.x;
    distY : Integer := toPosition.y - fromPosition.y;

    x : Integer := toPosition.x;
    y : Integer := toPosition.y;
  begin

    if (State.Turn /= ID) then
      -- Ada.Text_IO.Put_Line("===> NOT MY TURN");
      return false;
    end if;

    if (x < 1 or x > 8 or y < 1 or y > 8) then
      -- Ada.Text_IO.Put_Line("===> WALL");
      return false;
    end if;

    if (State.GameBoard(fromPosition.x,fromPosition.y) /= ID) then
      -- Ada.Text_IO.Put_Line("===> NOT MY PAWN");
      return false;
    end if;

    if (abs distX /= abs distY) then
      -- Ada.Text_IO.Put_Line("===> WRONG DIRECTION");
      return false;
    end if;

    if (State.GameBoard(x,y) /= 0) then
      -- Ada.Text_IO.Put_Line("===> OTHER PLAYER IS THERE");
      return false;
    end if;

    if (State.Privileged(fromPosition.x,fromPosition.y) /= 0) then
      -- Ada.Text_IO.Put_Line("===> PAWN IS PRIVILEGED");
      for i in 1 .. abs distX loop
        if (distX > 0) then
          if (distY > 0) then
            if State.GameBoard(fromPosition.x+i,fromPosition.y+i) = ID then
              return false;
            end if;
          else
            if State.GameBoard(fromPosition.x+i,fromPosition.y-i) = ID then
              return false;
            end if;
          end if;
        else
          if (distY > 0) then
            if State.GameBoard(fromPosition.x-i,fromPosition.y+i) = ID then
              return false;
            end if;
          else
            if State.GameBoard(fromPosition.x-i,fromPosition.y-i) = ID then
              return false;
            end if;
          end if;
        end if;
      end loop;

      return true;
    end if;

    if (abs distX = 1 and abs distY = 1) then
      if (distX*direction >= 0) then
        -- Ada.Text_IO.Put_Line("===> MOVING BACKWARDS");
        return false;
      end if;
      -- Ada.Text_IO.Put_Line("===> IS FINE");
      return true;
    end if;
    if (abs distX = 2 and abs distY = 2) then
      -- Ada.Text_IO.Put_Line("===> DISTANCE = 2");
      if (State.GameBoard(fromPosition.x+(distX/2),fromPosition.y+(distY/2)) = ID or State.GameBoard(fromPosition.x+(distX/2),fromPosition.y+(distY/2)) = 0) then
        -- Ada.Text_IO.Put_Line("===> CANNOT OVERTAKE");
        return false;
      end if;
      -- Ada.Text_IO.Put_Line("===> IS FINE");
      return true;
    end if;
      -- Ada.Text_IO.Put_Line("===> TOO FAR");
    return false;

  end testMove;

  function move(ID : PlayerID; fromPosition: Position; toPosition : Position; State : in out GameInstance) return Board is
    direction : Integer := (if (State.Players(1) = ID) then 1 else -1);

    distX : Integer := toPosition.x - fromPosition.x;
    distY : Integer := toPosition.y - fromPosition.y;

    x : Integer := toPosition.x;
    y : Integer := toPosition.y;
  begin
    if (testMove(ID, fromPosition, toPosition, State)) then
      for i in 1 .. abs distX-1 loop
        if (distX > 0) then
          if (distY > 0) then
            State.GameBoard(fromPosition.x+i,fromPosition.y+i) := 0;
          else
            State.GameBoard(fromPosition.x+i,fromPosition.y-i) := 0;
          end if;
        else
          if (distY > 0) then
            State.GameBoard(fromPosition.x-i,fromPosition.y+i) := 0;
          else
            State.GameBoard(fromPosition.x-i,fromPosition.y-i) := 0;
          end if;
        end if;
      end loop;

      State.GameBoard(fromPosition.x,fromPosition.y) := 0;
      State.GameBoard(toPosition.x,toPosition.y) := ID;

      if (State.Privileged(fromPosition.x,fromPosition.y) /= 0) then
        State.Privileged(fromPosition.x,fromPosition.y) := 0;
        State.Privileged(toPosition.x,toPosition.y) := 1;
      end if;

      if (State.Players(1) = ID) then
        if (toPosition.x = 1) then
          State.Privileged(toPosition.x,toPosition.y) := 1;
        end if;
      end if;

      if (State.Players(2) = ID) then
        if (toPosition.x = 8) then
          State.Privileged(toPosition.x,toPosition.y) := 1;
        end if;
      end if;

      State.Turn := (if (State.Players(1) = ID) then State.Players(2) else State.Players(1));
    end if;
    return State.GameBoard;
  end move;

  function getWinner(Game: GameInstance) return Integer is
    Winner : Integer := 0;
  begin
    for I in Game.GameBoard'Range(1) loop
      for J in Game.GameBoard'Range(2) loop
        if Game.GameBoard(I,J) /= 0 then
          if Winner = 0 then
            Winner := Game.GameBoard(I,J);
          elsif Game.GameBoard(I,J) /= Winner then
            return 0;
          end if;
        end if;
      end loop;
    end loop;

    return Winner;
  end getWinner;

  function getGameState(Game : GameInstance) return JSON_Value is
    GameState : JSON_Value := Create_Object;
  begin
    GameState.Set_Field("gameID",Create(Game.ID));
    GameState.Set_Field("player1", Create(Game.Players(1)));
    GameState.Set_Field("player2", Create(Game.Players(2)));
    GameState.Set_Field("board",Create(boardToJSON(Game.GameBoard)));
    GameState.Set_Field("winner",Create(getWinner(Game)));
    GameState.Set_Field("turn",Create(Game.Turn));
    return GameState;
  end getGameState;

  procedure printResponse(Response : String) is
  begin
    Ada.Text_IO.Put_Line("=========<< RESPONSE START >>==========");
    Ada.Text_IO.Put_Line(Response);
    Ada.Text_IO.Put_Line("==========<< RESPONSE END >>===========");
    Ada.Text_IO.Put_Line("");
  end printResponse;

  function boardToJSON(GameBoard: Board) return JSON_Array is
    BoardArray : JSON_Array := Empty_Array;
    RowArray : JSON_Array := Empty_Array;
  begin
    for i in 1..8 loop
      RowArray := Empty_Array;
      for j in 1..8 loop
        Append(RowArray,Create(GameBoard(i,j)));
      end loop;
      Append(BoardArray,Create(RowArray));
    end loop;
    return BoardArray;
  end boardToJSON;

end game;
