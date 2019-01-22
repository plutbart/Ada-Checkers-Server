with Ada.Text_IO;
with Ada.Streams; use type Ada.Streams.Stream_Element;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Numerics.discrete_Random;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters;
with GNATCOLL.JSON; use GNATCOLL.JSON;
limited with serverHandler;

package game is

    package UB renames Ada.Strings.Unbounded;

    subtype GameID is Integer range 1000 .. 9999;
    subtype PlayerID is Integer range 10000 .. 99999;

    type Board is array (1..8,1..8) of Integer;

    subtype PlayerAccess is Integer range 0 .. 99999;
    type PlayerList is array (Positive range <>) of PlayerID;

    type GameInstance is tagged record
    	ID : Integer := 0;
    	GameBoard : Board;
      Privileged : Board;
    	Winner : Integer := 0;
    	Turn : Integer := 0;
      Players : PlayerList(1..2);
      hasBeenActive : Boolean;
    end record;

    type Position is record
      X,Y : Integer := 8;
    end record;

    procedure printGame(game : GameInstance);

    function init(ID : Integer) return GameInstance;

    function "+" (X : in GameInstance; Y : in GameInstance) return Boolean;

    function joinToTheGame(G : in out GameInstance) return PlayerID;

    function move(ID : PlayerID; fromPosition: Position; toPosition : Position; State : in out GameInstance) return Board;

    function testMove(ID : PlayerID; fromPosition:Position; toPosition : Position; State : GameInstance) return Boolean;

    function getGameState(Game : GameInstance) return JSON_Value;

    function getWinner(Game: GameInstance) return Integer;

    function boardToJSON(GameBoard: Board) return JSON_Array;

    package RandPlayerID is new Ada.Numerics.Discrete_Random(PlayerID);
    package RandGameID is new Ada.Numerics.Discrete_Random(game.GameID);
    package GameList is new Ada.Containers.Vectors(Index_Type => Natural,Element_Type => GameInstance);

    type GameServer is tagged record
         CurrentGames : GameList.Vector;
    end record;

    function initBoard(Player1 : PlayerID; Player2 : PlayerID) return Board;

    function initPrivileged return Board;

    function getNewPlayerID(SecondPlayerID : PlayerID) return PlayerID;

    function getNewPlayerID return PlayerID;

    function isGameIdAllowed(G : GameServer; ID : game.GameID) return Boolean;

    function getNewGameID(G: GameServer) return game.GameID;

    function initNewGameResponse(GS : in out GameServer) return String;

    function joinGameResponse(ID: GameID; GS : in out GameServer) return String;

    function getStateResponse(ID : GameID; GS : in out GameServer) return String;

    function getMoveResponse(ID : Integer; RequestData : String; GS : in out GameServer) return String;

    procedure printResponse(Response : String);


end game;
