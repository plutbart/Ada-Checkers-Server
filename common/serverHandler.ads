with Ada.Text_IO;
with Ada.Streams; use type Ada.Streams.Stream_Element_Count;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with GNAT.Sockets;  use GNAT.Sockets;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Numerics.discrete_Random;
with serverEngine;
with Game;
with Parser;
with RequestHandler;
with GNAT.Regpat;   use GNAT.Regpat;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package serverHandler is
  pragma Suppress (Elaboration_Check);

  GameServer : Game.GameServer;
  CheckingIndicator : Boolean := false with atomic;

  package UB renames Ada.Strings.Unbounded;

  task ServerHandling is
    entry Start;
    entry HandleRequest(Data: Ada.Streams.Stream_Element_Array; Offset: Ada.Streams.Stream_Element_Offset; Channel: Stream_Access);
  end ServerHandling;

  task CheckGame is
    entry Start;
  end CheckGame;

  function getGameServer return game.GameServer;

  function getResponse(Method : String; Path : String; Data : String) return String;

  procedure removeAbandoned;

  procedure printRequest(Req : requestHandler.RequestData);

end serverHandler;
