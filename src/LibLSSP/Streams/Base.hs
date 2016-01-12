-- InputGameContext a
-- OutputGameContext b
-- Log [Text]
-- Input Text
-- Output Text

{-
type TextIOM = Free TextIO
type ContextTextIOM = StateT Context TextIOM
type ContextSource = Source ContextTextIOM StartInfo
type ContextConduit a b = Conduit a ContextTextIOM b
type ContextSink a b = Sink a ContextTextIOM b
-}

{-
+--------------------------------------+　　　　　　　AI Start
|             Connection               |  Rule Info
|    +----------------------------+    | <---------
|    |      Specify Protocol      |    |
|    +----------------------------+    |
|    |     Notify Server Info     |    |
|    +----------------------------+    |
|                                      |
+--------------------------------------+
|           Rule Consensus             |
|    +----------------------------+    |
|    |     Intention of Client    |    |
|    +----------------------------+    |
|    | Create Consensus on Server |    | Consensus
|    +----------------------------+    |--------->
|    |    Consensus of Client     |    |<---------
|    +----------------------------+    | Answer
|                                      |
+--------------------------------------+
|             Game Start               |  Is-Ready
|    +----------------------------+    |----------->
|    |       Wait to Ready        |    |
|    +----------------------------+    |<-----------
|    |         Ready Game         |    |
|    +----------------------------+    |
|    |         Game Start         |    |
|    +----------------------------+    |
|                                      |
+--------------------------------------+
|          Game Communicate            |
|    +----------------------------+    |
|    |      Notify Game Context   |    |   Notify
|    +----------------------------+    |------------>
|    | +------------------------+ |    |
|    | |       Game Stop        | |    |   Stop
|    | +------------------------+ |    |------------>
|    |             or             |    |
|    | +------------------------+ |    |   Action
|    | |     Action of Client   | |    |<-----------
|    | +------------------------+ |    |
|    +----------------------------+    |   Status
|    |     Return Game Status     |    |----------->
|    +----------------------------+    |
|                                      |
+--------------------------------------+
|              Game End                |
|    +----------------------------+    |  Result
|    |         Game End           |    |---------->
|    +----------------------------+    |
|                                      |
+--------------------------------------+
-}
