-- InputGameContext a
-- OutputGameContext b
-- Log [Text]
-- Input Text
-- Output Text

{-
liftTextIO :: (MonadTextIO m) => TextIO a -> m a
type TextIOM = Free TextIO
type EHContextM = StateT EventHandlers TextIOM
type EHM = ErrorT ProtocolError EHContextM
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

{-
(MonadIO m) =>
on start     :: ()      -> m Rule
on consensus :: Info    -> m Answer
on ready     :: ()      -> m ()
on go        :: Context -> m Action
on stop      :: ()      -> m ()
on status    :: Status  -> m ()
on end       :: Result  -> m ()
-}

{-
type Receiver = Conduit a m (a, b)
type Sender   = Conduit a m a
-}
