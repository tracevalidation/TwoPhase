--------------------------- MODULE TwoPhaseTrace ---------------------------
(***************************************************************************)
(* Trace spec that refines 2PC *)
(***************************************************************************)

EXTENDS TLC, Sequences, SequencesExt, Naturals, FiniteSets, Bags, Json, IOUtils, TwoPhase, TVOperators, TraceSpec

(* Override CONSTANTS *)

(* Replace Nil constant *)
TraceNil == "null"

(* Replace RM constant using the Config set in TraceSpec (from CONFIG_PATH) *)
TraceRM ==
    ToSet(Config[1].RM)

(* Can be extracted from init *)
TPDefault(varName) ==
    CASE varName = "rmState" -> [r \in RM |-> "working"]
    []  varName = "tmState" -> "init"
    []  varName = "tmPrepared" -> {}
    []  varName = "msgs" -> {}

TPUpdateVariables(t) ==
    /\
        IF "rmState" \in DOMAIN t
        THEN rmState' = UpdateVariable(rmState, "rmState", t)
        ELSE TRUE
    /\
        IF "tmState" \in DOMAIN t
        THEN tmState' = UpdateVariable(tmState, "tmState", t)
        ELSE TRUE
    /\
        IF "tmPrepared" \in DOMAIN t
        THEN tmPrepared' = UpdateVariable(tmPrepared, "tmPrepared", t)
        ELSE TRUE
    /\
        IF "msgs" \in DOMAIN t
        THEN msgs' = UpdateVariable(msgs, "msgs", t)
        ELSE TRUE

(* Predicate actions *)
IsTMCommit ==
    /\ IsEvent("TMCommit")
    /\ TMCommit

IsTMAbort ==
    /\ IsEvent("TMAbort")
    /\ TMAbort

IsTMRcvPrepared ==
    /\ IsEvent("TMRcvPrepared")
    /\
        IF "event_args" \in DOMAIN logline /\ Len(logline.event_args) >= 1 THEN
            TMRcvPrepared(logline.event_args[1])
        ELSE
            \E r \in RM : TMRcvPrepared(r)

IsRMPrepare ==
    /\ IsEvent("RMPrepare")
    /\
        IF "event_args" \in DOMAIN logline /\ Len(logline.event_args) >= 1 THEN
            RMPrepare(logline.event_args[1])
        ELSE
            \E r \in RM : RMPrepare(r)

IsRMRcvCommitMsg ==
    /\ IsEvent("RMRcvCommitMsg")
    /\
        IF "event_args" \in DOMAIN logline /\ Len(logline.event_args) >= 1 THEN
            RMRcvCommitMsg(logline.event_args[1])
        ELSE
            \E r \in RM : RMRcvCommitMsg(r)

IsRMRcvAbortMsg ==
    /\ IsEvent("RMRcvAbortMsg")
    /\
        IF "event_args" \in DOMAIN logline /\ Len(logline.event_args) >= 1 THEN
            RMRcvAbortMsg(logline.event_args[1])
        ELSE
            \E r \in RM : RMRcvAbortMsg(r)

TPTraceNext ==
        \/ IsTMCommit
        \/ IsTMAbort
        \/ IsTMRcvPrepared
        \/ IsRMPrepare
        \/ IsRMRcvCommitMsg
        \/ IsRMRcvAbortMsg

(* Eventually composed actions *)
ComposedNext == FALSE

BaseSpec == TPInit /\ [][TPNext \/ ComposedNext]_vars
-----------------------------------------------------------------------------
=============================================================================