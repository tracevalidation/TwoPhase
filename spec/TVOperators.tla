(* Generic module that aims to handle operators and applications *)
(* Just need to import it, and override some operators in order to have a trace spec that works *)
---- MODULE TVOperators ----
EXTENDS TLC, Sequences, SequencesExt, Naturals, FiniteSets, Bags, Json, IOUtils

(* Constant to override *)
CONSTANT Nil
(* Operators to override *)
Default(varName) ==  Print(<<"Trace spec isn't valid, you should override 'Default' operator.">>, Nil)
MapArgs(mapFunction, cur, op, args) == Print(<<"Trace spec isn't valid, you should override 'MapArgs' operator.">>, Nil)
MapArgsBase(mapFunction, cur, op, args) == args

(* Generic operators *)
Update(cur, val) == val

AddElement(cur, val) == cur \cup {val}
AddElements(cur, vals) == cur \cup ToSet(vals)
RemoveElement(cur, val) == cur \ {val}
Clear(cur, val) == {}

AddElementToBag(cur, val) ==
    IF val \in DOMAIN cur THEN
        [cur EXCEPT ![val] = cur[val] + 1]
    ELSE
        cur @@ (val :> 1)
RemoveElementFromBag(cur, val) ==
    IF val \in DOMAIN cur THEN
        [cur EXCEPT ![val] = cur[val] - 1]
    ELSE
        cur
ClearBag(cur, val) == <<>>

AppendElement(cur, val) == Append(cur, val)

ResetKey(cur, val) == [k \in DOMAIN cur |-> IF k = val THEN Nil ELSE cur[k]]
SetKey(cur, val, new) == [k \in DOMAIN cur |-> IF k = val THEN new ELSE cur[k]]
UpdateRec(cur, val) == [k \in DOMAIN cur |-> IF k \in DOMAIN val THEN val[k] ELSE cur[k]]

AddInteger(cur, val) == cur + val
SubInteger(cur, val) == cur - val

Unchanged(cur, val) == cur


Apply(op, var, args) ==
    CASE op = "Update" -> Update(var, args[1])
    []   op = "AddElement" -> AddElement(var, args[1])
    []   op = "AddElements" -> AddElements(var, args[1])
    []   op = "RemoveElement" -> RemoveElement(var, args[1])
    []   op = "Clear" -> Clear(var, {})
    []   op = "AddElementToBag" -> AddElementToBag(var, args[1])
    []   op = "RemoveElementFromBag" -> RemoveElementFromBag(var, args[1])
    []   op = "ClearBag" -> Clear(var, <<>>)
    []   op = "AppendElement" -> AppendElement(var, args[1])
    []   op = "ResetKey" -> ResetKey(var, args[1])
    []   op = "SetKey" -> SetKey(var, args[1],args[2])
    []   op = "UpdateRec" -> UpdateRec(var, args[1])
    []   op = "Add" -> AddInteger(var, args[1])
    []   op = "Sub" -> SubInteger(var, args[1])
    []   op = "Unchanged" -> Unchanged(var, args[1])

RECURSIVE ExceptAtPath(_,_,_,_)
LOCAL ExceptAtPath(op, var, path, args) ==
    LET h == Head(path) IN
    IF Len(path) > 1 THEN
        [var EXCEPT ![h] = ExceptAtPath(op, var[h], Tail(path), args)]
    ELSE
        [var EXCEPT ![h] = Apply(op, @, args)]

RECURSIVE ApplyUpdates(_,_)
LOCAL ApplyUpdates(var, updates) ==
    LET update == Head(updates) IN

    LET applied ==
        IF Len(update.path) > 0 THEN
            ExceptAtPath(update.op, var, update.path, update.args)
        ELSE
            LET mapArgs ==
                IF "map" \in DOMAIN update THEN
                    MapArgs(update.op, update.map, var, update.args)
                ELSE
                    update.args
            IN
            Apply(update.op, var, mapArgs)
    IN
    IF Len(updates) > 1 THEN
        ApplyUpdates(applied, Tail(updates))
    ELSE
        applied

====
