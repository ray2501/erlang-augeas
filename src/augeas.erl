%% ----------------------------------
%% Erlang bindings for Augeas
%% ----------------------------------

-module(augeas).

%% API exports
-export([new/3, get/2, set/3, setm/4, insert/4]).
-export([rm/2, mv/3, cp/3, rename/3, match/2]).
-export([save/1, close/1]).
-on_load(init/0).

-define(APPNAME, augeas).
-define(LIBNAME, "augeas_nif").

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize the augeas library

new(_, _, _) ->
    not_loaded(?LINE).

%% @doc Lookup the value associated with PATH

get(_, _) ->
    not_loaded(?LINE).

%% @doc Set the value associated with PATH to VALUE

set(_, _, _) ->
    not_loaded(?LINE).

%% @doc Set the value of multiple nodes in one operation

setm(_, _, _, _) ->
    not_loaded(?LINE).

%% @doc Create a new sibling LABEL for PATH by inserting into the tree

insert(_, _, _, _) ->
    not_loaded(?LINE).

%% @doc Remove path and all its children

rm(_, _) ->
    not_loaded(?LINE).

%% @doc Move the node SRC to DST

mv(_, _, _) ->
    not_loaded(?LINE).

%% @doc Copy the node SRC to DST

cp(_, _, _) ->
    not_loaded(?LINE).

%% @doc Rename the label of all nodes matching SRC to LBL

rename(_, _, _) ->
    not_loaded(?LINE).

%% @doc the number of matches of the path expression PATH in AUG

match(_, _) ->
    not_loaded(?LINE).

%% @doc Write all pending changes to disk

save(_) ->
    not_loaded(?LINE).

%% @doc Close this Augeas instance and free any storage associated with it

close(_) ->
    not_loaded(?LINE).


%%====================================================================
%% Internal functions
%%====================================================================

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

