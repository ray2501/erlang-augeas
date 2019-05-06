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
-spec new(Root::string(), Loadpath::string(), Flags::integer()) -> reference() | {error, any()}.

new(_, _, _) ->
    not_loaded(?LINE).

%% @doc Lookup the value associated with PATH
-spec get(Aug::reference(), Path::string()) -> string() | {error, any()}.

get(_, _) ->
    not_loaded(?LINE).

%% @doc Set the value associated with PATH to VALUE
-spec set(Aug::reference(), Path::string(), Value::string()) -> ok | {error, any()}.

set(_, _, _) ->
    not_loaded(?LINE).

%% @doc Set the value of multiple nodes in one operation
-spec setm(Aug::reference(), Base::string(), Sub::string(), Value::string()) -> integer() | {error, any()}.

setm(_, _, _, _) ->
    not_loaded(?LINE).

%% @doc Create a new sibling LABEL for PATH by inserting into the tree
-spec insert(Aug::reference(), Path::string(), Value::string(), Before::integer()) -> ok | {error, any()}.

insert(_, _, _, _) ->
    not_loaded(?LINE).

%% @doc Remove path and all its children
-spec rm(Aug::reference(), Path::string()) -> integer() | {error, any()}.

rm(_, _) ->
    not_loaded(?LINE).

%% @doc Move the node SRC to DST
-spec mv(Aug::reference(), Src::string(), Dst::string()) -> ok | {error, any()}.

mv(_, _, _) ->
    not_loaded(?LINE).

%% @doc Copy the node SRC to DST
-spec cp(Aug::reference(), Src::string(), Dst::string()) -> ok | {error, any()}.

cp(_, _, _) ->
    not_loaded(?LINE).

%% @doc Rename the label of all nodes matching SRC to LBL
-spec rename(Aug::reference(), Src::string(), Lbl::string()) -> integer() | {error, any()}.

rename(_, _, _) ->
    not_loaded(?LINE).

%% @doc the number of matches of the path expression PATH in AUG
-spec match(Aug::reference(), Path::string()) -> list(string) | {error, any()}.

match(_, _) ->
    not_loaded(?LINE).

%% @doc Write all pending changes to disk
-spec save(Aug::reference()) -> ok | {error, any()}.

save(_) ->
    not_loaded(?LINE).

%% @doc Close this Augeas instance and free any storage associated with it
-spec close(Aug::reference()) -> ok | {error, any()}.

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

