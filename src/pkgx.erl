%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Main

-module(pkgx).

-export([main/1]).

main(Targets) ->
    ok = application:load(erlydtl),
    ok = application:load(pkgx),
    VarsFile = "pkgx.config",
    case filelib:is_regular(VarsFile) of
        false ->
            cli_error("File not found: " ++ VarsFile);
        true ->
            {ok, PkgVars} = file:consult(VarsFile),
            {package_name, PkgName} = proplists:lookup(package_name, PkgVars),
            ReleasesFile = "_rel/releases/RELEASES",
            case filelib:is_regular(ReleasesFile) of
                false ->
                    cli_error("No RELEASE file found for " ++ PkgName ++ ". Run './relx release' first.");
                true ->
                    {ok, [ReleasesList0]} = file:consult(ReleasesFile),
                    [Release|_] = lists:sort(ReleasesList0),
                    {release, AppName, Vsn, ErtsVsn, _Deps, _Permanent} = Release,
                    io:format(user, "Using release: ~s ~s~n", [AppName, Vsn]),
                    Vars = [{app, AppName}, {version, Vsn}, {erts_version, ErtsVsn}, {basedir, "_rel"} | PkgVars],
                    [ok = run_target(AppName, Vsn, Vars, T) || T <- Targets],
                    ok
            end
    end.

run_target(AppName, Vsn, PkgVars, T) ->
    try
        Target = list_to_existing_atom("pkgx_target_" ++ T),
        ok = Target:run(AppName, Vsn, PkgVars)
    catch
        _:badarg ->
            io:format(user, "Unknown target: ~s~n", [T]),
            ok
    end.
            


usage() ->
    io:format(user, "usage: pkgx <package-x.y.tar.gz> <deb|rpm>~n", []),
    ok.

cli_error(Msg) ->
    io:format(user, "error: ~s~n", [Msg]),
    ok.
    
