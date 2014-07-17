%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Debian package generation for relx

-module(pkgx_target_deb).

-export([run/3]).

run(AppName, Vsn, PkgVars) ->
    FileMap =
        [
         {"debian/changelog", deb_debian_changelog_dtl},
         {"debian/compat", <<"7">>},
         {"debian/control", deb_debian_control_dtl},
         {"debian/copyright", deb_debian_copyright_dtl},
         {"debian/postinst", deb_debian_postinst_dtl},
         {"debian/postrm", deb_debian_postrm_dtl},
         {"debian/rules", deb_debian_rules_dtl},
         {"debian/" ++ AppName ++ ".init", deb_debian_init_dtl},
         {"debian/" ++ AppName ++ ".install", deb_debian_install_dtl},
         {AppName ++ ".config", package_config_dtl}
        ],
    Basedir = proplists:get_value(basedir, PkgVars),
    lists:map(fun ({F, V}) ->
                      TargetFile = filename:join(Basedir, F),
                      filelib:ensure_dir(TargetFile),
                      process_file_entry(TargetFile, V, PkgVars)
              end, FileMap),
    Output = os:cmd("cd \"" ++ Basedir ++ "\" && debuild --no-tgz-check -i -us -uc -b"),
    io:format(user, "~s~n", [Output]),
    ok.

process_file_entry(File, Module, Vars) when is_atom(Module) ->
    {ok, Output} = Module:render(Vars),
    ok = file:write_file(File, Output);
    
process_file_entry(File, Output, _Vars) when is_binary(Output) ->
    ok = file:write_file(File, Output).
    



