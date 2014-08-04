-define(URL, config:get_value(cdash_url)).
-define(USER, config:get_value(cdash_user)).
-define(PASS, config:get_value(cdash_password)).
-define(PLIST, config:get_value(projects_list)).
-define(NOTIFY, config:get_value(notify_strategy)).
-define(PURLS, config:get_value(show_urls)).
-define(KEYS, os:getenv("HOME") ++ "/.config/cdashbot/cdash_keys").
-define(USERS, config:get_value(cdash_users)).
-define(API_LIST, "/api/?method=project&task=list").
-define(API_SUMM, "/api/?method=build&task=sitetestfailures&project=").
-define(USERNAME, config:get_value(xmpp_account)).
-define(JSERVER, config:get_value(xmpp_host)).
-define(JDOMAIN, config:get_value(xmpp_domain)).
-define(PASSWORD, config:get_value(xmpp_password)).
-define(NICK, config:get_value(xmpp_room_nickname)).
-define(ROOM, config:get_value(xmpp_room)).
-define(HELP, file:read_file(os:getenv("HOME") ++ "/.config/cdashbot/cbothelp")).
-define(CONF, os:getenv("HOME")  ++ "/.config/cdashbot/cbotrc").
-define(CONT, config:get_value(xmpp_cont)).
-define(SUMM, "http://open.cdash.org/buildSummary.php?buildid=").
-define(API_VER, "/api/?method=cdash&task=version").
-define(API_BL, "/api/?method=build&task=list&project=").
-define(API_DI, "/api/?method=build&task=describe&bid=").
-define(API_CN, "&count=1").
-define(API_CN10, "&count=10").
-define(API_DL, "/api/?method=build&task=list&project=").
-define(API_DN, "&date=").
-define(API_DESC, "/api/?method=project&task=describe&project=").
-define(API_LOGIN, "/api/?method=project&task=login&project=").
-define(API_LOGIN_TOKEN, "&key=").
-define(API_SHED_ADD, "/api/?method=schedule&task=add&project=").
-define(API_SHED_TOKEN, "&token=").
-define(API_SHED_USER, "&user=").
-define(API_STATUS_LIST, "/api/?method=schedule&task=list&status=running").
-define(API_STATUS_DESCRIBE, "/api/?method=schedule&task=describe&schid=").
-define(API_SITE, "/api/?method=site&task=list").
-define(API_BUILD_SITE, "&site=").
-define(API_SITE_DESCRIBE, "/api/?method=site&task=describe&site=").
-define(API_BUILD_DIFF, "/api/?method=build&task=diff&bid=").
-define(BSUMM, "/buildSummary.php?buildid=").

%% ------------------------------------------------------------------
%% Records
%% ------------------------------------------------------------------

-record(jid, {nick, 
              jid}).