%%
%% Autogenerated by Thrift Compiler (0.9.1)
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(config_thrift).
-behaviour(thrift_service).


-include("config_thrift.hrl").

-export([struct_info/1, function_info/2]).

struct_info('i am a dummy struct') -> undefined.
%%% interface
% ping(This)
function_info('ping', params_type) ->
  {struct, []}
;
function_info('ping', reply_type) ->
  string;
function_info('ping', exceptions) ->
  {struct, []}
;
% set_service_down_threshold(This, N)
function_info('set_service_down_threshold', params_type) ->
  {struct, [{1, byte}]}
;
function_info('set_service_down_threshold', reply_type) ->
  {struct, []};
function_info('set_service_down_threshold', exceptions) ->
  {struct, [{1, {struct, {'api_types', 'invalidInput'}}}]}
;
% get_service_down_threshold(This, WhichDb)
function_info('get_service_down_threshold', params_type) ->
  {struct, [{1, i32}]}
;
function_info('get_service_down_threshold', reply_type) ->
  byte;
function_info('get_service_down_threshold', exceptions) ->
  {struct, []}
;
% get_filters(This, WhichDb)
function_info('get_filters', params_type) ->
  {struct, [{1, i32}]}
;
function_info('get_filters', reply_type) ->
  {list, {struct, {'api_types', 'filter'}}};
function_info('get_filters', exceptions) ->
  {struct, []}
;
% add_filters(This, Db, Filter)
function_info('add_filters', params_type) ->
  {struct, [{1, i32},
          {2, {list, {struct, {'api_types', 'filter'}}}}]}
;
function_info('add_filters', reply_type) ->
  {struct, []};
function_info('add_filters', exceptions) ->
  {struct, [{1, {struct, {'api_types', 'invalidInput'}}},
          {2, {struct, {'api_types', 'duplicateEntry'}}}]}
;
% del_filters(This, Db, Id)
function_info('del_filters', params_type) ->
  {struct, [{1, i32},
          {2, {list, i32}}]}
;
function_info('del_filters', reply_type) ->
  {struct, []};
function_info('del_filters', exceptions) ->
  {struct, [{1, {struct, {'api_types', 'noEntry'}}}]}
;
% get_session_mgrs(This, WhichDb)
function_info('get_session_mgrs', params_type) ->
  {struct, [{1, i32}]}
;
function_info('get_session_mgrs', reply_type) ->
  {set, string};
function_info('get_session_mgrs', exceptions) ->
  {struct, []}
;
% add_session_mgrs(This, SessionMgrs)
function_info('add_session_mgrs', params_type) ->
  {struct, [{1, {set, string}}]}
;
function_info('add_session_mgrs', reply_type) ->
  i32;
function_info('add_session_mgrs', exceptions) ->
  {struct, [{1, {struct, {'api_types', 'invalidInput'}}},
          {2, {struct, {'api_types', 'duplicateEntry'}}}]}
;
% del_session_mgrs(This, SessionMgrs)
function_info('del_session_mgrs', params_type) ->
  {struct, [{1, {set, string}}]}
;
function_info('del_session_mgrs', reply_type) ->
  i32;
function_info('del_session_mgrs', exceptions) ->
  {struct, [{1, {struct, {'api_types', 'noEntry'}}}]}
;
% restore_default_session_mgrs(This)
function_info('restore_default_session_mgrs', params_type) ->
  {struct, []}
;
function_info('restore_default_session_mgrs', reply_type) ->
  oneway_void;
function_info('restore_default_session_mgrs', exceptions) ->
  {struct, []}
;
% get_monitoring_scope(This, WhichDb)
function_info('get_monitoring_scope', params_type) ->
  {struct, [{1, i32}]}
;
function_info('get_monitoring_scope', reply_type) ->
  {set, {struct, {'api_types', 'network'}}};
function_info('get_monitoring_scope', exceptions) ->
  {struct, []}
;
% add_monitoring_scope(This, Scope)
function_info('add_monitoring_scope', params_type) ->
  {struct, [{1, {set, {struct, {'api_types', 'network'}}}}]}
;
function_info('add_monitoring_scope', reply_type) ->
  i32;
function_info('add_monitoring_scope', exceptions) ->
  {struct, [{1, {struct, {'api_types', 'invalidInput'}}},
          {2, {struct, {'api_types', 'duplicateEntry'}}}]}
;
% del_monitoring_scope(This, Scope)
function_info('del_monitoring_scope', params_type) ->
  {struct, [{1, {set, {struct, {'api_types', 'network'}}}}]}
;
function_info('del_monitoring_scope', reply_type) ->
  i32;
function_info('del_monitoring_scope', exceptions) ->
  {struct, [{1, {struct, {'api_types', 'noEntry'}}}]}
;
% restore_default_monitoring_scope(This)
function_info('restore_default_monitoring_scope', params_type) ->
  {struct, []}
;
function_info('restore_default_monitoring_scope', reply_type) ->
  oneway_void;
function_info('restore_default_monitoring_scope', exceptions) ->
  {struct, []}
;
function_info(_Func, _Info) -> no_function.

