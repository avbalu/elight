%%
%% Autogenerated by Thrift Compiler (0.9.1)
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(alerts_thrift).
-behaviour(thrift_service).


-include("alerts_thrift.hrl").

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
% filter_triggered(This, Serverity, Alert)
function_info('filter_triggered', params_type) ->
  {struct, [{1, i32},
          {2, {struct, {'api_types', 'filterAlert'}}}]}
;
function_info('filter_triggered', reply_type) ->
  oneway_void;
function_info('filter_triggered', exceptions) ->
  {struct, []}
;
% session_mgr_down(This, Severity, SessionMgr)
function_info('session_mgr_down', params_type) ->
  {struct, [{1, i32},
          {2, string}]}
;
function_info('session_mgr_down', reply_type) ->
  oneway_void;
function_info('session_mgr_down', exceptions) ->
  {struct, []}
;
% session_mgr_up(This, Severity, SessionMgr)
function_info('session_mgr_up', params_type) ->
  {struct, [{1, i32},
          {2, string}]}
;
function_info('session_mgr_up', reply_type) ->
  oneway_void;
function_info('session_mgr_up', exceptions) ->
  {struct, []}
;
% new_host(This, Serevrity, Host)
function_info('new_host', params_type) ->
  {struct, [{1, i32},
          {2, string}]}
;
function_info('new_host', reply_type) ->
  oneway_void;
function_info('new_host', exceptions) ->
  {struct, []}
;
% new_service(This, Serevrity, Server, _service)
function_info('new_service', params_type) ->
  {struct, [{1, i32},
          {2, string},
          {3, {struct, {'api_types', 'serviceId'}}}]}
;
function_info('new_service', reply_type) ->
  oneway_void;
function_info('new_service', exceptions) ->
  {struct, []}
;
% no_response(This, Severity, Server, _service)
function_info('no_response', params_type) ->
  {struct, [{1, i32},
          {2, string},
          {3, {struct, {'api_types', 'serviceId'}}}]}
;
function_info('no_response', reply_type) ->
  oneway_void;
function_info('no_response', exceptions) ->
  {struct, []}
;
% service_inactive(This, Severity, Server, _service)
function_info('service_inactive', params_type) ->
  {struct, [{1, i32},
          {2, string},
          {3, {struct, {'api_types', 'serviceId'}}}]}
;
function_info('service_inactive', reply_type) ->
  oneway_void;
function_info('service_inactive', exceptions) ->
  {struct, []}
;
% host_inactive(This, Serevrity, Host)
function_info('host_inactive', params_type) ->
  {struct, [{1, i32},
          {2, string}]}
;
function_info('host_inactive', reply_type) ->
  oneway_void;
function_info('host_inactive', exceptions) ->
  {struct, []}
;
function_info(_Func, _Info) -> no_function.

