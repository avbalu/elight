%%
%% Autogenerated by Thrift Compiler (0.9.1)
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(systemAlerts_thrift).
-behaviour(thrift_service).


-include("systemAlerts_thrift.hrl").

-export([struct_info/1, function_info/2]).

struct_info('i am a dummy struct') -> undefined.
%%% interface
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
function_info(_Func, _Info) -> no_function.

