%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc filer.hrl
%%% contains the  definitions needed to use filer's services
%%% @end
%%% Created : 22 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------

%% @doc pcap_save message
%% This message will be used by applications that want to
%% save a file 
%% 

%% if there exists a file with the same name already, 
%% the file will be over-written
%%
%% The files will be saved in the cabinet directory
%% ../cabinet/

-record(file, { 
	  name :: string(), 
	  data :: iodata()
	 }).
