-module(htcodec_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([
   encode_json/1,
   decode_json/1,
   decode_json_with_charset/1,
   encode_text/1,
   decode_text/1,
   decode_text_with_charset/1,
   encode_www_form/1,
   decode_www_form/1,
   decode_www_form_with_charset/1,
   decode_http_stream/1
]).

all() ->
   [
      encode_json,
      decode_json,
      decode_json_with_charset,
      encode_text,
      decode_text,
      decode_text_with_charset,
      encode_www_form,
      decode_www_form,
      decode_www_form_with_charset,
      decode_http_stream
   ].

%%
encode_json(_) ->
   {ok, <<"{\"a\":1,\"b\":\"text\"}">>} = htcodec:encode(<<"application/json">>, #{a => 1, b => <<"text">>}).

decode_json(_) ->
   {ok, #{<<"a">> := 1, <<"b">> := <<"text">>}} = htcodec:decode(<<"application/json">>, <<"{\"a\":1,\"b\":\"text\"}">>).

decode_json_with_charset(_) ->
   {ok, #{<<"a">> := 1, <<"b">> := <<"text">>}} = htcodec:decode(<<"application/json; charset=utf-8">>, <<"{\"a\":1,\"b\":\"text\"}">>).

%%
encode_text(_) ->
   {ok, <<"a">>} = htcodec:encode(<<"text/plain">>, a).

decode_text(_) ->
   {ok, <<"a">>} = htcodec:decode(<<"text/plain">>, <<"a">>).

decode_text_with_charset(_) ->
   {ok, <<"a">>} = htcodec:decode(<<"text/plain; charset=utf-8">>, <<"a">>).


%%
encode_www_form(_) ->
   {ok, <<"a=1&b=text">>} = htcodec:encode(<<"application/x-www-form-urlencoded">>, #{a => 1, b => <<"text">>}).

decode_www_form(_) ->
   {ok, #{<<"a">> := <<"1">>, <<"b">> := <<"text">>}} = htcodec:decode(<<"application/x-www-form-urlencoded">>, <<"a=1&b=text">>).

decode_www_form_with_charset(_) ->
   {ok, #{<<"a">> := <<"1">>, <<"b">> := <<"text">>}} = htcodec:decode(<<"application/x-www-form-urlencoded; charset=utf-8">>, <<"a=1&b=text">>).

%%
decode_http_stream(_) ->
   {ok, #{<<"a">> := 1, <<"b">> := <<"text">>}} = htcodec:decode([{200, <<"OK">>, [{<<"Content-Type">>, <<"application/json">>}]}, <<"{\"a\":1,\"b\":\"text\"}">>]).



