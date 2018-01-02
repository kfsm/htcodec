%% @doc
%%   Codec (encoder/decoder) for HTTP Content-Types
%%    * application/json
%%    * application/x-www-form-urlencoded
%%    * text/plain
-module(htcodec).
-compile({parse_transform, category}).


-export([
   encode/2,
   decode/2,
   decode/1,
   to_json/1, 
   as_json/1,
   to_www_form/1, 
   as_www_form/1,
   to_text/1,
   as_text/1
]).

%%
%% data types
-type content_type() :: binary().
-type content()      :: binary().


%%
%% encode content types
-spec encode(content_type(), _) -> datum:either(content()).

encode(<<"application/json">>, Json) ->
   to_json(Json);
encode(<<"application/x-www-form-urlencoded">>, Form) ->
   to_www_form(Form);
encode(<<"text/plain">>, Text) ->
   to_text(Text);
encode(Type, _) ->
   {error, {unsupported, Type}}.


%%
%% decode content type
-spec decode(content_type(), content()) -> datum:either(_).

decode(<<"application/json">>, Json) ->
   as_json(Json);
decode(<<"application/x-www-form-urlencoded">>, Form) ->
   as_www_form(Form);
decode(<<"text/plain">>, Text) ->
   as_text(Text);
decode(Type, _) ->
   {error, {unsupported, Type}}.

%%
%% decode content type
-spec decode(_) -> datum:either(_).

decode([{_Code, _Text, Head} | Payload]) ->
   decode(lens:get(lens:pair(<<"Content-Type">>), Head), scalar:s(Payload)).



%%
%%
-spec to_json(_) -> datum:either(content()).

to_json(Json) ->
   try
      {ok, 
         jsx:encode(Json)
      }
   catch _:_ ->
      {error, badarg}
   end.

-spec as_json(content()) -> datum:either(_).

as_json(Json) ->
   try
      {ok, 
         jsx:decode(erlang:iolist_to_binary(Json), [return_maps])
      }
   catch _:_ ->
      {error, badarg}
   end.

%%
%%
-spec to_www_form(_) -> datum:either(content()).

to_www_form(Form) ->
   try
      {ok, [identity ||
         maps:to_list(Form),
         lists:map(fun to_pair/1, _),
         scalar:s(lists:join(<<$&>>, _))
      ]}
   catch E:R ->
      {error, badarg}
   end.

to_pair(Pair) ->
   scalar:s(
      lists:join(<<$=>>, 
         [uri:escape(X) || X <- erlang:tuple_to_list(Pair)]
      )
   ).

-spec as_www_form(content()) -> datum:either(_).

as_www_form(Form) ->
   try
      {ok, [identity ||
         binary:split(scalar:s(Form), <<$&>>, [trim, global]),
         lists:map(fun as_pair/1, _),
         maps:from_list(_)
      ]}
   catch _:_ ->
      {error, badarg}
   end.

as_pair(Pair) ->
   erlang:list_to_tuple(
      [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
   ).

%%
%%
-spec to_text(_) -> datum:either(content()).

to_text(Text) ->
   try
      {ok, scalar:s(Text)}
   catch _:_ ->
      {error, badarg}
   end.


-spec as_text(content()) -> datum:either(_).

as_text(Text) ->
   {ok, Text}.
