-module('erl_ip4_utils').

-export([
    decimal_to_ip/1,
    ip_to_decimal/1,
    network_to_ip_list/2,
    bits_to_number_of_addresses/1,
    network_to_decimal_range/2,
    decimal_range_to_ip_list/1,
    networks_to_ip_addresses/2,
    ip_to_network_address/2,
    network_bit_to_netmask/1
]).

decimal_to_ip(Decimal) when is_integer(Decimal) ->
    <<
        A:8/big-unsigned-integer-unit:1,
        B:8/big-unsigned-integer-unit:1,
        C:8/big-unsigned-integer-unit:1,
        D:8/big-unsigned-integer-unit:1,
        _Rest/binary
    >> = <<Decimal:32>>,
    {A, B, C, D}.

ip_to_decimal(Address) when is_list(Address) ->
    {ok, Addr} = inet:parse_ipv4_address(Address),
    ip_to_decimal(Addr);
ip_to_decimal({A, B, C, D}) ->
    <<Decimal:32>> = <<A, B, C, D>>,
    Decimal.

network_to_ip_list(Network, Bits) when is_list(Network) andalso is_integer(Bits) ->
    {ok, Addr} = inet:parse_ipv4_address(Network),
    network_to_ip_list(Addr, Bits);
network_to_ip_list(IP = {_,_,_,_}, Bits) when is_integer(Bits) ->
    Range = network_to_decimal_range(IP, Bits),
    decimal_range_to_ip_list(Range).

bits_to_number_of_addresses(Bits) when is_integer(Bits) ->
    round(math:pow(2, 32-Bits)).

network_to_decimal_range(IP = {_,_,_,_}, Bits) when is_integer(Bits) ->
    NetworkStartIP = ip_to_network_address(IP, Bits),
    From = ip_to_decimal(NetworkStartIP),
    To = bits_to_number_of_addresses(Bits) + From - 1,
    {From, To}.

decimal_range_to_ip_list({From, To}) when is_integer(From) andalso is_integer(To)->
    [decimal_to_ip(Decimal) || Decimal <- lists:seq(From, To)].

networks_to_ip_addresses([], State) ->
    State;
networks_to_ip_addresses([{Network, Bits}|Tail], State) ->
    NewState = State ++ network_to_ip_list(Network, Bits),
    networks_to_ip_addresses(Tail, NewState).

ip_to_network_address(_IP = {A,B,C,D}, Bits) when is_integer(Bits) ->
    {MA, MB, MC, MD} = network_bit_to_netmask(Bits),
    {A band MA, B band MB, C band MC, D band MD}.

network_bit_to_netmask(Bits) when is_integer(Bits) ->
    decimal_to_ip(round(math:pow(2,32) - math:pow(2,32-Bits))).
