-module(erl_ip4_utils_tests).
-include_lib("eunit/include/eunit.hrl").

decimal_to_ip_test() ->
    Decimal = 1474122079,
    Result = {87,221,81,95},
    ?assertEqual(Result, erl_ip4_utils:decimal_to_ip(Decimal)).

decimal_to_ip2_test() ->
    Decimal = 0,
    Result = {0,0,0,0},
    ?assertEqual(Result, erl_ip4_utils:decimal_to_ip(Decimal)).

decimal_to_ip3_test() ->
    Decimal = 2,
    Result = {0,0,0,0},
    ?assertNotEqual(Result, erl_ip4_utils:decimal_to_ip(Decimal)).

ip_to_decimal_test() ->
    IP = {87,221,81,95},
    Result = 1474122079,
    ?assertEqual(Result, erl_ip4_utils:ip_to_decimal(IP)).

ip_to_decimal2_test() ->
    IP = {0,0,0,0},
    Result = 0,
    ?assertEqual(Result, erl_ip4_utils:ip_to_decimal(IP)).

ip_to_decimal3_test() ->
    IP = {87,221,81,95},
    Result = 1474145679,
    ?assertNotEqual(Result, erl_ip4_utils:ip_to_decimal(IP)).

network_to_ip_list_test() ->
    Network = "192.168.0.0",
    Bits = 24,
    [Head|_Tail] = erl_ip4_utils:network_to_ip_list(Network, Bits),
    FirstElement = {192,168,0,0},
    ?assertEqual(FirstElement, Head).

network_to_ip_list2_test() ->
    Network = "192.168.0.0",
    Bits = 24,
    IPList = erl_ip4_utils:network_to_ip_list(Network, Bits),
    LastElement = {192,168,0,255},
    ?assertEqual(LastElement, lists:last(IPList)).

network_to_ip_list3_test() ->
    Network = "192.168.0.0",
    Bits = 24,
    IPList = erl_ip4_utils:network_to_ip_list(Network, Bits),
    ?assertEqual(256, length(IPList)).

bits_to_number_of_addresses_test() ->
    Bits = 24,
    WhatIGet = erl_ip4_utils:bits_to_number_of_addresses(Bits),
    Result = 256,
    ?assertEqual(Result, WhatIGet).

bits_to_number_of_addresses2_test() ->
    Bits = 16,
    WhatIGet = erl_ip4_utils:bits_to_number_of_addresses(Bits),
    Result = 65536,
    ?assertEqual(Result, WhatIGet).

bits_to_number_of_addresses3_test() ->
    Bits = 8,
    WhatIGet = erl_ip4_utils:bits_to_number_of_addresses(Bits),
    Result = 16777216,
    ?assertEqual(Result, WhatIGet).

network_to_decimal_range_test() ->
    Network = {192,168,0,0},
    Bits = 24,
    Range = erl_ip4_utils:network_to_decimal_range(Network, Bits),
    Result = {3232235520, 3232235775},
    ?assertEqual(Result, Range).


decimal_range_to_ip_list_test() ->
    Range = {3232235520, 3232235775},
    IPList = erl_ip4_utils:decimal_range_to_ip_list(Range),
    Result = {192,168,0,150},
    ?assertEqual(Result, lists:nth(151, IPList)).
