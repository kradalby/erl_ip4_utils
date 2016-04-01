-module(erl_ip4_utils_tests).
-include_lib("eunit/include/eunit.hrl").

decimal_to_ip_test() ->
    Decimal = 1474122079,
    Expected = {87,221,81,95},
    ?assertEqual(Expected, erl_ip4_utils:decimal_to_ip(Decimal)).

decimal_to_ip2_test() ->
    Decimal = 0,
    Expected = {0,0,0,0},
    ?assertEqual(Expected, erl_ip4_utils:decimal_to_ip(Decimal)).

decimal_to_ip3_test() ->
    Decimal = 2,
    Expected = {0,0,0,0},
    ?assertNotEqual(Expected, erl_ip4_utils:decimal_to_ip(Decimal)).

ip_to_decimal_test() ->
    IP = {87,221,81,95},
    Expected = 1474122079,
    ?assertEqual(Expected, erl_ip4_utils:ip_to_decimal(IP)).

ip_to_decimal2_test() ->
    IP = {0,0,0,0},
    Expected = 0,
    ?assertEqual(Expected, erl_ip4_utils:ip_to_decimal(IP)).

ip_to_decimal3_test() ->
    IP = {87,221,81,95},
    Expected = 1474145679,
    ?assertNotEqual(Expected, erl_ip4_utils:ip_to_decimal(IP)).

ip_to_decimal_string_test() ->
    IP = "87.221.81.95",
    Expected = 1474122079,
    ?assertEqual(Expected, erl_ip4_utils:ip_to_decimal(IP)).

ip_to_decimal_string2_test() ->
    IP = "8.8.8.8",
    Expected = 134744072,
    ?assertEqual(Expected, erl_ip4_utils:ip_to_decimal(IP)).

network_to_ip_list_test() ->
    Network = {192,168,0,0},
    Bits = 24,
    [Head|_Tail] = erl_ip4_utils:network_to_ip_list(Network, Bits),
    FirstElement = {192,168,0,0},
    ?assertEqual(FirstElement, Head).

network_to_ip_list2_test() ->
    Network = {192,168,0,0},
    Bits = 24,
    IPList = erl_ip4_utils:network_to_ip_list(Network, Bits),
    LastElement = {192,168,0,255},
    ?assertEqual(LastElement, lists:last(IPList)).

network_to_ip_list3_test() ->
    Network = {192,168,0,0},
    Bits = 24,
    IPList = erl_ip4_utils:network_to_ip_list(Network, Bits),
    ?assertEqual(256, length(IPList)).

network_to_ip_list_string_test() ->
    Network = "192.168.0.0",
    Bits = 24,
    [Head|_Tail] = erl_ip4_utils:network_to_ip_list(Network, Bits),
    FirstElement = {192,168,0,0},
    ?assertEqual(FirstElement, Head).

network_to_ip_list_string2_test() ->
    Network = "192.168.0.0",
    Bits = 24,
    IPList = erl_ip4_utils:network_to_ip_list(Network, Bits),
    LastElement = {192,168,0,255},
    ?assertEqual(LastElement, lists:last(IPList)).

network_to_ip_list_string3_test() ->
    Network = "192.168.0.0",
    Bits = 24,
    IPList = erl_ip4_utils:network_to_ip_list(Network, Bits),
    ?assertEqual(256, length(IPList)).

bits_to_number_of_addresses_test() ->
    Bits = 24,
    WhatIGet = erl_ip4_utils:bits_to_number_of_addresses(Bits),
    Expected = 256,
    ?assertEqual(Expected, WhatIGet).

bits_to_number_of_addresses2_test() ->
    Bits = 16,
    WhatIGet = erl_ip4_utils:bits_to_number_of_addresses(Bits),
    Expected = 65536,
    ?assertEqual(Expected, WhatIGet).

bits_to_number_of_addresses3_test() ->
    Bits = 8,
    WhatIGet = erl_ip4_utils:bits_to_number_of_addresses(Bits),
    Expected = 16777216,
    ?assertEqual(Expected, WhatIGet).

network_to_decimal_range_test() ->
    Network = {192,168,0,0},
    Bits = 24,
    Range = erl_ip4_utils:network_to_decimal_range(Network, Bits),
    Expected = {3232235520, 3232235775},
    ?assertEqual(Expected, Range).

network_to_decimal_range_given_not_start_ip_test() ->
    Network = {192,168,0,16},
    Bits = 24,
    Range = erl_ip4_utils:network_to_decimal_range(Network, Bits),
    Expected = {3232235520, 3232235775},
    ?assertEqual(Expected, Range).

network_to_decimal_range_given_not_start_ip2_test() ->
    Network = {172,16,34,0},
    Bits = 16,
    Range = erl_ip4_utils:network_to_decimal_range(Network, Bits),
    Expected = {2886729728, 2886795263},
    ?assertEqual(Expected, Range).

network_to_decimal_range_given_not_start_ip3_test() ->
    Network = {8,6,0,0},
    Bits = 8,
    Range = erl_ip4_utils:network_to_decimal_range(Network, Bits),
    Expected = {134217728, 150994943},
    ?assertEqual(Expected, Range).

decimal_range_to_ip_list_test() ->
    Range = {3232235520, 3232235775},
    IPList = erl_ip4_utils:decimal_range_to_ip_list(Range),
    Expected = {192,168,0,150},
    ?assertEqual(Expected, lists:nth(151, IPList)).

ip_to_network_address_test() ->
    IP = {192,168,1,34},
    Bits = 23,
    Expected = {192,168,0,0},
    Result = erl_ip4_utils:ip_to_network_address(IP, Bits),
    ?assertEqual(Expected, Result).

ip_to_network_address2_test() ->
    IP = {172,16,85,0},
    Bits = 16,
    Expected = {172,16,0,0},
    Result = erl_ip4_utils:ip_to_network_address(IP, Bits),
    ?assertEqual(Expected, Result).

ip_to_network_address3_test() ->
    IP = {8,7,6,5},
    Bits = 8,
    Expected = {8,0,0,0},
    Result = erl_ip4_utils:ip_to_network_address(IP, Bits),
    ?assertEqual(Expected, Result).

network_bit_to_netmask_test() ->
    Bits = 24,
    Expected = {255,255,255,0},
    Result = erl_ip4_utils:network_bit_to_netmask(Bits),
    ?assertEqual(Expected, Result).

network_bit_to_netmask2_test() ->
    Bits = 12,
    Expected = {255,240,0,0},
    Result = erl_ip4_utils:network_bit_to_netmask(Bits),
    ?assertEqual(Expected, Result).

network_bit_to_netmask3_test() ->
    Bits = 8,
    Expected = {255,0,0,0},
    Result = erl_ip4_utils:network_bit_to_netmask(Bits),
    ?assertEqual(Expected, Result).

network_bit_to_netmask4_test() ->
    Bits = 32,
    Expected = {255,255,255,255},
    Result = erl_ip4_utils:network_bit_to_netmask(Bits),
    ?assertEqual(Expected, Result).
