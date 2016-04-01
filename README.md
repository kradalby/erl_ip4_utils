# erl_ip4_utils

An OTP library

## Build

    $ rebar3 compile


## Test

    $ rebar3 eunit

## Usage

Get a range of IP addresses based on a subnet:

    1> erl_ip4_utils:network_to_ip_list("192.168.0.0", 28).
    OR
    1> erl_ip4_utils:network_to_ip_list({192,168,0,0}, 28).
        [{192,168,0,0},
         {192,168,0,1},
         {192,168,0,2},
         {192,168,0,3},
         {192,168,0,4},
         {192,168,0,5},
         {192,168,0,6},
         {192,168,0,7},
         {192,168,0,8},
         {192,168,0,9},
         {192,168,0,10},
         {192,168,0,11},
         {192,168,0,12},
         {192,168,0,13},
         {192,168,0,14},
         {192,168,0,15}]

Convert an IP address to decimal:

    1> erl_ip4_utils:ip_to_decimal("8.8.8.8").
    OR
    1> erl_ip4_utils:ip_to_decimal({8,8,8,8}).
        134744072

Convert a decimal to an IP address:

    1> erl_ip4_utils:decimal_to_ip(134744072).
        {8,8,8,8}

## Changelog

### 0.0.3
- adds network_bit_to_netmask(Bits) for 24 -> 255.255.255.0 conversions.
- adds ip_to_network_address(IP, Bits) for 192.168.1.14/23 -> 192.168.0.0 conversions.
- fixes issue #1, where network_to_decimal_range would not give a valid range if not given the network address of the network.

### 0.0.2
- adds networks_to_ip_addresses([{Network, Bits}], []) returns a big list of ips given N networks

### 0.0.1
- Initial release
