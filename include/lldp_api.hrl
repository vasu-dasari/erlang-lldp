%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 9:57 AM
%%%-------------------------------------------------------------------
-author("vdasari").

-define(LldpMac, "01:80:c2:00:00:0e").
-define(LldpEtherType, 16#88CC).

-record(lldp_entity_t, {
    src_mac,
    chassis_id = "",
    port_id = "",
    ttl = 120,
    sys_name = "",
    sys_descr = "",
    mgmt_ip = "",
    other_tlvs = [],
    if_index,
    if_state
}).