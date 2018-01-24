%%%-------------------------------------------------------------------
%%% @author vdasari
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2018 9:56 AM
%%%-------------------------------------------------------------------
-module(lldp_pdu).
-author("vdasari").

-include("logger.hrl").
-include("lldp_api.hrl").
-include_lib("pkt/include/pkt.hrl").

-define(LldpMac, "01:80:c2:00:00:0e").
-define(LldpEtherType, 16#88CC).

%% API
-export([encode/1, encode_mgmt_tlv/2, decode_mgmt_tlv/1, decode/1]).

encode(
        #lldp_entity_t{
            src_mac = SrcMac,
            chassis_id = ChassisId,
            port_id = PortId,
            ttl = Ttl,
            sys_name = SysName,
            sys_descr = SysDescr,
            mgmt_ip = MgmtIp,
            if_index = IfIndex
        }) ->
    pkt:encode([
        #ether{
            shost = inet_utils:convert_mac(to_binary,SrcMac),
            dhost = inet_utils:convert_mac(to_binary,?LldpMac),
            type = ?LldpEtherType
        },
        #lldp{
            pdus = [
                #chassis_id{
                    subtype = mac_address,
                    value = inet_utils:convert_mac(to_binary, ChassisId)
                },
                #port_id{
                    subtype = interface_name,
                    value = PortId
                },
                #ttl{
                    value = Ttl
                },
                #system_name{
                    value = SysName
                },
                #system_desc{
                    value = SysDescr
                },
                #management_address{
                    value = encode_mgmt_tlv(inet_utils:convert_ip(to_binary, MgmtIp),
                        IfIndex)
                },
                #system_capability{
                    enabled = [router,bridge],
                    system = [router,bridge]
                },
                end_of_lldpdu
            ]
        }
    ]).

decode(LldpPduBin) ->
    {ok,{[#ether{shost = SrcMac}, #lldp{pdus = Pdus}], _}} = pkt:decode(LldpPduBin),

    lists:foldl(fun
        (#chassis_id{subtype = SubType, value = Value}, Acc) when
            SubType == mac_address; SubType == chassis_component ->
            Acc#lldp_entity_t{chassis_id = inet_utils:convert_mac(to_string, Value)};
        (#chassis_id{subtype = network_address, value = Value}, Acc) ->
            Acc#lldp_entity_t{chassis_id = inet_utils:convert_ip(to_string, Value)};
        (#chassis_id{value = Value}, Acc) ->
            Acc#lldp_entity_t{chassis_id = binary_to_list(Value)};

        (#port_id{subtype = mac_address, value = Value}, Acc) ->
            Acc#lldp_entity_t{port_id = inet_utils:convert_mac(to_string, Value)};
        (#port_id{subtype = network_address, value = Value}, Acc) ->
            Acc#lldp_entity_t{port_id = inet_utils:convert_ip(to_string, Value)};
        (#port_id{value = Value}, Acc) ->
            Acc#lldp_entity_t{port_id = binary_to_list(Value)};

        (#ttl{value = Value}, Acc) ->
            Acc#lldp_entity_t{ttl = Value};

        (#system_name{value = Value}, Acc) ->
            Acc#lldp_entity_t{sys_name = binary_to_list(Value)};

        (#system_desc{value = Value}, Acc) ->
            Acc#lldp_entity_t{sys_descr = binary_to_list(Value)};

        (#management_address{value = Value}, Acc) ->
            {MgmtIp, IfIndex} = decode_mgmt_tlv(Value),
            Acc#lldp_entity_t{
                if_index = IfIndex,
                mgmt_ip = MgmtIp
            };
        (_, Acc) ->
            Acc
    end, #lldp_entity_t{
        src_mac = inet_utils:convert_mac(to_string, SrcMac)
    }, Pdus).

%% AddrLength:?BYTE, AddrSubType:?BYTE, Address/bytes,
%% IfSubType:?BYTE, IfIndex:?LONG, OidStrLen:?BYTE, OidStr:OidStrLen/bytes
encode_mgmt_tlv(AddrBin, IfIndex) when size(AddrBin) == 4 ->
    <<5, 1, AddrBin/binary, 2, IfIndex:32/unsigned-big-integer, 0, <<>>/binary>>;
encode_mgmt_tlv(AddrBin, IfIndex) when size(AddrBin) == 6 ->
    <<7, 6, AddrBin/binary, 2, IfIndex:32/unsigned-big-integer, 0, <<>>/binary>>;
encode_mgmt_tlv(_,_) ->
    <<>>.

decode_mgmt_tlv(<<5, 1, AddrBin:4/bytes, 2, IfIndex:32/unsigned-big-integer, 0>>) ->
    {inet_utils:convert_ip(to_string, AddrBin), IfIndex};
decode_mgmt_tlv(<<7, 6, AddrBin:6/bytes, 2, IfIndex:32/unsigned-big-integer, 0>>) ->
    {inet_utils:convert_mac(to_string, AddrBin), IfIndex};
decode_mgmt_tlv(_) ->
    {"", 0}.
