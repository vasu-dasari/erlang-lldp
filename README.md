
# erlang-lldp
## About
This package implements LLDP protocol. It can be included as a library or as a standalone application. It exposes a behavior `gen_lldp` that can be included in projects where user can provide custom interfaces to their operating system or their use case. For example, this can be used in Erlang based OpenFlow Controller(originally from FlowForwarding/loom) [loom](https://github.com/vasu-dasari/loom) to discover topology.
## How to use
    
### Standalone App
Development and test environment is captured in [docker-compose.yml](https://github.com/vasu-dasari/erlang-lldp/blob/master/docker/docker-compose.yml)

|Name|What is this  |
|--|--|
|lldp|Docker for compiling running `erlang-lldp` code  |
|lldp1|Linux Docker container connected to `lldp`|
|lldp2|Linux Docker container connected to `lldp`|
    $ make up    # Bring up development and test environment
    $ make.      # Compile the code
    $ make run   # Run the application
![Setup Diagram](https://github.com/vasu-dasari/erlang-lldp/blob/master/docs/Setup.jpeg)

Now from Erlang shell one can do the following to display local interface information and discovered neighbors information

    (lldp@lldp)10> lldp_manager:dashboard(lldp_netlink).
    -------- ------------------ ------------ ------------ ------------------ ---- ---------- ----------
      IfName         Chassis Id      Port Id  System Name       Mgmt Address Nbrs    Rx Pkts    Tx Pkts
    -------- ------------------ ------------ ------------ ------------------ ---- ---------- ----------
        eth2  02:42:0A:00:7B:0A         eth2         lldp         10.0.101.1    1         11         15
        eth1  02:42:0A:00:7B:0A         eth1         lldp         10.0.100.1    1         11         15
    ------------------ ------------------ --- ------------ ------------------ ------------------
            Chassis Id            Port Id Ttl  System Name          Mgmt Info         Expires At
    ------------------ ------------------ --- ------------ ------------------ ------------------
     02:42:0A:00:7B:1E  02:42:0A:00:65:02 120        lldp2        10.0.123.30   4/23/2019 6:08pm
     02:42:0A:00:7B:14  02:42:0A:00:64:02 120        lldp1        10.0.123.20   4/23/2019 6:08pm

### As a library
Include the library as a rebar [lldp from hex](https://hex.pm/packages/lldp) dependency as follows:
```erlang  
{deps, [
    lldp
 ]}.  
```
![Software Model](https://github.com/vasu-dasari/erlang-lldp/blob/master/docs/Software%20Model.jpeg)
`erlang-lldp` can be adapted to be used on a custom platform as well. Platform specific information can be captured in an Erlang module by following `gen_lldp` behavior. It is expected to support the following callback functions by the custom module.

|Callback|Description|
|--|--|
|init|Initialization function which can be used to relay platform specific state variable|
|handle_message|Typical message like `tx_packet` to transmit packet, or `started` to indicate, module is to be initialized and hence populate interface information, etc|
|notify|Notifications like, neighbor has been added/deleted/updated|
|info|Platform specific information that needs to be plumbed through `lldp_mamager:info` calls|
|terminate|LLDP module is being shutdown|

### Changelog