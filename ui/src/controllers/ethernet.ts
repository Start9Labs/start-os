type PortState = {
    vlanid: VlanId | null;
};

type RichPortState = PortState & {
    permissions: string | null,
};

type EthernetState = {
    wan_port: string | null;
    ports: Map<string, PortState>,
};

type RichEthernetState = EthernetState & {
    ports: Map<string, RichPortState>,
};

async function get_ethernet(): Promise<RichEthernetState> {
    throw Error('not implemented');
}

async function set_ethernet(state: EthernetState) {
    throw Error('not implemented');
}
