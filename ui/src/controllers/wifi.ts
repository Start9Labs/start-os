type PasswordState = {
    vlanid: VlanId | null,
    password: string,
    name: string,
};

type RichPasswordState = PasswordState & {
    permissions: string | null,
};

type WifiState = {
    enabled: boolean,
    ssid: string,
    broadcast: boolean,
    passwords: PasswordState[],
};

type RichWifiState = WifiState & {
    working: boolean,
    passwords: RichPasswordState[],
};

async function get_wifi(): Promise<RichWifiState> {
    throw Error('not implemented');
}

async function set_wifi(state: WifiState) {
    throw Error('not implemented');
}
