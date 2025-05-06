type LanAccess = "ALL" | "SAME_PROFILE" | string[];
type WanAccess = "ALL" | "NONE";

type VlanId = number;

type ProfileState = {
    vlanid: VlanId,
    name: string,
    gateway_digit: number,
    subnet_digit: number,
    lan_access: LanAccess,
    wan_access: WanAccess,
    block_new_profiles: boolean,
};

type RichProfileState = ProfileState & {
    gateway_ip: string,
    subnet_ip: string,
};

async function available_vlanid(): Promise<VlanId> {
    throw Error('not implemented');
}

async function create_profile(state: ProfileState) {
    throw Error('not implemented');
}

async function get_profile(id: VlanId): Promise<RichProfileState> {
    throw Error('not implemented');
}

async function update_profile(state: ProfileState): Promise<ProfileState> {
    throw Error('not implemented');
}

async function delete_profile(id: VlanId) {
    throw Error('not implemented');
}
