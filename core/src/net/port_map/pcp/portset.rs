//! PCP PORT_SET option (RFC 7753): maps a contiguous block of external ports in
//! one MAP request. The code is optional-to-process, so a server lacking it
//! silently treats the request as single-port — the StartOS client detects the
//! missing echoed option and skips range forwarding there.
//!
//! Shared by the StartTunnel PCP server (parses/echoes) and the StartOS client
//! (emits via crab_nat `PcpOption`, reads the echo off the response).

/// PORT_SET option code (RFC 7753 §3, optional-to-process range).
pub const OPTION_PORT_SET: u8 = 130;

/// A parsed PORT_SET option (RFC 7753 §3).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PortSet {
    /// Contiguous port count (MUST NOT be zero per the RFC).
    pub size: u16,
    /// In a request, MUST equal the MAP opcode internal port.
    pub first_internal_port: u16,
    /// Request that the external set preserve the internal port's parity.
    pub parity: bool,
}

impl PortSet {
    /// 5-byte payload: size (BE u16), first internal port (BE u16), then a byte
    /// whose low bit is parity (upper 7 reserved).
    pub fn to_payload(&self) -> Vec<u8> {
        let mut d = Vec::with_capacity(5);
        d.extend_from_slice(&self.size.to_be_bytes());
        d.extend_from_slice(&self.first_internal_port.to_be_bytes());
        d.push(u8::from(self.parity));
        d
    }

    /// Parse a 5-byte PORT_SET payload (the `data` of a PCP option).
    pub fn from_payload(data: &[u8]) -> Option<Self> {
        if data.len() < 5 {
            return None;
        }
        Some(Self {
            size: u16::from_be_bytes([data[0], data[1]]),
            first_internal_port: u16::from_be_bytes([data[2], data[3]]),
            parity: data[4] & 1 != 0,
        })
    }
}

/// Append a framed PORT_SET option (RFC 6887 §7.3, zero-padded to 32 bits).
/// Used by the server to echo the granted set in a MAP response.
pub fn encode_port_set_option(buf: &mut Vec<u8>, ps: &PortSet) {
    super::encode_pcp_option(buf, OPTION_PORT_SET, &ps.to_payload());
}

/// First PORT_SET option in a PCP opcode's option area. `Err` on a truncated or
/// malformed option so the caller can reply MALFORMED_OPTION.
pub fn parse_port_set_options(tail: &[u8]) -> Result<Option<PortSet>, ()> {
    for opt in super::pcp_options(tail) {
        let (code, value) = opt?;
        if code == OPTION_PORT_SET {
            return PortSet::from_payload(value).ok_or(()).map(Some);
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn payload_round_trip() {
        let ps = PortSet {
            size: 64,
            first_internal_port: 8080,
            parity: true,
        };
        let payload = ps.to_payload();
        assert_eq!(payload.len(), 5);
        assert_eq!(PortSet::from_payload(&payload), Some(ps));
    }

    #[test]
    fn parity_is_low_bit_only() {
        assert!(!PortSet::from_payload(&[0, 1, 0, 0, 0]).unwrap().parity);
        assert!(PortSet::from_payload(&[0, 1, 0, 0, 1]).unwrap().parity);
        // Upper bits reserved/ignored.
        assert!(!PortSet::from_payload(&[0, 1, 0, 0, 0xfe]).unwrap().parity);
    }

    #[test]
    fn framed_option_round_trip() {
        let ps = PortSet {
            size: 10,
            first_internal_port: 443,
            parity: false,
        };
        let mut buf = Vec::new();
        encode_port_set_option(&mut buf, &ps);
        // 4 header + 5 payload = 9 -> padded to 12.
        assert_eq!(buf.len(), 12);
        assert!(buf.len().is_multiple_of(4));
        assert_eq!(buf[0], OPTION_PORT_SET);
        assert_eq!(u16::from_be_bytes([buf[2], buf[3]]), 5);
        assert_eq!(parse_port_set_options(&buf).unwrap(), Some(ps));
    }

    #[test]
    fn skips_other_options_and_handles_absent() {
        // An unknown 4-byte option, then a PORT_SET.
        let mut buf = vec![224u8, 0, 0, 0];
        encode_port_set_option(
            &mut buf,
            &PortSet {
                size: 3,
                first_internal_port: 1000,
                parity: false,
            },
        );
        assert_eq!(parse_port_set_options(&buf).unwrap().unwrap().size, 3);
        assert_eq!(parse_port_set_options(&[]).unwrap(), None);
        assert_eq!(parse_port_set_options(&[224, 0, 0, 0]).unwrap(), None);
    }

    #[test]
    fn rejects_truncated() {
        // PORT_SET claiming 5 bytes but only 2 present.
        assert!(parse_port_set_options(&[OPTION_PORT_SET, 0, 0, 5, 0, 64]).is_err());
    }
}
