//! PCP PORT_SET option (RFC 7753 — "PCP Extension for Port-Set Allocation").
//! Lets a single MAP request map a contiguous block of external ports. The
//! option code is in the optional-to-process range, so a PCP server that does
//! not implement it silently treats the request as a single-port MAP — the
//! StartOS client detects the missing echoed option and skips range forwarding
//! on such gateways.
//!
//! Shared by the StartTunnel PCP server (parses requests, echoes responses) and
//! the StartOS client (emits via the crab_nat `PcpOption` carrier and reads the
//! echo back off the response).

/// PORT_SET option code (RFC 7753 §3, optional-to-process range).
pub const OPTION_PORT_SET: u8 = 130;

/// A parsed PORT_SET option (RFC 7753 §3).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PortSet {
    /// Number of contiguous ports (MUST NOT be zero per the RFC).
    pub size: u16,
    /// First internal port; in a request MUST equal the MAP opcode internal port.
    pub first_internal_port: u16,
    /// Request that the external port set preserve the parity of the internal port.
    pub parity: bool,
}

impl PortSet {
    /// The 5-byte PORT_SET payload: size (BE u16), first internal port (BE u16),
    /// then a byte whose low bit is the parity flag (upper 7 bits reserved).
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

/// Append a fully-framed PORT_SET option (RFC 6887 §7.3 framing: code,
/// reserved, length, payload, zero-padded to 32 bits). Used by the server to
/// echo the granted set in a MAP response.
pub fn encode_port_set_option(buf: &mut Vec<u8>, ps: &PortSet) {
    let data = ps.to_payload();
    buf.push(OPTION_PORT_SET);
    buf.push(0); // reserved
    buf.extend_from_slice(&(data.len() as u16).to_be_bytes());
    buf.extend_from_slice(&data);
    while !buf.len().is_multiple_of(4) {
        buf.push(0);
    }
}

/// Parse the first PORT_SET option from the framed option area that follows a
/// PCP opcode's fixed payload (server request parsing). `Err` on a truncated
/// or malformed PORT_SET option so the caller can reply MALFORMED_OPTION.
pub fn parse_port_set_options(mut tail: &[u8]) -> Result<Option<PortSet>, ()> {
    while tail.len() >= 4 {
        let code = tail[0];
        let len = u16::from_be_bytes([tail[2], tail[3]]) as usize;
        if 4 + len > tail.len() {
            return Err(());
        }
        if code == OPTION_PORT_SET {
            return PortSet::from_payload(&tail[4..4 + len]).ok_or(()).map(Some);
        }
        let end = (4 + len + 3) & !3;
        tail = &tail[end.min(tail.len())..];
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
