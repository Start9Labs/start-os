//! PCP `HOSTNAME` option (RFC 6887 extension — "PCP Hostname Extension for
//! SNI-Demultiplexed Port Mappings"). Lets an internal host associate FQDNs
//! with a MAP request so a gateway can demultiplex inbound TLS on a shared
//! external port (e.g. 443) by the SNI in the ClientHello.
//!
//! The option code and the two new result codes are not yet IANA-assigned, so
//! we use values from the PCP **Private Use** ranges (Options 224-254, Result
//! Codes 192-254 per the IANA PCP Parameters registry). The StartTunnel PCP
//! server parses and echoes these options; the StartOS client emits them by
//! attaching a `crab_nat::pcp::PcpOption { code: OPTION_HOSTNAME, data }` to a
//! MAP request (see the `crab_nat` fork's custom-option support).

/// HOSTNAME option code (optional-to-process Private Use range).
pub const OPTION_HOSTNAME: u8 = 224;

/// Result code: the requested hostname is already bound on this external
/// address+port by another client (short-lifetime error). Private Use range.
pub const RESULT_HOSTNAME_TAKEN: u8 = 192;
/// Result code: the HOSTNAME option requests an unsupported feature (e.g. a
/// wildcard, or UDP without QUIC demux) — long-lifetime error. Private Use.
pub const RESULT_UNSUPP_HOSTNAME: u8 = 193;

/// A hostname is valid as an SNI demux key: 1-255 octets, ASCII, dot-separated
/// labels of `[A-Za-z0-9-]` (a leading `*` label allowed for wildcards), no
/// leading/trailing dot, no empty labels.
pub fn validate_hostname(name: &str) -> bool {
    if name.is_empty() || name.len() > 255 || name.starts_with('.') || name.ends_with('.') {
        return false;
    }
    name.split('.').enumerate().all(|(i, label)| {
        !label.is_empty()
            && label
                .bytes()
                .enumerate()
                .all(|(j, b)| b.is_ascii_alphanumeric() || b == b'-' || (b == b'*' && i == 0 && j == 0 && label.len() == 1))
    })
}

/// Append a HOSTNAME option to a PCP message buffer (RFC 6887 §7.3 option
/// format: code, reserved, length, data, zero-padded to a 32-bit boundary).
/// `buf` MUST already be 32-bit aligned (true after the fixed opcode payload).
pub fn encode_hostname_option(buf: &mut Vec<u8>, hostname: &str) {
    let data = hostname.as_bytes();
    buf.push(OPTION_HOSTNAME);
    buf.push(0); // reserved
    buf.extend_from_slice(&(data.len() as u16).to_be_bytes());
    buf.extend_from_slice(data);
    while buf.len() % 4 != 0 {
        buf.push(0);
    }
}

/// Parse the option area that follows a PCP opcode's fixed payload, returning
/// the (lowercased) hostnames carried by HOSTNAME options. Other optional
/// options are skipped. `Err` on a malformed option (truncated, or a HOSTNAME
/// whose value fails [`validate_hostname`]) so the caller can reply
/// MALFORMED_OPTION.
pub fn parse_hostname_options(mut tail: &[u8]) -> Result<Vec<String>, ()> {
    let mut names = Vec::new();
    while tail.len() >= 4 {
        let code = tail[0];
        let len = u16::from_be_bytes([tail[2], tail[3]]) as usize;
        let padded = (len + 3) & !3;
        let end = 4 + padded;
        if 4 + len > tail.len() {
            return Err(());
        }
        if code == OPTION_HOSTNAME {
            let value = std::str::from_utf8(&tail[4..4 + len]).map_err(|_| ())?;
            if !validate_hostname(value) {
                return Err(());
            }
            names.push(value.to_ascii_lowercase());
        }
        // A trailing option may omit final padding; clamp to the buffer.
        tail = &tail[end.min(tail.len())..];
    }
    Ok(names)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validate() {
        assert!(validate_hostname("git.example.com"));
        assert!(validate_hostname("a"));
        assert!(validate_hostname("*.example.com"));
        assert!(!validate_hostname(""));
        assert!(!validate_hostname(".example.com"));
        assert!(!validate_hostname("example.com."));
        assert!(!validate_hostname("ex ample.com"));
        assert!(!validate_hostname("ex*ample.com"));
        assert!(!validate_hostname("foo..bar"));
        assert!(!validate_hostname(&"a".repeat(256)));
    }

    #[test]
    fn round_trip_single() {
        let mut buf = Vec::new();
        encode_hostname_option(&mut buf, "git.example.com");
        // 4 header + 15 data = 19 -> padded to 20.
        assert_eq!(buf.len(), 20);
        assert_eq!(buf.len() % 4, 0);
        assert_eq!(buf[0], OPTION_HOSTNAME);
        assert_eq!(parse_hostname_options(&buf).unwrap(), vec!["git.example.com"]);
    }

    #[test]
    fn round_trip_multiple_and_lowercases() {
        let mut buf = Vec::new();
        encode_hostname_option(&mut buf, "First.Example.com");
        encode_hostname_option(&mut buf, "second.example.com");
        assert_eq!(
            parse_hostname_options(&buf).unwrap(),
            vec!["first.example.com", "second.example.com"]
        );
    }

    #[test]
    fn skips_unknown_options() {
        // An unknown 4-byte option (code 200, len 0) before a HOSTNAME.
        let mut buf = vec![200u8, 0, 0, 0];
        encode_hostname_option(&mut buf, "host.example.com");
        assert_eq!(parse_hostname_options(&buf).unwrap(), vec!["host.example.com"]);
    }

    #[test]
    fn rejects_truncated() {
        // code, reserved, length=10 but no data.
        assert!(parse_hostname_options(&[OPTION_HOSTNAME, 0, 0, 10]).is_err());
    }

    #[test]
    fn empty_tail_is_no_names() {
        assert_eq!(parse_hostname_options(&[]).unwrap(), Vec::<String>::new());
    }
}
