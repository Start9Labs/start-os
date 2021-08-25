use crate::net::tor::TorController;

async fn tor_health_check_daemon(tor_controller: &TorController) {
    loop {
        // call out to tor address
        // if success, do nothing
        // if failure, disconnect tor control port, and restart tor controller
    }
}
