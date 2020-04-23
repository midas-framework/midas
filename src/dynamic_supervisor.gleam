// Fleet supervisor
import process

type SupProtocol {
    StartChild
    Exit
    Down
}

type Restart {
    Temporary
    Transient
    // Does that even make send for dymanic, how is it found, can do self discovery
    Permanent
}

fn run() {
    0
}


fn start_link(restart: Restart) -> process.Process(SupProtocol) {
    process.start_link(fn(receive) {
        let process.Message(StartChild) = receive()
        Nil
    })
}

// fn start_child(supervisor) -> process.Process(p) {
//     let Ok(Nil) = process.send(supervisor, StartChild)
//     0
// }
