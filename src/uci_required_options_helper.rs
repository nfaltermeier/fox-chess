use fox_chess_proc::uci_fields_struct;

// Fields will be added to this struct according to the fields listed in fox-chess-proc/src/lib.rs.
// Also a struct 'RequiredUciOptionsAsOptions' will be generated.
#[uci_fields_struct()]
pub struct RequiredUciOptions {}
