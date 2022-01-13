use getmdl::loader::{Identifier, Symbol};

mod setup;

#[test]
fn has_entry_point_symbol() {
    setup::initialize_from_str(
        r#"
        .module {
            .name "SymbolTest";
        };

        .format {
            .major 0;
            .minor 3;
        };

        .code @code {
            .entry $BLOCK;
            .block $BLOCK () {
                ret;
            };
        };

        .function @f () returns () export "Helper" {
            .name "unused";
            .body defined @code;
        };
        "#,
        |runtime| {
            assert!(runtime
                .program()
                .lookup_function(Symbol::Owned(Identifier::try_from("Helper").unwrap()))
                .is_some())
        },
    );
}

#[test]
fn returns_exit_code() {
    setup::initialize_from_str(
        include_str!(r"../../asmdl_cli/samples/return.txtmdl"),
        |runtime| assert_eq!(0, runtime.invoke_entry_point(&[], None).unwrap()),
    );
}
