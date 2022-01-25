use getmdl::loader::{Identifier, Symbol};
use runmdl::interpreter;

mod setup;

macro_rules! basic_module_str {
    ($name: ident, $contents: expr) => {{
        use registir::format::FormatVersion;
        &format!(
            ".module {{ .name \"{}\"; }};\n.format {{ .major {}; .minor {}; }};\n{}",
            stringify!($name),
            FormatVersion::minimum_supported_version().major,
            FormatVersion::minimum_supported_version().minor,
            $contents
        )
    }};
}

#[test]
fn returns_exit_code() {
    setup::initialize_from_str(
        include_str!(r"../../asmdl_cli/samples/return.txtmdl"),
        |_, _| (),
        |_, runtime| assert_eq!(0, runtime.invoke_entry_point(&[], None).unwrap()),
    );
}

#[test]
fn successful_function_symbol_lookup() {
    setup::initialize_from_str(
        basic_module_str!(
            SymbolTest,
            r#"
.code @code {
    .entry $BLOCK;
    .block $BLOCK () {
        ret;
    };
};

.function @Helper () returns () export {
    .name "unused";
    .body defined @code;
};
        "#
        ),
        |_, _| (),
        |_, runtime| {
            assert!(runtime
                .program()
                .lookup_function(Symbol::Owned(Identifier::try_from("Helper").unwrap()))
                .is_some())
        },
    );
}

#[test]
fn call_stack_overflow() {
    setup::initialize_from_str(
        basic_module_str!(
            StackOverflowTest,
            r#"
.code @code {
    .entry $BLOCK;
    .block $BLOCK () {
        call @exploder;
        ret;
    };
};

.function @exploder () returns () export {
    .name "boom";
    .body defined @code;
};

.entry @exploder;
"#
        ),
        |_, _| (),
        |_, runtime| match runtime.invoke_entry_point(&[], None) {
            Err(runmdl::runtime::Error::InterpreterError(error)) => match error.kind() {
                interpreter::ErrorKind::CallStackOverflow(capacity) => {
                    assert_eq!(error.stack_trace().len(), capacity.get())
                }
                error => panic!("unexpected error kind {:?}", error),
            },
            result => panic!("unexpected result {:?}", result),
        },
    );
}

#[test]
fn breakpoints_are_set_during_pause() {
    #[derive(Default)]
    struct CollectedData {
        value_1: Option<i32>,
        value_2: Option<i32>,
    }

    setup::initialize_from_str(
        basic_module_str!(
            BreakpointTest,
            r#"
.code @code {
    .entry $BLOCK;
    .block $BLOCK () {
        %t_ignored = const.i s32 5;
        %t_exit = const.i s32 42;
        ret %t_exit;
    };
};

.function @test () returns (s32) {
    .name "ThisIsAVeryLongTestOfThingsToSeeIfTheyWillFitAndAllThatYouKnow";
    .body defined @code;
};

.entry @test;
"#
        ),
        |program, _| {
            let program_name = program.header.0.identifier.clone();
            let returned_data = std::rc::Rc::new(std::cell::RefCell::new(CollectedData::default()));
            let mut pause_count = 0u8;
            let data = returned_data.clone();
            let debugger = move |interpreter: &mut interpreter::Interpreter| {
                use interpreter::debugger;

                let call_stack = interpreter.call_stack();
                let trace = call_stack.stack_trace();

                let reply = match pause_count {
                    0 => {
                        call_stack
                            .breakpoints_mut()
                            .insert(debugger::Breakpoint::new_owned(
                                debugger::BlockIndex::entry(),
                                1,
                                program_name.clone(),
                                registir::format::Identifier::try_from("test").unwrap(),
                            ));

                        debugger::Reply::Continue
                    }
                    1 => {
                        call_stack
                            .breakpoints_mut()
                            .insert(debugger::Breakpoint::with_symbol(
                                debugger::BlockIndex::entry(),
                                2,
                                trace[0].function().clone(),
                            ));

                        data.borrow_mut().value_1 =
                            Some(i32::try_from(&trace[0].temporary_registers()[0]).unwrap());
                        debugger::Reply::Continue
                    }
                    2.. => {
                        data.borrow_mut().value_2 =
                            Some(i32::try_from(&trace[0].temporary_registers()[1]).unwrap());
                        debugger::Reply::Detach
                    }
                };

                pause_count += 1;
                reply
            };
            (returned_data, debugger)
        },
        |(data, debugger), runtime| {
            assert_eq!(42, runtime.invoke_entry_point(&[], Some(debugger)).unwrap());
            assert_eq!(Some(5), data.borrow().value_1);
            assert_eq!(Some(42), data.borrow().value_2);
        },
    );
}

#[test]
fn conditional_branching_is_correct() {
    setup::initialize_from_str(
        basic_module_str!(
            IfTest,
            r#"
.code @code {
    .entry $BLOCK;
    .block $BLOCK (%i_input) {
        br.if %i_input then $RET_GOOD else $RET_BAD;
    };
    .block $RET_GOOD () {
        %t_result = const.i s32 61453;
        ret %t_result;
    };
    .block $RET_BAD () {
        %t_result = const.i s32 2989;
        ret %t_result;
    };
};

.function @test (s32) returns (s32) export {
    .name "test";
    .body defined @code;
};
"#
        ),
        |_, _| (),
        |_, runtime| {
            let test_function = runtime
                .program()
                .lookup_function(Symbol::Owned(Identifier::try_from("test").unwrap()))
                .unwrap();

            let mut arguments = [interpreter::Register::from(5i32)];

            assert_eq!(
                vec![interpreter::Register::from(0xF00Di32)],
                runtime.invoke(test_function, &arguments, None).unwrap()
            );

            arguments[0] = interpreter::Register::from(0i32);

            assert_eq!(
                vec![interpreter::Register::from(0xBADi32)],
                runtime.invoke(test_function, &arguments, None).unwrap()
            );
        },
    );
}

#[test]
fn switch_is_correct() {
    setup::initialize_from_str(
        basic_module_str!(
            SwitchTest,
            r#"
.code @code {
    .entry $BLOCK;
    .block $BLOCK (%i_input) {
        switch s32 %i_input default $BRANCH_DEFAULT or 1 $BRANCH_ONE or 3 $BRANCH_THREE;
    };
    .block $BRANCH_ONE () {
        %t_result = const.i s32 100;
        ret %t_result;
    };
    .block $BRANCH_THREE () {
        %t_result = const.i s32 333;
        ret %t_result;
    };
    .block $BRANCH_DEFAULT () {
        %t_result = const.i s32 0;
        ret %t_result;
    };
};

.function @test (s32) returns (s32) export {
    .name "test";
    .body defined @code;
};
"#
        ),
        |_, _| (),
        |_, runtime| {
            let test_function = runtime
                .program()
                .lookup_function(Symbol::Owned(Identifier::try_from("test").unwrap()))
                .unwrap();

            let mut arguments = [interpreter::Register::from(1i32)];

            assert_eq!(
                vec![interpreter::Register::from(100i32)],
                runtime.invoke(test_function, &arguments, None).unwrap()
            );

            arguments[0] = interpreter::Register::from(3i32);

            assert_eq!(
                vec![interpreter::Register::from(333i32)],
                runtime.invoke(test_function, &arguments, None).unwrap()
            );

            arguments[0] = interpreter::Register::from(4i32);

            assert_eq!(
                vec![interpreter::Register::from(0i32)],
                runtime.invoke(test_function, &arguments, None).unwrap()
            );
        },
    );
}

#[test]
fn integer_conversions_are_correct() {
    setup::initialize_from_str(
        basic_module_str!(
            IntegerConversions,
            r#"
.code @code {
    .entry $BLOCK;
    .block $BLOCK (%i_input) {
        %s8 = conv.i %i_input to s8;
        %u8 = conv.i %i_input to u8;
        %s16 = conv.i %i_input to s16;
        %u16 = conv.i %i_input to u16;
        %u32 = conv.i %i_input to u32;
        %s64 = conv.i %i_input to s64;
        %u64 = conv.i %i_input to u64;
        ret %s8, %u8, %s16, %u16, %u32, %s64, %u64;
    };
};

.function @convert_int (s32) returns (s8, u8, s16, u16, u32, s64, u64) export {
    .name "convert_int";
    .body defined @code;
};
"#
        ),
        |_, _| (),
        |_, runtime| {
            let test_function = runtime
                .program()
                .lookup_function(Symbol::Owned(Identifier::try_from("convert_int").unwrap()))
                .unwrap();

            assert_eq!(
                vec![
                    interpreter::Register::from(-1i8),
                    interpreter::Register::from(u8::MAX),
                    interpreter::Register::from(-1i16),
                    interpreter::Register::from(u16::MAX),
                    interpreter::Register::from(u32::MAX),
                    interpreter::Register::from(-1i64),
                    interpreter::Register::from(u64::MAX),
                ],
                runtime
                    .invoke(test_function, &[interpreter::Register::from(-1i32)], None)
                    .unwrap()
            );
        },
    );
}
