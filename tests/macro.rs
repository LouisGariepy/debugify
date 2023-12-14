use debugify::Debugify;

#[derive(Debugify)]
enum Foo {
    Bleh,
    Biz { x: i32 },
    Blaz(u32, u32),
}
