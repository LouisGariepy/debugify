use debugify::Debugify;

#[derive(Debugify)]
enum Foo {
    Bleh,
    Biz {
        #[debugify("wow{}")]
        x: i32,
    },
    Blaz(#[debugify("wow{}")] u32, u32),
}

#[derive(Debugify)]
struct Blaz(#[debugify("wow{}")] u32, u32);

#[derive(Debugify)]
struct Biz {
    #[debugify("wow{}")]
    x: i32,
}
