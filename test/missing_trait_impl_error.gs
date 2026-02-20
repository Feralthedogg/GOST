module main
trait Foo {
    fn foo() -> i32
}
fn call_foo[T: Foo]() -> i32 {
    return T.foo()
}
fn main() -> i32 {
    return call_foo[i32]()
}
